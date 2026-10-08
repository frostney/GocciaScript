#!/usr/bin/env python3
"""Deterministic CodeRabbit review trigger, wait and completion adapter.

This script is the only definition of how CodeRabbit is triggered, waited
for, and judged complete. Every decision reads sources bound to the exact
head commit, merged across the open pull requests whose head it is:

- CodeRabbit's commit statuses on the head, newest first;
- CodeRabbit review objects whose `commit_id` is the head;
- the summary comment's coverage marker naming the head, or, on a merge
  commit, its first parent;
- the summary comment version CodeRabbit edited in just before a rate-limit
  status, which carries the only stated retry time; and
- the requested pull request's trigger commands, and when the head last
  arrived on it (its first push, or no earlier than a later force push
  leaving or reaching it), it was opened, and it was last marked ready,
  after which CodeRabbit starts its own review.

The latest review cycle wins: evidence, triggers and pending statuses from
before the latest trigger or the requested pull request's latest push, open
or ready event are superseded, and among a pending status, a trigger and a
findings review the newest decides. A review that completed the commit
after the head last reached the requested pull request's branch, after its
latest trigger and after any newer review of the head still unfinished keeps
it complete for every pull request on it, whatever pull request was opened
or marked ready since and whatever status CodeRabbit posts on the commit
later. A newer review is unfinished from its pending status until CodeRabbit
completes, pauses or skips it, in the order of time and status id; on a head
other open pull requests share, a skip may answer one of them, so the
pending status stays current. Every
state that waits on CodeRabbit ends after a fixed bound: the wait for its
own review becomes a trigger, and an unanswered trigger, a stalled review,
an unexplained refusal and a second lock loss each become a blocked state.
A refusal whose newest paired notice is CodeRabbit's line saying waiting
won't change it is blocked at once. A GitHub read that
fails is an operational error, never missing evidence. Triggers are
serialized by a lock per user account, GitHub API root and repository
owner: CodeRabbit's review allowance belongs to the organization or account
that owns the repository, so runs for different owners never wait on each
other. The API root is the one gh sends requests to.
"""

from __future__ import annotations

import argparse
import fcntl
import json
import os
import pwd
import re
import sys
import time
from datetime import datetime, timezone
from pathlib import Path
from typing import Any, Callable, TextIO

sys.dont_write_bytecode = True  # Keep installed skill trees free of __pycache__.

sys.path.insert(0, str(Path(__file__).resolve().parents[2] / "delivery-wait" / "scripts"))

from kgr_github import (  # noqa: E402
    Gh,
    Metrics,
    RateLimited,
    TransientError,
    WaitError,
    emit,
    parse_time,
    positive_interval,
    result_envelope,
    stable_digest,
)


BOT_LOGINS = {"coderabbitai", "coderabbitai[bot]"}
TRIGGERS = {
    "incremental": "@coderabbitai review",
    "full": "@coderabbitai full review",
}

# Commit-status descriptions CodeRabbit posts on a head (context "CodeRabbit").
STATUS_RATE_LIMITED = re.compile(r"^\s*review rate limited\b", re.IGNORECASE)
STATUS_COMPLETED = re.compile(r"^\s*review completed\b", re.IGNORECASE)
STATUS_DRAFT_SKIP = re.compile(r"^\s*review skipped:\s*draft pull request\b", re.IGNORECASE)
STATUS_SKIPPED = re.compile(r"^\s*review skipped\b", re.IGNORECASE)
STATUS_PAUSED = re.compile(r"^\s*review paused\b", re.IGNORECASE)
STATUS_LOCK_LOSS = re.compile(r"^\s*review stopped after lock loss\b", re.IGNORECASE)

# "> **Next review available in:** **13 minutes**" and
# "Next included review available in 12 minutes."
STATED_WAIT = re.compile(
    r"Next (?:included )?review available in[\s*:]*(\d+)\s*(minute|second)s?\b", re.IGNORECASE
)
# CodeRabbit's notice when every seat is assigned and the review ran on the free
# tier; its stated wait is then no retry time. Only this line counts, beside the
# "Next included review available in N minutes" line of the same comment:
# > This review ran on the free tier because every seat on this organization's
# > plan is already assigned. Waiting won't change this — ask an organization admin ...
NO_CAPACITY = re.compile(
    r"^> This review ran on the free tier\b[^\n]*\. Waiting won['\u2019]t change this\b", re.MULTILINE
)
INCLUDED_WAIT = re.compile(r"Next included review available in \d+ minutes", re.IGNORECASE)

# A findings review names its findings in its body.
ACTIONABLE = re.compile(r"Actionable comments posted:\s*(\d+)", re.IGNORECASE)
REVIEW_FINDINGS = re.compile(
    r"(?:Outside diff range|Nitpick|Duplicate) comments \(\d+\)", re.IGNORECASE
)

# The first line of every version of CodeRabbit's summary comment. Its replies
# to commands start "<!-- This is an auto-generated reply by CodeRabbit -->".
SUMMARY_MARKER = "<!-- This is an auto-generated comment: summarize by coderabbit.ai -->"

# The summary comment marks the commit its latest review covered:
# <!-- final_review_risk_coverage:{"sourceCommitId":..,"coveredCommitId":..,"kind":"reviewed"} -->
COVERAGE_MARKER = re.compile(r"final_review_risk_coverage:(\{[^{}]*\})")
NO_ACTIONABLE = "No actionable comments were generated"
# The summary carries this block while CodeRabbit reviews any head of the pull request.
REVIEWING_BLOCK = "<!-- This is an auto-generated comment: review in progress by coderabbit.ai -->"

# CodeRabbit edited the stated wait into its summary comment 0-53 s before it
# posted the rate-limit status in 79 of 80 recorded refusals; 60 s bounds the
# pairing.
CORRELATION_SECONDS = 60
# CodeRabbit states waits in whole minutes, so a stated time can be a minute early.
WAIT_BUFFER_SECONDS = 60
# Where CodeRabbit reviewed a head unprompted, 95% of first statuses came
# within 38 s of the push, and all within 119 s of the pull request being
# marked ready. A push during its review of an earlier head was reviewed
# within 98 s after that review ended, and a second pass of a head started
# 13 s after its first completed.
AUTOMATIC_REVIEW_SECONDS = 120
# 95% of recorded triggers got a status within 142 s.
TRIGGER_ANSWER_SECONDS = 10 * 60
# The longest recorded pending status lasted 1,805 s.
IN_PROGRESS_SECONDS = 60 * 60
# How long a refusal without a paired stated wait holds the head's trigger.
UNKNOWN_WAIT_SECONDS = 15 * 60
EDIT_HISTORY_PAGES = 10

COMMENT_EDITS_QUERY = """query($id: ID!, $cursor: String) {
  node(id: $id) { ... on IssueComment {
    userContentEdits(first: 20, after: $cursor) {
      pageInfo { hasNextPage endCursor } nodes { editedAt diff }
    }
  } }
}"""
READY_QUERY = """query($owner: String!, $name: String!, $number: Int!) {
  repository(owner: $owner, name: $name) { pullRequest(number: $number) {
    timelineItems(last: 1, itemTypes: [READY_FOR_REVIEW_EVENT]) {
      nodes { ... on ReadyForReviewEvent { createdAt } }
    }
  } }
}"""
# Read backwards, newest page first, until a force push touching the head or the first one.
FORCE_PUSH_QUERY = """query($owner: String!, $name: String!, $number: Int!, $cursor: String) {
  repository(owner: $owner, name: $name) { pullRequest(number: $number) {
    timelineItems(last: 100, before: $cursor, itemTypes: [HEAD_REF_FORCE_PUSHED_EVENT]) {
      pageInfo { hasPreviousPage startCursor }
      nodes { ... on HeadRefForcePushedEvent { createdAt beforeCommit { oid } afterCommit { oid } } }
    }
  } }
}"""
FORCE_PUSH_PAGES = 20

# Every state a command reports, and the commands that report it. `status`
# reports one state per head and one for the whole set; `run` reports its
# head's state, which is a pending one when its deadline arrives first.
HEAD_STATES = frozenset(
    {
        "review-complete",
        "clean-complete",
        "trigger-incremental",
        "trigger-full",
        "awaiting-automatic",
        "triggered",
        "in-progress",
        "waiting",
        "rate-limited-unknown-wait",
        "draft",
        "skipped",
        "paused",
        "blocked-unanswered",
        "blocked-stalled",
        "blocked-unknown-wait",
        "blocked-no-capacity",
        "blocked-unconfirmed",
        "blocked-lock-loss",
        "unrecognized-status",
        "closed",
        "invalidated",
    }
)
STATE_COMMANDS: dict[str, frozenset[str]] = {
    **{state: frozenset({"run", "status"}) for state in HEAD_STATES},
    "satisfied": frozenset({"status"}),
    "blocked": frozenset({"status"}),
    "pending": frozenset({"run", "status"}),
    "lock-held": frozenset({"run"}),
    "operational-error": frozenset({"run", "status"}),
}

# `run` stops at a final state; it waits on the others until its deadline.
COMPLETE_STATES = frozenset({"review-complete", "clean-complete"})
BLOCKED_STATES = frozenset(
    {
        "skipped",
        "paused",
        "blocked-unanswered",
        "blocked-stalled",
        "blocked-unknown-wait",
        "blocked-no-capacity",
        "blocked-unconfirmed",
        "blocked-lock-loss",
        "unrecognized-status",
        "closed",
    }
)
FINAL_STATES = COMPLETE_STATES | BLOCKED_STATES | {"draft", "invalidated"}


def repo_parts(repo: str) -> tuple[str, str]:
    pieces = repo.split("/", 1)
    if len(pieces) != 2 or not all(pieces):
        raise WaitError("repository must be OWNER/REPO")
    return pieces[0], pieces[1]


def positive_pr(value: int) -> int:
    if value <= 0:
        raise WaitError("pull-request numbers must be positive")
    return value


def parse_timestamp(value: Any, label: str) -> float:
    if not isinstance(value, str) or not value:
        raise WaitError(f"CodeRabbit evidence is missing {label}")
    return parse_time(value)


def format_timestamp(value: float) -> str:
    return datetime.fromtimestamp(value, timezone.utc).isoformat().replace("+00:00", "Z")


def login_of(item: dict[str, Any], field: str = "user") -> str:
    return str((item.get(field) or {}).get("login") or "").lower()


def is_bot(item: dict[str, Any], field: str = "user") -> bool:
    return login_of(item, field) in BOT_LOGINS


def rest_items(gh: Gh, endpoint: str) -> list[dict[str, Any]]:
    pages = gh.rest_pages(endpoint)
    items: list[dict[str, Any]] = []
    for page in pages:
        if not isinstance(page, list) or not all(isinstance(item, dict) for item in page):
            raise WaitError(f"paginated GitHub response for {endpoint} is invalid")
        items.extend(page)
    return items


def no_capacity(body: str) -> bool:
    """Whether a comment version holds CodeRabbit's notice that waiting won't change its wait."""
    return bool(NO_CAPACITY.search(body) and INCLUDED_WAIT.search(body))


def stated_seconds(body: str) -> int | None:
    match = STATED_WAIT.search(body)
    if not match:
        return None
    return int(match.group(1)) * (60 if match.group(2).lower() == "minute" else 1)


# --- Head statuses ---------------------------------------------------------


def is_coderabbit(status: dict[str, Any]) -> bool:
    return "coderabbit" in str(status.get("context") or "").lower() and is_bot(status, "creator")


def coderabbit_statuses(gh: Gh, repo: str, sha: str) -> list[dict[str, Any]]:
    """CodeRabbit's statuses on one commit, oldest first."""
    statuses = [
        status
        for status in rest_items(gh, f"repos/{repo}/commits/{sha}/statuses?per_page=100")
        if is_coderabbit(status)
    ]
    return sorted(
        statuses,
        key=lambda status: (
            parse_timestamp(status.get("created_at"), "status created_at"),
            int(status.get("id") or 0),
        ),
    )


def status_kind(status: dict[str, Any] | None) -> str:
    if status is None:
        return "none"
    state = str(status.get("state") or "").lower()
    description = str(status.get("description") or "")
    if state == "pending":
        return "in-progress"
    if state in {"success", "failure"} and STATUS_RATE_LIMITED.search(description):
        return "rate-limited"
    if state == "failure" and STATUS_LOCK_LOSS.search(description):
        return "lock-loss"
    if state == "success":
        if STATUS_COMPLETED.search(description):
            return "completed"
        if STATUS_DRAFT_SKIP.search(description):
            return "draft-skip"
        if STATUS_SKIPPED.search(description):
            return "skipped"
        if STATUS_PAUSED.search(description):
            return "paused"
    return "unrecognized"


def status_summary(status: dict[str, Any] | None) -> dict[str, Any] | None:
    if status is None:
        return None
    return {
        "id": status.get("id"),
        "state": status.get("state"),
        "description": status.get("description"),
        "createdAt": status.get("created_at"),
        "kind": status_kind(status),
    }


# --- Stated waits ------------------------------------------------------------


def comment_versions(gh: Gh, item: dict[str, Any], since: float) -> list[tuple[float, str]]:
    """Every body the comment showed from `since` on, each with when it was set.

    An unedited comment has one version, its creation. An edited one has its
    edit history, newest first, where each entry is the full body from its
    `editedAt`. A history that cannot be read back to `since` is an
    operational error: the missing versions may hold a stated wait or the
    reviewing block.
    """
    body = str(item.get("body") or "")
    created = parse_timestamp(item.get("created_at"), "comment created_at")
    updated = parse_timestamp(item.get("updated_at"), "comment updated_at")
    if updated <= created:
        return [(created, body)]
    versions: list[tuple[float, str]] = [(updated, body)]
    unreadable = (
        f"the edit history of CodeRabbit comment {item.get('id')} could not be read back to "
        f"{format_timestamp(since)}"
    )
    node_id = item.get("node_id")
    if not isinstance(node_id, str) or not node_id:
        raise WaitError(f"{unreadable}: it has no node id")
    cursor: str | None = None
    for _page in range(EDIT_HISTORY_PAGES):
        data = gh.graphql(COMMENT_EDITS_QUERY, {"id": node_id, "cursor": cursor})
        edits = ((data or {}).get("node") or {}).get("userContentEdits") or {}
        times: list[float] = []
        for node in edits.get("nodes") or []:
            if not isinstance(node, dict):
                continue
            at = parse_timestamp(node.get("editedAt"), "edit editedAt")
            times.append(at)
            if isinstance(node.get("diff"), str):
                versions.append((at, node["diff"]))
        page = edits.get("pageInfo") or {}
        descending = times == sorted(times, reverse=True)
        if not page.get("hasNextPage") or (times and descending and times[-1] < since):
            return versions
        cursor = page.get("endCursor")
    raise WaitError(f"{unreadable} within {EDIT_HISTORY_PAGES} pages")


def is_summary(item: dict[str, Any]) -> bool:
    return is_bot(item) and str(item.get("body") or "").lstrip().startswith(SUMMARY_MARKER)


def correlated_wait(
    gh: Gh, comments: list[dict[str, Any]], status_at: float
) -> tuple[dict[str, Any] | None, str | None]:
    """The stated wait CodeRabbit edited in for a rate-limit status at `status_at`.

    Only summary-comment versions set within CORRELATION_SECONDS at or before
    the status count; when several state a wait, the latest retry time wins.
    Returns the wait, or None and why there is none.
    """
    start = status_at - CORRELATION_SECONDS
    best: tuple[float, float, int, dict[str, Any]] | None = None
    # The newest paired version alone says whether waiting can help, whatever retry time is latest;
    # between comments, the first listed wins a tie.
    newest: tuple[float, bool] | None = None
    for item in comments:
        if not is_summary(item):
            continue
        if parse_timestamp(item.get("updated_at"), "comment updated_at") < start:
            continue
        # Within one comment the current body, then GitHub's newest-first edit history, decides
        # what the comment showed at a second, whether or not that version states a wait.
        shown: set[float] = set()
        for at, body in comment_versions(gh, item, start):
            if at in shown:
                continue
            shown.add(at)
            seconds = stated_seconds(body)
            if seconds is None or not start <= at <= status_at:
                continue
            if best is None or at + seconds > best[0]:
                best = (at + seconds, at, seconds, item)
            if newest is None or at > newest[0]:
                newest = (at, no_capacity(body))
    if best is None or newest is None:
        return None, (
            f"no summary-comment version stating a wait was set within {CORRELATION_SECONDS} s "
            "before the status"
        )
    available, at, seconds, item = best
    capacity_refused = newest[1]
    return (
        {
            "commentId": item.get("id"),
            "editedAt": format_timestamp(at),
            "statedSeconds": seconds,
            "noCapacity": capacity_refused,
            "availableAt": format_timestamp(available),
            "retryAt": format_timestamp(available + WAIT_BUFFER_SECONDS),
            "retryAtEpoch": available + WAIT_BUFFER_SECONDS,
            "statusAt": format_timestamp(status_at),
        },
        None,
    )


# --- Completion ----------------------------------------------------------------


def findings_review(reviews: list[dict[str, Any]], head: str) -> dict[str, Any] | None:
    """The newest CodeRabbit review of exactly `head` that states findings."""
    found = []
    for review in reviews:
        body = str(review.get("body") or "")
        actionable = ACTIONABLE.search(body)
        evidence = bool(actionable and int(actionable.group(1)) >= 1) or bool(
            REVIEW_FINDINGS.search(body)
        )
        if is_bot(review) and review.get("commit_id") == head and evidence:
            found.append(
                {
                    "id": review.get("id"),
                    "submittedAt": review.get("submitted_at"),
                    "actionable": int(actionable.group(1)) if actionable else None,
                }
            )
    return max(found, key=lambda item: str(item["submittedAt"]), default=None)


def coverage(body: str) -> dict[str, Any] | None:
    match = COVERAGE_MARKER.search(body)
    if not match:
        return None
    try:
        value = json.loads(match.group(1))
    except json.JSONDecodeError:
        return None
    return value if isinstance(value, dict) else None


def clean_review(bot_comments: list[dict[str, Any]], covered: list[str]) -> dict[str, Any] | None:
    """A clean pass: a coverage marker naming one of `covered` and no actionable comments."""
    for item in bot_comments:
        body = str(item.get("body") or "")
        marker = coverage(body)
        if (
            marker
            and marker.get("coveredCommitId") in covered
            and marker.get("kind") == "reviewed"
            and NO_ACTIONABLE in body
        ):
            return {"commentId": item.get("id"), "coveredCommitId": marker["coveredCommitId"]}
    return None


# --- Triggers ----------------------------------------------------------------


def pull_timeline(gh: Gh, repo: str, pr: int) -> dict[str, Any]:
    owner, name = repo_parts(repo)
    data = gh.graphql(READY_QUERY, {"owner": owner, "name": name, "number": pr})
    return ((data or {}).get("repository") or {}).get("pullRequest") or {}


def head_arrival(
    gh: Gh,
    repo: str,
    pull: dict[str, Any],
    head: str,
    statuses: list[dict[str, Any]],
    pr: int,
) -> tuple[float, str]:
    """When the head last reached the PR branch, and the evidence for it.

    GitHub creates check suites for a pushed commit on its branch at push
    time, once per commit: a rerun or a later push of the same commit adds
    none. Without one, CodeRabbit's first status on the head, which follows
    the push; without that, the commit time, which precedes it. A branch that
    left the head did so by a force push, which the pull request's timeline
    dates; it came back no earlier, by that force push's return or by a push
    no event dates, so the latest force push leaving or reaching the head
    bounds its arrival.
    """
    first = first_arrival(gh, repo, pull, head, statuses)
    move = latest_force_push(gh, repo, pr, head)
    if move is None or move[0] <= first[0]:
        return first
    return move[0], "force-push" if move[1] else "force-push-away"


def latest_force_push(gh: Gh, repo: str, pr: int, head: str) -> tuple[float, bool] | None:
    """The latest force push that left or reached `head`, and whether it reached it.

    A history that cannot be read back to such a force push or to its start is
    an operational error: an unread force push may have left the head.
    """
    owner, name = repo_parts(repo)
    cursor: str | None = None
    for _page in range(FORCE_PUSH_PAGES):
        data = gh.graphql(FORCE_PUSH_QUERY, {"owner": owner, "name": name, "number": pr, "cursor": cursor})
        items = (((data or {}).get("repository") or {}).get("pullRequest") or {}).get("timelineItems")
        page = (items or {}).get("pageInfo")
        if not isinstance(page, dict) or not isinstance(page.get("hasPreviousPage"), bool):
            raise WaitError("the pull request's force-push history has no page information")
        moves = []
        for node in items.get("nodes") or []:
            commits = [((node or {}).get(key) or {}).get("oid") for key in ("beforeCommit", "afterCommit")]
            if not all(isinstance(oid, str) and oid for oid in commits):
                raise WaitError("a force push on the pull request's timeline is missing its commits")
            if head in commits:
                moves.append((parse_timestamp(node.get("createdAt"), "force push createdAt"), commits[1] == head))
        if moves:
            return max(moves)
        if not page["hasPreviousPage"]:
            return None
        cursor = page.get("startCursor")
        if not isinstance(cursor, str) or not cursor:
            raise WaitError("the pull request's force-push history has no cursor for its earlier page")
    raise WaitError(f"the pull request's force-push history was not read to its start within {FORCE_PUSH_PAGES} pages")


def first_arrival(
    gh: Gh, repo: str, pull: dict[str, Any], head: str, statuses: list[dict[str, Any]]
) -> tuple[float, str]:
    head_ref = (pull.get("head") or {}).get("ref")
    pages = gh.rest_pages(f"repos/{repo}/commits/{head}/check-suites?per_page=100")
    created = [
        parse_timestamp(suite.get("created_at"), "check suite created_at")
        for page in pages
        if isinstance(page, dict)
        for suite in page.get("check_suites") or []
        if isinstance(suite, dict) and suite.get("head_branch") == head_ref
    ]
    if created:
        return min(created), "check-suite"
    if statuses:
        return parse_timestamp(statuses[0].get("created_at"), "status created_at"), "status"
    commit = gh.rest(f"repos/{repo}/commits/{head}")
    committed = (((commit or {}).get("commit") or {}).get("committer") or {}).get("date")
    return parse_timestamp(committed, "head commit time"), "commit"


def automatic_review_start(
    pull: dict[str, Any], timeline: dict[str, Any], arrival: tuple[float, str]
) -> tuple[float, str]:
    """The latest event after which CodeRabbit starts its own review of the head.

    It reviews a head when it is pushed, when the pull request is opened, and
    when the pull request is marked ready for review.
    """
    nodes = ((timeline.get("timelineItems") or {}).get("nodes")) or []
    events = [arrival, (parse_timestamp(pull.get("created_at"), "pull request created_at"), "opened")]
    events += [
        (parse_timestamp(node.get("createdAt"), "ready-for-review createdAt"), "ready-for-review")
        for node in nodes
        if isinstance(node, dict)
    ]
    return max(events, key=lambda event: event[0])


def earlier_review(gh: Gh, bot_comments: list[dict[str, Any]], start: float) -> dict[str, Any] | None:
    """CodeRabbit's review of an earlier head running since `start`, and when it ended.

    CodeRabbit reviews one head of a pull request at a time and starts its own
    review of a newer head only after the running one ends. While it reviews,
    its summary on that pull request carries the reviewing block.
    """
    for item in bot_comments:
        if not is_summary(item):
            continue
        versions = comment_versions(gh, item, start)
        ordered = sorted(versions, key=lambda version: version[0])
        shown = [(at, REVIEWING_BLOCK in body) for at, body in ordered if at <= start][-1:]
        shown += [(at, REVIEWING_BLOCK in body) for at, body in ordered if at > start]
        busy = [at for at, reviewing in shown if reviewing]
        if not busy:
            continue
        ended = next((at for at, reviewing in shown if at > busy[-1] and not reviewing), None)
        return {"commentId": item.get("id"), "endedAt": format_timestamp(ended) if ended else None}
    return None


def trigger_mode(body: str) -> str | None:
    """The review a comment requests: a line that is exactly a trigger command, a full one first.

    The command may come with other lines of explanation; a command quoted
    inside a sentence requests nothing.
    """
    modes = {command: mode for mode, command in TRIGGERS.items()}
    requested = {modes.get(" ".join(line.split()).lower()) for line in body.splitlines()}
    return next((mode for mode in ("full", "incremental") if mode in requested), None)


def head_triggers(comments: list[dict[str, Any]], arrival: float) -> list[dict[str, Any]]:
    """Trigger commands posted on the PR since the head arrived, oldest first."""
    found = []
    for item in comments:
        mode = trigger_mode(str(item.get("body") or ""))
        if mode is None or is_bot(item):
            continue
        created = parse_timestamp(item.get("created_at"), "trigger created_at")
        if created >= arrival:
            found.append(
                {"id": item.get("id"), "mode": mode, "createdAt": item["created_at"], "at": created}
            )
    return found


# --- Evidence and decision ------------------------------------------------------


def sharing_pulls(gh: Gh, repo: str, pr: int, head: str) -> list[int]:
    """The other open pull requests whose head is `head`."""
    pulls = rest_items(gh, f"repos/{repo}/commits/{head}/pulls?per_page=100")
    return sorted(
        {
            item["number"]
            for item in pulls
            if isinstance(item.get("number"), int)
            and item["number"] != pr
            and item.get("state") == "open"
            and (item.get("head") or {}).get("sha") == head
        }
    )


def pull_evidence(gh: Gh, repo: str, pr: int) -> dict[str, Any]:
    repo_parts(repo)
    pr = positive_pr(pr)
    pull = gh.rest(f"repos/{repo}/pulls/{pr}")
    if not isinstance(pull, dict):
        raise WaitError(f"pull request {repo}#{pr} response is invalid")
    head = (pull.get("head") or {}).get("sha")
    if not isinstance(head, str) or not head:
        raise WaitError(f"pull request {repo}#{pr} is missing its head SHA")
    statuses = coderabbit_statuses(gh, repo, head)
    newest = statuses[-1] if statuses else None
    commit = gh.rest(f"repos/{repo}/commits/{head}")
    parents = [str(item.get("sha")) for item in (commit or {}).get("parents") or []]
    # Open pull requests sharing the head add their reviews, coverage markers and stated
    # waits. Triggers and the events that start CodeRabbit's own review are the
    # requested pull request's alone.
    numbers = [pr, *sharing_pulls(gh, repo, pr, head)]
    own = rest_items(gh, f"repos/{repo}/issues/{pr}/comments?per_page=100")
    comments: list[dict[str, Any]] = []
    reviews: list[dict[str, Any]] = []
    for number in numbers:
        comments += own if number == pr else rest_items(gh, f"repos/{repo}/issues/{number}/comments?per_page=100")
        reviews += rest_items(gh, f"repos/{repo}/pulls/{number}/reviews?per_page=100")
    timeline = pull_timeline(gh, repo, pr)
    arrival = head_arrival(gh, repo, pull, head, statuses, pr)
    automatic, automatic_source = automatic_review_start(
        pull, timeline, (arrival[0], f"head-{arrival[1]}")
    )
    bot_comments = [item for item in comments if is_bot(item)]
    earlier = None
    if status_kind(newest) == "none" or parse_timestamp(newest["created_at"], "status created_at") < automatic:
        earlier = earlier_review(gh, [item for item in own if is_bot(item)], automatic)
    rate_limit = None
    if status_kind(newest) == "rate-limited":
        at = parse_timestamp(newest["created_at"], "status created_at")
        wait, missing = correlated_wait(gh, bot_comments, at)
        rate_limit = {"statusAt": format_timestamp(at), "wait": wait, "missing": missing}
    # On a merge commit CodeRabbit may leave the marker on the first parent it reviewed.
    covered = [head, parents[0]] if len(parents) > 1 else [head]
    return {
        "repo": repo,
        "pr": pr,
        "sharedWith": numbers[1:],
        "head": head,
        "parents": parents,
        "open": pull.get("state") == "open",
        "draft": bool(pull.get("draft")),
        "headArrival": {"at": format_timestamp(arrival[0]), "source": arrival[1]},
        "automaticReview": {"from": format_timestamp(automatic), "source": automatic_source},
        "earlierReview": earlier,
        "status": status_summary(newest),
        "findingsReview": findings_review(reviews, head),
        "cleanReview": clean_review(bot_comments, covered),
        "triggers": sorted(
            head_triggers(own, arrival[0]), key=lambda trigger: (trigger["at"], int(trigger["id"] or 0))
        ),
        "statusHistory": [
            {"createdAt": status["created_at"], "kind": status_kind(status), "description": status.get("description")}
            for status in statuses
        ],
        "rateLimit": rate_limit,
    }


def with_posted(evidence: dict[str, Any], posted: list[dict[str, Any]]) -> dict[str, Any]:
    """Evidence with the triggers this process posted that no listing shows yet."""
    listed = {trigger["id"] for trigger in evidence["triggers"]}
    unlisted = [
        {"id": item["commentId"], "mode": item["mode"], "createdAt": item["postedAt"], "at": item["at"]}
        for item in posted
        if item["commentId"] not in listed
    ]
    if not unlisted:
        return evidence
    triggers = sorted(evidence["triggers"] + unlisted, key=lambda trigger: trigger["at"])
    return evidence | {"triggers": triggers}


def decide(evidence: dict[str, Any], expected_head: str, now: float) -> dict[str, Any]:
    """The head's state, why, the trigger mode to post, and when it can change."""

    def result(state: str, reason: str, mode: str | None = None, **extra: Any) -> dict[str, Any]:
        if state not in HEAD_STATES:
            raise WaitError(f"CodeRabbit adapter produced an undeclared state {state!r}")
        return {"state": state, "reason": reason, "nextMode": mode, "retryAt": None} | extra

    def bounded(
        pending: str,
        blocked: str,
        since: float,
        seconds: int,
        why: str,
        mode: str | None = None,
        **extra: Any,
    ) -> dict[str, Any]:
        """`pending` for `seconds` after `since`, then `blocked`."""
        bound = since + seconds
        timing = {"since": format_timestamp(since), "boundAt": format_timestamp(bound)} | extra
        if now < bound:
            return result(pending, f"{why}; {blocked} at {timing['boundAt']}", mode, **timing)
        return result(blocked, f"{why} and nothing changed for {seconds // 60} minutes", **timing)

    def gate(mode: str) -> dict[str, Any]:
        """Hold the trigger while the head's own refusal still applies."""
        refusal = evidence["rateLimit"] if kind == "rate-limited" else None
        if refusal and not refusal["wait"]:
            return bounded(
                "rate-limited-unknown-wait",
                "blocked-unknown-wait",
                status_at,
                UNKNOWN_WAIT_SECONDS,
                f"CodeRabbit rate-limited the head at {refusal['statusAt']} and {refusal['missing']}",
                mode,
            )
        if refusal and refusal["wait"]["noCapacity"]:
            return result(
                "blocked-no-capacity",
                f"CodeRabbit refused the head at {refusal['statusAt']} and its notice says waiting "
                "won't change this",
            )
        if refusal and refusal["wait"]["retryAtEpoch"] > now:
            return result(
                "waiting",
                f"CodeRabbit stated a wait that ends at {refusal['wait']['retryAt']}",
                mode,
                retryAt=refusal["wait"]["retryAt"],
            )
        return result(f"trigger-{mode}", f"a {mode} review trigger is permitted", mode)

    def awaiting(since: float, until: float, after: str) -> dict[str, Any]:
        return result(
            "awaiting-automatic",
            f"CodeRabbit may still start its own review after {after}; a trigger is "
            f"permitted from {format_timestamp(until)}",
            since=format_timestamp(since),
            boundAt=format_timestamp(until),
        )

    if evidence["head"] != expected_head:
        return result("invalidated", f"expected head {expected_head}, observed {evidence['head']}")
    if not evidence["open"]:
        return result("closed", "the pull request is closed")
    automatic = evidence["automaticReview"]
    start = parse_timestamp(automatic["from"], "automatic review start")
    status = evidence["status"]
    kind = status["kind"] if status else "none"
    status_at = parse_timestamp(status["createdAt"], "status createdAt") if status else None
    history = [
        (parse_timestamp(item["createdAt"], "status createdAt"), item["kind"], item.get("description"))
        for item in evidence["statusHistory"]
    ]
    # On a head other open pull requests share, a skip can answer one of them while CodeRabbit
    # still reviews the head: the pending status before it stays current.
    if evidence["sharedWith"] and kind == "skipped":
        answered = [entry for entry in history if entry[1] != "skipped"]
        if answered and answered[-1][1] == "in-progress":
            status_at, kind = answered[-1][0], "in-progress"
            status = {"description": answered[-1][2], "createdAt": format_timestamp(status_at)}
    # A newer push, open or ready event starts a new cycle: older triggers no longer
    # await an answer, and a pending status's bound counts from the new cycle.
    triggers = [trigger for trigger in evidence["triggers"] if trigger["at"] >= start]
    latest = triggers[-1] if triggers else None
    # A trigger or an automatic start begins a new review; evidence from before it is superseded.
    begun = max(start, latest["at"]) if latest else start
    # No answer comes within a second of its trigger, so a status in that second predates it.
    trigger_newer = latest is not None and (status_at is None or latest["at"] >= status_at)
    # Completion belongs to the commit: the pull request's own open or ready event leaves a
    # review of the head standing, while its triggers, the head's latest push and a newer review
    # of the head supersede it. A newer review counts from its pending status until CodeRabbit
    # completes, pauses or skips it; a lock loss or rate limit ends it unfinished, and on a shared
    # head a skip may answer another pull request.
    arrived = parse_timestamp(evidence["headArrival"]["at"], "head arrival")
    asked = [trigger["at"] for trigger in evidence["triggers"]]
    # The history is in CodeRabbit's order, by time and then status id: what follows the pending
    # status in it came after it, whatever second each shows.
    ending = {"completed", "paused", "draft-skip"} | (set() if evidence["sharedWith"] else {"skipped"})
    pending = [index for index, (_when, seen, _description) in enumerate(history) if seen == "in-progress"]
    newer = [
        history[index][0] for index in pending[-1:]
        if not any(seen in ending for _when, seen, _description in history[index + 1:])
    ]
    reviewed_since = max([arrived, *asked, *newer])
    review = evidence["findingsReview"]
    reviewed_at = parse_timestamp(review["submittedAt"], "review submitted_at") if review else None
    completed_at = max((when for when, seen, _description in history if seen == "completed"), default=None)
    # Among a pending status, a trigger and a findings review, the newest decides.
    if reviewed_at is not None and reviewed_at > reviewed_since and (kind != "in-progress" or reviewed_at > status_at):
        return result("review-complete", "a CodeRabbit review of exactly this head states findings")
    if kind == "in-progress" and not trigger_newer:
        return bounded(
            "in-progress",
            "blocked-stalled",
            max(status_at, start),
            IN_PROGRESS_SECONDS,
            f"CodeRabbit has reported {status['description']!r} on the head since {status['createdAt']}",
        )
    # CodeRabbit keeps the marker while it reviews again; the completed status dates the pass.
    # A later status that is not pending, such as a skip for another pull request on the
    # commit, leaves the commit reviewed.
    if evidence["cleanReview"] and completed_at is not None and completed_at > reviewed_since:
        return result(
            "clean-complete",
            f"CodeRabbit's summary marks {evidence['cleanReview']['coveredCommitId']} covered "
            "with no actionable comments",
        )
    unreviewed = kind in {"none", "draft-skip"}
    if unreviewed and evidence["draft"]:
        return result("draft", "CodeRabbit does not review a draft pull request")
    if trigger_newer:
        return bounded(
            "triggered",
            "blocked-unanswered",
            latest["at"],
            TRIGGER_ANSWER_SECONDS,
            f"the {latest['mode']} trigger posted at {latest['createdAt']} awaits a CodeRabbit status",
        )
    if unreviewed or start > status_at:
        since, after = start, f"{automatic['source']} at {automatic['from']}"
        earlier = evidence["earlierReview"]
        if earlier and not earlier["endedAt"]:
            until = start + IN_PROGRESS_SECONDS
            after = f"its review of an earlier head, running at {automatic['from']}, ends"
        elif earlier:
            since = max(start, parse_timestamp(earlier["endedAt"], "earlier review end"))
            until = since + AUTOMATIC_REVIEW_SECONDS
            after = f"its review of an earlier head ended at {earlier['endedAt']}"
        else:
            until = start + AUTOMATIC_REVIEW_SECONDS
        if now < until:
            return awaiting(since, until, after)
    if unreviewed:
        return gate("incremental")
    if kind == "lock-loss":
        losses = [when for when, seen, _description in history if seen == "lock-loss" and when >= start]
        if len(losses) > 1:
            return result(
                "blocked-lock-loss",
                f"CodeRabbit stopped its review of the head after lock loss {len(losses)} times "
                f"since {automatic['source']} at {automatic['from']}",
            )
        # Retry the interrupted review once: the latest trigger's mode when nothing else answered it.
        answered = latest is not None and any(
            latest["at"] < when < status_at and seen != "in-progress" for when, seen, _description in history
        )
        return gate(latest["mode"] if latest and not answered else "incremental")
    if kind == "skipped":
        return result("skipped", f"CodeRabbit skipped the head: {status['description']}")
    if kind == "paused":
        return result("paused", "CodeRabbit paused its reviews of the pull request")
    if kind == "rate-limited":
        return gate(latest["mode"] if latest else "incremental")
    if kind == "completed":
        if any(trigger["mode"] == "full" for trigger in triggers):
            return result(
                "blocked-unconfirmed",
                "CodeRabbit reported the head's review completed without a review or "
                "coverage marker for it, after a full review was requested",
            )
        if now < status_at + AUTOMATIC_REVIEW_SECONDS:
            return awaiting(
                status_at,
                status_at + AUTOMATIC_REVIEW_SECONDS,
                f"its completed status at {status['createdAt']}, which has no review or coverage marker",
            )
        return gate("full")
    return result(
        "unrecognized-status",
        f"unrecognized CodeRabbit status: {status['state']} {status['description']!r}",
    )


def public(evidence: dict[str, Any]) -> dict[str, Any]:
    """Evidence without internal epoch fields."""
    value = dict(evidence)
    value["triggers"] = [
        {key: item[key] for key in ("id", "mode", "createdAt")} for item in evidence["triggers"]
    ]
    if evidence["rateLimit"] and evidence["rateLimit"]["wait"]:
        wait = evidence["rateLimit"]["wait"]
        value["rateLimit"] = evidence["rateLimit"] | {
            "wait": {key: entry for key, entry in wait.items() if key != "retryAtEpoch"}
        }
    return value


class ObservationCache:
    """Serves each GitHub read once per observation; writes pass through."""

    def __init__(self, gh: Gh) -> None:
        self.gh = gh
        self.metrics = gh.metrics
        self.memo: dict[Any, Any] = {}

    def remember(self, key: Any, read: Callable[[], Any]) -> Any:
        if key not in self.memo:
            self.memo[key] = read()
        return self.memo[key]

    def rest_pages(self, endpoint: str) -> list[Any]:
        return self.remember(("pages", endpoint), lambda: self.gh.rest_pages(endpoint))

    def rest(self, endpoint: str, method: str = "GET", fields: dict[str, str] | None = None) -> Any:
        if method != "GET":
            return self.gh.rest(endpoint, method, fields)
        return self.remember(("rest", endpoint), lambda: self.gh.rest(endpoint))

    def graphql(self, query: str, variables: dict[str, Any]) -> Any:
        key = ("graphql", query, repr(sorted(variables.items())))
        return self.remember(key, lambda: self.gh.graphql(query, variables))


def observation(
    gh: Gh,
    repo: str,
    prs: list[int],
    expected_heads: dict[int, str],
    now: float,
    posted: list[dict[str, Any]] | None = None,
) -> dict[str, Any]:
    gh = ObservationCache(gh)  # type: ignore[assignment]
    results = []
    for pr in prs:
        evidence = with_posted(pull_evidence(gh, repo, pr), posted or [])
        results.append(public(evidence) | decide(evidence, expected_heads[pr], now))
    return {"pullRequests": results}


def aggregate_status(value: dict[str, Any]) -> tuple[str, str]:
    states = [item["state"] for item in value["pullRequests"]]
    if "invalidated" in states:
        return "invalidated", "at least one requested PR head changed"
    if all(state in COMPLETE_STATES for state in states):
        return "satisfied", "every requested head has a confirmed CodeRabbit review"
    if any(state in BLOCKED_STATES for state in states):
        return "blocked", "at least one requested head cannot get a CodeRabbit review without a person"
    return "pending", "at least one requested head still awaits a CodeRabbit transition"


# --- Trigger lock --------------------------------------------------------------


def lock_directory() -> Path:
    """The user account's home directory from the password database, whatever TMPDIR or HOME says."""
    return Path(pwd.getpwuid(os.getuid()).pw_dir) / ".cache" / "known-good-route" / "coderabbit"


def lock_owner(repo: str) -> str:
    """The owner whose CodeRabbit allowance a review of `repo` uses; GitHub owners ignore case."""
    return repo_parts(repo)[0].lower()


def api_root(gh: Gh) -> str:
    """The API root gh sends requests to, such as https://api.github.com."""
    url = (gh.rest("/") or {}).get("current_user_url")
    if not isinstance(url, str) or not url.endswith("/user"):
        raise WaitError("GitHub's API root response has no current_user_url ending in /user")
    return url.removesuffix("/user")


def lock_path(root: str, owner: str) -> Path:
    return lock_directory() / f"owner-{stable_digest([root, owner])[:16]}.lock"


def open_lock(root: str, owner: str) -> tuple[Path, TextIO]:
    path = lock_path(root, owner)
    try:
        path.parent.mkdir(parents=True, exist_ok=True)
        return path, path.open("a+")
    except OSError as error:
        raise WaitError(f"the trigger lock {path} cannot be opened: {error}") from error


def read_holder(handle: TextIO) -> dict[str, Any] | None:
    handle.seek(0)
    raw = handle.read().strip()
    if not raw:
        return None
    try:
        value = json.loads(raw)
    except json.JSONDecodeError:
        return {"unreadable": raw}
    return value if isinstance(value, dict) else {"unreadable": raw}


def lock_status(root: str, owner: str) -> dict[str, Any]:
    """Whether a `run` holds the owner's trigger lock, and which PR and head it serves."""
    path, handle = open_lock(root, owner)
    lock = {"apiRoot": root, "owner": owner, "path": str(path)}
    with handle:
        try:
            fcntl.flock(handle.fileno(), fcntl.LOCK_EX | fcntl.LOCK_NB)
        except BlockingIOError:
            return lock | {"held": True, "holder": read_holder(handle)}
        fcntl.flock(handle.fileno(), fcntl.LOCK_UN)
    return lock | {"held": False, "holder": None}


def acquire_lock(
    root: str,
    owner: str,
    holder: dict[str, Any],
    deadline: float,
    interval: float,
    *,
    clock: Callable[[], float] = time.time,
    sleeper: Callable[[float], None] = time.sleep,
) -> TextIO | None:
    _path, handle = open_lock(root, owner)
    while True:
        try:
            fcntl.flock(handle.fileno(), fcntl.LOCK_EX | fcntl.LOCK_NB)
        except BlockingIOError:
            if clock() >= deadline:
                handle.close()
                return None
            sleeper(min(interval, max(0.0, deadline - clock())))
            continue
        handle.seek(0)
        handle.truncate()
        handle.write(
            json.dumps(
                holder
                | {"apiRoot": root, "owner": owner, "pid": os.getpid(), "acquiredAt": format_timestamp(clock())},
                sort_keys=True,
            )
            + "\n"
        )
        handle.flush()
        return handle


def release_lock(handle: TextIO) -> None:
    handle.seek(0)
    handle.truncate()
    handle.flush()
    fcntl.flock(handle.fileno(), fcntl.LOCK_UN)
    handle.close()


# --- Commands ----------------------------------------------------------------


def public_posted(posted: list[dict[str, Any]]) -> list[dict[str, Any]]:
    return [{key: value for key, value in item.items() if key != "at"} for item in posted]


def run_review(
    gh: Gh,
    repo: str,
    pr: int,
    head: str,
    deadline: float,
    interval: float,
    *,
    clock: Callable[[], float] = time.time,
    sleeper: Callable[[float], None] = time.sleep,
) -> tuple[str, str, dict[str, Any]]:
    """Trigger, wait and poll until the head reaches a final state or the deadline."""
    posted: list[dict[str, Any]] = []
    last: dict[str, Any] = {}
    while True:
        try:
            evidence = pull_evidence(ObservationCache(gh), repo, pr)  # type: ignore[arg-type]
        except (RateLimited, TransientError):
            gh.metrics.retries += 1
            if clock() >= deadline:
                last["triggers"] = public_posted(posted)
                return "pending", "GitHub transport remained unavailable until the deadline", last
            sleeper(min(interval, max(0.0, deadline - clock())))
            continue
        item = decide(with_posted(evidence, posted), head, clock())
        permitted = item["state"].startswith("trigger-")
        if permitted and clock() < deadline:
            mode = item["nextMode"]
            created = gh.rest(f"repos/{repo}/issues/{pr}/comments", "POST", {"body": TRIGGERS[mode]})
            posted.append(
                {
                    "mode": mode,
                    "body": TRIGGERS[mode],
                    "commentId": (created or {}).get("id"),
                    "postedAt": format_timestamp(clock()),
                    "at": clock(),
                }
            )
            # The same evidence with the posted trigger, so a run ending here reports it.
            item, permitted = decide(with_posted(evidence, posted), head, clock()), False
        last = {"pullRequests": [public(with_posted(evidence, posted)) | item], "triggers": public_posted(posted)}
        state, reason = item["state"], item["reason"]
        if state in FINAL_STATES:
            return state, reason, last
        if permitted:
            return state, "deadline reached before the permitted trigger", last
        if clock() >= deadline:
            return state, f"deadline reached; {reason}", last
        # Wake when the state can next change: a stated wait ends or a bound passes.
        wakes = [interval, deadline - clock()]
        wakes += [parse_time(item[key]) - clock() for key in ("retryAt", "boundAt") if item.get(key)]
        sleeper(max(0.0, min(wakes)))


def parser() -> argparse.ArgumentParser:
    result = argparse.ArgumentParser()
    subparsers = result.add_subparsers(dest="command", required=True)
    status = subparsers.add_parser("status")
    status.add_argument("--repo", required=True)
    status.add_argument("--pr", type=int, action="append", required=True)
    status.add_argument("--head", action="append", required=True, metavar="PR=SHA")
    status.add_argument("--json", action="store_true")
    run = subparsers.add_parser("run")
    run.add_argument("--repo", required=True)
    run.add_argument("--pr", type=int, required=True)
    run.add_argument("--head", required=True)
    run.add_argument("--deadline", required=True)
    run.add_argument("--interval", type=float, default=60.0)
    run.add_argument("--json", action="store_true")
    return result


def parse_heads(values: list[str], prs: list[int]) -> dict[int, str]:
    heads: dict[int, str] = {}
    for value in values:
        number, separator, head = value.partition("=")
        if not separator or not number.isdigit() or not head:
            raise WaitError("--head must use PR=SHA")
        pr = positive_pr(int(number))
        if pr in heads:
            raise WaitError(f"duplicate --head for PR #{pr}")
        heads[pr] = head
    if set(heads) != set(prs):
        raise WaitError("--head entries must match every --pr exactly")
    return heads


def envelope(
    command: str,
    state: str,
    identity: dict[str, Any],
    value: dict[str, Any],
    metrics: Metrics,
    reason: str,
) -> dict[str, Any]:
    """The command's result, refused unless STATE_COMMANDS declares `state` for `command`."""
    if command not in STATE_COMMANDS.get(state, frozenset()):
        raise WaitError(f"CodeRabbit {command} produced an undeclared state {state!r}")
    kind = f"coderabbit-{command}" if state == "operational-error" else "coderabbit"
    return result_envelope(kind, state, identity, value, metrics, reason)


def main() -> int:
    args = parser().parse_args()
    metrics = Metrics(time.monotonic())
    identity: dict[str, Any] = {"repo": args.repo, "prs": getattr(args, "pr", None)}
    try:
        owner = lock_owner(args.repo)
        gh = Gh(metrics)
        root = api_root(gh)
        if args.command == "status":
            prs = [positive_pr(value) for value in args.pr]
            if len(set(prs)) != len(prs):
                raise WaitError("--pr values must be unique")
            heads = parse_heads(args.head, prs)
            identity["heads"] = heads
            lock = lock_status(root, owner)
            value = observation(gh, args.repo, prs, heads, time.time()) | {"lock": lock}
            metrics.observations += 1
            state, reason = aggregate_status(value)
            output = envelope("status", state, identity, value, metrics, reason)
        else:
            args.pr = positive_pr(args.pr)
            args.interval = positive_interval(args.interval)
            deadline = parse_time(args.deadline)
            identity = {"repo": args.repo, "prs": [args.pr], "heads": {args.pr: args.head}}
            holder = {"repo": args.repo, "pr": args.pr, "head": args.head}
            lock = acquire_lock(root, owner, holder, deadline, args.interval)
            if lock is None:
                output = envelope(
                    "run", "lock-held", identity, {"lock": lock_status(root, owner)}, metrics,
                    f"another CodeRabbit run for {owner} at {root} held the trigger lock until the deadline",
                )
            else:
                try:
                    state, reason, value = run_review(
                        gh, args.repo, args.pr, args.head, deadline, args.interval,
                    )
                    metrics.observations += 1
                    output = envelope("run", state, identity, value, metrics, reason)
                finally:
                    release_lock(lock)
        emit(output, args.json)
        return 0
    except WaitError as error:
        output = envelope(args.command, "operational-error", identity, {}, metrics, str(error))
        emit(output, getattr(args, "json", False))
        return 2


if __name__ == "__main__":
    sys.exit(main())
