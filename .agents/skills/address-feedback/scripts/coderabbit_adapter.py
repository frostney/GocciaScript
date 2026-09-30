#!/usr/bin/env python3
"""Deterministic CodeRabbit review pacing and completion adapter."""

from __future__ import annotations

import argparse
import fcntl
import re
import sys
import tempfile
import time
from datetime import datetime, timezone
from pathlib import Path
from typing import Any, Callable, TextIO
from urllib.parse import quote

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
STATED_WAIT = re.compile(
    r"(?:available in\D{0,10}|please wait\W{0,10})"
    r"(?:(\d+)\s*minutes?(?:\D{0,10}(\d+)\s*seconds?)?|(\d+)\s*seconds?)",
    re.IGNORECASE,
)
FINISHED = re.compile(r"review finished|reviews? (?:is|are) complete|no actionable comments", re.IGNORECASE)
SKIPPED = re.compile(r"review skipped", re.IGNORECASE)
RATE_LIMITED = re.compile(r"rate limited|review limit", re.IGNORECASE)
ALREADY_REVIEWED = re.compile(
    r"action not completed[\s\S]{0,300}?already reviewed", re.IGNORECASE
)
ACTIONABLE = re.compile(r"Actionable comments posted:\s*(\d+)", re.IGNORECASE)
UNAVAILABLE_SOURCE = re.compile(r"\(HTTP 40[34]\)")
WAIT_BUFFER_SECONDS = 60
TRUSTED_ACK_SECONDS = 30
# CodeRabbit states the account's included review allowance in each review's
# summary or review body, e.g. "1 included review remains after this review.
# ... set your current allowance at 2 reviews per hour." tests/fixtures holds
# every allowance line CodeRabbit has published on the sampled repositories.
ALLOWANCE_LABEL = re.compile(
    r"\*\*(?:Included review availability|Limit details):?\*\*|Review rate limit:",
    re.IGNORECASE,
)
# April-May 2026 summaries ended with "Review rate limit: 3/5 reviews
# remaining, refill in 19 minutes and 7 seconds."; it states no rate unit.
ALLOWANCE_LEGACY = re.compile(
    r"(\d+)/(\d+) reviews? remaining,?\s*(?:refill in "
    r"(?:(\d+) minutes?(?:\D{0,10}(\d+) seconds?)?|(\d+) seconds?))?",
    re.IGNORECASE,
)
ALLOWANCE_RATE = re.compile(
    r"(?:allowance at|refill at|provides up to) (\d+)(?: included)?(?: reviews?)?"
    r" per (minute|hour|day)\b",
    re.IGNORECASE,
)
ALLOWANCE_USED_ALL = re.compile(
    r"used (?:all \d+ included reviews?|the included review) currently available",
    re.IGNORECASE,
)
ALLOWANCE_REMAINING = (
    re.compile(r"(\d+)(?: included reviews?)? remains? after this review", re.IGNORECASE),
    re.compile(r"(\d+) reviews? (?:is|are) currently available", re.IGNORECASE),
)
RUN_ID = re.compile(r"Run ID\W{0,6}`([0-9A-Za-z-]+)`", re.IGNORECASE)
# A summary is a stack of generated blocks; a statement belongs to the run
# printed in its own block, and a rate-limit block's run was refused.
BLOCK_START = re.compile(
    r"<!-- (?:This is an auto-generated comment: [^>]*|recent_review_start) -->"
)
BLOCK_END = re.compile(
    r"<!-- (?:end of auto-generated comment: [^>]*|recent_review_end) -->"
)
REFUSED_BLOCK = re.compile(r"rate limited by coderabbit", re.IGNORECASE)
# A clean automatic review edits the summary's recent-review block in place:
# "No actionable comments were generated in the recent review" for the range
# "between <base> and <head>". It posts no review object.
RECENT_REVIEW = re.compile(r"<!-- recent_review_start -->([\s\S]*?)<!-- recent_review_end -->")
NO_ACTIONABLE = re.compile(r"No actionable comments were generated in the recent review", re.IGNORECASE)
REVIEWED_RANGE = re.compile(r"between ([0-9a-f]{40}) and ([0-9a-f]{40})\b")
# While either block shows, the summary does not report a settled review. A
# "Reviews paused" or "Review skipped" block is written beside a finished
# review (auto-pause, or "no new commits") and does not unsettle it: a skipped
# or paused new head leaves the recent block naming the previous head.
UNSETTLED_BLOCK = re.compile(
    r"auto-generated comment: (?:review in progress|rate limited) by coderabbit"
    r"|Currently processing new changes",
    re.IGNORECASE,
)
# A review object completes its head only with review evidence: an
# "Actionable comments posted: N" line with N >= 1, or a findings section.
REVIEW_FINDINGS = re.compile(
    r"(?:Outside diff range|Nitpick|Duplicate) comments \(\d+\)", re.IGNORECASE
)
# A CodeRabbit review body that is a notice rather than a review (#87); a
# second guard beside the evidence rule.
REVIEW_NOTICE = re.compile(
    r"auto-generated comment: (?:rate limited|skip review|review paused) by coderabbit"
    r"|^\W*#* *(?:Review skipped|Reviews? paused|Rate limit exceeded|Review limit reached)"
    r"|Review rate limited\.",
    re.IGNORECASE | re.MULTILINE,
)
# A successful check or status that reports a skipped or paused review is not
# a completed review (#87); rate-limited ones are handled separately.
INCOMPLETE_CHECK = re.compile(r"skipped|paused", re.IGNORECASE)
WINDOW_SECONDS = {"minute": 60, "hour": 3600, "day": 86400}
# A statement without a rate, or with a wording the parser does not recognize,
# is read against the hour: the unit of every rated statement CodeRabbit has
# published. The budget then reports `degraded`.
UNRATED_WINDOW_SECONDS = WINDOW_SECONDS["hour"]
# Repository-wide reads cover two windows plus the retry buffer: a statement
# stays current until one window and the buffer after it, and its own window
# reaches back one more. For the hour this is also the stated-wait horizon.
# Waits last until a slot in the hourly window frees; the longest in
# frostney/GocciaScript's history is 59 minutes, so a wait edited before the
# horizon would have to state over two hours to still be active. A PR's own
# refusal is read from its comments without a horizon.
ALLOWANCE_LOOKBACK_WINDOWS = 2
EDIT_HISTORY_PAGES = 10
COMMENT_EDITS_QUERY = """query($id: ID!, $cursor: String) {
  node(id: $id) { ... on IssueComment {
    userContentEdits(first: 20, after: $cursor) {
      pageInfo { hasNextPage endCursor } nodes { editedAt diff }
    }
  } }
}"""


def is_coderabbit_check_name(value: str) -> bool:
    return "coderabbit" in value.lower()


def check_text_rate_limited(*parts: Any) -> bool:
    """True when any supplied check/status text matches RATE_LIMITED."""
    for part in parts:
        text = str(part or "")
        if text and RATE_LIMITED.search(text):
            return True
    return False


def coderabbit_check_output_texts(run: dict[str, Any]) -> tuple[str, str, str]:
    output = run.get("output") if isinstance(run.get("output"), dict) else {}
    return (
        str(output.get("title") or ""),
        str(output.get("summary") or ""),
        str(output.get("text") or ""),
    )


def page_entries(pages: list[Any], key: str) -> list[dict[str, Any]]:
    """Flatten the named array of each object-shaped page, skipping malformed items."""
    return [
        entry
        for page in pages
        if isinstance(page, dict)
        for entry in page.get(key, [])
        if isinstance(entry, dict)
    ]


def optional_items(fetch: Callable[[], list[Any]]) -> list[Any]:
    """Items from a source the repository may not expose.

    A missing or forbidden endpoint yields no items; every other failure,
    including rate limits and transport errors, propagates.
    """
    try:
        return fetch()
    except (RateLimited, TransientError):
        raise
    except WaitError as error:
        if UNAVAILABLE_SOURCE.search(str(error)):
            return []
        raise


def coderabbit_check_for_head(gh: Gh, repo: str, head: str) -> dict[str, Any]:
    """Resolve head-scoped CodeRabbit check-run or commit-status SUCCESS.

    A completed SUCCESS conclusion whose output title/summary/text (or commit-
    status description) matches RATE_LIMITED is not treated as SUCCESS — it
    surfaces rateLimited so callers do not count a rate-limited pass as
    codeRabbitCheckSuccess.
    """
    declined: dict[str, Any] | None = None
    run_pages = gh.rest_pages(f"repos/{repo}/commits/{head}/check-runs?per_page=100")
    for run in page_entries(run_pages, "check_runs"):
        name = str(run.get("name") or "")
        if not is_coderabbit_check_name(name):
            continue
        run_head = run.get("head_sha")
        if isinstance(run_head, str) and run_head and run_head != head:
            continue
        if (
            str(run.get("status") or "").upper() == "COMPLETED"
            and str(run.get("conclusion") or "").upper() == "SUCCESS"
        ):
            title, summary, text = coderabbit_check_output_texts(run)
            if check_text_rate_limited(title, summary, text):
                return {
                    "state": "RATE_LIMITED",
                    "source": "check-run",
                    "name": name,
                    "head": head,
                    "rateLimited": True,
                }
            if any(INCOMPLETE_CHECK.search(part) for part in (title, summary, text)):
                declined = declined or {"state": "INCOMPLETE", "source": "check-run", "name": name, "head": head}
                continue
            return {
                "state": "SUCCESS",
                "source": "check-run",
                "name": name,
                "head": head,
            }

    statuses = rest_items(gh, f"repos/{repo}/commits/{head}/statuses?per_page=100")
    # Newest first. A skipped or paused success says CodeRabbit declined to
    # review the commit again, not that its review is undone, so it is passed
    # over; the newest other status decides. Only declines means incomplete.
    newest_first = sorted(
        (status for status in statuses if is_coderabbit_check_name(str(status.get("context") or ""))),
        key=lambda status: str(status.get("created_at") or ""),
        reverse=True,
    )
    for status in newest_first:
        context = str(status.get("context") or "")
        state = str(status.get("state") or "").upper()
        description = str(status.get("description") or "")
        # A rate limit wins over a decline in the same text, as for check runs.
        if state == "SUCCESS" and check_text_rate_limited(description):
            return {
                "state": "RATE_LIMITED",
                "source": "status",
                "name": context,
                "head": head,
                "rateLimited": True,
            }
        if state == "SUCCESS" and INCOMPLETE_CHECK.search(description):
            declined = declined or {"state": "INCOMPLETE", "source": "status", "name": context, "head": head}
            continue
        return {
            "state": state or None,
            "source": "status",
            "name": context,
            "head": head,
        }

    return declined or {"state": None, "source": None, "name": None, "head": head}

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


def is_bot(item: dict[str, Any]) -> bool:
    return str((item.get("user") or {}).get("login") or "").lower() in BOT_LOGINS


def rest_items(gh: Gh, endpoint: str) -> list[dict[str, Any]]:
    pages = gh.rest_pages(endpoint)
    items: list[dict[str, Any]] = []
    for page in pages:
        if not isinstance(page, list) or not all(isinstance(item, dict) for item in page):
            raise WaitError(f"paginated GitHub response for {endpoint} is invalid")
        items.extend(page)
    return items


def latest(items: list[dict[str, Any]], field: str) -> dict[str, Any] | None:
    candidates = [item for item in items if isinstance(item.get(field), str)]
    return max(candidates, key=lambda item: str(item[field]), default=None)


def scan_since(now: float, window: float = WINDOW_SECONDS["hour"]) -> float:
    return now - ALLOWANCE_LOOKBACK_WINDOWS * window - WAIT_BUFFER_SECONDS


def recent_comments_endpoint(repo: str, since: float) -> str:
    """Issue comments edited at or after `since`, newest edit first."""
    return (
        f"repos/{repo}/issues/comments?sort=updated&direction=desc"
        f"&per_page=100&since={format_timestamp(int(since))}"
    )


def issue_number(item: dict[str, Any]) -> int | None:
    tail = str(item.get("issue_url") or "").rsplit("/", 1)[-1]
    return int(tail) if tail.isdigit() else None


def stated_wait(item: dict[str, Any]) -> dict[str, Any] | None:
    """The wait a CodeRabbit comment states, timed from its last edit."""
    match = STATED_WAIT.search(str(item.get("body") or ""))
    if not match:
        return None
    updated_at = parse_timestamp(item.get("updated_at"), "wait updated_at")
    minutes, seconds, only_seconds = (int(group or 0) for group in match.groups())
    seconds = minutes * 60 + seconds + only_seconds
    return {
        "pr": issue_number(item),
        "commentId": item.get("id"),
        "updatedAt": item["updated_at"],
        "updatedAtEpoch": updated_at,
        "statedSeconds": seconds,
        "retryAt": format_timestamp(updated_at + seconds + WAIT_BUFFER_SECONDS),
        "retryAtEpoch": updated_at + seconds + WAIT_BUFFER_SECONDS,
    }


def account_wait(gh: Gh, repos: list[str], now: float) -> dict[str, Any] | None:
    """The newest stated wait in comments edited since scan_since(now)."""
    candidates: list[dict[str, Any]] = []
    for repo in repos:
        repo_parts(repo)
        for item in rest_items(gh, recent_comments_endpoint(repo, scan_since(now))):
            wait = stated_wait(item) if is_bot(item) else None
            if wait:
                candidates.append({"repo": repo} | wait)
    return max(candidates, key=lambda item: item["updatedAt"], default=None)


def parse_allowance(text: str) -> dict[str, Any] | None:
    """CodeRabbit's stated review allowance in `text`, or None when it states none.

    `allowance` and `unit` are None without a (non-zero) rate. `recognized` is
    False when the text carries an allowance label or rate but no count this
    parser knows; callers hold on such a statement rather than skip it.
    """
    rate = ALLOWANCE_RATE.search(text)
    if rate and int(rate.group(1)) < 1:
        rate = None
    label = ALLOWANCE_LABEL.search(text)
    remaining: int | None = None
    offsets = [match.start() for match in (rate, label) if match]
    legacy = ALLOWANCE_LEGACY.search(text)
    used_all = ALLOWANCE_USED_ALL.search(text)
    if legacy:
        remaining = int(legacy.group(1))
        offsets.append(legacy.start())
        minutes, seconds, only_seconds = legacy.group(3, 4, 5)
        refill = (
            int(minutes or 0) * 60 + int(seconds or 0) + int(only_seconds or 0)
            if minutes or only_seconds
            else None
        )
        return {
            "offset": min(offsets),
            "recognized": True,
            "remaining": remaining,
            "allowance": int(legacy.group(2)) if int(legacy.group(2)) >= 1 else None,
            "unit": None,
            "windowSeconds": UNRATED_WINDOW_SECONDS,
            "refillSeconds": refill,
        }
    if used_all:
        remaining = 0
        offsets.append(used_all.start())
    else:
        for pattern in ALLOWANCE_REMAINING:
            match = pattern.search(text)
            if match:
                remaining = int(match.group(1))
                offsets.append(match.start())
                break
    if not offsets:
        return None
    unit = rate.group(2).lower() if rate else None
    return {
        "offset": min(offsets),
        "recognized": remaining is not None,
        "remaining": remaining,
        "allowance": int(rate.group(1)) if rate else None,
        "unit": unit,
        "windowSeconds": WINDOW_SECONDS[unit] if unit else UNRATED_WINDOW_SECONDS,
        "refillSeconds": None,
    }


def statement_in(body: str) -> dict[str, Any] | None:
    """The first allowance statement in a body, bound to its block's run."""
    position = 0
    for line in body.splitlines(keepends=True):
        parsed = parse_allowance(line)
        if parsed is None:
            position += len(line)
            continue
        offset = position + parsed["offset"]
        starts = list(BLOCK_START.finditer(body, 0, offset))
        start = starts[-1].start() if starts else 0
        end_match = BLOCK_END.search(body, offset)
        end = end_match.start() if end_match else len(body)
        before = RUN_ID.findall(body, start, offset)
        after = RUN_ID.findall(body, offset, end)
        run_id = before[-1] if before else (after[0] if after else None)
        refused = bool(starts and REFUSED_BLOCK.search(starts[-1].group(0)))
        return parsed | {"line": line.strip(), "runId": run_id, "refused": refused}
    return None


def clean_review_in(body: str) -> dict[str, Any] | None:
    """The range a settled, clean recent review in a CodeRabbit summary covered.

    The recent-review block must say no actionable comments were generated
    and name the reviewed range; the summary must show no review in progress,
    rate limit, skip or pause.
    """
    if UNSETTLED_BLOCK.search(body):
        return None
    block = RECENT_REVIEW.search(body)
    if not block or not NO_ACTIONABLE.search(block.group(1)):
        return None
    reviewed = REVIEWED_RANGE.search(block.group(1))
    if not reviewed:
        return None
    run = RUN_ID.search(block.group(1))
    return {"base": reviewed.group(1), "head": reviewed.group(2), "runId": run.group(1) if run else None}


def bot_review_runs(reviews: list[dict[str, Any]]) -> dict[str, float]:
    """The earliest submission of each Run ID among CodeRabbit's reviews.

    A PENDING review is not submitted, so it dates nothing.
    """
    runs: dict[str, float] = {}
    for review in reviews:
        run = RUN_ID.search(str(review.get("body") or ""))
        if not is_bot(review) or review.get("state") == "PENDING" or not run:
            continue
        at = parse_timestamp(review.get("submitted_at"), "review submitted_at")
        runs[run.group(1)] = min(runs.get(run.group(1), at), at)
    return runs


def date_shown(
    gh: Gh,
    item: dict[str, Any],
    review_runs: dict[str, float],
    run_id: str | None,
    shows: Callable[[str], bool],
) -> tuple[float | None, str]:
    """When CodeRabbit made what a comment shows, and how that was established.

    In order: the review object of the same Run ID ("run"), a comment never
    edited ("unedited"), or the first edit that still showed it
    ("edit-history"). A summary edited in place keeps showing old content, so
    its last edit never dates it. Otherwise the time is unknown ("undated").
    """
    updated = parse_timestamp(item.get("updated_at"), "comment updated_at")
    if run_id and run_id in review_runs:
        return min(review_runs[run_id], updated), "run"
    if item.get("created_at") == item.get("updated_at"):
        return updated, "unedited"
    first = first_shown(gh, item, shows, contiguous=run_id is None)
    return first, "edit-history" if first is not None else "undated"


def statement_key(shown: dict[str, Any] | None) -> tuple[str, Any] | None:
    return (shown["line"], shown["runId"]) if shown else None


def clean_key(shown: dict[str, Any] | None) -> tuple[str, Any] | None:
    return (shown["head"], shown["runId"]) if shown else None


def first_shown(
    gh: Gh, item: dict[str, Any], shows: Callable[[str], bool], *, contiguous: bool = False
) -> float | None:
    """When a comment first showed what `shows` accepts, from its edit history.

    For content keyed by a Run ID, returns the earliest version that shows it:
    a later reappearance, for example after an in-progress block for another
    push came and went, is the same review and keeps its first time. Content
    without a Run ID can recur word for word after separate reviews, so with
    `contiguous` the walk stops at the first older version that does not show
    it. Returns None when the history cannot be read far enough.
    """
    node_id = item.get("node_id")
    if not isinstance(node_id, str) or not node_id:
        return None
    first_seen: float | None = None
    cursor: str | None = None
    for _page in range(EDIT_HISTORY_PAGES):
        try:
            data = gh.graphql(COMMENT_EDITS_QUERY, {"id": node_id, "cursor": cursor})
        except (RateLimited, TransientError):
            raise
        except WaitError:
            return None
        edits = ((data or {}).get("node") or {}).get("userContentEdits") or {}
        versions = sorted(
            (node for node in edits.get("nodes") or [] if isinstance(node, dict)),
            key=lambda node: str(node.get("editedAt") or ""),
            reverse=True,
        )
        for version in versions:
            if shows(str(version.get("diff") or "")):
                first_seen = parse_timestamp(version.get("editedAt"), "edit editedAt")
            elif contiguous:
                return first_seen
        page = edits.get("pageInfo") or {}
        if not page.get("hasNextPage"):
            return first_seen
        cursor = page.get("endCursor")
    return None


def scan_allowance(
    gh: Gh, repos: list[str], since: float
) -> tuple[dict[str, Any] | None, dict[tuple[str, str], float]]:
    """The newest current-evidence allowance statement and every counted run.

    A run is one CodeRabbit review, identified by the Run ID it prints in its
    statement's block and in any review object it submits. Each run counts
    once, at its earliest observed time; a rate-limit block's run was refused
    and is not counted. Evidence without a Run ID is not counted.

    A statement is timed by when it was made: a review's submission, the
    submission of the review object for the same run, a comment that was
    never edited, or else the first edit that showed it. A summary edited in
    place (a processing or pause block added) keeps showing an old statement,
    so its last edit does not date it. A statement that cannot be dated never
    outranks a dated one unless it reports no review left or uses an
    unrecognized wording.
    """
    statements: list[dict[str, Any]] = []
    pending: list[tuple[dict[str, Any], dict[str, Any]]] = []
    runs: dict[tuple[str, str], float] = {}

    def note_run(repo: str, run_id: str, at: float) -> None:
        runs[(repo, run_id)] = min(runs.get((repo, run_id), at), at)

    for repo in repos:
        repo_parts(repo)
        prs: set[int] = set()
        repo_reviews: list[dict[str, Any]] = []
        for item in rest_items(gh, recent_comments_endpoint(repo, since)):
            if not is_bot(item):
                continue
            pr = issue_number(item)
            if pr is not None:
                prs.add(pr)
            found = statement_in(str(item.get("body") or ""))
            if found is not None:
                pending.append((item, found | {"repo": repo, "pr": pr, "commentId": item.get("id")}))
        for pr in sorted(prs):
            reviews = optional_items(
                lambda: rest_items(gh, f"repos/{repo}/pulls/{pr}/reviews?per_page=100")
            )
            repo_reviews.extend(reviews)
            for item in reviews:
                body = str(item.get("body") or "")
                run = RUN_ID.search(body)
                if not is_bot(item) or item.get("state") == "PENDING" or not run:
                    continue
                submitted = parse_timestamp(item.get("submitted_at"), "review submitted_at")
                note_run(repo, run.group(1), submitted)
                found = statement_in(body)
                if found is not None:
                    statements.append(
                        found
                        | {
                            "repo": repo,
                            "pr": pr,
                            "reviewId": item.get("id"),
                            "updatedAt": item["submitted_at"],
                            "updatedAtEpoch": submitted,
                            "statedAtEpoch": submitted,
                            "dating": "review",
                        }
                    )
        review_runs = bot_review_runs(repo_reviews)
        for item, found in [entry for entry in pending if entry[1]["repo"] == repo]:
            updated = parse_timestamp(item.get("updated_at"), "comment updated_at")
            run_id = found["runId"]
            key = (found["line"], run_id)
            stated, dating = date_shown(
                gh,
                item,
                review_runs,
                run_id,
                lambda body, key=key: statement_key(statement_in(body)) == key,
            )
            statements.append(
                found
                | {
                    "updatedAt": item["updated_at"],
                    "updatedAtEpoch": updated,
                    "statedAtEpoch": stated,
                    "dating": dating,
                }
            )
            if run_id and not found["refused"]:
                # An undated run is counted at its last edit: later, so longer.
                note_run(repo, run_id, stated if stated is not None else updated)
    dated = [item for item in statements if item["statedAtEpoch"] is not None]
    # An undated "none left" (or unrecognized) statement is timed by its last
    # edit, which can only make it hold longer; other undated ones are ignored.
    holding = [
        item | {"statedAtEpoch": item["updatedAtEpoch"]}
        for item in statements
        if item["statedAtEpoch"] is None
        and (item["remaining"] == 0 or not item["recognized"])
    ]
    statement = max(dated + holding, key=lambda item: item["statedAtEpoch"], default=None)
    if statement is not None:
        statement = statement | {
            "undatedCount": sum(1 for item in statements if item["dating"] == "undated")
        }
    elif any(item["dating"] == "undated" for item in statements):
        statement = {
            "undatedOnly": True,
            "undatedCount": sum(1 for item in statements if item["dating"] == "undated"),
        }
    return statement, runs


def allowance_retry(
    runs: list[float], window: float, allowance: int, at: float, *, proven: bool
) -> float | None:
    """When a review slot frees after `at`, from the runs in the window ending then.

    A slot frees once enough counted runs leave the trailing window. With fewer
    counted runs than the allowance, only a `proven` exhaustion (a statement or
    notice from CodeRabbit) yields a time: its oldest counted run. The time is
    never early only when every run in the window is counted. Runs in an
    unscanned repository are missed, and so is a review that posts no review
    object (no actionable comments) when a later review on the same PR
    overwrites its summary block before a scan sees it. An uncounted run
    between counted ones leaves a slot free later than derived.
    """
    counted = sorted(run for run in runs if at - window < run <= at)
    excess = len(counted) - allowance + 1
    if not counted or (excess < 1 and not proven):
        return None
    return counted[min(max(excess, 1), len(counted)) - 1] + window + WAIT_BUFFER_SECONDS


def release_time(
    runs: list[float],
    window: float,
    allowance: int | None,
    exhausted_at: float,
    stated_refill: float | None = None,
) -> float:
    """When a used-up allowance frees a slot (maintainer ruling, PR #94 CR-4).

    The window is rolling: a slot frees when enough counted runs in the
    window ending at the exhaustion have left it that fewer than the
    allowance remain, plus the buffer. With several reviews per window that
    is the oldest such run leaving, not the statement's own run. When no
    counted run can be tied to it, or there is no rate, the run the statement
    reports on finished by the exhaustion, so a slot frees one window plus the
    buffer after it. A refill time CodeRabbit states ("refill in N minutes")
    is used as given, plus the buffer.
    """
    if stated_refill is not None:
        return exhausted_at + stated_refill + WAIT_BUFFER_SECONDS
    derived = (
        allowance_retry(runs, window, allowance, exhausted_at, proven=True)
        if allowance
        else None
    )
    return derived if derived is not None else exhausted_at + window + WAIT_BUFFER_SECONDS


def account_allowance(gh: Gh, repos: list[str], now: float) -> dict[str, Any]:
    """The account's review budget from CodeRabbit's statements and counted runs.

    `mode` is `enforced` when the newest statement is current, dated,
    recognized and states a rate with its unit, and `degraded` otherwise. Without a current statement only
    stated waits gate triggers. A statement stays current until one window and
    the retry buffer after it was made.
    """
    window = WINDOW_SECONDS["hour"]
    statement, runs = scan_allowance(gh, repos, scan_since(now, window))
    if statement and statement.get("windowSeconds", window) > window:
        window = statement["windowSeconds"]
        statement, runs = scan_allowance(gh, repos, scan_since(now, window))
    undated = (statement or {}).get("undatedCount", 0)
    undated_note = f"; {undated} allowance statements could not be dated" if undated else ""
    budget: dict[str, Any] = {
        "mode": "degraded",
        "reason": (
            "no allowance statement within its window in the scanned "
            f"repositories; only stated waits gate triggers{undated_note}"
        ),
        "remaining": None,
        "availableNow": None,
        "perHour": None,
        "reviewsPerWindow": None,
        "windowSeconds": None,
        "statementAt": None,
        "statementDating": None,
        "source": None,
        "attempts": {"count": 0, "windowStart": None, "runs": []},
        "exhausted": False,
        "retryAt": None,
        "exhaustionRetryEpoch": None,
        "runEpochs": [],
    }
    if (
        statement is None
        or statement.get("undatedOnly")
        or statement["statedAtEpoch"]
        + max(statement["windowSeconds"], statement.get("refillSeconds") or 0)
        + WAIT_BUFFER_SECONDS
        <= now
    ):
        return budget
    window = statement["windowSeconds"]
    allowance = statement["allowance"]
    remaining = statement["remaining"]
    stated_at = statement["statedAtEpoch"]
    run_epochs = sorted(runs.values())
    in_window = sorted(
        (at, repo, run_id)
        for (repo, run_id), at in runs.items()
        if now - window < at <= now
    )
    budget |= {
        "mode": (
            "enforced"
            if statement["unit"] and statement["recognized"] and statement["dating"] != "undated"
            else "degraded"
        ),
        "remaining": remaining,
        "perHour": allowance if statement["unit"] == "hour" else None,
        "reviewsPerWindow": allowance,
        "windowSeconds": window,
        "statementAt": format_timestamp(stated_at),
        "statementDating": statement["dating"],
        "source": {
            "repo": statement["repo"],
            "pr": statement["pr"],
            **(
                {"reviewId": statement["reviewId"]}
                if "reviewId" in statement
                else {"commentId": statement["commentId"]}
            ),
        },
        "attempts": {
            "count": len(in_window),
            "windowStart": format_timestamp(int(now - window)),
            "runs": [
                {"repo": repo, "runId": run_id, "at": format_timestamp(at)}
                for at, repo, run_id in in_window
            ],
        },
        "runEpochs": run_epochs,
    }
    # The newest statement anchors the count: with R reviews left when it was
    # made, the allowance is used up from the R-th counted run after it.
    # Counted times can trail a run's start, so a larger window count alone
    # never overrides the statement. A wording the parser does not recognize
    # counts as used up.
    after = [at for at in run_epochs if at > stated_at]
    exhausted_at: float | None = None
    stated_refill: float | None = None
    if not statement["recognized"] or remaining == 0:
        exhausted_at = stated_at
        stated_refill = statement.get("refillSeconds")
    elif len(after) >= remaining:
        exhausted_at = after[remaining - 1]
    retry = (
        release_time(run_epochs, window, allowance, exhausted_at, stated_refill)
        if exhausted_at is not None
        else None
    )
    exhausted = retry is not None and retry > now
    if not statement["recognized"]:
        detail = "the newest allowance statement uses an unrecognized wording"
    elif allowance:
        detail = (
            f"{len(in_window)} counted review runs in the trailing window; "
            f"the newest statement leaves {remaining} of {allowance}"
        )
        if not statement["unit"]:
            detail += "; it states no rate unit, so its window is read as one hour"
    else:
        detail = "the newest allowance statement states no rate; it is read against one hour"
    if statement["dating"] == "undated":
        detail += "; its time could not be established, so its last edit times it"
    reason = (
        f"the included review allowance is used up until retryAt; {detail}"
        if exhausted
        else detail
    )
    if exhausted_at is None:
        available_now = max(0, remaining - len(after)) if remaining is not None else None
    else:
        # After a used-up allowance frees, at least one slot is free, but the
        # evidence does not say how many.
        available_now = 0 if exhausted else None
    budget |= {
        "exhausted": exhausted,
        "availableNow": available_now,
        "retryAt": format_timestamp(retry) if exhausted and retry else None,
        "exhaustionRetryEpoch": retry,
        "reason": reason + undated_note,
    }
    return budget


def trigger_gate(
    evidence: dict[str, Any], wait: dict[str, Any] | None, budget: dict[str, Any] | None
) -> dict[str, Any] | None:
    """The latest evidenced time before which CodeRabbit would refuse a trigger.

    Stated waits come from the account scan and all of the PR's own comments;
    for a rate-limit notice, only one posted at or after it counts. The
    allowance adds the time its exhaustion ends and, for a notice, when the
    runs counted before it free a slot; an allowance time that precedes the
    notice cannot explain it.

    Returns None when nothing gates a trigger. For a rate-limit notice that
    nothing explains, returns a gate with source "unexplained" and no time.
    """
    limited = bool(evidence.get("rateLimited"))
    limited_at = evidence.get("rateLimitedAtEpoch") if limited else None
    unexplained = {"retryAtEpoch": None, "retryAt": None, "source": "unexplained"}
    if limited and limited_at is None:
        return unexplained
    gates: list[tuple[float, str]] = []
    for stated in (wait, *(evidence.get("prWaits") or [])):
        if stated is not None and (limited_at is None or stated["updatedAtEpoch"] >= limited_at):
            gates.append((stated["retryAtEpoch"], "stated-wait"))
    allowance_times: list[float] = []
    if budget and budget.get("exhaustionRetryEpoch") is not None:
        allowance_times.append(budget["exhaustionRetryEpoch"])
    if budget and budget.get("reviewsPerWindow") and limited_at is not None:
        derived = allowance_retry(
            budget["runEpochs"],
            budget["windowSeconds"],
            budget["reviewsPerWindow"],
            limited_at,
            proven=True,
        )
        if derived is not None:
            allowance_times.append(derived)
    gates.extend(
        (at, "allowance")
        for at in allowance_times
        if limited_at is None or at > limited_at
    )
    if not gates:
        return unexplained if limited else None
    retry, source = max(gates)
    return {"retryAtEpoch": retry, "retryAt": format_timestamp(retry), "source": source}


GATE_REASONS = {
    "stated-wait": "a stated wait gates the next trigger",
    "allowance": "the account's included CodeRabbit review allowance is used up",
}


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


def head_push_time(
    gh: Gh, repo: str, pull: dict[str, Any], head: str
) -> tuple[str | None, str | None]:
    """Return when GitHub received the head and which evidence recorded it.

    The committer date is set locally and can precede the push by any amount,
    so it cannot bound evidence about this head. Prefer the branch activity
    entry that moved the head ref to this commit, then the earliest check suite
    GitHub created for the commit on the head branch. Suites are per commit, so
    one created for another branch may predate this push and is ignored. A
    missing or forbidden endpoint only removes that source.
    """
    head_info = pull.get("head") or {}
    head_ref = head_info.get("ref")
    head_repo = (head_info.get("repo") or {}).get("full_name") or repo
    if not isinstance(head_ref, str) or not head_ref:
        return None, None
    ref = quote(f"refs/heads/{head_ref}", safe="/")
    entries = optional_items(
        lambda: rest_items(gh, f"repos/{head_repo}/activity?ref={ref}&per_page=100")
    )
    arrival = latest(
        [entry for entry in entries if entry.get("after") == head], "timestamp"
    )
    if arrival:
        return arrival["timestamp"], "activity"
    pages = optional_items(
        lambda: gh.rest_pages(f"repos/{repo}/commits/{head}/check-suites?per_page=100")
    )
    created = [
        suite["created_at"]
        for suite in page_entries(pages, "check_suites")
        if suite.get("head_branch") == head_ref
        and isinstance(suite.get("created_at"), str)
    ]
    if created:
        return min(created, key=lambda value: parse_timestamp(value, "check suite created_at")), "check-suite"
    return None, None


def exact_head_clean_review(
    gh: Gh,
    head: str,
    pushed_epoch: float,
    bot_comments: list[dict[str, Any]],
    reviews: list[dict[str, Any]],
) -> dict[str, Any] | None:
    """A clean automatic review of exactly this head, from the PR's summary.

    The summary's settled recent-review block must report no actionable
    comments for a range ending at `head`. It is dated as allowance
    statements are (the review object of its Run ID, an unedited comment, or
    the first edit that showed it) and must not predate the head's push. A
    review that cannot be dated does not count.
    """
    review_runs = bot_review_runs(reviews)
    found: list[dict[str, Any]] = []
    for item in bot_comments:
        clean = clean_review_in(str(item.get("body") or ""))
        if not clean or clean["head"] != head:
            continue
        key = clean_key(clean)
        reviewed, dating = date_shown(
            gh,
            item,
            review_runs,
            clean["runId"],
            lambda body, key=key: clean_key(clean_review_in(body)) == key,
        )
        if reviewed is None or reviewed < pushed_epoch:
            continue
        found.append(
            clean
            | {
                "commentId": item.get("id"),
                "reviewedAt": format_timestamp(reviewed),
                "reviewedAtEpoch": reviewed,
                "dating": dating,
            }
        )
    return max(found, key=lambda item: item["reviewedAtEpoch"], default=None)


def pull_evidence(gh: Gh, repo: str, pr: int) -> dict[str, Any]:
    repo_parts(repo)
    pr = positive_pr(pr)
    pull = gh.rest(f"repos/{repo}/pulls/{pr}")
    if not isinstance(pull, dict):
        raise WaitError(f"pull request {repo}#{pr} response is invalid")
    head = (pull.get("head") or {}).get("sha")
    if not isinstance(head, str) or not head:
        raise WaitError(f"pull request {repo}#{pr} is missing its head SHA")
    commit = gh.rest(f"repos/{repo}/commits/{head}")
    committed_at = (
        ((commit or {}).get("commit") or {}).get("committer") or {}
    ).get("date")
    committed_epoch = parse_timestamp(committed_at, "head commit time")
    pushed_at, push_source = head_push_time(gh, repo, pull, head)
    pushed_epoch = (
        max(committed_epoch, parse_timestamp(pushed_at, "head push time"))
        if pushed_at
        else committed_epoch
    )

    comments = rest_items(gh, f"repos/{repo}/issues/{pr}/comments?per_page=100")
    reviews = rest_items(gh, f"repos/{repo}/pulls/{pr}/reviews?per_page=100")
    files = rest_items(gh, f"repos/{repo}/pulls/{pr}/files?per_page=100")
    bot_comments = [item for item in comments if is_bot(item)]
    trigger_comments = [
        item
        for item in comments
        if not is_bot(item)
        and str(item.get("body") or "").strip().lower() in set(TRIGGERS.values())
        and parse_timestamp(item.get("created_at"), "trigger created_at") >= pushed_epoch
    ]
    trigger = latest(trigger_comments, "created_at")
    boundary = max(
        pushed_epoch,
        parse_timestamp(trigger.get("created_at"), "trigger created_at") if trigger else pushed_epoch,
    )

    exact_reviews = []
    for review in reviews:
        body = str(review.get("body") or "")
        match = ACTIONABLE.search(body)
        # Only review evidence counts: actionable comments, or findings that
        # are only outside the diff, nitpicks or duplicates (those carry no
        # "Actionable comments posted" line). Reply-only records are empty,
        # and any other body, including an unknown notice, is not a review.
        evidence = bool(match and int(match.group(1)) >= 1) or bool(REVIEW_FINDINGS.search(body))
        if (
            is_bot(review)
            and review.get("state") != "PENDING"
            and review.get("commit_id") == head
            and evidence
            and not REVIEW_NOTICE.search(body)
        ):
            exact_reviews.append(
                {
                    "id": review.get("id"),
                    "submittedAt": review.get("submitted_at"),
                    "actionable": int(match.group(1)) if match else None,
                }
            )
    exact_review = latest(exact_reviews, "submittedAt")
    clean_review = exact_head_clean_review(gh, head, pushed_epoch, bot_comments, reviews)

    after_boundary = [
        item
        for item in bot_comments
        if parse_timestamp(item.get("created_at"), "comment created_at") >= boundary
    ]
    finished = latest(
        [
            item
            for item in after_boundary
            if FINISHED.search(str(item.get("body") or ""))
            and not SKIPPED.search(str(item.get("body") or ""))
        ],
        "created_at",
    )
    skipped = latest(
        [item for item in after_boundary if SKIPPED.search(str(item.get("body") or ""))],
        "created_at",
    )
    limited = latest(
        [item for item in after_boundary if RATE_LIMITED.search(str(item.get("body") or ""))],
        "created_at",
    )
    refused = latest(
        [item for item in after_boundary if ALREADY_REVIEWED.search(str(item.get("body") or ""))],
        "created_at",
    )
    walkthrough = latest(
        [
            item
            for item in bot_comments
            if "summarize by coderabbit" in str(item.get("body") or "").lower()
            and parse_timestamp(item.get("updated_at"), "walkthrough updated_at") >= pushed_epoch
        ],
        "updated_at",
    )
    filenames = [str(item.get("filename") or "") for item in files]
    walkthrough_body = str((walkthrough or {}).get("body") or "")
    matched = sum(
        1
        for filename in filenames
        if filename and (filename in walkthrough_body or Path(filename).name in walkthrough_body)
    )
    ack_latency = (
        parse_timestamp(finished.get("created_at"), "finished created_at") - boundary
        if finished
        else None
    )
    trigger_mode = None
    if trigger:
        trigger_mode = next(
            mode
            for mode, body in TRIGGERS.items()
            if body == str(trigger.get("body") or "").strip().lower()
        )

    code_rabbit_check = coderabbit_check_for_head(gh, repo, head)
    check_rate_limited = bool(code_rabbit_check.get("rateLimited"))
    # Every wait stated on the PR, read without the account scan's horizon:
    # an old refusal can then be retried once its own wait elapses, and a
    # still-active wait is never hidden by a newer, shorter one elsewhere.
    pr_waits = [wait for wait in map(stated_wait, bot_comments) if wait]

    return {
        "repo": repo,
        "pr": pr,
        "head": head,
        "headCommittedAt": committed_at,
        "headPushedAt": pushed_at,
        "headPushSource": push_source,
        "trigger": (
            {
                "id": trigger.get("id"),
                "mode": trigger_mode,
                "createdAt": trigger.get("created_at"),
            }
            if trigger
            else None
        ),
        "exactHeadReview": exact_review,
        "cleanReview": clean_review,
        "finishedAck": (
            {
                "id": finished.get("id"),
                "createdAt": finished.get("created_at"),
                "latencySeconds": int(max(0, ack_latency or 0)),
            }
            if finished
            else None
        ),
        "skippedAck": bool(skipped),
        "rateLimited": bool(limited) or check_rate_limited,
        "rateLimitedAt": limited.get("created_at") if limited else None,
        "rateLimitedAtEpoch": (
            parse_timestamp(limited.get("created_at"), "rate-limit created_at")
            if limited
            else None
        ),
        "alreadyReviewed": bool(refused),
        "prWaits": pr_waits,
        "coverage": {
            "walkthroughId": (walkthrough or {}).get("id"),
            "matched": matched,
            "changed": len(filenames),
            "verified": matched > 0,
        },
        "codeRabbitCheck": code_rabbit_check,
        "codeRabbitCheckSuccess": code_rabbit_check.get("state") == "SUCCESS",
    }


def classify(
    evidence: dict[str, Any],
    expected_head: str,
    wait: dict[str, Any] | None,
    now: float,
    budget: dict[str, Any] | None = None,
) -> tuple[str, str, str | None]:
    return classify_with_gate(evidence, expected_head, wait, now, budget)[:3]


def classify_with_gate(
    evidence: dict[str, Any],
    expected_head: str,
    wait: dict[str, Any] | None,
    now: float,
    budget: dict[str, Any] | None = None,
) -> tuple[str, str, str | None, dict[str, Any] | None]:
    """The head's state, its reason and next mode, and the gate when waiting."""
    gate = trigger_gate(evidence, wait, budget)
    state, reason, mode = decide(evidence, expected_head, gate, now)
    return state, reason, mode, gate if state == "waiting" else None


def decide(
    evidence: dict[str, Any],
    expected_head: str,
    gate: dict[str, Any] | None,
    now: float,
) -> tuple[str, str, str | None]:
    if evidence["head"] != expected_head:
        return "invalidated", f"expected head {expected_head}, observed {evidence['head']}", None
    if evidence["exactHeadReview"]:
        return "review-complete", "exact-head CodeRabbit review object observed", None
    # A clean automatic review of exactly this head, with the head-scoped
    # check or status reporting a completed review, completes it. It is bound
    # by commit SHA, so a later refusal of another trigger does not undo it.
    if evidence.get("cleanReview") and evidence.get("codeRabbitCheckSuccess"):
        return (
            "clean-complete",
            "CodeRabbit's summary reports no actionable comments for a review "
            "ending at this head, and its head-scoped check succeeded",
            None,
        )
    ack = evidence["finishedAck"]

    # Rate-limit evidence must win over a SUCCESS check short-circuit: a green
    # CodeRabbit check whose description is "Review rate limited" is pending,
    # not clean-complete.
    if evidence["rateLimited"]:
        if gate is None or gate["retryAtEpoch"] is None:
            return (
                "pending-retry-source",
                "rate limited without a stated retry time or an allowance-derived one",
                None,
            )
        desired = evidence["trigger"]["mode"] if evidence["trigger"] else "incremental"
        if gate["retryAtEpoch"] > now:
            return "waiting", GATE_REASONS[gate["source"]], desired
        return f"trigger-{desired}", f"{desired} review trigger is permitted", desired

    code_rabbit_ok = bool(evidence.get("codeRabbitCheckSuccess")) or (
        (evidence.get("codeRabbitCheck") or {}).get("state") == "SUCCESS"
    )
    # Without a GitHub-recorded push time, an acknowledgment may belong to the
    # previous head, so only the head-scoped check can complete the review.
    push_known = evidence.get("headPushedAt") is not None
    trusted_latency = bool(ack) and ack["latencySeconds"] >= TRUSTED_ACK_SECONDS
    latency_ok = push_known and trusted_latency
    if ack and evidence["coverage"]["verified"] and (latency_ok or code_rabbit_ok):
        if code_rabbit_ok and not latency_ok:
            reason = (
                "finished acknowledgment has current walkthrough coverage "
                "and head-scoped CodeRabbit check SUCCESS"
            )
        else:
            reason = "finished acknowledgment has current walkthrough coverage"
        return "clean-complete", reason, None
    if evidence["skippedAck"]:
        return "pending-skipped", "CodeRabbit reported that review was skipped", None
    if trusted_latency and not push_known and evidence["coverage"]["verified"]:
        return (
            "pending-check-required",
            "head push time is unknown, so clean completion requires the "
            "head-scoped CodeRabbit check",
            None,
        )

    desired: str | None = None
    trigger_mode = (evidence.get("trigger") or {}).get("mode")
    if evidence["alreadyReviewed"] and trigger_mode == "full":
        return "pending-full-refused", "CodeRabbit refused an explicit full review", None
    if ack and trigger_mode == "full":
        return "pending-full-unverified", "full-review acknowledgment did not prove current diff coverage", None
    if evidence["alreadyReviewed"] or ack:
        desired = "full"
    elif evidence["trigger"]:
        return "in-flight", "trigger is awaiting completion evidence", None
    else:
        desired = "incremental"

    if gate is not None and gate["retryAtEpoch"] is not None and gate["retryAtEpoch"] > now:
        return "waiting", GATE_REASONS[gate["source"]], desired
    return f"trigger-{desired}", f"{desired} review trigger is permitted", desired


def observation(
    gh: Gh,
    repo: str,
    prs: list[int],
    expected_heads: dict[int, str],
    scan_repos: list[str],
    now: float,
) -> dict[str, Any]:
    gh = ObservationCache(gh)  # type: ignore[assignment]
    wait = account_wait(gh, scan_repos, now)
    budget = account_allowance(gh, scan_repos, now)
    results = []
    for pr in prs:
        evidence = pull_evidence(gh, repo, pr)
        state, reason, next_mode, gate = classify_with_gate(
            evidence, expected_heads[pr], wait, now, budget
        )
        results.append(
            evidence
            | {
                "state": state,
                "reason": reason,
                "nextMode": next_mode,
                "retryAt": gate["retryAt"] if gate else None,
                "retrySource": gate["source"] if gate else None,
            }
        )
    return {
        "scanRepos": scan_repos,
        "accountWait": (
            {
                key: value
                for key, value in wait.items()
                if key not in {"retryAtEpoch", "updatedAtEpoch"}
            }
            if wait
            else None
        ),
        "allowance": {
            key: value
            for key, value in budget.items()
            if key not in {"exhaustionRetryEpoch", "runEpochs"}
        },
        "pullRequests": results,
    }


def aggregate_status(value: dict[str, Any]) -> tuple[str, str]:
    states = [item["state"] for item in value["pullRequests"]]
    if any(state == "invalidated" for state in states):
        return "invalidated", "at least one requested PR head changed"
    if all(state in {"review-complete", "clean-complete"} for state in states):
        return "satisfied", "every requested exact head has trustworthy completion evidence"
    return "pending", "at least one requested exact head still needs a review transition"


def acquire_lock(login: str, deadline: float, interval: float) -> TextIO | None:
    directory = Path(tempfile.gettempdir()) / "known-good-route-coderabbit"
    directory.mkdir(parents=True, exist_ok=True)
    path = directory / f"account-{stable_digest(login)[:16]}.lock"
    handle = path.open("a+")
    while True:
        try:
            fcntl.flock(handle.fileno(), fcntl.LOCK_EX | fcntl.LOCK_NB)
            handle.seek(0)
            handle.truncate()
            handle.write(f"{login} {time.time()}\n")
            handle.flush()
            return handle
        except BlockingIOError:
            if time.time() >= deadline:
                handle.close()
                return None
            time.sleep(min(interval, max(0.0, deadline - time.time())))


def run_review(
    gh: Gh,
    repo: str,
    pr: int,
    head: str,
    scan_repos: list[str],
    deadline: float,
    interval: float,
    *,
    clock: Callable[[], float] = time.time,
    sleeper: Callable[[float], None] = time.sleep,
) -> tuple[str, str, dict[str, Any]]:
    triggers: list[dict[str, Any]] = []
    last: dict[str, Any] = {}
    while True:
        try:
            last = observation(gh, repo, [pr], {pr: head}, scan_repos, clock())
        except (RateLimited, TransientError):
            gh.metrics.retries += 1
            if clock() >= deadline:
                return "pending", "GitHub transport remained unavailable until the deadline", last
            sleeper(min(interval, max(0.0, deadline - clock())))
            continue
        item = last["pullRequests"][0]
        state = item["state"]
        if state in {"review-complete", "clean-complete"}:
            last["triggers"] = triggers
            return "satisfied", item["reason"], last
        if state == "invalidated":
            last["triggers"] = triggers
            return "invalidated", item["reason"], last
        if state in {
            "pending-skipped",
            "pending-full-refused",
            "pending-full-unverified",
            "pending-check-required",
        }:
            last["triggers"] = triggers
            return "pending", item["reason"], last
        if state.startswith("trigger-"):
            if clock() >= deadline:
                last["triggers"] = triggers
                return "pending", "deadline reached before the permitted trigger", last
            mode = item["nextMode"]
            body = TRIGGERS[mode]
            created = gh.rest(
                f"repos/{repo}/issues/{pr}/comments", "POST", {"body": body}
            )
            triggers.append(
                {
                    "mode": mode,
                    "body": body,
                    "commentId": (created or {}).get("id"),
                }
            )
        if clock() >= deadline:
            last["triggers"] = triggers
            return "pending", "deadline reached before trustworthy review completion", last
        sleeper(min(interval, max(0.0, deadline - clock())))


def parser() -> argparse.ArgumentParser:
    result = argparse.ArgumentParser()
    subparsers = result.add_subparsers(dest="command", required=True)
    status = subparsers.add_parser("status")
    status.add_argument("--repo", required=True)
    status.add_argument("--pr", type=int, action="append", required=True)
    status.add_argument("--head", action="append", required=True, metavar="PR=SHA")
    status.add_argument("--scan-repo", action="append", default=[])
    status.add_argument("--json", action="store_true")
    run = subparsers.add_parser("run")
    run.add_argument("--repo", required=True)
    run.add_argument("--pr", type=int, required=True)
    run.add_argument("--head", required=True)
    run.add_argument("--scan-repo", action="append", default=[])
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


def main() -> int:
    args = parser().parse_args()
    metrics = Metrics(time.monotonic())
    identity: dict[str, Any] = {"repo": args.repo, "prs": getattr(args, "pr", None)}
    try:
        gh = Gh(metrics)
        scan_repos = list(dict.fromkeys([args.repo, *args.scan_repo]))
        if args.command == "status":
            prs = [positive_pr(value) for value in args.pr]
            if len(set(prs)) != len(prs):
                raise WaitError("--pr values must be unique")
            heads = parse_heads(args.head, prs)
            identity["heads"] = heads
            output_observation = observation(
                gh, args.repo, prs, heads, scan_repos, time.time()
            )
            metrics.observations += 1
            state, reason = aggregate_status(output_observation)
            output = result_envelope(
                "coderabbit", state, identity, output_observation, metrics, reason,
            )
        else:
            args.pr = positive_pr(args.pr)
            args.interval = positive_interval(args.interval)
            deadline = parse_time(args.deadline)
            identity = {"repo": args.repo, "prs": [args.pr], "heads": {args.pr: args.head}}
            account = gh.rest("user")
            login = str((account or {}).get("login") or "")
            if not login:
                raise WaitError("authenticated GitHub login is unavailable")
            lock = acquire_lock(login, deadline, args.interval)
            if lock is None:
                output = result_envelope(
                    "coderabbit", "pending", identity, {}, metrics,
                    "another CodeRabbit run held the account lock until the deadline",
                )
            else:
                try:
                    state, reason, output_observation = run_review(
                        gh, args.repo, args.pr, args.head, scan_repos,
                        deadline, args.interval,
                    )
                    metrics.observations += 1
                    output = result_envelope(
                        "coderabbit", state, identity, output_observation, metrics, reason
                    )
                finally:
                    fcntl.flock(lock.fileno(), fcntl.LOCK_UN)
                    lock.close()
        emit(output, args.json)
        return 0
    except WaitError as error:
        output = result_envelope(
            f"coderabbit-{args.command}", "operational-error", identity, {}, metrics, str(error)
        )
        emit(output, getattr(args, "json", False))
        return 2


if __name__ == "__main__":
    sys.exit(main())
