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
    r"available in\D{0,10}(\d+)\s*minutes?(?:\D{0,10}(\d+)\s*seconds?)?",
    re.IGNORECASE,
)
FINISHED = re.compile(r"review finished|reviews? (?:is|are) complete|no actionable comments", re.IGNORECASE)
SKIPPED = re.compile(r"review skipped", re.IGNORECASE)
RATE_LIMITED = re.compile(r"rate limited|review limit", re.IGNORECASE)
ALREADY_REVIEWED = re.compile(
    r"action not completed[\s\S]{0,300}?already reviewed", re.IGNORECASE
)
ACTIONABLE = re.compile(r"Actionable comments posted:\s*(\d+)", re.IGNORECASE)
WAIT_BUFFER_SECONDS = 60
TRUSTED_ACK_SECONDS = 30



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


def coderabbit_check_for_head(gh: Gh, repo: str, head: str) -> dict[str, Any]:
    """Resolve head-scoped CodeRabbit check-run or commit-status SUCCESS.

    A completed SUCCESS conclusion whose output title/summary/text (or commit-
    status description) matches RATE_LIMITED is not treated as SUCCESS — it
    surfaces rateLimited so callers do not count a rate-limited pass as
    codeRabbitCheckSuccess.
    """
    run_pages = gh.rest_pages(f"repos/{repo}/commits/{head}/check-runs?per_page=100")
    for page in run_pages:
        runs = page.get("check_runs", []) if isinstance(page, dict) else []
        for run in runs:
            if not isinstance(run, dict):
                continue
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
                return {
                    "state": "SUCCESS",
                    "source": "check-run",
                    "name": name,
                    "head": head,
                }

    statuses = rest_items(gh, f"repos/{repo}/commits/{head}/statuses?per_page=100")
    seen_contexts: set[str] = set()
    for status in statuses:
        context = str(status.get("context") or "")
        if not is_coderabbit_check_name(context) or context in seen_contexts:
            continue
        seen_contexts.add(context)
        state = str(status.get("state") or "").upper()
        description = str(status.get("description") or "")
        if state == "SUCCESS" and check_text_rate_limited(description):
            return {
                "state": "RATE_LIMITED",
                "source": "status",
                "name": context,
                "head": head,
                "rateLimited": True,
            }
        return {
            "state": state or None,
            "source": "status",
            "name": context,
            "head": head,
        }

    return {"state": None, "source": None, "name": None, "head": head}

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


def account_wait(gh: Gh, repos: list[str]) -> dict[str, Any] | None:
    candidates: list[dict[str, Any]] = []
    for repo in repos:
        repo_parts(repo)
        comments = rest_items(
            gh,
            f"repos/{repo}/issues/comments?sort=updated&direction=desc&per_page=100",
        )
        for comment in comments:
            if not is_bot(comment):
                continue
            match = STATED_WAIT.search(str(comment.get("body") or ""))
            if not match:
                continue
            updated_at = parse_timestamp(comment.get("updated_at"), "wait updated_at")
            seconds = int(match.group(1)) * 60 + int(match.group(2) or 0)
            issue_url = str(comment.get("issue_url") or "")
            candidates.append(
                {
                    "repo": repo,
                    "pr": int(issue_url.rsplit("/", 1)[-1]) if issue_url.rsplit("/", 1)[-1].isdigit() else None,
                    "updatedAt": comment["updated_at"],
                    "updatedAtEpoch": updated_at,
                    "statedSeconds": seconds,
                    "retryAt": format_timestamp(updated_at + seconds + WAIT_BUFFER_SECONDS),
                    "retryAtEpoch": updated_at + seconds + WAIT_BUFFER_SECONDS,
                }
            )
    return max(candidates, key=lambda item: item["updatedAt"], default=None)


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
    pushed_at = (
        ((commit or {}).get("commit") or {}).get("committer") or {}
    ).get("date")
    pushed_epoch = parse_timestamp(pushed_at, "head commit time")

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
        match = ACTIONABLE.search(str(review.get("body") or ""))
        if (
            is_bot(review)
            and review.get("state") != "PENDING"
            and review.get("commit_id") == head
            and match
        ):
            exact_reviews.append(
                {
                    "id": review.get("id"),
                    "submittedAt": review.get("submitted_at"),
                    "actionable": int(match.group(1)),
                }
            )
    exact_review = latest(exact_reviews, "submittedAt")

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

    return {
        "repo": repo,
        "pr": pr,
        "head": head,
        "headPushedAt": pushed_at,
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
) -> tuple[str, str, str | None]:
    if evidence["head"] != expected_head:
        return "invalidated", f"expected head {expected_head}, observed {evidence['head']}", None
    if evidence["exactHeadReview"]:
        return "review-complete", "exact-head actionable review object observed", None
    ack = evidence["finishedAck"]

    # Rate-limit evidence must win over a SUCCESS check short-circuit: a green
    # CodeRabbit check whose description is "Review rate limited" is pending,
    # not clean-complete.
    if evidence["rateLimited"]:
        rate_limited_at = evidence.get("rateLimitedAtEpoch")
        if (
            wait is None
            or rate_limited_at is None
            or wait["updatedAtEpoch"] < rate_limited_at
        ):
            return "pending-retry-source", "rate limited without a stated retry time", None
        desired = evidence["trigger"]["mode"] if evidence["trigger"] else "incremental"
        if wait["retryAtEpoch"] > now:
            return "waiting", "account-scoped stated wait has not elapsed", desired
        return f"trigger-{desired}", f"{desired} review trigger is permitted", desired

    code_rabbit_ok = bool(evidence.get("codeRabbitCheckSuccess")) or (
        (evidence.get("codeRabbitCheck") or {}).get("state") == "SUCCESS"
    )
    if (
        ack
        and evidence["coverage"]["verified"]
        and (ack["latencySeconds"] >= TRUSTED_ACK_SECONDS or code_rabbit_ok)
    ):
        if code_rabbit_ok and ack["latencySeconds"] < TRUSTED_ACK_SECONDS:
            reason = (
                "finished acknowledgment has current walkthrough coverage "
                "and head-scoped CodeRabbit check SUCCESS"
            )
        else:
            reason = "finished acknowledgment has current walkthrough coverage"
        return "clean-complete", reason, None
    if evidence["skippedAck"]:
        return "pending-skipped", "CodeRabbit reported that review was skipped", None

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

    if wait is not None and wait["retryAtEpoch"] > now:
        return "waiting", "account-scoped stated wait gates the next trigger", desired
    return f"trigger-{desired}", f"{desired} review trigger is permitted", desired


def observation(
    gh: Gh,
    repo: str,
    prs: list[int],
    expected_heads: dict[int, str],
    scan_repos: list[str],
    now: float,
) -> dict[str, Any]:
    wait = account_wait(gh, scan_repos)
    results = []
    for pr in prs:
        evidence = pull_evidence(gh, repo, pr)
        state, reason, next_mode = classify(evidence, expected_heads[pr], wait, now)
        results.append(
            evidence
            | {
                "state": state,
                "reason": reason,
                "nextMode": next_mode,
                "retryAt": wait["retryAt"] if state == "waiting" and wait else None,
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
