#!/usr/bin/env python3
"""Inspect, wait for, reply to, and resolve pull-request feedback."""

from __future__ import annotations

import argparse
import json
import sys
import time
from pathlib import Path
from typing import Any

sys.path.insert(0, str(Path(__file__).resolve().parents[2] / "delivery-wait" / "scripts"))

from kgr_github import (  # noqa: E402
    Gh,
    Metrics,
    StateLock,
    WaitError,
    default_state_path,
    emit,
    parse_time,
    positive_interval,
    result_envelope,
    stable_digest,
    wait_for_transition,
)
from review_observation import review_census, terminal_evidence  # noqa: E402
from review_mutations import reply as reply_feedback, resolve as resolve_feedback  # noqa: E402


def repo_parts(repo: str) -> tuple[str, str]:
    pieces = repo.split("/", 1)
    if len(pieces) != 2 or not all(pieces):
        raise WaitError("--repo must be OWNER/REPO")
    return pieces[0], pieces[1]


def load_policy(path: Path) -> dict[str, Any]:
    try:
        policy = json.loads(path.read_text())
    except (OSError, json.JSONDecodeError) as error:
        raise WaitError(f"cannot read review policy {path}: {error}") from error
    if not isinstance(policy, dict):
        raise WaitError(f"review policy {path} must be an object")
    automations = policy.get("automations")
    if not isinstance(automations, list):
        raise WaitError(f"review policy {path} needs an automations array")
    for automation in automations:
        if not isinstance(automation, dict) or not isinstance(automation.get("id"), str):
            raise WaitError(f"review policy {path} has an invalid automation")
        if "check_contexts" not in automation and automation.get("check_context"):
            automation["check_contexts"] = [automation["check_context"]]
        for key in (
            "actors",
            "check_contexts",
            "check_app_slugs",
            "terminal_check_conclusions",
            "terminal_review_states",
            "nonterminal_review_markers",
        ):
            values = automation.get(key, [])
            if not isinstance(values, list) or not all(
                isinstance(value, str) for value in values
            ):
                raise WaitError(
                    f"review policy {path} automation {automation['id']} has an invalid {key}"
                )
        if not (
            automation.get("check_contexts")
            or automation.get("terminal_review_states")
        ):
            raise WaitError(
                f"review policy {path} automation {automation['id']} has no terminal evidence"
            )
        if automation.get("check_contexts") and not automation.get(
            "terminal_check_conclusions"
        ):
            automation["terminal_check_conclusions"] = ["success", "neutral"]
    return policy


def normalize_login(value: Any) -> str:
    return str(value or "").lower()


def review_snapshot(
    gh: Gh,
    repo: str,
    number: int,
    policy: dict[str, Any],
    include_bodies: bool = False,
    page_size: int = 100,
) -> dict[str, Any]:
    policy_available = not policy.get("unavailableReason")
    owner, name = repo_parts(repo)
    census = review_census(gh, {"owner": owner, "name": name, "number": number}, page_size)

    threads = []
    unanswered = 0
    unresolved = 0
    actor_sets = {
        item["id"]: {normalize_login(actor) for actor in item.get("actors", [])}
        for item in policy["automations"]
    }
    all_actors = set().union(*actor_sets.values()) if actor_sets else set()
    for thread in census["reviewThreads"]:
        comments = thread["comments"]
        is_resolved = bool(thread.get("isResolved"))
        if not is_resolved:
            unresolved += 1
        automation_comments = [
            comment for comment in comments
            if normalize_login((comment.get("author") or {}).get("login")) in all_actors
        ]
        automation_ids = sorted(
            automation_id
            for automation_id, actors in actor_sets.items()
            if any(
                normalize_login((comment.get("author") or {}).get("login")) in actors
                for comment in comments
            )
        )
        unanswered_comments = [
            finding for finding in automation_comments
            if not any(
                comment.get("authorAssociation") in {"OWNER", "MEMBER", "COLLABORATOR"}
                and normalize_login((comment.get("author") or {}).get("login")) not in all_actors
                and str(comment.get("createdAt") or "") > str(finding.get("createdAt") or "")
                for comment in comments
            )
        ]
        has_maintainer_reply = bool(automation_comments) and not unanswered_comments
        if unanswered_comments:
            unanswered += 1
        threads.append({
            "id": thread.get("id"),
            "resolved": is_resolved,
            "automation": bool(automation_comments) if policy_available else None,
            "automationIds": automation_ids,
            "maintainerReply": has_maintainer_reply if policy_available else None,
            "comments": [
                ({
                    "id": comment.get("databaseId"),
                    "nodeId": comment.get("id"),
                    "author": (comment.get("author") or {}).get("login"),
                    "association": comment.get("authorAssociation"),
                    "createdAt": comment.get("createdAt"),
                    "reply": comment.get("replyTo") is not None,
                } | (
                    {"body": comment.get("body")}
                    if include_bodies
                    else {"bodyDigest": stable_digest(str(comment.get("body") or ""))}
                ))
                for comment in comments
            ],
        })

    head = census["headRefOid"]
    reviews = census["reviews"]
    checks = census["contexts"]
    automation_states = []
    for automation in policy["automations"]:
        actors = actor_sets[automation["id"]]
        contexts_wanted = set(automation.get("check_contexts", []))
        apps_wanted = {normalize_login(value) for value in automation.get("check_app_slugs", [])}
        matching_checks = []
        for check in checks:
            if check.get("__typename") == "CheckRun":
                check_name = check.get("name")
                app = normalize_login(
                    ((check.get("checkSuite") or {}).get("app") or {}).get("slug")
                )
                conclusion = str(check.get("conclusion") or "").lower()
                status = str(check.get("status") or "").upper()
            else:
                check_name = check.get("context")
                app = normalize_login((check.get("creator") or {}).get("login"))
                conclusion = str(check.get("state") or "").lower()
                status = "PENDING" if conclusion in {"pending", "expected"} else "COMPLETED"
            if check_name in contexts_wanted and (not apps_wanted or app in apps_wanted):
                matching_checks.append({"id": check.get("id"), "source": check.get("__typename"), "name": check_name, "app": app,
                    "status": status, "conclusion": conclusion,
                    "startedAt": check.get("startedAt") if check.get("__typename") == "CheckRun" else check.get("createdAt"),
                    "completedAt": check.get("completedAt") if check.get("__typename") == "CheckRun" else check.get("createdAt")})
        matching_reviews = [
            review for review in reviews
            if normalize_login((review.get("author") or {}).get("login")) in actors
            and (review.get("commit") or {}).get("oid") == head
        ]
        automation_states.append({
            "id": automation["id"],
            **terminal_evidence(matching_checks, matching_reviews, automation),
            "checks": matching_checks,
            "reviews": [
                ({
                    "id": review.get("databaseId"),
                    "nodeId": review.get("id"),
                    "author": (review.get("author") or {}).get("login"),
                    "state": review.get("state"),
                    "submittedAt": review.get("submittedAt"),
                    "hasBody": bool(str(review.get("body") or "").strip()),
                }
                | (
                    {"body": review.get("body")}
                    if include_bodies
                    else {"bodyDigest": stable_digest(str(review.get("body") or ""))}
                ))
                for review in matching_reviews
            ],
        })
    top_level = []
    for comment in census["comments"]:
        author = normalize_login((comment.get("author") or {}).get("login"))
        item = {
            "id": comment.get("databaseId"),
            "nodeId": comment.get("id"),
            "author": (comment.get("author") or {}).get("login"),
            "createdAt": comment.get("createdAt"),
            "automationIds": sorted(
                automation_id
                for automation_id, actors in actor_sets.items()
                if author in actors
            ),
            "hasBody": bool(str(comment.get("body") or "").strip()),
        }
        if include_bodies:
            item["body"] = comment.get("body")
        else:
            item["bodyDigest"] = stable_digest(str(comment.get("body") or ""))
        top_level.append(item)

    finding_surfaces = []
    for thread in threads:
        if policy_available and thread["resolved"] and (
            not thread["automation"] or thread["maintainerReply"]
        ):
            continue
        finding_surfaces.append({
            "kind": "inline-thread",
            "id": thread["id"],
            "headBinding": "current-thread-state",
            "automationIds": thread["automationIds"],
            "resolved": thread["resolved"],
            "maintainerReply": thread["maintainerReply"],
            "comments": thread["comments"],
        })
    for comment in top_level:
        if not comment["hasBody"]:
            continue
        finding_surfaces.append({
            "kind": "top-level-comment",
            "id": comment["nodeId"],
            "headBinding": "pull-request",
            "automationIds": comment["automationIds"],
            "comment": comment,
        })
    # Review bodies belong to the feedback census even when their author is not
    # registered as an automation. Do not drop a human's exact-head finding.
    for review in reviews:
        if ((review.get("commit") or {}).get("oid") != head
                or (not str(review.get("body") or "").strip()
                    and review.get("state") != "CHANGES_REQUESTED")):
            continue
        item = {
            "id": review.get("databaseId"), "nodeId": review.get("id"),
            "author": (review.get("author") or {}).get("login"),
            "state": review.get("state"), "submittedAt": review.get("submittedAt"),
            "hasBody": bool(str(review.get("body") or "").strip()),
        }
        item.update({"body": review.get("body")} if include_bodies else
                    {"bodyDigest": stable_digest(str(review.get("body") or ""))})
        finding_surfaces.append({
            "kind": "review", "id": review.get("id"), "headBinding": "exact-head",
            "automationIds": sorted(
                automation_id for automation_id, actors in actor_sets.items()
                if normalize_login((review.get("author") or {}).get("login")) in actors
            ),
            "review": item,
        })
    return {
        "head": head,
        "policyAvailable": policy_available,
        "policyError": policy.get("unavailableReason"),
        "automations": automation_states,
        "checks": checks,
        "unresolvedThreads": unresolved,
        "unansweredAutomationThreads": unanswered if policy_available else None,
        "findingSurfaceCount": len(finding_surfaces),
        "findingSurfaces": finding_surfaces,
        "threads": threads,
        "topLevelAutomationComments": [item for item in top_level if item["automationIds"]],
        "unclassifiedTopLevelComments": [item for item in top_level if not item["automationIds"]],
    }


def classify(expected_head: str, observation: dict[str, Any]) -> tuple[str, str]:
    if observation.get("head") != expected_head:
        return "invalidated", f"expected head {expected_head}, observed {observation.get('head')}"
    if observation.get("policyAvailable") is False:
        return "waiting", "current feedback was inspected but review policy is unavailable; completion is unverified"
    automations_terminal = all(
        item.get("terminal") for item in observation.get("automations", [])
    )
    if automations_terminal and observation.get("findingSurfaceCount", 0) > 0:
        return (
            "judgment-required",
            "automation completed; finding surfaces require exact-head classification",
        )
    if (
        automations_terminal
        and observation.get("unresolvedThreads") == 0
        and observation.get("unansweredAutomationThreads") == 0
    ):
        return "satisfied", "review gates satisfied"
    return "waiting", "review gates are pending"


def review_transition_key(observation: dict[str, Any]) -> dict[str, Any]:
    """Ignore polling progress while retaining evidence that needs judgment."""
    automations = []
    for automation in observation.get("automations", []):
        completed_checks = [
            check
            for check in automation.get("checks", [])
            if check.get("status") == "COMPLETED"
        ]
        automations.append(
            {
                "id": automation.get("id"),
                "terminal": automation.get("terminal"),
                "completedChecks": completed_checks,
                "reviews": automation.get("reviews", []),
                "latestEvidence": automation.get("latestEvidence", []),
            }
        )
    return {
        "head": observation.get("head"),
        "policyAvailable": observation.get("policyAvailable"),
        "automations": automations,
        "findingSurfaces": observation.get("findingSurfaces", []),
        "threads": observation.get("threads", []),
        "topLevelAutomationComments": observation.get(
            "topLevelAutomationComments", []
        ),
        "unclassifiedTopLevelComments": observation.get("unclassifiedTopLevelComments", []),
    }


def base_parser() -> argparse.ArgumentParser:
    result = argparse.ArgumentParser()
    subparsers = result.add_subparsers(dest="command", required=True)
    for name in ("inspect", "wait"):
        sub = subparsers.add_parser(name)
        sub.add_argument("--repo", required=True)
        sub.add_argument("--pr", type=int, required=True)
        sub.add_argument("--head", required=True)
        sub.add_argument("--policy", type=Path, default=Path(".github/delivery/review-automations.json"))
        sub.add_argument("--json", action="store_true")
        sub.add_argument("--page-size", type=int, choices=range(1, 101), default=100, metavar="1..100")
        if name == "wait":
            sub.add_argument("--deadline", required=True)
            sub.add_argument("--interval", type=float, default=30.0)
            sub.add_argument("--state", type=Path)
    reply = subparsers.add_parser("reply")
    reply.add_argument("--repo", required=True)
    reply.add_argument("--pr", type=int, required=True)
    reply.add_argument("--head", required=True)
    reply.add_argument("--comment-id", type=int, required=True)
    reply.add_argument("--body", required=True)
    reply.add_argument("--operation-id", required=True)
    reply.add_argument("--json", action="store_true")
    reply.add_argument("--state", type=Path)
    resolve = subparsers.add_parser("resolve")
    resolve.add_argument("--repo", required=True)
    resolve.add_argument("--pr", type=int, required=True)
    resolve.add_argument("--head", required=True)
    resolve.add_argument("--thread-id", required=True)
    resolve.add_argument("--json", action="store_true")
    return result


def main() -> int:
    args = base_parser().parse_args()
    metrics = Metrics(time.monotonic())
    identity = {"repo": args.repo, "pr": getattr(args, "pr", None), "head": getattr(args, "head", None)}
    try:
        gh = Gh(metrics)
        if args.command in {"inspect", "wait"}:
            try:
                policy = load_policy(args.policy)
            except WaitError as error:
                if args.command != "inspect":
                    raise
                policy = {"automations": [], "unavailableReason": str(error)}
            identity["policyDigest"] = stable_digest(policy)
            observe = lambda: review_snapshot(
                gh, args.repo, args.pr, policy, include_bodies=args.command == "inspect", page_size=args.page_size
            )
            if args.command == "inspect":
                observation = observe()
                metrics.observations += 1
                state, reason = classify(args.head, observation)
                output = result_envelope("review", state, identity, observation, metrics, reason)
            else:
                args.interval = positive_interval(args.interval)
                state_path = args.state or default_state_path("review", identity)
                with StateLock(state_path):
                    output = wait_for_transition(
                        kind="review",
                        identity=identity,
                        observe=observe,
                        classify=lambda value: classify(args.head, value),
                        state_path=state_path,
                        deadline=parse_time(args.deadline),
                        interval=args.interval,
                        metrics=metrics,
                        transition_key=review_transition_key,
                        change_precedes_terminal=True,
                    )
        elif args.command == "reply":
            repo_parts(args.repo)
            state_path = args.state or default_state_path("review-reply", {
                "repo": args.repo, "pr": args.pr, "operationId": args.operation_id,
            })
            mutation = reply_feedback(gh, args.repo, args.pr, args.head, args.comment_id,
                                      args.body, args.operation_id, state_path)
            output = result_envelope("review-reply", mutation["state"], identity,
                                     mutation["observation"], metrics, mutation["reason"])
        else:
            repo_parts(args.repo)
            mutation = resolve_feedback(gh, args.repo, args.pr, args.head, args.thread_id)
            output = result_envelope("review-resolve", mutation["state"], identity,
                                     mutation["observation"], metrics, mutation["reason"])
        emit(output, args.json)
        return 0
    except WaitError as error:
        output = result_envelope(f"review-{args.command}", "operational-error", identity, {}, metrics, str(error))
        emit(output, getattr(args, "json", False))
        return 2


if __name__ == "__main__":
    sys.exit(main())
