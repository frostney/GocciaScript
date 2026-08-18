#!/usr/bin/env python3
"""Inspect, wait for, reply to, and resolve GitHub review activity."""

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


REVIEW_QUERY = """
query($owner:String!,$name:String!,$number:Int!){
  repository(owner:$owner,name:$name){
    pullRequest(number:$number){
      headRefOid
      comments(first:100){nodes{id databaseId body createdAt author{login} authorAssociation} pageInfo{hasNextPage}}
      reviews(first:100){nodes{id databaseId author{login} authorAssociation state body submittedAt commit{oid}} pageInfo{hasNextPage}}
      reviewThreads(first:100){nodes{id isResolved comments(first:100){nodes{
        id databaseId body createdAt author{login} authorAssociation replyTo{id}
      } pageInfo{hasNextPage}}} pageInfo{hasNextPage}}
      commits(last:1){nodes{commit{statusCheckRollup{contexts(first:100){nodes{
        __typename
        ... on CheckRun{name status conclusion checkSuite{app{slug}}}
        ... on StatusContext{context state creator{login}}
      } pageInfo{hasNextPage}}}}}}
    }
  }
}
"""


RESOLVE_MUTATION = """
mutation($thread:ID!){resolveReviewThread(input:{threadId:$thread}){thread{id isResolved}}}
"""

THREAD_QUERY = """
query($thread:ID!){node(id:$thread){... on PullRequestReviewThread{id isResolved}}}
"""


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
) -> dict[str, Any]:
    owner, name = repo_parts(repo)
    data = gh.graphql(REVIEW_QUERY, {"owner": owner, "name": name, "number": number})
    pull = data.get("repository", {}).get("pullRequest")
    if not isinstance(pull, dict):
        raise WaitError(f"pull request {repo}#{number} was not found")
    reviews_connection = pull.get("reviews") or {}
    threads_connection = pull.get("reviewThreads") or {}
    comments_connection = pull.get("comments") or {}
    contexts = (
        pull.get("commits", {}).get("nodes", [{}])[0]
        .get("commit", {}).get("statusCheckRollup", {}).get("contexts") or {}
    )
    if reviews_connection.get("pageInfo", {}).get("hasNextPage"):
        raise WaitError("pull request has more than 100 reviews; complete pagination is required")
    if threads_connection.get("pageInfo", {}).get("hasNextPage"):
        raise WaitError("pull request has more than 100 review threads; complete pagination is required")
    if comments_connection.get("pageInfo", {}).get("hasNextPage"):
        raise WaitError("pull request has more than 100 top-level comments; complete pagination is required")
    if contexts.get("pageInfo", {}).get("hasNextPage"):
        raise WaitError("pull request has more than 100 check contexts; complete pagination is required")

    threads = []
    unanswered = 0
    unresolved = 0
    actor_sets = {
        item["id"]: {normalize_login(actor) for actor in item.get("actors", [])}
        for item in policy["automations"]
    }
    all_actors = set().union(*actor_sets.values()) if actor_sets else set()
    for thread in threads_connection.get("nodes") or []:
        thread_comments_connection = thread.get("comments") or {}
        if thread_comments_connection.get("pageInfo", {}).get("hasNextPage"):
            raise WaitError(f"review thread {thread.get('id')} has more than 100 comments")
        comments = thread_comments_connection.get("nodes") or []
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
            "automation": bool(automation_comments),
            "automationIds": automation_ids,
            "maintainerReply": has_maintainer_reply,
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

    head = pull.get("headRefOid")
    reviews = reviews_connection.get("nodes") or []
    checks = contexts.get("nodes") or []
    automation_states = []
    for automation in policy["automations"]:
        actors = actor_sets[automation["id"]]
        contexts_wanted = set(automation.get("check_contexts", []))
        apps_wanted = {normalize_login(value) for value in automation.get("check_app_slugs", [])}
        terminal_conclusions = {str(value).lower() for value in automation.get("terminal_check_conclusions", [])}
        terminal_reviews = {str(value).upper() for value in automation.get("terminal_review_states", [])}
        markers = [str(value).lower() for value in automation.get("nonterminal_review_markers", [])]
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
                status = "COMPLETED"
            if check_name in contexts_wanted and (not apps_wanted or app in apps_wanted):
                matching_checks.append({"name": check_name, "app": app, "status": status, "conclusion": conclusion})
        matching_reviews = [
            review for review in reviews
            if normalize_login((review.get("author") or {}).get("login")) in actors
            and (review.get("commit") or {}).get("oid") == head
        ]
        review_terminal = any(
            str(review.get("state") or "").upper() in terminal_reviews
            and not any(marker in str(review.get("body") or "").lower() for marker in markers)
            for review in matching_reviews
        )
        check_terminal = any(
            check["status"] == "COMPLETED" and check["conclusion"] in terminal_conclusions
            for check in matching_checks
        )
        automation_states.append({
            "id": automation["id"],
            "terminal": check_terminal or review_terminal,
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
    for comment in comments_connection.get("nodes") or []:
        author = normalize_login((comment.get("author") or {}).get("login"))
        if author not in all_actors:
            continue
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
        if thread["resolved"] and (
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
    for automation in automation_states:
        for review in automation["reviews"]:
            if not review["hasBody"]:
                continue
            finding_surfaces.append({
                "kind": "review",
                "id": review["nodeId"],
                "headBinding": "exact-head",
                "automationIds": [automation["id"]],
                "review": review,
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
    return {
        "head": head,
        "automations": automation_states,
        "unresolvedThreads": unresolved,
        "unansweredAutomationThreads": unanswered,
        "findingSurfaceCount": len(finding_surfaces),
        "findingSurfaces": finding_surfaces,
        "threads": threads,
        "topLevelAutomationComments": top_level,
    }


def classify(expected_head: str, observation: dict[str, Any]) -> tuple[str, str]:
    if observation.get("head") != expected_head:
        return "invalidated", f"expected head {expected_head}, observed {observation.get('head')}"
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
        return "satisfied", "review convergence reached"
    return "waiting", "review convergence is pending"


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
            }
        )
    return {
        "head": observation.get("head"),
        "automations": automations,
        "threads": observation.get("threads", []),
        "topLevelAutomationComments": observation.get(
            "topLevelAutomationComments", []
        ),
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
            policy = load_policy(args.policy)
            identity["policyDigest"] = stable_digest(policy)
            observe = lambda: review_snapshot(
                gh, args.repo, args.pr, policy, include_bodies=args.command == "inspect"
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
            if not args.operation_id.replace("-", "").replace("_", "").replace(".", "").replace(":", "").isalnum():
                raise WaitError("--operation-id may contain letters, digits, dot, colon, dash, and underscore")
            owner, name = repo_parts(args.repo)
            head_data = gh.graphql(
                "query($owner:String!,$name:String!,$number:Int!){repository(owner:$owner,name:$name){pullRequest(number:$number){headRefOid}}}",
                {"owner": owner, "name": name, "number": args.pr},
            )
            observed_head = head_data.get("repository", {}).get("pullRequest", {}).get("headRefOid")
            if observed_head != args.head:
                output = result_envelope("review-reply", "invalidated", identity, {"head": observed_head}, metrics, f"expected head {args.head}, observed {observed_head}")
                emit(output, args.json)
                return 0
            marker = f"<!-- known-good-route-operation:{args.operation_id} -->"
            pages = gh.rest_pages(f"repos/{args.repo}/pulls/{args.pr}/comments?per_page=100")
            comments = [item for page in pages for item in page]
            existing = next((item for item in comments if marker in str(item.get("body") or "")), None)
            if existing:
                observation = {"commentId": existing.get("id"), "operationId": args.operation_id, "created": False}
            else:
                created = gh.rest(f"repos/{args.repo}/pulls/{args.pr}/comments/{args.comment_id}/replies", "POST", {"body": f"{args.body}\n\n{marker}"})
                observation = {"commentId": created.get("id"), "operationId": args.operation_id, "created": True}
            output = result_envelope("review-reply", "satisfied", identity, observation, metrics, "inline reply present")
        else:
            owner, name = repo_parts(args.repo)
            head_data = gh.graphql(
                "query($owner:String!,$name:String!,$number:Int!){repository(owner:$owner,name:$name){pullRequest(number:$number){headRefOid}}}",
                {"owner": owner, "name": name, "number": args.pr},
            )
            observed_head = head_data.get("repository", {}).get("pullRequest", {}).get("headRefOid")
            if observed_head != args.head:
                output = result_envelope("review-resolve", "invalidated", identity, {"head": observed_head}, metrics, f"expected head {args.head}, observed {observed_head}")
                emit(output, args.json)
                return 0
            existing = gh.graphql(THREAD_QUERY, {"thread": args.thread_id}).get("node") or {}
            if existing.get("isResolved"):
                thread = existing
            else:
                data = gh.graphql(RESOLVE_MUTATION, {"thread": args.thread_id})
                thread = data.get("resolveReviewThread", {}).get("thread") or {}
            observation = {"threadId": thread.get("id"), "resolved": thread.get("isResolved")}
            if not observation["resolved"]:
                raise WaitError(f"thread {args.thread_id} was not resolved")
            output = result_envelope("review-resolve", "satisfied", identity, observation, metrics, "thread resolved")
        emit(output, args.json)
        return 0
    except WaitError as error:
        output = result_envelope(f"review-{args.command}", "operational-error", identity, {}, metrics, str(error))
        emit(output, getattr(args, "json", False))
        return 2


if __name__ == "__main__":
    sys.exit(main())
