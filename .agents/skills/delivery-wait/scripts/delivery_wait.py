#!/usr/bin/env python3
"""Wait for a meaningful GitHub delivery transition without model polling."""

from __future__ import annotations

import argparse
import json
import sys
import time
from datetime import datetime, timezone
from pathlib import Path
from typing import Any
from urllib.parse import quote

sys.dont_write_bytecode = True  # Keep installed skill trees free of __pycache__.

from kgr_github import (
    Gh,
    Metrics,
    RateLimited,
    StateLock,
    TransientError,
    WaitError,
    default_state_path,
    emit,
    parse_time,
    positive_interval,
    result_envelope,
    stable_digest,
    wait_for_transition,
)


PR_QUERY = """
query($owner:String!,$name:String!,$number:Int!){
  repository(owner:$owner,name:$name){
    pullRequest(number:$number){
      headRefOid merged mergedAt mergeCommit{oid}
      commits(last:1){nodes{commit{statusCheckRollup{contexts(first:100){nodes{
        __typename ... on CheckRun{name status conclusion detailsUrl startedAt completedAt}
        ... on StatusContext{context state targetUrl createdAt}
      } pageInfo{hasNextPage}}}}}}
    }
  }
}
"""


TAG_QUERY = """
query($owner:String!,$name:String!,$qualified:String!,$tag:String!){
  repository(owner:$owner,name:$name){
    ref(qualifiedName:$qualified){target{oid ... on Tag{target{oid}}}}
    release(tagName:$tag){isDraft isPrerelease url releaseAssets(first:100){nodes{name} pageInfo{hasNextPage}}}
  }
}
"""


TERMINAL_CHECK_CONCLUSIONS = {
    "SUCCESS",
    "FAILURE",
    "ERROR",
    "CANCELLED",
    "SKIPPED",
    "NEUTRAL",
    "TIMED_OUT",
    "ACTION_REQUIRED",
}


def repo_parts(repo: str) -> tuple[str, str]:
    pieces = repo.split("/", 1)
    if len(pieces) != 2 or not all(pieces):
        raise WaitError("--repo must be OWNER/REPO")
    return pieces[0], pieces[1]


def pr_snapshot(gh: Gh, repo: str, number: int) -> dict[str, Any]:
    owner, name = repo_parts(repo)
    try:
        data = gh.graphql(PR_QUERY, {"owner": owner, "name": name, "number": number})
        pull = data.get("repository", {}).get("pullRequest")
        if not isinstance(pull, dict):
            raise WaitError(f"pull request {repo}#{number} was not found")
        contexts = (
            pull.get("commits", {}).get("nodes", [{}])[0]
            .get("commit", {}).get("statusCheckRollup") or {}
        )
        contexts = contexts.get("contexts") or {}
        if contexts.get("pageInfo", {}).get("hasNextPage"):
            raise WaitError("pull request has more than 100 check contexts; complete pagination is required")
        nodes = contexts.get("nodes", [])
        checks = []
        for node in nodes or []:
            if node.get("__typename") == "CheckRun":
                checks.append({"name": node.get("name"), "status": node.get("status"), "conclusion": node.get("conclusion"), "completedAt": node.get("completedAt"), "observedAt": node.get("startedAt") or node.get("completedAt")})
            else:
                checks.append({"name": node.get("context"), "status": "COMPLETED", "conclusion": node.get("state"), "observedAt": node.get("createdAt")})
        return {"head": pull.get("headRefOid"), "merged": bool(pull.get("merged")), "mergedAt": pull.get("mergedAt"), "mergeCommit": (pull.get("mergeCommit") or {}).get("oid"), "checks": sorted(checks, key=lambda item: str(item["name"]))}
    except RateLimited:
        gh.metrics.rate_limit_fallbacks += 1
        pull = gh.rest(f"repos/{repo}/pulls/{number}")
        run_pages = gh.rest_pages(
            f"repos/{repo}/commits/{pull['head']['sha']}/check-runs?per_page=100"
        )
        status_pages = gh.rest_pages(
            f"repos/{repo}/commits/{pull['head']['sha']}/statuses?per_page=100"
        )
        runs = [run for page in run_pages for run in page.get("check_runs", [])]
        statuses = [status for page in status_pages for status in page]
        checks = [
            {"name": run.get("name"), "status": str(run.get("status", "")).upper(), "conclusion": str(run.get("conclusion") or "").upper(), "completedAt": run.get("completed_at"), "observedAt": run.get("started_at") or run.get("completed_at")}
            for run in runs
        ] + [
            {"name": item.get("context"), "status": "COMPLETED", "conclusion": str(item.get("state", "")).upper(), "observedAt": item.get("created_at")}
            for item in statuses
        ]
        return {"head": pull["head"]["sha"], "merged": bool(pull.get("merged")), "mergedAt": pull.get("merged_at"), "mergeCommit": pull.get("merge_commit_sha"), "checks": sorted(checks, key=lambda item: str(item["name"]))}


def check_is_terminal(check: dict[str, Any]) -> bool:
    return (
        str(check.get("conclusion", "")).upper() in TERMINAL_CHECK_CONCLUSIONS
        and (
            str(check.get("status", "")).upper() == "COMPLETED"
            or check.get("completedAt") is not None
        )
    )


def classify_pr(
    kind: str,
    expected_head: str,
    expected_checks: set[str],
    observation: dict[str, Any],
) -> tuple[str, str]:
    if observation.get("head") != expected_head:
        return "invalidated", f"expected head {expected_head}, observed {observation.get('head')}"
    if kind == "pr-merged":
        return ("satisfied", "pull request merged") if observation.get("merged") else ("waiting", "pull request remains open")
    observed: dict[str, dict[str, Any]] = {}
    for check in observation.get("checks", []):
        name = str(check.get("name"))
        current = observed.get(name)
        if current is None or str(check.get("observedAt") or "") >= str(
            current.get("observedAt") or ""
        ):
            observed[name] = check
    missing = expected_checks - set(observed)
    if missing:
        return "waiting", f"expected checks have not appeared: {', '.join(sorted(missing))}"
    checks = [observed[name] for name in sorted(expected_checks)]
    if not checks or any(not check_is_terminal(check) for check in checks):
        return "waiting", "checks are not terminal"
    successful = {"SUCCESS", "NEUTRAL", "SKIPPED"}
    if any(str(check.get("conclusion", "")).upper() not in successful for check in checks):
        return "changed", "a check reached a non-success terminal result"
    return "satisfied", "all expected checks are terminal-success"


HEAD_QUERY = """
query($owner:String!,$name:String!,$number:Int!){
  repository(owner:$owner,name:$name){pullRequest(number:$number){headRefOid}}
}
"""


# GitHub Actions run and job conclusions that pass, matching the check-context
# convention in classify_pr. Every other terminal conclusion is a failure.
PASSING_WORKFLOW_CONCLUSIONS = {"success", "neutral", "skipped"}


def pr_head(gh: Gh, repo: str, number: int) -> Any:
    owner, name = repo_parts(repo)
    try:
        data = gh.graphql(HEAD_QUERY, {"owner": owner, "name": name, "number": number})
        pull = (data.get("repository") or {}).get("pullRequest")
        if not isinstance(pull, dict):
            raise WaitError(f"pull request {repo}#{number} was not found")
        return pull.get("headRefOid")
    except RateLimited:
        gh.metrics.rate_limit_fallbacks += 1
        return (gh.rest(f"repos/{repo}/pulls/{number}").get("head") or {}).get("sha")


def rest_census(pages: list[Any], field: str, label: str) -> list[dict[str, Any]]:
    """Join REST pages; never convert partial or racing data to absence."""
    items: list[dict[str, Any]] = []
    ids: set[int] = set()
    total = None
    for page in pages:
        if not isinstance(page, dict) or type(page.get("total_count")) is not int:
            raise WaitError(f"missing {label} total count")
        if not isinstance(page.get(field), list):
            raise WaitError(f"missing {label} list")
        if total is None:
            total = page["total_count"]
        elif page["total_count"] != total:
            raise TransientError(f"{label} count changed during pagination")
        for item in page[field]:
            if not isinstance(item, dict) or type(item.get("id")) is not int:
                raise WaitError(f"missing {label} identity")
            if item["id"] in ids:
                raise TransientError(f"duplicate {label} across pages")
            ids.add(item["id"])
            items.append(item)
    if total is None:
        raise WaitError(f"missing {label} pages")
    if len(items) != total:
        raise TransientError(f"incomplete {label} census: {len(items)} of {total}")
    return sorted(items, key=lambda item: item["id"])


def workflow_census(gh: Gh, repo: str, number: int, head: str, checks: bool) -> dict[str, Any]:
    """Every GitHub Actions run for the pull request's exact head, with its jobs."""
    snapshot = pr_snapshot(gh, repo, number) if checks else {"head": pr_head(gh, repo, number)}
    observed = snapshot.get("head")
    if not isinstance(observed, str) or not observed:
        raise WaitError(f"pull request {repo}#{number} has no verified head")
    observation: dict[str, Any] = {"head": observed}
    if checks:
        observation["checks"] = snapshot["checks"]
    if observed != head:
        return observation
    runs = rest_census(
        gh.rest_pages(f"repos/{repo}/actions/runs?head_sha={quote(head, safe='')}&per_page=100"),
        "workflow_runs",
        "workflow run",
    )
    observation["runs"] = []
    for run in runs:
        if run.get("head_sha") != head:
            raise WaitError(f"workflow run {run['id']} is for head {run.get('head_sha')}, not {head}")
        jobs = rest_census(
            gh.rest_pages(f"repos/{repo}/actions/runs/{run['id']}/jobs?filter=latest&per_page=100"),
            "jobs",
            f"workflow run {run['id']} job",
        )
        for job in jobs:
            if job.get("run_id") != run["id"]:
                raise WaitError(f"job {job['id']} belongs to run {job.get('run_id')}, not {run['id']}")
        observation["runs"].append({
            "id": run["id"],
            "name": run.get("name"),
            "event": run.get("event"),
            "attempt": run.get("run_attempt"),
            "status": run.get("status"),
            "conclusion": run.get("conclusion"),
            "url": run.get("html_url"),
            "jobs": [
                {
                    "id": job["id"],
                    "name": job.get("name"),
                    "status": job.get("status"),
                    "conclusion": job.get("conclusion"),
                    "completedAt": job.get("completed_at"),
                }
                for job in jobs
            ],
        })
    return observation


def workflow_failures(observation: dict[str, Any]) -> list[str]:
    failures = []
    for run in observation.get("runs", []):
        conclusion = run.get("conclusion")
        if run.get("status") == "completed" and conclusion and conclusion not in PASSING_WORKFLOW_CONCLUSIONS:
            failures.append(f"{run.get('name')}: {conclusion}")
        for job in run.get("jobs", []):
            conclusion = job.get("conclusion")
            terminal = job.get("status") == "completed" or job.get("completedAt") is not None
            if terminal and conclusion and conclusion not in PASSING_WORKFLOW_CONCLUSIONS:
                failures.append(f"{run.get('name')} / {job.get('name')}: {conclusion}")
    return sorted(failures)


class WorkflowCensusGate:
    """Classify a workflow census; success needs two identical observations.

    A job added by a later matrix or `needs` stage keeps its run in progress,
    and a run triggered after another run finishes appears only later, so one
    all-success snapshot cannot establish the complete census.
    """

    def __init__(self, head: str, checks: set[str], confirm: bool) -> None:
        self.head = head
        self.checks = checks
        self.confirm = confirm
        self.previous: str | None = None

    def failures(self, observation: dict[str, Any]) -> list[str]:
        failures = workflow_failures(observation)
        if self.checks and classify_pr("checks-terminal", self.head, self.checks, observation)[0] == "changed":
            failures += sorted(
                f"check {check.get('name')}: {str(check.get('conclusion')).lower()}"
                for check in observation.get("checks", [])
                if check.get("name") in self.checks and check_is_terminal(check)
                and str(check.get("conclusion", "")).upper() not in {"SUCCESS", "NEUTRAL", "SKIPPED"}
            )
        return failures

    def __call__(self, observation: dict[str, Any]) -> tuple[str, str]:
        if observation.get("head") != self.head:
            self.previous = None
            return "invalidated", f"expected head {self.head}, observed {observation.get('head')}"
        failures = self.failures(observation)
        if failures:
            self.previous = None
            return "changed", "a workflow run, job or check reached a non-success terminal result: " + "; ".join(failures)
        runs = observation.get("runs", [])
        jobs = [job for run in runs for job in run.get("jobs", [])]
        pending_runs = [run for run in runs if run.get("status") != "completed" or not run.get("conclusion")]
        pending_jobs = [
            job for job in jobs
            if not job.get("conclusion") or (job.get("status") != "completed" and job.get("completedAt") is None)
        ]
        reason = None
        if not runs:
            reason = "no workflow runs have appeared for the head"
        elif pending_runs or pending_jobs:
            reason = (
                f"{len(pending_runs)} of {len(runs)} workflow runs and "
                f"{len(pending_jobs)} of {len(jobs)} jobs are not terminal"
            )
        elif self.checks:
            state, check_reason = classify_pr("checks-terminal", self.head, self.checks, observation)
            if state != "satisfied":
                reason = check_reason
        if reason:
            self.previous = None
            return "waiting", reason
        summary = f"{len(runs)} workflow runs and {len(jobs)} jobs"
        if not self.confirm:
            return "waiting", f"all {summary} succeeded in this snapshot; only wait can confirm the census is stable"
        digest = stable_digest(observation)
        confirmed = digest == self.previous
        self.previous = digest
        if not confirmed:
            return "waiting", f"all {summary} succeeded; confirming the census is stable"
        return "satisfied", f"all {summary} on the head succeeded across two identical observations"


def workflow_snapshot(gh: Gh, repo: str, run_id: int) -> dict[str, Any]:
    run = gh.rest(f"repos/{repo}/actions/runs/{run_id}")
    return {"runId": run.get("id"), "head": run.get("head_sha"), "status": run.get("status"), "conclusion": run.get("conclusion"), "url": run.get("html_url")}


def tag_snapshot(gh: Gh, repo: str, tag: str) -> dict[str, Any]:
    owner, name = repo_parts(repo)
    try:
        data = gh.graphql(TAG_QUERY, {"owner": owner, "name": name, "qualified": f"refs/tags/{tag}", "tag": tag})
        repository = data.get("repository") or {}
        ref = repository.get("ref") or {}
        target = ref.get("target") or {}
        oid = (target.get("target") or {}).get("oid") or target.get("oid")
        release = repository.get("release") or None
        assets = (release or {}).get("releaseAssets") or {}
        if assets.get("pageInfo", {}).get("hasNextPage"):
            raise WaitError("release has more than 100 assets; complete pagination is required")
        return {"tag": tag, "target": oid, "release": None if release is None else {"draft": release.get("isDraft"), "prerelease": release.get("isPrerelease"), "url": release.get("url"), "assets": sorted(node.get("name") for node in assets.get("nodes", []))}}
    except RateLimited:
        gh.metrics.rate_limit_fallbacks += 1
        try:
            ref = gh.rest(f"repos/{repo}/git/ref/tags/{quote(tag, safe='')}")
            oid = ref.get("object", {}).get("sha")
            if ref.get("object", {}).get("type") == "tag" and oid:
                annotated = gh.rest(f"repos/{repo}/git/tags/{oid}")
                oid = annotated.get("object", {}).get("sha")
        except WaitError as error:
            if "404" in str(error) or "not found" in str(error).lower():
                oid = None
            else:
                raise
        try:
            release = gh.rest(f"repos/{repo}/releases/tags/{quote(tag, safe='')}")
        except WaitError as error:
            if "404" in str(error) or "not found" in str(error).lower():
                release = None
            else:
                raise
        return {"tag": tag, "target": oid, "release": None if release is None else {"draft": release.get("draft"), "prerelease": release.get("prerelease"), "url": release.get("html_url"), "assets": sorted(asset.get("name") for asset in release.get("assets", []))}}


def parser() -> argparse.ArgumentParser:
    result = argparse.ArgumentParser()
    commands = result.add_subparsers(dest="command", required=True)
    for command in ("inspect", "wait"):
        sub = commands.add_parser(command)
        kinds = ["checks-terminal", "pr-merged", "workflow-terminal", "tag-target", "release-assets"]
        if command == "wait":
            kinds.append("wake-at")
        sub.add_argument("kind", choices=kinds)
        sub.add_argument("--repo")
        sub.add_argument("--pr", type=int)
        sub.add_argument("--head")
        sub.add_argument("--run-id", type=int)
        sub.add_argument("--tag")
        sub.add_argument("--asset", action="append", default=[])
        sub.add_argument("--check", action="append", default=[])
        sub.add_argument("--all-workflows", action="store_true")
        sub.add_argument("--json", action="store_true")
        if command == "wait":
            sub.add_argument("--deadline", required=True)
            sub.add_argument("--interval", type=float, default=30.0)
            sub.add_argument("--state", type=Path)
    return result


def main() -> int:
    args = parser().parse_args()
    metrics = Metrics(time.monotonic())
    identity = {
        "repo": args.repo,
        "pr": args.pr,
        "head": args.head,
        "runId": args.run_id,
        "tag": args.tag,
        "checks": sorted(args.check),
        "assets": sorted(args.asset),
    }
    if args.all_workflows:
        # Added only when requested, so existing checkpoints keep their identity.
        identity["allWorkflows"] = True
    try:
        # wake-at observes nothing external, so it keeps no checkpoint.
        state_path = (
            None
            if args.command != "wait" or args.kind == "wake-at"
            else args.state or default_state_path(args.kind, identity)
        )
        if args.kind == "wake-at":
            args.interval = positive_interval(args.interval)
            deadline = parse_time(args.deadline)
            with StateLock(state_path):
                output = wait_for_transition(
                    kind=args.kind,
                    identity=identity,
                    observe=lambda: {"wakeAt": args.deadline},
                    classify=lambda _value: ("waiting", "wake time has not arrived"),
                    state_path=state_path,
                    deadline=deadline,
                    interval=args.interval,
                    metrics=metrics,
                    deadline_result=("satisfied", "wake time reached"),
                )
        else:
            if not args.repo:
                raise WaitError("--repo is required")
            gh = Gh(metrics)
            if args.all_workflows and args.kind != "checks-terminal":
                raise WaitError("--all-workflows applies only to checks-terminal")
            if args.kind in {"checks-terminal", "pr-merged"}:
                if not args.pr or not args.head:
                    raise WaitError("--pr and --head are required")
                if args.kind == "checks-terminal" and not args.check and not args.all_workflows:
                    raise WaitError(
                        "checks-terminal requires --all-workflows or at least one --check context"
                    )
                if args.all_workflows:
                    observe = lambda: workflow_census(gh, args.repo, args.pr, args.head, bool(args.check))
                    gate = WorkflowCensusGate(args.head, set(args.check), args.command == "wait")
                    classify = gate
                else:
                    observe = lambda: pr_snapshot(gh, args.repo, args.pr)
                    classify = lambda value: classify_pr(args.kind, args.head, set(args.check), value)
            elif args.kind == "workflow-terminal":
                if not args.run_id or not args.head:
                    raise WaitError("--run-id and --head are required")
                observe = lambda: workflow_snapshot(gh, args.repo, args.run_id)
                def classify(value: dict[str, Any]) -> tuple[str, str]:
                    if value.get("head") != args.head:
                        return "invalidated", f"expected head {args.head}, observed {value.get('head')}"
                    if value.get("status") != "completed":
                        return "waiting", "workflow is not terminal"
                    if value.get("conclusion") != "success":
                        return "changed", f"workflow concluded {value.get('conclusion')}"
                    return "satisfied", "workflow is terminal-success"
            else:
                if not args.tag:
                    raise WaitError("--tag is required")
                observe = lambda: tag_snapshot(gh, args.repo, args.tag)
                if args.kind == "tag-target":
                    if not args.head:
                        raise WaitError("--head is required")
                    classify = lambda value: (("waiting", "tag does not exist") if not value.get("target") else (("satisfied", "tag targets expected object") if value.get("target") == args.head else ("invalidated", f"tag targets {value.get('target')}")))
                else:
                    wanted = set(args.asset)
                    if not args.head:
                        raise WaitError("--head is required")
                    def classify(value: dict[str, Any]) -> tuple[str, str]:
                        if value.get("target") and value.get("target") != args.head:
                            return "invalidated", f"tag targets {value.get('target')}"
                        if not value.get("target"):
                            return "waiting", "tag does not exist"
                        if not value.get("release") or value["release"].get("draft"):
                            return "waiting", "release is not published"
                        if wanted.issubset(set(value["release"].get("assets", []))):
                            return "satisfied", "release assets are present"
                        return "waiting", "release assets are incomplete"

            if args.command == "inspect":
                observation = observe()
                metrics.observations += 1
                state, reason = classify(observation)
                output = result_envelope(args.kind, state, identity, observation, metrics, reason)
            else:
                def transition_key(value: dict[str, Any]) -> Any:
                    if args.all_workflows:
                        # The gate is stateful, so the key must not call it.
                        return {"head": value.get("head"), "failures": gate.failures(value)}
                    if args.kind == "checks-terminal":
                        terminal = classify(value)[0]
                        return {
                            "head": value.get("head"),
                            "nonSuccess": sorted(
                                check.get("name")
                                for check in value.get("checks", [])
                                if check_is_terminal(check)
                                and str(check.get("conclusion", "")).upper() != "SUCCESS"
                            ),
                            "terminal": terminal == "satisfied",
                        }
                    if args.kind == "pr-merged":
                        return {
                            "head": value.get("head"),
                            "merged": value.get("merged"),
                            "mergeCommit": value.get("mergeCommit") if value.get("merged") else None,
                        }
                    if args.kind == "workflow-terminal":
                        completed = value.get("status") == "completed"
                        return {
                            "head": value.get("head"),
                            "completed": completed,
                            "conclusion": value.get("conclusion") if completed else None,
                        }
                    return {"terminal": classify(value)[0] != "waiting"}
                deadline = parse_time(args.deadline)
                args.interval = positive_interval(args.interval)
                with StateLock(state_path):
                    output = wait_for_transition(kind=args.kind, identity=identity, observe=observe, classify=classify, state_path=state_path, deadline=deadline, interval=args.interval, metrics=metrics, transition_key=transition_key)
        emit(output, args.json)
        return 0
    except WaitError as error:
        output = result_envelope(args.kind, "operational-error", identity, {}, metrics, str(error))
        emit(output, args.json)
        return 2


if __name__ == "__main__":
    sys.exit(main())
