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
    load_state,
    parse_time,
    positive_interval,
    result_envelope,
    stable_digest,
    wait_for_transition,
    write_state,
)


PR_QUERY = """
query($owner:String!,$name:String!,$number:Int!){
  repository(owner:$owner,name:$name){
    pullRequest(number:$number){
      headRefOid state merged mergedAt mergeCommit{oid}
      commits(last:1){nodes{commit{statusCheckRollup{contexts(first:100){nodes{
        __typename ... on CheckRun{name status conclusion detailsUrl startedAt completedAt databaseId
          checkSuite{databaseId app{slug} workflowRun{event workflow{databaseId}}}}
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
    "STARTUP_FAILURE",
    "STALE",
}
PASSING_CHECK_CONCLUSIONS = {"SUCCESS", "NEUTRAL", "SKIPPED"}


def repo_parts(repo: str) -> tuple[str, str]:
    pieces = repo.split("/", 1)
    if len(pieces) != 2 or not all(pieces):
        raise WaitError("--repo must be OWNER/REPO")
    return pieces[0], pieces[1]


def pr_snapshot(gh: Gh, repo: str, number: int, rest_page_size: int = 100) -> dict[str, Any]:
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
                suite = node.get("checkSuite") or {}
                workflow_run = suite.get("workflowRun") or {}
                source = check_source(
                    (suite.get("app") or {}).get("slug"),
                    (workflow_run.get("workflow") or {}).get("databaseId"),
                    workflow_run.get("event"),
                )
                checks.append({"name": node.get("name"), "status": node.get("status"), "conclusion": node.get("conclusion"), "completedAt": node.get("completedAt"), "observedAt": node.get("startedAt") or node.get("completedAt"), "suite": suite.get("databaseId"), "source": source})
            else:
                checks.append({"name": node.get("context"), "status": "COMPLETED", "conclusion": node.get("state"), "observedAt": node.get("createdAt"), "source": "status"})
        return {"head": pull.get("headRefOid"), "state": pull.get("state"), "merged": bool(pull.get("merged")), "mergedAt": pull.get("mergedAt"), "mergeCommit": (pull.get("mergeCommit") or {}).get("oid"), "checks": sorted(checks, key=lambda item: str(item["name"]))}
    except RateLimited:
        gh.metrics.rate_limit_fallbacks += 1
        pull = gh.rest(f"repos/{repo}/pulls/{number}")
        run_pages = gh.rest_pages(
            f"repos/{repo}/commits/{pull['head']['sha']}/check-runs?per_page={rest_page_size}"
        )
        status_pages = gh.rest_pages(
            f"repos/{repo}/commits/{pull['head']['sha']}/statuses?per_page={rest_page_size}"
        )
        runs = [run for page in run_pages for run in page.get("check_runs", [])]
        # A check run names only its suite; the workflow run of that suite
        # names the workflow and event, as GraphQL does in one query.
        workflows = {
            item.get("check_suite_id"): item
            for item in rest_census(
                gh.rest_pages(f"repos/{repo}/actions/runs?head_sha={quote(pull['head']['sha'], safe='')}&per_page={rest_page_size}"),
                "workflow_runs",
                "workflow run",
            )
        } if runs else {}
        for run in runs:
            suite = (run.get("check_suite") or {}).get("id")
            if (run.get("app") or {}).get("slug") == "github-actions" and suite not in workflows:
                raise TransientError(f"no workflow run is listed yet for check suite {suite}")
        # The statuses endpoint lists every state a context has had, newest
        # first; the GraphQL rollup has only the current one.
        statuses: dict[str, dict[str, Any]] = {}
        for page in status_pages:
            for item in page:
                statuses.setdefault(item.get("context"), item)
        checks = [
            {"name": run.get("name"), "status": str(run.get("status", "")).upper(), "conclusion": str(run.get("conclusion") or "").upper(), "completedAt": run.get("completed_at"), "observedAt": run.get("started_at") or run.get("completed_at"), "suite": (run.get("check_suite") or {}).get("id"), "source": check_source(
                (run.get("app") or {}).get("slug"),
                workflows.get((run.get("check_suite") or {}).get("id"), {}).get("workflow_id"),
                workflows.get((run.get("check_suite") or {}).get("id"), {}).get("event"),
            )}
            for run in runs
        ] + [
            {"name": item.get("context"), "status": "COMPLETED", "conclusion": str(item.get("state", "")).upper(), "observedAt": item.get("created_at"), "source": "status"}
            for item in statuses.values()
        ]
        # REST reports a merged pull request as closed and gives an unmerged one
        # its test merge commit; GraphQL reports MERGED and no merge commit.
        merged = bool(pull.get("merged"))
        state = "MERGED" if merged else str(pull.get("state") or "").upper() or None
        return {"head": pull["head"]["sha"], "state": state, "merged": merged, "mergedAt": pull.get("merged_at"), "mergeCommit": pull.get("merge_commit_sha") if merged else None, "checks": sorted(checks, key=lambda item: str(item["name"]))}


def check_is_terminal(check: dict[str, Any]) -> bool:
    return (
        str(check.get("conclusion", "")).upper() in TERMINAL_CHECK_CONCLUSIONS
        and (
            str(check.get("status", "")).upper() == "COMPLETED"
            or check.get("completedAt") is not None
        )
    )


def check_source(app: Any, workflow: Any, event: Any) -> str:
    """What produced a check run: its app and, for Actions, workflow and event.

    GraphQL gives no app for one the token cannot see, so such apps share a source.
    """
    return "/".join(str(part) for part in (app, workflow, event) if part is not None)


def checks_by_name(observation: dict[str, Any]) -> dict[str, list[dict[str, Any]]]:
    """The current contexts of each check name from each source.

    A source's newer check suite replaces its older one: a re-run, a run
    cancelled by a newer one, or a re-check after a pull request edit. GitHub
    numbers suites in creation order, while a job's start time can come after
    a newer run's. Sources differ by app, workflow and event, so one workflow
    triggered by both push and pull_request has two current runs. A commit
    status has no suite and counts by its latest state.
    """
    def order(check: dict[str, Any]) -> tuple[int, str]:
        suite = check.get("suite")
        return (suite, "") if suite else (0, str(check.get("observedAt") or ""))

    current: dict[tuple[str, Any], list[dict[str, Any]]] = {}
    for check in observation.get("checks", []):
        key = (str(check.get("name")), check.get("source"))
        kept = current.get(key)
        if not kept or order(check) > order(kept[0]):
            current[key] = [check]
        elif order(check) == order(kept[0]):
            kept.append(check)
    observed: dict[str, list[dict[str, Any]]] = {}
    for (name, _source), checks in current.items():
        observed.setdefault(name, []).extend(checks)
    return observed


def check_failed(check: dict[str, Any]) -> bool:
    return check_is_terminal(check) and str(check.get("conclusion", "")).upper() not in PASSING_CHECK_CONCLUSIONS


def failed_checks(expected: set[str], observed: dict[str, list[dict[str, Any]]]) -> list[str]:
    """Expected check names with at least one context that ended without passing."""
    return sorted(
        name for name in expected & set(observed)
        if any(check_failed(check) for check in observed[name])
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
        if observation.get("merged"):
            return "satisfied", "pull request merged"
        if observation.get("state") == "CLOSED":
            return "changed", "pull request was closed without merging"
        return "waiting", "pull request remains open"
    observed = checks_by_name(observation)
    failed = failed_checks(expected_checks, observed)
    if failed:
        # One failed check decides the result; the others need not finish.
        return "changed", f"a check reached a non-success terminal result: {', '.join(failed)}"
    missing = expected_checks - set(observed)
    if missing:
        return "waiting", f"expected checks have not appeared: {', '.join(sorted(missing))}"
    if not expected_checks or not all(
        check_is_terminal(check) for name in expected_checks for check in observed[name]
    ):
        return "waiting", "checks are not terminal"
    return "satisfied", "all expected checks are terminal-success"


HEAD_QUERY = """
query($owner:String!,$name:String!,$number:Int!){
  repository(owner:$owner,name:$name){pullRequest(number:$number){headRefOid}}
}
"""


# GitHub Actions run and job conclusions that pass, matching
# PASSING_CHECK_CONCLUSIONS. Every other terminal conclusion is a failure.
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


def workflow_census(gh: Gh, repo: str, number: int, head: str, checks: bool, rest_page_size: int = 100) -> dict[str, Any]:
    """Every GitHub Actions run for the pull request's exact head, with its jobs."""
    snapshot = pr_snapshot(gh, repo, number, rest_page_size) if checks else {"head": pr_head(gh, repo, number)}
    observed = snapshot.get("head")
    if not isinstance(observed, str) or not observed:
        raise WaitError(f"pull request {repo}#{number} has no verified head")
    observation: dict[str, Any] = {"head": observed}
    if checks:
        observation["checks"] = snapshot["checks"]
    if observed != head:
        return observation
    runs = rest_census(
        gh.rest_pages(f"repos/{repo}/actions/runs?head_sha={quote(head, safe='')}&per_page={rest_page_size}"),
        "workflow_runs",
        "workflow run",
    )
    observation["runs"] = []
    for run in runs:
        if run.get("head_sha") != head:
            raise WaitError(f"workflow run {run['id']} is for head {run.get('head_sha')}, not {head}")
        jobs = rest_census(
            gh.rest_pages(f"repos/{repo}/actions/runs/{run['id']}/jobs?filter=latest&per_page={rest_page_size}"),
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
        observed = checks_by_name(observation)
        failures += [
            f"check {name}: " + ", ".join(sorted({
                str(check.get("conclusion")).lower() for check in observed[name] if check_failed(check)
            }))
            for name in failed_checks(self.checks, observed)
        ]
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


def legacy_checks_digests(observation: dict[str, Any]) -> set[str]:
    """Digests that helpers before 2026-10 saved for a checks-terminal observation.

    Their key listed every terminal check that did not succeed, named or not.
    Only a wait on the same head reuses a checkpoint, so this can go once the
    heads awaited before 2026-10 are merged or abandoned.
    """
    names = sorted(
        check.get("name") for check in observation.get("checks", [])
        if check_is_terminal(check) and str(check.get("conclusion", "")).upper() != "SUCCESS"
    )
    return {
        stable_digest({"head": observation.get("head"), "nonSuccess": names, "terminal": terminal})
        for terminal in (False, True)
    }


def rekey_legacy_checkpoint(path: Path | None, identity: dict[str, Any], transition_key: Any) -> None:
    """Re-digest a checkpoint saved under the old key, so upgrading is not a change."""
    prior = load_state(path)
    observation = (prior or {}).get("observation")
    # Another wait's checkpoint is invalidated, never rewritten.
    if not observation or prior.get("kind") != "checks-terminal" or prior.get("identity") != identity:
        return
    digest = stable_digest(transition_key(observation))
    if prior.get("digest") != digest and prior.get("digest") in legacy_checks_digests(observation):
        write_state(path, {**prior, "digest": digest})


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
        sub.add_argument("--rest-page-size", type=int, choices=range(1, 101), default=100, metavar="1..100")
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
                    observe = lambda: workflow_census(gh, args.repo, args.pr, args.head, bool(args.check), args.rest_page_size)
                    gate = WorkflowCensusGate(args.head, set(args.check), args.command == "wait")
                    classify = gate
                else:
                    observe = lambda: pr_snapshot(gh, args.repo, args.pr, args.rest_page_size)
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
                        # Only the named checks can wake the wait, judged as classify_pr
                        # judges them.
                        return {
                            "head": value.get("head"),
                            "nonSuccess": failed_checks(set(args.check), checks_by_name(value)),
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
                    if args.kind == "checks-terminal" and not args.all_workflows:
                        rekey_legacy_checkpoint(state_path, identity, transition_key)
                    output = wait_for_transition(kind=args.kind, identity=identity, observe=observe, classify=classify, state_path=state_path, deadline=deadline, interval=args.interval, metrics=metrics, transition_key=transition_key)
        emit(output, args.json)
        return 0
    except WaitError as error:
        output = result_envelope(args.kind, "operational-error", identity, {}, metrics, str(error))
        emit(output, args.json)
        return 2


if __name__ == "__main__":
    sys.exit(main())
