#!/usr/bin/env python3
"""Replay recorded CodeRabbit history through the adapter and check what happened next.

    python3 coderabbit_replay.py [--corpus FILE] [--json]

`RecordedGitHub` serves the GitHub reads the adapter makes as a recorded pull
request showed them at a chosen time. The harness replays the adapter's
`status` decision at every recorded event time of every recorded head, and at
every bound or retry time a decision names, then checks each decision against
the recorded history:

- `completion`: a head is complete only when CodeRabbit posted a findings
  review of that exact head, or a matching coverage marker followed by a
  `Review completed` status on it, on any recorded pull request of that
  commit, after the head last reached the pull request's branch, after the
  latest trigger on the pull request, and after a newer pending status on
  the head that CodeRabbit has not completed, paused or skipped since, in
  the order of time and status id; a force push leaving or reaching the head
  counts as its push;
- `refused-trigger`: no trigger while the head's latest refusal states a wait
  that is still running, unless a recorded trigger at that point was
  accepted;
- `no-capacity`: no trigger after a refusal whose newest paired notice says
  waiting won't change it;
- `automatic-review`: no trigger while CodeRabbit's own review of the head
  starts within 120 s;
- `unbounded`: every decision is final, a trigger, or a wait with a bound or
  retry time;
- `expired-bound`: a wait's bound or retry time is still ahead; and
- `error`: the adapter never fails on recorded data.

Exits 1 when it finds a violation that `fixtures/coderabbit_replay_pinned.json`
does not pin.
"""

from __future__ import annotations

import argparse
import bisect
import importlib.util
import json
import re
import sys
from collections import defaultdict
from pathlib import Path
from typing import Any, Iterable
from urllib.parse import urlparse

sys.dont_write_bytecode = True

ROOT = Path(__file__).resolve().parents[2]
SPEC = importlib.util.spec_from_file_location(
    "coderabbit_adapter", ROOT / "address-feedback" / "scripts" / "coderabbit_adapter.py"
)
assert SPEC and SPEC.loader
ADAPTER = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(ADAPTER)

FIXTURES = Path(__file__).resolve().parent / "fixtures"
CORPUS = FIXTURES / "coderabbit_recorded.json"
PINNED = FIXTURES / "coderabbit_replay_pinned.json"
TRIGGER_STATES = {"trigger-incremental", "trigger-full"}
REVIEW_COMMANDS = {"@coderabbitai review", "@coderabbitai full review"}


def at(value: str) -> float:
    return ADAPTER.parse_time(value)


def iso(value: float) -> str:
    return ADAPTER.format_timestamp(value)


def load(path: Path = CORPUS) -> list[dict[str, Any]]:
    return json.loads(path.read_text())["pullRequests"]


class Metrics:
    retries = 0


class RecordedGitHub:
    """GitHub as the recorded pull requests showed it at `now`; POSTs are recorded."""

    def __init__(self, pulls: Iterable[dict[str, Any]], now: float | str) -> None:
        self.pulls = {(pull["repo"], pull["number"]): pull for pull in pulls}
        self.now = at(now) if isinstance(now, str) else now
        self.metrics = Metrics()
        self.posts: list[tuple[str, dict[str, str], float]] = []
        self.on_post: Any = None
        self.unreadable_histories: set[int] = set()
        self.next_id = 9_000_000_000
        self.timelines = {key: head_timeline(pull) for key, pull in self.pulls.items()}

    # Recorded state at `now` --------------------------------------------------

    def pull(self, repo: str, number: int) -> dict[str, Any]:
        return self.pulls[(repo, number)]

    def head_at(self, repo: str, number: int) -> str:
        timeline = self.timelines[(repo, number)]
        index = bisect.bisect_right([arrival for arrival, _sha in timeline], self.now) - 1
        return timeline[max(index, 0)][1]

    def flag_at(self, pull: dict[str, Any], on: str, off: str, initial: bool) -> bool:
        value = initial
        for event in pull["events"]:
            if at(event["at"]) <= self.now and event["type"] in {on, off}:
                value = event["type"] == on
        return value

    def is_open(self, pull: dict[str, Any]) -> bool:
        closed = any(at(event["at"]) <= self.now for event in pull["events"] if event["type"] == "merged")
        return not closed and not self.flag_at(pull, "closed", "reopened", False)

    def is_draft(self, pull: dict[str, Any]) -> bool:
        return self.flag_at(pull, "draft", "ready", pull["initialDraft"])

    def statuses(self, repo: str, sha: str) -> list[dict[str, Any]]:
        found = {
            status["id"]: status
            for pull in self.pulls.values()
            if pull["repo"] == repo and sha in pull["heads"]
            for status in pull["heads"][sha]["statuses"]
            if at(status["created_at"]) <= self.now
        }
        return sorted(found.values(), key=lambda item: (item["created_at"], item["id"]), reverse=True)

    def suites(self, repo: str, sha: str) -> list[dict[str, Any]]:
        found = {
            (suite["created_at"], suite["head_branch"]): suite
            for pull in self.pulls.values()
            if pull["repo"] == repo and sha in pull["heads"]
            for suite in pull["heads"][sha]["checkSuites"]
            if at(suite["created_at"]) <= self.now
        }
        return list(found.values())

    def comment_view(self, repo: str, number: int, item: dict[str, Any]) -> dict[str, Any] | None:
        versions = [version for version in item["versions"] if at(version[0]) <= self.now]
        if not versions:
            return None
        return {
            "id": item["id"],
            "node_id": f"IC_{item['id']}",
            "user": item["user"],
            "issue_url": f"https://api.github.com/repos/{repo}/issues/{number}",
            "created_at": item["created_at"],
            "updated_at": versions[-1][0],
            "body": versions[-1][1],
        }

    # GitHub reads ------------------------------------------------------------

    def rest(self, endpoint: str, method: str = "GET", fields: dict[str, str] | None = None) -> Any:
        if method == "POST":
            self.posts.append((endpoint, dict(fields or {}), self.now))
            if self.on_post:
                return {"id": self.on_post(endpoint, dict(fields or {}))}
            self.next_id += 1
            return {"id": self.next_id}
        match = re.fullmatch(r"repos/([^/]+/[^/]+)/pulls/(\d+)", endpoint)
        if match and (match.group(1), int(match.group(2))) in self.pulls:
            repo, number = match.group(1), int(match.group(2))
            pull = self.pull(repo, number)
            return {
                "number": number,
                "head": {"sha": self.head_at(repo, number), "ref": pull["ref"]},
                "state": "open" if self.is_open(pull) else "closed",
                "draft": self.is_draft(pull),
                "created_at": pull["createdAt"],
            }
        match = re.fullmatch(r"repos/([^/]+/[^/]+)/commits/([0-9a-f]{40})", endpoint)
        if match:
            repo, sha = match.groups()
            for key, pull in self.pulls.items():
                if pull["repo"] == repo and sha in pull["heads"]:
                    arrival = next(time for time, head in self.timelines[key] if head == sha)
                    return {
                        "sha": sha,
                        "parents": [{"sha": parent} for parent in pull["heads"][sha]["parents"]],
                        # Commit times are not recorded; the head's arrival stands in.
                        "commit": {"committer": {"date": iso(arrival)}},
                    }
        raise ADAPTER.WaitError(f"gh: Not Found (HTTP 404) {endpoint}")

    def rest_pages(self, endpoint: str) -> list[Any]:
        path = urlparse(endpoint).path
        match = re.fullmatch(r"repos/([^/]+/[^/]+)/commits/([0-9a-f]+)/statuses", path)
        if match:
            return [self.statuses(*match.groups())]
        match = re.fullmatch(r"repos/([^/]+/[^/]+)/commits/([0-9a-f]+)/check-suites", path)
        if match:
            return [{"check_suites": self.suites(*match.groups())}]
        match = re.fullmatch(r"repos/([^/]+/[^/]+)/commits/([0-9a-f]+)/pulls", path)
        if match:
            repo, sha = match.groups()
            return [
                [
                    {"number": number, "state": "open", "head": {"sha": sha}}
                    for (owner_repo, number), pull in sorted(self.pulls.items())
                    if owner_repo == repo and self.is_open(pull) and self.head_at(repo, number) == sha
                    and at(pull["createdAt"]) <= self.now
                ]
            ]
        match = re.fullmatch(r"repos/([^/]+/[^/]+)/issues/(\d+)/comments", path)
        if match:
            repo, number = match.group(1), int(match.group(2))
            views = [self.comment_view(repo, number, item) for item in self.pull(repo, number)["comments"]]
            return [[view for view in views if view]]
        match = re.fullmatch(r"repos/([^/]+/[^/]+)/pulls/(\d+)/reviews", path)
        if match:
            pull = self.pull(match.group(1), int(match.group(2)))
            return [[review for review in pull["reviews"] if at(review["submitted_at"]) <= self.now]]
        raise AssertionError(f"unexpected read {endpoint}")

    def graphql(self, query: str, variables: dict[str, Any]) -> dict[str, Any]:
        if "READY_FOR_REVIEW_EVENT" in query:
            pull = self.pull(f"{variables['owner']}/{variables['name']}", int(variables["number"]))
            ready = [event["at"] for event in pull["events"] if event["type"] == "ready" and at(event["at"]) <= self.now]
            nodes = [{"createdAt": ready[-1]}] if ready else []
            return {"repository": {"pullRequest": {"timelineItems": {"nodes": nodes}}}}
        if "HEAD_REF_FORCE_PUSHED_EVENT" in query:
            # Pages of `last: N` force pushes, newest page first; a cursor is the index a page starts at.
            pull = self.pull(f"{variables['owner']}/{variables['name']}", int(variables["number"]))
            limit = int(re.search(r"timelineItems\(last: (\d+)", query).group(1))
            pushes = [
                {"createdAt": event["at"], "beforeCommit": {"oid": event["from"]}, "afterCommit": {"oid": event["head"]}}
                for event in pull["events"]
                if event["type"] == "force-pushed" and at(event["at"]) <= self.now
            ]
            end = int(variables["cursor"]) if variables.get("cursor") else len(pushes)
            begin = max(0, end - limit)
            info = {"hasPreviousPage": begin > 0, "startCursor": str(begin)}
            return {"repository": {"pullRequest": {"timelineItems": {"pageInfo": info, "nodes": pushes[begin:end]}}}}
        if "userContentEdits" in query:
            identifier = int(variables["id"].removeprefix("IC_"))
            if identifier in self.unreadable_histories:
                raise ADAPTER.WaitError("GitHub GraphQL error: edit history unavailable")
            item = next(
                entry for pull in self.pulls.values() for entry in pull["comments"] if entry["id"] == identifier
            )
            versions = [version for version in item["versions"] if at(version[0]) <= self.now]
            nodes = [{"editedAt": edited, "diff": body} for edited, body in reversed(versions)]
            if len(nodes) < 2:
                nodes = []  # GitHub lists no edits for an unedited comment.
            start = int(variables.get("cursor") or 0)
            return {
                "node": {
                    "userContentEdits": {
                        "pageInfo": {"hasNextPage": start + 20 < len(nodes), "endCursor": str(start + 20)},
                        "nodes": nodes[start:start + 20],
                    }
                }
            }
        raise AssertionError(f"unexpected GraphQL query {query[:60]}")


def force_pushes(pull: dict[str, Any]) -> list[tuple[float, str]]:
    """Each recorded force push of the branch, with the head it brought, oldest first."""
    return sorted((at(event["at"]), event["head"]) for event in pull["events"] if event["type"] == "force-pushed")


def open_at(pull: dict[str, Any], now: float) -> bool:
    state = True
    for event in pull["events"]:
        if at(event["at"]) <= now and event["type"] in {"closed", "merged", "reopened"}:
            state = event["type"] == "reopened"
    return state


def head_timeline(pull: dict[str, Any]) -> list[tuple[float, str]]:
    """Each head the branch had, with when it arrived, oldest first.

    A head arrives with its first check suite on the branch; without one, with
    CodeRabbit's first status on it. The recorded final head comes last. A
    recorded force push brings its head back to the branch.
    """
    arrivals = []
    for sha, head in pull["heads"].items():
        suites = [at(suite["created_at"]) for suite in head["checkSuites"] if suite["head_branch"] == pull["ref"]]
        statuses = [at(status["created_at"]) for status in head["statuses"]]
        if suites or (statuses and sha != pull["head"]):
            arrivals.append((min(suites or statuses), sha))
    arrivals.sort()
    if force_pushes(pull):
        timeline = sorted(set(arrivals + force_pushes(pull)))
        return timeline if timeline[-1][1] == pull["head"] else timeline + [(timeline[-1][0], pull["head"])]
    final = [entry for entry in arrivals if entry[1] == pull["head"]]
    arrivals = [entry for entry in arrivals if entry[1] != pull["head"]]
    last = max([arrivals[-1][0] if arrivals else at(pull["createdAt"])] + [entry[0] for entry in final])
    return arrivals + [(last, pull["head"])]


# --- Ground truth ------------------------------------------------------------------


def shown_versions(pulls: list[dict[str, Any]], now: float) -> list[tuple[float, str]]:
    """What each CodeRabbit comment showed at each second by `now`, comment by comment.

    Within one comment a later version in its recorded order wins a second it
    shares with an earlier one, whether or not either states a wait.
    """
    shown = []
    for pull in pulls:
        for item in pull["comments"]:
            if item["user"]["login"].lower() not in ADAPTER.BOT_LOGINS:
                continue
            versions: dict[float, str] = {}
            for edited, body in item["versions"]:
                if at(edited) <= now:
                    versions[at(edited)] = body
            shown += sorted(versions.items())
    return shown


def refused_for_capacity(pulls: list[dict[str, Any]], status: dict[str, Any], now: float) -> bool:
    """Whether the newest CodeRabbit notice stating a wait before the refusal says waiting won't change it.

    Between comments, the first listed wins a tie.
    """
    status_at = at(status["created_at"])
    paired = [
        (edited, ADAPTER.no_capacity(body))
        for edited, body in shown_versions(pulls, now)
        if ADAPTER.stated_seconds(body) is not None and status_at - ADAPTER.CORRELATION_SECONDS <= edited <= status_at
    ]
    return bool(paired) and max(paired, key=lambda entry: entry[0])[1]


def stated_wait(pulls: list[dict[str, Any]], status: dict[str, Any], now: float) -> tuple[float, float] | None:
    """The recorded refusal's stated wait, as (status time, available time)."""
    status_at = at(status["created_at"])
    best = None
    for edited, body in shown_versions(pulls, now):
        seconds = ADAPTER.stated_seconds(body)
        if seconds is not None and status_at - ADAPTER.CORRELATION_SECONDS <= edited <= status_at:
            best = max(best or 0.0, edited + seconds)
    return (status_at, best) if best else None


class History:
    """What the recorded pull requests show actually happened."""

    def __init__(self, pulls: list[dict[str, Any]]) -> None:
        self.pulls = pulls
        self.by_repo: dict[str, list[dict[str, Any]]] = defaultdict(list)
        for pull in pulls:
            self.by_repo[pull["repo"]].append(pull)

    def statuses(self, repo: str, sha: str) -> list[dict[str, Any]]:
        found = {
            status["id"]: status
            for pull in self.by_repo[repo]
            if sha in pull["heads"]
            for status in pull["heads"][sha]["statuses"]
        }
        return sorted(found.values(), key=lambda item: (item["created_at"], item["id"]))

    def triggers(self, pull: dict[str, Any], whole_line: bool = False) -> list[float]:
        """Every comment that mentions a review command, or, when `whole_line`, has a line that is one."""

        def asks(body: str) -> bool:
            lines = [line.strip().lower() for line in body.splitlines()]
            if whole_line:
                return any(line in REVIEW_COMMANDS for line in lines)
            return any(command in line for line in lines for command in REVIEW_COMMANDS)

        return [
            at(item["created_at"])
            for item in pull["comments"]
            if item["user"]["login"] == "maintainer" and asks(item["versions"][0][1])
        ]

    def review_cutoff(self, pull: dict[str, Any], sha: str, now: float) -> float:
        """The head's latest push to the pull request's branch, its latest trigger, or a newer review by `now`.

        Opening the pull request or marking it ready leaves an existing review of the commit standing.
        A newer review starts with a pending status on the head and ends when CodeRabbit completes,
        pauses or skips it; a skip on a head another open pull request shares may answer that one.
        """
        suites = [at(suite["created_at"]) for suite in pull["heads"][sha]["checkSuites"] if suite["head_branch"] == pull["ref"]]
        pushed = min(suites) if suites else next(time for time, head in head_timeline(pull) if head == sha)
        pushed = max([pushed] + [
            at(event["at"]) for event in pull["events"]
            if event["type"] == "force-pushed" and sha in (event["from"], event["head"]) and at(event["at"]) <= now
        ])
        moments = [pushed] + [time for time in self.triggers(pull, whole_line=True) if time >= pushed]
        # In CodeRabbit's order, by time and then status id.
        statuses = [(at(status["created_at"]), ADAPTER.status_kind(status)) for status in self.statuses(pull["repo"], sha)]
        statuses = [(when, kind) for when, kind in statuses if when <= now]
        shared = any(
            other is not pull and sha in other["heads"] and at(other["createdAt"]) <= now and open_at(other, now)
            for other in self.by_repo[pull["repo"]]
        )
        ending = {"completed", "paused", "draft-skip"} | (set() if shared else {"skipped"})
        pending = [index for index, (_when, kind) in enumerate(statuses) if kind == "in-progress"][-1:]
        moments += [statuses[index][0] for index in pending if not any(kind in ending for _when, kind in statuses[index + 1:])]
        return max(moment for moment in moments if moment <= now)

    def completed_with_evidence(self, pull: dict[str, Any], sha: str, now: float) -> bool:
        """Whether the commit was reviewed since its push to this pull request's branch, on any pull request."""
        cutoff = self.review_cutoff(pull, sha, now)
        sharing = [other for other in self.by_repo[pull["repo"]] if sha in other["heads"]]
        for review in (review for other in sharing for review in other["reviews"]):
            submitted = at(review["submitted_at"])
            if review["commit_id"] == sha and cutoff < submitted <= now and ADAPTER.findings_review([review], sha):
                return True
        parents = pull["heads"][sha]["parents"]
        covered = {sha} | ({parents[0]} if len(parents) > 1 else set())
        markers = [
            at(edited)
            for other in sharing
            for item in other["comments"]
            for edited, body in item["versions"]
            if at(edited) <= now
            and ADAPTER.clean_review([{"body": body}], sorted(covered))
        ]
        completed = [
            at(status["created_at"])
            for status in self.statuses(pull["repo"], sha)
            if ADAPTER.status_kind(status) == "completed" and cutoff < at(status["created_at"]) <= now
        ]
        return any(marker <= done for marker in markers for done in completed)

    def violations(self, pull: dict[str, Any], sha: str, now: float, decision: dict[str, Any]) -> list[str]:
        state = decision["state"]
        found = []
        if state in ADAPTER.COMPLETE_STATES and not self.completed_with_evidence(pull, sha, now):
            found.append("completion")
        if state not in ADAPTER.FINAL_STATES | TRIGGER_STATES:
            bound = decision.get("retryAt") or decision.get("boundAt")
            if not bound:
                found.append("unbounded")
            elif at(bound) <= now:
                found.append("expired-bound")
        if state not in TRIGGER_STATES:
            return found
        statuses = self.statuses(pull["repo"], sha)
        before = [status for status in statuses if at(status["created_at"]) <= now]
        after = [status for status in statuses if at(status["created_at"]) > now]
        triggers = self.triggers(pull)
        # A recorded trigger at that point that CodeRabbit started reviewing.
        accepted = any(
            abs(trigger - now) <= 60
            and ADAPTER.status_kind(next((s for s in statuses if at(s["created_at"]) > trigger), None))
            == "in-progress"
            for trigger in triggers
        )
        refusals = [status for status in before if ADAPTER.status_kind(status) == "rate-limited"]
        sharing = [other for other in self.by_repo[pull["repo"]] if sha in other["heads"]]
        own = stated_wait(sharing, refusals[-1], now) if refusals else None
        if own and now < own[1] and not accepted:
            found.append("refused-trigger")
        if refusals and refused_for_capacity(sharing, refusals[-1], now):
            found.append("no-capacity")
        for status in after:
            started = at(status["created_at"])
            if started > now + ADAPTER.AUTOMATIC_REVIEW_SECONDS:
                break
            prompted = any(started - ADAPTER.TRIGGER_ANSWER_SECONDS <= trigger <= started for trigger in triggers)
            if ADAPTER.status_kind(status) == "in-progress" and not prompted:
                found.append("automatic-review")
                break
        return found


# --- Replay ----------------------------------------------------------------------


def event_times(pull: dict[str, Any], start: float, end: float) -> list[float]:
    times = {start}
    for head in pull["heads"].values():
        times |= {at(status["created_at"]) for status in head["statuses"]}
    for item in pull["comments"]:
        times |= {at(version[0]) for version in item["versions"]}
    times |= {at(review["submitted_at"]) for review in pull["reviews"]}
    times |= {at(event["at"]) for event in pull["events"]}
    return sorted(time for time in times if start <= time < end)


def replay(pulls: list[dict[str, Any]]) -> tuple[list[dict[str, Any]], dict[str, int]]:
    """Every violation, and how many decisions reached each state."""
    history = History(pulls)
    by_repo = {repo: RecordedGitHub(group, 0.0) for repo, group in history.by_repo.items()}
    violations: list[dict[str, Any]] = []
    checked: dict[str, int] = defaultdict(int)
    for pull in pulls:
        gh = by_repo[pull["repo"]]
        timeline = gh.timelines[(pull["repo"], pull["number"])]
        closes = [at(event["at"]) for event in pull["events"] if event["type"] in {"closed", "merged"}]
        for index, (arrival, sha) in enumerate(timeline):
            start = max(arrival, at(pull["createdAt"]))
            if index + 1 < len(timeline):
                end = timeline[index + 1][0]
            elif closes:
                end = max(closes[-1] + 1, start + 1)
            else:
                end = max([start] + event_times(pull, start, float("inf"))) + 6 * 3600
            if end <= start:
                continue
            pending = event_times(pull, start, end) + [end - 1]
            seen: set[float] = set()
            while pending:
                now = pending.pop(0)
                if now in seen or not start <= now < end:
                    continue
                seen.add(now)
                gh.now = now
                try:
                    value = ADAPTER.observation(gh, pull["repo"], [pull["number"]], {pull["number"]: sha}, now)
                    decision = value["pullRequests"][0]
                except Exception as error:  # noqa: BLE001 - every failure on real data is a finding.
                    decision = {"state": "error", "reason": f"{type(error).__name__}: {error}"}
                checked[decision["state"]] += 1
                kinds = ["error"] if decision["state"] == "error" else history.violations(pull, sha, now, decision)
                for kind in kinds:
                    violations.append(
                        {
                            "key": f"{pull['repo']}#{pull['number']} {sha[:8]} {iso(now)} {kind}",
                            "invariant": kind,
                            "state": decision["state"],
                            "reason": decision["reason"],
                        }
                    )
                for key in ("boundAt", "retryAt"):
                    if decision.get(key):
                        moment = at(decision[key])
                        if start <= moment < end and moment not in seen:
                            bisect.insort(pending, moment)
    return violations, checked


def pinned() -> dict[str, str]:
    return json.loads(PINNED.read_text()) if PINNED.exists() else {}


def main() -> int:
    arguments = argparse.ArgumentParser(description=__doc__.split("\n", 1)[0])
    arguments.add_argument("--corpus", type=Path, default=CORPUS)
    arguments.add_argument("--json", action="store_true")
    args = arguments.parse_args()
    pulls = load(args.corpus)
    violations, checked = replay(pulls)
    known = pinned() if args.corpus == CORPUS else {}
    unpinned = [item for item in violations if item["key"] not in known]
    heads = sum(len(head_timeline(pull)) for pull in pulls)
    summary = {
        "pullRequests": len(pulls),
        "heads": heads,
        "decisions": sum(checked.values()),
        "states": dict(sorted(checked.items())),
        "unpinned": len(unpinned),
    }
    counts: dict[str, int] = defaultdict(int)
    for item in violations:
        counts[item["invariant"]] += 1
    summary["violations"] = dict(sorted(counts.items()))
    if args.json:
        print(json.dumps(summary | {"details": violations}, indent=1))
    else:
        print(json.dumps(summary))
        for item in unpinned:
            print(f"{item['key']}: {item['state']} - {item['reason']}")
    return 1 if unpinned else 0


if __name__ == "__main__":
    sys.exit(main())
