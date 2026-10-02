#!/usr/bin/env python3
"""Behavioral tests for the CodeRabbit adapter, replayed from recorded public pull requests.

Each test names the recorded case it replays. "Synthetic" marks a change to a
recorded case that the history does not show.
"""

from __future__ import annotations

import copy
import importlib.util
import io
import json
import os
import pwd
import re
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path
from typing import Any, Callable
from unittest import mock


ROOT = Path(__file__).resolve().parents[2]
MODULE_PATH = ROOT / "address-feedback" / "scripts" / "coderabbit_adapter.py"
SPEC = importlib.util.spec_from_file_location(
    "coderabbit_replay", Path(__file__).resolve().parent / "coderabbit_replay.py"
)
assert SPEC and SPEC.loader
REPLAY = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(REPLAY)
ADAPTER = REPLAY.ADAPTER

CORPUS = {f"{pull['repo'].split('/')[1]}#{pull['number']}": pull for pull in REPLAY.load()}
REVIEW_NOTICE = re.compile(r"\*\*Next review available in:\*\* \*\*(\d+) minutes\*\*")
at, iso = REPLAY.at, REPLAY.iso


class Replay(REPLAY.RecordedGitHub):
    """The named recorded pull requests as GitHub showed them at `now`."""

    def __init__(self, *refs: str, now: str) -> None:
        super().__init__([copy.deepcopy(CORPUS[ref]) for ref in refs], now)
        self.refs = {ref: (CORPUS[ref]["repo"], CORPUS[ref]["number"]) for ref in refs}

    def case(self, ref: str) -> dict[str, Any]:
        return self.pulls[self.refs[ref]]

    def head(self, ref: str) -> str:
        return self.head_at(*self.refs[ref])

    def comment(self, ref: str, identifier: int) -> dict[str, Any]:
        return next(item for item in self.case(ref)["comments"] if item["id"] == identifier)

    def summary(self, ref: str) -> dict[str, Any]:
        return next(
            item for item in self.case(ref)["comments"]
            if item["versions"][0][1].startswith(ADAPTER.SUMMARY_MARKER)
        )

    def add_status(self, ref: str, description: str, when: str, state: str = "success") -> None:
        self.next_id += 1
        self.case(ref)["heads"][self.head(ref)]["statuses"].append(
            {
                "id": self.next_id,
                "state": state,
                "description": description,
                "context": "CodeRabbit",
                "created_at": when,
                "creator": {"login": "coderabbitai[bot]"},
            }
        )

    def add_comment(self, ref: str, body: str, when: str, login: str = "maintainer") -> int:
        self.next_id += 1
        self.case(ref)["comments"].append(
            {"id": self.next_id, "user": {"login": login}, "created_at": when, "versions": [[when, body]]}
        )
        return self.next_id

    def add_version(self, ref: str, identifier: int, body: str, when: str) -> None:
        self.comment(ref, identifier)["versions"].append([when, body])
        self.comment(ref, identifier)["versions"].sort()

    def move_version(self, ref: str, identifier: int, old: str, new: str) -> None:
        versions = self.comment(ref, identifier)["versions"]
        next(version for version in versions if version[0] == old)[0] = new
        versions.sort()

    def add_event(self, ref: str, kind: str, when: str) -> None:
        self.case(ref)["events"].append({"type": kind, "at": when})
        self.case(ref)["events"].sort(key=lambda event: event["at"])

    def truncate(self, ref: str, after: str) -> None:
        """Drop everything recorded after `after`, so synthetic answers replace it."""
        case = self.case(ref)
        for item in case["comments"]:
            item["versions"] = [version for version in item["versions"] if version[0] <= after]
        case["comments"] = [item for item in case["comments"] if item["versions"]]
        case["reviews"] = [item for item in case["reviews"] if item["submitted_at"] <= after]
        for head in case["heads"].values():
            head["statuses"] = [item for item in head["statuses"] if item["created_at"] <= after]

    def remove_comment(self, ref: str, identifier: int) -> None:
        self.case(ref)["comments"].remove(self.comment(ref, identifier))


def owned(ref: str, owner: str, now: str) -> REPLAY.RecordedGitHub:
    """The recorded pull request under an invented owner."""
    case = copy.deepcopy(CORPUS[ref])
    case["repo"] = f"{owner}/{case['repo'].split('/', 1)[1]}"
    return REPLAY.RecordedGitHub([case], now)


def observe(gh: Replay, ref: str, head: str | None = None, **options: Any) -> dict[str, Any]:
    repo, number = gh.refs[ref]
    expected = head or gh.head(ref)
    return ADAPTER.observation(gh, repo, [number], {number: expected}, gh.now, **options)["pullRequests"][0]


def state(ref: str, now: str, *others: str) -> dict[str, Any]:
    return observe(Replay(ref, *others, now=now), ref)


def observed_at(gh: Replay, ref: str, now: str) -> dict[str, Any]:
    gh.now = at(now)
    return observe(gh, ref)


def run(gh: Replay, ref: str, deadline: str, interval: float = 60) -> tuple[str, str, dict[str, Any]]:
    sleeps = [0]

    def sleep(seconds: float) -> None:
        sleeps[0] += 1
        if sleeps[0] > 1000:
            raise AssertionError("run_review kept polling without the clock reaching the deadline")
        gh.now += seconds

    repo, number = gh.refs[ref]
    return ADAPTER.run_review(
        gh, repo, number, gh.head(ref), at(deadline), interval, clock=lambda: gh.now, sleeper=sleep
    )


def answering(gh: Replay, ref: str, *answers: Callable[[float], None], listed: bool = True) -> None:
    """GitHub lists each posted trigger unless `listed` is false; CodeRabbit gives the next answer."""
    pending = list(answers)

    def post(_endpoint: str, fields: dict[str, str]) -> int:
        if listed:
            identifier = gh.add_comment(ref, fields["body"], iso(gh.now))
        else:
            gh.next_id += 1
            identifier = gh.next_id
        if pending:
            pending.pop(0)(gh.now)
        return identifier

    gh.on_post = post


def posts(gh: Replay) -> list[tuple[str, str]]:
    return [(fields["body"], iso(when)) for _endpoint, fields, when in gh.posts]


def lock_loss(gh: Replay, ref: str) -> Callable[[float], None]:
    """CodeRabbit starts the triggered review and stops it after lock loss."""

    def answer(posted: float) -> None:
        gh.add_status(ref, "Review in progress", iso(posted + 5), state="pending")
        gh.add_status(ref, "Review stopped after lock loss", iso(posted + 10), state="failure")

    return answer


GITHUB_API, ENTERPRISE_API = "https://api.github.com", "https://ghe.example.com/api/v3"


def main(
    gh: REPLAY.RecordedGitHub,
    *argv: str,
    root: str | None = GITHUB_API,
    while_waiting: Callable[[], None] | None = None,
) -> tuple[int, dict[str, Any]]:
    """The adapter's CLI against `gh` from `gh.now`, with gh's API root at `root`.

    `gh api /` fails when `root` is None; `gh.root_reads` counts its calls. A
    `run` polls on `gh`'s clock and calls `while_waiting` at each wait.
    """
    output: list[str] = []
    read, review = gh.rest, ADAPTER.run_review
    gh.root_reads = 0

    def rest(endpoint: str, *args: Any) -> Any:
        if endpoint != "/":
            return read(endpoint, *args)
        gh.root_reads += 1
        if root is None:
            raise ADAPTER.WaitError("gh: Bad Gateway (HTTP 502)")
        return {"current_user_url": f"{root}/user", "repository_url": f"{root}/repos/{{owner}}/{{repo}}"}

    def wait(seconds: float) -> None:
        if while_waiting:
            while_waiting()
        gh.now += seconds

    def run_review(*args: Any) -> Any:
        return review(*args, clock=lambda: gh.now, sleeper=wait)

    gh.rest = rest
    with mock.patch.object(ADAPTER, "Gh", lambda _metrics: gh), mock.patch.object(
        ADAPTER, "run_review", run_review
    ), mock.patch.object(ADAPTER.time, "time", lambda: gh.now), mock.patch.object(
        ADAPTER.sys, "argv", ["coderabbit_adapter.py", *argv]
    ), mock.patch("builtins.print", output.append):
        code = ADAPTER.main()
    gh.rest = read
    return code, json.loads(output[0])


class HeadStatusTest(unittest.TestCase):
    """Each status CodeRabbit posted on a recorded head reaches its state."""

    def test_draft_skip_on_a_draft_waits_for_it_to_be_ready(self) -> None:
        # GocciaScript#1162 opened as a draft; CodeRabbit skipped f824e3ec at 09:55:11.
        result = state("GocciaScript#1162", "2026-08-15T09:58:00Z")
        self.assertEqual(result["status"]["description"], "Review skipped: draft pull request")
        self.assertEqual((result["state"], result["nextMode"]), ("draft", None))

    def test_review_in_progress_is_waited_on_for_an_hour(self) -> None:
        result = state("GocciaScript#1212", "2026-08-23T00:55:00Z")
        self.assertEqual(result["status"]["description"], "Review in progress")
        self.assertEqual((result["state"], result["since"], result["boundAt"]), (
            "in-progress", "2026-08-23T00:53:38Z", "2026-08-23T01:53:38Z"
        ))
        self.assertIsNone(result["nextMode"])

    def test_review_queued_is_pending(self) -> None:
        # GocciaScript#1212 was marked ready at 23:12:10; CodeRabbit queued its review at 23:12:14.
        result = state("GocciaScript#1212", "2026-08-22T23:12:20Z")
        self.assertEqual(result["status"]["description"], "Review queued")
        self.assertEqual((result["state"], result["boundAt"]), ("in-progress", "2026-08-23T00:12:14Z"))

    def test_rate_limit_waits_for_the_stated_wait_edited_in_before_it(self) -> None:
        # GocciaScript#1162: "Next review available in: 57 minutes" edited in 2 s before the status.
        result = state("GocciaScript#1162", "2026-08-15T17:00:00Z")
        self.assertEqual((result["status"]["state"], result["status"]["description"]), ("success", "Review rate limited"))
        self.assertEqual(result["rateLimit"]["wait"]["editedAt"], "2026-08-15T16:49:30Z")
        self.assertEqual(result["rateLimit"]["wait"]["statedSeconds"], 57 * 60)
        self.assertEqual((result["state"], result["retryAt"], result["nextMode"]), (
            "waiting", "2026-08-15T17:47:30Z", "incremental"
        ))

    def test_a_rate_limit_with_state_failure_waits_the_same_way(self) -> None:
        # Synthetic: GocciaScript#1162's refusal posted with state failure, as CodeRabbit did
        # on lwpt#94 in July, before the recorded window.
        gh = Replay("GocciaScript#1162", now="2026-08-15T17:00:00Z")
        refusal = gh.case("GocciaScript#1162")["heads"][gh.head("GocciaScript#1162")]["statuses"]
        next(item for item in refusal if item["description"] == "Review rate limited")["state"] = "failure"
        result = observe(gh, "GocciaScript#1162")
        self.assertEqual((result["status"]["state"], result["status"]["kind"]), ("failure", "rate-limited"))
        self.assertEqual((result["state"], result["retryAt"]), ("waiting", "2026-08-15T17:47:30Z"))

    def test_a_wait_stated_in_seconds(self) -> None:
        # duetto#27: "Next review available in: 58 seconds", edited in at 09:54:41.
        result = state("duetto#27", "2026-08-09T09:55:00Z")
        self.assertEqual(result["rateLimit"]["wait"]["statedSeconds"], 58)
        self.assertEqual((result["state"], result["retryAt"]), ("waiting", "2026-08-09T09:56:39Z"))
        self.assertEqual(state("duetto#27", "2026-08-09T09:56:39Z")["state"], "trigger-incremental")

    def test_the_included_review_wording_states_the_same_wait(self) -> None:
        # Synthetic: GocciaScript#1162's notice in CodeRabbit's other wording.
        gh = Replay("GocciaScript#1162", now="2026-08-15T17:00:00Z")
        versions = gh.summary("GocciaScript#1162")["versions"]
        for version in versions:
            version[1] = REVIEW_NOTICE.sub(r"Next included review available in \1 minutes.", version[1])
        self.assertTrue(any("Next included review available in 57 minutes." in body for _at, body in versions))
        result = observe(gh, "GocciaScript#1162")
        self.assertEqual((result["state"], result["retryAt"]), ("waiting", "2026-08-15T17:47:30Z"))

    def test_a_skip_is_terminal(self) -> None:
        cases = {
            "GocciaScript#1237": ("2026-09-14T12:05:00Z", "Review skipped: bot user not eligible for review"),
            "GocciaScript#1286": ("2026-09-28T16:18:20Z", "Review skipped"),
        }
        for ref, (now, description) in cases.items():
            with self.subTest(ref=ref):
                result = state(ref, now)
                self.assertEqual(result["status"]["description"], description)
                self.assertEqual((result["state"], result["nextMode"]), ("skipped", None))

    def test_the_rate_limited_block_alone_is_not_a_refusal(self) -> None:
        # A file-limit skip also carries this block. Synthetic: it on GocciaScript#1237's summary.
        self.assertNotIn("rate limited by coderabbit.ai", MODULE_PATH.read_text())
        gh = Replay("GocciaScript#1237", now="2026-09-14T12:05:00Z")
        block = (
            "<!-- This is an auto-generated comment: rate limited by coderabbit.ai -->\n"
            "<!-- end of auto-generated comment: rate limited by coderabbit.ai -->"
        )
        for version in gh.summary("GocciaScript#1237")["versions"]:
            version[1] = f"{version[1]}\n{block}"
        result = observe(gh, "GocciaScript#1237")
        self.assertEqual((result["state"], result["rateLimit"]), ("skipped", None))

    def test_a_paused_review_needs_a_person(self) -> None:
        # duetto#60: CodeRabbit paused reviews of 5e681fec at 06:28:25.
        result = state("duetto#60", "2026-09-26T06:28:26Z")
        self.assertEqual(result["status"]["description"], "Review paused")
        self.assertEqual((result["state"], result["nextMode"]), ("paused", None))
        self.assertIn("paused", ADAPTER.BLOCKED_STATES)

    def test_a_review_stopped_after_lock_loss_is_triggered_again(self) -> None:
        # GocciaScript#1113: "Review stopped after lock loss" (failure) at 17:42:57 on ba711c3f.
        result = state("GocciaScript#1113", "2026-08-09T17:45:00Z")
        self.assertEqual((result["status"]["state"], result["status"]["description"]), (
            "failure", "Review stopped after lock loss"
        ))
        self.assertEqual((result["state"], result["nextMode"]), ("trigger-incremental", "incremental"))

    def test_a_second_lock_loss_blocks_after_one_retry(self) -> None:
        # GocciaScript#1113 from 17:43. Synthetic: CodeRabbit stops every triggered review after
        # lock loss.
        ref = "GocciaScript#1113"
        gh = Replay(ref, now="2026-08-09T17:43:00Z")
        gh.truncate(ref, "2026-08-09T17:43:00Z")
        answering(gh, ref, *[lock_loss(gh, ref)] * 5)
        final, _reason, _value = run(gh, ref, "2026-08-09T18:40:00Z")
        self.assertEqual(posts(gh), [("@coderabbitai review", "2026-08-09T17:43:00Z")])
        self.assertEqual(final, "blocked-lock-loss")
        self.assertIn("blocked-lock-loss", ADAPTER.BLOCKED_STATES)

    def test_lock_loss_retries_a_full_review_as_a_full_review(self) -> None:
        # GocciaScript#1216 at 14:47:03 needs a full review. Synthetic: CodeRabbit stops it after
        # lock loss, then completes the retry without a review or coverage marker.
        ref = "GocciaScript#1216"
        gh = Replay(ref, now="2026-08-26T14:47:03Z")
        gh.truncate(ref, "2026-08-26T14:47:03Z")

        def completes(posted: float) -> None:
            gh.add_status(ref, "Review in progress", iso(posted + 5), state="pending")
            gh.add_status(ref, "Review completed", iso(posted + 60))

        answering(gh, ref, lock_loss(gh, ref), completes)
        final, _reason, _value = run(gh, ref, "2026-08-26T15:30:00Z")
        self.assertEqual(posts(gh), [
            ("@coderabbitai full review", "2026-08-26T14:47:03Z"),
            ("@coderabbitai full review", "2026-08-26T14:48:03Z"),
        ])
        self.assertEqual(final, "blocked-unconfirmed")

    def test_a_findings_review_of_the_head_completes_it(self) -> None:
        # duetto#55: CodeRabbit reviewed 4bccca05 with one actionable comment.
        result = state("duetto#55", "2026-09-26T06:40:00Z")
        self.assertEqual((result["state"], result["findingsReview"]["actionable"]), ("review-complete", 1))

    def test_a_coverage_marker_for_the_head_completes_a_clean_pass(self) -> None:
        # duetto#55: the summary marks 1ca17f50 reviewed with no actionable comments.
        result = state("duetto#55", "2026-09-25T19:50:00Z")
        self.assertEqual(result["state"], "clean-complete")
        self.assertEqual(result["cleanReview"]["coveredCommitId"][:8], "1ca17f50")

    def test_a_completed_status_without_evidence_waits_for_a_second_pass_then_escalates_once(self) -> None:
        # GocciaScript#1216: a triggered review of a8af1719 completed at 14:45:03 with no
        # coverage marker or findings review.
        waiting = state("GocciaScript#1216", "2026-08-26T14:45:10Z")
        self.assertEqual((waiting["state"], waiting["boundAt"]), ("awaiting-automatic", "2026-08-26T14:47:03Z"))
        result = state("GocciaScript#1216", "2026-08-26T14:47:03Z")
        self.assertIsNone(result["findingsReview"])
        self.assertIsNone(result["cleanReview"])
        self.assertEqual((result["state"], result["nextMode"]), ("trigger-full", "full"))

    def test_unrecognized_status_is_terminal(self) -> None:
        # Synthetic: a status CodeRabbit never posted on the recorded heads.
        gh = Replay("GocciaScript#1162", now="2026-08-15T17:00:00Z")
        gh.add_status("GocciaScript#1162", "Review failed", "2026-08-15T16:59:00Z", state="error")
        self.assertEqual(observe(gh, "GocciaScript#1162")["state"], "unrecognized-status")

    def test_a_merged_pull_request_is_closed(self) -> None:
        self.assertEqual(state("duetto#55", "2026-09-26T06:47:00Z")["state"], "closed")

    def test_only_coderabbits_own_statuses_decide(self) -> None:
        gh = Replay("GocciaScript#1162", now="2026-08-15T17:00:00Z")
        case = gh.case("GocciaScript#1162")
        case["heads"][gh.head("GocciaScript#1162")]["statuses"].append(
            {
                "id": 1,
                "state": "success",
                "description": "Review completed",
                "context": "CodeRabbit",
                "created_at": "2026-08-15T16:59:00Z",
                "creator": {"login": "maintainer"},
            }
        )
        self.assertEqual(observe(gh, "GocciaScript#1162")["status"]["description"], "Review rate limited")


# duetto#57: merge commit 422c3f66 completed at 06:41:55 while the summary marked its first parent.
MERGE_PARENT_CASE = ("duetto#57", "422c3f66", "2026-09-26T06:42:00Z")


class MergeHeadTest(unittest.TestCase):
    """A merge commit's completed review counts with the marker on it or its first parent."""

    def test_a_marker_naming_the_first_parent_completes_a_merge_head(self) -> None:
        ref, sha, now = MERGE_PARENT_CASE
        gh = Replay(ref, now=now)
        result = observe(gh, ref)
        self.assertEqual(gh.head(ref)[:8], sha)
        parents = gh.case(ref)["heads"][gh.head(ref)]["parents"]
        self.assertEqual(len(parents), 2)
        self.assertEqual(result["cleanReview"]["coveredCommitId"], parents[0])
        self.assertEqual(result["state"], "clean-complete")

    def test_a_marker_naming_the_second_parent_does_not(self) -> None:
        ref, _sha, now = MERGE_PARENT_CASE
        gh = Replay(ref, now=now)
        parents = gh.case(ref)["heads"][gh.head(ref)]["parents"]
        gh.case(ref)["heads"][gh.head(ref)]["parents"] = [parents[1], parents[0]]
        self.assertNotIn(observe(gh, ref)["state"], ADAPTER.COMPLETE_STATES)

    def test_a_first_parent_marker_does_not_complete_an_ordinary_head(self) -> None:
        # duetto#55 39a6ab08 has one parent, 4bccca05, whose clean marker is shown at 06:41:25.
        result = state("duetto#55", "2026-09-26T06:41:30Z")
        self.assertIsNone(result["cleanReview"])
        self.assertNotIn(result["state"], ADAPTER.COMPLETE_STATES)


class AutomaticReviewTest(unittest.TestCase):
    """CodeRabbit's own review of a new or ready head is awaited before any trigger."""

    def test_a_push_waits_120_s_for_the_automatic_review(self) -> None:
        # GocciaScript#1212: d4c66c7f pushed at 23:56:32, review queued at 23:56:57.
        result = state("GocciaScript#1212", "2026-08-22T23:56:40Z")
        self.assertEqual(result["automaticReview"], {"from": "2026-08-22T23:56:32Z", "source": "head-check-suite"})
        self.assertEqual((result["state"], result["boundAt"]), ("awaiting-automatic", "2026-08-22T23:58:32Z"))
        self.assertEqual(state("GocciaScript#1212", "2026-08-22T23:56:57Z")["state"], "in-progress")

    def test_a_ready_event_after_a_draft_skip_waits_120_s(self) -> None:
        # GocciaScript#1212: draft skip at 23:11:57, marked ready at 23:12:10.
        result = state("GocciaScript#1212", "2026-08-22T23:12:12Z")
        self.assertEqual(result["status"]["kind"], "draft-skip")
        self.assertEqual(result["automaticReview"], {"from": "2026-08-22T23:12:10Z", "source": "ready-for-review"})
        self.assertEqual((result["state"], result["boundAt"]), ("awaiting-automatic", "2026-08-22T23:14:10Z"))

    def test_a_push_during_the_review_of_an_earlier_head_waits_for_that_review_to_end(self) -> None:
        # GocciaScript#1265: 43621f9d pushed at 09:22:36 while 6bddc38d was reviewed; that review
        # ended at 09:25:32 and CodeRabbit's own review of 43621f9d started at 09:26:34.
        running = state("GocciaScript#1265", "2026-09-27T09:24:36Z")
        self.assertEqual(running["earlierReview"]["endedAt"], None)
        self.assertEqual((running["state"], running["boundAt"]), ("awaiting-automatic", "2026-09-27T10:22:36Z"))
        ended = state("GocciaScript#1265", "2026-09-27T09:25:32Z")
        self.assertEqual(ended["earlierReview"]["endedAt"], "2026-09-27T09:25:32Z")
        self.assertEqual((ended["state"], ended["boundAt"]), ("awaiting-automatic", "2026-09-27T09:27:32Z"))
        self.assertEqual(state("GocciaScript#1265", "2026-09-27T09:26:34Z")["state"], "in-progress")

    def test_an_unreadable_history_never_reads_as_no_earlier_review(self) -> None:
        # GocciaScript#1265 at 09:24:36: CodeRabbit is still reviewing 6bddc38d. Synthetic: the
        # summary's edit history cannot be read.
        gh = Replay("GocciaScript#1265", now="2026-09-27T09:23:00Z")
        gh.unreadable_histories.add(gh.summary("GocciaScript#1265")["id"])
        answering(gh, "GocciaScript#1265")
        with self.assertRaisesRegex(ADAPTER.WaitError, "edit history unavailable"):
            run(gh, "GocciaScript#1265", "2026-09-27T09:30:00Z", interval=5)
        self.assertEqual(gh.posts, [])

    def test_a_refused_github_read_is_an_operational_error(self) -> None:
        # Synthetic: GitHub answers 403 for the head's check suites or associated pull requests.
        for source in ("check-suites", "pulls"):
            with self.subTest(source=source):
                gh = Replay("GocciaScript#1212", now="2026-08-22T23:56:40Z")
                read = gh.rest_pages

                def refusing(endpoint: str, read: Callable[[str], list[Any]] = read, source: str = source) -> list[Any]:
                    if f"/{source}?" in endpoint:
                        raise ADAPTER.WaitError(f"gh: Resource not accessible (HTTP 403) {endpoint}")
                    return read(endpoint)

                gh.rest_pages = refusing  # type: ignore[method-assign]
                with self.assertRaisesRegex(ADAPTER.WaitError, "HTTP 403"):
                    observe(gh, "GocciaScript#1212")

    def test_another_pull_requests_running_review_does_not_hold_this_head(self) -> None:
        # GocciaScript#1265 at 09:24:36 waits on its review of 6bddc38d. Synthetic: another open
        # pull request with the same branch and head, whose summary never showed a review running.
        gh = Replay("GocciaScript#1265", now="2026-09-27T09:24:36Z")
        twin = copy.deepcopy(gh.case("GocciaScript#1265"))
        twin["number"] = 99999
        for item in twin["comments"]:
            item["id"] += 10**12
            item["versions"] = [[edited, body.replace(ADAPTER.REVIEWING_BLOCK, "")] for edited, body in item["versions"]]
        gh.pulls[(twin["repo"], 99999)] = twin
        gh.timelines[(twin["repo"], 99999)] = REPLAY.head_timeline(twin)
        gh.refs["twin"] = (twin["repo"], 99999)
        self.assertEqual(observe(gh, "GocciaScript#1265")["state"], "awaiting-automatic")
        result = observe(gh, "twin")
        self.assertEqual((result["sharedWith"], result["earlierReview"]), ([1265], None))
        self.assertEqual(result["state"], "trigger-incremental")

    def test_a_second_pass_after_a_completed_status_is_awaited(self) -> None:
        # GocciaScript#1256: fd3eec03 completed at 11:07:39; CodeRabbit started again at 11:07:52.
        first = state("GocciaScript#1256", "2026-09-26T11:07:39Z")
        self.assertEqual((first["state"], first["boundAt"]), ("awaiting-automatic", "2026-09-26T11:09:39Z"))
        self.assertEqual(state("GocciaScript#1256", "2026-09-26T11:07:52Z")["state"], "in-progress")


class CycleTest(unittest.TestCase):
    """The latest trigger or push, open or ready event starts the cycle every decision reads."""

    def test_a_newer_ready_event_ends_an_unanswered_trigger(self) -> None:
        # duetto#65: the trigger at 16:38:17 got no answer. Synthetic: marked ready again at 16:40.
        gh = Replay("duetto#65", now="2026-09-26T16:40:30Z")
        self.assertEqual(observe(gh, "duetto#65")["state"], "triggered")
        gh.add_event("duetto#65", "ready", "2026-09-26T16:40:00Z")
        self.assertEqual(observe(gh, "duetto#65")["state"], "awaiting-automatic")
        # The bot-user skip before the ready event stands once no new status follows it.
        later = observed_at(gh, "duetto#65", "2026-09-26T16:49:00Z")
        self.assertEqual(later["state"], "skipped")

    def test_a_newer_ready_event_restarts_the_bound_of_a_pending_review(self) -> None:
        # GocciaScript#1172: in progress since 13:33:10 with no completed status.
        # Synthetic: marked ready again at 14:00. CodeRabbit can still start a review queued
        # before a ready event, as on todomcp#11 in June, before the recorded window.
        gh = Replay("GocciaScript#1172", now="2026-08-17T14:33:10Z")
        self.assertEqual(observe(gh, "GocciaScript#1172")["state"], "blocked-stalled")
        gh.add_event("GocciaScript#1172", "ready", "2026-08-17T14:00:00Z")
        result = observe(gh, "GocciaScript#1172")
        self.assertEqual((result["state"], result["since"], result["boundAt"]), (
            "in-progress", "2026-08-17T14:00:00Z", "2026-08-17T15:00:00Z"
        ))

    def test_a_trigger_this_run_posted_counts_before_any_listing_shows_it(self) -> None:
        # duetto#55: clean pass of 1ca17f50 at 19:47:10. Synthetic: a run posted a full review
        # at 22:34:00 that no listing shows yet (the recorded one at 22:34:56 is removed).
        gh = Replay("duetto#55", now="2026-09-25T22:34:30Z")
        recorded = next(
            item for item in gh.case("duetto#55")["comments"]
            if item["versions"][0][1].startswith("@coderabbitai full review\n")
        )
        gh.remove_comment("duetto#55", recorded["id"])
        self.assertEqual(observe(gh, "duetto#55")["state"], "clean-complete")
        posted = [{"commentId": 1, "mode": "full", "postedAt": "2026-09-25T22:34:00Z", "at": at("2026-09-25T22:34:00Z")}]
        result = observe(gh, "duetto#55", posted=posted)
        self.assertEqual((result["state"], result["since"]), ("triggered", "2026-09-25T22:34:00Z"))
        self.assertEqual(result["triggers"][-1]["mode"], "full")


    def test_a_trigger_after_a_pending_status_awaits_its_answer(self) -> None:
        # GocciaScript#1172: in progress since 13:33:10. Synthetic: a review trigger at 14:33:30.
        gh = Replay("GocciaScript#1172", now="2026-08-17T14:34:00Z")
        gh.add_comment("GocciaScript#1172", "@coderabbitai review", "2026-08-17T14:33:30Z")
        result = observe(gh, "GocciaScript#1172")
        self.assertEqual((result["state"], result["since"], result["boundAt"]), (
            "triggered", "2026-08-17T14:33:30Z", "2026-08-17T14:43:30Z"
        ))

    def test_a_findings_review_after_a_pending_status_completes_the_head(self) -> None:
        # GocciaScript#1067: in progress since 23:09:39; the findings review of 19371184 came at
        # 23:13:50, three seconds before CodeRabbit's completed status.
        result = state("GocciaScript#1067", "2026-08-04T23:13:51Z")
        self.assertEqual(result["status"]["description"], "Review in progress")
        self.assertEqual((result["state"], result["findingsReview"]["actionable"]), ("review-complete", 2))
        self.assertEqual(state("GocciaScript#1067", "2026-08-04T23:13:49Z")["state"], "in-progress")

    def test_a_status_in_the_same_second_as_a_trigger_predates_it(self) -> None:
        # GocciaScript#1162 was rate limited at 16:49:32. Synthetic: a trigger in that second.
        gh = Replay("GocciaScript#1162", now="2026-08-15T16:50:00Z")
        gh.add_comment("GocciaScript#1162", "@coderabbitai review", "2026-08-15T16:49:32Z")
        result = observe(gh, "GocciaScript#1162")
        self.assertEqual((result["state"], result["since"]), ("triggered", "2026-08-15T16:49:32Z"))

    def test_a_ready_event_after_a_clean_pass_awaits_the_automatic_review(self) -> None:
        # duetto#55: clean pass of 1ca17f50 at 19:47:10. Synthetic: marked ready at 19:49:00.
        gh = Replay("duetto#55", now="2026-09-25T19:50:00Z")
        self.assertEqual(observe(gh, "duetto#55")["state"], "clean-complete")
        gh.add_event("duetto#55", "ready", "2026-09-25T19:49:00Z")
        result = observe(gh, "duetto#55")
        self.assertEqual((result["state"], result["boundAt"]), ("awaiting-automatic", "2026-09-25T19:51:00Z"))


class TriggerCommentTest(unittest.TestCase):
    """A line that is exactly a review command is a trigger, whatever else the comment says."""

    def test_a_command_line_followed_by_an_explanation_is_a_trigger(self) -> None:
        # duetto#55: clean pass of 1ca17f50 at 19:47:10; at 22:34:56 a comment requested a full
        # review on its first line and explained why below. CodeRabbit started at 22:35:09.
        gh = Replay("duetto#55", now="2026-09-25T22:34:56Z")
        self.assertEqual(gh.comment("duetto#55", 5840595432)["versions"][0][1], "@coderabbitai full review\n\n[text]")
        result = observe(gh, "duetto#55")
        self.assertEqual(result["state"], "triggered")
        self.assertEqual((result["since"], result["triggers"][-1]["mode"]), ("2026-09-25T22:34:56Z", "full"))
        self.assertEqual(observed_at(gh, "duetto#55", "2026-09-25T22:35:09Z")["state"], "in-progress")
        self.assertEqual(observed_at(gh, "duetto#55", "2026-09-25T22:40:53Z")["state"], "clean-complete")

    def test_a_command_quoted_inside_a_sentence_is_not_a_trigger(self) -> None:
        # duetto#65: a full review was requested at 17:08:58; the comment at 17:10:48 quotes
        # `@coderabbitai review` inside a sentence.
        result = state("duetto#65", "2026-09-26T17:10:50Z")
        self.assertEqual([trigger["createdAt"] for trigger in result["triggers"]], [
            "2026-09-26T16:38:17Z", "2026-09-26T17:08:58Z"
        ])
        self.assertEqual((result["state"], result["since"]), ("triggered", "2026-09-26T17:08:58Z"))

    def test_a_command_line_in_a_coderabbit_comment_is_not_a_trigger(self) -> None:
        # duetto#55: clean pass of 1ca17f50 at 19:47:10. Synthetic: CodeRabbit quotes a command
        # on its own line at 19:48.
        gh = Replay("duetto#55", now="2026-09-25T19:50:00Z")
        gh.add_comment("duetto#55", "To review again, comment:\n@coderabbitai full review", "2026-09-25T19:48:00Z", login="coderabbitai[bot]")
        result = observe(gh, "duetto#55")
        self.assertEqual((result["state"], len(result["triggers"])), ("clean-complete", 1))

    def test_the_mode_is_read_from_the_command_line(self) -> None:
        self.assertEqual(ADAPTER.trigger_mode("@coderabbitai full review\n\nThe last pass missed a file."), "full")
        self.assertEqual(ADAPTER.trigger_mode("Please look again.\n  @CodeRabbitAI   review  "), "incremental")
        self.assertIsNone(ADAPTER.trigger_mode("CodeRabbit ignored `@coderabbitai review` twice."))
        self.assertIsNone(ADAPTER.trigger_mode("@coderabbitai configuration"))


class SharedHeadTest(unittest.TestCase):
    """Open pull requests sharing a head share its evidence."""

    @staticmethod
    def shared() -> Replay:
        # duetto#34's head b340d5ca was reviewed there with two findings at 06:13:09.
        # Synthetic: the same commit pushed to duetto#58's branch at 06:04:30.
        gh = Replay("duetto#58", "duetto#34", now="2026-09-26T06:14:00Z")
        sha = gh.head("duetto#34")
        commit = copy.deepcopy(gh.case("duetto#34")["heads"][sha])
        commit["checkSuites"] = [{"created_at": "2026-09-26T06:04:30Z", "head_branch": gh.case("duetto#58")["ref"]}]
        gh.case("duetto#58")["heads"][sha] = commit
        gh.timelines = {key: REPLAY.head_timeline(pull) for key, pull in gh.pulls.items()}
        return gh

    def test_a_review_posted_on_another_pull_request_of_the_same_head_completes_it(self) -> None:
        gh = self.shared()
        self.assertEqual(gh.head("duetto#58"), gh.head("duetto#34"))
        result = observe(gh, "duetto#58")
        self.assertEqual(result["sharedWith"], [34])
        self.assertEqual((result["state"], result["findingsReview"]["actionable"]), ("review-complete", 2))
        del gh.pulls[gh.refs["duetto#34"]]
        self.assertNotIn(observe(gh, "duetto#58")["state"], ADAPTER.COMPLETE_STATES)

    def test_the_latest_retry_among_the_shared_summaries_holds(self) -> None:
        # Synthetic: a trigger on #58 at 06:14:50 refused at 06:15, with a 30-minute wait stated
        # on #34 and a 5-minute one on #58.
        gh = self.shared()
        gh.add_comment("duetto#58", "@coderabbitai review", "2026-09-26T06:14:50Z")
        for ref, minutes in (("duetto#34", 30), ("duetto#58", 5)):
            notice = f"{ADAPTER.SUMMARY_MARKER}\n> **Next review available in:** **{minutes} minutes**"
            gh.add_version(ref, gh.summary(ref)["id"], notice, "2026-09-26T06:14:59Z")
        gh.case("duetto#34")["heads"][gh.head("duetto#34")]["statuses"].append(
            {"id": 1, "state": "success", "description": "Review rate limited", "context": "CodeRabbit",
             "created_at": "2026-09-26T06:15:00Z", "creator": {"login": "coderabbitai[bot]"}}
        )
        result = observed_at(gh, "duetto#58", "2026-09-26T06:15:05Z")
        self.assertEqual((result["state"], result["retryAt"]), ("waiting", "2026-09-26T06:45:59Z"))

    def test_another_pull_requests_events_and_triggers_do_not_start_this_heads_cycle(self) -> None:
        # Synthetic: duetto#58 opened on #34's reviewed head at 06:13:20, with a trigger, and
        # CodeRabbit skipped it there because its base branch has reviews disabled.
        gh = Replay("duetto#58", "duetto#34", now="2026-09-26T06:14:00Z")
        sha = gh.head("duetto#34")
        commit = copy.deepcopy(gh.case("duetto#34")["heads"][sha])
        commit["checkSuites"] = [{"created_at": "2026-09-26T06:13:20Z", "head_branch": gh.case("duetto#58")["ref"]}]
        gh.case("duetto#58")["heads"][sha] = commit
        gh.case("duetto#58")["createdAt"] = "2026-09-26T06:13:20Z"
        gh.timelines = {key: REPLAY.head_timeline(pull) for key, pull in gh.pulls.items()}
        gh.add_comment("duetto#58", "@coderabbitai full review", "2026-09-26T06:13:30Z")
        gh.add_status("duetto#34", "Review skipped: reviews are disabled for this base branch", "2026-09-26T06:13:40Z")
        # 06:15:21 is past the 120 s that #58's opening would otherwise have started.
        for now in ("2026-09-26T06:14:00Z", "2026-09-26T06:15:21Z"):
            with self.subTest(now=now):
                result = observed_at(gh, "duetto#34", now)
                self.assertEqual(result["sharedWith"], [58])
                self.assertEqual(result["automaticReview"]["from"], "2026-09-26T06:04:27Z")
                self.assertEqual([trigger["createdAt"] for trigger in result["triggers"]], ["2026-09-26T06:07:32Z"])
                self.assertEqual((result["state"], result["findingsReview"]["actionable"]), ("review-complete", 2))


class BoundTest(unittest.TestCase):
    """Every state that waits on CodeRabbit becomes blocked after its bound."""

    def test_an_unanswered_trigger_blocks_after_ten_minutes(self) -> None:
        # duetto#65: the trigger at 16:38:17 got no status.
        waiting = state("duetto#65", "2026-09-26T16:48:16Z")
        self.assertEqual((waiting["state"], waiting["boundAt"]), ("triggered", "2026-09-26T16:48:17Z"))
        blocked = state("duetto#65", "2026-09-26T16:48:17Z")
        self.assertEqual((blocked["state"], blocked["since"]), ("blocked-unanswered", "2026-09-26T16:38:17Z"))
        self.assertIsNone(blocked["nextMode"])

    def test_a_review_in_progress_for_an_hour_blocks_as_stalled(self) -> None:
        # GocciaScript#1172: in progress since 13:33:10 and never completed.
        self.assertEqual(state("GocciaScript#1172", "2026-08-17T14:33:09Z")["state"], "in-progress")
        blocked = state("GocciaScript#1172", "2026-08-17T14:33:10Z")
        self.assertEqual((blocked["state"], blocked["since"]), ("blocked-stalled", "2026-08-17T13:33:10Z"))

    def test_a_refusal_without_a_paired_wait_blocks_after_fifteen_minutes(self) -> None:
        # pascal-mcp-sdk#48: the notice for 7b39784a was edited in at 10:21:04, 64 s before the
        # refusal at 10:22:08, so no stated wait pairs with it.
        unknown = state("pascal-mcp-sdk#48", "2026-08-10T10:37:07Z")
        self.assertEqual(unknown["status"]["description"], "Review rate limited")
        self.assertIsNone(unknown["rateLimit"]["wait"])
        self.assertEqual((unknown["state"], unknown["boundAt"]), ("rate-limited-unknown-wait", "2026-08-10T10:37:08Z"))
        blocked = state("pascal-mcp-sdk#48", "2026-08-10T10:37:08Z")
        self.assertEqual((blocked["state"], blocked["since"]), ("blocked-unknown-wait", "2026-08-10T10:22:08Z"))
        self.assertEqual((blocked["retryAt"], blocked["nextMode"]), (None, None))


class StatedWaitTest(unittest.TestCase):
    """A rate limit waits only on the wait CodeRabbit edited into its summary just before it."""

    REF = "GocciaScript#1162"
    EDITED = "2026-08-15T16:49:30Z"

    def moved(self, when: str) -> dict[str, Any]:
        gh = Replay(self.REF, now="2026-08-15T17:00:00Z")
        gh.move_version(self.REF, gh.summary(self.REF)["id"], self.EDITED, when)
        return observe(gh, self.REF)

    def test_the_trigger_is_permitted_at_retry_time_and_not_a_second_before(self) -> None:
        self.assertEqual(state(self.REF, "2026-08-15T17:47:29Z")["state"], "waiting")
        self.assertEqual(state(self.REF, "2026-08-15T17:47:30Z")["state"], "trigger-incremental")

    def test_an_edit_sixty_seconds_before_the_status_correlates(self) -> None:
        result = self.moved("2026-08-15T16:48:32Z")
        self.assertEqual((result["state"], result["retryAt"]), ("waiting", "2026-08-15T17:46:32Z"))

    def test_an_edit_sixty_one_seconds_before_the_status_leaves_the_wait_unknown(self) -> None:
        result = self.moved("2026-08-15T16:48:31Z")
        self.assertEqual((result["state"], result["since"]), ("rate-limited-unknown-wait", "2026-08-15T16:49:32Z"))

    def test_an_edit_after_the_status_does_not_correlate(self) -> None:
        self.assertEqual(self.moved("2026-08-15T16:49:33Z")["state"], "rate-limited-unknown-wait")

    def test_an_edit_older_than_sixty_seconds_does_not_correlate_after_a_later_edit(self) -> None:
        # Synthetic: the notice edited in five minutes before the refusal, and edited away at 16:55.
        gh = Replay(self.REF, now="2026-08-15T17:00:00Z")
        summary = gh.summary(self.REF)["id"]
        gh.move_version(self.REF, summary, self.EDITED, "2026-08-15T16:44:30Z")
        notice = [body for edited, body in gh.comment(self.REF, summary)["versions"] if edited <= "2026-08-15T16:44:30Z"][-1]
        gh.add_version(self.REF, summary, ADAPTER.STATED_WAIT.sub("", notice), "2026-08-15T16:55:00Z")
        result = observe(gh, self.REF)
        self.assertEqual((result["state"], result["rateLimit"]["wait"]), ("rate-limited-unknown-wait", None))

    def test_an_unreadable_edit_history_is_an_operational_error_not_a_refusal(self) -> None:
        gh = Replay(self.REF, now="2026-08-15T17:05:00Z")
        gh.unreadable_histories.add(gh.summary(self.REF)["id"])
        with self.assertRaisesRegex(ADAPTER.WaitError, "edit history unavailable"):
            observe(gh, self.REF)
        head = gh.head(self.REF)
        code, output = main(gh, "status", "--repo", gh.refs[self.REF][0], "--pr", "1162", "--head", f"1162={head}", "--json")
        self.assertEqual((code, output["state"]), (2, "operational-error"))

    def test_a_notice_gone_from_the_current_body_is_read_from_its_history(self) -> None:
        # GocciaScript#1067: the notice for bb43ced6's refusal was edited away by 23:47:23.
        gh = Replay("GocciaScript#1067", now="2026-08-04T23:47:23Z")
        current = gh.comment_view(*gh.refs["GocciaScript#1067"], gh.summary("GocciaScript#1067"))
        self.assertNotRegex(current["body"], ADAPTER.STATED_WAIT)
        result = observe(gh, "GocciaScript#1067")
        self.assertEqual((result["state"], result["retryAt"]), ("waiting", "2026-08-05T00:10:02Z"))

    def test_a_bot_reply_is_not_the_notice(self) -> None:
        gh = Replay(self.REF, now="2026-08-15T17:00:00Z")
        reply = "<!-- This is an auto-generated reply by CodeRabbit -->\n> **Next review available in:** **1 minutes**"
        gh.add_comment(self.REF, reply, "2026-08-15T16:49:31Z", login="coderabbitai[bot]")
        result = observe(gh, self.REF)
        self.assertEqual(result["rateLimit"]["wait"]["commentId"], gh.summary(self.REF)["id"])
        self.assertEqual((result["state"], result["retryAt"]), ("waiting", "2026-08-15T17:47:30Z"))

    def test_another_pull_requests_wait_does_not_hold_this_head(self) -> None:
        # pascal-mcp-sdk#45 was refused at 12:11:32 until 12:58:31, on the same account as
        # duetto#27, whose own wait had ended.
        other = state("pascal-mcp-sdk#45", "2026-08-09T12:29:52Z")
        self.assertEqual((other["state"], other["retryAt"]), ("waiting", "2026-08-09T12:59:31Z"))
        result = state("duetto#27", "2026-08-09T12:29:52Z")
        self.assertEqual((result["state"], result["retryAt"]), ("trigger-incremental", None))


class HeadScopeTest(unittest.TestCase):
    """Evidence about another head never completes the current one."""

    def test_a_changed_head_invalidates(self) -> None:
        result = observe(Replay("GocciaScript#1162", now="2026-08-15T17:00:00Z"), "GocciaScript#1162", head="0" * 40)
        self.assertEqual(result["state"], "invalidated")

    def test_the_previous_heads_review_and_marker_do_not_complete_the_new_head(self) -> None:
        # duetto#55: 39a6ab08 pushed at 06:41:14 while the summary still marked 4bccca05.
        gh = Replay("duetto#55", now="2026-09-26T06:41:30Z")
        self.assertEqual(gh.head("duetto#55")[:8], "39a6ab08")
        result = observe(gh, "duetto#55")
        self.assertIsNone(result["findingsReview"])
        self.assertIsNone(result["cleanReview"])
        self.assertEqual(result["state"], "awaiting-automatic")

    def test_a_findings_review_counts_only_for_its_own_commit(self) -> None:
        gh = Replay("duetto#55", now="2026-09-26T06:40:00Z")
        for review in gh.case("duetto#55")["reviews"]:
            review["commit_id"] = "0" * 40
        self.assertIsNone(observe(gh, "duetto#55")["findingsReview"])

    def test_a_marker_of_another_kind_does_not_complete(self) -> None:
        gh = Replay("duetto#55", now="2026-09-25T19:50:00Z")
        for version in gh.summary("duetto#55")["versions"]:
            version[1] = version[1].replace('"kind":"reviewed"', '"kind":"skipped"')
        self.assertIsNone(observe(gh, "duetto#55")["cleanReview"])

    def test_a_check_suite_on_another_branch_does_not_date_the_head(self) -> None:
        gh = Replay("GocciaScript#1212", now="2026-08-22T23:56:40Z")
        head = gh.case("GocciaScript#1212")["heads"][gh.head("GocciaScript#1212")]
        head["checkSuites"].append({"created_at": "2026-08-22T23:00:00Z", "head_branch": "another-branch"})
        self.assertEqual(observe(gh, "GocciaScript#1212")["headArrival"], {"at": "2026-08-22T23:56:32Z", "source": "check-suite"})


class EscalationTest(unittest.TestCase):
    """An unconfirmed completion gets one full review per head, then blocks."""

    def test_a_full_review_answered_without_evidence_blocks(self) -> None:
        # GocciaScript#1164: the full review requested at 20:50:13 completed at 20:53:24 with no
        # coverage marker or findings review for 096a3344.
        result = state("GocciaScript#1164", "2026-08-15T20:53:24Z")
        self.assertEqual([trigger["mode"] for trigger in result["triggers"]], ["incremental", "full"])
        self.assertEqual((result["state"], result["nextMode"]), ("blocked-unconfirmed", None))


class RunTest(unittest.TestCase):
    def test_a_run_triggers_once_exactly_when_the_stated_wait_ends(self) -> None:
        # GocciaScript#1162: the recorded full review at 17:47:55 is replaced by the run's trigger.
        gh = Replay("GocciaScript#1162", now="2026-08-15T17:00:00Z")
        answering(gh, "GocciaScript#1162")
        final, _reason, value = run(gh, "GocciaScript#1162", "2026-08-15T17:47:50Z")
        self.assertEqual(posts(gh), [("@coderabbitai review", "2026-08-15T17:47:30Z")])
        self.assertEqual(final, "triggered")
        self.assertEqual(value["triggers"][0]["mode"], "incremental")

    def test_a_run_started_while_an_earlier_head_is_reviewed_does_not_post_before_the_automatic_review(self) -> None:
        gh = Replay("GocciaScript#1265", now="2026-09-27T09:23:00Z")
        answering(gh, "GocciaScript#1265")
        final, _reason, _value = run(gh, "GocciaScript#1265", "2026-09-27T09:30:00Z", interval=5)
        self.assertEqual(gh.posts, [])
        self.assertEqual(final, "in-progress")

    def test_a_trigger_no_listing_shows_yet_is_never_posted_twice(self) -> None:
        gh = Replay("GocciaScript#1162", now="2026-08-15T17:47:30Z")
        answering(gh, "GocciaScript#1162", listed=False)
        final, _reason, _value = run(gh, "GocciaScript#1162", "2026-08-15T17:47:50Z", interval=5)
        self.assertEqual(len(gh.posts), 1)
        self.assertEqual(final, "triggered")

    def test_run_waits_out_a_refusal_then_retries(self) -> None:
        # Synthetic answers after GocciaScript#1162's wait ended.
        ref = "GocciaScript#1162"
        gh = Replay(ref, now="2026-08-15T17:47:30Z")
        gh.truncate(ref, "2026-08-15T17:47:30Z")
        summary = gh.summary(ref)["id"]

        def refuses(posted: float) -> None:
            gh.add_status(ref, "Review in progress", iso(posted + 10), state="pending")
            gh.add_version(ref, summary, f"{ADAPTER.SUMMARY_MARKER}\n> **Next review available in:** **5 minutes**", iso(posted + 15))
            gh.add_status(ref, "Review rate limited", iso(posted + 16))

        def reviews(posted: float) -> None:
            gh.add_status(ref, "Review in progress", iso(posted + 10), state="pending")
            gh.case(ref)["reviews"].append(
                {
                    "id": 1,
                    "user": {"login": "coderabbitai[bot]"},
                    "body": "**Actionable comments posted: 2**",
                    "commit_id": gh.head(ref),
                    "submitted_at": iso(posted + 200),
                }
            )
            gh.add_status(ref, "Review completed", iso(posted + 205))

        answering(gh, ref, refuses, reviews)
        final, _reason, _value = run(gh, ref, "2026-08-15T18:30:00Z")
        self.assertEqual(posts(gh), [("@coderabbitai review", "2026-08-15T17:47:30Z"), ("@coderabbitai review", "2026-08-15T17:53:45Z")])
        self.assertEqual(final, "review-complete")

    def test_in_progress_is_polled_until_the_review_completes(self) -> None:
        # duetto#55: the triggered review of 1ca17f50 completed clean at 19:47:10.
        gh = Replay("duetto#55", now="2026-09-25T19:43:00Z")
        final, _reason, _value = run(gh, "duetto#55", "2026-09-25T20:00:00Z", interval=30)
        self.assertEqual((final, gh.posts), ("clean-complete", []))
        self.assertGreaterEqual(gh.now, at("2026-09-25T19:47:10Z"))

    def test_an_unknown_wait_is_polled_then_blocks_and_never_triggers(self) -> None:
        gh = Replay("GocciaScript#1162", now="2026-08-15T16:50:00Z")
        gh.move_version("GocciaScript#1162", gh.summary("GocciaScript#1162")["id"], "2026-08-15T16:49:30Z", "2026-08-15T16:47:30Z")
        final, _reason, _value = run(gh, "GocciaScript#1162", "2026-08-15T17:40:00Z")
        self.assertEqual((final, gh.posts), ("blocked-unknown-wait", []))
        self.assertEqual(iso(gh.now), "2026-08-15T17:04:32Z")

    def test_a_trigger_posted_as_the_deadline_passes_is_reported(self) -> None:
        # GocciaScript#1162 at 17:47:29. Synthetic: the POST takes 2 s, past the deadline.
        ref = "GocciaScript#1162"
        gh = Replay(ref, now="2026-08-15T17:47:29Z")
        gh.truncate(ref, "2026-08-15T17:47:29Z")

        def post(_endpoint: str, fields: dict[str, str]) -> int:
            identifier = gh.add_comment(ref, fields["body"], iso(gh.now))
            gh.now += 2
            return identifier

        gh.now = at("2026-08-15T17:47:30Z")
        gh.on_post = post
        final, _reason, value = run(gh, ref, "2026-08-15T17:47:31Z", interval=1)
        self.assertEqual(final, "triggered")
        self.assertEqual([item["mode"] for item in value["triggers"]], ["incremental"])
        self.assertEqual(value["pullRequests"][0]["state"], "triggered")
        again, _reason, _value = run(gh, ref, "2026-08-15T17:57:31Z")
        self.assertEqual((again, len(gh.posts)), ("blocked-unanswered", 1))

    def test_an_expired_deadline_never_posts(self) -> None:
        gh = Replay("GocciaScript#1162", now="2026-08-15T17:47:30Z")
        final, reason, _value = run(gh, "GocciaScript#1162", "2026-08-15T17:47:30Z")
        self.assertEqual((final, gh.posts), ("trigger-incremental", []))
        self.assertIn("before the permitted trigger", reason)


class TriggerLockTest(unittest.TestCase):
    def setUp(self) -> None:
        self.directory = tempfile.TemporaryDirectory()
        patcher = mock.patch.object(ADAPTER, "lock_directory", lambda: Path(self.directory.name))
        patcher.start()
        self.addCleanup(patcher.stop)
        self.addCleanup(self.directory.cleanup)

    def test_the_lock_path_is_the_same_whatever_tmpdir_or_home_a_process_has(self) -> None:
        program = (
            "import importlib.util, sys; sys.dont_write_bytecode = True; "
            f"spec = importlib.util.spec_from_file_location('adapter', {str(MODULE_PATH)!r}); "
            "module = importlib.util.module_from_spec(spec); spec.loader.exec_module(module); "
            "print(module.lock_path('https://api.github.com', 'quillworks'))"
        )
        paths = set()
        for variable in ("TMPDIR", "HOME"):
            for value in ("/var/empty/one", "/var/empty/two"):
                environment = dict(os.environ, **{variable: value})
                completed = subprocess.run(
                    [sys.executable, "-c", program], env=environment, capture_output=True, text=True, check=True
                )
                paths.add(completed.stdout.strip())
        self.assertEqual(len(paths), 1)
        self.assertTrue(paths.pop().startswith(pwd.getpwuid(os.getuid()).pw_dir + os.sep))

    def test_status_names_the_run_holding_the_lock(self) -> None:
        self.assertEqual(ADAPTER.lock_status(GITHUB_API, "quillworks")["held"], False)
        holder = {"repo": "quillworks/duetto", "pr": 55, "head": CORPUS["duetto#55"]["head"]}
        handle = ADAPTER.acquire_lock(GITHUB_API, "quillworks", holder, 0, 1)
        self.assertIsNotNone(handle)
        try:
            status = ADAPTER.lock_status(GITHUB_API, "quillworks")
            self.assertEqual((status["apiRoot"], status["owner"], status["held"]), (GITHUB_API, "quillworks", True))
            self.assertEqual(
                {key: status["holder"][key] for key in ("repo", "pr", "head", "apiRoot", "owner")},
                holder | {"apiRoot": GITHUB_API, "owner": "quillworks"},
            )
            self.assertIsInstance(status["holder"]["pid"], int)
            self.assertIsNone(ADAPTER.acquire_lock(GITHUB_API, "quillworks", {"pr": 58}, 0, 1))
        finally:
            ADAPTER.release_lock(handle)
        self.assertEqual(
            ADAPTER.lock_status(GITHUB_API, "quillworks"),
            {"apiRoot": GITHUB_API, "owner": "quillworks", "path": status["path"], "held": False, "holder": None},
        )

    def test_two_owners_hold_their_locks_at_the_same_time(self) -> None:
        first = ADAPTER.acquire_lock(GITHUB_API, "quillworks", {"repo": "quillworks/duetto", "pr": 55}, 0, 1)
        self.assertIsNotNone(first)
        self.addCleanup(ADAPTER.release_lock, first)
        second = ADAPTER.acquire_lock(GITHUB_API, "marlowe-labs", {"repo": "marlowe-labs/duetto", "pr": 7}, 0, 1)
        self.assertIsNotNone(second)
        self.addCleanup(ADAPTER.release_lock, second)
        quill, marlowe = ADAPTER.lock_status(GITHUB_API, "quillworks"), ADAPTER.lock_status(GITHUB_API, "marlowe-labs")
        self.assertNotEqual(quill["path"], marlowe["path"])
        self.assertEqual((quill["held"], quill["holder"]["repo"]), (True, "quillworks/duetto"))
        self.assertEqual((marlowe["held"], marlowe["holder"]["repo"]), (True, "marlowe-labs/duetto"))

    def test_the_owner_is_the_repository_owner_in_lowercase(self) -> None:
        self.assertEqual(ADAPTER.lock_owner("Quillworks/Duetto"), "quillworks")
        self.assertEqual(ADAPTER.lock_owner("quillworks/ledger"), "quillworks")
        with self.assertRaises(ADAPTER.WaitError):
            ADAPTER.lock_owner("quillworks")

    def test_the_api_root_is_gh_current_user_url_without_user(self) -> None:
        for url, expected in (
            ("https://api.github.com/user", GITHUB_API),
            ("https://ghe.example.com/api/v3/user", ENTERPRISE_API),
        ):
            with self.subTest(url=url):
                gh = mock.Mock()
                gh.rest.return_value = {"current_user_url": url}
                self.assertEqual(ADAPTER.api_root(gh), expected)
                gh.rest.assert_called_once_with("/")
        for value in ({}, {"current_user_url": None}, {"current_user_url": "https://api.github.com/users"}):
            with self.subTest(response=value), self.assertRaises(ADAPTER.WaitError):
                gh = mock.Mock()
                gh.rest.return_value = value
                ADAPTER.api_root(gh)

    def test_github_and_an_enterprise_root_hold_separate_locks_for_one_owner_name(self) -> None:
        public = ADAPTER.acquire_lock(GITHUB_API, "quillworks", {"repo": "Quillworks/one", "pr": 1}, 0, 1)
        self.assertIsNotNone(public)
        self.addCleanup(ADAPTER.release_lock, public)
        gh = owned("duetto#55", "quillworks", now="2026-09-25T19:50:00Z")
        head = gh.head_at("quillworks/duetto", 55)
        code, status = main(
            gh, "status", "--repo", "quillworks/duetto", "--pr", "55", "--head", f"55={head}", "--json", root=ENTERPRISE_API,
        )
        lock = status["observation"]["lock"]
        self.assertEqual((code, lock["apiRoot"], lock["held"]), (0, ENTERPRISE_API, False))
        self.assertNotEqual(lock["path"], str(ADAPTER.lock_path(GITHUB_API, "quillworks")))
        code, finished = main(
            gh, "run", "--repo", "quillworks/duetto", "--pr", "55", "--head", head,
            "--deadline", "2026-09-25T20:50:00Z", "--interval", "1", "--json", root=ENTERPRISE_API,
        )
        self.assertEqual((code, finished["state"]), (0, "clean-complete"))
        self.assertTrue(ADAPTER.lock_status(GITHUB_API, "quillworks")["held"])

    def test_one_root_and_differently_cased_owners_share_a_lock(self) -> None:
        gh = owned("duetto#55", "quillworks", now="2026-09-25T19:50:00Z")
        head = gh.head_at("quillworks/duetto", 55)
        holder = {"repo": "quillworks/ledger", "pr": 58, "head": "abc"}
        handle = ADAPTER.acquire_lock(GITHUB_API, "quillworks", holder, 0, 1)
        self.addCleanup(ADAPTER.release_lock, handle)
        code, status = main(gh, "status", "--repo", "quillworks/duetto", "--pr", "55", "--head", f"55={head}", "--json")
        self.assertEqual((code, status["state"]), (0, "satisfied"))
        lock = status["observation"]["lock"]
        self.assertEqual(
            (lock["apiRoot"], lock["owner"], lock["path"]),
            (GITHUB_API, "quillworks", str(ADAPTER.lock_path(GITHUB_API, "quillworks"))),
        )
        self.assertEqual((lock["holder"]["repo"], lock["holder"]["pr"]), ("quillworks/ledger", 58))
        code, blocked = main(
            gh, "run", "--repo", "QuillWorks/duetto", "--pr", "55", "--head", head,
            "--deadline", "2000-01-01T00:00:00Z", "--interval", "1", "--json",
        )
        self.assertEqual((code, blocked["state"]), (0, "lock-held"))
        lock = blocked["observation"]["lock"]
        self.assertEqual((lock["apiRoot"], lock["owner"], lock["holder"]["pr"]), (GITHUB_API, "quillworks", 58))
        self.assertEqual((gh.root_reads, gh.posts), (1, []))

    def test_a_run_for_another_owner_does_not_wait_for_the_held_lock(self) -> None:
        gh = owned("duetto#55", "marlowe-labs", now="2026-09-25T19:50:00Z")
        head = gh.head_at("marlowe-labs/duetto", 55)
        handle = ADAPTER.acquire_lock(GITHUB_API, "quillworks", {"repo": "quillworks/duetto", "pr": 55}, 0, 1)
        self.addCleanup(ADAPTER.release_lock, handle)
        code, status = main(gh, "status", "--repo", "marlowe-labs/duetto", "--pr", "55", "--head", f"55={head}", "--json")
        self.assertEqual((code, status["state"]), (0, "satisfied"))
        self.assertEqual(
            status["observation"]["lock"],
            {
                "apiRoot": GITHUB_API, "owner": "marlowe-labs", "path": str(ADAPTER.lock_path(GITHUB_API, "marlowe-labs")),
                "held": False, "holder": None,
            },
        )
        code, finished = main(
            gh, "run", "--repo", "marlowe-labs/duetto", "--pr", "55", "--head", head,
            "--deadline", "2026-09-25T20:50:00Z", "--interval", "1", "--json",
        )
        self.assertEqual((code, finished["state"]), (0, "clean-complete"))
        self.assertEqual((gh.root_reads, gh.posts), (1, []))
        self.assertTrue(ADAPTER.lock_status(GITHUB_API, "quillworks")["held"])
        self.assertFalse(ADAPTER.lock_status(GITHUB_API, "marlowe-labs")["held"])

    def test_a_failing_api_root_read_is_an_operational_error_without_lock_or_trigger(self) -> None:
        # Synthetic: GitHub fails `gh api /` while CodeRabbit has not yet reviewed the head.
        gh = owned("duetto#55", "quillworks", now="2026-09-25T19:43:00Z")
        head = gh.head_at("quillworks/duetto", 55)
        locked: list[Any] = []
        acquire = ADAPTER.acquire_lock
        with mock.patch.object(ADAPTER, "acquire_lock", lambda *args, **kw: locked.append(args) or acquire(*args, **kw)):
            for command in (
                ["status", "--repo", "quillworks/duetto", "--pr", "55", "--head", f"55={head}", "--json"],
                ["run", "--repo", "quillworks/duetto", "--pr", "55", "--head", head,
                 "--deadline", "2026-09-25T20:50:00Z", "--interval", "60", "--json"],
            ):
                with self.subTest(command=command[0]):
                    code, failed = main(gh, *command, root=None)
                    self.assertEqual((code, failed["state"]), (2, "operational-error"))
                    self.assertIn("HTTP 502", failed["reason"])
                    self.assertNotIn("lock", failed["observation"])
        self.assertEqual((locked, gh.posts), ([], []))
        self.assertEqual(list(Path(self.directory.name).iterdir()), [])

    def test_a_running_review_holds_its_owner_lock_until_it_ends(self) -> None:
        # duetto#55 at 19:43:00: CodeRabbit completes the head at 19:47:10, so `run` polls until then.
        gh = owned("duetto#55", "quillworks", now="2026-09-25T19:43:00Z")
        head = gh.head_at("quillworks/duetto", 55)
        waits: list[dict[str, Any]] = []

        def contend() -> None:
            seen = {}
            for name, root, owner in (
                ("same owner", GITHUB_API, "quillworks"),
                ("other owner", GITHUB_API, "marlowe-labs"),
                ("other root", ENTERPRISE_API, "quillworks"),
            ):
                handle = ADAPTER.acquire_lock(root, owner, {"repo": f"{owner}/ledger", "pr": 9}, 0, 1)
                seen[name] = handle is not None
                if handle:
                    ADAPTER.release_lock(handle)
            waits.append(seen | {"holder": ADAPTER.lock_status(GITHUB_API, "quillworks")["holder"]})

        code, finished = main(
            gh, "run", "--repo", "quillworks/duetto", "--pr", "55", "--head", head,
            "--deadline", "2026-09-25T20:50:00Z", "--interval", "60", "--json", while_waiting=contend,
        )
        self.assertEqual((code, finished["state"]), (0, "clean-complete"))
        self.assertEqual(gh.posts, [])
        self.assertGreaterEqual(len(waits), 2)
        for seen in waits:
            self.assertEqual(
                {key: seen[key] for key in ("same owner", "other owner", "other root")},
                {"same owner": False, "other owner": True, "other root": True},
            )
            self.assertEqual((seen["holder"]["repo"], seen["holder"]["pr"]), ("quillworks/duetto", 55))
        released = ADAPTER.acquire_lock(GITHUB_API, "quillworks", {"pr": 9}, 0, 1)
        self.assertIsNotNone(released)
        ADAPTER.release_lock(released)


class ContractTest(unittest.TestCase):
    def test_only_the_two_included_review_commands_exist(self) -> None:
        self.assertEqual(set(ADAPTER.TRIGGERS.values()), {"@coderabbitai review", "@coderabbitai full review"})
        commands = set(re.findall(r"@coderabbitai[a-z ]*", MODULE_PATH.read_text()))
        self.assertEqual(commands, {"@coderabbitai review", "@coderabbitai full review"})

    def test_cli_keeps_status_and_run_without_an_account_scan(self) -> None:
        status = ADAPTER.parser().parse_args(["status", "--repo", "o/r", "--pr", "1", "--head", "1=abc", "--json"])
        self.assertEqual((status.command, status.pr, status.head), ("status", [1], ["1=abc"]))
        run_args = ADAPTER.parser().parse_args(
            ["run", "--repo", "o/r", "--pr", "1", "--head", "abc", "--deadline", "2026-10-01T00:00:00Z", "--json"]
        )
        self.assertEqual((run_args.command, run_args.interval), ("run", 60.0))
        with self.assertRaises(SystemExit), mock.patch("sys.stderr"):
            ADAPTER.parser().parse_args(["status", "--repo", "o/r", "--pr", "1", "--head", "1=abc", "--scan-repo", "o/s"])

    def test_status_aggregates_every_pull_request(self) -> None:
        def aggregate(*states: str) -> str:
            return ADAPTER.aggregate_status({"pullRequests": [{"state": value} for value in states]})[0]

        self.assertEqual(aggregate("review-complete", "clean-complete"), "satisfied")
        self.assertEqual(aggregate("review-complete", "invalidated", "skipped"), "invalidated")
        for blocked in sorted(ADAPTER.BLOCKED_STATES):
            with self.subTest(state=blocked):
                self.assertEqual(aggregate("waiting", blocked), "blocked")
        for pending in ("rate-limited-unknown-wait", "awaiting-automatic", "triggered", "in-progress"):
            with self.subTest(state=pending):
                self.assertEqual(aggregate("review-complete", pending), "pending")

    def test_a_command_never_reports_a_state_it_does_not_declare(self) -> None:
        metrics = ADAPTER.Metrics(0.0)
        with self.assertRaises(ADAPTER.WaitError):
            ADAPTER.envelope("status", "lock-held", {}, {}, metrics, "")
        with self.assertRaises(ADAPTER.WaitError):
            ADAPTER.envelope("run", "satisfied", {}, {}, metrics, "")
        self.assertEqual(ADAPTER.envelope("run", "pending", {}, {}, metrics, "")["state"], "pending")
        self.assertLessEqual(ADAPTER.FINAL_STATES, ADAPTER.HEAD_STATES)

    def test_the_docs_list_every_state_with_the_commands_that_report_it(self) -> None:
        text = (ROOT / "address-feedback" / "references" / "pr-readiness.md").read_text()
        section = text.split("\n## CodeRabbit\n", 1)[1].split("\n## ", 1)[0]
        documented: dict[str, frozenset[str]] = {}
        for line in section.splitlines():
            cells = [cell.strip() for cell in line.strip().strip("|").split("|")]
            if not line.startswith("|") or len(cells) != 3 or not cells[0].startswith("`"):
                continue
            commands = frozenset(re.findall(r"`([a-z]+)`", cells[1]))
            for name in re.findall(r"`([a-z-]+)`", cells[0]):
                self.assertNotIn(name, documented, f"{name} is listed twice")
                documented[name] = commands
        self.assertEqual(documented, ADAPTER.STATE_COMMANDS)


class RecorderTest(unittest.TestCase):
    def test_every_recorded_body_keeps_only_lines_the_adapter_reads(self) -> None:
        spec = importlib.util.spec_from_file_location(
            "record_coderabbit", Path(__file__).resolve().parent / "record_coderabbit.py"
        )
        assert spec and spec.loader
        recorder = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(recorder)
        for pull in CORPUS.values():
            for item in pull["comments"]:
                for _edited, body in item["versions"]:
                    if item["user"]["login"] == "maintainer":
                        self.assertEqual(recorder.reduce_command(body), body)
                        self.assertTrue(all(line in {"", "[text]"} or "@coderabbitai " in line for line in body.splitlines()), body)
                    else:
                        self.assertEqual(recorder.reduce_body(body), body)
            for review in pull["reviews"]:
                self.assertEqual(recorder.reduce_body(review["body"]), review["body"])

    def test_the_recorder_refuses_a_repository_that_is_not_public(self) -> None:
        spec = importlib.util.spec_from_file_location(
            "record_coderabbit", Path(__file__).resolve().parent / "record_coderabbit.py"
        )
        assert spec and spec.loader
        recorder = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(recorder)
        error = recorder.urllib.error.HTTPError("https://api.github.com/repos/o/r", 404, "Not Found", None, io.BytesIO())
        self.addCleanup(error.close)
        with mock.patch.object(recorder.urllib.request, "urlopen", side_effect=error):
            with self.assertRaisesRegex(SystemExit, "refusing o/r"):
                recorder.require_public("o/r")


if __name__ == "__main__":
    unittest.main()
