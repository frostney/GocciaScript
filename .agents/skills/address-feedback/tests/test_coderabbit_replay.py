#!/usr/bin/env python3
"""The adapter replayed over every recorded public pull request breaks no invariant."""

from __future__ import annotations

import copy
import importlib.util
import unittest
from pathlib import Path
from typing import Any

PATH = Path(__file__).resolve().parent / "coderabbit_replay.py"
SPEC = importlib.util.spec_from_file_location("coderabbit_replay", PATH)
assert SPEC and SPEC.loader
REPLAY = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(REPLAY)


def recorded(ref: str) -> dict[str, Any]:
    name, number = ref.split("#")
    return copy.deepcopy(
        next(pull for pull in REPLAY.load() if pull["repo"].endswith(f"/{name}") and pull["number"] == int(number))
    )


def head(pull: dict[str, Any], prefix: str) -> str:
    return next(sha for sha in pull["heads"] if sha.startswith(prefix))


class OracleTest(unittest.TestCase):
    """The harness judges each decision only by the head's own recorded history."""

    def test_another_pull_requests_wait_is_no_violation(self) -> None:
        # duetto#27's own wait had ended at 12:29:52 while pascal-mcp-sdk#45 was refused until
        # 12:59:31. Synthetic: duetto#27's head is refused again at 12:30:00.
        duetto, other = recorded("duetto#27"), recorded("pascal-mcp-sdk#45")
        sha = REPLAY.RecordedGitHub([duetto], "2026-08-09T12:29:52Z").head_at(duetto["repo"], 27)
        duetto["heads"][sha]["statuses"].append(
            {"id": 1, "state": "success", "description": "Review rate limited", "context": "CodeRabbit",
             "created_at": "2026-08-09T12:30:00Z", "creator": {"login": "coderabbitai[bot]"}}
        )
        history = REPLAY.History([duetto, other])
        decision = {"state": "trigger-incremental"}
        self.assertEqual(history.violations(duetto, sha, REPLAY.at("2026-08-09T12:29:52Z"), decision), [])

    def test_completion_evidence_from_before_a_ready_event_is_superseded(self) -> None:
        # duetto#55: clean pass of 1ca17f50 at 19:47:10. Synthetic: marked ready at 19:49:00.
        pull = recorded("duetto#55")
        sha, now = head(pull, "1ca17f50"), REPLAY.at("2026-09-25T19:50:00Z")
        decision = {"state": "clean-complete"}
        self.assertEqual(REPLAY.History([pull]).violations(pull, sha, now, decision), [])
        pull["events"].append({"type": "ready", "at": "2026-09-25T19:49:00Z"})
        self.assertEqual(REPLAY.History([pull]).violations(pull, sha, now, decision), ["completion"])

    def test_completion_evidence_from_before_a_trigger_line_is_superseded(self) -> None:
        # duetto#55: a full review was requested at 22:34:56 above an explanation.
        pull = recorded("duetto#55")
        sha = head(pull, "1ca17f50")
        found = REPLAY.History([pull]).violations(pull, sha, REPLAY.at("2026-09-25T22:34:56Z"), {"state": "clean-complete"})
        self.assertEqual(found, ["completion"])

    def test_a_wait_whose_bound_has_passed_is_a_violation(self) -> None:
        # homebrew-tap#17: a5c60e1f in progress since 23:54:19 and never completed.
        pull = recorded("homebrew-tap#17")
        sha, now = head(pull, "a5c60e1f"), REPLAY.at("2026-09-27T08:22:32Z")
        history = REPLAY.History([pull])
        stalled = {"state": "in-progress", "boundAt": "2026-09-27T00:54:19Z"}
        self.assertEqual(history.violations(pull, sha, now, stalled), ["expired-bound"])
        self.assertEqual(history.violations(pull, sha, now, {"state": "in-progress"}), ["unbounded"])
        self.assertEqual(history.violations(pull, sha, now, {"state": "blocked-stalled", "boundAt": stalled["boundAt"]}), [])


class ReplayTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.pulls = REPLAY.load()
        cls.violations, cls.checked = REPLAY.replay(cls.pulls)

    def test_the_corpus_is_broad_and_recent(self) -> None:
        self.assertGreaterEqual(len(self.pulls), 200)
        self.assertGreaterEqual(len({pull["repo"] for pull in self.pulls}), 7)
        self.assertGreater(sum(self.checked.values()), 1000)
        since = REPLAY.json.loads(REPLAY.CORPUS.read_text())["since"]
        spec = importlib.util.spec_from_file_location("record_coderabbit", PATH.parent / "record_coderabbit.py")
        assert spec and spec.loader
        recorder = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(recorder)
        stale = [f"{pull['repo']}#{pull['number']}" for pull in self.pulls if not recorder.in_window(pull, since)]
        self.assertEqual(stale, [], f"pull requests with CodeRabbit activity before {since}")

    def test_every_violation_is_pinned_and_every_pin_still_occurs(self) -> None:
        found = {item["key"] for item in self.violations}
        pinned = set(REPLAY.pinned())
        self.assertEqual(sorted(found - pinned), [], "unpinned invariant violations")
        self.assertEqual(sorted(pinned - found), [], "pinned violations that no longer occur")


if __name__ == "__main__":
    unittest.main()
