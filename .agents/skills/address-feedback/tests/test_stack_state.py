#!/usr/bin/env python3
"""Behavioral tests for native stack observation and invalidation."""

from __future__ import annotations

import json
import importlib.util
import os
import stat
import subprocess
import sys
import tempfile
import unittest
from datetime import datetime, timedelta, timezone
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
STACK_STATE = ROOT / "address-feedback" / "scripts" / "stack_state.py"


FAKE_GH = r'''#!/usr/bin/env python3
import json, os, pathlib, sys

scenario = json.loads(pathlib.Path(os.environ["FAKE_GH_SCENARIO"]).read_text())
counter_path = pathlib.Path(os.environ["FAKE_GH_COUNTER"])
counter = int(counter_path.read_text()) if counter_path.exists() else 0
counter_path.write_text(str(counter + 1))
args = sys.argv[1:]
endpoint = next((arg for arg in args if arg.startswith("repos/")), "")
if args[:3] != ["api", "--method", "GET"]:
    print("unexpected fake gh invocation: " + repr(args), file=sys.stderr)
    sys.exit(3)
states = scenario.get("states", [scenario["stack"]])
value = states[min(counter // 4, len(states) - 1)]
if endpoint == "repos/owner/repo/stacks/7":
    print(json.dumps(value))
elif endpoint.startswith("repos/owner/repo/git/ref/heads/"):
    ref = value["base"]["ref"]
    sha = value.get("resolvedBase", "a"*40)
    if counter % 4 == 3 and (scenario.get("racyReads") == "always" or (scenario.get("racyReads") and counter < 4)):
        sha = "b"*40
    print(json.dumps({"ref": "refs/heads/"+ref, "object": {"type":"commit", "sha":sha}}))
else:
    print("unexpected fake gh endpoint: " + endpoint, file=sys.stderr)
    sys.exit(3)
'''


def member(number: int, head: str, *, draft: bool = False) -> dict:
    return {
        "number": number,
        "state": "open",
        "draft": draft,
        "merged_at": None,
        "head": {"ref": f"layer-{number}", "sha": head},
    }


def stack(*members: dict, number: int = 7) -> dict:
    return {
        "id": 70,
        "number": number,
        "node_id": "STACK_node",
        "url": "https://api.github.test/repos/owner/repo/stacks/7",
        "base": {"ref": "main", "sha": "base-sha"},
        "open": True,
        "pull_requests": list(members),
    }


class StackStateTest(unittest.TestCase):
    def setUp(self) -> None:
        self.temp = tempfile.TemporaryDirectory()
        self.directory = Path(self.temp.name)
        fake = self.directory / "gh"
        fake.write_text(FAKE_GH)
        fake.chmod(fake.stat().st_mode | stat.S_IXUSR)
        self.scenario = self.directory / "scenario.json"
        self.counter = self.directory / "counter"
        self.environment = os.environ.copy()
        self.environment.update(
            {
                "PATH": f"{self.directory}{os.pathsep}{self.environment['PATH']}",
                "FAKE_GH_SCENARIO": str(self.scenario),
                "FAKE_GH_COUNTER": str(self.counter),
                "PYTHONDONTWRITEBYTECODE": "1",
            }
        )

    def tearDown(self) -> None:
        self.temp.cleanup()

    def run_json(self, *args: str) -> tuple[subprocess.CompletedProcess[str], dict]:
        result = subprocess.run(
            [sys.executable, str(STACK_STATE), *args, "--json"],
            text=True,
            capture_output=True,
            cwd=self.directory,
            env=self.environment,
            timeout=6,
        )
        self.assertTrue(result.stdout, result.stderr)
        return result, json.loads(result.stdout)

    def write_scenario(self, value: dict, *later: dict) -> None:
        self.scenario.write_text(json.dumps({"stack": value, "states": [value, *later]}))

    def write_expected(self, output: dict) -> Path:
        path = self.directory / "expected.json"
        path.write_text(json.dumps(output))
        return path

    def test_inspect_preserves_native_bottom_to_top_order_and_exact_heads(self) -> None:
        self.write_scenario(stack(member(11, "head-11"), member(12, "head-12")))
        result, output = self.run_json(
            "inspect", "--repo", "owner/repo", "--stack", "7"
        )
        self.assertEqual(result.returncode, 0)
        self.assertEqual(output["state"], "observed")
        self.assertEqual(
            [(item["pr"], item["head"]) for item in output["observation"]["members"]],
            [(11, "head-11"), (12, "head-12")],
        )

    def test_inspect_rejects_a_different_returned_stack_identity(self) -> None:
        self.write_scenario(
            stack(member(11, "head-11"), member(12, "head-12"), number=8)
        )
        result, output = self.run_json(
            "inspect", "--repo", "owner/repo", "--stack", "7"
        )
        self.assertEqual(result.returncode, 2)
        self.assertEqual(output["state"], "operational-error")
        self.assertIn("requested native stack 7", output["reason"])

    def test_append_only_fix_layer_requires_review_without_invalidating_prefix(self) -> None:
        original = stack(member(11, "head-11"), member(12, "head-12"))
        self.write_scenario(original)
        _, baseline = self.run_json(
            "inspect", "--repo", "owner/repo", "--stack", "7"
        )
        self.counter.unlink()
        self.write_scenario(
            stack(member(11, "head-11"), member(12, "head-12"), member(13, "head-13"))
        )
        _, output = self.run_json(
            "inspect",
            "--repo",
            "owner/repo",
            "--stack",
            "7",
            "--expect",
            str(self.write_expected(baseline)),
        )
        comparison = output["observation"]["comparison"]
        self.assertEqual(output["state"], "changed")
        self.assertTrue(comparison["appendOnly"])
        self.assertEqual(comparison["added"], [13])
        self.assertIsNone(comparison["firstInvalidatedPosition"])

    def test_changed_lower_head_invalidates_it_and_every_descendant(self) -> None:
        original = stack(member(11, "head-11"), member(12, "head-12"), member(13, "head-13"))
        self.write_scenario(original)
        _, baseline = self.run_json(
            "inspect", "--repo", "owner/repo", "--stack", "7"
        )
        self.counter.unlink()
        self.write_scenario(
            stack(member(11, "head-11"), member(12, "new-head-12"), member(13, "head-13"))
        )
        _, output = self.run_json(
            "inspect",
            "--repo",
            "owner/repo",
            "--stack",
            "7",
            "--expect",
            str(self.write_expected(baseline)),
        )
        self.assertEqual(output["state"], "invalidated")
        self.assertEqual(
            output["observation"]["comparison"]["firstInvalidatedPosition"], 1
        )

    def test_reordered_members_invalidate_from_the_first_changed_position(self) -> None:
        original = stack(member(11, "head-11"), member(12, "head-12"))
        self.write_scenario(original)
        _, baseline = self.run_json(
            "inspect", "--repo", "owner/repo", "--stack", "7"
        )
        self.counter.unlink()
        self.write_scenario(stack(member(12, "head-12"), member(11, "head-11")))
        _, output = self.run_json(
            "inspect",
            "--repo",
            "owner/repo",
            "--stack",
            "7",
            "--expect",
            str(self.write_expected(baseline)),
        )
        self.assertEqual(output["state"], "invalidated")
        self.assertEqual(
            output["observation"]["comparison"]["firstInvalidatedPosition"], 0
        )

    def test_changed_base_invalidates_the_whole_stack(self) -> None:
        original = stack(member(11, "head-11"), member(12, "head-12"))
        self.write_scenario(original)
        _, baseline = self.run_json(
            "inspect", "--repo", "owner/repo", "--stack", "7"
        )
        changed = stack(member(11, "head-11"), member(12, "head-12"))
        changed["base"] = {"ref": "release", "sha": "new-base"}
        self.counter.unlink()
        self.write_scenario(changed)
        _, output = self.run_json(
            "inspect",
            "--repo",
            "owner/repo",
            "--stack",
            "7",
            "--expect",
            str(self.write_expected(baseline)),
        )
        self.assertEqual(output["state"], "invalidated")
        self.assertTrue(output["observation"]["comparison"]["baseChanged"])
        self.assertEqual(
            output["observation"]["comparison"]["firstInvalidatedPosition"], 0
        )

    def test_same_base_ref_with_new_remote_commit_invalidates_whole_stack(self) -> None:
        original = stack(member(11, "head-11"), member(12, "head-12"))
        original["base"] = {"ref": "main"}  # Actual native API omits sha.
        self.write_scenario(original)
        _, baseline = self.run_json("inspect", "--repo", "owner/repo", "--stack", "7")
        self.assertEqual(baseline["observation"]["base"]["sha"], "a" * 40)
        self.counter.unlink()
        changed = {**original, "resolvedBase": "b" * 40}
        self.write_scenario(changed)
        _, output = self.run_json("inspect", "--repo", "owner/repo", "--stack", "7",
                                  "--expect", str(self.write_expected(baseline)))
        self.assertEqual(output["state"], "invalidated")
        self.assertTrue(output["observation"]["comparison"]["baseChanged"])
        self.assertEqual(output["observation"]["comparison"]["firstInvalidatedPosition"], 0)

    def test_malformed_expected_member_returns_a_structured_error(self) -> None:
        self.write_scenario(stack(member(11, "head-11"), member(12, "head-12")))
        expected = self.directory / "malformed-expected.json"
        expected.write_text(json.dumps({"members": ["not-an-object"]}))
        result, output = self.run_json(
            "inspect",
            "--repo",
            "owner/repo",
            "--stack",
            "7",
            "--expect",
            str(expected),
        )
        self.assertEqual(result.returncode, 2)
        self.assertEqual(output["state"], "operational-error")
        self.assertIn("expected stack member 0", output["reason"])

    def test_wait_returns_when_a_new_fix_layer_appears(self) -> None:
        original = stack(member(11, "head-11"), member(12, "head-12"))
        changed = stack(
            member(11, "head-11"), member(12, "head-12"), member(13, "head-13")
        )
        self.write_scenario(original)
        _, baseline = self.run_json(
            "inspect", "--repo", "owner/repo", "--stack", "7"
        )
        self.counter.unlink()
        self.write_scenario(original, changed)
        expected = self.write_expected(baseline)
        # This checks the transition, not subprocess startup speed. run_json's
        # six-second timeout still bounds a hung test; deadline tests are separate.
        deadline = (datetime.now(timezone.utc) + timedelta(seconds=30)).isoformat()
        _, output = self.run_json(
            "wait",
            "--repo",
            "owner/repo",
            "--stack",
            "7",
            "--expect",
            str(expected),
            "--deadline",
            deadline,
            "--interval",
            "0.01",
        )
        self.assertEqual(output["state"], "changed")
        self.assertEqual(output["observation"]["comparison"]["added"], [13])

    def test_wait_retries_a_racing_packet_before_accepting_a_stable_transition(self) -> None:
        original = stack(member(11, "head-11"), member(12, "head-12"))
        self.write_scenario(original)
        _, baseline = self.run_json("inspect", "--repo", "owner/repo", "--stack", "7")
        self.counter.unlink()
        changed = {**original, "resolvedBase": "b" * 40}
        self.scenario.write_text(json.dumps({"stack": original, "states": [original, changed], "racyReads": True}))
        _, output = self.run_json("wait", "--repo", "owner/repo", "--stack", "7",
            "--expect", str(self.write_expected(baseline)), "--interval", "0.01",
            "--deadline", (datetime.now(timezone.utc) + timedelta(seconds=30)).isoformat())
        self.assertEqual(output["state"], "invalidated")
        self.assertEqual(output["metrics"]["retries"], 1)
        self.assertEqual(output["metrics"]["observations"], 1)
        self.assertEqual(output["observation"]["base"]["sha"], "b" * 40)

    def test_racing_until_deadline_does_not_claim_the_stack_was_unchanged(self) -> None:
        original = stack(member(11, "head-11"), member(12, "head-12"))
        self.write_scenario(original)
        _, baseline = self.run_json("inspect", "--repo", "owner/repo", "--stack", "7")
        self.counter.unlink()
        self.scenario.write_text(json.dumps({"stack": original, "racyReads": "always"}))
        _, output = self.run_json("wait", "--repo", "owner/repo", "--stack", "7",
            "--expect", str(self.write_expected(baseline)), "--interval", "0.01",
            "--deadline", datetime.now(timezone.utc).isoformat())
        self.assertEqual(output["state"], "pending")
        self.assertEqual(output["metrics"]["observations"], 0)
        self.assertNotIn("unchanged", output["reason"])


class ObservationRaceTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        spec = importlib.util.spec_from_file_location("stack_state_test_subject", STACK_STATE)
        cls.subject = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(cls.subject)

    def observe(self, responses: list) -> dict:
        class SequenceGh:
            def rest(self, endpoint):
                return responses.pop(0)
        return self.subject.stack_snapshot(SequenceGh(), "owner/repo", 7)

    def ref(self, sha: str = "a" * 40) -> dict:
        return {"ref": "refs/heads/main", "object": {"type": "commit", "sha": sha}}

    def test_base_change_during_observation_is_not_accepted_as_a_snapshot(self):
        value = stack(member(11, "head-11"), member(12, "head-12"))
        with self.assertRaisesRegex(self.subject.TransientError, "changed during observation"):
            self.observe([value, self.ref(), value, self.ref("b" * 40)])

    def test_member_change_during_observation_is_not_accepted_as_a_snapshot(self):
        before = stack(member(11, "head-11"), member(12, "head-12"))
        after = stack(member(11, "head-11"), member(12, "new-head"))
        with self.assertRaises(self.subject.TransientError):
            self.observe([before, self.ref(), after, self.ref()])

    def test_missing_or_wrong_ref_cannot_stand_in_for_the_base_commit(self):
        value = stack(member(11, "head-11"), member(12, "head-12"))
        for wrong in [{}, {"ref": "refs/heads/other"}, self.ref("unknown")]:
            with self.subTest(wrong=wrong), self.assertRaises(self.subject.WaitError):
                self.observe([value, wrong])


if __name__ == "__main__":
    unittest.main()
