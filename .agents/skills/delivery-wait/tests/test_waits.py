#!/usr/bin/env python3
"""Behavioral tests for deterministic GitHub transition commands."""

from __future__ import annotations

import json
import hashlib
import os
import stat
import subprocess
import sys
import tempfile
import unittest
from datetime import datetime, timedelta, timezone
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
DELIVERY = ROOT / "delivery-wait" / "scripts" / "delivery_wait.py"
REVIEW = ROOT / "address-feedback" / "scripts" / "review_wait.py"


FAKE_GH = r'''#!/usr/bin/env python3
import json, os, pathlib, sys

scenario = json.loads(pathlib.Path(os.environ["FAKE_GH_SCENARIO"]).read_text())
counter_path = pathlib.Path(os.environ["FAKE_GH_COUNTER"])
counter = int(counter_path.read_text()) if counter_path.exists() else 0
counter_path.write_text(str(counter + 1))
args = sys.argv[1:]
stdin = sys.stdin.read()

def output(value):
    print(json.dumps(value))
    sys.exit(0)

def with_connection_counts(value):
    if isinstance(value, dict):
        if "nodes" in value and "pageInfo" in value:
            value["totalCount"] = len(value["nodes"])
        for item in value.values():
            with_connection_counts(item)
    elif isinstance(value, list):
        for item in value:
            with_connection_counts(item)
    return value

if args[:2] == ["api", "graphql"]:
    request = json.loads(stdin)
    query = request.get("query", "")
    if scenario.get("graphqlRateLimited") and counter == 0:
        print("GraphQL API rate limit exceeded", file=sys.stderr)
        sys.exit(1)
    if "resolveReviewThread" in query:
        scenario["thread"]["isResolved"] = True
        pathlib.Path(os.environ["FAKE_GH_SCENARIO"]).write_text(json.dumps(scenario))
        output({"data":{"resolveReviewThread":{"thread":{"id":request["variables"]["thread"],"isResolved":True}}}})
    if "node(id:" in query:
        output({"data":{"node":scenario["thread"]}})
    if "reviewThreads" in query:
        if scenario.get("reviewHeadsByRequest"):
            heads = scenario["reviewHeadsByRequest"]
            scenario["review"]["headRefOid"] = heads[min(counter, len(heads) - 1)]
        for review in scenario["review"]["reviews"]["nodes"]:
            review.setdefault("comments", {"nodes":[], "pageInfo":{"hasNextPage":False}})
        output({"data":{"repository":{"pullRequest":with_connection_counts(scenario["review"])}}})
    if "releaseAssets" in query:
        output({"data":{"repository":scenario["tag"]}})
    output({"data":{"repository":{"pullRequest":scenario["pull"]}}})

endpoint = next((arg for arg in args if arg.startswith("repos/")), "")
if "user" in args:
    output({"login":"maintainer"})
if endpoint.endswith("/pulls/comments/10"):
    output({"id":10,"pull_request_url":"https://api.github.com/repos/owner/repo/pulls/7","body":"finding","user":{"login":"reviewer"}})
if endpoint.endswith("/pulls/7"):
    output(scenario["restPull"])
if "/check-runs" in endpoint:
    value = scenario.get("restChecks", {"check_runs":[]})
    output([value] if "--slurp" in args else value)
if "/statuses" in endpoint:
    value = scenario.get("restStatuses", [])
    output([value] if "--slurp" in args else value)
if endpoint.endswith("/comments?per_page=100"):
    output([scenario.get("comments", [])] if "--slurp" in args else scenario.get("comments", []))
if endpoint.endswith("/replies"):
    output({"id":901,"body":"created"})
if "/actions/runs/" in endpoint:
    output(scenario["workflow"])
print("unexpected fake gh invocation: " + repr(args), file=sys.stderr)
sys.exit(3)
'''


def check(
    name: str,
    status: str,
    conclusion: str | None,
    started_at: str = "2026-08-12T08:00:00Z",
    app: str = "automated-review-app",
    completed_at: str | None = None,
) -> dict:
    return {
        "__typename": "CheckRun",
        "id": f"check-{name}-{app}-{started_at}",
        "name": name,
        "status": status,
        "conclusion": conclusion,
        "detailsUrl": "https://example.invalid/check",
        "startedAt": started_at,
        "completedAt": completed_at or (started_at if status == "COMPLETED" else None),
        "checkSuite": {"app": {"slug": app}},
    }


def pull(head: str, checks: list[dict], merged: bool = False) -> dict:
    return {
        "headRefOid": head,
        "merged": merged,
        "mergedAt": "2026-08-12T08:00:00Z" if merged else None,
        "mergeCommit": {"oid": "merge-sha"} if merged else None,
        "commits": {"nodes":[{"commit":{"statusCheckRollup":{"contexts":{"nodes":checks}}}}]},
    }


class WaitCommandsTest(unittest.TestCase):
    def setUp(self) -> None:
        self.temp = tempfile.TemporaryDirectory()
        self.directory = Path(self.temp.name)
        fake = self.directory / "gh"
        fake.write_text(FAKE_GH)
        fake.chmod(fake.stat().st_mode | stat.S_IXUSR)
        self.scenario = self.directory / "scenario.json"
        self.counter = self.directory / "counter"
        self.environment = os.environ.copy()
        self.environment.update({
            "PATH": f"{self.directory}{os.pathsep}{self.environment['PATH']}",
            "FAKE_GH_SCENARIO": str(self.scenario),
            "FAKE_GH_COUNTER": str(self.counter),
        })
        self.environment["PYTHONDONTWRITEBYTECODE"] = "1"
        self.policy = self.directory / "policy.json"
        self.policy.write_text(json.dumps({"automations":[{
            "id":"automated-review", "actors":["review-bot[bot]"],
            "check_contexts":["Automated review"], "check_app_slugs":["automated-review-app"],
            "terminal_check_conclusions":["success"], "terminal_review_states":[],
            "nonterminal_review_markers":["rate limit"],
        }]}))

    def tearDown(self) -> None:
        self.temp.cleanup()

    def deadline(self) -> str:
        return (datetime.now(timezone.utc) + timedelta(seconds=3)).isoformat()

    def run_json(self, script: Path, *args: str) -> tuple[subprocess.CompletedProcess[str], dict]:
        result = subprocess.run(
            [sys.executable, str(script), *args, "--json"],
            text=True,
            capture_output=True,
            cwd=self.directory,
            env=self.environment,
            timeout=6,
        )
        self.assertTrue(result.stdout, result.stderr)
        return result, json.loads(result.stdout)

    def write_scenario(self, **values: object) -> None:
        defaults = {
            "pull": pull("head-1", [check("CI", "COMPLETED", "SUCCESS")]),
            "restPull": {"head":{"sha":"head-1"},"merged":False,"merged_at":None,"merge_commit_sha":None},
            "review": {
                "headRefOid":"head-1",
                "comments":{"nodes":[],"pageInfo":{"hasNextPage":False}},
                "reviews":{"nodes":[],"pageInfo":{"hasNextPage":False}},
                "reviewThreads":{"nodes":[],"pageInfo":{"hasNextPage":False}},
                "commits":{"nodes":[{"commit":{"statusCheckRollup":{"contexts":{"nodes":[check("Automated review","COMPLETED","SUCCESS")],"pageInfo":{"hasNextPage":False}}}}}]},
            },
            "tag":{"ref":None,"release":None},
            "workflow":{"id":44,"head_sha":"head-1","status":"completed","conclusion":"success","html_url":"https://example.invalid/run"},
            "thread":{"id":"thread-1","isResolved":False,"pullRequest":{"number":7,"headRefOid":"head-1","repository":{"nameWithOwner":"owner/repo"}}},
        }
        defaults.update(values)
        self.scenario.write_text(json.dumps(defaults))

    def test_terminal_checks_satisfy_without_model_polling(self) -> None:
        self.write_scenario()
        result, output = self.run_json(
            DELIVERY, "wait", "checks-terminal", "--repo", "owner/repo", "--pr", "7",
            "--head", "head-1", "--check", "CI", "--deadline", self.deadline(), "--interval", "0.01",
        )
        self.assertEqual(result.returncode, 0)
        self.assertEqual(output["state"], "satisfied")
        self.assertEqual(output["metrics"]["observations"], 1)

    def test_delivery_inspect_takes_one_snapshot_without_checkpoint(self) -> None:
        self.write_scenario()
        result, output = self.run_json(
            DELIVERY, "inspect", "checks-terminal", "--repo", "owner/repo",
            "--pr", "7", "--head", "head-1", "--check", "CI",
        )
        self.assertEqual(result.returncode, 0)
        self.assertEqual(output["state"], "satisfied")
        self.assertEqual(output["metrics"]["observations"], 1)

    def test_completed_check_with_stale_status_satisfies_inspect_and_wait(self) -> None:
        completed_at = "2026-08-12T08:01:00Z"
        self.write_scenario(
            pull=pull(
                "head-1",
                [check("CI", "IN_PROGRESS", "SUCCESS", completed_at=completed_at)],
            )
        )
        _, inspected = self.run_json(
            DELIVERY, "inspect", "checks-terminal", "--repo", "owner/repo",
            "--pr", "7", "--head", "head-1", "--check", "CI",
        )
        _, waited = self.run_json(
            DELIVERY, "wait", "checks-terminal", "--repo", "owner/repo", "--pr", "7",
            "--head", "head-1", "--check", "CI", "--deadline", self.deadline(), "--interval", "0.01",
        )
        self.assertEqual(inspected["state"], "satisfied")
        self.assertEqual(waited["state"], "satisfied")
        self.assertEqual(waited["metrics"]["observations"], 1)
        self.assertEqual(inspected["observation"]["checks"][0]["status"], "IN_PROGRESS")
        self.assertEqual(inspected["observation"]["checks"][0]["completedAt"], completed_at)

    def test_success_without_completion_remains_waiting(self) -> None:
        self.write_scenario(
            pull=pull("head-1", [check("CI", "IN_PROGRESS", "SUCCESS")])
        )
        _, output = self.run_json(
            DELIVERY, "inspect", "checks-terminal", "--repo", "owner/repo",
            "--pr", "7", "--head", "head-1", "--check", "CI",
        )
        self.assertEqual(output["state"], "waiting")

    def test_latest_duplicate_check_context_controls_the_gate(self) -> None:
        self.write_scenario(
            pull=pull(
                "head-1",
                [
                    check("CI", "COMPLETED", "SUCCESS", "2026-08-12T08:00:00Z"),
                    check("CI", "IN_PROGRESS", None, "2026-08-12T08:01:00Z"),
                ],
            )
        )
        _, output = self.run_json(
            DELIVERY, "inspect", "checks-terminal", "--repo", "owner/repo",
            "--pr", "7", "--head", "head-1", "--check", "CI",
        )
        self.assertEqual(output["state"], "waiting")

    def test_wait_rejects_a_deadline_without_timezone(self) -> None:
        self.write_scenario()
        result, output = self.run_json(
            DELIVERY, "wait", "checks-terminal", "--repo", "owner/repo",
            "--pr", "7", "--head", "head-1", "--check", "CI",
            "--deadline", "2026-08-12T12:00:00", "--interval", "0.01",
        )
        self.assertEqual(result.returncode, 2)
        self.assertEqual(output["state"], "operational-error")
        self.assertIn("timezone", output["reason"])

    def test_head_change_invalidates(self) -> None:
        self.write_scenario(pull=pull("head-2", []))
        _, output = self.run_json(
            DELIVERY, "wait", "checks-terminal", "--repo", "owner/repo", "--pr", "7",
            "--head", "head-1", "--check", "CI", "--deadline", self.deadline(), "--interval", "0.01",
        )
        self.assertEqual(output["state"], "invalidated")
        self.assertEqual(output["observation"]["head"], "head-2")

    def test_deadline_performs_a_final_authoritative_observation(self) -> None:
        self.write_scenario()
        past = (datetime.now(timezone.utc) - timedelta(seconds=1)).isoformat()
        _, output = self.run_json(
            DELIVERY, "wait", "checks-terminal", "--repo", "owner/repo",
            "--pr", "7", "--head", "head-1", "--check", "CI",
            "--deadline", past, "--interval", "0.01",
        )
        self.assertEqual(output["state"], "satisfied")
        self.assertEqual(output["metrics"]["observations"], 1)

    def test_graphql_rate_limit_falls_back_to_rest(self) -> None:
        self.write_scenario(
            graphqlRateLimited=True,
            restChecks={"check_runs":[{"name":"CI","status":"completed","conclusion":"success"}]},
            restStatuses=[],
        )
        _, output = self.run_json(
            DELIVERY, "wait", "checks-terminal", "--repo", "owner/repo", "--pr", "7",
            "--head", "head-1", "--check", "CI", "--deadline", self.deadline(), "--interval", "0.01",
        )
        self.assertEqual(output["state"], "satisfied")
        self.assertEqual(output["metrics"]["rateLimitFallbacks"], 1)

    def test_rest_fallback_accepts_completed_check_with_stale_status(self) -> None:
        completed_at = "2026-08-12T08:01:00Z"
        self.write_scenario(
            graphqlRateLimited=True,
            restChecks={"check_runs":[{
                "name":"CI", "status":"in_progress", "conclusion":"success",
                "started_at":"2026-08-12T08:00:00Z", "completed_at":completed_at,
            }]},
            restStatuses=[],
        )
        _, output = self.run_json(
            DELIVERY, "wait", "checks-terminal", "--repo", "owner/repo", "--pr", "7",
            "--head", "head-1", "--check", "CI", "--deadline", self.deadline(), "--interval", "0.01",
        )
        self.assertEqual(output["state"], "satisfied")
        self.assertEqual(output["metrics"]["observations"], 1)
        self.assertEqual(output["observation"]["checks"][0]["status"], "IN_PROGRESS")
        self.assertEqual(output["observation"]["checks"][0]["completedAt"], completed_at)

    def test_checkpoint_reconciliation_reports_missed_change(self) -> None:
        self.write_scenario(pull=pull("head-1", []))
        state = self.directory / "wait.json"
        state.write_text(json.dumps({
            "schemaVersion":1, "kind":"checks-terminal",
            "identity":{"repo":"owner/repo","pr":7,"head":"head-1","runId":None,"tag":None,"checks":["CI"],"assets":[]},
            "observation":{"head":"head-1","checks":[]}, "digest":"old", "updatedAt":"earlier",
        }))
        _, output = self.run_json(
            DELIVERY, "wait", "checks-terminal", "--repo", "owner/repo", "--pr", "7",
            "--head", "head-1", "--check", "CI", "--deadline", self.deadline(), "--interval", "0.01", "--state", str(state),
        )
        self.assertEqual(output["state"], "changed")

    def test_checkpoint_identity_change_invalidates(self) -> None:
        self.write_scenario(pull=pull("head-1", []))
        state = self.directory / "wait.json"
        state.write_text(json.dumps({
            "schemaVersion":1, "kind":"checks-terminal",
            "identity":{"repo":"other/repo","pr":7,"head":"head-1","runId":None,"tag":None,"checks":["CI"],"assets":[]},
            "observation":{"head":"head-1","checks":[]}, "digest":"old", "updatedAt":"earlier",
        }))
        _, output = self.run_json(
            DELIVERY, "wait", "checks-terminal", "--repo", "owner/repo", "--pr", "7",
            "--head", "head-1", "--check", "CI", "--deadline", self.deadline(), "--interval", "0.01", "--state", str(state),
        )
        self.assertEqual(output["state"], "invalidated")

    def test_dead_checkpoint_lock_is_reclaimed(self) -> None:
        self.write_scenario()
        state = self.directory / "wait.json"
        Path(f"{state}.lock").write_text("99999999:stale\n")
        _, output = self.run_json(
            DELIVERY, "wait", "checks-terminal", "--repo", "owner/repo", "--pr", "7",
            "--head", "head-1", "--check", "CI", "--deadline", self.deadline(),
            "--interval", "0.01", "--state", str(state),
        )
        self.assertEqual(output["state"], "satisfied")
        self.assertFalse(Path(f"{state}.lock").exists())

    def test_review_convergence_uses_policy_and_thread_state(self) -> None:
        self.write_scenario()
        _, output = self.run_json(
            REVIEW, "inspect", "--repo", "owner/repo", "--pr", "7", "--head", "head-1", "--policy", str(self.policy),
        )
        self.assertEqual(output["state"], "satisfied")
        self.assertEqual(output["observation"]["unresolvedThreads"], 0)

    def test_review_wait_retries_racing_census_then_reports_changed_head(self) -> None:
        self.write_scenario(reviewHeadsByRequest=["head-1", "head-2", "head-2", "head-2"])
        result, output = self.run_json(
            REVIEW, "wait", "--repo", "owner/repo", "--pr", "7", "--head", "head-1",
            "--policy", str(self.policy), "--deadline", self.deadline(), "--interval", "0.01",
        )
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(output["state"], "invalidated")
        self.assertEqual(output["metrics"]["retries"], 1)
        self.assertEqual(output["metrics"]["observations"], 1)
        self.assertEqual(output["observation"]["head"], "head-2")

    def test_review_policy_accepts_the_existing_single_context_shape(self) -> None:
        self.write_scenario()
        self.policy.write_text(json.dumps({"automations":[{
            "id":"automated-review", "actors":["review-bot[bot]"],
            "check_context":"Automated review",
            "terminal_review_states":["APPROVED", "COMMENTED"],
        }]}))
        _, output = self.run_json(
            REVIEW, "inspect", "--repo", "owner/repo", "--pr", "7",
            "--head", "head-1", "--policy", str(self.policy),
        )
        self.assertEqual(output["state"], "satisfied")

    def test_review_wait_requires_judgment_for_unresolved_finding(self) -> None:
        self.write_scenario()
        scenario = json.loads(self.scenario.read_text())
        scenario["review"]["reviewThreads"]["nodes"] = [{
            "id":"thread-1", "isResolved":False,
            "comments":{"nodes":[{"id":"node-1","databaseId":10,"body":"finding","createdAt":"now","author":{"login":"review-bot[bot]"},"authorAssociation":"NONE","replyTo":None}],"pageInfo":{"hasNextPage":False}},
        }]
        self.scenario.write_text(json.dumps(scenario))
        _, output = self.run_json(
            REVIEW, "wait", "--repo", "owner/repo", "--pr", "7", "--head", "head-1", "--policy", str(self.policy),
            "--deadline", (datetime.now(timezone.utc) + timedelta(seconds=.08)).isoformat(), "--interval", "0.02",
        )
        self.assertEqual(output["state"], "judgment-required")
        self.assertEqual(output["observation"]["findingSurfaceCount"], 1)

    def test_terminal_neutral_check_cannot_hide_review_or_comment_bodies(self) -> None:
        self.policy.write_text(json.dumps({"automations":[{
            "id":"automated-review", "actors":["review-bot[bot]"],
            "check_contexts":["Automated review"], "check_app_slugs":["review-app"],
            "terminal_check_conclusions":["success", "neutral"],
            "terminal_review_states":["COMMENTED"],
            "nonterminal_review_markers":[],
        }]}))
        self.write_scenario()
        scenario = json.loads(self.scenario.read_text())
        scenario["review"]["commits"]["nodes"][0]["commit"]["statusCheckRollup"]["contexts"]["nodes"] = [
            check("Automated review", "COMPLETED", "NEUTRAL", app="review-app")
        ]
        scenario["review"]["reviews"]["nodes"] = [{
            "id":"review-node-1", "databaseId":31,
            "author":{"login":"review-bot[bot]"}, "authorAssociation":"NONE",
            "state":"COMMENTED", "body":"Critical: unsafe state transition.",
            "submittedAt":"2026-08-12T08:02:00Z", "commit":{"oid":"head-1"},
        }]
        scenario["review"]["comments"]["nodes"] = [{
            "id":"comment-node-1", "databaseId":32,
            "author":{"login":"review-bot[bot]"}, "authorAssociation":"NONE",
            "body":"Review summary with one critical issue.",
            "createdAt":"2026-08-12T08:02:01Z",
        }]
        self.scenario.write_text(json.dumps(scenario))

        _, output = self.run_json(
            REVIEW, "inspect", "--repo", "owner/repo", "--pr", "7",
            "--head", "head-1", "--policy", str(self.policy),
        )

        self.assertEqual(output["state"], "judgment-required")
        surfaces = output["observation"]["findingSurfaces"]
        self.assertEqual(output["observation"]["findingSurfaceCount"], 2)
        self.assertEqual(
            {surface["kind"] for surface in surfaces},
            {"review", "top-level-comment"},
        )
        review_surface = next(surface for surface in surfaces if surface["kind"] == "review")
        self.assertIn("Critical", review_surface["review"]["body"])

    def test_unknown_review_policy_inspects_feedback_without_claiming_readiness(self) -> None:
        for content in (None, "[]", "{", '{"automations":false}'):
            with self.subTest(content=content):
                self.write_scenario()
                scenario = json.loads(self.scenario.read_text())
                scenario["review"]["comments"]["nodes"] = [{
                    "id":"human-comment", "body":"Please handle rollback.",
                    "author":{"login":"maintainer"},
                }]
                scenario["review"]["reviews"]["nodes"] = [{
                    "id":"unknown-review", "body":"Falsy values disappear.",
                    "author":{"login":"unconfigured-bot[bot]"},
                    "state":"COMMENTED", "commit":{"oid":"head-1"},
                }]
                scenario["review"]["reviewThreads"]["nodes"] = [{
                    "id":"thread", "isResolved":False,
                    "comments":{"nodes":[],"pageInfo":{"hasNextPage":False}},
                }]
                self.scenario.write_text(json.dumps(scenario))
                if content is None:
                    self.policy.unlink(missing_ok=True)
                else:
                    self.policy.write_text(content)
                result, output = self.run_json(
                    REVIEW, "inspect", "--repo", "owner/repo", "--pr", "7",
                    "--head", "head-1", "--policy", str(self.policy),
                )
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertEqual(output["state"], "waiting")
                self.assertEqual(output["metrics"]["apiRequests"], 2)
                observation = output["observation"]
                self.assertFalse(observation["policyAvailable"])
                self.assertTrue(observation["policyError"])
                self.assertEqual(observation["checks"][0]["name"], "Automated review")
                self.assertIsNone(observation["unansweredAutomationThreads"])
                self.assertEqual(observation["unresolvedThreads"], 1)
                self.assertIsNone(observation["threads"][0]["automation"])
                self.assertIsNone(observation["threads"][0]["maintainerReply"])
                self.assertEqual(observation["findingSurfaceCount"], 3)
                self.assertEqual(observation["unclassifiedTopLevelComments"][0]["body"], "Please handle rollback.")

    def test_missing_policy_never_turns_empty_feedback_into_success(self) -> None:
        self.write_scenario()
        self.policy.unlink()
        _, output = self.run_json(
            REVIEW, "inspect", "--repo", "owner/repo", "--pr", "7",
            "--head", "head-1", "--policy", str(self.policy),
        )
        self.assertEqual(output["state"], "waiting")
        self.assertEqual(output["observation"]["findingSurfaceCount"], 0)
        _, output = self.run_json(
            REVIEW, "inspect", "--repo", "owner/repo", "--pr", "7",
            "--head", "old-head", "--policy", str(self.policy),
        )
        self.assertEqual(output["state"], "invalidated")

    def test_wait_requires_valid_policy_before_creating_checkpoint(self) -> None:
        self.write_scenario()
        self.policy.unlink()
        state = self.directory / "review.json"
        result, output = self.run_json(
            REVIEW, "wait", "--repo", "owner/repo", "--pr", "7",
            "--head", "head-1", "--policy", str(self.policy),
            "--deadline", self.deadline(), "--state", str(state),
        )
        self.assertNotEqual(result.returncode, 0)
        self.assertEqual(output["state"], "operational-error")
        self.assertEqual(output["metrics"]["apiRequests"], 0)
        self.assertFalse(state.exists())

    def test_current_review_feedback_does_not_depend_on_automation_membership(self) -> None:
        for author, body, state, head, expected in (
            ("maintainer", "Rollback fails.", "COMMENTED", "head-1", "judgment-required"),
            ("other-bot[bot]", "Rollback fails.", "COMMENTED", "head-1", "judgment-required"),
            ("maintainer", "", "CHANGES_REQUESTED", "head-1", "judgment-required"),
            ("maintainer", "Previous review.", "COMMENTED", "old-head", "satisfied"),
        ):
            with self.subTest(author=author, body=body, head=head):
                self.write_scenario()
                scenario = json.loads(self.scenario.read_text())
                scenario["review"]["reviews"]["nodes"] = [{
                    "id":"review-1", "author":{"login":author}, "body":body,
                    "state":state, "commit":{"oid":head},
                }]
                self.scenario.write_text(json.dumps(scenario))
                _, output = self.run_json(
                    REVIEW, "inspect", "--repo", "owner/repo", "--pr", "7",
                    "--head", "head-1", "--policy", str(self.policy),
                )
                self.assertEqual(output["state"], expected)
                if expected == "judgment-required":
                    self.assertEqual(output["observation"]["findingSurfaces"][0]["review"]["body"], body)

    def test_human_top_level_feedback_is_retained_with_valid_automation_policy(self) -> None:
        self.write_scenario()
        scenario = json.loads(self.scenario.read_text())
        scenario["review"]["comments"]["nodes"] = [{
            "id":"human-comment", "body":"The migration drops existing data.",
            "author":{"login":"maintainer"},
        }]
        self.scenario.write_text(json.dumps(scenario))
        _, output = self.run_json(
            REVIEW, "inspect", "--repo", "owner/repo", "--pr", "7",
            "--head", "head-1", "--policy", str(self.policy),
        )
        self.assertEqual(output["state"], "judgment-required")
        self.assertEqual(output["observation"]["findingSurfaces"][0]["id"], "human-comment")
        self.assertEqual(output["observation"]["topLevelAutomationComments"], [])
        self.assertEqual(len(output["observation"]["unclassifiedTopLevelComments"]), 1)

    def test_edited_human_review_wakes_wait_without_storing_body(self) -> None:
        self.write_scenario()
        scenario = json.loads(self.scenario.read_text())
        scenario["review"]["reviews"]["nodes"] = [{
            "id":"human-review", "body":"First finding.", "state":"COMMENTED",
            "author":{"login":"maintainer"}, "commit":{"oid":"head-1"},
        }]
        self.scenario.write_text(json.dumps(scenario))
        state = self.directory / "review.json"
        arguments = (
            "wait", "--repo", "owner/repo", "--pr", "7", "--head", "head-1",
            "--policy", str(self.policy), "--deadline", self.deadline(),
            "--interval", "0.01", "--state", str(state),
        )
        _, first = self.run_json(REVIEW, *arguments)
        self.assertEqual(first["state"], "judgment-required")
        scenario["review"]["reviews"]["nodes"][0]["body"] = "New finding with private details."
        self.scenario.write_text(json.dumps(scenario))
        _, second = self.run_json(REVIEW, *arguments)
        self.assertEqual(second["state"], "changed")
        self.assertNotIn("private details", state.read_text())
        self.assertNotIn("private details", json.dumps(second))
        self.assertIn("bodyDigest", state.read_text())

    def test_pr_without_check_rollup_is_pending_not_an_exception(self) -> None:
        self.write_scenario()
        scenario = json.loads(self.scenario.read_text())
        scenario["review"]["commits"]["nodes"][0]["commit"]["statusCheckRollup"] = None
        self.scenario.write_text(json.dumps(scenario))
        result, output = self.run_json(
            REVIEW, "inspect", "--repo", "owner/repo", "--pr", "7",
            "--head", "head-1", "--policy", str(self.policy),
        )
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(output["state"], "waiting")
        self.assertFalse(output["observation"]["automations"][0]["terminal"])

    def test_review_wait_reports_new_finding_before_convergence(self) -> None:
        self.write_scenario()
        state = self.directory / "review.json"
        identity = {
            "repo":"owner/repo", "pr":7, "head":"head-1",
            "policyDigest": hashlib.sha256(
                json.dumps(json.loads(self.policy.read_text()), sort_keys=True, separators=(",", ":")).encode()
            ).hexdigest(),
        }
        state.write_text(json.dumps({
            "schemaVersion":1, "kind":"review", "identity":identity,
            "observation":{}, "digest":"prior", "updatedAt":"earlier",
        }))
        _, output = self.run_json(
            REVIEW, "wait", "--repo", "owner/repo", "--pr", "7",
            "--head", "head-1", "--policy", str(self.policy),
            "--deadline", self.deadline(), "--interval", "0.01", "--state", str(state),
        )
        self.assertEqual(output["state"], "changed")

    def test_wait_checkpoint_does_not_store_comment_bodies(self) -> None:
        self.write_scenario()
        scenario = json.loads(self.scenario.read_text())
        scenario["review"]["reviewThreads"]["nodes"] = [{
            "id":"thread-1", "isResolved":False,
            "comments":{"nodes":[{"id":"node-1","databaseId":10,"body":"sensitive finding detail","createdAt":"now","author":{"login":"review-bot[bot]"},"authorAssociation":"NONE","replyTo":None}],"pageInfo":{"hasNextPage":False}},
        }]
        self.scenario.write_text(json.dumps(scenario))
        state = self.directory / "review.json"
        self.run_json(
            REVIEW, "wait", "--repo", "owner/repo", "--pr", "7", "--head", "head-1",
            "--policy", str(self.policy), "--deadline",
            (datetime.now(timezone.utc) + timedelta(seconds=.05)).isoformat(),
            "--interval", "0.01", "--state", str(state),
        )
        checkpoint = state.read_text()
        self.assertNotIn("sensitive finding detail", checkpoint)
        self.assertIn("bodyDigest", checkpoint)

    def test_unbound_legacy_reply_marker_is_not_a_valid_receipt(self) -> None:
        marker = "<!-- known-good-route-operation:op-1 -->"
        self.write_scenario(comments=[{"id":777,"body":f"already done {marker}"}])
        _, output = self.run_json(
            REVIEW, "reply", "--repo", "owner/repo", "--pr", "7", "--comment-id", "10",
            "--head", "head-1", "--body", "Fixed with regression coverage.", "--operation-id", "op-1",
        )
        self.assertEqual(output["state"], "operational-error")
        self.assertIn("does not match", output["reason"])

    def test_resolve_operation_is_explicit(self) -> None:
        self.write_scenario()
        _, output = self.run_json(
            REVIEW, "resolve", "--repo", "owner/repo", "--pr", "7", "--head", "head-1", "--thread-id", "thread-1",
        )
        self.assertEqual(output["state"], "satisfied")
        self.assertTrue(output["observation"]["resolved"])


if __name__ == "__main__":
    unittest.main()
