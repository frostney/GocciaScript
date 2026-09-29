#!/usr/bin/env python3
"""Behavioral tests for the executable CodeRabbit adapter."""

from __future__ import annotations

import importlib.util
import json
import re
import unittest
from datetime import datetime, timezone
from unittest import mock
from pathlib import Path
from typing import Any, Callable


ROOT = Path(__file__).resolve().parents[2]
MODULE_PATH = (
    ROOT / "address-feedback" / "scripts" / "coderabbit_adapter.py"
)
SPEC = importlib.util.spec_from_file_location("coderabbit_adapter", MODULE_PATH)
assert SPEC and SPEC.loader
ADAPTER = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(ADAPTER)


def iso(seconds: int) -> str:
    return datetime.fromtimestamp(seconds, timezone.utc).isoformat().replace(
        "+00:00", "Z"
    )


START = int(datetime(2026, 8, 21, tzinfo=timezone.utc).timestamp())


class FakeMetrics:
    def __init__(self) -> None:
        self.retries = 0


class FakeGh:
    def __init__(self) -> None:
        self.metrics = FakeMetrics()
        self.values: dict[str, Any] = {}
        self.pages: dict[str, list[list[dict[str, Any]]]] = {}
        self.posts: list[tuple[str, dict[str, str]]] = []
        self.on_post: Callable[[str, dict[str, str]], None] | None = None
        # Comment edit histories by node ID: (editedAt, body) pairs.
        self.edits: dict[str, list[tuple[str, str]]] = {}
        self.requests: list[str] = []

    def rest(
        self,
        endpoint: str,
        method: str = "GET",
        fields: dict[str, str] | None = None,
    ) -> Any:
        if method == "POST":
            payload = fields or {}
            self.posts.append((endpoint, payload))
            if self.on_post:
                self.on_post(endpoint, payload)
            return {"id": 900 + len(self.posts)}
        return self.values[endpoint]

    def rest_pages(self, endpoint: str) -> list[Any]:
        self.requests.append(endpoint)
        return self.pages.get(endpoint, [[]])

    def graphql(self, query: str, variables: dict[str, Any]) -> dict[str, Any]:
        """Serves a comment's edit history; unedited history is its current body."""
        self.requests.append(f"graphql:{variables['id']}")
        versions = self.edits.get(variables["id"])
        if versions is None:
            versions = [
                (item["updated_at"], item["body"])
                for page in self.pages.values()
                for batch in page
                if isinstance(batch, list)
                for item in batch
                if isinstance(item, dict) and item.get("node_id") == variables["id"]
            ][:1]
        nodes = [{"editedAt": at, "diff": body} for at, body in sorted(versions, reverse=True)]
        # GitHub pages edits newest first, 20 at a time.
        start = int(variables.get("cursor") or 0)
        return {
            "node": {
                "userContentEdits": {
                    "totalCount": len(nodes),
                    "pageInfo": {
                        "hasNextPage": start + 20 < len(nodes),
                        "endCursor": str(start + 20),
                    },
                    "nodes": nodes[start:start + 20],
                }
            }
        }


def comment(
    identifier: int,
    body: str,
    created: int,
    *,
    bot: bool = True,
    updated: int | None = None,
    issue_url: str = "https://api.github.test/repos/owner/repo/issues/7",
) -> dict[str, Any]:
    return {
        "id": identifier,
        "node_id": f"IC_{identifier}",
        "user": {"login": "coderabbitai[bot]" if bot else "maintainer"},
        "body": body,
        "created_at": iso(created),
        "updated_at": iso(updated if updated is not None else created),
        "issue_url": issue_url,
    }


def review(
    identifier: int,
    body: str,
    head: str = "head-7",
    state: str = "COMMENTED",
) -> dict[str, Any]:
    return {
        "id": identifier,
        "user": {"login": "coderabbitai[bot]"},
        "body": body,
        "state": state,
        "commit_id": head,
        "submitted_at": iso(START + 120),
    }


def configured_gh(
    *,
    comments: list[dict[str, Any]] | None = None,
    reviews: list[dict[str, Any]] | None = None,
    head: str = "head-7",
    coderabbit_check_runs: list[dict[str, Any]] | None = None,
    coderabbit_statuses: list[dict[str, Any]] | None = None,
    pushed: int | None = START,
    check_suites_created: list[int] | None = None,
    check_suite_branch: str = "feature",
) -> FakeGh:
    gh = FakeGh()
    gh.values.update(
        {
            "repos/owner/repo/pulls/7": {
                "head": {
                    "sha": head,
                    "ref": "feature",
                    "repo": {"full_name": "owner/repo"},
                }
            },
            f"repos/owner/repo/commits/{head}": {
                "commit": {"committer": {"date": iso(START)}}
            },
        }
    )
    gh.pages.update(
        {
            "repos/owner/repo/issues/comments?sort=updated&direction=desc&per_page=100": [
                []
            ],
            "repos/owner/repo/issues/7/comments?per_page=100": [comments or []],
            "repos/owner/repo/pulls/7/reviews?per_page=100": [reviews or []],
            "repos/owner/repo/pulls/7/files?per_page=100": [
                [{"filename": "src/feature.ts"}]
            ],
            f"repos/owner/repo/commits/{head}/check-runs?per_page=100": [
                {"check_runs": coderabbit_check_runs or []}
            ],
            f"repos/owner/repo/commits/{head}/statuses?per_page=100": [
                coderabbit_statuses or []
            ],
            "repos/owner/repo/activity?ref=refs/heads/feature&per_page=100": [
                [
                    {"after": "previous-head", "timestamp": iso(START - 600)},
                    *(
                        [{"after": head, "timestamp": iso(pushed)}]
                        if pushed is not None
                        else []
                    ),
                ]
            ],
            f"repos/owner/repo/commits/{head}/check-suites?per_page=100": [
                {
                    "check_suites": [
                        {"created_at": iso(created), "head_branch": check_suite_branch}
                        for created in check_suites_created or []
                    ]
                }
            ],
        }
    )
    return gh


def classify(gh: FakeGh, now: int = START + 180) -> tuple[str, str, str | None]:
    evidence = ADAPTER.pull_evidence(gh, "owner/repo", 7)
    return ADAPTER.classify(evidence, "head-7", None, now)


class CodeRabbitAdapterTest(unittest.TestCase):
    def test_exact_head_actionable_review_is_completion(self) -> None:
        gh = configured_gh(
            reviews=[review(31, "**Actionable comments posted: 2**")]
        )
        state, _, mode = classify(gh)
        self.assertEqual(state, "review-complete")
        self.assertIsNone(mode)

    def test_pending_review_object_is_not_completion(self) -> None:
        gh = configured_gh(
            reviews=[
                review(
                    31,
                    "Actionable comments posted: 2",
                    state="PENDING",
                )
            ]
        )
        state, _, mode = classify(gh)
        self.assertEqual(state, "trigger-incremental")
        self.assertEqual(mode, "incremental")

    def test_empty_review_and_instant_ack_escalate_to_full(self) -> None:
        comments = [
            comment(10, "@coderabbitai review", START + 60, bot=False),
            comment(11, "Review finished", START + 70),
            comment(
                12,
                "<!-- summarize by coderabbit --> src/feature.ts",
                START + 71,
            ),
        ]
        gh = configured_gh(comments=comments, reviews=[review(32, "")])
        state, _, mode = classify(gh)
        self.assertEqual(state, "trigger-full")
        self.assertEqual(mode, "full")

    def test_skipped_ack_never_completes_review(self) -> None:
        comments = [
            comment(10, "@coderabbitai review", START + 60, bot=False),
            comment(11, "Review skipped: draft pull request", START + 90),
        ]
        gh = configured_gh(comments=comments)
        state, _, mode = classify(gh)
        self.assertEqual(state, "pending-skipped")
        self.assertIsNone(mode)

    def test_newest_edited_wait_wins_across_supplied_repositories(self) -> None:
        gh = FakeGh()
        first = ADAPTER.recent_comments_endpoint("owner/one", ADAPTER.scan_since(START + 180))
        second = ADAPTER.recent_comments_endpoint("owner/two", ADAPTER.scan_since(START + 180))
        gh.pages[first] = [[comment(1, "Next review available in: 9 minutes", START, updated=START + 30)]]
        gh.pages[second] = [[
            comment(
                2,
                "**Next review available in:** **2 minutes**",
                START,
                updated=START + 60,
                issue_url="https://api.github.test/repos/owner/two/issues/88",
            )
        ]]
        wait = ADAPTER.account_wait(gh, ["owner/one", "owner/two"], START + 180)
        self.assertIsNotNone(wait)
        assert wait
        self.assertEqual(wait["repo"], "owner/two")
        self.assertEqual(wait["pr"], 88)
        self.assertEqual(wait["retryAtEpoch"], START + 60 + 120 + 60)

    def test_rate_limit_without_stated_wait_has_no_guessed_retry(self) -> None:
        comments = [
            comment(10, "@coderabbitai review", START + 60, bot=False),
            comment(11, "Action not completed: review rate limited", START + 90),
        ]
        gh = configured_gh(comments=comments)
        state, reason, mode = classify(gh)
        self.assertEqual(state, "pending-retry-source")
        self.assertIn("without a stated retry time", reason)
        self.assertIsNone(mode)

    def test_stale_elapsed_wait_does_not_authorize_a_new_rate_limit_retry(self) -> None:
        comments = [
            comment(10, "@coderabbitai review", START + 120, bot=False),
            comment(11, "Action not completed: review rate limited", START + 150),
        ]
        gh = configured_gh(comments=comments)
        evidence = ADAPTER.pull_evidence(gh, "owner/repo", 7)
        stale_wait = {
            "updatedAtEpoch": START + 60,
            "retryAtEpoch": START + 90,
        }
        state, reason, mode = ADAPTER.classify(
            evidence, "head-7", stale_wait, START + 180
        )
        self.assertEqual(state, "pending-retry-source")
        self.assertIn("without a stated retry time", reason)
        self.assertIsNone(mode)

    def test_full_review_refusal_stops_instead_of_retriggering(self) -> None:
        comments = [
            comment(10, "@coderabbitai full review", START + 60, bot=False),
            comment(
                11,
                "Action not completed. These commits are already reviewed.",
                START + 90,
            ),
        ]
        gh = configured_gh(comments=comments)
        state, _, mode = classify(gh)
        self.assertEqual(state, "pending-full-refused")
        self.assertIsNone(mode)

    def test_untrusted_full_review_ack_stops_instead_of_retriggering(self) -> None:
        comments = [
            comment(10, "@coderabbitai full review", START + 60, bot=False),
            comment(11, "Review finished", START + 70),
        ]
        gh = configured_gh(comments=comments)
        state, _, mode = classify(gh)
        self.assertEqual(state, "pending-full-unverified")
        self.assertIsNone(mode)

    def test_changed_head_invalidates_expected_review(self) -> None:
        gh = configured_gh(head="new-head")
        evidence = ADAPTER.pull_evidence(gh, "owner/repo", 7)
        state, reason, mode = ADAPTER.classify(
            evidence, "head-7", None, START + 180
        )
        self.assertEqual(state, "invalidated")
        self.assertIn("new-head", reason)
        self.assertIsNone(mode)

    def test_run_posts_one_safe_incremental_trigger_then_observes_review(self) -> None:
        gh = configured_gh()
        clock = [float(START + 180)]

        def after_post(_endpoint: str, payload: dict[str, str]) -> None:
            self.assertEqual(payload["body"], "@coderabbitai review")
            gh.pages["repos/owner/repo/pulls/7/reviews?per_page=100"] = [[
                review(33, "Actionable comments posted: 1")
            ]]

        gh.on_post = after_post

        def sleep(seconds: float) -> None:
            clock[0] += seconds

        state, _, evidence = ADAPTER.run_review(
            gh,
            "owner/repo",
            7,
            "head-7",
            ["owner/repo"],
            START + 300,
            10,
            clock=lambda: clock[0],
            sleeper=sleep,
        )
        self.assertEqual(state, "satisfied")
        self.assertEqual(len(gh.posts), 1)
        self.assertEqual(evidence["triggers"][0]["mode"], "incremental")
        self.assertEqual(
            {item["body"] for item in evidence["triggers"]},
            {"@coderabbitai review"},
        )

    def test_adapter_exposes_no_paid_review_command(self) -> None:
        self.assertEqual(
            set(ADAPTER.TRIGGERS.values()),
            {"@coderabbitai review", "@coderabbitai full review"},
        )

    def test_expired_deadline_never_posts_a_trigger(self) -> None:
        gh = configured_gh()
        state, reason, _ = ADAPTER.run_review(
            gh,
            "owner/repo",
            7,
            "head-7",
            ["owner/repo"],
            START + 180,
            10,
            clock=lambda: float(START + 180),
            sleeper=lambda _seconds: None,
        )
        self.assertEqual(state, "pending")
        self.assertIn("before the permitted trigger", reason)
        self.assertEqual(gh.posts, [])

    def test_status_aggregation_surfaces_invalidated_and_pending_members(self) -> None:
        invalidated = {
            "pullRequests": [
                {"state": "review-complete"},
                {"state": "invalidated"},
            ]
        }
        pending = {"pullRequests": [{"state": "trigger-incremental"}]}
        complete = {"pullRequests": [{"state": "clean-complete"}]}
        self.assertEqual(ADAPTER.aggregate_status(invalidated)[0], "invalidated")
        self.assertEqual(ADAPTER.aggregate_status(pending)[0], "pending")
        self.assertEqual(ADAPTER.aggregate_status(complete)[0], "satisfied")

    def test_fast_ack_without_coverage_stays_pending_full_unverified(self) -> None:
        comments = [
            comment(10, "@coderabbitai full review", START + 60, bot=False),
            comment(11, "Review finished", START + 65),
        ]
        gh = configured_gh(comments=comments)
        state, _, mode = classify(gh)
        self.assertEqual(state, "pending-full-unverified")
        self.assertIsNone(mode)

    def test_fast_ack_verified_coverage_with_coderabbit_check_is_clean_complete(
        self,
    ) -> None:
        comments = [
            comment(10, "@coderabbitai full review", START + 60, bot=False),
            comment(11, "Review finished", START + 65),
            comment(
                12,
                "<!-- summarize by coderabbit --> src/feature.ts",
                START + 66,
            ),
        ]
        gh = configured_gh(
            comments=comments,
            coderabbit_statuses=[
                {
                    "context": "CodeRabbit",
                    "state": "success",
                    "created_at": iso(START + 64),
                }
            ],
        )
        evidence = ADAPTER.pull_evidence(gh, "owner/repo", 7)
        self.assertTrue(evidence["codeRabbitCheckSuccess"])
        self.assertEqual(evidence["codeRabbitCheck"]["state"], "SUCCESS")
        state, reason, mode = classify(gh)
        self.assertEqual(state, "clean-complete")
        self.assertIn("CodeRabbit check SUCCESS", reason)
        self.assertIsNone(mode)

    def test_fast_ack_verified_coverage_without_coderabbit_check_stays_unverified(
        self,
    ) -> None:
        comments = [
            comment(10, "@coderabbitai full review", START + 60, bot=False),
            comment(11, "Review finished", START + 65),
            comment(
                12,
                "<!-- summarize by coderabbit --> src/feature.ts",
                START + 66,
            ),
        ]
        gh = configured_gh(comments=comments)
        evidence = ADAPTER.pull_evidence(gh, "owner/repo", 7)
        self.assertFalse(evidence["codeRabbitCheckSuccess"])
        state, _, mode = classify(gh)
        self.assertEqual(state, "pending-full-unverified")
        self.assertIsNone(mode)

    def test_trusted_latency_ack_after_recorded_push_is_clean_complete_without_check(
        self,
    ) -> None:
        comments = [
            comment(10, "@coderabbitai full review", START + 60, bot=False),
            comment(11, "Review finished", START + 100),
            comment(
                12,
                "<!-- summarize by coderabbit --> src/feature.ts",
                START + 101,
            ),
        ]
        gh = configured_gh(comments=comments)
        state, reason, mode = classify(gh)
        self.assertEqual(state, "clean-complete")
        self.assertEqual(
            reason, "finished acknowledgment has current walkthrough coverage"
        )
        self.assertIsNone(mode)

    def ack_before_push(self) -> list[dict[str, Any]]:
        # The commit is made at START, CodeRabbit finishes the previous head at
        # START + 100, and the commit only reaches GitHub at START + 200.
        return [
            comment(11, "Review finished", START + 100),
            comment(
                12,
                "<!-- summarize by coderabbit --> src/feature.ts",
                START + 101,
            ),
        ]

    def test_ack_before_the_head_was_pushed_does_not_complete_review(
        self,
    ) -> None:
        gh = configured_gh(comments=self.ack_before_push(), pushed=START + 200)
        evidence = ADAPTER.pull_evidence(gh, "owner/repo", 7)
        self.assertEqual(evidence["headPushedAt"], iso(START + 200))
        self.assertEqual(evidence["headPushSource"], "activity")
        self.assertIsNone(evidence["finishedAck"])
        state, _, mode = ADAPTER.classify(evidence, "head-7", None, START + 240)
        self.assertEqual(state, "trigger-incremental")
        self.assertEqual(mode, "incremental")

    def test_check_suite_creation_bounds_ack_when_activity_is_unavailable(
        self,
    ) -> None:
        gh = configured_gh(
            comments=self.ack_before_push(),
            pushed=None,
            check_suites_created=[START + 260, START + 200],
        )
        evidence = ADAPTER.pull_evidence(gh, "owner/repo", 7)
        self.assertEqual(evidence["headPushedAt"], iso(START + 200))
        self.assertEqual(evidence["headPushSource"], "check-suite")
        state, _, _ = ADAPTER.classify(evidence, "head-7", None, START + 240)
        self.assertNotEqual(state, "clean-complete")

    def test_unknown_push_time_requires_head_scoped_check_for_clean_complete(
        self,
    ) -> None:
        gh = configured_gh(comments=self.ack_before_push(), pushed=None)
        evidence = ADAPTER.pull_evidence(gh, "owner/repo", 7)
        self.assertIsNone(evidence["headPushedAt"])
        state, reason, mode = ADAPTER.classify(evidence, "head-7", None, START + 240)
        self.assertEqual(state, "pending-check-required")
        self.assertIn("push time is unknown", reason)
        self.assertIsNone(mode)

        gh = configured_gh(
            comments=self.ack_before_push(),
            pushed=None,
            coderabbit_check_runs=[
                {
                    "name": "CodeRabbit",
                    "head_sha": "head-7",
                    "status": "completed",
                    "conclusion": "success",
                }
            ],
        )
        state, reason, _ = classify(gh)
        self.assertEqual(state, "clean-complete")
        self.assertIn("CodeRabbit check SUCCESS", reason)

    def test_activity_lookup_failure_falls_back_but_rate_limits_propagate(
        self,
    ) -> None:
        activity = "repos/owner/repo/activity?ref=refs/heads/feature&per_page=100"

        def failing(error: Exception) -> FakeGh:
            gh = configured_gh(
                comments=self.ack_before_push(),
                check_suites_created=[START + 200],
            )
            original = gh.rest_pages

            def rest_pages(endpoint: str) -> list[Any]:
                if endpoint == activity:
                    raise error
                return original(endpoint)

            gh.rest_pages = rest_pages  # type: ignore[method-assign]
            return gh

        evidence = ADAPTER.pull_evidence(
            failing(ADAPTER.WaitError("gh: Not Found (HTTP 404)")), "owner/repo", 7
        )
        self.assertEqual(evidence["headPushSource"], "check-suite")
        with self.assertRaises(ADAPTER.RateLimited):
            ADAPTER.pull_evidence(
                failing(ADAPTER.RateLimited("rate limit")), "owner/repo", 7
            )
        with self.assertRaises(ADAPTER.WaitError):
            ADAPTER.pull_evidence(
                failing(ADAPTER.WaitError("gh: Server Error (HTTP 500)")), "owner/repo", 7
            )

    def test_check_suite_from_another_branch_does_not_bound_the_push(self) -> None:
        # The commit reached another branch at START + 50, before CodeRabbit
        # finished the previous PR head; its suite says nothing about this push.
        gh = configured_gh(
            comments=self.ack_before_push(),
            pushed=None,
            check_suites_created=[START + 50],
            check_suite_branch="scratch",
        )
        evidence = ADAPTER.pull_evidence(gh, "owner/repo", 7)
        self.assertIsNone(evidence["headPushedAt"])
        state, _, _ = ADAPTER.classify(evidence, "head-7", None, START + 240)
        self.assertEqual(state, "pending-check-required")

    def test_run_returns_pending_check_required_without_waiting_or_triggering(
        self,
    ) -> None:
        gh = configured_gh(comments=self.ack_before_push(), pushed=None)
        clock = [float(START + 240)]

        def sleep(seconds: float) -> None:
            clock[0] += seconds

        state, reason, _ = ADAPTER.run_review(
            gh,
            "owner/repo",
            7,
            "head-7",
            ["owner/repo"],
            START + 900,
            10,
            clock=lambda: clock[0],
            sleeper=sleep,
        )
        self.assertEqual(state, "pending")
        self.assertIn("push time is unknown", reason)
        self.assertEqual(clock[0], START + 240)
        self.assertEqual(gh.posts, [])

    def test_coderabbit_check_run_success_binds_exact_head(self) -> None:
        comments = [
            comment(10, "@coderabbitai full review", START + 60, bot=False),
            comment(11, "Review finished", START + 65),
            comment(
                12,
                "<!-- summarize by coderabbit --> src/feature.ts",
                START + 66,
            ),
        ]
        gh = configured_gh(
            comments=comments,
            coderabbit_check_runs=[
                {
                    "name": "CodeRabbit",
                    "head_sha": "head-7",
                    "status": "completed",
                    "conclusion": "success",
                }
            ],
        )
        state, _, mode = classify(gh)
        self.assertEqual(state, "clean-complete")
        self.assertIsNone(mode)

    def test_check_run_success_with_rate_limited_output_is_not_check_success(
        self,
    ) -> None:
        comments = [
            comment(10, "@coderabbitai full review", START + 60, bot=False),
            comment(11, "Review finished", START + 65),
            comment(
                12,
                "<!-- summarize by coderabbit --> src/feature.ts",
                START + 66,
            ),
        ]
        gh = configured_gh(
            comments=comments,
            coderabbit_check_runs=[
                {
                    "name": "CodeRabbit",
                    "head_sha": "head-7",
                    "status": "completed",
                    "conclusion": "success",
                    "output": {
                        "title": "Review rate limited",
                        "summary": "Review rate limited",
                        "text": "Action not completed: review rate limited",
                    },
                }
            ],
        )
        evidence = ADAPTER.pull_evidence(gh, "owner/repo", 7)
        self.assertFalse(evidence["codeRabbitCheckSuccess"])
        self.assertEqual(evidence["codeRabbitCheck"]["state"], "RATE_LIMITED")
        self.assertTrue(evidence["rateLimited"])
        state, reason, mode = classify(gh)
        self.assertNotEqual(state, "clean-complete")
        self.assertEqual(state, "pending-retry-source")
        self.assertIn("rate limited", reason)
        self.assertIsNone(mode)

    def test_commit_status_success_with_rate_limited_description_is_not_check_success(
        self,
    ) -> None:
        gh = configured_gh(
            coderabbit_statuses=[
                {
                    "context": "CodeRabbit",
                    "state": "success",
                    "description": "Review rate limited",
                    "created_at": iso(START + 64),
                }
            ],
        )
        evidence = ADAPTER.pull_evidence(gh, "owner/repo", 7)
        self.assertFalse(evidence["codeRabbitCheckSuccess"])
        self.assertEqual(evidence["codeRabbitCheck"]["state"], "RATE_LIMITED")
        self.assertTrue(evidence["codeRabbitCheck"].get("rateLimited"))
        self.assertTrue(evidence["rateLimited"])

    def test_comment_rate_limit_blocks_clean_complete_despite_success_check(
        self,
    ) -> None:
        comments = [
            comment(10, "@coderabbitai full review", START + 60, bot=False),
            comment(11, "Review finished", START + 65),
            comment(
                12,
                "<!-- summarize by coderabbit --> src/feature.ts",
                START + 66,
            ),
            comment(13, "Action not completed: review rate limited", START + 70),
        ]
        gh = configured_gh(
            comments=comments,
            coderabbit_check_runs=[
                {
                    "name": "CodeRabbit",
                    "head_sha": "head-7",
                    "status": "completed",
                    "conclusion": "success",
                }
            ],
        )
        evidence = ADAPTER.pull_evidence(gh, "owner/repo", 7)
        self.assertTrue(evidence["codeRabbitCheckSuccess"])
        self.assertTrue(evidence["rateLimited"])
        state, reason, mode = ADAPTER.classify(
            evidence, "head-7", None, START + 180
        )
        self.assertEqual(state, "pending-retry-source")
        self.assertIn("rate limited", reason)
        self.assertIsNone(mode)
        self.assertNotEqual(state, "clean-complete")


NOW = START + 180
HOUR = 3600


def availability(remaining: int, per_hour: int) -> str:
    reviews = "review remains" if remaining == 1 else "reviews remain"
    return (
        "**Included review availability:** This review used your included "
        f"allowance. {remaining} included {reviews} after this review. Your "
        "included PR review attempts over the past 7 days set your current "
        f"allowance at {per_hour} reviews per hour."
    )


def summary_comment(
    identifier: int,
    pr: int,
    updated: int,
    statement: str,
    run_id: str | None,
) -> dict[str, Any]:
    run = f"**Run ID**: `{run_id}`\n\n" if run_id else ""
    return comment(
        identifier,
        "<!-- This is an auto-generated comment: summarize by coderabbit.ai -->\n"
        f"{run}{statement}",
        updated - 900,
        updated=updated,
        issue_url=f"https://api.github.test/repos/owner/repo/issues/{pr}",
    )


def run_review_object(
    identifier: int, run_id: str, submitted: int, body: str = "Actionable comments posted: 1"
) -> dict[str, Any]:
    return {
        "id": identifier,
        "user": {"login": "coderabbitai[bot]"},
        "body": f"{body}\n\n**Run ID**: `{run_id}`",
        "state": "COMMENTED",
        "commit_id": "other-head",
        "submitted_at": iso(submitted),
    }


def budget_gh(
    account_comments: list[dict[str, Any]],
    *,
    now: int = NOW,
    pr_comments: list[dict[str, Any]] | None = None,
    other_reviews: dict[int, list[dict[str, Any]]] | None = None,
    wait_comments: list[dict[str, Any]] | None = None,
) -> FakeGh:
    gh = configured_gh(comments=pr_comments)
    scan = ADAPTER.recent_comments_endpoint("owner/repo", ADAPTER.scan_since(now))
    gh.pages[scan] = [account_comments + (wait_comments or [])]
    for pr, reviews in (other_reviews or {}).items():
        gh.pages[f"repos/owner/repo/pulls/{pr}/reviews?per_page=100"] = [reviews]
    return gh


def observe(gh: FakeGh, now: int = NOW) -> dict[str, Any]:
    return ADAPTER.observation(
        gh, "owner/repo", [7], {7: "head-7"}, ["owner/repo"], now
    )


class CodeRabbitAllowanceBudgetTest(unittest.TestCase):
    def test_statement_parsing_handles_singular_plural_and_zero(self) -> None:
        cases = {
            availability(1, 2): 1,
            availability(2, 5): 2,
            availability(0, 2): 0,
            "**Included review availability:** 1 review is currently available. "
            "Your included PR review attempts over the past 7 days set your "
            "current allowance at 10 reviews per hour.": 1,
            "**Included review availability:** 7 reviews are currently available. "
            "Your included PR review attempts over the past 7 days set your "
            "current allowance at 8 reviews per hour.": 7,
            "**Limit details:** You’ve used all 2 included reviews currently "
            "available. Your 84 included PR review attempts over the past 7 days "
            "set your current allowance at 2 reviews per hour.": 0,
        }
        for text, remaining in cases.items():
            with self.subTest(text=text):
                parsed = ADAPTER.parse_allowance(text)
                assert parsed
                self.assertEqual(parsed["remaining"], remaining)
                self.assertEqual(parsed["windowSeconds"], HOUR)
        self.assertEqual(ADAPTER.parse_allowance(availability(1, 2))["allowance"], 2)
        rate_only = ADAPTER.parse_allowance("set your current allowance at 1 review per hour.")
        assert rate_only
        self.assertEqual((rate_only["allowance"], rate_only["remaining"]), (1, None))
        self.assertFalse(rate_only["recognized"])
        self.assertIsNone(ADAPTER.parse_allowance("Review finished"))
        unrated = ADAPTER.parse_allowance("1 included review remains after this review.")
        assert unrated
        self.assertEqual((unrated["remaining"], unrated["allowance"]), (1, None))
        self.assertTrue(unrated["recognized"])
        # A zero rate is no rate.
        zero_rate = ADAPTER.parse_allowance(
            "0 included reviews remain after this review. ... allowance at 0 reviews per hour."
        )
        assert zero_rate
        self.assertIsNone(zero_rate["allowance"])

    def test_used_up_allowance_waits_until_the_oldest_attempt_leaves_the_window(
        self,
    ) -> None:
        gh = budget_gh(
            [
                summary_comment(81, 8, NOW - 1800, availability(1, 2), "run-a"),
                summary_comment(91, 9, NOW - 600, availability(0, 2), "run-b"),
            ]
        )
        value = observe(gh)
        item = value["pullRequests"][0]
        self.assertEqual(item["state"], "waiting")
        self.assertEqual(item["nextMode"], "incremental")
        self.assertEqual(item["retryAt"], iso(NOW - 1800 + HOUR + 60))
        self.assertEqual(item["retrySource"], "allowance")
        allowance = value["allowance"]
        self.assertEqual(allowance["mode"], "enforced")
        self.assertTrue(allowance["exhausted"])
        self.assertEqual(allowance["remaining"], 0)
        self.assertEqual(allowance["perHour"], 2)
        self.assertEqual(allowance["statementAt"], iso(NOW - 600))
        self.assertEqual(
            allowance["source"], {"repo": "owner/repo", "pr": 9, "commentId": 91}
        )
        self.assertEqual(allowance["attempts"]["count"], 2)
        self.assertEqual(allowance["retryAt"], iso(NOW - 1800 + HOUR + 60))

    def test_review_objects_count_once_per_run_across_pull_requests(self) -> None:
        # Run c's summary was overwritten by run a; its review object remains.
        # Run b appears as both a review object and a summary statement.
        gh = budget_gh(
            [
                summary_comment(81, 8, NOW - 1800, availability(1, 2), "run-a"),
                summary_comment(91, 9, NOW - 600, availability(0, 2), "run-b"),
            ],
            other_reviews={
                8: [run_review_object(801, "run-c", NOW - 3000)],
                9: [
                    run_review_object(901, "run-b", NOW - 700),
                    {**run_review_object(902, "run-x", NOW - 650), "body": ""},
                ],
            },
        )
        value = observe(gh)
        allowance = value["allowance"]
        self.assertEqual(allowance["attempts"]["count"], 3)
        self.assertTrue(allowance["exhausted"])
        # Three attempts against two per hour: two must leave the window.
        self.assertEqual(allowance["retryAt"], iso(NOW - 1800 + HOUR + 60))
        self.assertEqual(value["pullRequests"][0]["state"], "waiting")

    def test_statement_binds_the_run_id_of_its_own_review(self) -> None:
        # A skip notice above the recent-review block carries the skipped run's
        # ID; only the run the statement describes is counted.
        skipped_then_reviewed = comment(
            81,
            "<!-- This is an auto-generated comment: summarize by coderabbit.ai -->\n"
            "> ## Review skipped\n> **Run ID**: `run-skip`\n"
            "<!-- recent_review_start -->\n**Run ID**: `run-a`\n\n"
            + availability(1, 2),
            NOW - 1500,
            updated=NOW - 600,
            issue_url="https://api.github.test/repos/owner/repo/issues/8",
        )
        allowance = observe(budget_gh([skipped_then_reviewed]))["allowance"]
        self.assertEqual(
            [run["runId"] for run in allowance["attempts"]["runs"]], ["run-a"]
        )

    def test_newest_statement_outranks_a_higher_window_count(self) -> None:
        # Counted times can trail a run's start, so a window count above the
        # allowance does not override CodeRabbit's own "1 remains".
        gh = budget_gh(
            [
                comment(
                    81,
                    "Review finished",
                    NOW - 1700,
                    issue_url="https://api.github.test/repos/owner/repo/issues/8",
                ),
                summary_comment(91, 9, NOW - 600, availability(1, 2), "run-b"),
            ],
            other_reviews={
                8: [
                    run_review_object(801, "run-c", NOW - 3000),
                    run_review_object(802, "run-a", NOW - 1800),
                ],
                9: [run_review_object(901, "run-b", NOW - 700)],
            },
        )
        value = observe(gh)
        self.assertEqual(value["allowance"]["attempts"]["count"], 3)
        self.assertFalse(value["allowance"]["exhausted"])
        self.assertEqual(value["pullRequests"][0]["state"], "trigger-incremental")

    def test_runs_after_the_statement_use_up_its_remaining_reviews(self) -> None:
        gh = budget_gh(
            [summary_comment(91, 9, NOW - 900, availability(1, 2), "run-b")],
            other_reviews={
                8: [run_review_object(801, "run-a", NOW - 2400)],
                9: [run_review_object(902, "run-d", NOW - 120)],
            },
        )
        value = observe(gh)
        self.assertTrue(value["allowance"]["exhausted"])
        self.assertEqual(value["allowance"]["retryAt"], iso(NOW - 900 + HOUR + 60))
        self.assertEqual(value["pullRequests"][0]["state"], "waiting")

    def test_available_allowance_permits_the_trigger(self) -> None:
        gh = budget_gh(
            [summary_comment(81, 8, NOW - 600, availability(1, 2), "run-a")]
        )
        value = observe(gh)
        item = value["pullRequests"][0]
        self.assertEqual(item["state"], "trigger-incremental")
        self.assertIsNone(item["retryAt"])
        allowance = value["allowance"]
        self.assertEqual(allowance["mode"], "enforced")
        self.assertFalse(allowance["exhausted"])
        self.assertEqual(allowance["remaining"], 1)
        self.assertEqual(allowance["attempts"]["count"], 1)
        self.assertIsNone(allowance["retryAt"])

    def test_exhaustion_ends_when_its_oldest_attempt_leaves_the_window(
        self,
    ) -> None:
        gh = budget_gh(
            [summary_comment(81, 8, NOW - 3000, availability(0, 2), "run-a")],
            other_reviews={8: [run_review_object(801, "run-c", NOW - 3700)]},
        )
        value = observe(gh)
        self.assertEqual(value["pullRequests"][0]["state"], "trigger-incremental")
        self.assertEqual(value["allowance"]["mode"], "enforced")
        self.assertFalse(value["allowance"]["exhausted"])
        self.assertEqual(value["allowance"]["attempts"]["count"], 1)

    def test_later_stated_wait_takes_precedence_over_the_allowance(self) -> None:
        exhausted = [
            summary_comment(81, 8, NOW - 1800, availability(1, 2), "run-a"),
            summary_comment(91, 9, NOW - 600, availability(0, 2), "run-b"),
        ]
        later = budget_gh(
            exhausted,
            wait_comments=[
                comment(95, "Next review available in: 40 minutes", NOW - 60)
            ],
        )
        item = observe(later)["pullRequests"][0]
        self.assertEqual(item["state"], "waiting")
        self.assertEqual(item["retryAt"], iso(NOW - 60 + 2400 + 60))
        self.assertEqual(item["retrySource"], "stated-wait")

        earlier = budget_gh(
            exhausted,
            wait_comments=[
                comment(95, "Next review available in: 5 minutes", NOW - 60)
            ],
        )
        item = observe(earlier)["pullRequests"][0]
        self.assertEqual(item["retryAt"], iso(NOW - 1800 + HOUR + 60))
        self.assertEqual(item["retrySource"], "allowance")

    def test_statement_older_than_its_window_is_ignored(self) -> None:
        gh = budget_gh(
            [summary_comment(81, 8, NOW - HOUR - 100, availability(0, 2), "run-a")]
        )
        value = observe(gh)
        self.assertEqual(value["pullRequests"][0]["state"], "trigger-incremental")
        allowance = value["allowance"]
        self.assertEqual(allowance["mode"], "degraded")
        self.assertIsNone(allowance["remaining"])
        self.assertIsNone(allowance["retryAt"])
        self.assertIn("no allowance statement", allowance["reason"])

    def test_used_up_statement_without_counted_runs_still_holds(self) -> None:
        # The run the statement reports on ended by its edit, so a slot frees
        # within one window of it even when no run can be counted.
        gh = budget_gh(
            [summary_comment(81, 8, NOW - 600, availability(0, 2), None)]
        )
        value = observe(gh)
        item = value["pullRequests"][0]
        self.assertEqual(item["state"], "waiting")
        self.assertEqual(item["retryAt"], iso(NOW - 600 + HOUR + 60))
        allowance = value["allowance"]
        self.assertEqual(allowance["mode"], "enforced")
        self.assertEqual(allowance["attempts"]["count"], 0)
        self.assertTrue(allowance["exhausted"])

    def test_statement_without_a_rate_degrades_but_still_holds(self) -> None:
        gh = budget_gh(
            [
                summary_comment(
                    81,
                    8,
                    NOW - 600,
                    "**Limit details:** You\u2019ve used the included review "
                    "currently available.",
                    "run-a",
                )
            ]
        )
        value = observe(gh)
        allowance = value["allowance"]
        self.assertEqual(allowance["mode"], "degraded")
        self.assertIn("states no rate", allowance["reason"])
        self.assertTrue(allowance["exhausted"])
        self.assertIsNone(allowance["perHour"])
        item = value["pullRequests"][0]
        self.assertEqual(item["state"], "waiting")
        self.assertEqual(item["retryAt"], iso(NOW - 600 + HOUR + 60))

    def rate_limited_pr(self) -> list[dict[str, Any]]:
        return [
            comment(10, "@coderabbitai review", START + 60, bot=False),
            comment(11, "Action not completed: review rate limited", START + 90),
        ]

    def test_rate_limit_without_stated_wait_uses_the_derived_retry(self) -> None:
        account = [
            summary_comment(81, 8, START - 1200, availability(1, 2), "run-a"),
            summary_comment(91, 9, START + 50, availability(0, 2), "run-b"),
        ]
        gh = budget_gh(account, pr_comments=self.rate_limited_pr())
        item = observe(gh)["pullRequests"][0]
        self.assertEqual(item["state"], "waiting")
        self.assertEqual(item["nextMode"], "incremental")
        self.assertEqual(item["retryAt"], iso(START - 1200 + HOUR + 60))
        self.assertEqual(item["retrySource"], "allowance")

        later = START + 2500
        gh = budget_gh(account, pr_comments=self.rate_limited_pr(), now=later)
        item = observe(gh, later)["pullRequests"][0]
        self.assertEqual(item["state"], "trigger-incremental")

    def test_rate_limit_without_attempts_before_it_keeps_pending_retry_source(
        self,
    ) -> None:
        # The only counted run follows the notice, so it cannot explain it.
        gh = budget_gh(
            [summary_comment(81, 8, START + 150, availability(1, 2), "run-a")],
            pr_comments=self.rate_limited_pr(),
        )
        item = observe(gh)["pullRequests"][0]
        self.assertEqual(item["state"], "pending-retry-source")
        self.assertIsNone(item["retryAt"])

    def test_run_never_posts_while_the_allowance_is_used_up(self) -> None:
        gh = budget_gh(
            [
                summary_comment(81, 8, NOW - 1800, availability(1, 2), "run-a"),
                summary_comment(91, 9, NOW - 600, availability(0, 2), "run-b"),
            ]
        )
        sleeps: list[float] = []
        state, reason, evidence = ADAPTER.run_review(
            gh,
            "owner/repo",
            7,
            "head-7",
            ["owner/repo"],
            NOW,
            10,
            clock=lambda: float(NOW),
            sleeper=sleeps.append,
        )
        self.assertEqual(state, "pending")
        self.assertIn("before trustworthy review completion", reason)
        self.assertEqual(gh.posts, [])
        self.assertEqual(evidence["pullRequests"][0]["state"], "waiting")


RECORDED = json.loads(
    (ROOT / "address-feedback" / "tests" / "fixtures" / "coderabbit_recorded.json")
    .read_text(encoding="utf-8")
)
RECORDED_SCAN = ["frostney/GocciaScript", "frostney/homebrew-tap"]


def at(value: str) -> float:
    return ADAPTER.parse_time(value)


class RecordedGh:
    """Serves what GitHub's REST API showed at `now` from recorded edit history."""

    def __init__(self, timeline: dict[str, Any], now: float) -> None:
        self.timeline = timeline
        self.now = now
        self.metrics = FakeMetrics()

    def visible(self, repo: str) -> list[dict[str, Any]]:
        items = []
        for item in self.timeline["comments"]:
            versions = [v for v in item["versions"] if at(v["at"]) <= self.now]
            if item["repo"] != repo or not versions:
                continue
            login = item["login"] + ("[bot]" if item["login"] == "coderabbitai" else "")
            items.append(
                {
                    "id": item["id"],
                    "node_id": f"IC_{item['id']}",
                    "user": {"login": login},
                    "body": versions[-1]["body"],
                    "created_at": item["createdAt"],
                    "updated_at": versions[-1]["at"],
                    "issue_url": f"https://api.github.com/repos/frostney/{repo}/issues/{item['pr']}",
                }
            )
        return sorted(items, key=lambda item: item["updated_at"], reverse=True)

    def graphql(self, query: str, variables: dict[str, Any]) -> dict[str, Any]:
        item = next(
            c for c in self.timeline["comments"] if f"IC_{c['id']}" == variables["id"]
        )
        nodes = [
            {"editedAt": v["at"], "diff": v["body"]}
            for v in reversed(item["versions"])
            if at(v["at"]) <= self.now
        ]
        return {
            "node": {
                "userContentEdits": {
                    "totalCount": len(nodes),
                    "pageInfo": {"hasNextPage": False, "endCursor": None},
                    "nodes": nodes,
                }
            }
        }

    def rest_pages(self, endpoint: str) -> list[Any]:
        scan = re.fullmatch(
            r"repos/frostney/([^/]+)/issues/comments\?sort=updated&direction=desc"
            r"&per_page=100(?:&since=(.+))?",
            endpoint,
        )
        if scan:
            items = self.visible(scan.group(1))
            if scan.group(2):
                items = [i for i in items if at(i["updated_at"]) >= at(scan.group(2))]
            return [items]
        reviews = re.fullmatch(
            r"repos/frostney/([^/]+)/pulls/(\d+)/reviews\?per_page=100", endpoint
        )
        if reviews:
            return [[
                {
                    "id": item["id"],
                    "user": {"login": "coderabbitai[bot]"},
                    "body": item["body"],
                    "state": item["state"],
                    "submitted_at": item["submittedAt"],
                }
                for item in self.timeline["reviews"]
                if item["repo"] == reviews.group(1)
                and item["pr"] == int(reviews.group(2))
                and at(item["submittedAt"]) <= self.now
            ]]
        return [[]]


def pending_evidence(rate_limited_at: float | None = None) -> dict[str, Any]:
    """A PR head that has no completed review and needs a trigger."""
    return {
        "head": "head",
        "exactHeadReview": None,
        "finishedAck": None,
        "skippedAck": False,
        "alreadyReviewed": False,
        "rateLimited": rate_limited_at is not None,
        "rateLimitedAtEpoch": rate_limited_at,
        "trigger": {"mode": "incremental"} if rate_limited_at is not None else None,
        "headPushedAt": None,
        "coverage": {"verified": False},
        "codeRabbitCheck": {"state": None},
        "codeRabbitCheckSuccess": False,
    }


def replay(
    timeline: str, now: str, rate_limited_at: str | None = None
) -> tuple[str, dict[str, Any] | None, dict[str, Any]]:
    gh = RecordedGh(RECORDED["timelines"][timeline], at(now))
    budget = ADAPTER.account_allowance(gh, RECORDED_SCAN, at(now))
    wait = ADAPTER.account_wait(gh, RECORDED_SCAN, at(now))
    evidence = pending_evidence(at(rate_limited_at) if rate_limited_at else None)
    state, _, _ = ADAPTER.classify(evidence, "head", wait, at(now), budget)
    return state, ADAPTER.trigger_gate(evidence, wait, budget), budget


class RecordedCodeRabbitEvidenceTest(unittest.TestCase):
    def test_every_recorded_allowance_wording_parses(self) -> None:
        self.assertEqual(len(RECORDED["variants"]), 20)
        for variant in RECORDED["variants"]:
            with self.subTest(text=variant["text"]):
                parsed = ADAPTER.parse_allowance(variant["text"])
                assert parsed
                self.assertEqual(parsed["remaining"], variant["remaining"])
                self.assertEqual(parsed["allowance"], variant["allowance"])
                self.assertEqual(parsed["windowSeconds"], HOUR)
                self.assertEqual(parsed["refillSeconds"], variant.get("refillSeconds"))

    def test_recorded_notices_classify_as_coderabbit_meant_them(self) -> None:
        notices = {item["kind"]: item["body"] for item in RECORDED["notices"]}
        waits = {
            "summary-rate-limit-exceeded-with-stated-wait": (4, "47"),
            "summary-rate-limited-with-stated-wait": (52, None),
            "reply-rate-limited-with-stated-wait": (15, None),
        }
        for kind, (minutes, seconds) in waits.items():
            with self.subTest(kind=kind):
                self.assertRegex(notices[kind], ADAPTER.RATE_LIMITED)
                match = ADAPTER.STATED_WAIT.search(notices[kind])
                assert match
                self.assertEqual((int(match.group(1)), match.group(2)), (minutes, seconds))
        self.assertRegex(notices["reply-rate-limited-without-wait"], ADAPTER.RATE_LIMITED)
        self.assertIsNone(ADAPTER.STATED_WAIT.search(notices["reply-rate-limited-without-wait"]))
        for kind in ("summary-reviews-paused", "summary-in-progress", "summary-draft-not-reviewed"):
            with self.subTest(kind=kind):
                self.assertNotRegex(notices[kind], ADAPTER.RATE_LIMITED)
                self.assertIsNone(ADAPTER.STATED_WAIT.search(notices[kind]))
        self.assertIsNone(ADAPTER.parse_allowance(notices["summary-in-progress"]))
        # A pause notice keeps the statement of the review before it.
        self.assertEqual(
            ADAPTER.parse_allowance(notices["summary-reviews-paused"])["remaining"], 2
        )
        self.assertRegex(notices["summary-review-skipped"], ADAPTER.SKIPPED)
        self.assertRegex(notices["reply-already-reviewed"], ADAPTER.ALREADY_REVIEWED)
        # The refused run's ID beside its limit details is not counted.
        limited = notices["summary-rate-limited-with-stated-wait"]
        self.assertEqual(ADAPTER.parse_allowance(limited)["remaining"], 0)
        gh = FakeGh()
        gh.pages[ADAPTER.recent_comments_endpoint("owner/repo", ADAPTER.scan_since(NOW))] = [
            [summary_comment(81, 8, NOW - 60, limited, None)]
        ]
        _, runs = ADAPTER.scan_allowance(gh, ["owner/repo"], ADAPTER.scan_since(NOW))
        self.assertEqual(runs, {})

    def test_replay_available_then_used_up_then_refused_on_2026_08_22(self) -> None:
        # 16:21:45 frostney/GocciaScript#1203 states 2 reviews available.
        state, _, budget = replay("2026-08-22", "2026-08-22T16:21:46Z")
        self.assertEqual(state, "trigger-incremental")
        self.assertEqual((budget["remaining"], budget["perHour"]), (2, 6))
        # 16:45:27 #1205 states all 5 used; the trigger at 16:46:02 was refused
        # at 16:46:09 with "Your next included review will be available in
        # 15 minutes". Just before that trigger the adapter holds.
        state, gate, budget = replay("2026-08-22", "2026-08-22T16:46:01Z")
        self.assertEqual(state, "waiting")
        assert gate
        self.assertEqual(gate["source"], "allowance")
        self.assertEqual(gate["retryAt"], "2026-08-22T17:14:58Z")
        self.assertEqual((budget["remaining"], budget["perHour"]), (0, 5))
        stated = at("2026-08-22T16:46:09Z") + 15 * 60 + 60
        self.assertGreaterEqual(gate["retryAtEpoch"], stated)
        # After the refusal, the notice and its stated wait keep it waiting.
        state, gate, _ = replay(
            "2026-08-22", "2026-08-22T16:46:10Z", rate_limited_at="2026-08-22T16:46:09Z"
        )
        self.assertEqual(state, "waiting")
        assert gate
        self.assertGreaterEqual(gate["retryAtEpoch"], stated)

    def test_replay_holds_before_every_refused_trigger_on_2026_08_22(self) -> None:
        refusals = [
            ("2026-08-22T16:46:02Z", "2026-08-22T16:46:09Z", 15),
            ("2026-08-22T17:56:07Z", "2026-08-22T17:56:15Z", 12),
            ("2026-08-22T18:18:09Z", "2026-08-22T18:18:15Z", 5),
        ]
        for trigger, refused, minutes in refusals:
            with self.subTest(trigger=trigger):
                before = ADAPTER.format_timestamp(at(trigger) - 1)
                state, gate, _ = replay("2026-08-22", before)
                self.assertEqual(state, "waiting")
                assert gate
                self.assertGreaterEqual(
                    gate["retryAtEpoch"], at(refused) + minutes * 60 + 60
                )

    def test_replay_2026_09_28_holds_until_the_next_review_ran(self) -> None:
        # 17:53:41 #1287: "0 included reviews remain", 2 per hour.
        state, gate, budget = replay("2026-09-28", "2026-09-28T18:00:00Z")
        self.assertEqual(state, "waiting")
        assert gate
        self.assertEqual(gate["retryAt"], "2026-09-28T18:48:09Z")
        self.assertEqual(budget["attempts"]["count"], 2)
        self.assertEqual(
            budget["source"],
            {"repo": "frostney/GocciaScript", "pr": 1287, "commentId": 5874954441},
        )
        # CodeRabbit's next review, #1288, began at 18:49:02 and reported
        # "1 included review remains"; the adapter permits a trigger by then.
        state, _, _ = replay("2026-09-28", "2026-09-28T18:49:00Z")
        self.assertEqual(state, "trigger-incremental")
        later = RecordedGh(RECORDED["timelines"]["2026-09-28"], at("2026-09-28T19:02:00Z"))
        statement, _ = ADAPTER.scan_allowance(later, RECORDED_SCAN, at("2026-09-28T17:02:00Z"))
        assert statement
        self.assertEqual((statement["pr"], statement["remaining"]), (1288, 1))


TABLE_NOW = START + 900
NOTICE_AT = START + 90
STATED_AT = START + 30
WAITS = {
    # stated wait comment (edited at, minutes) -> retry at
    "active": (START + 800, 90),
    "elapsed": (START + 100, 5),
    "before-notice": (START + 80, 90),
}
UNRATED = {
    1: "**Included review availability:** 1 included review remains after this review.",
    0: "**Limit details:** You’ve used the included review currently available.",
}
# Counted-run layouts around a statement made at STATED_AT (2 per hour when rated):
# free: "1 remains" with its run; used: "1 remains", then one counted run after it;
# zero: "0 remain" after an earlier run; zero-uncounted: "0 remain" with no Run ID;
# none: "1 remains" with no Run ID.
LAYOUTS = {
    "free": (1, "run-b", []),
    "used": (1, "run-b", [("run-a", START - 1200), ("run-c", START + 45)]),
    "zero": (0, "run-b", [("run-a", START - 1200)]),
    "zero-uncounted": (0, None, []),
    "none": (1, None, []),
}
# Hand-derived allowance times per (statement, layout): the used-up allowance's
# retry_at (A) and, for a notice at NOTICE_AT, when the runs counted before it
# free a slot (N).
FACTS: dict[tuple[str, str], tuple[int | None, int | None]] = {
    ("rated", "free"): (None, STATED_AT + HOUR + 60),
    ("rated", "used"): (STATED_AT + HOUR + 60, STATED_AT + HOUR + 60),
    ("rated", "zero"): (START - 1200 + HOUR + 60, START - 1200 + HOUR + 60),
    ("rated", "zero-uncounted"): (STATED_AT + HOUR + 60, None),
    ("rated", "none"): (None, None),
    ("unrated", "free"): (None, None),
    ("unrated", "used"): (START + 45 + HOUR + 60, None),
    ("unrated", "zero"): (STATED_AT + HOUR + 60, None),
    ("unrated", "zero-uncounted"): (STATED_AT + HOUR + 60, None),
    ("unrated", "none"): (None, None),
}


def documented_state(
    statement: str, layout: str, wait: str, notice: str
) -> tuple[str, str | None, int | None]:
    """The decision rules in references/pr-readiness.md, applied to one input."""
    if notice == "check":
        return "pending-retry-source", None, None
    used_up, freed = FACTS.get((statement, layout), (None, None))
    gates: list[tuple[int, str]] = []
    if wait in WAITS and not (notice == "comment" and wait == "before-notice"):
        edited, minutes = WAITS[wait]
        gates.append((edited + minutes * 60 + 60, "stated-wait"))
    if used_up is not None:
        gates.append((used_up, "allowance"))
    if notice == "comment" and freed is not None:
        gates.append((freed, "allowance"))
    if notice == "comment" and not gates:
        return "pending-retry-source", None, None
    if gates:
        retry, source = max(gates)
        if retry > TABLE_NOW:
            return "waiting", source, retry
    return "trigger-incremental", None, None


def decision_gh(statement: str, layout: str, wait: str, notice: str) -> FakeGh:
    remaining, run_id, runs = LAYOUTS[layout]
    stated = STATED_AT if statement in {"rated", "unrated"} else TABLE_NOW - HOUR - 200
    account: list[dict[str, Any]] = [
        comment(80, "Review finished", START, issue_url="https://api.github.test/repos/owner/repo/issues/8"),
    ]
    reviews: dict[int, list[dict[str, Any]]] = {
        8: [run_review_object(800 + i, run, when) for i, (run, when) in enumerate(runs)]
    }
    if statement == "missing":
        if run_id:
            reviews[8].append(run_review_object(890, run_id, stated))
    else:
        text = UNRATED[remaining] if statement == "unrated" else availability(remaining, 2)
        account.append(summary_comment(91, 9, stated, text, run_id))
    if wait in WAITS:
        edited, minutes = WAITS[wait]
        account.append(
            comment(95, f"**Next review available in:** **{minutes} minutes**", edited,
                    issue_url="https://api.github.test/repos/owner/repo/issues/5")
        )
    refused = [
        comment(10, "@coderabbitai review", START + 60, bot=False),
        comment(11, "<summary>⚠️ Action not completed</summary>\n\nReview rate limited.", NOTICE_AT),
    ]
    gh = budget_gh(
        account,
        now=TABLE_NOW,
        pr_comments=refused if notice == "comment" else None,
        other_reviews=reviews,
    )
    if notice == "check":
        gh.pages["repos/owner/repo/commits/head-7/check-runs?per_page=100"] = [{
            "check_runs": [{
                "name": "CodeRabbit", "head_sha": "head-7", "status": "completed",
                "conclusion": "success", "output": {"title": "Review rate limited"},
            }]
        }]
    return gh


class AllowanceDecisionTableTest(unittest.TestCase):
    def test_every_input_combination_reaches_its_documented_state(self) -> None:
        combinations = [
            (statement, layout, wait, notice)
            for statement in ("rated", "unrated", "stale", "missing")
            for layout in LAYOUTS
            for wait in ("none", "active", "elapsed", "before-notice")
            for notice in ("none", "comment", "check")
        ]
        self.assertEqual(len(combinations), 240)
        outcomes = set()
        for statement, layout, wait, notice in combinations:
            with self.subTest(statement=statement, layout=layout, wait=wait, notice=notice):
                state, source, retry = documented_state(statement, layout, wait, notice)
                outcomes.add((state, source))
                gh = decision_gh(statement, layout, wait, notice)
                item = observe(gh, TABLE_NOW)["pullRequests"][0]
                self.assertEqual(item["state"], state)
                self.assertEqual(item["retrySource"], source)
                self.assertEqual(item["retryAt"], iso(retry) if retry else None)
        self.assertEqual(
            outcomes,
            {
                ("trigger-incremental", None),
                ("waiting", "allowance"),
                ("waiting", "stated-wait"),
                ("pending-retry-source", None),
            },
        )


class BoundedScanTest(unittest.TestCase):
    """Repository-wide reads stay within the scan horizon on busy repositories."""

    def busy_gh(self, now: int, wait_age: int, stated_minutes: int) -> FakeGh:
        gh = FakeGh()
        requested: list[str] = []
        unbounded = "repos/owner/busy/issues/comments?sort=updated&direction=desc&per_page=100"
        # 60 pages of old comments, as on a repository with 6,000 of them.
        gh.pages[unbounded] = [
            [comment(page * 100 + i, "LGTM", START - 86400 * 30) for i in range(100)]
            for page in range(60)
        ]
        gh.pages[ADAPTER.recent_comments_endpoint("owner/busy", ADAPTER.scan_since(now))] = [[
            comment(
                7001,
                f"**Next review available in:** **{stated_minutes} minutes**",
                now - wait_age,
                issue_url="https://api.github.test/repos/owner/busy/issues/5",
            )
        ]]
        original = gh.rest_pages

        def rest_pages(endpoint: str) -> list[Any]:
            requested.append(endpoint)
            if endpoint == unbounded:
                raise AssertionError("unbounded repository-wide comment scan")
            return original(endpoint)

        gh.rest_pages = rest_pages  # type: ignore[method-assign]
        gh.requested = requested  # type: ignore[attr-defined]
        return gh

    def test_account_wait_reads_only_the_horizon_and_keeps_an_active_wait(self) -> None:
        # The longest wait CodeRabbit has stated is 59 minutes; one stated
        # 58 minutes ago is still active and lies inside the two-hour horizon.
        gh = self.busy_gh(NOW, 58 * 60, 59)
        wait = ADAPTER.account_wait(gh, ["owner/busy"], NOW)
        assert wait
        self.assertEqual(wait["retryAtEpoch"], NOW - 58 * 60 + 59 * 60 + 60)
        self.assertGreater(wait["retryAtEpoch"], NOW)
        self.assertEqual(
            gh.requested,  # type: ignore[attr-defined]
            [ADAPTER.recent_comments_endpoint("owner/busy", ADAPTER.scan_since(NOW))],
        )

    def test_status_observation_never_scans_a_repository_unbounded(self) -> None:
        gh = self.busy_gh(NOW, 58 * 60, 59)
        original = gh.rest_pages

        def rest_pages(endpoint: str) -> list[Any]:
            if endpoint.startswith("repos/owner/repo/"):
                return configured_gh().rest_pages(endpoint)
            return original(endpoint)

        gh.rest_pages = rest_pages  # type: ignore[method-assign]
        gh.values.update(configured_gh().values)
        value = ADAPTER.observation(
            gh, "owner/repo", [7], {7: "head-7"}, ["owner/repo", "owner/busy"], NOW
        )
        self.assertEqual(value["pullRequests"][0]["state"], "waiting")
        self.assertEqual(value["pullRequests"][0]["retrySource"], "stated-wait")
        scans = [e for e in gh.requested if "/issues/comments?" in e]  # type: ignore[attr-defined]
        self.assertTrue(scans)
        self.assertTrue(all("&since=" in endpoint for endpoint in scans))

    def test_allowance_statement_in_a_review_object_counts(self) -> None:
        # frostney/GocciaScript#1291, 2026-09-29: the statement appears only in
        # CodeRabbit's review body, not in the summary comment.
        statement = next(v["text"] for v in RECORDED["variants"] if v.get("reviewId") == 5350559230)
        gh = budget_gh(
            [comment(81, "<!-- summarize by coderabbit.ai -->", NOW - 700, updated=NOW - 300,
                     issue_url="https://api.github.test/repos/owner/repo/issues/8")],
            other_reviews={
                8: [run_review_object(801, "run-a", NOW - 600, body=f"Actionable comments posted: 1\n\n{statement}")]
            },
        )
        value = observe(gh)
        allowance = value["allowance"]
        self.assertEqual(allowance["mode"], "enforced")
        self.assertEqual((allowance["remaining"], allowance["perHour"]), (0, 1))
        self.assertEqual(allowance["source"], {"repo": "owner/repo", "pr": 8, "reviewId": 801})
        self.assertEqual(allowance["statementAt"], iso(NOW - 600))
        self.assertEqual(value["pullRequests"][0]["state"], "waiting")
        self.assertEqual(value["pullRequests"][0]["retryAt"], iso(NOW - 600 + HOUR + 60))


NON_STATEMENTS = {
    # pattern -> (why the line carries no allowance figure, whether it states a wait)
    r"has exceeded the limit for the number of commits that can be reviewed per hour": ("an old refusal notice", True),
    r"^> More reviews will be available in": ("a refusal notice's stated wait", True),
    r"^Your included review limit is currently reached": ("a refusal reply", False),
    r"^Review rate limited\.$": ("a refusal reply", False),
    r"we couldn't start this review because you've reached your PR review rate limit": ("a refusal notice", False),
    r"CodeRabbit enforces per-developer PR review limits": ("explains limits in general", False),
    r"Enable \*\*\[usage-based reviews\]": ("a billing offer the adapter never follows", False),
}


def edited_summary(identifier: int, pr: int, versions: list[tuple[int, str]]) -> dict[str, Any]:
    """A summary comment whose current body is its last version."""
    item = comment(
        identifier, versions[-1][1], versions[0][0], updated=versions[-1][0],
        issue_url=f"https://api.github.test/repos/owner/repo/issues/{pr}",
    )
    return item


def summary_body(statement: str, run_id: str | None, *, processing: str | None = None) -> str:
    parts = ["<!-- This is an auto-generated comment: summarize by coderabbit.ai -->"]
    if processing:
        parts += [
            "<!-- This is an auto-generated comment: review in progress by coderabbit.ai -->",
            "> Currently processing new changes in this PR. This may take a few minutes, please wait...",
            f"> **Run ID**: `{processing}`",
            "<!-- end of auto-generated comment: review in progress by coderabbit.ai -->",
        ]
    parts.append("<!-- recent_review_start -->")
    if run_id:
        parts.append(f"**Run ID**: `{run_id}`")
    parts += [statement, "<!-- recent_review_end -->"]
    return "\n".join(parts)


class ReviewFindingsTest(unittest.TestCase):
    """Regressions from the independent review of PR #94 (CR-1 to CR-13)."""

    # CR-1
    def test_every_sampled_allowance_line_parses_or_is_a_known_non_statement(self) -> None:
        corpus = RECORDED["allowanceLines"]
        self.assertEqual(
            corpus["sample"],
            {
                "pullRequests": 914, "comments": 1175, "commentVersions": 7701,
                "reviewBodies": 1368, "distinctLines": 412, "occurrences": 1106,
            },
        )
        self.assertEqual(len(corpus["lines"]), 412)
        legacy = [line for line in corpus["lines"] if "reviews remaining" in line or "review remaining" in line]
        self.assertEqual(len(legacy), 180)
        for line in legacy:
            with self.subTest(legacy=line):
                # "R/P ... refill in [M minute(s)] [S second(s)]", read independently.
                remaining, allowance = map(int, re.search(r"(\d+)/(\d+)", line).groups())
                minutes = re.search(r"(\d+) minutes?", line)
                seconds = re.search(r"(\d+) seconds?", line)
                parsed = ADAPTER.parse_allowance(line)
                assert parsed
                self.assertEqual((parsed["remaining"], parsed["allowance"]), (remaining, allowance))
                self.assertEqual(
                    parsed["refillSeconds"],
                    (int(minutes.group(1)) * 60 if minutes else 0) + (int(seconds.group(1)) if seconds else 0),
                )
        modern = [line for line in corpus["lines"] if line not in legacy and not any(re.search(p, line) for p in NON_STATEMENTS)]
        self.assertEqual(len(modern), 65)
        for line in modern:
            with self.subTest(modern=line):
                # Read independently: "used all"/"used the" means none left;
                # otherwise the count before "remain(s)"/"is|are currently
                # available"; the rate is the number before "per hour".
                if re.search(r"used (?:all|the)", line):
                    remaining = 0
                else:
                    remaining = int(re.search(r"(\d+)(?: included)?(?: reviews?)? (?:remains?|is currently|are currently)", line).group(1))
                rates = re.findall(r"(\d+)(?: included)?(?: reviews?)? per hour", line)
                parsed = ADAPTER.parse_allowance(line)
                assert parsed
                self.assertEqual(parsed["remaining"], remaining)
                self.assertEqual(parsed["allowance"], int(rates[-1]) if rates else None)
        used: set[str] = set()
        for line in corpus["lines"]:
            reason = next((p for p in NON_STATEMENTS if re.search(p, line)), None)
            if reason:
                used.add(reason)
                if NON_STATEMENTS[reason][1]:
                    self.assertRegex(line, ADAPTER.STATED_WAIT)
                continue
            with self.subTest(line=line):
                parsed = ADAPTER.parse_allowance(line)
                assert parsed, "allowance-looking line does not parse"
                self.assertTrue(parsed["recognized"])
                self.assertIsNotNone(parsed["remaining"])
        self.assertEqual(used, set(NON_STATEMENTS), "every exclusion still matches real text")
        self.assertIn(
            "**Included review availability:** Your plan provides up to 1 included review per hour; 0 remain after this review.",
            corpus["lines"],
        )

    def test_unrecognized_allowance_wording_holds_instead_of_falling_back(self) -> None:
        for wording, retry in (
            # No rate: one hour after the statement.
            ("**Included review availability:** Your reviews reset at the top of the hour.", NOW - 120 + HOUR + 60),
            # A rate: read as "0 remain", so the counted runs free a slot.
            ("**Included review availability:** Your current allowance at 2 reviews per hour applies.", NOW - 1800 + HOUR + 60),
        ):
            with self.subTest(wording=wording):
                gh = budget_gh(
                    [
                        summary_comment(81, 8, NOW - 1800, availability(2, 2), "run-a"),
                        summary_comment(91, 9, NOW - 120, wording, "run-b"),
                    ]
                )
                value = observe(gh)
                allowance = value["allowance"]
                self.assertEqual(allowance["mode"], "degraded")
                self.assertIn("unrecognized", allowance["reason"])
                self.assertEqual(value["pullRequests"][0]["state"], "waiting")
                self.assertEqual(value["pullRequests"][0]["retryAt"], iso(retry))

    # CR-2
    def test_replay_2026_09_26_dates_a_carried_statement_by_its_first_appearance(self) -> None:
        # 20:52:53 #1262 adds a "Currently processing" block above the
        # 17:11:23 "4 remain" statement (run f7aac9ef): it is stale, not new.
        _, _, budget = replay("2026-09-26", "2026-09-26T20:53:00Z")
        self.assertEqual(budget["mode"], "degraded")
        self.assertIsNone(budget["statementAt"])
        # 20:59:31 run 22c0208d states "4 remain"; the 21:32:55 processing
        # edit carries it without re-dating it.
        for now in ("2026-09-26T21:00:00Z", "2026-09-26T21:33:00Z"):
            with self.subTest(now=now):
                _, _, budget = replay("2026-09-26", now)
                self.assertEqual(budget["statementAt"], "2026-09-26T20:59:31Z")
                self.assertEqual((budget["remaining"], budget["perHour"]), (4, 5))

    def test_carried_old_statement_does_not_outrank_a_newer_zero(self) -> None:
        old = summary_body(availability(4, 5), "run-old")
        carried = summary_body(availability(4, 5), "run-old", processing="run-new")
        gh = budget_gh(
            [
                summary_comment(81, 8, NOW - 300, availability(0, 2), "run-z"),
                edited_summary(91, 9, [(NOW - 5000, old), (NOW - 60, carried)]),
            ]
        )
        gh.edits["IC_91"] = [(iso(NOW - 5000), old), (iso(NOW - 60), carried)]
        value = observe(gh)
        self.assertEqual(value["allowance"]["source"], {"repo": "owner/repo", "pr": 8, "commentId": 81})
        self.assertEqual(value["pullRequests"][0]["state"], "waiting")
        self.assertEqual(value["pullRequests"][0]["retryAt"], iso(NOW - 300 + HOUR + 60))

    def test_statement_that_cannot_be_dated_never_permits_a_trigger(self) -> None:
        def failing(_query: str, _variables: dict[str, Any]) -> dict[str, Any]:
            raise ADAPTER.WaitError("gh: Something went wrong (HTTP 422)")

        for remaining, state in ((4, "trigger-incremental"), (0, "waiting")):
            with self.subTest(remaining=remaining):
                body = summary_body(availability(remaining, 5), "run-old", processing="run-new")
                gh = budget_gh([edited_summary(91, 9, [(NOW - 5000, body), (NOW - 60, body)])])
                gh.graphql = failing  # type: ignore[method-assign]
                value = observe(gh)
                self.assertEqual(value["pullRequests"][0]["state"], state)
                self.assertEqual(value["allowance"]["mode"], "degraded")
                self.assertIn("could not be dated", value["allowance"]["reason"])

    # CR-3
    def refusal(self, wait_minutes: int) -> list[dict[str, Any]]:
        return [
            comment(10, "@coderabbitai review", START + 60, bot=False),
            comment(
                11,
                "Your included review limit is currently reached under our Fair Usage Limits "
                "Policy. This review may still proceed through usage-based billing if eligible. "
                f"Your next included review will be available in {wait_minutes} minutes.",
                START + 90,
            ),
        ]

    def test_old_refusal_whose_own_stated_wait_elapsed_can_retrigger(self) -> None:
        # Reviewer's branch-vs-main case: refused 3 h ago with a 15-minute wait,
        # outside the account scan's horizon. main returned trigger-incremental.
        gh = configured_gh(comments=self.refusal(15))
        value = ADAPTER.observation(gh, "owner/repo", [7], {7: "head-7"}, ["owner/repo"], START + 3 * HOUR)
        self.assertIsNone(value["accountWait"])
        self.assertEqual(value["pullRequests"][0]["state"], "trigger-incremental")

    def test_refusal_that_carries_its_own_wait_waits_on_it(self) -> None:
        gh = configured_gh(comments=self.refusal(15))
        value = ADAPTER.observation(gh, "owner/repo", [7], {7: "head-7"}, ["owner/repo"], START + 600)
        item = value["pullRequests"][0]
        self.assertEqual(item["state"], "waiting")
        self.assertEqual(item["retrySource"], "stated-wait")
        self.assertEqual(item["retryAt"], iso(START + 90 + 15 * 60 + 60))

    # CR-5
    def test_statement_stays_current_until_its_own_retry_time(self) -> None:
        for age, state in ((HOUR + 30, "waiting"), (HOUR + 60, "trigger-incremental")):
            with self.subTest(age=age):
                gh = budget_gh([summary_comment(81, 8, NOW - age, availability(0, 2), None)])
                item = observe(gh)["pullRequests"][0]
                self.assertEqual(item["state"], state)
                if state == "waiting":
                    self.assertEqual(item["retryAt"], iso(NOW - age + HOUR + 60))

    # CR-9
    def test_available_now_subtracts_runs_counted_after_the_statement(self) -> None:
        gh = budget_gh(
            [summary_comment(91, 9, NOW - 600, availability(2, 5), "run-b")],
            other_reviews={9: [run_review_object(901, "run-c", NOW - 300)]},
        )
        allowance = observe(gh)["allowance"]
        self.assertEqual((allowance["remaining"], allowance["availableNow"]), (2, 1))
        gh = budget_gh([summary_comment(91, 9, NOW - 600, availability(0, 5), "run-b")])
        self.assertEqual(observe(gh)["allowance"]["availableNow"], 0)
        self.assertIsNone(observe(budget_gh([]))["allowance"]["availableNow"])

    # CR-10
    def test_observation_requests_each_endpoint_once(self) -> None:
        gh = budget_gh(
            [
                summary_comment(71, 7, NOW - 600, availability(1, 2), "run-a"),
                comment(95, "**Next review available in:** **5 minutes**", NOW - 100,
                        issue_url="https://api.github.test/repos/owner/repo/issues/5"),
            ]
        )
        observe(gh)
        duplicates = {r for r in gh.requests if gh.requests.count(r) > 1}
        self.assertEqual(duplicates, set())

    # CR-8: rules the mutation run left unpinned
    def test_recorded_limit_block_binds_its_refused_run_without_counting_it(self) -> None:
        body = next(n["body"] for n in RECORDED["notices"] if n["kind"] == "summary-rate-limited-with-stated-wait")
        gh = FakeGh()
        gh.pages[ADAPTER.recent_comments_endpoint("owner/repo", ADAPTER.scan_since(NOW))] = [
            [comment(81, body, NOW - 60, issue_url="https://api.github.test/repos/owner/repo/issues/8")]
        ]
        statement, runs = ADAPTER.scan_allowance(gh, ["owner/repo"], ADAPTER.scan_since(NOW))
        assert statement
        self.assertEqual(statement["runId"], "73d78305-1fd6-45f7-8c1b-dfe914ae8f96")
        self.assertTrue(statement["refused"])
        self.assertEqual(runs, {})

    def test_allowance_time_before_the_notice_does_not_explain_it(self) -> None:
        refused = [
            comment(10, "@coderabbitai review", START + 150, bot=False),
            comment(11, "Review rate limited.", START + 200),
        ]
        gh = budget_gh(
            [
                comment(80, "Review finished", START - 3500, issue_url="https://api.github.test/repos/owner/repo/issues/8"),
                summary_comment(91, 9, START - 2600, availability(0, 2), None),
            ],
            now=START + 900,
            pr_comments=refused,
            other_reviews={8: [run_review_object(801, "run-a", START - 3500)]},
        )
        item = observe(gh, START + 900)["pullRequests"][0]
        self.assertEqual(item["state"], "pending-retry-source")

    def test_a_run_counts_at_its_earliest_observation(self) -> None:
        gh = budget_gh(
            [
                comment(80, "Review finished", NOW - 900, issue_url="https://api.github.test/repos/owner/repo/issues/8"),
                summary_comment(91, 9, NOW - 600, availability(0, 2), "run-z"),
            ],
            other_reviews={8: [run_review_object(801, "run-a", NOW - 1900), run_review_object(802, "run-a", NOW - 1000)]},
        )
        self.assertEqual(observe(gh)["pullRequests"][0]["retryAt"], iso(NOW - 1900 + HOUR + 60))

    def test_per_day_statement_rescans_two_days_of_evidence(self) -> None:
        daily = (
            "**Included review availability:** This review used your included allowance. "
            "0 included reviews remain after this review. Your included PR review attempts "
            "over the past 7 days set your current allowance at 24 reviews per day."
        )
        gh = budget_gh([summary_comment(91, 9, NOW - 600, daily, "run-z")])
        day_scan = ADAPTER.recent_comments_endpoint("owner/repo", ADAPTER.scan_since(NOW, 86400))
        gh.pages[day_scan] = [[
            summary_comment(91, 9, NOW - 600, daily, "run-z"),
            comment(80, "Review finished", NOW - 40000, issue_url="https://api.github.test/repos/owner/repo/issues/8"),
        ]]
        gh.pages["repos/owner/repo/pulls/8/reviews?per_page=100"] = [[run_review_object(801, "run-a", NOW - 40000)]]
        self.assertEqual(observe(gh)["allowance"]["retryAt"], iso(NOW - 40000 + 86400 + 60))

    def test_a_run_exactly_one_window_old_is_outside_it(self) -> None:
        at_ = NOW
        self.assertEqual(
            ADAPTER.allowance_retry([at_ - HOUR, at_ - 10], HOUR, 2, at_, proven=True),
            at_ - 10 + HOUR + 60,
        )

    def test_the_nth_run_after_a_statement_uses_up_its_n_reviews(self) -> None:
        two = availability(2, 2)
        gh = budget_gh(
            [summary_comment(91, 9, NOW - 900, two, "run-b")],
            other_reviews={9: [
                run_review_object(901, "run-c", NOW - 600),
                run_review_object(902, "run-d", NOW - 300),
                run_review_object(903, "run-e", NOW - 100),
            ]},
        )
        self.assertEqual(observe(gh)["allowance"]["retryAt"], iso(NOW - 600 + HOUR + 60))

    def test_statement_is_stale_exactly_one_window_and_buffer_after_it(self) -> None:
        # At the boundary the statement no longer explains a new notice.
        refused = [
            comment(10, "@coderabbitai review", NOW - 20, bot=False),
            comment(11, "Review rate limited.", NOW - 10),
        ]
        gh = budget_gh(
            [summary_comment(91, 9, NOW - HOUR - 60, availability(1, 2), "run-b")],
            pr_comments=refused,
            other_reviews={9: [run_review_object(901, "run-c", NOW - 100)]},
        )
        self.assertEqual(observe(gh)["pullRequests"][0]["state"], "pending-retry-source")

    def test_a_summary_statement_is_dated_by_its_runs_review(self) -> None:
        def failing(_query: str, _variables: dict[str, Any]) -> dict[str, Any]:
            raise ADAPTER.WaitError("gh: Something went wrong (HTTP 422)")

        body = summary_body(availability(0, 2), "run-z", processing="run-new")
        gh = budget_gh(
            [edited_summary(91, 9, [(NOW - 700, body), (NOW - 60, body)])],
            other_reviews={9: [run_review_object(901, "run-z", NOW - 600)]},
        )
        gh.graphql = failing  # type: ignore[method-assign]
        allowance = observe(gh)["allowance"]
        self.assertEqual(allowance["statementDating"], "run")
        self.assertEqual(allowance["statementAt"], iso(NOW - 600))
        self.assertEqual(allowance["retryAt"], iso(NOW - 600 + HOUR + 60))

    def test_a_statement_binds_only_a_run_in_its_own_block(self) -> None:
        body = "\n".join([
            "<!-- This is an auto-generated comment: summarize by coderabbit.ai -->",
            "<!-- This is an auto-generated comment: review in progress by coderabbit.ai -->",
            "> **Run ID**: `run-new`",
            "<!-- end of auto-generated comment: review in progress by coderabbit.ai -->",
            "<!-- recent_review_start -->",
            availability(1, 2),
            "**Run ID**: `run-a`",
            "<!-- recent_review_end -->",
        ])
        found = ADAPTER.statement_in(body)
        assert found
        self.assertEqual((found["runId"], found["refused"]), ("run-a", False))

    def test_the_scan_reaches_the_window_of_a_statement_about_to_go_stale(self) -> None:
        # "0 remain" made one window and 30 s ago is current for 30 s more; the
        # run it reports on after is inside its window, just past two windows.
        gh = budget_gh([])
        scan = ADAPTER.recent_comments_endpoint("owner/repo", ADAPTER.scan_since(NOW))
        items = [
            summary_comment(81, 8, NOW - 2 * HOUR - 20, availability(1, 2), "run-a"),
            summary_comment(91, 9, NOW - HOUR - 30, availability(0, 2), None),
        ]
        for item in items:
            item["created_at"] = item["updated_at"]
        original = gh.rest_pages

        def rest_pages(endpoint: str) -> list[Any]:
            if endpoint.startswith("repos/owner/repo/issues/comments?"):
                since = at(endpoint.split("since=")[1])
                return [[i for i in items if at(i["updated_at"]) >= since]]
            return original(endpoint)

        gh.rest_pages = rest_pages  # type: ignore[method-assign]
        self.assertTrue(scan)
        value = observe(gh)
        self.assertFalse(value["allowance"]["exhausted"])
        self.assertEqual(value["pullRequests"][0]["state"], "trigger-incremental")

    def test_pending_and_human_reviews_and_human_comments_are_not_evidence(self) -> None:
        human_review = {**run_review_object(902, "run-h", NOW - 50, body=availability(0, 2)), "user": {"login": "maintainer"}}
        pending = {**run_review_object(903, "run-p", NOW - 100), "state": "PENDING"}
        gh = budget_gh(
            [
                summary_comment(91, 9, NOW - 600, availability(1, 2), "run-b"),
                comment(97, f"**Run ID**: `run-q`\n{availability(0, 2)}", NOW - 60, bot=False,
                        issue_url="https://api.github.test/repos/owner/repo/issues/9"),
            ],
            other_reviews={9: [human_review, pending]},
        )
        value = observe(gh)
        self.assertEqual(value["allowance"]["source"], {"repo": "owner/repo", "pr": 9, "commentId": 91})
        self.assertEqual(value["allowance"]["attempts"]["count"], 1)
        self.assertEqual(value["pullRequests"][0]["state"], "trigger-incremental")


class ZeroRemainReleaseRuleTest(unittest.TestCase):
    """Maintainer ruling on PR #94 CR-4 (amended acceptance criterion 5).

    A current "0 remain" statement holds until enough counted runs leave the
    window ending at it, plus 60 s; with no counted run tied to it, until the
    statement time plus one window plus 60 s.
    """

    def failing_history(self, _query: str, _variables: dict[str, Any]) -> dict[str, Any]:
        raise ADAPTER.WaitError("gh: Something went wrong (HTTP 422)")

    def test_each_branch_of_the_release_rule(self) -> None:
        unrated = "**Limit details:** You’ve used the included review currently available."
        undated = summary_body(availability(0, 2), None, processing="run-new")
        cases = {
            # Counted runs: the earlier run leaves the statement's window first.
            "counted runs": (
                [
                    comment(80, "Review finished", NOW - 2000, issue_url="https://api.github.test/repos/owner/repo/issues/8"),
                    summary_comment(91, 9, NOW - 600, availability(0, 2), "run-b"),
                ],
                {8: [run_review_object(801, "run-a", NOW - 1800)]},
                False,
                NOW - 1800 + HOUR + 60,
                "enforced",
            ),
            # No run can be tied to it: statement time plus one window.
            "no identifiable run": (
                [summary_comment(91, 9, NOW - 600, availability(0, 2), None)],
                {},
                False,
                NOW - 600 + HOUR + 60,
                "enforced",
            ),
            # No rate: read against one hour.
            "unrated": (
                [summary_comment(91, 9, NOW - 600, unrated, "run-b")],
                {},
                False,
                NOW - 600 + HOUR + 60,
                "degraded",
            ),
            # Its time cannot be established: its last edit times it.
            "undated": (
                [edited_summary(91, 9, [(NOW - 5000, undated), (NOW - 600, undated)])],
                {},
                True,
                NOW - 600 + HOUR + 60,
                "degraded",
            ),
        }
        for name, (account, reviews, history_fails, retry, mode) in cases.items():
            with self.subTest(branch=name):
                gh = budget_gh(account, other_reviews=reviews)
                if history_fails:
                    gh.graphql = self.failing_history  # type: ignore[method-assign]
                value = observe(gh)
                self.assertEqual(value["allowance"]["mode"], mode)
                self.assertTrue(value["allowance"]["exhausted"])
                self.assertEqual(value["allowance"]["retryAt"], iso(retry))
                self.assertEqual(value["pullRequests"][0]["state"], "waiting")
                self.assertEqual(value["pullRequests"][0]["retryAt"], iso(retry))
                released = observe(gh, retry)["pullRequests"][0]
                self.assertEqual(released["state"], "trigger-incremental")

    def test_counted_runs_release_before_the_statements_own_window_ends(self) -> None:
        # The ruling keeps the counted-run time even when it is earlier than
        # statement time + one window + 60 s: here the statement is still
        # current, yet the trigger is permitted.
        gh = budget_gh(
            [
                comment(80, "Review finished", NOW - 4000, issue_url="https://api.github.test/repos/owner/repo/issues/8"),
                summary_comment(91, 9, NOW - 3000, availability(0, 2), "run-b"),
            ],
            other_reviews={8: [run_review_object(801, "run-a", NOW - 3700)]},
        )
        value = observe(gh)
        self.assertEqual((value["allowance"]["mode"], value["allowance"]["remaining"]), ("enforced", 0))
        self.assertFalse(value["allowance"]["exhausted"])
        self.assertEqual(value["pullRequests"][0]["state"], "trigger-incremental")


def legacy_footer(remaining: int, allowance: int, refill: str) -> str:
    """CodeRabbit's April-May 2026 summary footer."""
    reviews = "review" if remaining == 1 else "reviews"
    return f"<sub>Review rate limit: {remaining}/{allowance} {reviews} remaining, refill in {refill}.</sub>"


class ReverificationFindingsTest(unittest.TestCase):
    """Regressions from the re-verification of PR #94 at 222f8e1 (NEW-1 to NEW-4, Q-1)."""

    def failing_history(self, _query: str, _variables: dict[str, Any]) -> dict[str, Any]:
        raise ADAPTER.WaitError("gh: Something went wrong (HTTP 422)")

    # NEW-1
    def test_legacy_none_remaining_holds_until_its_stated_refill(self) -> None:
        gh = budget_gh([summary_comment(91, 9, NOW - 600, legacy_footer(0, 5, "54 minutes and 32 seconds"), "run-b")])
        value = observe(gh)
        allowance = value["allowance"]
        self.assertEqual((allowance["remaining"], allowance["reviewsPerWindow"]), (0, 5))
        self.assertEqual(allowance["mode"], "degraded")
        self.assertTrue(allowance["exhausted"])
        release = NOW - 600 + 54 * 60 + 32 + 60
        self.assertEqual(value["pullRequests"][0]["state"], "waiting")
        self.assertEqual(value["pullRequests"][0]["retryAt"], iso(release))
        self.assertEqual(observe(gh, release)["pullRequests"][0]["state"], "trigger-incremental")

    def test_legacy_refill_keeps_a_statement_current_until_it_passes(self) -> None:
        # A refill longer than the assumed hour keeps the statement current
        # past one window plus the buffer.
        gh = budget_gh([summary_comment(91, 9, NOW - HOUR - 120, legacy_footer(0, 5, "70 minutes"), None)])
        item = observe(gh)["pullRequests"][0]
        self.assertEqual(item["state"], "waiting")
        self.assertEqual(item["retryAt"], iso(NOW - HOUR - 120 + 70 * 60 + 60))

    def test_legacy_reviews_remaining_permit_the_trigger(self) -> None:
        gh = budget_gh([summary_comment(91, 9, NOW - 600, legacy_footer(3, 5, "20 minutes and 56 seconds"), "run-b")])
        value = observe(gh)
        self.assertEqual((value["allowance"]["remaining"], value["allowance"]["availableNow"]), (3, 3))
        self.assertEqual(value["pullRequests"][0]["state"], "trigger-incremental")

    def test_unknown_legacy_footer_holds(self) -> None:
        gh = budget_gh(
            [summary_comment(91, 9, NOW - 600, "<sub>Review rate limit: paused for this hour.</sub>", "run-b")]
        )
        value = observe(gh)
        self.assertIn("unrecognized", value["allowance"]["reason"])
        self.assertEqual(value["pullRequests"][0]["retryAt"], iso(NOW - 600 + HOUR + 60))

    def test_a_seconds_only_stated_wait_is_read(self) -> None:
        body = next(n["body"] for n in RECORDED["notices"] if n["kind"] == "summary-rate-limit-exceeded-seconds-wait")
        refused = [comment(10, "@coderabbitai review", START + 60, bot=False), comment(11, body, START + 90)]
        gh = configured_gh(comments=refused)
        value = ADAPTER.observation(gh, "owner/repo", [7], {7: "head-7"}, ["owner/repo"], START + 120)
        item = value["pullRequests"][0]
        self.assertEqual(item["state"], "waiting")
        self.assertEqual(item["retryAt"], iso(START + 90 + 29 + 60))

    # NEW-2
    def test_undated_nonzero_statement_never_outranks_a_dated_zero(self) -> None:
        # With the edit history unreadable, a carried "4 remain" re-stamped
        # after a newer "0 remain" must not release the hold.
        carried = summary_body(availability(4, 5), "run-old", processing="run-new")
        gh = budget_gh(
            [
                summary_comment(81, 8, NOW - 300, availability(0, 2), "run-z"),
                edited_summary(91, 9, [(NOW - 5000, carried), (NOW - 60, carried)]),
            ],
            other_reviews={8: [run_review_object(801, "run-z", NOW - 300)]},
        )
        gh.graphql = self.failing_history  # type: ignore[method-assign]
        value = observe(gh)
        self.assertEqual(value["allowance"]["source"], {"repo": "owner/repo", "pr": 8, "commentId": 81})
        self.assertEqual(value["pullRequests"][0]["state"], "waiting")

    def history(self, count: int, statement: str) -> list[tuple[str, str]]:
        """`count` versions showing `statement`, one minute apart, after one that does not."""
        return [(iso(NOW - 100 * 60), summary_body("no statement yet", None))] + [
            (iso(NOW - (count - i) * 60), statement) for i in range(count)
        ]

    def test_edit_history_is_read_across_pages(self) -> None:
        body = summary_body(availability(0, 2), None, processing="run-new")
        versions = self.history(25, body)
        gh = budget_gh([edited_summary(91, 9, [(NOW - 100 * 60, versions[0][1]), (NOW - 60, body)])])
        gh.edits["IC_91"] = versions
        allowance = observe(gh)["allowance"]
        self.assertEqual(allowance["statementDating"], "edit-history")
        self.assertEqual(allowance["statementAt"], iso(NOW - 25 * 60))
        self.assertEqual(sum(1 for r in gh.requests if r == "graphql:IC_91"), 2)

    def test_edit_history_longer_than_the_page_limit_leaves_the_statement_undated(self) -> None:
        body = summary_body(availability(0, 2), None, processing="run-new")
        versions = self.history(45, body)
        gh = budget_gh([edited_summary(91, 9, [(NOW - 100 * 60, versions[0][1]), (NOW - 60, body)])])
        gh.edits["IC_91"] = versions
        with mock.patch.object(ADAPTER, "EDIT_HISTORY_PAGES", 2):
            allowance = observe(gh)["allowance"]
        self.assertEqual(allowance["statementDating"], "undated")
        self.assertEqual(allowance["statementAt"], iso(NOW - 60))

    def test_a_still_active_wait_from_before_the_push_gates_the_trigger(self) -> None:
        # The account scan's newest wait has elapsed; the PR's older, longer
        # wait is still active and is read from the PR's own comments.
        gh = configured_gh(
            comments=[
                comment(12, "**Next review available in:** **90 minutes**", START - 600),
                # A newer, shorter wait on the same PR has already elapsed.
                comment(13, "**Next review available in:** **1 minutes**", START - 300),
            ]
        )
        gh.pages[ADAPTER.recent_comments_endpoint("owner/repo", ADAPTER.scan_since(START + 180))] = [[
            comment(95, "**Next review available in:** **1 minutes**", START + 60,
                    issue_url="https://api.github.test/repos/owner/repo/issues/5")
        ]]
        value = ADAPTER.observation(gh, "owner/repo", [7], {7: "head-7"}, ["owner/repo"], START + 180)
        item = value["pullRequests"][0]
        self.assertEqual(item["state"], "waiting")
        self.assertEqual(item["retryAt"], iso(START - 600 + 90 * 60 + 60))

    def test_a_statement_prefers_the_run_printed_before_it(self) -> None:
        body = "\n".join([
            "<!-- recent_review_start -->",
            "**Run ID**: `run-before`",
            availability(1, 2),
            "**Run ID**: `run-after`",
            "<!-- recent_review_end -->",
        ])
        self.assertEqual(ADAPTER.statement_in(body)["runId"], "run-before")

    def test_a_statement_never_binds_a_run_from_a_later_block(self) -> None:
        body = "\n".join([
            "<!-- recent_review_start -->",
            availability(1, 2),
            "<!-- recent_review_end -->",
            "<!-- This is an auto-generated comment: review in progress by coderabbit.ai -->",
            "> **Run ID**: `run-later`",
            "<!-- end of auto-generated comment: review in progress by coderabbit.ai -->",
        ])
        self.assertIsNone(ADAPTER.statement_in(body)["runId"])

    def test_a_human_comment_states_no_wait(self) -> None:
        gh = FakeGh()
        gh.pages[ADAPTER.recent_comments_endpoint("owner/repo", ADAPTER.scan_since(NOW))] = [[
            comment(95, "**Next review available in:** **90 minutes**", NOW - 60, bot=False)
        ]]
        self.assertIsNone(ADAPTER.account_wait(gh, ["owner/repo"], NOW))

    def test_an_edit_dated_run_counts_when_it_ran_not_when_it_was_re_edited(self) -> None:
        first = summary_body(availability(1, 2), "run-old")
        carried = summary_body(availability(1, 2), "run-old", processing="run-new")
        gh = budget_gh([edited_summary(91, 9, [(NOW - 3000, first), (NOW - 60, carried)])])
        gh.edits["IC_91"] = [(iso(NOW - 3000), first), (iso(NOW - 60), carried)]
        value = observe(gh)
        self.assertEqual(value["allowance"]["attempts"]["runs"][0]["at"], iso(NOW - 3000))
        self.assertEqual(value["pullRequests"][0]["state"], "trigger-incremental")

    # Q-1: rolling-window reading of the CR-4 ruling
    def test_rolling_window_releases_when_the_oldest_counted_run_leaves(self) -> None:
        # 3 per hour: runs at NOW-3000 and NOW-2000, then "0 remain" at
        # NOW-1000. A slot frees when the NOW-3000 run leaves the window, not
        # when the statement's own run does (NOW-1000 + 1 h).
        gh = budget_gh(
            [
                comment(80, "Review finished", NOW - 3500, issue_url="https://api.github.test/repos/owner/repo/issues/8"),
                summary_comment(91, 9, NOW - 1000, availability(0, 3), "run-c"),
            ],
            other_reviews={8: [
                run_review_object(801, "run-a", NOW - 3000),
                run_review_object(802, "run-b", NOW - 2000),
            ]},
        )
        allowance = observe(gh)["allowance"]
        self.assertEqual(allowance["perHour"], 3)
        self.assertEqual(allowance["retryAt"], iso(NOW - 3000 + HOUR + 60))
        self.assertNotEqual(allowance["retryAt"], iso(NOW - 1000 + HOUR + 60))


class RoundThreeVerificationTest(unittest.TestCase):
    """Non-blocking improvements from the round-3 verification of PR #94 (RV3-1 to RV3-4)."""

    def test_a_prs_own_wait_before_a_notice_does_not_explain_it(self) -> None:
        # An old refusal's wait has elapsed; a later bare notice names no wait
        # and no allowance explains it, so no trigger is permitted.
        gh = configured_gh(
            comments=[
                comment(9, "Your next included review will be available in 15 minutes.", START + 30),
                comment(10, "@coderabbitai review", START + 1200, bot=False),
                comment(11, "Review rate limited.", START + 1230),
            ]
        )
        value = ADAPTER.observation(gh, "owner/repo", [7], {7: "head-7"}, ["owner/repo"], START + 1800)
        self.assertEqual(value["pullRequests"][0]["state"], "pending-retry-source")

    def test_a_human_comment_on_the_pr_states_no_wait(self) -> None:
        gh = configured_gh(comments=[comment(12, "Please wait 30 minutes before the next review.", START + 60, bot=False)])
        value = ADAPTER.observation(gh, "owner/repo", [7], {7: "head-7"}, ["owner/repo"], START + 180)
        self.assertEqual(value["pullRequests"][0]["state"], "trigger-incremental")

    def test_a_footers_refill_frees_only_its_own_none_remaining(self) -> None:
        # "1/5 remaining" used up by a later counted run is freed by counted
        # runs, not by the footer's refill.
        gh = budget_gh(
            [summary_comment(91, 9, NOW - 600, legacy_footer(1, 5, "50 minutes"), "run-b")],
            other_reviews={9: [run_review_object(901, "run-c", NOW - 300)]},
        )
        allowance = observe(gh)["allowance"]
        self.assertTrue(allowance["exhausted"])
        self.assertEqual(allowance["retryAt"], iso(NOW - 600 + HOUR + 60))


CLEAN = RECORDED["cleanReviews"]["1292"]
HEAD_1292 = CLEAN["head"]
CLEAN_NOW = "2026-09-29T13:45:00Z"
# Vercel's first status for 29204dc is 13:24:02; 52803a95 was reviewed from 09:38.
PUSHED_1292 = int(ADAPTER.parse_time("2026-09-29T13:24:00Z"))
PUSHED_PREVIOUS = int(ADAPTER.parse_time("2026-09-29T09:30:00Z"))


def clean_gh(
    version_at: str = "2026-09-29T13:39:41Z",
    *,
    statuses: list[dict[str, Any]] | None = None,
    head: str = HEAD_1292,
    pushed: int | None = PUSHED_1292,
    body: str | None = None,
    extra: list[dict[str, Any]] | None = None,
) -> FakeGh:
    """PR 7 at `head` with #1292's summary as CodeRabbit showed it at `version_at`."""
    versions = [v for v in CLEAN["versions"] if v["at"] <= version_at]
    summary = {
        "id": CLEAN["commentId"],
        "node_id": f"IC_{CLEAN['commentId']}",
        "user": {"login": "coderabbitai[bot]"},
        "body": body if body is not None else versions[-1]["body"],
        "created_at": CLEAN["createdAt"],
        "updated_at": versions[-1]["at"],
        "issue_url": "https://api.github.test/repos/owner/repo/issues/7",
    }
    gh = configured_gh(
        comments=[summary, *(extra or [])],
        head=head,
        coderabbit_statuses=CLEAN["statuses"] if statuses is None else statuses,
        pushed=pushed,
    )
    gh.edits[summary["node_id"]] = [(v["at"], v["body"]) for v in versions]
    return gh


def clean_observe(gh: FakeGh, head: str = HEAD_1292, now: str = CLEAN_NOW) -> dict[str, Any]:
    return ADAPTER.observation(gh, "owner/repo", [7], {7: head}, ["owner/repo"], at(now))["pullRequests"][0]


class CleanAutomaticReviewTest(unittest.TestCase):
    """Issue #95: a clean automatic review of the exact head completes it."""

    def test_recorded_1292_clean_automatic_review_is_clean_complete(self) -> None:
        item = clean_observe(clean_gh())
        self.assertEqual(item["state"], "clean-complete")
        self.assertIsNone(item["nextMode"])
        clean = item["cleanReview"]
        self.assertEqual(
            (clean["commentId"], clean["runId"], clean["head"], clean["reviewedAt"], clean["dating"]),
            (5887035508, "38da5e68-0cea-4549-91ec-a86ebd24bb85", HEAD_1292, "2026-09-29T13:39:41Z", "edit-history"),
        )

    def test_run_is_satisfied_without_posting(self) -> None:
        gh = clean_gh()
        clock = [at(CLEAN_NOW)]

        def sleep(seconds: float) -> None:
            clock[0] += seconds

        state, _, evidence = ADAPTER.run_review(
            gh, "owner/repo", 7, HEAD_1292, ["owner/repo"], at(CLEAN_NOW) + 30, 10,
            clock=lambda: clock[0], sleeper=sleep,
        )
        self.assertEqual(state, "satisfied")
        self.assertEqual(gh.posts, [])
        self.assertEqual(evidence["pullRequests"][0]["state"], "clean-complete")

    def test_an_already_reviewed_refusal_plans_no_full_review_of_a_clean_head(self) -> None:
        extra = [
            comment(10, "@coderabbitai review", START + 60, bot=False),
            comment(11, "Action not completed. These commits are already reviewed.", START + 90),
        ]
        item = clean_observe(clean_gh(extra=extra))
        self.assertEqual(item["state"], "clean-complete")

    def test_a_range_ending_at_an_earlier_head_never_counts(self) -> None:
        # 13:31:34: the recent review covers 72cb2050..52803a95.
        item = clean_observe(clean_gh("2026-09-29T13:31:34Z"))
        self.assertIsNone(item["cleanReview"])
        self.assertEqual(item["state"], "trigger-incremental")
        # The same summary does complete the head it names.
        previous = CLEAN["previousHead"]
        item = clean_observe(
            clean_gh("2026-09-29T13:31:34Z", head=previous, statuses=[CLEAN["statuses"][0]], pushed=PUSHED_PREVIOUS),
            head=previous,
        )
        self.assertEqual(item["state"], "clean-complete")

    def test_a_summary_still_processing_never_counts(self) -> None:
        # 13:35:11: the range naming the head is inside the in-progress block.
        item = clean_observe(clean_gh("2026-09-29T13:35:11Z"))
        self.assertIsNone(item["cleanReview"])
        self.assertEqual(item["state"], "trigger-incremental")
        processing = CLEAN["versions"][-1]["body"].replace(
            "<!-- recent_review_start -->",
            "> Currently processing new changes in this PR. This may take a few minutes, please wait...\n<!-- recent_review_start -->",
        )
        self.assertIsNone(clean_observe(clean_gh(body=processing))["cleanReview"])

    def test_a_status_that_is_not_a_completed_review_never_counts(self) -> None:
        cases = {
            "rate limited": ("success", "Review rate limited", "pending-retry-source"),
            "skipped": ("success", "Review skipped: draft pull request", "trigger-incremental"),
            "paused": ("success", "Reviews paused", "trigger-incremental"),
            "in progress": ("pending", "Review in progress", "trigger-incremental"),
        }
        for name, (state, description, expected) in cases.items():
            with self.subTest(status=name):
                statuses = [{"context": "CodeRabbit", "state": state, "description": description, "created_at": "2026-09-29T13:39:44Z"}]
                item = clean_observe(clean_gh(statuses=statuses))
                self.assertEqual(item["state"], expected)
        self.assertEqual(clean_observe(clean_gh(statuses=[]))["state"], "trigger-incremental")

    def test_a_summary_without_the_range_or_the_clean_verdict_never_counts(self) -> None:
        body = CLEAN["versions"][-1]["body"]
        no_range = "\n".join(l for l in body.splitlines() if not l.startswith("Reviewing files"))
        no_verdict = body.replace("No actionable comments were generated in the recent review. 🎉", "Actionable comments posted: 1")
        limited = body.replace(
            "<!-- recent_review_start -->",
            "<!-- This is an auto-generated comment: rate limited by coderabbit.ai -->\n> ## Review limit reached\n"
            "<!-- end of auto-generated comment: rate limited by coderabbit.ai -->\n<!-- recent_review_start -->",
        )
        for name, text in {"no range": no_range, "no clean verdict": no_verdict, "rate-limit block": limited}.items():
            with self.subTest(body=name):
                self.assertIsNone(clean_observe(clean_gh(body=text))["cleanReview"])

    def test_a_clean_review_older_than_the_heads_push_never_counts(self) -> None:
        pushed = int(at("2026-09-29T13:40:00Z"))
        item = clean_observe(clean_gh(pushed=pushed))
        self.assertIsNone(item["cleanReview"])
        self.assertNotEqual(item["state"], "clean-complete")

    def failing_history(self, _query: str, _variables: dict[str, Any]) -> dict[str, Any]:
        raise ADAPTER.WaitError("gh: Something went wrong (HTTP 422)")

    def clean_body(self, run_id: str, extra: str = "") -> str:
        return "\n".join([
            "<!-- This is an auto-generated comment: summarize by coderabbit.ai -->",
            "<!-- recent_review_start -->",
            "No actionable comments were generated in the recent review. 🎉",
            f"**Run ID**: `{run_id}`",
            f"Reviewing files that changed from the base of the PR and between {CLEAN['previousHead']} and {HEAD_1292}.",
            "<!-- recent_review_end -->",
            extra,
        ])

    def test_the_clean_verdict_must_be_in_the_recent_review_block(self) -> None:
        body = "\n".join([
            "<!-- This is an auto-generated comment: summarize by coderabbit.ai -->",
            "<!-- recent_review_start -->",
            "Actionable comments posted: 1",
            f"Reviewing files that changed from the base of the PR and between {CLEAN['previousHead']} and {HEAD_1292}.",
            "<!-- recent_review_end -->",
            "<!-- walkthrough_start -->",
            "No actionable comments were generated in the recent review.",
            "<!-- walkthrough_end -->",
        ])
        self.assertIsNone(ADAPTER.clean_review_in(body))

    def test_a_clean_review_that_cannot_be_dated_never_counts(self) -> None:
        gh = clean_gh()
        gh.graphql = self.failing_history  # type: ignore[method-assign]
        item = clean_observe(gh)
        self.assertIsNone(item["cleanReview"])
        self.assertNotEqual(item["state"], "clean-complete")

    def test_a_clean_review_is_dated_by_its_runs_review_object(self) -> None:
        gh = clean_gh()
        gh.graphql = self.failing_history  # type: ignore[method-assign]
        gh.pages["repos/owner/repo/pulls/7/reviews?per_page=100"] = [[{
            "id": 1,
            "user": {"login": "coderabbitai[bot]"},
            "body": "**Run ID**: `38da5e68-0cea-4549-91ec-a86ebd24bb85`",
            "state": "COMMENTED",
            # On another commit, so only its dating is exercised: a review
            # object on the head itself would already be review-complete.
            "commit_id": CLEAN["previousHead"],
            "submitted_at": "2026-09-29T13:39:40Z",
        }]]
        item = clean_observe(gh)
        self.assertEqual((item["state"], item["cleanReview"]["dating"]), ("clean-complete", "run"))
        self.assertEqual(item["cleanReview"]["reviewedAt"], "2026-09-29T13:39:40Z")

    def test_a_skipped_check_run_is_not_a_completed_review(self) -> None:
        gh = clean_gh(statuses=[])
        gh.pages[f"repos/owner/repo/commits/{HEAD_1292}/check-runs?per_page=100"] = [{
            "check_runs": [{
                "name": "CodeRabbit", "head_sha": HEAD_1292, "status": "completed",
                "conclusion": "success", "output": {"title": "Review skipped"},
            }]
        }]
        self.assertNotEqual(clean_observe(gh)["state"], "clean-complete")

    def test_a_carried_clean_block_is_dated_by_its_first_appearance(self) -> None:
        # The same run's clean block was shown before the head's push and is
        # only carried by a later edit, so it does not count.
        body = self.clean_body("run-early")
        gh = clean_gh(body=body)
        gh.edits[f"IC_{CLEAN['commentId']}"] = [("2026-09-29T13:10:00Z", body), ("2026-09-29T13:39:41Z", body + "\n")]
        self.assertIsNone(clean_observe(gh)["cleanReview"])

    def test_a_new_run_of_the_same_head_is_dated_by_its_own_first_appearance(self) -> None:
        early, late = self.clean_body("run-early"), self.clean_body("run-late")
        gh = clean_gh(body=late)
        gh.edits[f"IC_{CLEAN['commentId']}"] = [("2026-09-29T13:10:00Z", early), ("2026-09-29T13:39:41Z", late)]
        item = clean_observe(gh)
        self.assertEqual(item["cleanReview"]["reviewedAt"], "2026-09-29T13:39:41Z")
        self.assertEqual(item["state"], "clean-complete")

    def test_a_later_refusal_does_not_undo_a_clean_review_of_the_head(self) -> None:
        extra = [
            comment(10, "@coderabbitai review", int(at("2026-09-29T13:41:00Z")), bot=False),
            comment(11, "Review rate limited.", int(at("2026-09-29T13:41:10Z"))),
        ]
        self.assertEqual(clean_observe(clean_gh(extra=extra))["state"], "clean-complete")

    def test_every_recorded_summary_version_names_the_head_it_covered(self) -> None:
        # Each version's clean verdict binds the range end of its own recent
        # review, which CodeRabbit's coverage marker independently names.
        for version in CLEAN["versions"]:
            with self.subTest(at=version["at"]):
                found = ADAPTER.clean_review_in(version["body"])
                recent = re.search(
                    r"<!-- recent_review_start -->([\s\S]*?)<!-- recent_review_end -->", version["body"]
                )
                processing = "Currently processing" in version["body"]
                if recent and "No actionable comments" in recent.group(1) and not processing:
                    assert found
                    covered = re.search(r'"coveredCommitId":"([0-9a-f]{40})"', version["body"]).group(1)
                    self.assertEqual(found["head"], covered)
                else:
                    self.assertIsNone(found)


def recorded_clean(number: str, at_: str | None = None, *, statuses: list[dict[str, Any]] | None = None) -> tuple[FakeGh, str, str]:
    """PR 7 as GitHub showed recorded PR `number` at `at_` (default: its evaluation time)."""
    case = RECORDED["cleanReviews"][number]
    now = at_ or case["evaluateAt"]
    versions = [v for v in case["versions"] if v["at"] <= now]
    summary = {
        "id": case["commentId"],
        "node_id": f"IC_{case['commentId']}",
        "user": {"login": "coderabbitai[bot]"},
        "body": versions[-1]["body"],
        "created_at": case["createdAt"],
        "updated_at": versions[-1]["at"],
        "issue_url": "https://api.github.test/repos/owner/repo/issues/7",
    }
    others = [
        {
            "id": c["id"],
            "user": {"login": c["login"] + ("[bot]" if c["login"] == "coderabbitai" else "")},
            "body": c["body"],
            "created_at": c["created_at"],
            "updated_at": c["updated_at"],
            "issue_url": "https://api.github.test/repos/owner/repo/issues/7",
        }
        for c in case.get("comments", [])
        if c["created_at"] <= now
    ]
    gh = configured_gh(
        comments=[summary, *others],
        head=case["head"],
        coderabbit_statuses=[s for s in case["statuses"] if s["created_at"] <= now] if statuses is None else statuses,
        pushed=int(at(case["pushedAt"])),
    )
    gh.edits[summary["node_id"]] = [(v["at"], v["body"]) for v in versions]
    gh.values[f"repos/owner/repo/commits/{case['head']}"] = {"commit": {"committer": {"date": case["pushedAt"]}}}
    return gh, case["head"], now


class CleanReviewVerificationTest(unittest.TestCase):
    """Findings from the verification of PR #96 at ca4110c."""

    # 1: a "Reviews paused" block written with the clean review does not hide it
    def test_recorded_clean_reviews_that_pause_later_reviews_complete_their_head(self) -> None:
        for number in ("1265", "1262", "1212"):
            with self.subTest(pr=number):
                gh, head, now = recorded_clean(number)
                self.assertIn("review paused by coderabbit", gh.pages["repos/owner/repo/issues/7/comments?per_page=100"][0][0]["body"])
                item = clean_observe(gh, head, now)
                self.assertEqual(item["state"], "clean-complete")
                self.assertEqual(item["cleanReview"]["head"], head)

    # 2: an "Already reviewed" refusal, with or without a skip block
    def test_recorded_already_reviewed_refusal_does_not_escalate(self) -> None:
        for now, skip_block in (("2026-08-15T13:43:44Z", False), ("2026-08-15T13:44:20Z", True)):
            with self.subTest(skip_block=skip_block):
                gh, head, _ = recorded_clean("1155", now)
                body = gh.pages["repos/owner/repo/issues/7/comments?per_page=100"][0][0]["body"]
                self.assertEqual("skip review by coderabbit" in body, skip_block)
                item = clean_observe(gh, head, now)
                self.assertTrue(item["alreadyReviewed"])
                self.assertEqual(item["state"], "clean-complete")

    # 3: statuses after a completed review
    def test_a_later_skipped_status_does_not_hide_a_completed_review(self) -> None:
        gh, head, now = recorded_clean("1154")
        self.assertEqual(
            gh.pages[f"repos/owner/repo/commits/{head}/statuses?per_page=100"][0][0]["description"],
            "Review skipped: reviews are disabled for this base branch",
        )
        item = clean_observe(gh, head, now)
        self.assertEqual(item["codeRabbitCheck"]["state"], "SUCCESS")
        self.assertEqual(item["state"], "clean-complete")

    def test_a_status_newer_than_the_completed_review_decides(self) -> None:
        completed = {"context": "CodeRabbit", "state": "success", "description": "Review completed", "created_at": "2026-09-29T13:39:44Z"}
        skipped = {"context": "CodeRabbit", "state": "success", "description": "Review skipped: draft pull request", "created_at": "2026-09-29T13:41:00Z"}
        cases = {
            "re-review in progress": ([{"context": "CodeRabbit", "state": "pending", "description": "Review in progress", "created_at": "2026-09-29T13:42:00Z"}, completed], "PENDING", "trigger-incremental"),
            "re-review rate limited": ([{"context": "CodeRabbit", "state": "success", "description": "Review rate limited", "created_at": "2026-09-29T13:42:00Z"}, completed], "RATE_LIMITED", "pending-retry-source"),
            "skipped, then in progress": ([{"context": "CodeRabbit", "state": "pending", "description": "Review in progress", "created_at": "2026-09-29T13:42:00Z"}, skipped, completed], "PENDING", "trigger-incremental"),
            "skipped only": ([skipped], "INCOMPLETE", "trigger-incremental"),
            "skipped after completed": ([skipped, completed], "SUCCESS", "clean-complete"),
        }
        for name, (statuses, check, state) in cases.items():
            with self.subTest(statuses=name):
                item = clean_observe(clean_gh(statuses=statuses))
                self.assertEqual((item["codeRabbitCheck"]["state"], item["state"]), (check, state))

    # 4: binding edges the mutation run left unpinned
    def test_a_range_starting_at_the_head_never_counts(self) -> None:
        previous = CLEAN["previousHead"]
        item = clean_observe(
            clean_gh(head=previous, statuses=[CLEAN["statuses"][0]], pushed=PUSHED_PREVIOUS), head=previous
        )
        self.assertIsNone(item["cleanReview"])
        self.assertNotEqual(item["state"], "clean-complete")

    def test_only_the_recent_review_blocks_range_binds(self) -> None:
        body = "\n".join([
            "<!-- This is an auto-generated comment: summarize by coderabbit.ai -->",
            "<!-- recent_review_start -->",
            "No actionable comments were generated in the recent review. 🎉",
            f"Reviewing files that changed from the base of the PR and between 72cb20505832bd2e0f4db0ef8d46d2a4ca85140c and {CLEAN['previousHead']}.",
            "<!-- recent_review_end -->",
            "<!-- walkthrough_start -->",
            f"Changes between {CLEAN['previousHead']} and {HEAD_1292}.",
            "<!-- walkthrough_end -->",
        ])
        self.assertIsNone(ADAPTER.clean_review_in(body)["head"] == HEAD_1292 or None)
        self.assertIsNone(clean_observe(clean_gh(body=body))["cleanReview"])

    def test_only_coderabbits_reviews_date_a_run(self) -> None:
        gh = clean_gh()
        gh.pages["repos/owner/repo/pulls/7/reviews?per_page=100"] = [[{
            "id": 2,
            "user": {"login": "maintainer"},
            "body": "**Run ID**: `38da5e68-0cea-4549-91ec-a86ebd24bb85`",
            "state": "COMMENTED",
            "commit_id": HEAD_1292,
            "submitted_at": "2026-09-29T13:00:00Z",
        }]]
        item = clean_observe(gh)
        self.assertEqual((item["state"], item["cleanReview"]["dating"]), ("clean-complete", "edit-history"))

    def test_a_clean_block_carried_across_a_processing_interlude_keeps_its_first_time(self) -> None:
        # #1292: 52803a95's clean block (run 22815d9c) first showed at 09:42:30,
        # disappeared under the 13:15:49 in-progress block, and came back at
        # 13:28:30. It is still the 09:42:30 review, so a push of that head at
        # 13:20 is not covered by it.
        previous = CLEAN["previousHead"]
        pushed = int(at("2026-09-29T13:20:00Z"))
        item = clean_observe(
            clean_gh("2026-09-29T13:31:34Z", head=previous, statuses=[CLEAN["statuses"][0]], pushed=pushed),
            head=previous,
        )
        self.assertIsNone(item["cleanReview"])
        item = clean_observe(
            clean_gh("2026-09-29T13:31:34Z", head=previous, statuses=[CLEAN["statuses"][0]], pushed=PUSHED_PREVIOUS),
            head=previous,
        )
        self.assertEqual(item["cleanReview"]["reviewedAt"], "2026-09-29T09:42:30Z")

    def test_the_clean_verdict_must_be_the_exact_sentence(self) -> None:
        body = CLEAN["versions"][-1]["body"].replace(
            "No actionable comments were generated in the recent review. 🎉",
            "Actionable comments posted: 2\nNo actionable comments were found in unchanged files.",
        )
        self.assertIsNone(ADAPTER.clean_review_in(body))

    def test_a_skipped_check_run_does_not_hide_an_earlier_completed_one(self) -> None:
        gh = clean_gh(statuses=[])
        gh.pages[f"repos/owner/repo/commits/{HEAD_1292}/check-runs?per_page=100"] = [{
            "check_runs": [
                {"name": "CodeRabbit", "head_sha": HEAD_1292, "status": "completed", "conclusion": "success", "output": {"title": "Review skipped"}},
                {"name": "CodeRabbit", "head_sha": HEAD_1292, "status": "completed", "conclusion": "success", "output": {"title": "Review completed"}},
            ]
        }]
        item = clean_observe(gh)
        self.assertEqual((item["codeRabbitCheck"]["state"], item["state"]), ("SUCCESS", "clean-complete"))

    def test_a_pending_review_dates_nothing(self) -> None:
        gh = clean_gh()
        gh.pages["repos/owner/repo/pulls/7/reviews?per_page=100"] = [[{
            "id": 3,
            "user": {"login": "coderabbitai[bot]"},
            "body": "**Run ID**: `38da5e68-0cea-4549-91ec-a86ebd24bb85`",
            "state": "PENDING",
            "commit_id": HEAD_1292,
            "submitted_at": "2026-09-29T13:00:00Z",
        }]]
        item = clean_observe(gh)
        self.assertEqual((item["state"], item["cleanReview"]["dating"]), ("clean-complete", "edit-history"))

    def test_a_clean_review_at_the_push_second_counts(self) -> None:
        item = clean_observe(clean_gh(pushed=int(at("2026-09-29T13:39:41Z"))))
        self.assertEqual(item["state"], "clean-complete")


class CleanReviewSecondVerificationTest(unittest.TestCase):
    """Findings from the re-verification of PR #96 at fad1ce0, and findings-only reviews."""

    def legacy_gh(self, remaining: str = "4/5") -> tuple[FakeGh, float]:
        history = RECORDED["legacyFooterHistory"]
        versions = [
            (v["at"], v["body"].replace("4/5 reviews remaining, refill in 12 minutes.", f"{remaining} reviews remaining, refill in 12 minutes."))
            for v in history["versions"]
        ]
        now = at("2026-05-02T13:43:14Z")
        gh = FakeGh()
        item = {
            "id": history["commentId"],
            "node_id": f"IC_{history['commentId']}",
            "user": {"login": "coderabbitai[bot]"},
            "body": versions[-1][1],
            "created_at": history["createdAt"],
            "updated_at": versions[-1][0],
            "issue_url": "https://api.github.test/repos/owner/repo/issues/490",
        }
        gh.pages[ADAPTER.recent_comments_endpoint("owner/repo", ADAPTER.scan_since(now))] = [[item]]
        gh.edits[item["node_id"]] = versions
        return gh, now

    # CR-A: a statement without a Run ID keeps the contiguous walk
    def test_a_runless_footer_repeated_across_reviews_is_dated_by_its_latest_run(self) -> None:
        gh, now = self.legacy_gh()
        budget = ADAPTER.account_allowance(gh, ["owner/repo"], now)
        self.assertEqual((budget["statementAt"], budget["remaining"]), ("2026-05-02T13:41:14Z", 4))
        self.assertEqual(budget["statementDating"], "edit-history")

    def test_a_runless_zero_footer_still_holds_until_its_refill(self) -> None:
        gh, now = self.legacy_gh("0/5")
        budget = ADAPTER.account_allowance(gh, ["owner/repo"], now)
        self.assertTrue(budget["exhausted"])
        self.assertEqual(budget["retryAt"], "2026-05-02T13:54:14Z")

    # CR-B: a rate limit wins over a decline in one status too
    def test_a_status_that_is_both_paused_and_rate_limited_is_rate_limited(self) -> None:
        statuses = [
            {"context": "CodeRabbit", "state": "success", "description": "Review paused: rate limited", "created_at": "2026-09-29T13:41:00Z"},
            CLEAN["statuses"][0],
        ]
        item = clean_observe(clean_gh(statuses=statuses))
        self.assertEqual((item["codeRabbitCheck"]["state"], item["state"]), ("RATE_LIMITED", "pending-retry-source"))

    # CR-C: each guard on its own
    def test_a_summary_showing_a_review_in_progress_never_counts(self) -> None:
        gh, head, now = recorded_clean("1155", "2026-08-15T13:44:55Z", statuses=[
            {"context": "CodeRabbit", "state": "success", "description": "Review completed", "created_at": "2026-08-15T05:07:24Z"}
        ])
        body = gh.pages["repos/owner/repo/issues/7/comments?per_page=100"][0][0]["body"]
        self.assertIn("review in progress by coderabbit", body)
        self.assertIn("No actionable comments were generated", body)
        item = clean_observe(gh, head, now)
        self.assertIsNone(item["cleanReview"])
        self.assertNotEqual(item["state"], "clean-complete")

    def test_only_coderabbit_statuses_decide(self) -> None:
        statuses = [
            {"context": "Vercel", "state": "success", "description": "Canceled by Ignored Build Step", "created_at": "2026-09-29T13:44:00Z"},
            {"context": "CodeRabbit", "state": "pending", "description": "Review in progress", "created_at": "2026-09-29T13:35:00Z"},
        ]
        item = clean_observe(clean_gh(statuses=statuses))
        self.assertEqual((item["codeRabbitCheck"]["state"], item["state"]), ("PENDING", "trigger-incremental"))

    def failing_history(self, _query: str, _variables: dict[str, Any]) -> dict[str, Any]:
        raise ADAPTER.WaitError("gh: Something went wrong (HTTP 422)")

    def run_review_on_previous_head(self, identifier: int, submitted: str) -> dict[str, Any]:
        return {
            "id": identifier,
            "user": {"login": "coderabbitai[bot]"},
            "body": "**Run ID**: `38da5e68-0cea-4549-91ec-a86ebd24bb85`",
            "state": "COMMENTED",
            "commit_id": CLEAN["previousHead"],
            "submitted_at": submitted,
        }

    def test_the_in_progress_block_marker_alone_unsettles_a_summary(self) -> None:
        body = CLEAN["versions"][-1]["body"].replace(
            "<!-- recent_review_start -->",
            "<!-- This is an auto-generated comment: review in progress by coderabbit.ai -->\n> Reviewing.\n"
            "<!-- end of auto-generated comment: review in progress by coderabbit.ai -->\n<!-- recent_review_start -->",
        )
        self.assertNotIn("Currently processing", body)
        self.assertIsNone(ADAPTER.clean_review_in(body))

    def test_a_run_is_dated_by_its_earliest_review_object(self) -> None:
        gh = clean_gh()
        gh.graphql = self.failing_history  # type: ignore[method-assign]
        gh.pages["repos/owner/repo/pulls/7/reviews?per_page=100"] = [[
            self.run_review_on_previous_head(4, "2026-09-29T13:39:40Z"),
            self.run_review_on_previous_head(5, "2026-09-29T13:10:00Z"),
        ]]
        # The earlier object predates the 13:24 push, so the block does not count.
        self.assertIsNone(clean_observe(gh)["cleanReview"])

    def test_a_run_dated_by_a_later_review_keeps_the_comments_edit_time(self) -> None:
        gh = clean_gh()
        gh.pages["repos/owner/repo/pulls/7/reviews?per_page=100"] = [[
            self.run_review_on_previous_head(6, "2026-09-29T13:39:50Z")
        ]]
        self.assertEqual(clean_observe(gh)["cleanReview"]["reviewedAt"], "2026-09-29T13:39:41Z")

    def test_an_unedited_summary_needs_no_edit_history(self) -> None:
        gh = clean_gh()
        gh.graphql = self.failing_history  # type: ignore[method-assign]
        summary = gh.pages["repos/owner/repo/issues/7/comments?per_page=100"][0][0]
        summary["created_at"] = summary["updated_at"]
        item = clean_observe(gh)
        self.assertEqual((item["state"], item["cleanReview"]["dating"]), ("clean-complete", "unedited"))

    # Item 4: reviews whose findings are only outside the diff or nitpicks
    def test_recorded_findings_only_reviews_complete_their_head(self) -> None:
        for recorded in RECORDED["findingsOnlyReviews"]["reviews"]:
            with self.subTest(pr=recorded["pr"]):
                self.assertNotRegex(recorded["body"], ADAPTER.ACTIONABLE)
                gh = configured_gh(
                    head=recorded["commitId"],
                    reviews=[{
                        "id": recorded["id"], "user": {"login": "coderabbitai[bot]"}, "body": recorded["body"],
                        "state": recorded["state"], "commit_id": recorded["commitId"], "submitted_at": recorded["submittedAt"],
                    }],
                )
                evidence = ADAPTER.pull_evidence(gh, "owner/repo", 7)
                state, _, mode = ADAPTER.classify(evidence, recorded["commitId"], None, START + 180)
                self.assertEqual((state, mode), ("review-complete", None))
                self.assertIsNone(evidence["exactHeadReview"]["actionable"])

    def test_a_review_object_that_is_a_notice_or_empty_never_completes(self) -> None:
        for body in (
            "",
            "<!-- This is an auto-generated comment: rate limited by coderabbit.ai -->\n> ## Review limit reached",
            "> ## Review skipped\n> Draft PR not reviewed.",
            "> ## Reviews paused",
            "Review rate limited.",
        ):
            with self.subTest(body=body[:30]):
                gh = configured_gh(reviews=[review(40, body)])
                state, _, _ = classify(gh)
                self.assertNotEqual(state, "review-complete")

    def test_a_findings_review_of_another_head_or_pending_never_completes(self) -> None:
        body = RECORDED["findingsOnlyReviews"]["reviews"][0]["body"]
        for name, item in {"other head": review(41, body, head="other-head"), "pending": review(42, body, state="PENDING")}.items():
            with self.subTest(review=name):
                self.assertNotEqual(classify(configured_gh(reviews=[item]))[0], "review-complete")


class ReviewEvidenceAllowListTest(unittest.TestCase):
    """Final verification of PR #96: only review evidence completes a head."""

    def complete(self, body: str) -> bool:
        return classify(configured_gh(reviews=[review(50, body)]))[0] == "review-complete"

    def test_unknown_or_unlisted_notice_bodies_never_complete(self) -> None:
        for body in (
            "> [!CAUTION]\n> ## Review failed\n> The pull request is closed.",
            "Review stopped after lock loss.",
            "<summary>⚠️ Action not completed</summary>\n\nAlready reviewed.",
            "Oops, something went wrong! Please try again later.",
            "CodeRabbit couldn't update its existing comment.",
            "Actionable comments posted: 0",
            "Something CodeRabbit has not said before.",
        ):
            with self.subTest(body=body[:40]):
                self.assertFalse(self.complete(body))

    def test_review_evidence_completes(self) -> None:
        for body in (
            "**Actionable comments posted: 1**",
            "<details>\n<summary>⚠️ Outside diff range comments (1)</summary>",
            "<details>\n<summary>🧹 Nitpick comments (3)</summary>",
            "<details>\n<summary>♻️ Duplicate comments (2)</summary>",
        ):
            with self.subTest(body=body[:40]):
                self.assertTrue(self.complete(body))
        for recorded in RECORDED["findingsOnlyReviews"]["reviews"]:
            with self.subTest(pr=recorded["pr"]):
                self.assertTrue(self.complete(recorded["body"]))

    def test_the_notice_guard_holds_even_beside_review_evidence(self) -> None:
        evidence = "<details>\n<summary>🧹 Nitpick comments (1)</summary>"
        for name, notice in {
            "marker only, after a leading comment": "<!-- internal-state -->\n<!-- This is an auto-generated comment: rate limited by coderabbit.ai -->",
            "lowercase heading": "> ## review skipped",
            "heading after a preamble": "Some preamble.\n> ## Reviews paused",
        }.items():
            with self.subTest(notice=name):
                self.assertFalse(self.complete(f"{notice}\n{evidence}"))


if __name__ == "__main__":
    unittest.main()
