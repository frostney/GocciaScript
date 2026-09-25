#!/usr/bin/env python3
"""Behavioral tests for the executable CodeRabbit adapter."""

from __future__ import annotations

import importlib.util
import unittest
from datetime import datetime, timezone
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
        return self.pages.get(endpoint, [[]])


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
) -> FakeGh:
    gh = FakeGh()
    gh.values.update(
        {
            "repos/owner/repo/pulls/7": {"head": {"sha": head}},
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
        first = "repos/owner/one/issues/comments?sort=updated&direction=desc&per_page=100"
        second = "repos/owner/two/issues/comments?sort=updated&direction=desc&per_page=100"
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
        wait = ADAPTER.account_wait(gh, ["owner/one", "owner/two"])
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
                review(33, "Actionable comments posted: 0")
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

    def test_trusted_latency_ack_with_coverage_is_clean_complete_without_check(
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


if __name__ == "__main__":
    unittest.main()
