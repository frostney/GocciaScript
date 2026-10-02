#!/usr/bin/env python3
"""Black-box tests for deterministic Milestone Rush event ledgers."""

from __future__ import annotations

import json
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path
from typing import Any


ROOT = Path(__file__).resolve().parents[2]
COMMAND = ROOT / "milestone-rush" / "scripts" / "event_ledger.py"
USAGE_FIELDS = (
    "inferences",
    "inputTokens",
    "cachedInputTokens",
    "uncachedInputTokens",
    "outputTokens",
    "reasoningTokens",
    "compactions",
)
RESOURCE_FIELDS = (
    "runnerMilliseconds",
    "agentMilliseconds",
    "toolCalls",
    "commandMilliseconds",
    "effectiveWorkers",
)


def metrics(fields: tuple[str, ...], **values: int | float | None) -> dict[str, Any]:
    result = {field: values.get(field) for field in fields}
    result["unavailableFields"] = [
        field for field in fields if result[field] is None
    ]
    return result


def event(
    event_id: str,
    *,
    event_type: str = "model_sample",
    run_id: str = "run-1",
    sequence: int | None = 0,
    mode: str = "delta",
    baseline: str | None = None,
    source: str = "codex",
    stream_id: str = "model-1",
    usage: dict[str, int | float | None] | None = None,
    resources: dict[str, int | float | None] | None = None,
    identity: dict[str, Any] | None = None,
    schema_version: int = 2,
    span_id: str | None = None,
    span_kind: str | None = None,
) -> dict[str, Any]:
    identity_values = {
        "laneId": "lane-1",
        "decisionId": None,
        "issue": 48,
        "pullRequest": None,
        "branch": "codex/issue-48",
        "head": "abc123",
        "sessionId": "session-1",
        "segmentId": "segment-1",
        "toolName": None,
        "waitId": None,
        "model": "gpt-test" if event_type == "model_sample" else None,
    }
    identity_values.update(identity or {})
    value = {
        "schemaVersion": schema_version,
        "runId": run_id,
        "eventId": event_id,
        "type": event_type,
        "timestamp": f"2026-08-21T12:00:{int(event_id.split('-')[-1]) % 60:02d}Z",
        "spanId": span_id,
        "spanKind": span_kind,
        "parentSpanId": None,
        "blockingSpanIds": [],
        "identity": identity_values,
        "actor": {"kind": "tool", "capabilityClass": None},
        "context": {"mode": "isolated", "exceptionDecisionId": None},
        "result": "started" if event_type == "span_started" else "succeeded",
        "blocker": None,
        "retryAt": None,
        "usage": metrics(USAGE_FIELDS, **(usage or {})),
        "resources": metrics(RESOURCE_FIELDS, **(resources or {})),
    }
    if schema_version == 2:
        value["measurement"] = (
            None
            if sequence is None
            else {
                "source": source,
                "streamId": stream_id,
                "sequence": sequence,
                "mode": mode,
                "baseline": baseline,
            }
        )
    return value


class EventLedgerTest(unittest.TestCase):
    def setUp(self) -> None:
        self.temp = tempfile.TemporaryDirectory()
        self.directory = Path(self.temp.name)
        self.ledger = self.directory / "events.jsonl"

    def tearDown(self) -> None:
        self.temp.cleanup()

    def write_input(self, name: str, *events: dict[str, Any]) -> Path:
        path = self.directory / name
        path.write_text("".join(json.dumps(item) + "\n" for item in events))
        return path

    def command(self, *args: str) -> tuple[subprocess.CompletedProcess[str], dict[str, Any]]:
        result = subprocess.run(
            [sys.executable, str(COMMAND), *args, "--json"],
            text=True,
            capture_output=True,
            timeout=10,
        )
        self.assertTrue(result.stdout, result.stderr)
        return result, json.loads(result.stdout)

    def ingest(self, *events: dict[str, Any]) -> tuple[subprocess.CompletedProcess[str], dict[str, Any]]:
        path = self.write_input("input.jsonl", *events)
        return self.command(
            "ingest", "--ledger", str(self.ledger), "--input", str(path)
        )

    def summarize(self) -> tuple[subprocess.CompletedProcess[str], dict[str, Any]]:
        return self.command(
            "summarize", "--ledger", str(self.ledger), "--run-id", "run-1"
        )

    def test_ingest_is_idempotent_and_rejects_conflicting_duplicates(self) -> None:
        sample = event("event-1", usage={"inputTokens": 3})
        first, first_output = self.ingest(sample)
        second, second_output = self.ingest(sample)
        self.assertEqual(first.returncode, 0)
        self.assertEqual(first_output["appended"], 1)
        self.assertEqual(second.returncode, 0)
        self.assertEqual(second_output["duplicates"], 1)

        conflicting = event("event-1", usage={"inputTokens": 4})
        result, output = self.ingest(conflicting)
        self.assertEqual(result.returncode, 2)
        self.assertIn("conflicts", output["reason"])
        self.assertEqual(len(self.ledger.read_text().splitlines()), 1)

        other_run = event("event-1", run_id="run-2", usage={"inputTokens": 4})
        result, output = self.ingest(other_run)
        self.assertEqual(result.returncode, 0, output)
        self.assertEqual(len(self.ledger.read_text().splitlines()), 2)

    def test_delta_counters_sum_and_zero_remains_measured(self) -> None:
        self.ingest(
            event("event-1", sequence=0, usage={"inputTokens": 0, "outputTokens": 2}),
            event("event-2", sequence=1, usage={"inputTokens": 5, "outputTokens": 3}),
        )
        result, output = self.summarize()
        self.assertEqual(result.returncode, 0)
        self.assertEqual(output["counters"]["usage"]["inputTokens"], 5)
        self.assertEqual(output["counters"]["usage"]["outputTokens"], 5)
        self.assertEqual(output["models"]["gpt-test"]["inputTokens"], 5)

    def test_snapshot_zero_and_start_baselines_aggregate_differences(self) -> None:
        self.ingest(
            event(
                "event-1", sequence=0, mode="snapshot", baseline="zero",
                usage={"inputTokens": 10}, stream_id="zero-stream",
            ),
            event(
                "event-2", sequence=1, mode="snapshot", baseline="zero",
                usage={"inputTokens": 14}, stream_id="zero-stream",
            ),
            event(
                "event-3", sequence=0, mode="snapshot", baseline="start",
                usage={"outputTokens": 50}, stream_id="start-stream",
            ),
            event(
                "event-4", sequence=1, mode="snapshot", baseline="start",
                usage={"outputTokens": 57}, stream_id="start-stream",
            ),
        )
        _, output = self.summarize()
        self.assertEqual(output["counters"]["usage"]["inputTokens"], 14)
        self.assertEqual(output["counters"]["usage"]["outputTokens"], 7)

    def test_snapshot_rejects_missing_baselines_resets_and_mixed_modes(self) -> None:
        cases = [
            (
                [event("event-1", sequence=1, mode="snapshot", baseline="zero", usage={"inputTokens": 2})],
                "missing baseline",
            ),
            (
                [
                    event("event-1", sequence=0, mode="snapshot", baseline="zero", usage={"inputTokens": 3}),
                    event("event-2", sequence=1, mode="snapshot", baseline="zero", usage={"inputTokens": 2}),
                ],
                "decreased",
            ),
            (
                [
                    event("event-1", sequence=0, mode="delta", usage={"inputTokens": 1}),
                    event("event-2", sequence=1, mode="snapshot", baseline="zero", usage={"inputTokens": 2}),
                ],
                "mixes",
            ),
        ]
        for index, (events, message) in enumerate(cases):
            with self.subTest(message=message):
                self.ledger.unlink(missing_ok=True)
                path = self.write_input(f"invalid-{index}.jsonl", *events)
                result, output = self.command(
                    "ingest", "--ledger", str(self.ledger), "--input", str(path)
                )
                self.assertEqual(result.returncode, 2)
                self.assertIn(message, output["reason"])

    def test_silent_nulls_fail_and_explicit_unavailable_fields_are_reported(self) -> None:
        sample = event("event-1", usage={"inputTokens": 0})
        sample["usage"]["unavailableFields"].append("inputTokens")
        result, output = self.ingest(sample)
        self.assertEqual(result.returncode, 2)
        self.assertIn("measured and unavailable", output["reason"])

        self.ledger.unlink(missing_ok=True)
        valid = event("event-2", usage={"inputTokens": 0})
        result, _ = self.ingest(valid)
        self.assertEqual(result.returncode, 0)
        _, summary = self.summarize()
        unavailable = summary["coverage"]["unavailable"]
        self.assertTrue(any(item["eventId"] == "event-2" for item in unavailable))
        self.assertEqual(summary["counters"]["usage"]["inputTokens"], 0)

        self.ledger.unlink(missing_ok=True)
        invalid_number = event("event-3", usage={"inputTokens": float("nan")})
        result, output = self.ingest(invalid_number)
        self.assertEqual(result.returncode, 2)
        self.assertIn("non-negative number", output["reason"])

    def test_identity_values_must_be_strings_or_issue_numbers(self) -> None:
        for field, value in (
            ("sessionId", ["session-1"]),
            ("sessionId", 7),
            ("branch", ""),
            ("issue", True),
            ("pullRequest", 0),
        ):
            with self.subTest(field=field, value=value):
                result, output = self.ingest(
                    event("event-1", usage={"inferences": 1}, identity={field: value})
                )
                self.assertEqual(result.returncode, 2)
                self.assertEqual(output["state"], "invalid")
                self.assertIn(f"identity.{field}", output["reason"])

        for identity in (
            {"issue": 48, "pullRequest": 52},
            {"issue": "48", "pullRequest": "52"},
            {field: None for field in ("laneId", "issue", "branch", "sessionId", "model")},
        ):
            with self.subTest(identity=identity):
                self.ledger.unlink(missing_ok=True)
                result, _ = self.ingest(
                    event(
                        "event-1", event_type="lane_sample",
                        usage={"inferences": 1}, identity=identity,
                    )
                )
                self.assertEqual(result.returncode, 0)

    def test_summarize_reports_a_stored_invalid_identity_as_invalid(self) -> None:
        stored = event(
            "event-1", usage={"inferences": 1}, identity={"sessionId": ["session-1"]}
        )
        self.ledger.write_text(json.dumps(stored) + "\n")
        result, output = self.summarize()
        self.assertEqual(result.returncode, 2)
        self.assertEqual(output["state"], "invalid")
        self.assertIn("identity.sessionId", output["reason"])

    def test_spans_require_one_matching_start_and_finish(self) -> None:
        start = event(
            "event-1", event_type="span_started", sequence=None,
            span_id="span-1", span_kind="command",
        )
        result, _ = self.ingest(start)
        self.assertEqual(result.returncode, 0)
        result, output = self.command(
            "validate", "--ledger", str(self.ledger), "--run-id", "run-1"
        )
        self.assertEqual(result.returncode, 2)
        self.assertIn("unclosed", output["reason"])

        early_finish = event(
            "event-0", event_type="span_finished", sequence=None,
            span_id="span-1", span_kind="command",
        )
        result, output = self.ingest(early_finish)
        self.assertEqual(result.returncode, 2)
        self.assertIn("before it starts", output["reason"])

        finish = event(
            "event-2", event_type="span_finished", sequence=None,
            span_id="span-1", span_kind="command",
        )
        result, output = self.ingest(finish)
        self.assertEqual(result.returncode, 0, output)
        result, output = self.command(
            "validate", "--ledger", str(self.ledger), "--run-id", "run-1"
        )
        self.assertEqual(result.returncode, 0)
        self.assertEqual(output["spanCount"], 1)

    def superseded_span(
        self,
        *,
        finish_result: str | None = None,
        finish_blocker: Any = None,
        superseded_blocker: Any = None,
    ) -> tuple[subprocess.CompletedProcess[str], dict[str, Any]]:
        events = [
            event(
                "event-1", event_type="span_started", sequence=None,
                span_id="span-1", span_kind="ci",
            ),
            event(
                "event-2", event_type="work_superseded", sequence=None,
                span_id="span-1", span_kind="ci",
            ),
        ]
        events[1]["blocker"] = superseded_blocker
        if finish_result is not None:
            finish = event(
                "event-3", event_type="span_finished", sequence=None,
                span_id="span-1", span_kind="ci",
            )
            finish["result"] = finish_result
            finish["blocker"] = finish_blocker
            events.append(finish)
        result, output = self.ingest(*events)
        self.assertEqual(result.returncode, 0, output)
        return self.command(
            "validate", "--ledger", str(self.ledger), "--run-id", "run-1"
        )

    def test_superseded_work_needs_a_cancellation_or_blocker_at_closure(self) -> None:
        result, output = self.superseded_span(finish_result="succeeded")
        self.assertEqual(result.returncode, 2)
        self.assertIn(
            "superseded work without a terminal cancellation or blocker: span-1",
            output["reason"],
        )

    def test_superseded_work_that_was_cancelled_is_valid_closure_evidence(self) -> None:
        result, output = self.superseded_span(finish_result="cancelled")
        self.assertEqual(result.returncode, 0, output)
        self.assertTrue(output["valid"])

    def test_superseded_work_with_a_blocker_is_valid_closure_evidence(self) -> None:
        result, output = self.superseded_span(
            finish_result="succeeded",
            superseded_blocker="the CI service cannot cancel a queued run",
        )
        self.assertEqual(result.returncode, 0, output)
        self.assertTrue(output["valid"])

    def test_a_blocker_on_the_finish_accounts_for_superseded_work(self) -> None:
        result, output = self.superseded_span(
            finish_result="blocked", finish_blocker="cancellation was refused"
        )
        self.assertEqual(result.returncode, 0, output)
        self.assertTrue(output["valid"])

    def test_a_blocker_that_states_nothing_does_not_account_for_superseded_work(self) -> None:
        for index, blocker in enumerate(("", "  ", True, 1, {"reason": "none"})):
            with self.subTest(blocker=blocker):
                self.ledger = self.directory / f"events-{index}.jsonl"
                result, output = self.superseded_span(
                    finish_result="failed", superseded_blocker=blocker
                )
                self.assertEqual(result.returncode, 2)
                self.assertIn("superseded work without", output["reason"])

    def test_a_later_superseded_event_can_supply_the_blocker(self) -> None:
        result, output = self.superseded_span(finish_result="succeeded")
        self.assertEqual(result.returncode, 2)
        for event_id, blocker in (("event-4", "the run had already finished"), ("event-5", None)):
            repair = event(
                event_id, event_type="work_superseded", sequence=None,
                span_id="span-1", span_kind="ci",
            )
            repair["blocker"] = blocker
            result, output = self.ingest(repair)
            self.assertEqual(result.returncode, 0, output)
        result, output = self.command(
            "validate", "--ledger", str(self.ledger), "--run-id", "run-1"
        )
        self.assertEqual(result.returncode, 0, output)

    def test_superseded_work_must_name_a_span_of_the_run(self) -> None:
        superseded = event(
            "event-1", event_type="work_superseded", sequence=None,
            span_id="span-typo", span_kind="ci",
        )
        superseded["blocker"] = "the CI service cannot cancel a queued run"
        result, output = self.ingest(superseded)
        self.assertEqual(result.returncode, 0, output)
        result, output = self.command(
            "validate", "--ledger", str(self.ledger), "--run-id", "run-1"
        )
        self.assertEqual(result.returncode, 2)
        self.assertIn(
            "superseded work names spans that never started: span-typo",
            output["reason"],
        )

    def test_ingest_accepts_superseded_work_that_is_still_in_flight(self) -> None:
        # Ingest validates without closure; the helper asserts it accepted the
        # unfinished superseded span. A blocker does not close the span.
        result, output = self.superseded_span(
            superseded_blocker="the CI service cannot cancel a queued run"
        )
        self.assertEqual(result.returncode, 2)
        self.assertIn("unclosed spans: span-1", output["reason"])
        self.assertEqual(len(self.ledger.read_text().splitlines()), 2)

    def test_superseded_work_must_name_its_span(self) -> None:
        result, output = self.ingest(
            event("event-1", event_type="work_superseded", sequence=None)
        )
        self.assertEqual(result.returncode, 2)
        self.assertIn("work_superseded requires spanId", output["reason"])
        self.assertFalse(self.ledger.exists() and self.ledger.read_text())

    def test_sessions_segments_and_manual_resumes_are_deterministic(self) -> None:
        started = event(
            "event-1", event_type="session_started", sequence=None,
            identity={"sessionId": "session-b", "segmentId": "segment-b"},
        )
        resumed = event(
            "event-2", event_type="manual_resume", sequence=None,
            identity={"sessionId": "session-a", "segmentId": "segment-a"},
        )
        self.ingest(started, resumed)
        _, first = self.summarize()
        _, second = self.summarize()
        self.assertEqual(first, second)
        self.assertEqual(first["sessions"], ["session-a", "session-b"])
        self.assertEqual(first["segments"], ["segment-a", "segment-b"])
        self.assertEqual(first["manualResumes"], 1)

    def test_schema_v1_identity_types_stay_readable(self) -> None:
        legacy = event(
            "event-1", schema_version=1, sequence=None,
            identity={"issue": 0, "pullRequest": [52]},
        )
        self.ledger.write_text(json.dumps(legacy) + "\n")
        result, output = self.summarize()
        self.assertEqual(result.returncode, 0, output)
        self.assertEqual(output["legacyEventCount"], 1)

    def test_schema_v1_lifecycle_is_readable_but_counters_are_not_aggregated(self) -> None:
        legacy = event(
            "event-1", schema_version=1, sequence=None,
            usage={"inputTokens": 99},
        )
        result, output = self.ingest(legacy)
        self.assertEqual(result.returncode, 2)
        self.assertIn("schema-v2", output["reason"])
        self.ledger.write_text(json.dumps(legacy) + "\n")
        _, output = self.summarize()
        self.assertEqual(output["legacyEventCount"], 1)
        self.assertEqual(output["counters"]["usage"]["inputTokens"], 0)
        self.assertEqual(
            output["coverage"]["nonAggregatable"][0]["fields"],
            ["usage.inputTokens"],
        )

    def test_model_tool_wait_and_gauge_summaries_retain_provenance(self) -> None:
        self.ingest(
            event(
                "event-1", event_type="tool_sample", source="claude-hooks",
                stream_id="tool-1", identity={"toolName": "Bash", "model": None, "waitId": "wait-1"},
                resources={"toolCalls": 1, "commandMilliseconds": 20, "effectiveWorkers": 2},
            ),
            event(
                "event-2", event_type="tool_sample", source="claude-hooks",
                stream_id="tool-1", sequence=1,
                identity={"toolName": "Bash", "model": None, "waitId": "wait-1"},
                resources={"toolCalls": 2, "commandMilliseconds": 30, "effectiveWorkers": 4},
            ),
        )
        _, output = self.summarize()
        self.assertEqual(output["tools"]["Bash"]["resources.toolCalls"], 3)
        self.assertEqual(output["waits"]["wait-1"]["resources.commandMilliseconds"], 50)
        self.assertEqual(
            output["gauges"]["resources.effectiveWorkers"],
            {"latest": 4, "max": 4, "min": 2},
        )
        self.assertEqual(output["provenance"][0]["source"], "claude-hooks")

    def test_gauge_latest_follows_time_across_streams(self) -> None:
        self.ingest(
            event("event-5", stream_id="a", resources={"effectiveWorkers": 8}),
            event("event-1", stream_id="b", resources={"effectiveWorkers": 2}),
        )
        _, output = self.summarize()
        self.assertEqual(
            output["gauges"]["resources.effectiveWorkers"],
            {"latest": 8, "max": 8, "min": 2},
        )

    def test_gauge_latest_breaks_timestamp_ties_by_event_id(self) -> None:
        for later_stream, earlier_stream in (("a", "b"), ("b", "a")):
            with self.subTest(later_stream=later_stream):
                self.ledger.unlink(missing_ok=True)
                self.ingest(
                    event("event-65", stream_id=later_stream, resources={"effectiveWorkers": 8}),
                    event("event-5", stream_id=earlier_stream, resources={"effectiveWorkers": 2}),
                )
                _, output = self.summarize()
                self.assertEqual(output["gauges"]["resources.effectiveWorkers"]["latest"], 8)

    def test_gauge_latest_compares_timestamps_across_offsets(self) -> None:
        later = event("event-1", stream_id="a", resources={"effectiveWorkers": 8})
        later["timestamp"] = "2026-08-21T09:00:05-05:00"
        earlier = event("event-2", stream_id="b", resources={"effectiveWorkers": 2})
        earlier["timestamp"] = "2026-08-21T12:00:04Z"
        self.ingest(later, earlier)
        _, output = self.summarize()
        self.assertEqual(output["gauges"]["resources.effectiveWorkers"]["latest"], 8)

    def test_concurrent_ingest_serializes_complete_json_lines(self) -> None:
        first = self.write_input(
            "first.jsonl",
            event("event-1", source="host-a", stream_id="stream-a", usage={"inputTokens": 1}),
        )
        second = self.write_input(
            "second.jsonl",
            event("event-2", source="host-b", stream_id="stream-b", usage={"inputTokens": 2}),
        )
        commands = [
            [sys.executable, str(COMMAND), "ingest", "--ledger", str(self.ledger), "--input", str(path), "--json"]
            for path in (first, second)
        ]
        processes = [
            subprocess.Popen(command, text=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
            for command in commands
        ]
        results = [process.communicate(timeout=10) + (process.returncode,) for process in processes]
        self.assertEqual([result[2] for result in results], [0, 0], results)
        parsed = [json.loads(line) for line in self.ledger.read_text().splitlines()]
        self.assertEqual({item["eventId"] for item in parsed}, {"event-1", "event-2"})


if __name__ == "__main__":
    unittest.main()
