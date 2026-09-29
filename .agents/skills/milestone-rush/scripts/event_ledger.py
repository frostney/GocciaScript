#!/usr/bin/env python3
"""Ingest, validate, and summarize normalized Milestone Rush events."""

from __future__ import annotations

import argparse
import json
import math
import os
import sys
from contextlib import contextmanager
from datetime import datetime
from pathlib import Path
from typing import Any, Iterator, TextIO

sys.dont_write_bytecode = True  # Keep installed skill trees free of __pycache__.


USAGE_COUNTERS = (
    "inferences",
    "inputTokens",
    "cachedInputTokens",
    "uncachedInputTokens",
    "outputTokens",
    "reasoningTokens",
    "compactions",
)
RESOURCE_COUNTERS = (
    "runnerMilliseconds",
    "agentMilliseconds",
    "toolCalls",
    "commandMilliseconds",
)
RESOURCE_GAUGES = ("effectiveWorkers",)
IDENTITY_FIELDS_V2 = (
    "laneId",
    "decisionId",
    "issue",
    "pullRequest",
    "branch",
    "head",
    "sessionId",
    "segmentId",
    "toolName",
    "waitId",
    "model",
)
NUMBERED_IDENTITY_FIELDS = ("issue", "pullRequest")
# Schema v1 never constrained identity types; summaries still aggregate these.
AGGREGATED_IDENTITY_FIELDS_V1 = ("sessionId", "segmentId")
ENVELOPE_FIELDS = (
    "schemaVersion",
    "runId",
    "eventId",
    "type",
    "timestamp",
    "spanId",
    "spanKind",
    "parentSpanId",
    "blockingSpanIds",
    "identity",
    "actor",
    "context",
    "result",
    "blocker",
    "retryAt",
    "usage",
    "resources",
)
ACTOR_KINDS = {"coordinator", "worker", "watcher", "tool", "user"}
CONTEXT_MODES = {"isolated", "recent-slice", "full-history", "coordinator"}
RESULTS = {
    "started",
    "succeeded",
    "failed",
    "pending",
    "blocked",
    "cancelled",
    "unavailable",
}


class LedgerError(ValueError):
    """Raised when ledger evidence is malformed or ambiguous."""


def canonical(value: Any) -> str:
    return json.dumps(value, sort_keys=True, separators=(",", ":"), ensure_ascii=False)


def parse_timestamp(value: Any, field: str) -> datetime:
    if not isinstance(value, str) or not value:
        raise LedgerError(f"{field} must be a non-empty RFC 3339 timestamp")
    try:
        parsed = datetime.fromisoformat(value.replace("Z", "+00:00"))
    except ValueError as error:
        raise LedgerError(f"{field} must be an RFC 3339 timestamp") from error
    if parsed.tzinfo is None:
        raise LedgerError(f"{field} must include a timezone")
    return parsed


def require_object(value: Any, field: str) -> dict[str, Any]:
    if not isinstance(value, dict):
        raise LedgerError(f"{field} must be an object")
    return value


def validate_metric_group(
    event_id: str,
    group_name: str,
    group: Any,
    counters: tuple[str, ...],
    gauges: tuple[str, ...] = (),
) -> bool:
    values = require_object(group, f"event {event_id} {group_name}")
    expected = set(counters + gauges + ("unavailableFields",))
    missing = expected - set(values)
    if missing:
        raise LedgerError(
            f"event {event_id} {group_name} is missing fields: {', '.join(sorted(missing))}"
        )
    unknown = set(values) - expected
    if unknown:
        raise LedgerError(
            f"event {event_id} {group_name} has unknown fields: {', '.join(sorted(unknown))}"
        )
    unavailable = values["unavailableFields"]
    if not isinstance(unavailable, list) or any(
        not isinstance(item, str) for item in unavailable
    ):
        raise LedgerError(
            f"event {event_id} {group_name}.unavailableFields must be a string array"
        )
    if len(set(unavailable)) != len(unavailable):
        raise LedgerError(
            f"event {event_id} {group_name}.unavailableFields contains duplicates"
        )
    unknown_unavailable = set(unavailable) - set(counters + gauges)
    if unknown_unavailable:
        raise LedgerError(
            f"event {event_id} {group_name}.unavailableFields names unknown fields: "
            + ", ".join(sorted(unknown_unavailable))
        )
    measured = False
    for field in counters + gauges:
        value = values[field]
        if value is None:
            if field not in unavailable:
                raise LedgerError(
                    f"event {event_id} {group_name}.{field} is silently null"
                )
            continue
        if field in unavailable:
            raise LedgerError(
                f"event {event_id} {group_name}.{field} is measured and unavailable"
            )
        if (
            isinstance(value, bool)
            or not isinstance(value, (int, float))
            or not math.isfinite(value)
            or value < 0
        ):
            raise LedgerError(
                f"event {event_id} {group_name}.{field} must be a non-negative number or null"
            )
        measured = True
    return measured


def validate_event(event: Any) -> dict[str, Any]:
    value = require_object(event, "event")
    missing = set(ENVELOPE_FIELDS) - set(value)
    event_id = str(value.get("eventId") or "<unknown>")
    if missing:
        raise LedgerError(
            f"event {event_id} is missing envelope fields: {', '.join(sorted(missing))}"
        )
    version = value["schemaVersion"]
    if version not in (1, 2):
        raise LedgerError(f"event {event_id} has unsupported schemaVersion {version!r}")
    for field in ("runId", "eventId", "type"):
        if not isinstance(value[field], str) or not value[field]:
            raise LedgerError(f"event {event_id} {field} must be a non-empty string")
    parse_timestamp(value["timestamp"], f"event {event_id} timestamp")
    if value["retryAt"] is not None:
        parse_timestamp(value["retryAt"], f"event {event_id} retryAt")
    if not isinstance(value["blockingSpanIds"], list) or any(
        not isinstance(item, str) or not item for item in value["blockingSpanIds"]
    ):
        raise LedgerError(f"event {event_id} blockingSpanIds must be a string array")
    for field in ("spanId", "spanKind", "parentSpanId"):
        if value[field] is not None and (
            not isinstance(value[field], str) or not value[field]
        ):
            raise LedgerError(f"event {event_id} {field} must be a non-empty string or null")

    identity = require_object(value["identity"], f"event {event_id} identity")
    required_identity = IDENTITY_FIELDS_V2 if version == 2 else IDENTITY_FIELDS_V2[:6]
    missing_identity = set(required_identity) - set(identity)
    if missing_identity:
        raise LedgerError(
            f"event {event_id} identity is missing fields: {', '.join(sorted(missing_identity))}"
        )
    typed_identity = (
        IDENTITY_FIELDS_V2 if version == 2 else AGGREGATED_IDENTITY_FIELDS_V1
    )
    for field in typed_identity:
        item = identity.get(field)
        numbered = field in NUMBERED_IDENTITY_FIELDS
        if (
            item is None
            or (isinstance(item, str) and item)
            or (
                numbered
                and isinstance(item, int)
                and not isinstance(item, bool)
                and item > 0
            )
        ):
            continue
        expected = (
            "a non-empty string, a positive integer or null"
            if numbered
            else "a non-empty string or null"
        )
        raise LedgerError(f"event {event_id} identity.{field} must be {expected}")
    actor = require_object(value["actor"], f"event {event_id} actor")
    if "kind" not in actor or "capabilityClass" not in actor:
        raise LedgerError(f"event {event_id} actor requires kind and capabilityClass")
    if actor["kind"] not in ACTOR_KINDS:
        raise LedgerError(f"event {event_id} actor.kind is invalid")
    context = require_object(value["context"], f"event {event_id} context")
    if "mode" not in context or "exceptionDecisionId" not in context:
        raise LedgerError(
            f"event {event_id} context requires mode and exceptionDecisionId"
        )
    if context["mode"] not in CONTEXT_MODES:
        raise LedgerError(f"event {event_id} context.mode is invalid")
    if value["result"] not in RESULTS:
        raise LedgerError(f"event {event_id} result is invalid")
    if value["type"] == "span_started" and value["result"] != "started":
        raise LedgerError(f"event {event_id} span_started requires result started")
    if value["type"] == "span_finished" and value["result"] in {
        "started",
        "pending",
    }:
        raise LedgerError(f"event {event_id} span_finished requires a terminal result")
    usage_measured = validate_metric_group(
        event_id, "usage", value["usage"], USAGE_COUNTERS
    )
    resources_measured = validate_metric_group(
        event_id,
        "resources",
        value["resources"],
        RESOURCE_COUNTERS,
        RESOURCE_GAUGES,
    )

    if version == 1:
        return value

    if "measurement" not in value:
        raise LedgerError(f"event {event_id} is missing measurement")
    measurement = value["measurement"]
    if measurement is None:
        if usage_measured or resources_measured:
            raise LedgerError(
                f"event {event_id} has measured values without measurement provenance"
            )
    else:
        measurement = require_object(measurement, f"event {event_id} measurement")
        expected = {"source", "streamId", "sequence", "mode", "baseline"}
        if set(measurement) != expected:
            raise LedgerError(
                f"event {event_id} measurement fields must be exactly: "
                + ", ".join(sorted(expected))
            )
        for field in ("source", "streamId"):
            if not isinstance(measurement[field], str) or not measurement[field]:
                raise LedgerError(
                    f"event {event_id} measurement.{field} must be a non-empty string"
                )
        sequence = measurement["sequence"]
        if isinstance(sequence, bool) or not isinstance(sequence, int) or sequence < 0:
            raise LedgerError(
                f"event {event_id} measurement.sequence must be a non-negative integer"
            )
        mode = measurement["mode"]
        baseline = measurement["baseline"]
        if mode == "delta":
            if baseline is not None:
                raise LedgerError(
                    f"event {event_id} delta measurements require a null baseline"
                )
        elif mode == "snapshot":
            if baseline not in ("zero", "start"):
                raise LedgerError(
                    f"event {event_id} snapshot baseline must be zero or start"
                )
        else:
            raise LedgerError(
                f"event {event_id} measurement.mode must be delta or snapshot"
            )
        if not usage_measured and not resources_measured:
            raise LedgerError(f"event {event_id} measurement contains no measured value")

    if value["type"] == "session_started" and not identity.get("sessionId"):
        raise LedgerError(f"event {event_id} session_started requires identity.sessionId")
    if value["type"] == "manual_resume" and (
        not identity.get("sessionId") or not identity.get("segmentId")
    ):
        raise LedgerError(
            f"event {event_id} manual_resume requires sessionId and segmentId"
        )
    if value["type"] == "model_sample" and not identity.get("model"):
        raise LedgerError(f"event {event_id} model_sample requires identity.model")
    if value["type"] == "tool_sample" and not identity.get("toolName"):
        raise LedgerError(f"event {event_id} tool_sample requires identity.toolName")
    return value


def parse_json_lines(source: TextIO, label: str) -> list[dict[str, Any]]:
    events: list[dict[str, Any]] = []
    for line_number, line in enumerate(source, 1):
        if not line.strip():
            continue
        try:
            parsed = json.loads(line)
        except json.JSONDecodeError as error:
            raise LedgerError(f"{label}:{line_number}: invalid JSON: {error.msg}") from error
        try:
            events.append(validate_event(parsed))
        except LedgerError as error:
            raise LedgerError(f"{label}:{line_number}: {error}") from error
    return events


def load_ledger(path: Path) -> list[dict[str, Any]]:
    if not path.exists():
        return []
    with path.open(encoding="utf-8") as source:
        return parse_json_lines(source, str(path))


@contextmanager
def locked_ledger(path: Path) -> Iterator[TextIO]:
    path.parent.mkdir(parents=True, exist_ok=True)
    stream = path.open("a+", encoding="utf-8", newline="\n")
    locked = False
    try:
        if os.name == "nt":
            import msvcrt

            stream.seek(0)
            msvcrt.locking(stream.fileno(), msvcrt.LK_LOCK, 1)
        else:
            import fcntl

            fcntl.flock(stream.fileno(), fcntl.LOCK_EX)
        locked = True
        yield stream
    finally:
        try:
            if locked and os.name == "nt":
                import msvcrt

                stream.seek(0)
                msvcrt.locking(stream.fileno(), msvcrt.LK_UNLCK, 1)
            elif locked:
                import fcntl

                fcntl.flock(stream.fileno(), fcntl.LOCK_UN)
        finally:
            stream.close()


def events_from_locked_stream(stream: TextIO, label: str) -> list[dict[str, Any]]:
    stream.seek(0)
    return parse_json_lines(stream, label)


def metric_paths() -> tuple[str, ...]:
    return tuple(f"usage.{field}" for field in USAGE_COUNTERS) + tuple(
        f"resources.{field}" for field in RESOURCE_COUNTERS
    )


def metric_value(event: dict[str, Any], path: str) -> int | float | None:
    group, field = path.split(".", 1)
    return event[group][field]


def validate_run(
    events: list[dict[str, Any]], run_id: str, *, closure: bool = True
) -> dict[str, Any]:
    selected = [event for event in events if event["runId"] == run_id]
    if not selected:
        raise LedgerError(f"runId {run_id!r} has no events")

    by_id: dict[str, dict[str, Any]] = {}
    for event in selected:
        previous = by_id.get(event["eventId"])
        if previous is not None and canonical(previous) != canonical(event):
            raise LedgerError(f"eventId {event['eventId']!r} has conflicting duplicates")
        by_id[event["eventId"]] = event

    started: dict[str, dict[str, Any]] = {}
    finished: dict[str, dict[str, Any]] = {}
    for event in by_id.values():
        if event["type"] not in ("span_started", "span_finished"):
            continue
        span_id = event["spanId"]
        if not isinstance(span_id, str) or not span_id:
            raise LedgerError(f"event {event['eventId']} {event['type']} requires spanId")
        target = started if event["type"] == "span_started" else finished
        if span_id in target:
            raise LedgerError(f"span {span_id!r} has multiple {event['type']} events")
        target[span_id] = event
    missing_starts = sorted(set(finished) - set(started))
    missing_finishes = sorted(set(started) - set(finished))
    if missing_starts or (closure and missing_finishes):
        details = []
        if closure and missing_finishes:
            details.append("unclosed spans: " + ", ".join(missing_finishes))
        if missing_starts:
            details.append("finishes without starts: " + ", ".join(missing_starts))
        raise LedgerError("; ".join(details))
    for span_id in started:
        if span_id not in finished:
            continue
        if started[span_id]["spanKind"] != finished[span_id]["spanKind"]:
            raise LedgerError(f"span {span_id!r} changes spanKind")
        if canonical(started[span_id]["identity"]) != canonical(
            finished[span_id]["identity"]
        ):
            raise LedgerError(f"span {span_id!r} changes identity")
        if parse_timestamp(
            finished[span_id]["timestamp"], f"span {span_id} finish timestamp"
        ) < parse_timestamp(
            started[span_id]["timestamp"], f"span {span_id} start timestamp"
        ):
            raise LedgerError(f"span {span_id!r} finishes before it starts")

    streams: dict[tuple[str, str], list[dict[str, Any]]] = {}
    for event in by_id.values():
        if event["schemaVersion"] != 2 or event["measurement"] is None:
            continue
        measurement = event["measurement"]
        key = (measurement["source"], measurement["streamId"])
        streams.setdefault(key, []).append(event)

    for (source, stream_id), stream_events in streams.items():
        modes = {event["measurement"]["mode"] for event in stream_events}
        baselines = {event["measurement"]["baseline"] for event in stream_events}
        if len(modes) != 1:
            raise LedgerError(f"stream {source}/{stream_id} mixes measurement modes")
        if len(baselines) != 1:
            raise LedgerError(f"stream {source}/{stream_id} changes its baseline")
        sequence_map: dict[int, dict[str, Any]] = {}
        for event in stream_events:
            sequence = event["measurement"]["sequence"]
            if sequence in sequence_map:
                raise LedgerError(
                    f"stream {source}/{stream_id} repeats sequence {sequence}"
                )
            sequence_map[sequence] = event
        sequences = sorted(sequence_map)
        expected_sequences = list(range(len(sequences)))
        if sequences != expected_sequences:
            raise LedgerError(
                f"stream {source}/{stream_id} has missing baseline or sequence gap: "
                f"observed {sequences}, expected {expected_sequences}"
            )
        if next(iter(modes)) != "snapshot":
            continue
        baseline = next(iter(baselines))
        ordered = [sequence_map[sequence] for sequence in sequences]
        for path in metric_paths():
            first = metric_value(ordered[0], path)
            if baseline == "start" and first is None and any(
                metric_value(event, path) is not None for event in ordered[1:]
            ):
                raise LedgerError(
                    f"stream {source}/{stream_id} {path} lacks a start baseline"
                )
            previous = first
            for event in ordered[1:]:
                current = metric_value(event, path)
                if current is None:
                    continue
                if previous is not None and current < previous:
                    raise LedgerError(
                        f"stream {source}/{stream_id} {path} decreased; start a new stream after reset"
                    )
                previous = current

    return {
        "schemaVersion": 2,
        "runId": run_id,
        "valid": True,
        "eventCount": len(selected),
        "uniqueEventCount": len(by_id),
        "streamCount": len(streams),
        "legacyEventCount": sum(
            event["schemaVersion"] == 1 for event in by_id.values()
        ),
        "spanCount": len(started),
    }


def zero_counters() -> dict[str, int | float]:
    return {path: 0 for path in metric_paths()}


def add_counter(target: dict[str, int | float], path: str, value: int | float) -> None:
    target[path] = target.get(path, 0) + value


def summarize_run(events: list[dict[str, Any]], run_id: str) -> dict[str, Any]:
    validation = validate_run(events, run_id)
    unique: dict[str, dict[str, Any]] = {}
    for event in events:
        if event["runId"] == run_id:
            unique[event["eventId"]] = event
    selected = sorted(
        unique.values(),
        key=lambda event: (
            parse_timestamp(event["timestamp"], f"event {event['eventId']} timestamp"),
            event["eventId"],
        ),
    )

    totals = zero_counters()
    models: dict[str, dict[str, int | float]] = {}
    tools: dict[str, dict[str, int | float]] = {}
    waits: dict[str, dict[str, int | float]] = {}
    gauges: dict[str, dict[str, int | float]] = {}
    unavailable: list[dict[str, Any]] = []
    non_aggregatable: list[dict[str, Any]] = []
    provenance: dict[str, set[str]] = {}
    sessions: set[str] = set()
    segments: set[str] = set()
    manual_resumes = 0

    streams: dict[tuple[str, str], list[dict[str, Any]]] = {}
    for event in selected:
        identity = event["identity"]
        if identity.get("sessionId"):
            sessions.add(identity["sessionId"])
        if identity.get("segmentId"):
            segments.add(identity["segmentId"])
        if event["type"] == "manual_resume":
            manual_resumes += 1
        for group_name in ("usage", "resources"):
            fields = event[group_name]["unavailableFields"]
            if fields:
                unavailable.append(
                    {
                        "eventId": event["eventId"],
                        "fields": [f"{group_name}.{field}" for field in sorted(fields)],
                    }
                )
        if event["schemaVersion"] == 1:
            measured = [
                path for path in metric_paths() if metric_value(event, path) is not None
            ]
            if measured:
                non_aggregatable.append(
                    {
                        "eventId": event["eventId"],
                        "fields": measured,
                        "reason": "schema-v1 counter mode and baseline are ambiguous",
                    }
                )
            continue
        measurement = event["measurement"]
        if measurement is None:
            continue
        gauge = event["resources"]["effectiveWorkers"]
        if gauge is not None:
            summary = gauges.setdefault(
                "resources.effectiveWorkers",
                {"min": gauge, "max": gauge, "latest": gauge},
            )
            summary["min"] = min(summary["min"], gauge)
            summary["max"] = max(summary["max"], gauge)
            summary["latest"] = gauge
        key = (measurement["source"], measurement["streamId"])
        streams.setdefault(key, []).append(event)
        provenance.setdefault(measurement["source"], set()).add(
            measurement["streamId"]
        )

    for _key, stream_events in sorted(streams.items()):
        ordered = sorted(stream_events, key=lambda event: event["measurement"]["sequence"])
        mode = ordered[0]["measurement"]["mode"]
        baseline = ordered[0]["measurement"]["baseline"]
        previous: dict[str, int | float | None] = {
            path: None for path in metric_paths()
        }
        for index, event in enumerate(ordered):
            contributions: dict[str, int | float] = {}
            for path in metric_paths():
                current = metric_value(event, path)
                if current is None:
                    continue
                if mode == "delta":
                    contribution = current
                elif index == 0:
                    contribution = current if baseline == "zero" else 0
                elif previous[path] is None:
                    contribution = current if baseline == "zero" else 0
                else:
                    contribution = current - previous[path]
                previous[path] = current
                contributions[path] = contribution
                add_counter(totals, path, contribution)

            identity = event["identity"]
            model = identity.get("model")
            if model:
                target = models.setdefault(model, {})
                for path, contribution in contributions.items():
                    if path.startswith("usage."):
                        add_counter(target, path.removeprefix("usage."), contribution)
            tool_name = identity.get("toolName")
            if tool_name:
                target = tools.setdefault(tool_name, {})
                for path, contribution in contributions.items():
                    add_counter(target, path, contribution)
            wait_id = identity.get("waitId")
            if wait_id:
                target = waits.setdefault(wait_id, {})
                for path, contribution in contributions.items():
                    add_counter(target, path, contribution)

    return {
        **validation,
        "sessions": sorted(sessions),
        "segments": sorted(segments),
        "manualResumes": manual_resumes,
        "counters": {
            "usage": {
                field: totals[f"usage.{field}"] for field in USAGE_COUNTERS
            },
            "resources": {
                field: totals[f"resources.{field}"]
                for field in RESOURCE_COUNTERS
            },
        },
        "gauges": gauges,
        "models": {key: models[key] for key in sorted(models)},
        "tools": {key: tools[key] for key in sorted(tools)},
        "waits": {key: waits[key] for key in sorted(waits)},
        "coverage": {
            "unavailable": unavailable,
            "nonAggregatable": non_aggregatable,
        },
        "provenance": [
            {"source": source, "streamIds": sorted(stream_ids)}
            for source, stream_ids in sorted(provenance.items())
        ],
    }


def ingest(ledger: Path, input_path: str) -> dict[str, Any]:
    if input_path == "-":
        incoming = parse_json_lines(sys.stdin, "stdin")
    else:
        with Path(input_path).open(encoding="utf-8") as source:
            incoming = parse_json_lines(source, input_path)
    if not incoming:
        raise LedgerError("input contains no events")
    legacy = [event["eventId"] for event in incoming if event["schemaVersion"] != 2]
    if legacy:
        raise LedgerError(
            "ingest accepts normalized schema-v2 events only; legacy eventIds: "
            + ", ".join(legacy)
        )

    with locked_ledger(ledger) as stream:
        existing = events_from_locked_stream(stream, str(ledger))
        by_id = {
            (event["runId"], event["eventId"]): event for event in existing
        }
        appended: list[dict[str, Any]] = []
        for event in incoming:
            key = (event["runId"], event["eventId"])
            previous = by_id.get(key)
            if previous is not None:
                if canonical(previous) != canonical(event):
                    raise LedgerError(
                        f"runId/eventId {key!r} conflicts with existing evidence"
                    )
                continue
            by_id[key] = event
            appended.append(event)
        combined = existing + appended
        for run_id in sorted({event["runId"] for event in incoming}):
            validate_run(combined, run_id, closure=False)
        if appended:
            stream.seek(0, os.SEEK_END)
            stream.write("".join(canonical(event) + "\n" for event in appended))
            stream.flush()
            os.fsync(stream.fileno())
    return {
        "schemaVersion": 2,
        "state": "ingested",
        "ledger": str(ledger),
        "received": len(incoming),
        "appended": len(appended),
        "duplicates": len(incoming) - len(appended),
        "runIds": sorted({event["runId"] for event in incoming}),
    }


def parser() -> argparse.ArgumentParser:
    result = argparse.ArgumentParser(
        description="Manage normalized Milestone Rush JSONL telemetry"
    )
    commands = result.add_subparsers(dest="command", required=True)

    ingest_parser = commands.add_parser("ingest")
    ingest_parser.add_argument("--ledger", type=Path, required=True)
    ingest_parser.add_argument("--input", required=True, help="JSONL path or - for stdin")
    ingest_parser.add_argument("--json", action="store_true")

    for name in ("validate", "summarize"):
        command = commands.add_parser(name)
        command.add_argument("--ledger", type=Path, required=True)
        command.add_argument("--run-id", required=True)
        command.add_argument("--json", action="store_true")
    return result


def emit(value: dict[str, Any], as_json: bool) -> None:
    if as_json:
        print(json.dumps(value, sort_keys=True, separators=(",", ":")))
    else:
        print(json.dumps(value, sort_keys=True, indent=2))


def main() -> int:
    args = parser().parse_args()
    try:
        if args.command == "ingest":
            output = ingest(args.ledger, args.input)
        else:
            events = load_ledger(args.ledger)
            output = (
                validate_run(events, args.run_id)
                if args.command == "validate"
                else summarize_run(events, args.run_id)
            )
        emit(output, args.json)
        return 0
    except (LedgerError, OSError) as error:
        output = {
            "schemaVersion": 2,
            "state": "invalid",
            "reason": str(error),
        }
        emit(output, getattr(args, "json", False))
        return 2


if __name__ == "__main__":
    raise SystemExit(main())
