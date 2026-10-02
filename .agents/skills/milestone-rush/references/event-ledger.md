# Milestone Rush event ledger

The bundled one-shot command owns normalized event ingestion, validation, and
aggregation. It never polls, runs as a daemon, or wakes a model. Run it from
the project root, which holds the ledger; `SKILL_DIR` is this skill's
directory, such as `.agents/skills/milestone-rush` in a project install:

```bash
python3 "$SKILL_DIR/scripts/event_ledger.py" ingest \
  --ledger .agent/milestone-rush-events.jsonl --input normalized-events.jsonl
python3 "$SKILL_DIR/scripts/event_ledger.py" validate \
  --ledger .agent/milestone-rush-events.jsonl --run-id "$RUN_ID" --json
python3 "$SKILL_DIR/scripts/event_ledger.py" summarize \
  --ledger .agent/milestone-rush-events.jsonl --run-id "$RUN_ID" --json
```

The ledger is append-only and ignored. Never stage or commit it. Preserve
earlier runs and distinguish them by stable `runId`. `ingest` writes only
validated schema-v2 events, treats a structurally identical repeated
`runId`/`eventId` pair as idempotent, and rejects a conflicting duplicate before
appending anything.

## Adapter boundary

Host integrations translate their native lifecycle and usage evidence into the
normalized envelope below, then invoke `ingest`. Harnesses expose different
boundaries, including thread/turn/item streams, lifecycle hooks, and persisted
session/agent/tool events. The core command therefore contains no provider
transcript parser or harness adapter. A host that cannot expose a measurement
records it as unavailable; it must not infer it from prose or substitute zero.

## Schema v2 envelope

Every normalized event contains the existing lifecycle envelope plus expanded
identity and explicit measurement provenance:

```json
{
  "schemaVersion": 2,
  "runId": "stable run ID",
  "eventId": "stable event ID",
  "type": "transition or sample type",
  "timestamp": "RFC 3339 timestamp",
  "spanId": "stable span ID or null",
  "spanKind": "span category or null",
  "parentSpanId": "stable span ID or null",
  "blockingSpanIds": [],
  "identity": {
    "laneId": null,
    "decisionId": null,
    "issue": null,
    "pullRequest": null,
    "branch": null,
    "head": null,
    "sessionId": null,
    "segmentId": null,
    "toolName": null,
    "waitId": null,
    "model": null
  },
  "actor": {
    "kind": "coordinator | worker | watcher | tool | user",
    "capabilityClass": null
  },
  "context": {
    "mode": "isolated | recent-slice | full-history | coordinator",
    "exceptionDecisionId": null
  },
  "result": "started | succeeded | failed | pending | blocked | cancelled | unavailable",
  "blocker": null,
  "retryAt": null,
  "measurement": {
    "source": "host-owned source name",
    "streamId": "stable source stream ID",
    "sequence": 0,
    "mode": "delta | snapshot",
    "baseline": null
  },
  "usage": {
    "inferences": 1,
    "inputTokens": 100,
    "cachedInputTokens": 80,
    "uncachedInputTokens": 20,
    "outputTokens": 10,
    "reasoningTokens": 4,
    "compactions": 0,
    "unavailableFields": []
  },
  "resources": {
    "runnerMilliseconds": null,
    "agentMilliseconds": 2500,
    "toolCalls": 2,
    "commandMilliseconds": null,
    "effectiveWorkers": 1,
    "unavailableFields": [
      "runnerMilliseconds",
      "commandMilliseconds"
    ]
  }
}
```

Use a null `measurement` only for a lifecycle event with no measured values.
Every null usage or resource value must appear in its adjacent
`unavailableFields`; zero is a measured value. `session_started`,
`manual_resume`, `model_sample`, and `tool_sample` make session boundaries,
manual continuation, model usage, and tool usage explicit. Their matching
identity fields are required. Each identity value is a non-empty string or
null; `issue` and `pullRequest` may also be positive integers.

## Counter semantics

Measurement streams are keyed by `source` and `streamId`. Sequence numbers
start at zero and remain contiguous. A stream uses exactly one mode and one
baseline:

- `delta` values are increments and use a null baseline;
- `snapshot` values are cumulative and declare `zero` when the first sample is
  measured from zero, or `start` when the first sample establishes a baseline
  that is not part of this run's total;
- a decrease is an unexplained reset and invalidates the stream; start a new
  stream instead;
- missing baselines, sequence gaps, mixed modes, and changed baselines are
  invalid; and
- `effectiveWorkers` is a gauge summarized as minimum, maximum, and latest by
  event timestamp across all streams, with ties broken by `eventId`. It is
  never added to counters.

The summary reports session and segment identities, manual resumes, usage and
resource counters, gauges, per-model usage, per-tool and per-wait totals,
unavailable fields, non-aggregatable evidence, and source/stream provenance.
Schema-v1 lifecycle events remain readable; of their identity values, only
`sessionId` and `segmentId` must be non-empty strings or null. Their numeric values are listed as
non-aggregatable because v1 did not identify delta versus snapshot semantics or
the baseline; they never enter totals.

## Lifecycle and closure validation

Use explicit `span_started` and `span_finished` events with the same `spanId`,
`spanKind`, and identity for worker activity, coordinator work, local commands
or gates, CI, review, cooldown, decision wait, remediation, rework, restack,
merge, and genuine idle time. Add stable blocking relationships so later
analysis can distinguish critical-path, overlapping, and masked work.

Record run, lane, gate, push, CI, review, wait, retry, remediation, restack,
merge, rework, and run transitions when they occur. Pair
`decision_requested` and `decision_resolved` with one stable decision ID and
record policy-threshold checkpoints and interventions.

Record work that a later head, base, topology, or decision made obsolete with
a `work_superseded` event whose `spanId` names the superseded span. At closure
that span must be one the run started and finished, and either its
`span_finished` has result `cancelled`, or a `blocker` string on the
`work_superseded` event or on that `span_finished` says why it was not
cancelled. Work that had already finished when it was superseded needs the
blocker too. The `work_superseded` event's own `result` is not consulted, and
a later `work_superseded` event for the same span can supply the blocker.
`ingest` accepts superseded work that is still in flight.

Before milestone closure, `validate` must succeed for the current `runId`.
Missing or conflicting IDs, invalid measurement streams, unclosed spans,
missing transition classes, silent nulls, absent command or CI identities the
host exposes, and superseded work without a terminal cancellation or explicit
blocker remain invalid closure evidence.
Command and CI events retain their real command, workflow, run, and job
identities when the host exposes them. Elapsed wall time stays separate from
aggregate runner time, concurrent agent time, tool calls, inferences, and
tokens.
