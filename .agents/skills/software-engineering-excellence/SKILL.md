---
name: software-engineering-excellence
description: >-
  Apply the user's engineering standards during substantial technical work:
  preserve scope, use current evidence, and complete authorized outcomes.
license: Unlicense OR MIT
---

# Software engineering excellence

Carry the user's intent to a verified result with less steering and rework.
Maintainability governs tradeoffs. Establish the requested outcome, constraints
and acceptance criteria before choosing implementation mechanics; reuse settled
answers instead of reopening them.

## Discover and choose the right layer

Read applicable project instructions, relevant code, tests and durable decisions.
Run the named reproduction or inspect the requested artifact when possible.
Treat historical notes and issue descriptions as leads to verify.

Before replacing a selected toolkit's capabilities, inspect its overview and
consumer guidance as well as the immediate subcommand. Reuse project build,
test, formatting and workflow commands. Add an adapter only for a demonstrated
missing capability. Generic execution mechanics belong in the harness or a
reusable helper; product-specific contracts belong in the repository.

Make the smallest complete change at the layer that owns the behavior. Preserve
required success, failure and transition paths. Include blockers that invalidate
the requested result; keep unrelated improvements outside its completion bar.
For multi-layer systems, establish a thin runnable path and deepen it in working
increments. Follow native language idioms and the project's conventions.

## Continue the authorized work

Implementation authority persists across questions, corrections, diagnoses,
worker returns and compaction. Update the affected decision or requirement while
retaining the parent objective. A question calls for an answer; it does not
silently cancel the remaining work. Follow an explicit change of direction.

When a check fails, diagnose it and fix an established in-scope cause. Use the
registered `diagnosing-bugs` skill when available and relevant. An unstable
benchmark blocks accepting a speedup while permitting authorized diagnosis and
repair. After a correction, revisit the failed assumption and take the practical
next action; reconsider the strategy if repeated work is not advancing the goal.

Return control when the requested outcome is verified, safe in-scope progress
is exhausted by an external blocker, or a material unresolved decision or new
authority is needed. A known available fix is not a reason to stop. Complete
independent authorized work before asking. When a skill requires a pause, link
the exact loaded file, quote its rule and explain the missing decision or
permission. User instructions and existing authorization override skill defaults.

Assessment-only work can finish with findings; it does not authorize remediation.
Stop adding work once the agreed acceptance criteria hold. For resumable work,
record the active outcome, settled decisions, authority, evidence, remaining
obligations and next executable action. Use
[references/workstream-continuity.md](references/workstream-continuity.md) when
multiple turns, external waits or workers require an explicit handoff.

## Verify the requested result

Use explicit requirements and the project's declared gates. Inspect the real
interface or artifact for claimed behavior. Qualitative acceptance requires
judgment against the user's reference or criteria; test counts, performance
metrics and a worker's success report do not establish the whole outcome.

Use focused checks while editing. The caller owns the final aggregate gate;
subskills contribute applicable evidence without rerunning it. Reuse results
only when content, command, environment and covered requirements match. Rerun
missing or invalidated checks after changes, failures or unresolved concerns.
Keep independent review judgment. Never weaken coverage or hide a failure to
obtain a pass. Diagnose an observed anomaly that affects acceptance; report
unrelated defects with enough evidence for a separate decision.

Write regression tests for observable behavior or consequential invariants,
with expectations derived from independent requirements or invariants. Do not
write tautological tests: never derive expected results from the implementation
under test or merely assert that a mock returns its configured value. Do not
couple tests to implementation details. Precise output or interaction assertions
are valid when they enforce a specified contract. A correct refactor or
equivalent instruction rewrite should not break tests. Each test should catch
a relevant incorrect behavior. Do not assert prose, private calls or source
tokens as proof of behavior.
Validate fixture preconditions so a failed setup cannot masquerade as a product
failure. Keep structural/schema checks distinct from behavioral acceptance.

A requested check or recorded action without a returned result remains
unverified. Keep implementation, local validation, external validation and
publication status distinct. Read a gate result before performing the dependent
action. Capture shell exit status before another command can overwrite it.

## Performance and maintainability

Consider both product performance and time from an edit to trustworthy feedback.
Measure representative before/after behavior for performance claims or changes
to frequent, latency-sensitive or resource-intensive paths, tooling, hooks,
startup, concurrency or external operations. Include workload, environment,
warm/cold state, samples and comparison statistic. For a PR, compare the target
branch baseline with the PR candidate under matching conditions and identify
both revisions. Do not claim gains where
measurements overlap or extrapolate a microbenchmark beyond its workload.

Reduce duplicate work with correct incremental checks, caching and shared
results. Preserve coverage and hooks. Diagnose material regressions, including
flakiness, retries, queue delay and redundant validation. Complexity needs a real
caller or demonstrated benefit, a clear contract and relevant regression
coverage. Comments should explain constraints or decisions the code cannot
express; improve unclear names rather than narrating implementation.

## Coordinating deliverables

For a chain of substantial deliverables, the coordinator owns confirmed
decisions, dependencies, integration and user communication. Use bounded workers
when the task and available host support delegation; keep small ordinary work
local. An applicable orchestrator owns its more specific coordination contract.

Give each worker the selected decisions, repository and work-item identity,
starting state, owned scope, dependencies, required behavior and gates. Prefer
an isolated context; include recent conversation only when needed to understand
the deliverable. Request an outcome, changed state, observed validation,
limitations and facts needed by dependent work. Keep investigation logs local
to the worker. Bring material choices or conflicting evidence to the coordinator.

Verify and integrate the returned result. Worker completion closes its lane,
not the parent objective. Continue the parent's remaining authorized work.
If isolated workers are unavailable, disclose the limitation and use an allowed
local route; do not claim delegation occurred or invent unavailable telemetry.

## Communication and situational depth

Lead with the outcome and decision-relevant evidence. Keep uncertainty and
blockers explicit without narrating routine activity. Choose references by need:

- [references/structural-delivery.md](references/structural-delivery.md):
  architecture, greenfield work and selecting the correct layer.
- [references/investigation.md](references/investigation.md): defect diagnosis,
  design evaluation and source comparisons.
- [references/barometer.md](references/barometer.md): check direction when the
  strategy needs reconsideration; it is not a score or mandatory ceremony.
