---
name: software-engineering-excellence
description: >-
  Applies the user's engineering standards during substantial technical work:
  preserve scope, use current evidence, delegate independent parts, and
  complete authorized outcomes. Use during substantial implementation,
  debugging, refactoring, or multi-part delivery work.
license: Unlicense OR MIT
metadata:
  agents-role: ambient
  agents-text: Keep the agreed scope, ground claims in current evidence, and finish verified work during substantial technical tasks.
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
Follow native language idioms and the project's conventions.

## Continue the authorized work

Treat an action-oriented request, such as "can you…" or "I want…", as an
instruction to do the work. A plain-language request for a workflow's outcome,
such as "open a PR" or "file an issue", runs that workflow with its gates. An
explicit instruction about one choice, such as a label, an endpoint or showing
a draft first, replaces only that default.

Implementation authority persists across questions, corrections, diagnoses,
worker returns and compaction. Update the affected decision or requirement while
retaining the parent objective. A question calls for an answer; it does not
silently cancel the remaining work. Follow an explicit change of direction.

When a check fails, diagnose it and fix an established in-scope cause. Use the
registered `diagnosing-bugs` skill when available and relevant. After a
correction, revisit the failed assumption and take the practical next action;
reconsider the strategy if repeated work is not advancing the goal.

Return control when the requested outcome is verified, safe in-scope progress
is exhausted by an external blocker, or a material unresolved decision or new
authority is needed. A known available fix is not a reason to stop. Complete
independent authorized work before asking. None of these ends a turn while
authorized work remains:

- a summary that announces the next step instead of taking it;
- an offer to continue unless the user prefers otherwise;
- a list of decisions when none of them blocks the remaining work;
- a long turn or a completed milestone.

Put status notes and recommendations on open decisions in the same message as
the next action, and continue the work that does not depend on the answer. When
a skill requires a pause, link the exact loaded file as a Markdown link, quote
its rule verbatim in a block quote, and explain the missing decision or
permission. Risky or irreversible actions keep their
confirmation gates.

Assessment-only work can finish with findings; it does not authorize remediation.
Stop adding work once the agreed acceptance criteria hold. When work spans
turns, compaction, external waits or workers, keep the record in
[references/workstream-continuity.md](references/workstream-continuity.md).

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
with expectations derived from independent requirements, never from the
implementation under test. Each test should catch a relevant incorrect behavior
and survive a correct refactor or equivalent instruction rewrite: do not assert
that a mock returns its configured value, or treat prose, private calls or
source tokens as proof of behavior. Precise output or interaction assertions
are valid when they enforce a specified contract.

Commit tests where the task asks for them or the repository already tests that
kind of change, sized like neighboring tests: about one focused test per stated
behavior. Remove scratch probes unless one becomes the regression test for a
defect found. A reversible, low-impact change without a behavioral contract,
such as copy, a configuration value or documentation, needs no new test; the
project gate covers it. Validate fixture preconditions so a failed setup
cannot masquerade as a product failure. Keep structural/schema checks distinct
from behavioral acceptance.

A requested check or recorded action without a returned result remains
unverified. Keep implementation, local validation, external validation and
publication status distinct. Read a gate result before performing the dependent
action. Capture shell exit status before another command can overwrite it.

## Performance and maintainability

Consider both product performance and time from an edit to trustworthy
feedback. Read [references/performance.md](references/performance.md) when
making a performance claim or changing a frequent, latency-sensitive or
resource-intensive path, tooling, hooks, CI, startup, concurrency or external
operations, or when diagnosing a slow, flaky or redundant feedback loop.

Complexity needs a real caller or demonstrated benefit, a clear contract and
relevant regression coverage. Comments should explain constraints or decisions
the code cannot express; improve unclear names rather than narrating
implementation.

## Coordinating deliverables

For substantial work with independent parts, the coordinator owns confirmed
decisions, dependencies, integration and user communication. Delegate each
sizeable independent part to a bounded worker by default when the host supports
delegation; keep small or tightly coupled work local. `no-subagents` or an
equivalent user instruction keeps all of the work local. An applicable
orchestrator owns its more specific coordination contract.

Give each worker the selected decisions, repository and work-item identity,
starting state, owned scope, dependencies, required behavior and gates. Prefer
an isolated context; include recent conversation only when needed to understand
the deliverable. Request an outcome, changed state, observed validation,
limitations and facts needed by dependent work. Keep investigation logs local
to the worker. Bring material choices or conflicting evidence to the coordinator.
A packet that includes publication or merge follows the delegation rule in
[deliver](../deliver/SKILL.md).

While workers run, continue the coordinator's work that does not depend on
their results. Check each returned result's evidence before accepting and
integrating it. Worker completion closes its lane, not the parent objective.
Continue the parent's remaining authorized work.
If isolated workers are unavailable, disclose the limitation and use an allowed
local route; do not claim delegation occurred or invent unavailable telemetry.

## Communication and situational depth

Lead with the outcome and decision-relevant evidence. Keep uncertainty and
blockers explicit without narrating routine activity. Choose references by need:

- [references/structural-delivery.md](references/structural-delivery.md):
  architecture, greenfield or multi-layer systems, and a symptom that may sit
  above the real fault.
- [references/investigation.md](references/investigation.md): defect diagnosis,
  design evaluation and source comparisons.
- [references/barometer.md](references/barometer.md): check direction when the
  strategy needs reconsideration; it is not a score or mandatory ceremony.
