---
name: software-engineering-excellence
description: >-
  Applies the user's ambient engineering bar: current evidence, complete
  in-scope solutions, reuse, real validation, right-sized value, and
  maintainability. Use for planning, orchestrating, implementing, debugging,
  reviewing, refactoring, architecture, release delivery, or substantial
  technical investigation.
license: Unlicense OR MIT
---

# Software engineering excellence

Leave the system more maintainable and the next change easier. Find simple
solutions for complex problems, but no shortcuts. Lead with questions that
reveal the problem's real shape before selecting implementation mechanics.
A better question separates the desired outcome, hard constraints, and observed
failure from the mechanism currently being discussed; its answer can change the
solution rather than merely select details within it.

## Working standard

1. **Ground in current reality.** Read applicable instructions, source,
   project-defined commands, primary specifications, and durable decisions.
   Treat issue text, comments, tests, docs, and prior notes as leads until
   verified. Run the named reproduction or artifact when possible, then act.
2. **Reuse by meaning.** Search for existing helpers, patterns, definitions, and
   vocabulary. Reuse when semantics match; do not abstract merely similar shapes.
3. **Solve the complete in-scope problem.** Cover real success, failure, and
   state-transition paths. Fix blockers that invalidate the requested result;
   report unrelated findings without absorbing them into scope.
4. **Validate the real bar.** Observe every claimed pass, number, behavior, and
   action in the current run. Reproduce defects when possible, add meaningful
   regression coverage, and run the repository's relevant gate. Never weaken a
   gate to obtain green output. When a gate fails, diagnose the failure, using
   the registered `diagnosing-bugs` skill when available. Report the diagnosis
   and evidence before changing code. If the failure blocks the requested result
   or belongs to its authorized scope, fix it and verify the fix; otherwise
   report it to the user without absorbing it into the current change.
5. **Make every surface earn its cost.** Add only code, tests, fallbacks,
   abstractions, or tools with a real caller, requirement, or failure mode.
   Remove unused surfaces.
6. **Respect authority boundaries.** Diagnosis, review, and planning authorize
   assessment; change requests authorize reversible in-scope implementation and
   relevant validation. Pause only for a material product, architecture,
   security, compatibility, or scope choice that evidence cannot resolve.

For genuinely multi-layer work, establish a thin runnable path and deepen it in
increments. Validate material performance changes against a relevant baseline.
Use the language's native idioms and the project's established conventions;
language and stack skills own exact forms. Follow surrounding comment density
and explain non-obvious decisions.

## Context boundaries

When one conversation coordinates a chain of two or more substantial
deliverables, keep that conversation thin. It owns confirmed decisions and
their provenance, ordering, cross-deliverable dependencies, and user
communication. Delegate each deliverable to one bounded worker using the host
equivalent of no inherited conversation history. A small local task or a single
ordinary deliverable stays local unless another applicable workflow requires
delegation.

Give a worker only the applicable selected decisions and contracts, repository
and issue or pull-request identity, exact starting state, owned scope,
dependencies, acceptance criteria, and gates. Include recent conversation only
when the deliverable cannot be understood without it; record that scoped
exception instead of inheriting the full history.

Require one terminal summary per worker: outcome, changed repository or forge
state, exact validation evidence, blockers or contradictory evidence, and facts
needed by a dependent deliverable. Keep intermediate investigation, command
output, and logs in the worker context. A material decision or conflict returns
to the coordinator; it is never resolved from an incomplete packet. If the host
cannot provide isolated workers, report that limitation and do not claim the
boundary was applied.

An applicable orchestrator such as `milestone-rush` retains ownership of its
more specific worker, checkpoint, integration, and telemetry contract.

## Evidence and communication

Ground progress and completion in current source or tool evidence. Report
material outcomes, limitations, and blockers without narrating routine activity.
Finish with the outcome first and enough evidence for a reader who did not see
the work trace.

After a correction, re-ground, identify the failed assumption, and make the
smallest change that restores the contract. Do not compensate with broader
scope, tooling, or framework churn.

## Situational depth

- [references/structural-delivery.md](references/structural-delivery.md):
  architecture, greenfield, and deciding the correct layer.
- [references/investigation.md](references/investigation.md): defect diagnosis,
  design evaluation, and source comparisons.
- [references/barometer.md](references/barometer.md): periodic direction check,
  not a score.
