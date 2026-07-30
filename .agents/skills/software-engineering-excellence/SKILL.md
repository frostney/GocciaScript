---
name: software-engineering-excellence
description: >-
  Applies the user's ambient engineering bar: current evidence, complete
  in-scope solutions, reuse, real validation, right-sized value, and
  maintainability. Use for planning, implementing, debugging, reviewing,
  refactoring, architecture, or substantial technical investigation.
license: Unlicense OR MIT
---

# Software engineering excellence

Leave the system more maintainable and the next change easier. Prefer the
smallest complete solution at the right layer: neither a symptom-hiding diff nor
speculative architecture.

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
   gate to obtain green output.
5. **Make every surface earn its cost.** Add only code, tests, fallbacks,
   abstractions, or tools with a real caller, requirement, or failure mode.
   Remove unused surfaces.
6. **Respect authority boundaries.** Diagnosis, review, and planning authorize
   assessment; change requests authorize reversible in-scope implementation and
   relevant validation. Pause only for a material product, architecture,
   security, compatibility, or scope choice that evidence cannot resolve.

For genuinely multi-layer work, establish a thin runnable path and deepen it in
increments. Validate material performance changes against a relevant baseline.
Follow the surrounding code's comment density and idiom; explain non-obvious
decisions.

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
