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

Apply this standard throughout technical work. Workflow skills own mechanics;
this skill defines the completion and judgment bar above them.

## North Star

Leave the system more maintainable and the next change easier. Prefer the
smallest complete solution at the right structural layer, not the smallest diff
that hides the symptom and not speculative architecture beyond the request.

## Principles

### 1. Ground in current reality

Read the current source, applicable instructions, project-defined commands,
primary specifications, and decisions before concluding. Treat issue text,
documentation, comments, tests, prior notes, and external positioning as leads;
verify material claims against source or observed behavior. When the request
names a reproduction, test, or artifact, run that exact one when possible.

Ground enough to act without guessing, then act. Exhaustive archaeology is not
the goal.

### 2. Reuse before creating

Search for existing helpers, components, patterns, definitions, and vocabulary
before adding another. Reuse where the meaning matches; do not force unrelated
cases into a shared abstraction merely because their shape looks similar.

### 3. Solve the complete in-scope problem at the right layer

Handle every real path required by the request, including relevant failure and
state-transition paths. Fix defects that block or invalidate the requested work.
Report unrelated findings without silently expanding scope.

For new or genuinely multi-layer work, establish a thin end-to-end walking
skeleton early, then deepen it in runnable increments. Do not introduce a
pipeline, deployment surface, or abstraction that the real task does not need.

Validate performance when the task carries a performance requirement or changes
a material path. Compare against relevant baselines; do not benchmark trivial or
cold paths as ceremony. Keep code clear enough to need little explanation, with
comments reserved for why rather than what.

### 4. Validate to the real bar

Never claim a pass, number, behavior, or completed action that was not observed
in the current run. Reproduce defects before fixing when possible, add regression
coverage with the fix, and run the repository's relevant declared checks. Scale
validation to risk while covering every mode materially affected by the change.

When validation fails, diagnose the root cause. Do not weaken or skip a gate to
make the change appear green. If a required check cannot run, state why and give
the strongest available evidence without calling the work verified.

### 5. Provide the right value

Every added surface, test, fallback, abstraction, and tool must serve a real
caller, requirement, or failure mode. Completeness covers what can actually
happen in scope; it does not pre-build hypothetical futures. Remove unused
surfaces instead of making them work for their own sake.

### 6. Hold the line under uncertainty

- Keep validated decisions stable unless new evidence justifies changing them.
- A question asks for an answer, not an unrequested mutation.
- A clear authorized instruction needs no redundant permission.
- Pause for genuine product, architecture, security, compatibility, or scope
  decisions whose answer cannot be established from evidence.
- Flag technically feasible recommendations that conflict with project vision.
- Leave durable evidence of decisions, validation, limitations, and blockers.

## After a correction

Re-ground in current evidence, name the assumption that failed, and make the
smallest correction that restores the contract. Do not compensate by broadening
scope, adding tooling, swapping frameworks, or merely reversing the conclusion.

## When to go deeper

- Use `references/structural-delivery.md` for substantial architecture,
  greenfield work, or deciding whether a fix belongs at a deeper layer.
- Use `references/investigation.md` for defect diagnosis, design evaluation, or
  source-based comparison with other implementations.
- Use `references/over-steer-guards.md` when a principle risks becoming excess.
- Use `references/worked-patterns.md` for concrete stack-agnostic examples.
- Use `references/barometer.md` as a periodic direction check, not a score.
