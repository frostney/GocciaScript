---
name: test-against-spec
description: >-
  Tests observable behavior against explicit requirements through the real
  interface. Use when asked to test or verify that a change meets its
  specification or acceptance criteria, when the user runs /test-against-spec,
  or when a delivery workflow needs real-interface acceptance evidence.
license: Unlicense OR MIT
metadata:
  agents-role: entry-point
  agents-text: Test a change against its requirements through the real interface.
---

# Test against spec

Establish whether the delivered behavior matches the explicit specification.
Report by default. With the exact `fix` qualifier, fix observed in-scope gaps
and retest them. This skill does not replace or run source review or the
repository's full project gate; the caller owns those, and source review and
unit-test success alone are not behavior evidence.

Reuse recorded real-interface evidence when the implementation, requirement,
environment, and inputs match, except that fix mode reproduces each failure
first-hand before fixing it. Run missing or invalidated behavior checks after
changes, failures, or unresolved concerns.

## Boundaries

- Derive expected behavior only from the user request, issue, confirmed
  mini-spec, product documentation, ADR, or another explicit decision. Never
  infer it from the implementation. Stop for a
  [material](../software-engineering-excellence/SKILL.md#blockers-and-decisions)
  conflict or ambiguity.
- Test externally observable outcomes. Use the rendered product for user-facing
  behavior and the real API, CLI, library entry point, job, package, migration,
  or deployed service for other behavior.
  For qualitative requirements, compare the actual artifact and behavior with
  the user's reference or acceptance criteria; passing metrics alone cannot
  establish visual fidelity or usability.
- Do not use implementation source, test source, unit tests, mocks, snapshots,
  or a patch as evidence that behavior works. Requirements with no executable
  behavior are outside this skill's scope and belong to code review or the
  project gate.
- Prefer a preview deployment when it is available and tied to the exact
  revision under test. Use the local environment when no suitable preview
  exists or when local execution is needed to cover the current working change.
  Use existing black-box automation when it exercises the real delivered
  interface and its result is current.
- This step does not commit, push, deploy, publish, or change external data
  unless the user separately authorized that action. Use disposable test data
  where possible and ask before a test would create a material external side effect.

## Workflow

1. Record the specification sources and the exact worktree revision, commit, or
   preview revision under test. Separate each externally observable requirement
   from requirements outside this skill's scope, and select the environment for
   each behavior under the boundaries above.
2. Exercise every testable requirement. Cover the intended path and the most
   consequential failure or boundary path. For user-facing changes, interact
   with the rendered product and cover affected states, accessibility, and
   relevant viewports.
3. Record the environment, setup, action or command, input, expected result, and
   observed result. A requirement passes only when the expected outcome is
   observed against the exact content under test. A submitted check or
   acknowledgment without a result is not a pass; re-run it.
4. When neither a current preview nor the local environment can reproduce
   required behavior, look in the project's docs and scripts for another route,
   such as a sandbox, emulator or harness, and use it. Mark the behavior
   unverified only when no route works, and report what was attempted.
5. Without `fix`, make no edits. With `fix`, read
   [references/fix-mode.md](references/fix-mode.md) as soon as `fix` is
   present, before inspecting implementation source, and follow it for every
   failed requirement.

Return a structured summary in the active workflow, not a committed or ignored
artifact. Include the tested revision, environments, specification sources,
each requirement labeled `passed`, `failed`, `unverified`, or `out of scope`
with its evidence or limitation, fixes and changed files when applicable, and whether any
behavior remains failed or unverified. Started directly, continue under the
[delivery settings](../deliver/SKILL.md#delivery-settings); with
`entry-points: deliver`, the tested change, including any fix, goes on to
`/deliver`, which reviews, gates and publishes it up to the configured endpoint
(`ready-to-merge` when there is no `ORCHESTRATION.md`). With `stop`, end with
this summary.
