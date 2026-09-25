---
name: test-against-spec
description: >-
  Test observable behavior against explicit requirements when requested or when
  a delivery workflow needs real-interface acceptance evidence.
license: Unlicense OR MIT
---

# Test against spec

Reuse recorded real-interface evidence when the implementation, requirement,
environment, and inputs match. Run missing or invalidated behavior checks after
changes, failures, or unresolved concerns. The caller owns the aggregate project
gate; source review and unit-test success alone are not behavior evidence.

Establish whether the delivered behavior matches the explicit specification.
Report by default. With the exact `fix` qualifier, fix observed in-scope gaps
and retest them. This skill does not replace source review or the repository's
full project gate.

## Boundaries

- Derive expected behavior only from the user request, issue, confirmed
  mini-spec, product documentation, ADR, or another explicit decision. Never
  infer it from the implementation. Stop for a material conflict or ambiguity.
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
  revision under test. Otherwise use the local environment. Use existing
  black-box automation when it exercises the real delivered interface and its
  result is current.
- Do not commit, push, deploy, publish, or change external data unless the user
  separately authorized that action. Use disposable test data where possible
  and ask before a test would create a material external side effect.

## Workflow

1. Record the specification sources and the exact worktree revision, commit, or
   preview revision under test. Separate each externally observable requirement
   from requirements outside this skill's scope.
2. Select the best available environment for each behavior. Prefer a current
   preview deployment, then use the local environment when no suitable preview
   exists or when local execution is needed to cover the current working change.
3. Exercise every testable requirement. Cover the intended path and the most
   consequential failure or boundary path. For user-facing changes, interact
   with the rendered product and cover affected states, accessibility, and
   relevant viewports.
4. Record the environment, setup, action or command, input, expected result, and
   observed result. A requirement passes only when the expected outcome is
   observed against the exact content under test. A submitted check or
   acknowledgment without a result leaves the requirement unverified.
5. Without `fix`, make no edits. Report every passed, failed, unverified, and
   out-of-scope requirement.
6. With `fix`, reproduce a failure through the external interface before
   inspecting implementation source. Apply the smallest in-scope fix, run only
   the focused developer checks needed for that fix, refresh the available test
   environment, and rerun the same black-box behavior. Each edit invalidates
   affected results. Repeat while available access and environments allow safe
   progress.
7. Stop fixing for a material product, architecture, security, compatibility,
   or scope decision. When neither a current preview nor the local environment
   can reproduce required behavior, report what was attempted and mark the
   behavior unverified.
8. When a fix can be exercised only through a preview and no preview contains
   the changed revision, report the fix as applied but unverified. The caller
   owns any authorized push or deployment and must invoke this skill again on
   the resulting exact-revision preview.

Return a structured summary in the active workflow, not a committed or ignored
artifact. Include the tested revision, environments, specification sources,
each requirement and its evidence or limitation, fixes and changed files when
applicable, and whether any behavior remains failed or unverified.
