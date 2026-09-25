---
name: implement
description: >-
  Develops a GitHub issue or idea until its requirements and fidelity criteria
  are verified. Use when asked to implement a change or run /implement.
license: Unlicense OR MIT
compatibility: >-
  Requires git and authenticated GitHub access when an issue supplies the scope.
  Network access is needed for forge operations and current external evidence.
  An unresolved non-automatic comparison requires the registered grilling skill.
  Verification uses the project's declared commands and completion contracts.
---

# Implement

Develop the smallest complete change that satisfies the agreed requirements.
Reuse settled scope, approach and authorization; ask only for a material
unresolved choice after completing independent work. User instructions override
skill defaults. If a skill requires a pause, identify its loaded file and rule.

## Establish the work

Infer the target from the request, conversation, handoff and repository; no
issue/idea selector is required. Verify an identified issue's repository,
requirements, comments, readiness and current state. A failed lookup does not
make it an unfiled idea. For a described outcome, search for matching work and
reuse its decisions. Otherwise establish a concise mini-spec with the outcome,
scope, constraints and testable success measures; do not file an issue merely
to implement it. Resolve conflicting or ambiguous targets before editing.

Read project instructions, relevant code, tests and durable decisions. Use the
nearest Definitions of Ready and Done; if absent after a real search, use the
workflow's checks and declared project commands and record the acceptance gap.
Inspect the selected toolkit's overall capabilities before adding machinery.
Run the named reproduction or inspect the artifact. If an issue is already
fixed, verify its current behavior and regression coverage rather than duplicate
it; add missing coverage when that is the remaining requirement.

Reuse a settled approach after checking it against current evidence. Research
primary sources when a decision depends on current external facts and reconcile
them with the installed version. For an unresolved material choice or requested
comparison, read [references/approach-selection.md](references/approach-selection.md).
It owns comparison evidence and the registered `grilling` loop. Explicit
`automatic` mode skips that interview while preserving investigation and the
user's ownership of material product, architecture, security or scope choices.
Required evidence or a conclusively failed readiness threshold blocks dependent
work, not independent investigation or an established in-scope correction.

## Develop and verify

Keep this loop local to development. `/deliver` owns end-to-end delivery;
`/create-pr` owns publication. Reuse the confirmed requirements and approach.

1. Reuse or create a focused branch/worktree under `git-workflow`. Apply its
   clean-worktree and fresh-base gate before new work; preserve owned in-progress
   changes when resuming the same implementation.
2. Implement the smallest complete change, then run and inspect the real result.
   For UI/UX work, compare affected states with the requested appearance,
   interactions and fidelity; include relevant accessibility and viewports.
   Run focused developer checks while fixing observed gaps.
3. Apply `/code-review fix-all` and `/test-against-spec fix`. Review the actual
   change against the agreed requirements; test observable behavior against
   explicit specification sources. Use their direct documented equivalents if
   unavailable. Reuse matching evidence without repeating already-covered work.
4. Repair every verified requirement gap, including visual or behavioral fidelity,
   regardless of a finding's severity. Optional improvements outside the agreed
   outcome do not become requirements. Any edit invalidates affected evidence:
   repeat the necessary review, inspection and behavior checks until they agree
   on the unchanged result. Own the final applicable Definition of Done and
   project gate; run only its missing or invalidated checks.

A failed check, diagnosis or available fix does not end implementation. Continue
safe in-scope repair; reconsider an approach that is not advancing acceptance.
If required behavior needs an unavailable exact-revision preview, return the
specific publication need to the active delivery or PR caller, which owns that
operation and resumes testing afterward. A standalone implementation asks only
for the missing authority or environment after completing independent work.

Return the implemented result and observed requirement evidence to the caller.
Finish when every verified requirement gap is resolved and applicable gates
pass. Report unresolved required behavior as incomplete, never waive it because
its finding is low severity. A material unresolved decision, new authority or
external blocker after safe alternatives are exhausted can stop dependent work;
unrelated improvements do not extend the task.
