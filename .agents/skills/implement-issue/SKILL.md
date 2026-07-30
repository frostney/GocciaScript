---
name: implement-issue
description: >-
  Validates and implements a GitHub issue against current repository evidence,
  runs the project's completion gate, reviews the change, and opens a draft pull
  request. Use when the user runs /implement-issue with an issue number.
license: Unlicense OR MIT
compatibility: >-
  Requires the GitHub CLI (gh) authenticated to the target repository and
  network access; verification is driven by the project's DEFINITION_OF_DONE.md
  and declared commands.
---

# Implement issue

## Instructions

Resolve the issue end to end against the current repository, or establish with
evidence that no implementation is needed.

### Gates

- Read the issue, project instructions, vision, contribution guidance,
  Definition of Ready, Definition of Done, relevant domain skills, real project
  commands, affected code paths, tests, and reproduction before deciding.
- When `grill-with-docs` or `grill-me` is registered, run its actual
  user-question loop before presenting options. Prefer `grill-with-docs`; if
  neither exists, note that once and continue.
- Present two to four genuinely distinct evidence-backed options, recommend one,
  and wait for the user's choice unless automatic mode applies.
- When current evidence conclusively fails a required prototype or readiness
  threshold, report that stop without asking whether to bypass the gate.
- For any code or test change, complete the project gate, one bounded
  `/code-review fix-all`, and `/create-pr`.

### Project definitions

Treat the nearest applicable `DEFINITION_OF_READY.md` and
`DEFINITION_OF_DONE.md` as canonical. If either is absent after a real search,
state that once, carry the gap into the plan and PR, and use only the workflow's
built-in checks plus commands the repository actually declares.

### Automatic mode

Automatic mode applies only when the original prompt says `automatic` or
explicitly requests it. Complete every gate, present the options, select the
evidence-backed recommendation, and continue. Material ambiguity, risk, or a
vision conflict disables automatic mode.

### Workflow

1. Fetch the issue, including comments and labels; verify it is open,
   implementation-ready, and not a PR, duplicate, blocked, or rejected item.
2. Load the applicable project contracts and specialized skills.
3. Trace the full behavior from entry point to symptom and run the cited
   reproduction or artifact when possible. Search callers, siblings, tests,
   configuration, and history far enough to identify the real layer.
4. If the behavior no longer reproduces, distinguish:
   - already fixed and covered: recommend closing with the fixing code/commit
     and test evidence;
   - fixed without regression coverage: add the missing test only;
   - shared-root-cause sibling paths still affected: include only those paths.
5. Run the grill gate, validate readiness, then present the options and
   recommendation. Ground the checkpoint in evidence from this run.
6. After selection, reuse or create a focused branch/worktree and merge the
   latest remote base before editing.
7. Implement the smallest complete change at the correct layer. Update tests and
   docs required by the issue and project contracts.
8. For UI/UX work, render every affected state; capture reviewable before/after
   evidence; check accessibility, responsive behavior, themes, and design-system
   consistency; attach the evidence to the PR.
9. Run targeted checks while developing, then the applicable Definition of Done
   and repository gate. Fix failures rather than weakening the gate.
10. Run one `/code-review fix-all` pass against the acceptance criteria,
    Definition of Done, project conventions, branch diff, and reproducible
    behavior. Resolve every validated in-scope finding and rerun affected
    checks. Stop for a material new decision; do not continue with unresolved
    Blocking or Important findings. If `/code-review` is unavailable, perform
    that same bounded review and fix pass directly.
11. Use `/create-pr`, include `Closes #<issue>`, and report only observed
    completion evidence.
