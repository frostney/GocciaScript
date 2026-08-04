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

Resolve the issue end to end against the current repository, or establish with
evidence that no implementation is needed.

## Gates

- Read the issue, project instructions, vision, contribution guidance,
  Definition of Ready, Definition of Done, relevant domain skills, real project
  commands, affected code paths, tests, and reproduction before deciding.
- Always perform and record web search for current evidence before presenting
  options. Prefer official and primary sources, reconcile them with the versions
  in the checkout, and treat remembered links only as search leads. Stop if the
  search cannot produce current evidence relevant to the decision.
- When `grill-with-docs` or `grill-me` is registered, run its actual
  user-question loop before presenting options. Prefer `grill-with-docs`; if
  neither exists, note that once and continue.
- During the grill, proactively give the user visual or dynamic context selected
  by the affected surface:
  - for UI/UX, show upfront mockups for every materially different experience;
  - for architecture or workflows, show a diagram or flow;
  - for interaction-heavy or technical behavior, create a short-lived dynamic
    prototype for the recommendation or the interaction that cannot be judged
    statically.
  Show a shared current-state view when it helps compare options. Clearly label
  observed facts, proposed behavior, and prototype-only shortcuts.
- Keep prototypes local and disposable, retain only reviewable captures and
  findings, and remove them when the grill concludes. Do not deploy or publish
  them. Preserve or promote a prototype only with explicit user approval; keep
  approved prototype material outside the selected worktree until its
  `git-workflow` synchronization gate passes.
- Present two to four genuinely distinct evidence-backed options, recommend one,
  and wait for the user's choice unless automatic mode applies. Include a
  compact evidence digest with links to the most relevant current sources,
  checked project versions, and any mismatch or remaining uncertainty.
- When current evidence conclusively fails a required prototype or readiness
  threshold, report that stop without asking whether to bypass the gate.
- For any code or test change, complete the project gate, one bounded
  `/code-review fix-all`, and `/create-pr`.

## Project definitions

Treat the nearest applicable `DEFINITION_OF_READY.md` and
`DEFINITION_OF_DONE.md` as canonical. If either is absent after a real search,
state that once, carry the gap into the plan and PR, and use only the workflow's
built-in checks plus commands the repository actually declares.

## Automatic mode

Automatic mode applies only when the original prompt says `automatic` or
explicitly requests it. Complete web research, source and documentation probing,
surface-appropriate artifacts, and every other gate; present the evidence and
options, select the evidence-backed recommendation, and continue. A material
product, architecture, security, scope, or vision decision disables automatic
mode.

## Workflow

1. Fetch the issue, including comments and labels; verify it is open,
   implementation-ready, and not a PR, duplicate, blocked, or rejected item.
2. Load the applicable project contracts and specialized skills.
3. Trace the full behavior from entry point to symptom and run the cited
   reproduction or artifact when possible. Search callers, siblings, tests,
   configuration, and history far enough to identify the real layer. Always
   perform current web search and reconcile its results with the checkout.
4. If the behavior no longer reproduces, distinguish:
   - already fixed and covered: recommend closing with the fixing code/commit
     and test evidence;
   - fixed without regression coverage: add the missing test only;
   - shared-root-cause sibling paths still affected: include only those paths.
5. Produce the surface-appropriate artifacts, run the grill gate with that
   context, validate readiness, confirm any clarified issue scope, then present
   the options and recommendation. Ground the checkpoint in evidence from this
   run.
6. After selection, reuse or create a focused branch/worktree and apply the
   `git-workflow` remote-default synchronization gate before editing.
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
