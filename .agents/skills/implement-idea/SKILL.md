---
name: implement-idea
description: >-
  Turns an unfiled idea into a confirmed mini-spec, implements and validates the
  selected approach, reviews it, and opens a draft pull request. Use when the
  user runs /implement-idea or asks to build something without an existing issue.
license: Unlicense OR MIT
compatibility: >-
  Requires git and the GitHub CLI (gh) for the /create-pr handoff, plus network
  access; verification is driven by the project's DEFINITION_OF_DONE.md and
  declared commands.
---

# Implement idea

## Instructions

Turn the idea into a confirmed mini-spec, then deliver it end to end in the
current repository.

### Gates

- Confirm a mini-spec covering the user-visible outcome, scope/non-goals, and
  verifiable success criteria before implementation.
- Read project instructions, vision, contribution guidance, Definition of Ready,
  Definition of Done, relevant domain skills, real project commands, affected
  code paths, tests, and related work before deciding.
- When `grill-with-docs` or `grill-me` is registered, run its actual
  user-question loop before presenting options. Prefer `grill-with-docs`; if
  neither exists, note that once and continue.
- Present two to four genuinely distinct evidence-backed options, recommend one,
  and wait for the user's choice unless automatic mode applies.
- For any code or test change, complete the project gate, one bounded
  `/code-review fix-all`, and `/create-pr`.

### Project definitions

Treat the nearest applicable `DEFINITION_OF_READY.md` and
`DEFINITION_OF_DONE.md` as canonical. If either is absent after a real search,
state that once, carry the gap into the plan and PR, and use only the workflow's
built-in checks plus commands the repository actually declares.

### Automatic mode

Automatic mode applies only when the original prompt says `automatic` or
explicitly requests it. It does not waive mini-spec confirmation or any other
gate. After presenting options, select the evidence-backed recommendation and
continue. Material ambiguity, risk, or a vision conflict disables automatic
mode.

### Workflow

1. Formulate and confirm the mini-spec with the user. Keep it proportional to the
   idea.
2. Load the applicable project contracts and specialized skills.
3. Find the existing implementation seam, reusable patterns, sibling features,
   tests, and architectural constraints. If the idea already exists, recommend
   using it; if partial, extend rather than duplicate it.
4. Run the grill gate, validate readiness, then present the options and
   recommendation. Ground the checkpoint in evidence from this run.
5. After selection, reuse or create a focused branch/worktree and merge the
   latest remote base before editing.
6. Implement the smallest complete change at the correct layer. Update tests and
   docs required by the mini-spec and project contracts.
7. For UI/UX work, render every affected state; capture reviewable before/after
   evidence; check accessibility, responsive behavior, themes, and design-system
   consistency; attach the evidence to the PR.
8. Run targeted checks while developing, then the applicable Definition of Done
   and repository gate. Fix failures rather than weakening the gate.
9. Run one `/code-review fix-all` pass against the success criteria, Definition
   of Done, project conventions, branch diff, and reproducible behavior.
   Resolve every validated in-scope finding and rerun affected checks. Stop for
   a material new decision; do not continue with unresolved Blocking or
   Important findings. If `/code-review` is unavailable, perform that same
   bounded review and fix pass directly.
10. Use `/create-pr` and summarize the mini-spec, delivered outcome, and observed
    completion evidence in the PR.
