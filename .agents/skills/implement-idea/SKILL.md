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

Turn the idea into a confirmed mini-spec, then deliver it end to end in the
current repository.

## Gates

- Start with a provisional mini-spec of at most 400 characters, including
  spaces. Confirm the final mini-spec covering the user-visible outcome,
  scope/non-goals, and verifiable success criteria only after the
  artifact-assisted grill.
- Read project instructions, vision, contribution guidance, Definition of Ready,
  Definition of Done, relevant domain skills, real project commands, affected
  code paths, tests, and related work before deciding.
- Always perform and record web search for current evidence before presenting
  options. Prefer official and primary sources, reconcile them with the versions
  in the checkout, and treat remembered links only as search leads. Stop if the
  search cannot produce current evidence relevant to the decision.
- When `grill-with-docs` or `grill-me` is registered, run its actual
  user-question loop before presenting options. Prefer `grill-with-docs`; if
  neither exists, note that once and continue.
- Before proposing an option, assemble one neutral evidence packet from the
  repository, reproduction, project contracts, current web research, and shared
  current-state artifacts. Derive every option from this same packet.
- Define one comparison rubric from the confirmed outcome, constraints, and
  success criteria before scoring. Give every viable option equivalent
  decision-relevant validation:
  - for UI/UX differences, show each materially different experience;
  - for architecture or workflow differences, show each relevant flow;
  - for interaction-heavy or technical claims, use comparable short-lived
    prototypes or measurements when static evidence cannot decide them.
  Equivalent checks do not require equal implementation effort. Label observed
  facts, proposed behavior, and prototype-only shortcuts.
- If an option reveals an evidence gap, run the same relevant check for every
  affected option. When that is impossible, disclose the unequal evidence and
  reduce confidence before comparison.
- Keep prototypes local and disposable, retain only reviewable captures and
  findings, and remove them when the grill concludes. Do not deploy or publish
  them. Preserve or promote a prototype only with explicit user approval; keep
  approved prototype material outside the selected worktree until its
  `git-workflow` synchronization gate passes.
- Compare two to four genuinely distinct viable options against the declared
  rubric, then recommend one and wait for the user's choice unless automatic
  mode applies. Include a compact evidence digest with the most relevant source
  links, checked project versions, scores, and remaining uncertainty.
- For any code or test change, complete the project gate, one bounded
  `/code-review fix-all`, and `/create-pr`.

## Project definitions

Treat the nearest applicable `DEFINITION_OF_READY.md` and
`DEFINITION_OF_DONE.md` as canonical. If either is absent after a real search,
state that once, carry the gap into the plan and PR, and use only the workflow's
built-in checks plus commands the repository actually declares.

## Automatic mode

Automatic mode applies only when the original prompt says `automatic` or
explicitly requests it. It does not waive any gate. Do not select an option until
the shared packet, predeclared rubric, and equivalent checks are complete. If a
comparison remains incomplete, report the gap and confidence impact rather than
treating the initial preference as validated. A material product, architecture,
security, scope, or vision decision disables automatic mode.

## Workflow

1. Draft the provisional mini-spec in at most 400 characters, including spaces.
   Treat it as a starting point, not a confirmed contract.
2. Load the applicable project contracts and specialized skills.
3. Find the existing extension point, reusable patterns, sibling features,
   tests, and architectural constraints. Always perform current web search and
   reconcile its results with the checkout. If the idea already exists,
   recommend using it; if partial, extend rather than duplicate it.
4. Build the neutral evidence packet and comparison rubric. Run the grill with
   that shared context, confirm the final mini-spec, derive the options, and
   validate each option with equivalent decision-relevant checks. Compare first;
   recommend only afterward.
5. After selection, reuse or create a focused branch/worktree and apply the
   `git-workflow` remote-default synchronization gate before editing.
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
