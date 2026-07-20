---
name: implement-idea
description: >-
  Turns an unfiled idea into a confirmed mini-spec, implements and validates the
  selected approach, reviews it, and opens a draft pull request. Use when the
  user runs /implement-idea or asks to build something without an existing issue.
license: Unlicense OR MIT
compatibility: >-
  Requires git and the GitHub CLI (gh) for the /create-pr handoff, plus network
  access; verification is driven by the project's DEFINITION_OF_DONE.md and its
  own declared commands rather than any assumed toolchain.
---

# Implement idea

## Instructions

Turn a raw idea into a confirmed mini-spec, then implement it in the current repository. This is `/implement-issue` without a GitHub issue — the idea-formulation phase (step 2) produces the spec that an issue would otherwise provide.

### Non-negotiable gates

1. **Formulate and confirm the idea.** Establish scope, outcome, and success
   criteria as a mini-spec before investigation or planning.
2. **Investigate before concluding.** Read the real project commands, relevant
   instructions, affected code paths, tests, and existing implementations.
3. **Run the grill skill when registered.** Use `grill-with-docs`, falling back
   to `grill-me`, and complete its actual user-question loop before presenting
   options. Ad-hoc questions are not a substitute. If neither is registered,
   note that once and continue.
4. **Present two to four real options and get a decision.** Recommend one and
   wait for the user's post-options choice unless automatic mode applies.
5. **Review and hand off.** For any code or test change, run `/review`, address
   validated in-scope findings, and finish through `/create-pr`.

### Formulating the idea

The idea-formulation phase (step 2) is what makes this skill different from `/implement-issue`. Its job is to convert a vague idea into a concrete, confirmed mini-spec that is good enough to implement against and to verify against later. Drive it with focused follow-up questions across three axes:

- **Scope.** What is in scope and — just as important — what is explicitly out of scope (non-goals)? What constraints apply (tech, deadlines, compatibility, data)? How big should this first cut be (MVP vs full)?
- **Outcome.** What does the end state look like? Who is the user and what problem does this solve for them? What is the desired behavior / UX / API surface? What changes for the user once it ships?
- **Success criteria.** How will we know it is done and working? What are the acceptance criteria, and which of them are testable / measurable? What would prove the idea succeeded versus merely "ran"?

Ask only the questions that are actually open — do not interrogate the user about things they already stated. Iterate until the three axes are pinned down, then write the mini-spec back to the user (a short Scope / Outcome / Success criteria block) and get explicit confirmation before proceeding. This is separate from the grill skill: formulation establishes the spec; grilling (step 5) sharpens the plan against it.

**Shape the mini-spec like a well-structured issue.** The `/create-issue` skill defines what a good issue contains; borrow those components so the formulated spec is as implementable as one that went through the proven `create-issue` → `implement-issue` loop:

- A specific, plain-language title — the idea in one line.
- A short problem statement: what is missing or wrong today.
- Current vs desired behavior, with a concrete example or minimal sample where it helps.
- Project context (related work, prior art, spec/RFC, `VISION.md`, related items) when relevant.
- User impact and which work it unblocks.
- Likely affected area, scope notes, constraints, and non-goals.
- For UI/UX ideas, also: affected screens/routes/components, current and expected visual state, accessibility expectations (keyboard, focus, ARIA, contrast, motion), responsive scope and themes, and the design system / tokens involved — mirroring `/create-issue`'s UI/UX checklist.

Map these onto the three axes: title + problem + current/desired behavior → **outcome**; affected area + constraints + non-goals → **scope**; acceptance examples + user-visible signals → **success criteria**.

### Planning checkpoint

After investigation and grilling, give one concise phase update naming the
material constraints and findings, then present the options. Do not narrate an
internal compliance checklist. Do not edit until the applicable Definition of
Ready is satisfied and the option gate has produced a selected path.

### Automatic mode

Automatic mode is opt-in. It is active only when the user's original `/implement-idea` prompt includes the standalone word `automatic` or explicitly asks for automatic mode.

In automatic mode, do **not** skip formulation, spec confirmation, investigation, grilling, readiness checks, the planning checkpoint, or the options presentation. After presenting two to four genuinely distinct options, select the recommendation from the mini-spec, project context, risk, and verification cost, state why, and continue without waiting.

If the best option is unclear, materially risky, or conflicts with `VISION.md`, automatic mode does not apply: stop and ask the user for clarification.

### Definition of Ready / Definition of Done are canonical (absence protocol)

`DEFINITION_OF_READY.md` and `DEFINITION_OF_DONE.md` (and the case/spelling variants searched in step 3) are the **canonical truth** for when this work may start and when it is shippable. When present, every applicable item is a hard gate — do not invent your own readiness or completion bar to replace them.

**DoR/DoD absence protocol — referenced by the steps below.** If either file is missing after the mandatory search, do **not** silently proceed and do **not** invent a substitute:

- State prominently to the user that no project-context `DEFINITION_OF_READY.md` / `DEFINITION_OF_DONE.md` was found, so readiness / completion cannot be checked against a project definition, and recommend adding one.
- Carry the flagged absence into the planning checkpoint and PR description.
- Continue with the workflow's built-in readiness/review checks and only the project's actually-declared commands — never a guessed or stack-assumed gate.

### Steps

1. Capture the raw idea from the user. If no idea was provided, ask for it.
2. **Formulate and confirm the idea.** Ask focused follow-up questions across **scope**, **outcome**, and **success criteria**, shaping the spec with `/create-issue`'s good-issue components. Write the result back as a short mini-spec and get explicit confirmation before investigating or planning.
3. **Read the project's agent context before forming a hypothesis or editing.** In this order:
   - The **root** `AGENTS.md`.
   - **Vision document (mandatory search).** Search for `VISION.md` at the repository root and in relevant product/docs areas. Read every discovered vision document and use it while shaping the mini-spec and options. If the idea asks for behavior contrary to the stated product or technical vision, call out the conflict explicitly and ask the user whether to revise the idea, override the vision for this work, or abandon the change before investigating, planning, or editing.
   - **Agent-alias files (mandatory search).** Search for `CLAUDE.md` and equivalent root agent aliases. Read each discovered alias and apply any additional instructions.
   - The **nearest** `<area>/AGENTS.md` to the files the idea touches in a multi-area repo. Nested files override the root for that area.
   - **Contribution rules (mandatory search).** Search the root, nearest affected area, `docs/`, and any `AGENTS.md`-referenced context for `CONTRIBUTING.md` or equivalent guidance. Read every match and apply the nearest relevant rules.
   - **Project-context Definition files (mandatory search).** Search for `DEFINITION_OF_READY.md`, `DEFINITION_OF_DONE.md`, and spelling/case variants in the root, affected area, and docs. Read every match and apply the nearest relevant definition.
   - The repo's `docs/` index (`docs/README.md`, `docs/architecture.md`, `docs/code-style.md`, or equivalent) for the area being changed.
   Do not skip this step even when the idea looks small — Hard Constraints sections (e.g. "Bun only, no npm/pnpm/yarn", "AI SDK via Vercel AI Gateway only") frequently change the implementation path.

   **Discover and use implementation-specific skills and context (required).** Before planning, check what specialized skills and context apply to the area being changed, and use them:
   - **Registered skills.** Look for skills that match the project's stack or domain (e.g. a stack skill like `react-stack` / `native-nostalgia-stack`, a conventions skill like `convex-conventions`, or any project-provided skill in `.agents/skills/` ↔ `.claude/skills/`). When one matches the change, read it and follow it — its rules override generic defaults for that area.
   - **In-repo context.** Honor area-specific `docs/` (e.g. `docs/code-style.md`, a feature MVP doc), `.cursor/rules`, and any `AGENTS.md`-referenced guides for the files you're touching.
   - If a clearly relevant skill or context exists, using it is **not optional**. Implementing without consulting an applicable stack/conventions skill is a shortcut (see step 14). If none applies, note that briefly and proceed.

4. **Investigate before concluding.** A conclusion drawn from a single file or an assumed command is invalid. Before forming a plan:
   - **Enumerate the project's real commands instead of guessing.** Read the manifest's script section (`package.json` `scripts`, `Makefile` / `Justfile` / `Taskfile`, `pyproject.toml`, `Cargo.toml`, etc.) and the `docs/tooling.md` / `docs/quick-start.md` equivalents. Use the commands the project actually defines (e.g. the project's `check`, `test`, `lint`, `dev` names) — never invent a command or assume a default that the repo hasn't declared.
   - **Search broadly, not narrowly.** Use codebase search and grep to find where the idea fits: existing patterns to reuse, the modules and layers it touches, sibling features, config, and tests. Read the surrounding modules, not just the first match.
   - **Assess feasibility against the confirmed spec.** Map each part of the mini-spec to where it would live in the codebase and flag anything the current architecture makes hard.
   - If after a genuine investigation something is still unclear, that is a finding to raise in the options — not a reason to guess.

   **If the idea (or a close variant) already exists, do not rebuild it.** Surface that and carry it into the options (step 7):
   - **Case: already implemented and covered.** Point to the existing implementation and its tests. The recommended outcome is to use it as-is or close the idea as already covered, not to write duplicate code.
   - **Case: partially implemented.** Identify what exists versus what the spec still needs; the work becomes extending the existing implementation, not starting fresh.
   - Confirm any "already exists" conclusion with evidence (the responsible code and tests) before presenting it.
5. **Run the grill gate.** Give the grill skill the mini-spec plus the material project and investigation context, complete its loop, and fold the result into every option and its verification plan. Do not implement product code during grilling unless that skill explicitly requires a context update.
6. Validate before coding:
   - The mini-spec is confirmed, the scope is bounded, and the success criteria are clear enough to verify against.
   - The idea is not already fully implemented (per step 4).
   - The Definition of Ready search from step 3 is complete, and the mini-spec, evidence, and selected path satisfy every applicable item before editing.
   - Absence protocol: if the mandatory search finds no Definition of Ready, apply the **DoR/DoD absence protocol** (see the section above the steps).
   - If the spec is still ambiguous or the scope keeps growing, stop and return to step 2.
7. **Give the planning checkpoint and present options.** Present the smallest set of two to four genuinely distinct approaches; never pad or collapse the real option space. For each, give its tradeoffs and a success-criteria-based verification plan, incorporate the grill findings, and recommend one from the investigation evidence. If the idea already exists, options should reflect using it, extending only what is missing, or a genuinely distinct thin alternative. Do not edit until the user makes a post-options choice or automatic mode selects the recommendation; otherwise stop here.
   *If the chosen option is "already covered" with no code or test change, skip steps 8–16: report the existing implementation and stop. The remaining steps apply only when there is a code or test change to ship.*

8. Branch / worktree:
   - Prefer reusing an existing focused branch or worktree for the idea.
   - If a branch exists without a worktree, use or create a worktree when that best isolates the work.
   - Otherwise create a focused branch named from the idea (e.g. `idea-short-slug`); use a worktree when practical.
   - **Update the branch/worktree against the latest baseline before implementing.** Run `git fetch origin`, then merge the remote default branch into the working branch (e.g. `git merge origin/<default-branch>` — never rebase, per the `git-workflow` skill). Resolve any conflicts and commit the merge before writing new code, so the work starts from the current remote base.
9. Implement the smallest complete change that satisfies the chosen approach and the confirmed success criteria.
10. Update tests and documentation per the contribution guidance discovered in step 3 (`CONTRIBUTING.md`, `AGENTS.md`, or equivalent). Absence protocol: after the mandatory search finds no contribution guidance, state that no project contribution guidance was found and follow the confirmed spec, Definition of Ready, Definition of Done, and local code patterns.
11. Run targeted verification first (focused tests, types, lint) on the changed area, then broader verification when the change has wider impact.
12. **If the change is UI/UX, rendering and visual evidence are mandatory (do not skip).** A UI/UX change handed off without screenshots of the actual rendered result is incomplete:
    - Run the app (or Storybook / component sandbox) and load every affected screen, component, and state — never assert the UI is correct without rendering it.
    - Capture before/after screenshots (or short recordings) of each affected screen and state, at the project's supported breakpoints and across light/dark/system themes when applicable.
    - Compare the captures against the outcome described in the confirmed spec, and fix any discrepancy before handoff.
    - Verify accessibility: keyboard navigation and focus order, visible focus styles, ARIA roles/labels for new interactive elements, color contrast meeting WCAG AA (or the project's standard), and `prefers-reduced-motion` respected for animations.
    - Reuse existing design-system components and tokens; do not introduce one-off styles for primitives that already exist.
    - **Attach the screenshots/recordings and accessibility notes to the PR — this is mandatory, not optional.** The PR must be fully reviewable from these artifacts alone, without re-running the app, so the change can be judged asynchronously. A UI/UX PR missing this visual evidence is not ready for `/create-pr`.
13. **Run the project's full verification gate before invoking `/create-pr`.** The gate is defined by the project-context `DEFINITION_OF_DONE.md` together with the project's aggregator script and actually-enumerated real commands (e.g. the project's `check`) — these are the canonical "ready to commit" signal. Run every verification the Definition of Done requires, using only the commands the repo actually declares. Do **not** invent commands or assume a stack: if a verification the Definition of Done requires has no corresponding command in the repo, that is a finding to raise with the user, not a command to make up.

    Absence protocol — no `DEFINITION_OF_DONE.md`: apply the **DoR/DoD absence protocol** (see the section above the steps) — run only the project's actually-declared commands; do not substitute an invented gate.

    Do not skip steps because they "should pass." If any step fails, fix the cause; do not invoke `/create-pr` with a red gate.

14. **Review the implementation before handoff (do not skip).** After the gate is green but before `/create-pr`, audit your own change critically — as a reviewer who did not write it would:
    - **Check against the spec, criterion by criterion.** Walk each success criterion in the confirmed mini-spec and confirm the change actually satisfies it, citing the code or test that does so; confirm the delivered scope and outcome match what was confirmed. Anything unmet is unfinished work, not a follow-up. When the scope was deliberately changed across later turns or grill sessions, match that updated intent instead — and note the divergence from the original idea in the PR description so reviewers understand why.
    - **Check the Definition of Done.** Re-read every project-context Definition of Done discovered in step 3 before handoff. Verify the implementation, tests, documentation, review evidence, and handoff artifacts satisfy every applicable item. Any unmet completion item is a hard stop: fix it before `/review` or `/create-pr`, or stop and get explicit user agreement that the item is out of scope. Absence protocol: if no Definition of Done was found, apply the **DoR/DoD absence protocol** (see the section above the steps).
    - **No shortcuts.** No stubbed logic, hardcoded values standing in for real behavior, `TODO`/`FIXME` left behind, swallowed errors, skipped/`.only`/commented-out tests, or "happy path only" handling of cases the spec requires. Confirm the real layer was built, not a façade.
    - **No tech debt introduced.** No dead code, no duplication that should be extracted, no copy-paste of an existing pattern that has a shared helper, no weakened types (`any`, unsafe casts) or loosened lint/type rules to make the gate pass, no leftover debug output.
    - **Consistency.** The change follows the repo's conventions (from step 3 agent context and `docs/code-style.md`) and reuses existing components/utilities rather than reinventing them.
    - If this review surfaces a problem, fix it and re-run the relevant verification (step 11/13) before proceeding. Do not defer found issues to "a follow-up" unless the user explicitly agrees.
15. **Run a code review before handoff.** Invoke the `/review` skill/command on the branch diff before opening the PR. This is a separate, fresh review from step 14.
    - Discovery hint: look for a skill or command named `review` / `code-review` (e.g. `~/.cursor/skills/review/`, `.cursor/skills/...`, `.agents/skills/...`). Run the actual skill; do not substitute a self-summary for it.
    - Validate its findings, fix every in-scope issue, and re-run the relevant verification (step 11/13). Surface scope-expanding findings rather than silently widening the change.
    - If no `/review` skill is registered, say so explicitly, then perform a thorough manual diff review covering correctness, security, error handling, tests, and style.
16. **Hand off via `/create-pr`.** For any code or test change, use `/create-pr` to commit, push, and open the draft pull request. Summarize the idea, success criteria, and verified evidence in the PR body.
