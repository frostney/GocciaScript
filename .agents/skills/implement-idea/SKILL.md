---
name: implement-idea
description: >-
  Turns a raw idea or feature request (no GitHub issue) into a confirmed
  mini-spec by asking follow-up questions about scope, desired outcome, and
  success criteria, then implements it with the same rigor as /implement-issue:
  reads AGENTS.md/area context, VISION.md when present, and applicable
  stack/conventions skills, runs a full-context investigation, invokes the grill
  skill when registered, presents exactly three options, and stops for the
  user's choice before coding unless automatic mode was requested, updates the
  branch against origin's default branch, implements the chosen option,
  searches project context for DEFINITION_OF_READY.md before implementation,
  runs verification (UI/UX checks plus the project's full check gate), reviews
  against the spec's success criteria and project-context DEFINITION_OF_DONE.md
  before handoff, runs a /review code review, and always hands off via
  /create-pr. Use when the user runs /implement-idea or wants to build something
  that is not an existing issue.
license: Unlicense OR MIT
compatibility: >-
  Requires git and the GitHub CLI (gh) for the /create-pr handoff, plus network
  access; verification is driven by the project's DEFINITION_OF_DONE.md and its
  own declared commands rather than any assumed toolchain.
---

# Implement idea

## Instructions

Turn a raw idea into a confirmed mini-spec, then implement it in the current repository. This is `/implement-issue` without a GitHub issue — the idea-formulation phase (step 2) produces the spec that an issue would otherwise provide.

### Non-negotiable gates (do not skip, do not rationalize)

These five gates are mandatory. Most failures of this skill come from skipping one of them under time pressure or because the idea "looks simple." Being an implementation command is **not** a license to skip any gate.

1. **GATE A — Formulate and confirm the idea before anything else.** You **must** complete the questioning in step 2 (scope, outcome, success criteria — shaped like a well-structured issue, borrowing `/create-issue`'s components) and get the user's explicit confirmation of the written mini-spec before investigating, grilling, or planning. Do not start designing from a vague one-liner.
2. **GATE B — Grill before planning.** When a grill skill is registered, you **must** invoke it (step 5) before presenting options. Do not proceed to options without it.
3. **GATE C — Full-context investigation before concluding.** You **must** complete the investigation in step 4 (enumerate the project's real commands, trace the relevant code paths) before forming any conclusion. A conclusion drawn from a single file or a guessed command is invalid.
4. **GATE D — Wait for the user's choice unless automatic mode was requested.** After presenting options (step 7) you **must stop and wait** for the user to pick one. The only exception is automatic mode: when the user's original prompt includes the standalone word `automatic` or explicitly asks for automatic mode, present the options, auto-select the recommendation based on the project context, state why, and continue.
5. **GATE E — Code review, then hand off (do not stop early).** Whenever there is a code or test change to ship, you **must** run a code review via `/review` (step 15) and then invoke `/create-pr` (step 16). Finishing by only summarizing the change in chat, without running the review and opening the PR, is a failure of this skill.

**Forbidden rationalizations** — if you catch yourself writing any of these, you are violating the skill, stop and follow the gate instead:

- ❌ "The idea is clear enough, I'll just start building." → No. Formulate scope, outcome, and success criteria and get confirmation first (GATE A).
- ❌ "Since `/implement-idea` is an implementation command, I'll proceed with option A." → No. The command requires you to wait for the choice. Implementation is what happens *after* the user picks.
- ❌ "The user said 'Agreed' / 'go ahead' / 'sounds good' before I ran grill and showed options." → No. That only counts as implementation approval if it comes **after** the required grill/options gate. To skip the gate entirely, the user must explicitly say something like: "skip the implement-idea gate and code directly."
- ❌ "The choice is obvious, so I'll skip the question." → No. Present options and wait unless the original prompt requested `automatic` mode. The user may know constraints you don't.
- ❌ "Grill isn't strictly necessary here." → No. If it's registered, run it.
- ❌ "I treated `grill-with-docs` as 'answer with doc-grounding' instead of actually running the grilling loop." → No. Invoking grill means **executing the grill skill itself** — its real, interactive question loop — not answering in a doc-grounded style, not paraphrasing what it would ask. Load the skill and run it.
- ❌ "I'll ask a couple of clarifying questions of my own; that's basically grilling." → No. That is not the grill skill. Run the actual `grill-with-docs` / `grill-me` skill.
- ❌ "The change looks complete, I'll summarize it here instead of opening a PR." → No. Run `/review`, then always invoke `/create-pr` (GATE E).
- ❌ "I already self-reviewed, so I'll skip the `/review` code review." → No. The `/review` pass is a separate, mandatory code review of the diff.

If you are a smaller / non-frontier model: treat steps 2, 4, 5, and 7 as literal hard stops, and treat steps 15 and 16 as mandatory always-run endings. Ask the questions, finish the checklist, run the tool, ask the question, then wait — and at the end, run the review and open the PR.

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

### Use the grill skill for thoroughness (always when available)

Before presenting options, the agent **always invokes the grill skill** when it is registered in this environment — not only when something is ambiguous. This is GATE B above. The grill output is folded into the implementation plan and verification so the result is more thorough than the confirmed mini-spec alone would produce.

**Invoking the grill skill means literally running that skill — not imitating its spirit.** `grill-with-docs` and `grill-me` are separate skills with their own multi-question interrogation loop. To invoke one you **read its `SKILL.md` and execute its procedure**: actually ask the user the grilling questions it generates and wait for the answers, iterating until the loop completes. The following are **NOT** invoking it and are forbidden substitutes:

- Treating the mention of `grill-with-docs` as a style instruction — "answer with doc-grounding," "be thorough," "cite the docs" — and then proceeding. ❌
- Summarizing or paraphrasing the questions grilling *would* ask instead of asking them. ❌
- Asking one or two clarifying questions of your own and calling that grilling. ❌
- Skipping it because you believe you already understand the idea. ❌

If you cannot run the grill skill, do not silently downgrade it to "doc-grounded answering" — say explicitly that no grill skill was found (see discovery hint) and proceed on the input as given.

- **`/grill-with-docs` is preferred.** Use `/grill-me` only when `/grill-with-docs` is not registered.
- Discovery hint: look for a skill or command named `grill-with-docs` or `grill-me` (e.g. `~/.cursor/skills/grill-with-docs/`, `~/.cursor/skills/grill-me/`, `.cursor/skills/...`, `.agents/skills/...`).
- If neither is registered, state explicitly that no grill skill was found, then proceed with the workflow on the input as given.

When this workflow invokes an external grill skill, treat it as a nested planning dependency: obey that skill's procedure exactly, do not implement product code during the grill sub-session unless that skill explicitly requires documentation/context updates, then return to this workflow and continue with validation and options.

### Active context declaration

Do not declare the active context at invocation time. First confirm the mini-spec, read project/area context, discover matching skills, investigate the relevant code paths, and run the grill skill when available. Then, immediately before presenting implementation options, briefly state the context and skills that are now active, for example:

```text
Active context before planning: AGENTS.md, project-area/AGENTS.md, project-area/CONTEXT.md, docs/adr/0003-..., react-stack, convex, grill-with-docs. No matching <domain> skill found.
```

This is a gate immediately before options: actively search for relevant project, stack, domain, and grill context before the declaration. If the declaration is missing any discovered context, load it before continuing. A context note before grill does not satisfy this gate; the active context must be restated with the implementation options.

### Pre-edit approval checklist

Before any file edit or implementation command, run this checklist out loud in commentary:

```text
Gate check: I have not edited files yet. I'm running the grill/options step first.
```

Then confirm all five items are true:

- Grill-with-docs / grill-me has run, or the user explicitly waived it.
- The project-context Definition of Ready search is complete, every discovered Definition of Ready has been read, and the selected path satisfies every applicable readiness item before any file edit. If no `DEFINITION_OF_READY.md` was found, the **DoR/DoD absence protocol** has been applied (flagged, not silently passed over).
- Exactly three implementation options were presented.
- One option was recommended.
- The user explicitly selected an option, explicitly approved the recommendation **after** seeing those options, or the original prompt requested `automatic` mode and the recommendation was auto-selected with a project-context rationale.

If any item is missing, stop. Ask for the missing grill step, Definition of Ready search/readiness resolution, options gate, recommendation, or post-options approval before editing. Generic approval language such as "Agreed", "go ahead", "sounds good", or equivalent does not satisfy this checklist unless it follows the three presented options. The only ways to bypass waiting for post-options approval are an explicit user instruction such as: "skip the implement-idea gate and code directly," or `automatic` mode in the original prompt.

### Automatic mode

Automatic mode is opt-in. It is active only when the user's original `/implement-idea` prompt includes the standalone word `automatic` or explicitly asks for automatic mode.

In automatic mode, do **not** skip idea formulation, spec confirmation, investigation, grill, readiness checks, active context declaration, or the exactly-three-options presentation. After presenting the three options, select the recommended option yourself based on the confirmed mini-spec, `VISION.md` when present, Definition of Ready, stack/conventions skills, project architecture, risk, and verification cost. State the selected option and why it best fits the project context, then continue without waiting for the user's choice.

If the best option is unclear, materially risky, or conflicts with `VISION.md`, automatic mode does not apply: stop and ask the user for clarification.

### Definition of Ready / Definition of Done are canonical (absence protocol)

`DEFINITION_OF_READY.md` and `DEFINITION_OF_DONE.md` (and the case/spelling variants searched in step 3) are the **canonical truth** for when this work may start and when it is shippable. When present, every applicable item is a hard gate — do not invent your own readiness or completion bar to replace them.

**DoR/DoD absence protocol — referenced by the steps below.** If either file is missing after the mandatory search, do **not** silently proceed and do **not** invent a substitute:

- State prominently to the user that no project-context `DEFINITION_OF_READY.md` / `DEFINITION_OF_DONE.md` was found, so readiness / completion cannot be checked against a project definition, and recommend adding one.
- Carry the flagged absence into the active context declaration and the PR description so it is visible to reviewers.
- Continue with the workflow's built-in readiness/review checks and only the project's actually-declared commands — never a guessed or stack-assumed gate.

### Steps

1. Capture the raw idea from the user. If no idea was provided, ask for it.
2. **Formulate and confirm the idea (GATE A).** Ask focused follow-up questions across **scope**, **outcome**, and **success criteria**, shaping the spec with `/create-issue`'s good-issue components (see "Formulating the idea" above). Iterate until all three are pinned down, then write the mini-spec back as a short Scope / Outcome / Success-criteria block and get the user's explicit confirmation. Do not investigate or plan until the spec is confirmed. If the user cannot commit to success criteria, surface that as a risk and agree on a provisional definition of done.
3. **Read the project's agent context before forming a hypothesis or editing.** In this order:
   - The **root** `AGENTS.md`.
   - **Vision document (mandatory search).** Search for `VISION.md` at the repository root and in relevant product/docs areas. Read every discovered vision document and use it while shaping the mini-spec and options. If the idea asks for behavior contrary to the stated product or technical vision, call out the conflict explicitly and ask the user whether to revise the idea, override the vision for this work, or abandon the change before investigating, planning, or editing.
   - **Agent-alias files (mandatory search).** Search for `CLAUDE.md` and equivalent root agent aliases. Read each discovered alias and confirm whether it is a symlink/alias to `AGENTS.md` or contains additional instructions. Carry any additional instructions into the active context declaration.
   - The **nearest** `<area>/AGENTS.md` to the files the idea touches in a multi-area repo. Nested files override the root for that area.
   - **Contribution rules (mandatory search).** Search the root, nearest affected area, `docs/`, and any `AGENTS.md`-referenced context for `CONTRIBUTING.md` or equivalent contribution/review guidance. Read every match. Treat the nearest/most specific match as authoritative for what may be merged, and carry all matches into the active context declaration.
   - **Project-context Definition files (mandatory search).** Search the root, nearest affected area, `docs/`, and any `AGENTS.md`-referenced context for `DEFINITION_OF_READY.md`, `DEFINITION_OF_DONE.md`, and spelling/case variants such as `Definition of Ready`, `Definition-of-Done`, `definition_of_ready`, or `defintion_of_done`. Read every match. Treat the nearest/most specific match as authoritative for the affected area, and carry all matches into the active context declaration.
   - The repo's `docs/` index (`docs/README.md`, `docs/architecture.md`, `docs/code-style.md`, or equivalent) for the area being changed.
   Do not skip this step even when the idea looks small — Hard Constraints sections (e.g. "Bun only, no npm/pnpm/yarn", "AI SDK via Vercel AI Gateway only") frequently change the implementation path.

   **Discover and use implementation-specific skills and context (required).** Before planning, check what specialized skills and context apply to the area being changed, and use them:
   - **Registered skills.** Look for skills that match the project's stack or domain (e.g. a stack skill like `react-stack` / `native-nostalgia-stack`, a conventions skill like `convex-conventions`, or any project-provided skill in `.agents/skills/` ↔ `.claude/skills/`). When one matches the change, read it and follow it — its rules override generic defaults for that area.
   - **In-repo context.** Honor area-specific `docs/` (e.g. `docs/code-style.md`, a feature MVP doc), `.cursor/rules`, and any `AGENTS.md`-referenced guides for the files you're touching.
   - If a clearly relevant skill or context exists, using it is **not optional**. Implementing without consulting an applicable stack/conventions skill is a shortcut (see step 14). If none applies, note that briefly and proceed.

4. **Investigate the whole codebase before concluding (GATE C). Do not take shortcuts.** A conclusion drawn from a single file or an assumed command is invalid. Before forming any plan:
   - **Enumerate the project's real commands instead of guessing.** Read the manifest's script section (`package.json` `scripts`, `Makefile` / `Justfile` / `Taskfile`, `pyproject.toml`, `Cargo.toml`, etc.) and the `docs/tooling.md` / `docs/quick-start.md` equivalents. Use the commands the project actually defines (e.g. the project's `check`, `test`, `lint`, `dev` names) — never invent a command or assume a default that the repo hasn't declared.
   - **Search broadly, not narrowly.** Use codebase search and grep to find where the idea fits: existing patterns to reuse, the modules and layers it touches, sibling features, config, and tests. Read the surrounding modules, not just the first match.
   - **Assess feasibility against the confirmed spec.** Map each part of the mini-spec to where it would live in the codebase and flag anything the current architecture makes hard.
   - If after a genuine investigation something is still unclear, that is a finding to raise in the options — not a reason to guess.

   **If the idea (or a close variant) already exists, do not rebuild it.** Surface that and carry it into the options (step 7):
   - **Case: already implemented and covered.** Point to the existing implementation and its tests. The recommended outcome is to use it as-is or close the idea as already covered, not to write duplicate code.
   - **Case: partially implemented.** Identify what exists versus what the spec still needs; the work becomes extending the existing implementation, not starting fresh.
   - Confirm any "already exists" conclusion with evidence (the responsible code and tests) before presenting it.
5. **Run the grill skill (GATE B).** When `grill-with-docs` / `grill-me` is registered, **read that skill and execute its actual question loop now** on the confirmed mini-spec plus your investigation findings — ask the questions, wait for answers, iterate to completion — then fold its output into the options and verification plan. Provide the grill skill with the project, stack, domain, docs, ADR, and investigation context discovered in steps 3–4. Do not substitute a "doc-grounded" answer or your own ad-hoc questions for the skill. Do not implement product code during the grill sub-session unless the grill skill explicitly requires documentation/context updates. If no grill skill is registered, say so explicitly and continue.
6. Validate before coding:
   - The mini-spec is confirmed, the scope is bounded, and the success criteria are clear enough to verify against.
   - The idea is not already fully implemented (per step 4).
   - The project-context Definition of Ready search from step 3 is complete and documented in the active context. The confirmed mini-spec, investigation findings, and selected implementation path satisfy every applicable Definition of Ready item before branching or editing. Any unmet readiness item is a hard stop: resolve it with the user before implementation.
   - Absence protocol: if the mandatory search finds no Definition of Ready, apply the **DoR/DoD absence protocol** (see the section above the steps).
   - If the spec is still ambiguous or the scope keeps growing, stop and return to step 2.
7. **Declare active context, present implementation options, then STOP and wait unless automatic mode applies (GATE D).** Before listing options, state the active context/skills discovered and used in steps 3–5, including any applicable project, vision, stack, domain, docs, ADR, and grill skills. If the declaration reveals a relevant missing skill or context file, load it before continuing. Then present exactly three distinct options for delivering the idea, with tradeoffs, a verification plan tied to the success criteria, and a recommendation grounded in the step 4 investigation and step 5 grill output. The three must be genuinely different approaches (e.g. scope/architecture/effort tradeoffs), not trivial variations of one. When step 4 found the idea **already implemented**, the options reflect that instead of inventing duplicate code — e.g. (a) use/close as already covered, (b) extend the existing implementation to meet the remaining spec, (c) a thin alternative that reuses the existing code. **Do not write any implementation code until the user explicitly picks an option, explicitly tells you to proceed with the recommendation, or automatic mode auto-selects the recommendation.** Do not interpret "this is an implementation command" as permission to skip the choice. If automatic mode applies, state the auto-selected option and why it best fits the project context, then continue; otherwise, end your turn here and wait for the user's reply.
   *If the chosen option is "already covered" with no code or test change, skip steps 8–16: report the existing implementation and stop. The remaining steps apply only when there is a code or test change to ship.*

8. Branch / worktree:
   - Prefer reusing an existing focused branch or worktree for the idea.
   - If a branch exists without a worktree, use or create a worktree when that best isolates the work.
   - Otherwise create a focused branch named from the idea (e.g. `idea-short-slug`); use a worktree when practical.
   - **Update the branch/worktree against the latest baseline before implementing.** Run `git fetch origin`, then merge the remote default branch into the working branch (e.g. `git merge origin/<default-branch>` — never rebase, per the `git-workflow` skill). Resolve any conflicts and commit the merge before writing new code, so the work starts from the current `origin` main.
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
15. **Run a code review before handoff (GATE E, do not skip).** Invoke the `/review` skill/command on the change (the branch diff) and address its findings before opening the PR. This is a separate, fresh review of the diff — distinct from your own step-14 self-review.
    - Discovery hint: look for a skill or command named `review` / `code-review` (e.g. `~/.cursor/skills/review/`, `.cursor/skills/...`, `.agents/skills/...`). Run the actual skill; do not substitute a self-summary for it.
    - Fix every issue it surfaces and re-run the relevant verification (step 11/13) before proceeding. Do not defer findings to "a follow-up" unless the user explicitly agrees.
    - If no `/review` skill is registered, say so explicitly, then perform a thorough manual diff review covering correctness, security, error handling, tests, and style.
16. **Hand off via `/create-pr` (GATE E, mandatory).** Always invoke `/create-pr` to commit, push, and open the draft pull request — this is the required end of the workflow whenever there is a code or test change to ship. The commit created during this handoff must use a Conventional Commit subject (`type(scope): summary`). Do not end the turn by only summarizing the change in chat; the opened PR is the deliverable. Summarize the idea and its success criteria in the PR body. There is no issue to close.
