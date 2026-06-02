---
name: implement-idea
description: >-
  Turns a raw idea or feature request (no GitHub issue) into a confirmed
  mini-spec by asking follow-up questions about scope, desired outcome, and
  success criteria, then implements it with the same rigor as /implement-issue:
  reads AGENTS.md/area context and applicable stack/conventions skills, runs a
  full-context investigation, invokes the grill skill when registered, presents
  exactly three options and stops for the user's choice before coding,
  implements the chosen option, runs verification (UI/UX checks plus the
  project's full check gate), reviews for shortcuts and tech debt, and prepares
  a draft PR via /create-pr. Use when the user runs /implement-idea or wants to
  build something that is not an existing issue.
license: Unlicense OR MIT
compatibility: >-
  Requires git and the GitHub CLI (gh) for the /create-pr handoff, plus network
  access; verification examples assume a Bun toolchain.
---

# Implement idea

## Instructions

Turn a raw idea into a confirmed mini-spec, then implement it in the current repository. This is `/implement-issue` without a GitHub issue — the idea-formulation phase (step 2) produces the spec that an issue would otherwise provide.

### Non-negotiable gates (do not skip, do not rationalize)

These four gates are mandatory. Most failures of this skill come from skipping one of them under time pressure or because the idea "looks simple." Being an implementation command is **not** a license to skip any gate.

1. **GATE A — Formulate and confirm the idea before anything else.** You **must** complete the questioning in step 2 (scope, outcome, success criteria — shaped like a well-structured issue, borrowing `/create-issue`'s components) and get the user's explicit confirmation of the written mini-spec before investigating, grilling, or planning. Do not start designing from a vague one-liner.
2. **GATE B — Grill before planning.** When a grill skill is registered, you **must** invoke it (step 5) before presenting options. Do not proceed to options without it.
3. **GATE C — Full-context investigation before concluding.** You **must** complete the investigation in step 4 (enumerate the project's real commands, trace the relevant code paths) before forming any conclusion. A conclusion drawn from a single file or a guessed command is invalid.
4. **GATE D — Wait for the user's choice.** After presenting options (step 7) you **must stop and wait** for the user to pick one. Do **not** continue to implementation on your own.

**Forbidden rationalizations** — if you catch yourself writing any of these, you are violating the skill, stop and follow the gate instead:

- ❌ "The idea is clear enough, I'll just start building." → No. Formulate scope, outcome, and success criteria and get confirmation first (GATE A).
- ❌ "Since `/implement-idea` is an implementation command, I'll proceed with option A." → No. The command requires you to wait for the choice. Implementation is what happens *after* the user picks.
- ❌ "The choice is obvious, so I'll skip the question." → No. Present options and wait. The user may know constraints you don't.
- ❌ "Grill isn't strictly necessary here." → No. If it's registered, run it.
- ❌ "I treated `grill-with-docs` as 'answer with doc-grounding' instead of actually running the grilling loop." → No. Invoking grill means **executing the grill skill itself** — its real, interactive question loop — not answering in a doc-grounded style, not paraphrasing what it would ask. Load the skill and run it.
- ❌ "I'll ask a couple of clarifying questions of my own; that's basically grilling." → No. That is not the grill skill. Run the actual `grill-with-docs` / `grill-me` skill.

If you are a smaller / non-frontier model: treat steps 2, 4, 5, and 7 as literal hard stops. Ask the questions, finish the checklist, run the tool, ask the question, then wait.

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
- Project context (related work, prior art, spec/RFC, related items) when relevant.
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

### Steps

1. Capture the raw idea from the user. If no idea was provided, ask for it.
2. **Formulate and confirm the idea (GATE A).** Ask focused follow-up questions across **scope**, **outcome**, and **success criteria**, shaping the spec with `/create-issue`'s good-issue components (see "Formulating the idea" above). Iterate until all three are pinned down, then write the mini-spec back as a short Scope / Outcome / Success-criteria block and get the user's explicit confirmation. Do not investigate or plan until the spec is confirmed. If the user cannot commit to success criteria, surface that as a risk and agree on a provisional definition of done.
3. **Read the project's agent context before forming a hypothesis or editing.** In this order:
   - The **root** `AGENTS.md` (and `CLAUDE.md` if present — usually an alias).
   - The **nearest** `<area>/AGENTS.md` to the files the idea touches in a multi-area repo. Nested files override the root for that area.
   - `CONTRIBUTING.md` when it exists; treat it as authoritative for what may be merged.
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
5. **Run the grill skill (GATE B).** When `grill-with-docs` / `grill-me` is registered, **read that skill and execute its actual question loop now** on the confirmed mini-spec plus your investigation findings — ask the questions, wait for answers, iterate to completion — then fold its output into the options and verification plan. Do not substitute a "doc-grounded" answer or your own ad-hoc questions for the skill. If none is registered, say so explicitly and continue.
6. Validate before coding:
   - The mini-spec is confirmed, the scope is bounded, and the success criteria are clear enough to verify against.
   - The idea is not already fully implemented (per step 4).
   - If the spec is still ambiguous or the scope keeps growing, stop and return to step 2.
7. **Present implementation options, then STOP and wait (GATE D).** Always present exactly three distinct options for delivering the idea, with tradeoffs, a verification plan tied to the success criteria, and a recommendation grounded in the step 4 investigation and step 5 grill output. The three must be genuinely different approaches (e.g. scope/architecture/effort tradeoffs), not trivial variations of one. When step 4 found the idea **already implemented**, the options reflect that instead of inventing duplicate code — e.g. (a) use/close as already covered, (b) extend the existing implementation to meet the remaining spec, (c) a thin alternative that reuses the existing code. **Do not write any implementation code until the user explicitly picks an option or explicitly tells you to proceed with the recommendation.** Do not interpret "this is an implementation command" as permission to skip the choice. End your turn here and wait for the user's reply.
   *If the chosen option is "already covered" with no code or test change, skip steps 8–15: report the existing implementation and stop. The remaining steps apply only when there is a code or test change to ship.*

8. Branch / worktree:
   - Prefer reusing an existing focused branch or worktree for the idea.
   - If a branch exists without a worktree, use or create a worktree when that best isolates the work.
   - Otherwise create a focused branch named from the idea (e.g. `idea-short-slug`); use a worktree when practical.
9. Implement the smallest complete change that satisfies the chosen approach and the confirmed success criteria.
10. Update tests and documentation per the repository's contribution guidance (`CONTRIBUTING.md`, `AGENTS.md`, or equivalent) when present.
11. Run targeted verification first (focused tests, types, lint) on the changed area, then broader verification when the change has wider impact.
12. **If the change is UI/UX, also:**
    - Run the app (or Storybook / component sandbox) and load the affected screens or components.
    - Compare against the outcome described in the confirmed spec. Capture before/after screenshots or short recordings.
    - Verify accessibility: keyboard navigation and focus order, visible focus styles, ARIA roles/labels for new interactive elements, color contrast meeting WCAG AA (or the project's standard), and `prefers-reduced-motion` respected for animations.
    - Verify responsive behavior at the project's supported breakpoints and across light/dark/system themes when applicable.
    - Reuse existing design-system components and tokens; do not introduce one-off styles for primitives that already exist.
    - Attach before/after media and accessibility notes to the PR description so reviewers can evaluate without re-running the app.
13. **Run the project's full verification gate before invoking `/create-pr`.** Prefer the project's aggregator script when it exists (e.g. `bun run check`) — that's the canonical "ready to commit" signal. Otherwise run the per-step gate explicitly:

    ```bash
    bun install --frozen-lockfile
    bun run format:check   # or biome check
    bun run lint           # or biome lint .
    bun test
    bun run typecheck      # or tsc --noEmit / bunx tsc --noEmit
    bun run build
    ```

    Do not skip steps because they "should pass." If any step fails, fix the cause; do not invoke `/create-pr` with a red gate.

14. **Review the implementation before handoff (do not skip).** After the gate is green but before `/create-pr`, audit your own change critically — as a reviewer who did not write it would:
    - **Matches the spec.** The change satisfies the confirmed mini-spec's scope, outcome, and success criteria. The exception: when the scope was deliberately changed across later turns or grill sessions, match that updated intent instead — and note the divergence from the original idea in the PR description so reviewers understand why.
    - **No shortcuts.** No stubbed logic, hardcoded values standing in for real behavior, `TODO`/`FIXME` left behind, swallowed errors, skipped/`.only`/commented-out tests, or "happy path only" handling of cases the spec requires. Confirm the real layer was built, not a façade.
    - **No tech debt introduced.** No dead code, no duplication that should be extracted, no copy-paste of an existing pattern that has a shared helper, no weakened types (`any`, unsafe casts) or loosened lint/type rules to make the gate pass, no leftover debug output.
    - **Consistency.** The change follows the repo's conventions (from step 3 agent context and `docs/code-style.md`) and reuses existing components/utilities rather than reinventing them.
    - If this review surfaces a problem, fix it and re-run the relevant verification (step 11/13) before proceeding. Do not defer found issues to "a follow-up" unless the user explicitly agrees.
15. Invoke `/create-pr` to commit, push, and open the draft pull request. Summarize the idea and its success criteria in the PR body. There is no issue to close.
