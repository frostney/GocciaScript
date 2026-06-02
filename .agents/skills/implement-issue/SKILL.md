---
name: implement-issue
description: >-
  Validates a GitHub issue against the current codebase, reading the nearest
  AGENTS.md/CLAUDE.md, area context, and any applicable stack/conventions
  skills first. Runs a full-context investigation (real project commands, whole
  code path, no shortcuts), handles already-fixed issues by closing or adding
  regression tests, invokes the grill skill when registered, then presents
  implementation options and stops for the user's choice before coding.
  Implements the chosen option, runs verification (UI/UX checks plus the
  project's full check gate), reviews for shortcuts and tech debt, and prepares
  a draft PR via /create-pr. Use when the user runs /implement-issue with an
  issue number.
license: Unlicense OR MIT
compatibility: >-
  Requires the GitHub CLI (gh) authenticated to the target repository and
  network access; verification examples assume a Bun toolchain.
---

# Implement issue

## Instructions

Validate and implement a GitHub issue in the current repository.

### Non-negotiable gates (do not skip, do not rationalize)

These three gates are mandatory. Most failures of this skill come from skipping one of them under time pressure or because the task "looks simple." Being an implementation command is **not** a license to skip any gate.

1. **GATE A — Grill before planning.** When a grill skill is registered, you **must** invoke it (step 6) before forming a hypothesis or presenting options. Do not proceed to options without it.
2. **GATE B — Full-context investigation before concluding.** You **must** complete the investigation in step 5 (enumerate the project's real commands, trace the full code path, reproduce) before forming any conclusion. A conclusion drawn from a single file or a guessed command is invalid.
3. **GATE C — Wait for the user's choice.** After presenting options (step 8) you **must stop and wait** for the user to pick one. Do **not** continue to implementation on your own.

**Forbidden rationalizations** — if you catch yourself writing any of these, you are violating the skill, stop and follow the gate instead:

- ❌ "Since `/implement-issue` is an implementation command, I'll proceed with option A." → No. The command requires you to wait for the choice. Implementation is what happens *after* the user picks.
- ❌ "The choice is obvious, so I'll skip the question." → No. Present options and wait. The user may know constraints you don't.
- ❌ "Grill isn't strictly necessary here." → No. If it's registered, run it.
- ❌ "I treated `grill-with-docs` as 'answer with doc-grounding' instead of actually running the grilling loop." → No. Invoking grill means **executing the grill skill itself** — its real, interactive question loop — not answering in a doc-grounded style, not paraphrasing what it would ask. Load the skill and run it.
- ❌ "I'll ask a couple of clarifying questions of my own; that's basically grilling." → No. That is not the grill skill. Run the actual `grill-with-docs` / `grill-me` skill.
- ❌ "I found the likely cause in this one file, no need to look further." → No. Complete the full-context investigation first.

If you are a smaller / non-frontier model: treat steps 6, 5, and 8 as literal hard stops. Run the tool, finish the checklist, ask the question, then wait.

### Use the grill skill for thoroughness (always when available)

Before forming a hypothesis or presenting implementation options, the agent **always invokes the grill skill** when it is registered in this environment — not only when something is ambiguous. This is GATE A above. The grill output is folded into the implementation plan and validation step so the result is more thorough than the raw issue text would produce.

**Invoking the grill skill means literally running that skill — not imitating its spirit.** `grill-with-docs` and `grill-me` are separate skills with their own multi-question interrogation loop. To invoke one you **read its `SKILL.md` and execute its procedure**: actually ask the user the grilling questions it generates and wait for the answers, iterating until the loop completes. The following are **NOT** invoking it and are forbidden substitutes:

- Treating the mention of `grill-with-docs` as a style instruction — "answer with doc-grounding," "be thorough," "cite the docs" — and then proceeding. ❌
- Summarizing or paraphrasing the questions grilling *would* ask instead of asking them. ❌
- Asking one or two clarifying questions of your own and calling that grilling. ❌
- Skipping it because you believe you already understand the request. ❌

If you cannot run the grill skill, do not silently downgrade it to "doc-grounded answering" — say explicitly that no grill skill was found (see discovery hint) and proceed on the input as given.

- **`/grill-with-docs` is preferred.** Use `/grill-me` only when `/grill-with-docs` is not registered.
- Discovery hint: look for a skill or command named `grill-with-docs` or `grill-me` (e.g. `~/.cursor/skills/grill-with-docs/`, `~/.cursor/skills/grill-me/`, `.cursor/skills/...`, `.agents/skills/...`).
- If neither is registered, state explicitly that no grill skill was found, then proceed with the workflow on the input as given.

### Steps

1. Parse the issue number. If missing or non-numeric, ask.
2. Fetch the issue with GraphQL first: title, body, state, labels, assignees, URL, comments, and whether the entity is actually a pull request.
3. Fall back to REST when GraphQL is unavailable:

   ```bash
   gh api "repos/$OWNER/$REPO/issues/$ISSUE_NUMBER"
   ```

4. **Read the project's agent context before forming a hypothesis or editing.** In this order:
   - The **root** `AGENTS.md` (and `CLAUDE.md` if present — usually an alias).
   - The **nearest** `<area>/AGENTS.md` to the files the issue touches in a multi-area repo. Nested files override the root for that area.
   - `CONTRIBUTING.md` when it exists; treat it as authoritative for what may be merged.
   - The repo's `docs/` index (`docs/README.md`, `docs/architecture.md`, `docs/code-style.md`, or equivalent) for the area being changed.
   Do not skip this step even when the issue looks small — Hard Constraints sections (e.g. "Bun only, no npm/pnpm/yarn", "AI SDK via Vercel AI Gateway only") frequently change the implementation path.

   **Discover and use implementation-specific skills and context (required).** Before planning, check what specialized skills and context apply to the area being changed, and use them:
   - **Registered skills.** Look for skills that match the project's stack or domain (e.g. a stack skill like `react-stack` / `native-nostalgia-stack`, a conventions skill like `convex-conventions`, or any project-provided skill in `.agents/skills/` ↔ `.claude/skills/`). When one matches the change, read it and follow it — its rules override generic defaults for that area.
   - **In-repo context.** Honor area-specific `docs/` (e.g. `docs/code-style.md`, a feature MVP doc), `.cursor/rules`, and any `AGENTS.md`-referenced guides for the files you're touching.
   - If a clearly relevant skill or context exists, using it is **not optional**. Implementing without consulting an applicable stack/conventions skill is a shortcut (see step 15). If none applies, note that briefly and proceed.

5. **Investigate the whole codebase before concluding (GATE B). Do not take shortcuts.** A conclusion drawn from a single file or an assumed command is invalid. Before forming any hypothesis:
   - **Enumerate the project's real commands instead of guessing.** Read the manifest's script section (`package.json` `scripts`, `Makefile` / `Justfile` / `Taskfile`, `pyproject.toml`, `Cargo.toml`, etc.) and the `docs/tooling.md` / `docs/quick-start.md` equivalents. Use the commands the project actually defines (e.g. the project's `check`, `test`, `lint`, `dev` names) — never invent a command or assume a default that the repo hasn't declared.
   - **Search broadly, not narrowly.** Use codebase search and grep to find every site related to the issue: the symbol, its callers, its tests, sibling implementations, and config. Read the surrounding modules, not just the first match.
   - **Trace the full code path** from entrypoint to the reported symptom. Identify the actual layer where the behavior originates rather than patching the first place the symptom appears.
   - **Reproduce the reported behavior** against current code; do not trust the issue text alone. When the issue cites a reproduction command, test path, or external artifact (test262, Playwright run, etc.), fetch and run that exact artifact. The title is a pointer, not a spec.
   - If after a genuine investigation something is still unclear, that is a finding to raise in the options — not a reason to guess.

   **If the reported behavior does NOT reproduce, the issue may already be fixed. Do not invent a change to justify the command.** Determine which case applies and carry it into the options (step 8):
   - **Case: already fixed AND already covered by a test.** Find the commit/code that fixed it and the test that locks it in. The recommended outcome is to close the issue as already resolved (referencing the fixing commit and the covering test), not to write new code. Only add something if the user wants extra coverage.
   - **Case: already fixed BUT not covered by a regression test.** The behavior works but nothing prevents it from regressing. The work becomes **adding a regression test** (and any missing edge-case tests) that would fail against the pre-fix code and passes now — no production-code change. Name the test after the issue so the linkage is obvious.
   - **Case: fixed for the reported path but adjacent paths are untested or still broken.** Add tests for the sibling paths surfaced in the broad search above, and fix any that are genuinely broken. Treat each broken sibling as in-scope only if it shares the issue's root cause; otherwise note it for a separate issue.
   - In all three cases, confirm the "already fixed" conclusion with evidence (the passing reproduction, the responsible code, the existing or missing test) before presenting it — a behavior that merely looks fixed in one spot may still fail on another path.
6. **Run the grill skill (GATE A).** When `grill-with-docs` / `grill-me` is registered, **read that skill and execute its actual question loop now** on the issue plus your investigation findings — ask the questions, wait for answers, iterate to completion — then fold its output into the options and verification plan. Do not substitute a "doc-grounded" answer or your own ad-hoc questions for the skill. If none is registered, say so explicitly and continue.
7. Validate before coding:
   - The issue exists, is open, and is not a pull request.
   - No `blocked`, `duplicate`, `wontfix`, or equivalent label/comment.
   - Expected behavior and acceptance criteria are clear enough to implement.
   - If validation fails or requirements are ambiguous, stop and ask.
8. **Present implementation options, then STOP and wait (GATE C).** Always present exactly three distinct options with tradeoffs, a verification plan, and a recommendation grounded in the step 5 investigation and step 6 grill output. The three must be genuinely different approaches, not trivial variations of one. When step 5 found the issue **already fixed**, the options reflect that situation instead of inventing a code change — e.g. (a) close as already resolved citing the fixing commit and covering test, (b) add a regression test (plus missing edge-case tests) that would have caught the original bug, (c) extend coverage/fixes to the adjacent paths surfaced in the search. **Do not write any implementation code until the user explicitly picks an option or explicitly tells you to proceed with the recommendation.** Do not interpret "this is an implementation command" as permission to skip the choice. End your turn here and wait for the user's reply.
   *If the chosen option is "close as already resolved" with no code or test change, skip steps 9–16: instead, comment on the issue with the evidence (fixing commit + covering test) and close it (or ask the user to). The remaining steps apply only when there is a code or test change to ship.*

9. Branch / worktree:
   - Prefer reusing an existing focused branch or worktree for the issue.
   - If a branch exists without a worktree, use or create a worktree when that best isolates the work.
   - Otherwise create a focused branch named from the issue (e.g. `issue-123-short-slug`); use a worktree when practical.
10. Implement the smallest complete change that satisfies the chosen approach.
11. Update tests and documentation per the repository's contribution guidance (`CONTRIBUTING.md`, `AGENTS.md`, or equivalent) when present.
12. Run targeted verification first (focused tests, types, lint) on the changed area, then broader verification when the change has wider impact.
13. **If the change is UI/UX, also:**
    - Run the app (or Storybook / component sandbox) and load the affected screens or components.
    - Compare against the design or current state from the issue. Capture before/after screenshots or short recordings.
    - Verify accessibility: keyboard navigation and focus order, visible focus styles, ARIA roles/labels for new interactive elements, color contrast meeting WCAG AA (or the project's standard), and `prefers-reduced-motion` respected for animations.
    - Verify responsive behavior at the project's supported breakpoints and across light/dark/system themes when applicable.
    - Reuse existing design-system components and tokens; do not introduce one-off styles for primitives that already exist.
    - Attach before/after media and accessibility notes to the PR description so reviewers can evaluate without re-running the app.
14. **Run the project's full verification gate before invoking `/create-pr`.** Prefer the project's aggregator script when it exists (e.g. `bun run check`) — that's the canonical "ready to commit" signal. Otherwise run the per-step gate explicitly:

    ```bash
    bun install --frozen-lockfile
    bun run format:check   # or biome check
    bun run lint           # or biome lint .
    bun test
    bun run typecheck      # or tsc --noEmit / bunx tsc --noEmit
    bun run build
    ```

    Do not skip steps because they "should pass." If any step fails, fix the cause; do not invoke `/create-pr` with a red gate.

15. **Review the implementation before handoff (do not skip).** After the gate is green but before `/create-pr`, audit your own change critically — as a reviewer who did not write it would:
    - **Matches the issue.** The change satisfies the original issue's intent, description, and acceptance criteria. The exception: when the scope was deliberately changed across later turns or grill sessions, match that updated intent instead — and note the divergence from the original issue text in the PR description so reviewers understand why.
    - **No shortcuts.** No stubbed logic, hardcoded values standing in for real behavior, `TODO`/`FIXME` left behind, swallowed errors, skipped/`.only`/commented-out tests, or "happy path only" handling of cases the issue requires. Re-trace the full code path from step 5 and confirm the real layer was fixed, not just the symptom.
    - **No tech debt introduced.** No dead code, no duplication that should be extracted, no copy-paste of an existing pattern that has a shared helper, no weakened types (`any`, unsafe casts) or loosened lint/type rules to make the gate pass, no leftover debug output.
    - **Consistency.** The change follows the repo's conventions (from step 4 agent context and `docs/code-style.md`) and reuses existing components/utilities rather than reinventing them.
    - If this review surfaces a problem, fix it and re-run the relevant verification (step 12/14) before proceeding. Do not defer found issues to "a follow-up" unless the user explicitly agrees.
16. Invoke `/create-pr` to commit, push, and open the draft pull request. Include `Closes #<issue>` in the PR body.
