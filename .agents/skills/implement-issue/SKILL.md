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
  and its own declared commands rather than any assumed toolchain.
---

# Implement issue

## Instructions

Validate and implement a GitHub issue in the current repository.

### Non-negotiable gates

1. **Investigate before concluding.** Read the real project commands, relevant
   instructions, affected code paths, tests, and reproduction evidence.
2. **Run the grill skill when registered.** Use `grill-with-docs`, falling back
   to `grill-me`, and complete its actual user-question loop before presenting
   options. Ad-hoc questions are not a substitute. If neither is registered,
   note that once and continue.
3. **Present two to four real options and get a decision.** Recommend one and
   wait for the user's post-options choice unless automatic mode applies.
4. **Review and hand off.** For any code or test change, run `/review`, address
   validated in-scope findings, and finish through `/create-pr`.

### Planning checkpoint

After investigation and grilling, give one concise phase update naming the
material constraints and findings, then present the options. Do not narrate an
internal compliance checklist. Do not edit until the applicable Definition of
Ready is satisfied and the option gate has produced a selected path.

### Automatic mode

Automatic mode is opt-in. It is active only when the user's original `/implement-issue` prompt includes the standalone word `automatic` or explicitly asks for automatic mode.

In automatic mode, do **not** skip investigation, grilling, readiness checks, the planning checkpoint, or the options presentation. After presenting two to four genuinely distinct options, select the recommendation from the issue, project context, risk, and verification cost, state why, and continue without waiting.

If the best option is unclear, materially risky, or conflicts with `VISION.md`, automatic mode does not apply: stop and ask the user for clarification.

### Definition of Ready / Definition of Done are canonical (absence protocol)

`DEFINITION_OF_READY.md` and `DEFINITION_OF_DONE.md` (and the case/spelling variants searched in step 4) are the **canonical truth** for when this work may start and when it is shippable. When present, every applicable item is a hard gate — do not invent your own readiness or completion bar to replace them.

**DoR/DoD absence protocol — referenced by the steps below.** If either file is missing after the mandatory search, do **not** silently proceed and do **not** invent a substitute:

- State prominently to the user that no project-context `DEFINITION_OF_READY.md` / `DEFINITION_OF_DONE.md` was found, so readiness / completion cannot be checked against a project definition, and recommend adding one.
- Carry the flagged absence into the planning checkpoint and PR description.
- Continue with the workflow's built-in readiness/review checks and only the project's actually-declared commands — never a guessed or stack-assumed gate.

### Steps

1. Parse the issue number. If missing or non-numeric, ask.
2. Fetch the issue with GraphQL first: title, body, state, labels, assignees, URL, comments, and whether the entity is actually a pull request.
3. Fall back to REST when GraphQL is unavailable:

   ```bash
   gh api "repos/$OWNER/$REPO/issues/$ISSUE_NUMBER"
   ```

4. **Read the project's agent context before forming a hypothesis or editing.** In this order:
   - The **root** `AGENTS.md`.
   - **Vision document (mandatory search).** Search for `VISION.md` at the repository root and in relevant product/docs areas. Read every discovered vision document. If the issue conflicts with project vision, surface that decision before planning or editing.
   - **Agent-alias files (mandatory search).** Search for `CLAUDE.md` and equivalent root agent aliases. Read each discovered alias and apply any additional instructions.
   - The **nearest** `<area>/AGENTS.md` to the files the issue touches in a multi-area repo. Nested files override the root for that area.
   - **Contribution rules (mandatory search).** Search the root, nearest affected area, `docs/`, and any `AGENTS.md`-referenced context for `CONTRIBUTING.md` or equivalent guidance. Read every match and apply the nearest relevant rules.
   - **Project-context Definition files (mandatory search).** Search for `DEFINITION_OF_READY.md`, `DEFINITION_OF_DONE.md`, and spelling/case variants in the root, affected area, and docs. Read every match and apply the nearest relevant definition.
   - The repo's `docs/` index (`docs/README.md`, `docs/architecture.md`, `docs/code-style.md`, or equivalent) for the area being changed.
   Do not skip this step even when the issue looks small — Hard Constraints sections (e.g. "Bun only, no npm/pnpm/yarn", "AI SDK via Vercel AI Gateway only") frequently change the implementation path.

   **Discover and use implementation-specific skills and context (required).** Before planning, check what specialized skills and context apply to the area being changed, and use them:
   - **Registered skills.** Look for skills that match the project's stack or domain (e.g. a stack skill like `react-stack` / `native-nostalgia-stack`, a conventions skill like `convex-conventions`, or any project-provided skill in `.agents/skills/` ↔ `.claude/skills/`). When one matches the change, read it and follow it — its rules override generic defaults for that area.
   - **In-repo context.** Honor area-specific `docs/` (e.g. `docs/code-style.md`, a feature MVP doc), `.cursor/rules`, and any `AGENTS.md`-referenced guides for the files you're touching.
   - If a clearly relevant skill or context exists, using it is **not optional**. Implementing without consulting an applicable stack/conventions skill is a shortcut (see step 15). If none applies, note that briefly and proceed.

5. **Investigate before concluding.** A conclusion drawn from a single file or an assumed command is invalid. Before forming a hypothesis:
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
6. **Run the grill gate.** Give the grill skill the issue plus the material project and investigation context, complete its loop, and fold the result into every option and its verification plan. Do not implement product code during grilling unless that skill explicitly requires a context update.
7. Validate before coding:
   - The issue exists, is open, and is not a pull request.
   - No `blocked`, `duplicate`, `wontfix`, or equivalent label/comment.
   - Expected behavior and acceptance criteria are clear enough to implement.
   - The Definition of Ready search from step 4 is complete, and the issue, evidence, and selected path satisfy every applicable item before editing.
   - Absence protocol: if the mandatory search finds no Definition of Ready, apply the **DoR/DoD absence protocol** (see the section above the steps).
   - If validation fails or requirements are ambiguous, stop and ask.
8. **Give the planning checkpoint and present options.** Present the smallest set of two to four genuinely distinct approaches; never pad or collapse the real option space. For each, give its tradeoffs and verification plan, incorporate the grill findings, and recommend one from the investigation evidence. If the issue is already fixed, options should reflect closing it, adding missing regression coverage, or addressing only genuinely affected sibling paths. Do not edit until the user makes a post-options choice or automatic mode selects the recommendation; otherwise stop here.
   *If the chosen option is "close as already resolved" with no code or test change, skip steps 9–17: instead, comment on the issue with the evidence (fixing commit + covering test) and close it (or ask the user to). The remaining steps apply only when there is a code or test change to ship.*

9. Branch / worktree:
   - Prefer reusing an existing focused branch or worktree for the issue.
   - If a branch exists without a worktree, use or create a worktree when that best isolates the work.
   - Otherwise create a focused branch named from the issue (e.g. `issue-123-short-slug`); use a worktree when practical.
   - **Update the branch/worktree against the latest baseline before implementing.** Run `git fetch origin`, then merge the remote default branch into the working branch (e.g. `git merge origin/<default-branch>` — never rebase, per the `git-workflow` skill). Resolve any conflicts and commit the merge before writing new code, so the work starts from the current remote base.
10. Implement the smallest complete change that satisfies the chosen approach.
11. Update tests and documentation per the contribution guidance discovered in step 4 (`CONTRIBUTING.md`, `AGENTS.md`, or equivalent). Absence protocol: after the mandatory search finds no contribution guidance, state that no project contribution guidance was found and follow the issue, Definition of Ready, Definition of Done, and local code patterns.
12. Run targeted verification first (focused tests, types, lint) on the changed area, then broader verification when the change has wider impact.
13. **If the change is UI/UX, rendering and visual evidence are mandatory (do not skip).** A UI/UX change handed off without screenshots of the actual rendered result is incomplete:
    - Run the app (or Storybook / component sandbox) and load every affected screen, component, and state — never assert the UI is correct without rendering it.
    - Capture before/after screenshots (or short recordings) of each affected screen and state, at the project's supported breakpoints and across light/dark/system themes when applicable.
    - Compare the captures against the design or the issue's expected state, and fix any discrepancy before handoff.
    - Verify accessibility: keyboard navigation and focus order, visible focus styles, ARIA roles/labels for new interactive elements, color contrast meeting WCAG AA (or the project's standard), and `prefers-reduced-motion` respected for animations.
    - Reuse existing design-system components and tokens; do not introduce one-off styles for primitives that already exist.
    - **Attach the screenshots/recordings and accessibility notes to the PR — this is mandatory, not optional.** The PR must be fully reviewable from these artifacts alone, without re-running the app, so the change can be judged asynchronously. A UI/UX PR missing this visual evidence is not ready for `/create-pr`.
14. **Run the project's full verification gate before invoking `/create-pr`.** The gate is defined by the project-context `DEFINITION_OF_DONE.md` together with the project's aggregator script and actually-enumerated real commands (e.g. the project's `check`) — these are the canonical "ready to commit" signal. Run every verification the Definition of Done requires, using only the commands the repo actually declares. Do **not** invent commands or assume a stack: if a verification the Definition of Done requires has no corresponding command in the repo, that is a finding to raise with the user, not a command to make up.

    Absence protocol — no `DEFINITION_OF_DONE.md`: apply the **DoR/DoD absence protocol** (see the section above the steps) — run only the project's actually-declared commands; do not substitute an invented gate.

    Do not skip steps because they "should pass." If any step fails, fix the cause; do not invoke `/create-pr` with a red gate.

15. **Review the implementation before handoff (do not skip).** After the gate is green but before `/create-pr`, audit your own change critically — as a reviewer who did not write it would:
    - **Check against the spec, criterion by criterion.** Walk each acceptance criterion in the issue (and any scope confirmed across later turns/grill) and confirm the change actually satisfies it, citing the code or test that does so. Anything unmet is unfinished work, not a follow-up. When the scope was deliberately changed, match that updated intent instead — and note the divergence from the original issue text in the PR description so reviewers understand why.
    - **Check the Definition of Done.** Re-read every project-context Definition of Done discovered in step 4 before handoff. Verify the implementation, tests, documentation, review evidence, and handoff artifacts satisfy every applicable item. Any unmet completion item is a hard stop: fix it before `/review` or `/create-pr`, or stop and get explicit user agreement that the item is out of scope. Absence protocol: if no Definition of Done was found, apply the **DoR/DoD absence protocol** (see the section above the steps).
    - **No shortcuts.** No stubbed logic, hardcoded values standing in for real behavior, `TODO`/`FIXME` left behind, swallowed errors, skipped/`.only`/commented-out tests, or "happy path only" handling of cases the issue requires. Re-trace the full code path from step 5 and confirm the real layer was fixed, not just the symptom.
    - **No tech debt introduced.** No dead code, no duplication that should be extracted, no copy-paste of an existing pattern that has a shared helper, no weakened types (`any`, unsafe casts) or loosened lint/type rules to make the gate pass, no leftover debug output.
    - **Consistency.** The change follows the repo's conventions (from step 4 agent context and `docs/code-style.md`) and reuses existing components/utilities rather than reinventing them.
    - If this review surfaces a problem, fix it and re-run the relevant verification (step 12/14) before proceeding. Do not defer found issues to "a follow-up" unless the user explicitly agrees.
16. **Run a code review before handoff.** Invoke the `/review` skill/command on the branch diff before opening the PR. This is a separate, fresh review from step 15.
    - Discovery hint: look for a skill or command named `review` / `code-review` (e.g. `~/.cursor/skills/review/`, `.cursor/skills/...`, `.agents/skills/...`). Run the actual skill; do not substitute a self-summary for it.
    - Validate its findings, fix every in-scope issue, and re-run the relevant verification (step 12/14). Surface scope-expanding findings rather than silently widening the change.
    - If no `/review` skill is registered, say so explicitly, then perform a thorough manual diff review covering correctness, security, error handling, tests, and style.
17. **Hand off via `/create-pr`.** For any code or test change, use `/create-pr` to commit, push, and open the draft pull request. Include `Closes #<issue>` and the verified evidence in the PR body.
