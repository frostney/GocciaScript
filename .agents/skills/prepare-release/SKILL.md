---
name: prepare-release
description: Run all GocciaScript release-preparation tasks before /create-release — verify the build is green, sync docs and the website to source truth (numbers and claims), preview the changelog, run the conformance and generated-data checks, and open a prep PR — ending strictly before the version/changelog/tag work that /create-release owns. Use when the user runs /prepare-release or asks to prepare, prep, or get the repo ready for a release.
---

# Prepare Release

Project-specific pre-flight that gets the GocciaScript repository into a releasable, truthful, verified state and hands off to `/create-release`. The deliverable is a **prep PR** (via `/create-pr`) plus a **readiness report**.

This skill **stops before** `/create-release` begins. Reuse the gates in [DEFINITION_OF_DONE.md](../../../DEFINITION_OF_DONE.md) rather than restating them, and defer the general engineering bar to `software-engineering-excellence`.

## Boundary

- **In scope:** verification, source-truth sync (docs, website, `CONTEXT.md`, README), changelog categorization preview, conformance and generated-data checks, housekeeping, and opening the prep PR.
- **Out of scope (owned by `/create-release`):** computing the next version, writing the changelog section, the `release/<version>` branch, the tag, and publishing. CI owns the tag-triggered publish — see `docs/build-system.md` § CI Integration.

## Flow

Read-only first, one approval checkpoint, then apply, then hand off:

1. Run every **non-mutating** check (verify, conformance read, currency sweep, generated-data verify).
2. Present **one consolidated readiness report** (see below).
3. Stop for the user to approve or veto **per category** (e.g. "skip the website this cycle").
4. Apply approved fixes (each category's diff visible), then **re-run the verify gate**.
5. Open a **draft** prep PR via `/create-pr` — the final review gate.

### Hard blockers — stop and report
- The `DEFINITION_OF_DONE` gate is red (build, `tests`, `tests --mode=bytecode`, or `format --check` fails).
- The working tree is dirty/unexpected, or the branch is behind the base (per `git-workflow`).

### Deliberate non-blocker
- A **test262 conformance regression reports loudly but never blocks** — it is non-blocking in CI by design, and is a judgment call for the user, not a gate.

## Steps

### 1. Preflight
- `git fetch origin`; confirm the branch is a fresh branch off the default branch and the tree is clean. Stop if not.
- After a merge, branch switch, or generated-resource change, build clean to rule out stale FPC artifacts before trusting any failure: `./build.pas --clean <target>` (see `docs/contributing/tooling.md` § Stale FPC Build Artifacts).

### 2. Verify green — the DEFINITION_OF_DONE gate (reused, not restated)
```bash
./build.pas testrunner
./build/GocciaTestRunner tests
./build/GocciaTestRunner tests --mode=bytecode
./format.pas --check
```
When the website is in scope, also run its configured checks: `cd website` then `bun test`, the Biome lint, and `tsc --noEmit` (see `website/package.json` scripts). **Read `website/AGENTS.md` first — this is _not_ the Next.js you know; consult `node_modules/next/dist/docs/` before any website edit.** A red gate is a hard blocker → stop.

### 3. Conformance baseline (read-only; reports, never blocks)
- Source the current numbers from the **latest main `test262-results.json`** CI artifact (`gh run download`) or the published dashboard data the website already consumes — **never** a local full-suite run (~52k tests, far too slow for prep).
- Report current pass rate/count, **delta vs the last release tag**, and any category-level regressions.
- This is the source of truth for any version-stamped conformance figure the truth-sync needs in step 4.

### 4. Source-truth sync — the core of the skill
First the existing structural doc checks (already gated in `lefthook` and `pr.yml`; re-run as insurance):
```bash
bun scripts/check-doc-links.ts
bun scripts/check-doc-symbols.ts
bun scripts/check-doc-duplication.ts
bun scripts/check-doc-length.ts
```
Then the **two-layer currency check** nothing else covers — stale numbers and misleading prose across **both** `docs/*.md` **and** the website's `.ts/.tsx` copy (the structural checks above are markdown-only):

**Layer 1 — deterministic candidate sweep.** Grep all docs and website source for hand-maintained quantitative/framing claims: percentages, `test262`, MB sizes, ES-year, `TOML`/`YAML` versions, and subset-framing words (`subset`, `only a`, `partial`). Produce a `file:line` candidate list. Marketing copy is **duplicated** across these — check them together so a claim doesn't get fixed in one and left stale in another:
- `website/src/components/landing.tsx`
- `website/src/lib/site-markdown.ts`
- `website/src/lib/agent-discovery.ts`
- `website/src/lib/landing-data.ts`

**Layer 2 — verify each candidate against the source of truth.** Fan out to subagents when the surface is large. Check every numeric, factual, and characterization claim against:
- the **code** (does the feature exist? is the flag spelled right, e.g. `--compat-asi`?),
- the **live test262 report** (conformance numbers),
- the **measured binary size** of the built loader (e.g. the "under 6MB" claim),
- **`CONTEXT.md`** as the canonical language — subset-implies-ceiling prose is a finding against *"Recommended defaults → Avoid: language ceiling, hard limit."*

Fix to current truth. **Governing rule: no hand-maintained conformance number anywhere** — a hand-typed test262 % is itself a finding (the kind that read "80%" long after the real figure had moved well past it), replaced by the `/compatibility` dashboard link or a freshly-sourced, **version-stamped** figure.

Also refresh `README.md` and `VISION.md` so claims and feature lists match shipped reality, and reconcile `CONTEXT.md` for any new or renamed terms (via `domain-modeling`).

### 5. Generated-data verify-clean (read-only; reports)
- Regenerate `intl` / `timezone` / `unicode` data into a throwaway location and **diff against committed `source/generated`** (use a generator `--check` mode if one exists, else regenerate-and-diff). Any diff means stale or hand-edited generated data → finding. **No upstream version bump.**
- **Report** the current test262/suite pins for context; **do not bump** — CI's weekly `test262-bump.yml` owns that.

### 6. Changelog categorization preview
- Dry-run `git cliff --unreleased` and scan for miscategorized commits or `📖 Other` bloat. Fix `cliff.toml` **now**, before `/create-release` renders the real changelog.

### 7. Housekeeping
- `skills-lock.json` is current and any installed good-known-route skill updates are committed. (This skill is **repo-native** and is **not** tracked in `skills-lock.json`.)
- No unrelated changes are mixed in (per `DEFINITION_OF_DONE`).

### 8. Hand off
- Open the prep PR via `/create-pr` as a **draft**, using a **content-reflecting conventional prefix** — default `docs:` for a truth-sync-heavy run; **never `chore(release):`**, which is reserved for `/create-release`'s release commit and is skipped by `cliff.toml`. Split into focused PRs if the categories diverge (e.g. docs vs tooling).
- File any **deferred follow-ups** the run surfaced — offer to open them via `/create-issue` (do **not** auto-run it mid-flow; do **not** silently drop them). Canonical cases: the committed `.ts/.tsx` conformance-number **sentinel** that hardens Layer 1 so the "80%" class can't silently recur, and any genuine conformance regression.
- Once the prep PR is merged, the repo is ready for `/create-release` — a **separate** invocation.

## Readiness report

Emit one consolidated report:

1. **Verdict** — `READY for /create-release` or `BLOCKED` (+ why).
2. **Gate results** — build / `tests` / `tests --mode=bytecode` / `format --check` (+ website checks if in scope).
3. **Conformance** — current rate, delta vs last release, regressions (non-blocking).
4. **Findings & fixes** — by category (docs, website, `CONTEXT.md`, changelog preview, housekeeping), each with the diff applied or proposed.
5. **Generated-data** — verify-clean result; current test262/suite pins.
6. **Recommended follow-ups** — with the `/create-issue` offer.
7. **Handoff** — "run `/create-release`," plus the version basis it would compute from.

## Notes
- This skill **prepares**; it never cuts. Cutting is `/create-release`; publishing is CI (see `docs/build-system.md`).
- Defers to `/create-pr`, `/create-release`, `/create-issue`, `git-workflow`, `software-engineering-excellence`, `domain-modeling`, `native-nostalgia-stack` (build/format/test contract), `project-structure`, and the `DEFINITION_OF_DONE` / `DEFINITION_OF_READY` gates.
- Website edits live behind `website/AGENTS.md` — "not the Next.js you know"; read `node_modules/next/dist/docs/` before touching it.
