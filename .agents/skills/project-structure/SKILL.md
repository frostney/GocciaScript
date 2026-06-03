---
name: project-structure
description: >-
  Language-agnostic repo structure conventions used across the user's projects
  in any language — the user-facing README.md structure (name, logo,
  description, install, usage, optional background, contribution, references),
  the docs/ template (application, architecture, code-style,
  quick-start, tooling, deployment), the AGENTS.md template with a Hard
  Constraints section (symlinked to CLAUDE.md), nested area AGENTS.md for
  multi-area repos, optional CONTRIBUTING.md, the pre-commit hook contract
  with Lefthook as the default and explicit alternatives, scripts directory in
  the project's own language, co-located tests, the .agents/skills folder
  symlinked to .claude/skills, and changelog generation with git-cliff.
  Stack-specific tools (test runners, linters, generators) live in the
  matching stack skill. Use when scaffolding or restructuring any repo,
  writing AGENTS.md or docs, or laying out folders.
license: Unlicense OR MIT
---

# Project structure

## Instructions

This skill describes **layout and documentation patterns that hold regardless of language or runtime**. Stack-specific tooling (test runners, linters, generators, hook config) lives in the matching stack skill (e.g. `react-stack/SKILL.md`, `native-nostalgia-stack/SKILL.md`).

### Top-level layout (by role, not filename)

A repo on this style has these **roles** at the root, named with whatever convention the language community uses:

| Role | What it holds | Examples (varies by language) |
| --- | --- | --- |
| **Agent context** | Operating manual for AI assistants. Symlinked across agent filenames. | `AGENTS.md` ↔ `CLAUDE.md` |
| **Contribution rules** | Authoritative rules for human contributors. Optional. | `CONTRIBUTING.md` |
| **Project intro** | What the project is and how to run it. | `README.md` |
| **Build / package manifest** | Dependency declaration. | `package.json`, `Cargo.toml`, `Gemfile`, `pyproject.toml`, `*.cabal`, `*.dpr` / `*.lpi`, etc. |
| **Lockfile** | One canonical lockfile for the project's PM. | Whatever the PM produces; forbid all others. |
| **Source** | Project code. | `src/`, `app/`, `lib/`, `source/`, `units/`, etc. |
| **Tests** | Co-located next to the source they cover. | `*_test.*`, `*.test.*`, language-native test units alongside the unit under test. |
| **Scripts** | One-off automation: env sync, codegen, seed, copy, generate. | `scripts/`, `bin/`, `tools/` |
| **Docs** | See the docs/ template below. | `docs/` |
| **Generator templates** (optional) | When a generator tool is in use. | `plop-templates/`, `templates/`, `_template/` |
| **Provisioned agent skills** | When the project adopts curated skill packs. Symlinked across agent paths. | `.agents/skills/` ↔ `.claude/skills/` |
| **Static assets** (when relevant) | Public files served as-is. | `public/`, `static/`, `assets/` |
| **Generated output** | Build artifacts. Not committed unless the language requires it. | `dist/`, `build/`, `target/`, `out/` |

The **filenames** are language- and ecosystem-specific. The **roles** are constant.

### `README.md` structure

The root `README.md` is the **project intro for a user**, not a contributor manual. It follows this section order. Sections marked optional are left off entirely when they don't apply — **never invent content to fill a section**.

1. **Headline.** A single top-level Markdown heading (`# Project Name`) — not an HTML `<h1>` tag — that is the project name.
2. **Logo** — shown only if a logo asset exists in the repo. Skip the section entirely when there is none.
3. **Short description.** Max 350 characters. What the project is and what its features are. Link to in-depth documentation (`docs/`, a docs site) when it exists rather than expanding here.
4. **Install.** Max 100 characters. The single canonical install line — `npm install <package>`, `brew install <formula>`, `cargo add <crate>`, etc.
5. **Usage.** How a **user** (not a contributor) uses the project — command-line, code, or a combination. Show the 1–3 most relevant flows and link back to deeper docs (developer flow, full API, configuration) for the rest. Do not document how to work *on* the project here.
6. **(Optional) Background.** Anything important for understanding the project's design — mission statement, philosophy, core decisions still open. Include only when it adds real understanding; leave off rather than invent.
7. **Contribution.** Max 150 characters describing how to install the project locally for development and what's required, then link to the detailed `CONTRIBUTING.md` / contributing docs when they exist.
8. **References.** Links to the agent documentation (`AGENTS.md`) and the license.

Rules:

- The README is **user-facing**; deep developer/build/test detail lives in `docs/` and `CONTRIBUTING.md`, linked from sections 5 and 7 — not inlined.
- Respect the character caps on sections 3, 4, and 7. When the content doesn't fit, link out instead of overflowing.
- Optional sections (logo, background) are omitted cleanly when they don't apply; do not emit an empty heading.
- Markdown linting and the link check (below) apply to `README.md` like any other committed markdown.

### Tests

Tests must provide regression coverage and real value. **Tests are not a checkbox exercise** — delete or rewrite tests that no longer catch regressions, and don't author trivial tests to satisfy a coverage target.

- **End-to-end tests are the core mechanism.** They exercise the system the way it's used in production and are the primary regression net. They live in a dedicated top-level location chosen by the project and run via the project's E2E runner.
- **Unit tests cover public API surfaces** of the modules under test. They are co-located next to the module they exercise. They do not duplicate coverage that an end-to-end test already provides.
- **Layer-specific tests** (CLI invocation, HTTP integration, language interop, fuzzing, etc.) are added when that layer is a delivered artifact or a real source of regressions. They are not added by default.

Project-specific conventions (test granularity, what counts as the public API for a given module, edge-case placement, fixture handling) live in the project's `docs/code-style.md` or `docs/testing.md`.

### `docs/` template (filenames are universal)

Every project ships these files in `docs/`:

| File | Contents |
| --- | --- |
| `README.md` (optional) | Index of the docs folder. |
| `application.md` or `architecture.md` | Tech stack, key concepts, core data model, cross-cutting concerns. |
| `quick-start.md` | Install + configure + run, fast. Zero to working. |
| `tooling.md` | Development commands, environment variables, lint/format/test setup. |
| `code-style.md` | Naming, file layout, design tokens (or equivalent project conventions), import/dependency rules. |
| `deployment.md` | Build profiles, release/store submission, CI/CD, rollback. |

Optional additions when they apply:

- `testing-pattern.md` — when tests follow a non-obvious convention.
- `mcp.md` — when the project exposes an MCP server.
- `<feature>-mvp.md` — feature scope notes for in-flight work.
- `decision-log.md` — append-only record of decisions.
- `spikes/` — point-in-time investigation snapshots.

**Documentation rules (apply to every project):**

- **Every `docs/` file (except `decision-log.md` and root entry points like `README.md` / `AGENTS.md` / `CONTRIBUTING.md`) must include an `## Executive Summary`** with 3–6 bulleted key points placed after the title and any subtitle. An italic subtitle alone does not satisfy this requirement.
- **No duplication.** Each topic has one authoritative document; other docs link to it with a one-liner.
- **Spikes are snapshots.** Files under `docs/spikes/` are point-in-time records; do not update after creation. Add a decision log entry instead.
- **Decision log entries are immutable.** New decisions get new entries; existing entries are not rewritten to match later reality. Cross-links from entries to other docs may be updated when the target is renamed.

### Agent file naming and symlinks

Multiple AI tools expect different filenames for the same content. Maintain **one canonical file** and **symlink the others**, so the source of truth is unambiguous and edits propagate.

- **Canonical:** `AGENTS.md` at the repo root.
- **Symlink:** `CLAUDE.md` → `AGENTS.md`. (Use a real symlink: `ln -s AGENTS.md CLAUDE.md`. A `@AGENTS.md` include line is acceptable when symlinks are not viable on the platform, but symlinks are preferred.)
- **Canonical:** `.agents/skills/` directory at the repo root.
- **Symlink:** `.claude/skills` → `.agents/skills`.

Edit only the canonical file or directory. Never write divergent content into the symlinked path.

### `AGENTS.md` template (universal across languages)

The **default** is a **single** `AGENTS.md` at the repo root (with `CLAUDE.md` symlinked to it as above). Promote to richer variants only when needed.

Required sections, in this order:

```markdown
# Agent Instructions

## Hard Constraints
- <forbiddens that would silently corrupt the project if violated>
- <e.g. "FreePascal only; do not introduce another compiled language", "Bun only; no npm/pnpm/yarn", "AI SDK via Vercel AI Gateway only">

## Runtime / Commands
<which commands to run, in what order, to build / test / format>

## Code Organization
<which folders hold what; ownership boundaries; layering rules>

## Testing
<test runner, where tests live, what to mock vs hit live, fixture policy>

## Safety / Boundaries
<what must not be written to memory, what cannot be called from where, secrets handling>
```

Optional sections (add only when they earn their token cost):

- **Product Positioning** — what this repo is and is not.
- **Built-ins / preferred APIs** — when the language or runtime has built-ins to prefer over third-party packages.
- **MCP Boundary** — when the project exposes an MCP server.
- **Quick Reference** — short build/run/test command table.

Variants:

- **Nested `<area>/AGENTS.md`** in multi-area repos. The root file points to the area files; each area file owns its rules. The nearest file wins for that area.
- **`CONTRIBUTING.md`** alongside `AGENTS.md` when humans contribute under strict rules. Treat `CONTRIBUTING.md` as authoritative for what may be merged; `AGENTS.md` is agent-only operating context. Do not duplicate content between them — link instead.

### Pre-commit hooks

Every project on this style has a pre-commit hook that runs the project's format/lint/fix on staged files and re-stages anything that gets autofixed. The contract:

1. Hook fires on `git commit`.
2. It runs the project's canonical "format and fix" command on the staged set only.
3. Files modified by the autofix are re-staged so the commit reflects the fixed state.
4. If the command fails on something it cannot autofix, the commit is rejected.

**Default tool: Lefthook.** Lefthook is the default because it is language-agnostic, declared in a single `lefthook.yml`, runs in parallel, supports per-glob commands, and re-stages autofixes via `stage_fixed: true`. The stack skill (e.g. `react-stack/SKILL.md`) holds the canonical `lefthook.yml` for that stack.

**Alternatives are only acceptable when Lefthook cannot meet a specific need:**

| Alternative | Pick when |
| --- | --- |
| **Husky** (Node-only) | The project is Node-only and you want hooks declared inside `package.json` rather than a separate config file. Note: this gains nothing Lefthook doesn't already provide; pick only if the team has prior Husky muscle memory and no language-mixed concerns. |
| **`pre-commit` framework** (Python) | The project's primary language is Python and the team already uses the upstream `pre-commit` ecosystem (`.pre-commit-config.yaml`) for shared hook repos. |
| **Native `.git/hooks/pre-commit`** | The project must avoid adding any toolchain just for hooks (single-language, single-binary repos with strict zero-extra-deps rules). |

For each deviation, record the reason in the PR introducing the alternative. Do not skip hooks (`--no-verify` and equivalents) unless the user explicitly asks.

### Scripts directory

`scripts/` (or the language's equivalent) holds **one-off automation** that's not part of the build: env sync, secrets pull, seed data, codegen, asset copy, release notes generation, mock data generation.

**Language of the scripts follows the project's own constraints**, not a generic preference. Pick from these in order of preference for the host project:

1. **The project's primary language** when it can run scripts directly (TypeScript scripts in a TypeScript project run by the project's runtime; Python scripts in a Python project; Ruby scripts in a Ruby project).
2. **A `Makefile` / `Justfile` / `Taskfile`** when the project's primary language is compiled and recompiling for one-off work is heavy.
3. **A different dynamic language** (typically Python or Ruby) when the project's primary language cannot run direct scripts and the team accepts a second runtime locally — common in compiled-native projects.

Rules that apply regardless of language:

- One file per task.
- Each script is invokable directly with the language's runner.
- Each script is also wired into the project's manifest under a stable name (`package.json` `scripts:`, `Makefile` target, `taskfile`, `justfile`, etc.) so contributors don't need to remember the path.
- Exit non-zero on failure. Print actionable error context.
- Do not commit secrets, generated data, or large fixtures into the script files themselves.

Promote a script to part of the build pipeline only when more than one workflow depends on its output.

### `.agents/skills/` and `skills-lock.json`

`.agents/skills/` materialises curated agent skill packs into the repo. `skills-lock.json`, when present, is **output produced by the agent-skills CLI / sync command** — it is not authored by hand and not a project pattern to invent.

- Treat `.agents/skills/<name>/SKILL.md` and `skills-lock.json` as **generated**.
- Symlink `.claude/skills` → `.agents/skills` so Claude tooling sees the same materialised content.
- Do not hand-edit either path; run the skills tool to add, update, or remove a skill.
- This whole pattern is **opt-in** — not every project needs curated skill packs.

### Changelog (git-cliff)

Generate the changelog from conventional commits with **git-cliff**.

- `cliff.toml` at the repo root configures sections, commit-type mapping, and the template.
- Adopt conventional commit messages so types map cleanly to changelog sections (`feat`, `fix`, `chore`, `docs`, `refactor`, `test`, `perf`, `build`, `ci`, `style`, `revert`).
- Verify the current `git-cliff` version before running.
- `git-cliff --tag <new-tag>` produces the release notes; do not hand-edit them. If wording is wrong, edit the conventional commit and regenerate.

git-cliff is the default because it is language-agnostic, single-binary, reads commit history directly, and uses one declarative config file. **Alternatives are only acceptable when git-cliff cannot meet a specific need:**

| Alternative | Pick when |
| --- | --- |
| **release-please** (Google) | The project lives in a polyrepo of similar npm packages and relies on automated PR-based release flows tied to GitHub. |
| **conventional-changelog / standard-version** | The project is Node-only, cannot add a non-npm binary, and the team accepts the older toolchain. |
| **Hand-written `CHANGELOG.md`** | The project is small enough that the cost of automating exceeds the benefit, and the team commits to keeping the file by hand. |

For each deviation, record the reason in the PR introducing the alternative.

### Markdown linting (markdownlint)

Lint all committed markdown — `README.md`, `AGENTS.md`, `CONTRIBUTING.md`, `CHANGELOG.md`, everything under `docs/` — so structure, links, headings, and code-fence usage stay consistent across the repo.

**Default tool: markdownlint** (typically via `markdownlint-cli2`), configured by a single `.markdownlint.json` / `.markdownlint-cli2.yaml` at the repo root. Wire it into the project's verification gate and into the Lefthook pre-commit hook for staged `.md` files.

markdownlint is the default because it is the de-facto standard for markdown linting, has a stable preset of structural rules, and integrates cleanly with Lefthook. **Alternatives are only acceptable when markdownlint cannot meet a specific need:**

| Alternative | Pick when |
| --- | --- |
| **`vale`** (alongside or instead of markdownlint) | The project also needs prose-quality linting (style guide, sentence-level checks). `vale` is for prose, not markdown structure; use it as a complement when prose quality matters, or as the sole linter when the project's docs are prose-heavy and structure matters less. |
| **`remark-lint`** | The project is Node-only, already uses the unified/remark ecosystem, and wants plugin-based markdown processing. |

For each deviation, record the reason in the PR introducing the alternative.

### Duplication check contract

Every project on this style runs a duplication check that surfaces copy-paste across the codebase. The **implementation is the project's choice** (a multi-language tool, a language-specific analyzer, a wrapper around an existing codebase-health tool, etc.) as long as it satisfies this contract:

- **Reports duplication** above a project-defined size threshold — both within a single file and across files.
- **Output names the duplicated regions and their locations** so the regions can be merged or extracted.
- **Configurable thresholds and ignores** live in a single project-owned config file at the repo root.
- **Non-zero exit on threshold breach** so CI and the pre-commit hook can enforce it.

The choice of implementation and the configured thresholds are recorded in the project's `docs/tooling.md` (or equivalent). The check is wired into the project's verification gate.

### Link check contract

Every project on this style runs a link check on its markdown content and any other documentation surface. The **implementation is the project's choice** (a markdown-specific link checker, a generic HTTP link checker, a wrapper, etc.) as long as it satisfies this contract:

- **Verifies every link** in committed markdown — internal anchors, relative paths, and external URLs.
- **Differentiates internal and external links** so an internal-only mode can run offline / fast in CI, separate from a slower full-network mode.
- **Configurable allowlist** for known-broken or intentionally-unstable external links, recorded in a single project-owned config file at the repo root.
- **Non-zero exit when any link in scope is broken** so CI can enforce it.

The choice of implementation, the network-mode policy, and the allowlist are recorded in the project's `docs/tooling.md` (or equivalent). The check is wired into the project's verification gate.

### Architectural drift check contract

Every project on this style runs a drift check that detects mismatches between **what the project's documentation claims** and **what the code, manifest, and tooling actually do**, and outputs each mismatch as an actionable finding. This is the project's authoritative answer to "does the implementation still match the architecture and ways of working we wrote down?"

The **implementation is the project's choice** (a dependency analyzer, a docs-vs-code crawler, a wrapper around an existing drift / codebase-intelligence tool, a custom script, etc.) as long as it satisfies this contract.

**Surfaces in scope (the check covers all of them):**

- **Layering / module boundaries.** The architecture documented in `docs/architecture.md` (or equivalent) — which modules or layers may depend on which — vs. the actual import / dependency graph.
- **Hard Constraints.** The forbiddens declared in `AGENTS.md` (e.g. package-manager restrictions, AI lane restrictions, language restrictions) vs. what the manifest, lockfile, and source actually contain or allow.
- **Setup, run, test, and build commands.** The commands documented in `docs/quick-start.md` / `docs/tooling.md` / `docs/build-system.md` vs. the commands actually defined in the project's runner, manifest, or build entry point. Stale or missing commands are findings.
- **Ways of working.** Conventions documented in `docs/code-style.md` (naming patterns, file layout, export policy, import ordering, etc.) vs. the actual codebase.
- **Dependency declarations.** Tools and libraries documented as "in use" vs. the actual manifest and lockfile. Undocumented dependencies and documented-but-absent dependencies are both findings.
- **Environment and configuration documentation.** Variables and config keys documented in `docs/tooling.md` / `.env.example` vs. what the code actually reads. Gaps in either direction are findings.

**Output requirements:**

- **Each finding names the documented claim, the code reality, and the file locations involved** (with line numbers where applicable) so the source of truth is unambiguous.
- **Each finding indicates the resolution direction** when determinable: update the docs to match the code, update the code to match the docs, or update the rule itself.
- **Findings are grouped by surface** (layering, hard constraints, commands, ways of working, dependencies, environment) so reviewers can triage.
- **Configurable allowlist** for findings that are intentional mismatches, recorded in a single project-owned config file at the repo root. Each allowlist entry carries a comment justifying it.
- **Non-zero exit on any unallowed drift** so CI and the pre-commit hook can enforce it.

The choice of implementation, the configured surfaces, and the allowlist live alongside the architecture docs (typically `docs/architecture.md`) and `docs/tooling.md`. The check is wired into the project's verification gate.

This contract is **opt-out** only for projects with effectively no documentation surface (single-file scripts, throwaway tools). Every project that ships a `docs/` folder adopts the contract from the start.

### Multi-area repos

When a repo holds multiple deliverables (a web app, mock data tooling, a prototype, a CLI, etc.):

- Root `README.md` and root `AGENTS.md` cover the whole repo and point to area docs.
- Each area has its own subfolder with a local `README.md` and a local `AGENTS.md` if its rules differ from the root.
- The root `AGENTS.md` should explicitly say "scope your changes to the right area" and list the areas.
- Area-specific docs live under `docs/<area>/` or under the area folder itself; pick one and stay consistent.

## Examples

**Minimal repo (single language, small project):** `AGENTS.md` (Hard Constraints + Commands + Code Organization), `CLAUDE.md` symlinked to `AGENTS.md`, `README.md`, `docs/quick-start.md`, a manifest, a lockfile, a Lefthook pre-commit, and a `check` / `format` / `test` command set.

**Multi-area repo:** Root `AGENTS.md` (constraints common to the whole repo + table of areas), nested `<area>/AGENTS.md` for each area, `docs/` at the root for cross-cutting docs, `docs/<area>/` for area-specific docs.

**Strict-rules project with human contributors:** `CONTRIBUTING.md` (authoritative for merge requirements), `AGENTS.md` short and agent-only with a link to `CONTRIBUTING.md` for the rules, plus the standard `docs/` template.

**Compiled-native project:** Source under the project's chosen single root (`source/` or `src/`) using the stack's source-organization rule, native unit tests co-located with the unit they cover, end-to-end suites in the project's chosen top-level E2E location.
