# Definition of Ready

Use this gate before implementing an issue, idea, refactor, or documentation change in GocciaScript. If an item does not apply, say why before proceeding.

## Ready to Investigate

- The desired outcome, current behavior, affected user or contributor surface, and non-goals are clear enough to verify later.
- The work has been checked against [VISION.md](VISION.md). Any conflict with the sandbox-first, embeddable, conformance-aware direction is explicit and accepted before implementation.
- The root [AGENTS.md](AGENTS.md), [CONTRIBUTING.md](CONTRIBUTING.md), and any nearest area-specific agent or contribution guidance have been read.
- Applicable project skills under `.agents/skills/` have been identified before planning.
- The relevant architecture, testing, and code-style docs have been identified before forming a plan. Start with [Architecture](docs/architecture.md), [Core patterns](docs/core-patterns.md), [Code Style](docs/contributing/code-style.md), and [Testing](docs/testing.md), then narrow by affected area.

## Ready to Plan

- The relevant code path has been traced broadly enough to identify the owning layer, using [Architecture](docs/architecture.md) and [CONTEXT.md](CONTEXT.md) for terminology.
- Existing behavior, tests, sibling implementations, and conventions have been searched before proposing new code.
- ECMAScript or ECMA-402 behavior has been checked against the official specification before accepting semantics. Use the project TC39 MCP server when available.
- New or restructured Pascal units have a stated public API and a justified home under `source/shared/` or `source/units/`, following [Source Directory Layout and Namespacing](docs/contributing/code-style.md#source-directory-layout-and-namespacing).
- The testing strategy has been identified from [Testing](docs/testing.md), including the public behavior surface that will be covered.

## Ready to Edit

- The current branch or worktree is up to date against the latest remote default branch: fetch the remote default branch and merge it into the working branch/worktree before editing, following the project `git-workflow` skill. Do not rebase.
- Documentation impact is known according to [CONTRIBUTING.md](CONTRIBUTING.md#documentation) and any more specific affected-area guidance.
- Existing local changes have been reviewed enough to avoid overwriting unrelated user work.
