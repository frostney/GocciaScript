# 0157 - Sandbox search is native and pinned by shared test vectors

**Date:** 2026-10-22
**Area:** `sandbox`, `embedding`
**Issue:** [#1794](https://github.com/frostney/GocciaScript/issues/1794)
**Related:** [ADR 0128](0128-regexp-vm-resource-limits.md), [ADR 0144](0144-read-only-paths-are-enforced-by-the-virtual-filesystem.md), [ADR 0151](0151-deterministic-sandbox-runs-stamp-files-with-the-deterministic-clock.md)

## Context

A sandboxed script could only search its virtual filesystem by walking it
with `fs.readdirSync` and `fs.statSync` and reading every file into the
engine. An agent working over a project tree needs three searches: by file
name (a glob), by content (literal text or a regular expression, with
positions) and by fuzzy name (a ranked "go to file" list). Embedders that
hold the same files outside the sandbox, such as a service that stores a
sandbox's files, want to answer the same questions without starting a run,
and get the same answers.

Issue #1794 left three questions open: whether content and fuzzy search
belong to the `goccia` module or a module of their own, how fuzzy matches
are ranked, and whether content search streams.

## Decision

**Names stay Node-shaped; content and fuzzy search get `goccia:search`.**
The sandbox `"fs"` module gains `globSync`, `glob` and `promises.glob`,
following Node's `fs.globSync` for a documented subset. `grep` and
`fuzzyFind` are not Node APIs, so they live in a separate import-only
module, `goccia:search`, which only sandbox mode registers. The `goccia`
module keeps orchestration (`$`, `runScript`).

**The search is native and walks the filesystem once.** Glob matching
(`SandboxGlob`) and fuzzy scoring (`SandboxFuzzy`) have no engine
dependency and live in `source/shared`; content search
(`Goccia.Sandbox.Search`) matches every line with the engine's own compiled
RegExp, so a pattern behaves as in `String.prototype.match` and keeps the
VM's step limit (ADR 0128). The walk reads directory listings and file bytes
through the filesystem's snapshot reads, so searching changes no timestamp
and adds no metadata change to a diff. Each file and line polls the run's
timeout and instruction limit; the decoded text of the file being searched
and every kept result are reserved against the memory limit.

**Fuzzy ranking is fzf's version 1 algorithm with its path scheme**, fixed
and not configurable: the first window that holds the query, narrowed from
the back, scored with fzf's match, gap and boundary constants over UTF-16
code units with ASCII-only case folding. It is cheap, well known, and
reproducible in a few dozen lines in any language.

**No streaming in this version.** `grep` returns `{ matches, truncated }`
with `maxResults` and `maxBytes` caps; a search that stops at a cap says so.
A streaming form can be added later without changing these results.

**One corpus holds every implementation to the same answers.**
`fixtures/sandbox-search/vectors.json` lists trees and cases with exact
expected results for globs, fuzzy matches, content matches (path, line,
column, line text, context) and errors, and
`scripts/test-cli-sandbox-search.ts` runs them through `GocciaRunner`. The
glob cases were checked against Node's own `fs.globSync`; they differ only
where the documentation says so (the `cwd` itself is never a result, and a
backslash is a separator). An embedder that searches outside the sandbox
tests against the same file.

## Considered options

- **Everything in `goccia`.** One import for orchestration and search, but
  it mixes two concerns in one module and makes `goccia` grow with every
  search option.
- **A configurable or optimal fuzzy matcher** (fzf version 2's
  Smith-Waterman alignment, or user weights). Better rankings in corner
  cases, but harder to reproduce exactly outside the engine, which is the
  point of the shared vectors.
- **A JavaScript implementation over `fs`.** Simpler to write, but every
  byte crosses into the engine and the search would update access times.

## Consequences

- `fs.globSync` does not support extended globs, POSIX classes, numeric
  brace ranges, `..` segments or an `exclude` function; brace expansion is
  capped at 1024 alternatives and one call's expanded patterns at 65,536
  code units, so a short pattern cannot allocate or match without bound.
- Changing a search rule means changing the vectors, which every
  implementation held to them then has to follow.
