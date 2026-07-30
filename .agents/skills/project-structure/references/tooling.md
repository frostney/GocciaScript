# Repository tooling contracts

Use the project's language and existing commands. Record chosen implementations
and thresholds in `docs/tooling.md`; stack skills may narrow these contracts.

## Pre-commit

Lefthook is the default. On commit it runs the canonical format/fix command on
staged files, re-stages fixes, and rejects unfixable failures. Use another hook
system only for a concrete project constraint and record why. Never bypass hooks
unless the user explicitly asks.

## Scripts

Prefer the project's directly runnable language, then its existing task runner,
then an accepted dynamic-language fallback. Each script:

- owns one task and is callable directly;
- has a stable manifest/task-runner entry;
- exits non-zero with actionable context;
- contains no secrets, generated data, or large fixtures.

Promote it into the build only when multiple workflows depend on its output.

## Changelog

Use git-cliff with conventional commits by default. Keep `cliff.toml` at root,
verify the live tool version, exclude release commits, and generate rather than
hand-edit release notes. Use a different mechanism only for a documented
ecosystem need. The `create-release` skill owns release sequencing.

## Markdown and link checks

Use markdownlint by default for all committed Markdown. A replacement needs a
specific project reason.

The link check covers internal anchors, relative paths, and external URLs. It
supports a fast internal-only mode, a separately runnable network mode, one
justified allowlist, and a non-zero exit for broken in-scope links.

## Duplication check

Report duplicated regions within and across files above project thresholds.
Output locations, keep thresholds and ignores in one root config, and exit
non-zero on a breach.

## Architectural drift

For projects with a documentation surface, compare documented claims with:

- dependency/layer boundaries;
- AGENTS hard constraints;
- build, run, test, and setup commands;
- code-style and layout rules;
- manifest and lockfile dependencies;
- environment variables and configuration keys.

Each finding names the claim, observed reality, locations, and preferred
resolution direction. Group findings by surface, keep justified exceptions in
one allowlist, and exit non-zero for unallowed drift. Single-file or throwaway
tools with effectively no docs may opt out.
