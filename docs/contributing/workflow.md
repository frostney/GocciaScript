# Workflow

*Local setup, GocciaScript change steps, and verification for every contributor.*

## Executive Summary

- **Local setup** — Install Lefthook for pre-commit formatting, then `lefthook install`
- **Change workflow** — Git and pull request mechanics follow known-good-route `git-workflow`; a GocciaScript change adds spec verification, tests, and docs or ADR updates
- **Pull request titles** — Conventional Commits, because `cliff.toml` builds the changelog from the squash-merge subject
- **Verification** — Run the all-executor JavaScript suite before every push: `./build.pas testrunner`, `./build/GocciaTestRunner -P tests`, and `./build/GocciaTestRunner -P tests --mode=bytecode` ([Testing requirements](../../CONTRIBUTING.md#3-testing-requirements) explains `-P`)

## Local setup

After cloning, install [Lefthook](https://github.com/evilmartians/lefthook) so `./format.pas` runs on pre-commit:

```bash
# macOS
brew install lefthook

# Linux (Snap)
sudo snap install lefthook

# Linux (APT — Debian/Ubuntu)
# See https://github.com/evilmartians/lefthook/blob/master/docs/install.md
curl -1sLf 'https://dl.cloudsmith.io/public/evilmartians/lefthook/setup.deb.sh' | sudo -E bash
sudo apt install lefthook

# Windows (Scoop)
scoop install lefthook

# Windows (Chocolatey)
choco install lefthook

# Any platform with Go installed
go install github.com/evilmartians/lefthook@latest

# Any platform with npm
npm install -g lefthook
```

Register hooks once per clone:

```bash
lefthook install
```

Formatting, editor integration, and CI behavior are covered in [Tooling](tooling.md).

## Feature workflow

Branching, commits, pushes, and pull requests follow known-good-route
[`git-workflow`](https://github.com/frostney/known-good-route/tree/main/git-workflow). A GocciaScript change also needs:

1. **Implementation** that follows [Implementation principles](../../CONTRIBUTING.md#implementation-principles), [Critical rules](../../CONTRIBUTING.md#critical-rules), [Code style](code-style.md), and the architecture docs for the area you touch.
2. **Spec verification and annotation.** For ECMAScript behavior, verify semantics against the current official ECMA-262 text, then add `// ESYYYY` spec comments as described in [ECMAScript spec annotations](code-style.md#ecmascript-spec-annotations).
3. **Tests.** JavaScript tests under `tests/` are primary. Add Pascal units under `source/units/*.Test.pas` when you touch AST, evaluator, or value types. See [testing.md](../testing.md).
4. **Documentation** per [CONTRIBUTING: Documentation](../../CONTRIBUTING.md#documentation). Record a new architectural or design decision as an ADR under [`docs/adr/`](../adr/).

## Issues and pull requests

Pull request titles are Conventional Commits ([`git-workflow` § Merge](https://github.com/frostney/known-good-route/blob/main/git-workflow/SKILL.md#merge)):
`cliff.toml` builds the changelog from the squash-merge subject.

### Code review scope

CodeRabbit reads `.coderabbit.config.ts`, which inherits the central
`frostney/coderabbit` settings and the web-UI settings and skips the vendored
Agent Skills. Every skill listed in `skills-lock.json` is installed from
upstream by the skills CLI and refreshed by
`.github/workflows/agent-skills-bump.yml`, so findings on it belong upstream. A
skill under `.agents/skills/` that the lock does not list is project-authored
and is reviewed like any other file. The config reads the lock through
`skills-lock.yaml`, a symlink, because CodeRabbit's config sandbox imports
`.yaml` but not `.json`.

`.github/delivery/review-automations.json` tells the `address-feedback` and
`delivery-wait` helpers which evidence shows CodeRabbit finished reviewing the
exact head: its `CodeRabbit` commit status and its reviews, excluding skipped,
paused, and rate-limited notices.

### Stacked pull requests

A stacked pull request targets the layer below it rather than `main`, so
CodeRabbit, which reviews only pull requests based on the default branch, skips
it. Request each layer's review through `/address-feedback`, whose CodeRabbit
adapter owns the trigger and the exact-head completion check; its
[stack reference](https://github.com/frostney/known-good-route/blob/main/address-feedback/references/stack.md)
owns when a review round ends.

The PR workflow itself runs for every pull request whatever its base. Every
stacked branch also needs a [full CI](#full-ci) run before it is marked ready.

## Verify changes

```bash
./build.pas testrunner
./build/GocciaTestRunner -P tests
./build/GocciaTestRunner -P tests --mode=bytecode
```

[Testing requirements](../../CONTRIBUTING.md#3-testing-requirements) explains `-P`
and the one-time `--trust` alternative.

For interpreter/VM internals, also run native Pascal tests as described under [Testing](../testing.md).

### Full CI

The PR workflow builds and tests on ubuntu-latest x64 only. Full CI (`ci.yml`,
six targets including Windows, macOS and AArch64) runs only on `main`, tags,
and manual dispatch. Before marking a pull request ready, run
`gh workflow run ci.yml --ref <branch>` for every stacked branch and for any
platform-sensitive change: file-system paths, processes, sockets or TLS, FFI,
time zones, `Int64`/`Double` conversion or byte layout (see
[Tooling: Platform-Specific Pitfalls](tooling.md#platform-specific-pitfalls)),
or any `{$IFDEF}` platform branch. A dispatched run's checks do not appear on
the pull request, and it belongs to the commit it started from, so check the
run for the branch's current head SHA rather than the newest run.
