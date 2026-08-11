# Workflow

*Branch workflow, local setup, and verification for every contributor.*

## Executive Summary

- **Local setup** — Install Lefthook for pre-commit formatting, then `lefthook install`
- **Branch workflow** — Branch from `main`, implement with [implementation principles](../../CONTRIBUTING.md#implementation-principles), [critical rules](../../CONTRIBUTING.md#critical-rules), and [code style](code-style.md), add tests, update docs, commit
- **Verification** — Run the all-executor JavaScript suite before every push: `./build.pas testrunner`, `./build/GocciaTestRunner tests`, and `./build/GocciaTestRunner tests --mode=bytecode`

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

Every change should follow this sequence:

1. **Create a branch** from `main` with a descriptive name (for example `feature/string-prototype-repeat`, `fix/nan-comparison`).
2. **Implement** on that branch, following [Implementation principles](../../CONTRIBUTING.md#implementation-principles), [Critical rules](../../CONTRIBUTING.md#critical-rules), [Code style](code-style.md), and the relevant architecture docs for the area you touch.
3. **Verify and annotate spec references** — For ECMAScript behavior, verify semantics against the current official ECMA-262 text, then add `// ESYYYY` spec comments as described in [ECMAScript spec annotations](code-style.md#ecmascript-spec-annotations).
4. **Add or update tests** — JavaScript tests under `tests/` are primary; Pascal units under `source/units/*.Test.pas` when you touch AST, evaluator, or value types. See [testing.md](../testing.md) and [Critical rules](../../CONTRIBUTING.md#critical-rules).
5. **Update documentation** that your change affects (`README.md`, `docs/*`, and CONTRIBUTING.md when workflow, rules, or style change). Edit **AGENTS.md** only when **agent-specific** guidance changes—not to duplicate CONTRIBUTING. If the change introduces a new **architectural or design decision** (not just a feature addition), create an ADR under [`docs/adr/`](../adr/).
6. **Commit** with a clear message. Do not commit directly to `main`.

```bash
git add .
git commit -m "Short imperative description of the change"
```

## Issues and pull requests

- **Issues:** Use `.github/ISSUE_TEMPLATE/default.md` (Summary, Why, current vs expected behavior, scope).
- **Pull requests:** Use `.github/pull_request_template.md` (Summary with constraints and links, testing checklist).
- **Pull request titles are Conventional Commits.** Pull requests are
  squash-merged, so the title — not the branch's commits — becomes the commit
  subject on `main`, and `cliff.toml` parses that subject to build the changelog.
  A title that does not match `type(scope): summary` still merges and is then
  silently absent from the release notes. Pick the type from the change as a
  whole: `feat` when the net effect is new capability, even if most of the
  commits under it are fixes.

### Stacked pull requests

A stacked pull request targets the layer below it rather than `main`, and
automation treats that base differently from a normal branch. Two consequences
are worth knowing before starting a stack:

- **Automatic review does not fire.** CodeRabbit reviews only pull requests
  based on the default branch, so every layer needs an explicit
  `@coderabbitai review` comment. A layer that is never triggered shows no
  review at all, and an instant acknowledgement of an already-reviewed commit
  is not a review of the current head.
- **A review round ends when its own fix layer reviews clean**, not when the
  findings from the layer below are dispositioned. A round that fixes findings
  creates a new top layer, and that layer needs its own review like any other.
  Confirm every layer has a review before calling a stack finished — the
  terminating one is the easiest to miss, because nothing after it prompts a
  sweep.

The PR workflow itself runs for every pull request whatever its base; it
previously filtered on `main`, which skipped stacked layers entirely. Full CI
(`ci.yml`) still runs only on `main`, tags, and manual dispatch, so use
`gh workflow run ci.yml --ref <branch>` when a stacked branch needs the full
matrix, and check the result against the branch's current head SHA rather than
the newest run — a dispatched run belongs to the commit it started from.

## Verify changes

```bash
./build.pas testrunner
./build/GocciaTestRunner tests
./build/GocciaTestRunner tests --mode=bytecode
```

For interpreter/VM internals, also run native Pascal tests as described under [Testing](../testing.md).
