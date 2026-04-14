# Workflow

*Branch workflow, local setup, and verification for every contributor.*

## Executive Summary

- **Local setup** — Install Lefthook for pre-commit formatting, then `lefthook install`
- **Branch workflow** — Branch from `main`, implement with [critical rules](../../CONTRIBUTING.md#critical-rules) and [code style](code-style.md), add tests, update docs, commit
- **Verification** — `./build.pas testrunner && ./build/TestRunner tests` before every push

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
2. **Implement** on that branch, following [Critical rules](../../CONTRIBUTING.md#critical-rules) and [Code style](code-style.md).
3. **Annotate spec references** — For ECMAScript behavior, add `// ESYYYY` spec comments as described in [ECMAScript spec annotations](code-style.md#ecmascript-spec-annotations).
4. **Add or update tests** — JavaScript tests under `tests/` are primary; Pascal units under `units/*.Test.pas` when you touch AST, evaluator, or value types. See [testing.md](../testing.md) and [Critical rules](../../CONTRIBUTING.md#critical-rules).
5. **Update documentation** that your change affects (`README.md`, `docs/*`, and CONTRIBUTING.md when workflow, rules, or style change). Edit **AGENTS.md** only when **agent-specific** guidance changes—not to duplicate CONTRIBUTING.
6. **Commit** with a clear message. Do not commit directly to `main`.

```bash
git add .
git commit -m "Short imperative description of the change"
```

## Issues and pull requests

- **Issues:** Use `.github/ISSUE_TEMPLATE/default.md` (Summary, Why, current vs expected behavior, scope).
- **Pull requests:** Use `.github/pull_request_template.md` (Summary with constraints and links, testing checklist).

## Verify changes

```bash
./build.pas testrunner && ./build/TestRunner tests
```

For interpreter/VM internals, also run native Pascal tests as described under [Testing](../testing.md).
