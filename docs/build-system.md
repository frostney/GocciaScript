# Build System

*For contributors setting up their development environment or troubleshooting builds.*

## Executive Summary

- **Self-hosted build** — `./build.pas` is a FreePascal script via `instantfpc`; no Make/CMake/npm required
- **Two modes** — `--dev` (default: debug info, checks) and `--prod` (O4, stripped, smart-linked)
- **CI/CD** — `ci.yml` for main/tags (full platform matrix), `pr.yml` for PRs (ubuntu-latest x64 only, with benchmark comparison)
- **Auto-formatter** — `./format.pas` with Lefthook pre-commit hook enforces code style automatically

GocciaScript uses a self-hosted build script written in FreePascal, executed via `instantfpc`. No external build tools (Make, CMake, npm) are required beyond the FreePascal compiler itself.

## Prerequisites

See [Prerequisites](../README.md#prerequisites) in the README for FPC installation instructions.

- **instantfpc** — Comes bundled with FreePascal. Used to run `build.pas` as a script.

## Build Commands

### Build Modes

The build script supports two modes via `--dev` (default) and `--prod` flags:

```bash
./build.pas loader              # Dev build (default)
./build.pas --dev loader        # Explicit dev build
./build.pas --prod loader       # Production build
./build.pas --prod              # Production build of all components
./build.pas --prod loader repl  # Production build of specific components
```

### Build Everything

```bash
./build.pas           # Dev build of all components
./build.pas --prod    # Production build of all components
```

Runs a clean (removes stale `.ppu`, `.o`, `.res` from `build/`), then builds all components in order: tests, loader, testrunner, benchmarkrunner, repl.

### Build Specific Components

```bash
./build.pas repl             # Interactive REPL
./build.pas loader           # Script file executor
./build.pas testrunner       # JavaScript test runner
./build.pas benchmarkrunner  # Performance benchmark runner
./build.pas tests            # Pascal unit tests
```

Multiple components can be specified:

```bash
./build.pas loader repl
```

### Clean Build Artifacts

```bash
./build.pas clean              # Remove stale .ppu, .o, .res from build/
./build.pas clean loader       # Clean then build loader
```

A full build (no specific targets) automatically cleans first.

### Compile and Run

```bash
./build.pas loader && ./build/ScriptLoader ./example.js
printf "const x = 2 + 2; x;" | ./build/ScriptLoader
printf 'suite("stdin", () => { bench("sum", { run: () => 1 + 1 }); });\n' | ./build/BenchmarkRunner
```

Leading Unix shebang lines such as `#!/usr/bin/env goccia` are treated as comments by the lexer, so executable scripts can be run directly without preprocessing.

### Compile and Test

```bash
./build.pas testrunner && ./build/TestRunner tests
```

### Bytecode Mode

All execution tools support `--mode=bytecode` to compile and run via the Goccia bytecode VM instead of the tree-walk interpreter:

```bash
# Execute via bytecode VM
./build/ScriptLoader example.js --mode=bytecode
printf "const x = 2 + 2; x;" | ./build/ScriptLoader --mode=bytecode

# Emit bytecode to .gbc file (no execution)
./build/ScriptLoader example.js --emit
./build/ScriptLoader example.js --output=output.gbc
printf "const x = 2 + 2; x;" | ./build/ScriptLoader --emit --output=output.gbc

# Load and execute a pre-compiled .gbc file
./build/ScriptLoader output.gbc

# Emit structured JSON for programmatic consumers
printf "console.log('hi'); 2 + 2;" | ./build/ScriptLoader --output=json

# Inject globals from the CLI
printf "x + y;" | ./build/ScriptLoader --global x=10 --global y=20
printf "name;" | ./build/ScriptLoader --globals=context.json --output=json
printf "name;" | ./build/ScriptLoader --globals=context.json5 --output=json
printf "name;" | ./build/ScriptLoader --globals=context.toml --output=json
# `--global name=value` parses inline values as JSON only; `--globals=file` accepts JSON, JSON5, TOML, or YAML by file extension.
# Injected globals can override earlier injected values, but not built-in globals like console

# Load an explicit import map
./build/ScriptLoader app.js --import-map=imports.json

# Add one-off import-map-style aliases from the CLI
./build/ScriptLoader app.js --alias @/=./src/ --alias config=./config/default.js

# The same module-resolution flags are available on TestRunner, BenchmarkRunner, and REPL.
./build/TestRunner tests --import-map=imports.json --alias @/=./tests/helpers/
./build/BenchmarkRunner benchmarks --import-map=imports.json
./build/REPL --import-map=imports.json

# When --import-map is omitted, the CLI walks up from the entry file (or cwd for stdin/REPL)
# and uses the first goccia.json it finds.
printf 'import { add } from "@/math"; add(1, 2);' | ./build/ScriptLoader

# Abort long-running scripts
printf "const f = () => f(); f();" | ./build/ScriptLoader --timeout=100

# Write .map source map alongside execution
./build/ScriptLoader example.jsx --source-map --mode=bytecode

# Write source map to specific path with bytecode emit
./build/ScriptLoader example.jsx --source-map=out.map --emit

# Run tests via bytecode VM
./build/TestRunner tests --mode=bytecode

# Run benchmarks via bytecode VM
./build/BenchmarkRunner benchmarks --mode=bytecode
printf 'suite("stdin", () => { bench("sum", { run: () => 1 + 1 }); });\n' | ./build/BenchmarkRunner - --mode=bytecode
```

See [bytecode-vm.md](bytecode-vm.md) for the bytecode VM architecture and binary format.

## Build Output

All compiled binaries go to the `build/` directory:

| Binary | Source | Description |
|--------|--------|-------------|
| `build/REPL` | `REPL.dpr` | Interactive read-eval-print loop |
| `build/ScriptLoader` | `ScriptLoader.dpr` | Execute `.js` files or stdin input, with optional JSON output |
| `build/TestRunner` | `TestRunner.dpr` | JavaScript test runner |
| `build/BenchmarkRunner` | `BenchmarkRunner.dpr` | Performance benchmark runner for files or stdin input |
| `build/Goccia.Values.Primitives.Test` | `*.Test.pas` | Pascal unit test binaries |

Intermediate files (`.o`, `.ppu`) also go to `build/` to keep the source tree clean.

## Compiler Configuration

### `config.cfg` (Shared Flags)

```text
-Fu./units      # Unit search path
-Fi./units      # Include file search path
-FUbuild        # Unit output directory
-FEbuild        # Executable output directory
```

These path flags are shared by both build modes. Mode-specific flags are added by `build.pas`.

### Build Mode Flags

| Flag | Development (`--dev`) | Production (`--prod`) |
|------|----------------------|----------------------|
| Optimization | `-O-` (disabled) | `-O4` (aggressive) |
| Debug info | `-gw -godwarfsets` (DWARF) | — (none) |
| Line info | `-gl` (debug line numbers) | — |
| Stack checking | `-Ct` | — |
| Range checking | `-Cr` | — |
| Assertions | `-Sa` | — |
| Strip symbols | — | `-Xs` |
| Smart linking | — | `-CX -XX` |
| Define | — | `-dPRODUCTION` |

### `Goccia.inc`

Shared compiler directives included by all units:

```pascal
{$mode delphi}                    // Delphi-compatible syntax
{H+}                              // Long strings (`string` = `AnsiString`) by default
{$IFNDEF PRODUCTION}
  {$overflowchecks on}            // Runtime arithmetic overflow detection
  {$rangechecks on}               // Runtime array bounds checking
{$ENDIF}
{$modeswitch advancedrecords}     // Records with methods
```

Under the current project settings, FreePascal behaves like this:

- `string` is an `AnsiString` alias, not `UnicodeString`.
- `UTF8String` is also an `AnsiString`, but tagged with code page `CP_UTF8` (`65001`).
- `UnicodeString` remains the explicit UTF-16 string type.
- `Char` is a single-byte `AnsiChar`, so `Length`, `Copy`, indexing, and similar operations on `string`/`UTF8String` count bytes, not Unicode code points.

That distinction matters for parser and file-loading code: a plain `string` temporary does not preserve “this text is UTF-8” on its own. If raw UTF-8 file text needs to survive byte-for-byte until parsing, keep it in `UTF8String` (or retag it explicitly) until the parser consumes it.

Overflow and range checks are enabled in development mode for safety. In production builds, the `-dPRODUCTION` define disables these checks for maximum performance.

## How `build.pas` Works

The build script is a FreePascal program that runs via the `instantfpc` shebang:

```pascal
#!/usr/bin/env instantfpc
```

It:

1. Creates the `build/` directory if it doesn't exist.
2. Parses `--dev`/`--prod` flags to determine the build mode (defaults to `--dev`).
3. Parses remaining arguments to determine which components to build.
4. Counts and prints source statistics: total lines of code (LOC), significant lines of code (SLOC, excluding blanks and comments), and file count.
5. Calls `fpc` with `@config.cfg`, mode-specific flags, and suppressed verbose output flags (`-vw-n-h-i-l-d-u-t-p-c-x-`).
6. If the `FPC_TARGET_CPU` environment variable is set, prepends `-P<arch>` to the compiler arguments (used by CI to target x86_64 on Windows where the FPC package defaults to i386).
7. For the `tests` target, auto-discovers all `*.Test.pas` files in `units/`.

The GitHub Actions cross-compilation workflow uses a reduced cached FPC toolchain rather than a full target-side FCL install. It prebuilds the RTL, `rtl-objpas`, `rtl-generics`, and `fcl-process`, and also caches the official `fcl-base` and `regexpr` sources so cross builds can resolve units such as `Base64` and `RegExpr` on demand from the shipped FPC packages.

## Project Structure for Compilation

```text
GocciaScript/
├── build.pas          # Build script (entry point)
├── config.cfg         # Shared FPC configuration
├── REPL.dpr              # REPL program source
├── ScriptLoader.dpr      # Script loader program source
├── TestRunner.dpr        # Test runner program source
├── BenchmarkRunner.dpr   # Benchmark runner program source
├── units/
│   ├── Goccia.inc     # Shared compiler directives
│   ├── TimingUtils.pas # Cross-platform microsecond timing and duration formatting
│   ├── Goccia.Bytecode*.pas # Bytecode definitions, templates, modules, binary I/O
│   ├── Goccia.VM*.pas       # Bytecode VM, frames, closures, upvalues, exceptions
│   ├── *.pas          # All unit source files
│   └── *.Test.pas     # Pascal unit test programs
└── build/             # All output (gitignored)
    ├── *.o            # Object files
    ├── *.ppu          # Compiled unit files
    └── (binaries)     # Executable output
```

## CI/CD

GitHub Actions CI is split into two workflow files:

### `ci.yml` — Push to main + tags

```text
build → test (JS + native)   → artifacts (main only)
      → toml-compliance      →
      → benchmark            →
      → examples             →
```

All matrix strategies use `fail-fast: false`, so one platform failing does not cancel other platforms. The post-build jobs (`test`, `toml-compliance`, `benchmark`, `examples`) are independent.

Runs on the full platform matrix:

| OS | Architecture |
|----|-------------|
| Ubuntu Latest | x64, ARM64 |
| macOS Latest | x64, ARM64 |
| Windows Latest | x86 (i386-win32) |

**`build`** — Installs FPC, compiles the top-level `*.dpr` programs with `--prod`, stages those binaries plus Pascal test executables, and uploads that staged set as intermediate artifacts.

**`test`** (needs build) — Runs all JavaScript tests and Pascal unit tests on all platforms.

**`toml-compliance`** — Downloads the prebuilt `GocciaTOMLCheck` harness from each matrix build artifact, runs the official `toml-test` TOML 1.1.0 suite on every CI platform via `python3 scripts/run_toml_test_suite.py --harness=...`, validates that the JSON summary reports zero failures, and uploads the per-platform JSON report.

**`json5-compliance`** — Downloads the prebuilt `GocciaJSON5Check` harness and `TestRunner` binary from each matrix build artifact, runs `python3 scripts/run_json5_test_suite.py --harness=... --test-runner=...` on every CI platform, validates both the parser and stringify summaries, and uploads the per-platform JSON report.

**`benchmark`** (needs build) — Runs all benchmarks on all platforms. On main (ubuntu-latest x64), saves benchmark results as JSON to `actions/cache` for PR comparison.

**`examples`** (needs build) — Runs all example scripts from the `examples/` folder on all platforms.

**`artifacts`** (needs test + toml-compliance + json5-compliance + benchmark + examples, main only) — Uploads production binaries after all checks pass, deriving the executable names from the top-level `*.dpr` entrypoints.

**`release`** (needs test + toml-compliance + json5-compliance + benchmark + examples, tags only) — Downloads all platform build artifacts, stages only the shipped binaries derived from the top-level `*.dpr` entrypoints, bundles them with `tests/`, `benchmarks/`, and `examples/` into per-platform archives (`.tar.gz` for Linux/macOS, `.zip` for Windows), generates categorized release notes via [git-cliff](https://git-cliff.org/) (`cliff.toml`), and creates a GitHub release using `softprops/action-gh-release`.

### `pr.yml` — Pull requests

```text
build → test (JS + native)
      → benchmark → PR comment (comparison)
      → examples
```

Runs on **ubuntu-latest x64 only** (single runner, no matrix).

**`build`** — Installs FPC, compiles all binaries with `--prod`, stages the top-level `*.dpr` binaries plus Pascal test executables, and uploads that staged set as `build-pr`.

**`test`** (needs build) — Runs all JavaScript tests and Pascal unit tests.

**`benchmark`** (needs build) — Restores the cached benchmark baseline JSON from main, runs all benchmarks with JSON output, and posts a collapsible comparison comment on the PR grouped by file. Each file section shows the cached baseline and PR `opsPerSec` point estimates side by side, with each point estimate carrying its min/max ops/sec range in brackets. Classification uses range overlap: fully above the baseline range is an improvement, fully below is a regression, and overlapping ranges are treated as unchanged noise. Percentage deltas are still shown as secondary context, and files with significant changes are auto-expanded. If no baseline exists, shows results without comparison.

**`examples`** (needs build) — Runs all example scripts from the `examples/` folder.

FPC is only installed once per platform in the `build` job. In `ci.yml`, the test, benchmark, example, and TOML conformance jobs reuse the pre-built binaries and artifacts from that job; in `pr.yml`, the test, benchmark, and example jobs do the same.

## Changelog

The project maintains a `CHANGELOG.md` generated from git history using [git-cliff](https://git-cliff.org/). The configuration lives in `cliff.toml` at the project root.

### How It Works

Commits are categorized by their leading verb into groups:

| Verb prefix | Category |
|-------------|----------|
| `Add`, `Implement`, `Support`, `Allow`, `Create`, `Enable` | 🚀 Added |
| `Fix`, `Fixes`, `Route`, `Thread`, `Harden`, `Handle` | 🐛 Fixed |
| `Improve`, `Include`, `Make` | ✨ Improved |
| `Replace`, `Optimize`, `Promote`, `Eliminate`, `Bypass`, `Inline` | ⚡ Performance |
| `Remove` | 🗑️ Removed |
| `Refactor`, `Extract`, `Update`, `Strip` | 🏗️ Internal |

This table shows common prefixes. See `cliff.toml` for the complete list of patterns, which includes additional verb-specific and phrase-specific matchers.

### Generating the Changelog

```bash
# Install git-cliff
brew install git-cliff           # macOS
cargo install git-cliff          # Any platform with Rust

# Generate full changelog
git-cliff -o CHANGELOG.md

# Preview unreleased changes
git-cliff --unreleased

# Preview the latest release notes
git-cliff --latest --strip header
```

### CI Integration

The release job in `ci.yml` uses the [`orhun/git-cliff-action`](https://github.com/orhun/git-cliff-action) to generate categorized release notes for each tagged release. After the GitHub release is created, the job regenerates the full `CHANGELOG.md` and opens a PR to merge it into `main` (via a `changelog/<version>` branch). This avoids branch protection issues and keeps the changelog update visible in the normal review flow. GitHub's auto-generated release notes (`.github/release.yml`) provide an additional PR-label-based view.

### PR Labels

PRs should be labeled for the GitHub release notes categorization:

| Label | Category |
|-------|----------|
| `new feature` | 🚀 New Features |
| `spec compliance` | 📐 Compliance |
| `bug`, `os specific` | 🐛 Bug Fixes |
| `performance` | ⚡ Performance |
| `documentation` | 📖 Documentation |
| `internal` | 🏗️ Internal |

## Auto-Formatter

The project includes `./format.pas`, an `instantfpc` script that auto-fixes Pascal source files. It enforces uses clause ordering, PascalCase function names, parameter `A` prefix naming, and stray space removal (spurious spaces before `;`, `)`, `,`). No build step is needed — it runs directly via the `instantfpc` shebang.

```bash
./format.pas              # Format all project Pascal files
./format.pas --check      # Check only (exit 1 if changes needed)
./format.pas file.pas     # Format specific files
```

### Pre-Commit Hook

The formatter runs automatically on staged `.pas`/`.dpr` files before each commit via [Lefthook](https://github.com/evilmartians/lefthook). To enable this after cloning:

```bash
# Install Lefthook (see CONTRIBUTING.md for all platforms)
brew install lefthook     # macOS
sudo snap install lefthook  # Linux
scoop install lefthook    # Windows

# Register git hooks
lefthook install
```

The hook configuration is in `lefthook.yml`. It auto-stages formatting fixes (`stage_fixed: true`) so you don't need to `git add` after the hook reformats files.

## Direct FPC Compilation

If you need to bypass the build script:

```bash
fpc @config.cfg -vw-n-h-i-l-d-u-t-p-c-x- REPL.dpr
fpc @config.cfg -vw-n-h-i-l-d-u-t-p-c-x- ScriptLoader.dpr
fpc @config.cfg -vw-n-h-i-l-d-u-t-p-c-x- TestRunner.dpr
fpc @config.cfg -vw-n-h-i-l-d-u-t-p-c-x- BenchmarkRunner.dpr
```

The `-v` flags suppress verbose output to keep the build clean. Use `fpc @config.cfg REPL.dpr` for full verbose output during debugging.

## Build System Design Decision

The build script (`build.pas`) is a FreePascal script executed via `instantfpc` — a cross-platform, out-of-the-box solution within the FreePascal ecosystem that requires no external build tools.
