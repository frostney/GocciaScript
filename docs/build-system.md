# Build System

GocciaScript uses a self-hosted build script written in FreePascal, executed via `instantfpc`. No external build tools (Make, CMake, npm) are required beyond the FreePascal compiler itself.

## Prerequisites

- **FreePascal** (`fpc`) — Install via:
  - macOS: `brew install fpc`
  - Ubuntu/Debian: `sudo apt-get install fpc`
  - Windows: `choco install freepascal`

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
```

### Compile and Test

```bash
./build.pas testrunner && ./build/TestRunner tests
```

## Build Output

All compiled binaries go to the `build/` directory:

| Binary | Source | Description |
|--------|--------|-------------|
| `build/REPL` | `REPL.dpr` | Interactive read-eval-print loop |
| `build/ScriptLoader` | `ScriptLoader.dpr` | Execute `.js` files |
| `build/TestRunner` | `TestRunner.dpr` | JavaScript test runner |
| `build/BenchmarkRunner` | `BenchmarkRunner.dpr` | Performance benchmark runner |
| `build/Goccia.Values.Primitives.Test` | `*.Test.pas` | Pascal unit test binaries |

Intermediate files (`.o`, `.ppu`) also go to `build/` to keep the source tree clean.

## Compiler Configuration

### `config.cfg` (Shared Flags)

```
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
{H+}                              // Long strings (AnsiString) by default
{$IFNDEF PRODUCTION}
  {$overflowchecks on}            // Runtime arithmetic overflow detection
  {$rangechecks on}               // Runtime array bounds checking
{$ENDIF}
{$modeswitch advancedrecords}     // Records with methods
{$modeswitch multihelpers}        // Multiple class helpers for same type
```

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
4. Calls `fpc` with `@config.cfg`, mode-specific flags, and suppressed verbose output flags (`-vw-n-h-i-l-d-u-t-p-c-x-`).
5. If the `FPC_TARGET_CPU` environment variable is set, prepends `-P<arch>` to the compiler arguments (used by CI to target x86_64 on Windows where the FPC package defaults to i386).
6. For the `tests` target, auto-discovers all `*.Test.pas` files in `units/`.

## Project Structure for Compilation

```
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

```
build → test (JS + native)  → artifacts (main only)
      → benchmark           → release (tags only)
      → examples            →
```

Runs on the full platform matrix:

| OS | Architecture |
|----|-------------|
| Ubuntu Latest | x64, ARM64 |
| macOS Latest | x64, ARM64 |
| Windows Latest | x64 |

**`build`** — Installs FPC, compiles all binaries with `--prod`, uploads them as intermediate artifacts.

**`test`** (needs build) — Runs all JavaScript tests and Pascal unit tests on all platforms.

**`benchmark`** (needs build) — Runs all benchmarks on all platforms. On main (ubuntu-latest x64), saves benchmark results as JSON to `actions/cache` for PR comparison.

**`examples`** (needs build) — Runs all example scripts from the `examples/` folder on all platforms.

**`artifacts`** (needs test + benchmark + examples, main only) — Uploads production binaries after all checks pass.

**`release`** (needs test + benchmark + examples, tags only) — Downloads all platform build artifacts, bundles them with `tests/`, `benchmarks/`, and `examples/` into per-platform archives (`.tar.gz` for Linux/macOS, `.zip` for Windows), and creates a GitHub release using `softprops/action-gh-release`.

### `pr.yml` — Pull requests

```
build → test (JS only)
      → benchmark → PR comment (comparison)
```

Runs on **ubuntu-latest x64 only** (single runner, no matrix).

**`build`** — Installs FPC, compiles all binaries with `--prod`.

**`test`** (needs build) — Runs all JavaScript tests. No native tests.

**`benchmark`** (needs build) — Restores the cached benchmark baseline from main, runs all benchmarks with JSON output, and posts a comparison comment on the PR with per-benchmark percentage changes (green for improvements > 2%, red for regressions > 2%). If no baseline exists, shows results without percentages.

FPC is only installed once per platform in the `build` job. Test, benchmark, and example jobs run in parallel, using pre-built binaries.

## Direct FPC Compilation

If you need to bypass the build script:

```bash
fpc @config.cfg -vw-n-h-i-l-d-u-t-p-c-x- REPL.dpr
fpc @config.cfg -vw-n-h-i-l-d-u-t-p-c-x- ScriptLoader.dpr
fpc @config.cfg -vw-n-h-i-l-d-u-t-p-c-x- TestRunner.dpr
fpc @config.cfg -vw-n-h-i-l-d-u-t-p-c-x- BenchmarkRunner.dpr
```

The `-v` flags suppress verbose output to keep the build clean. Use `fpc @config.cfg REPL.dpr` for full verbose output during debugging.
