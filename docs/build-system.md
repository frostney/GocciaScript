# Build System

GocciaScript uses a self-hosted build script written in FreePascal, executed via `instantfpc`. No external build tools (Make, CMake, npm) are required beyond the FreePascal compiler itself.

## Prerequisites

- **FreePascal** (`fpc`) — Install via:
  - macOS: `brew install fpc`
  - Ubuntu/Debian: `sudo apt-get install fpc`
  - Windows: `choco install freepascal`

- **instantfpc** — Comes bundled with FreePascal. Used to run `build.pas` as a script.

## Build Commands

### Build Everything

```bash
./build.pas
```

Builds all components in order: tests, loader, testrunner, benchmarkrunner, repl.

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

### `config.cfg`

```
-Fu./units      # Unit search path
-Fi./units      # Include file search path
-FUbuild        # Unit output directory
-FEbuild        # Executable output directory
-O3             # Optimization level 3
-gw             # Generate DWARF debug info
-godwarfsets    # DWARF set representation
```

### `Goccia.inc`

Shared compiler directives included by all units:

```pascal
{$mode delphi}                    // Delphi-compatible syntax
{H+}                              // Long strings (AnsiString) by default
{$overflowchecks on}              // Runtime arithmetic overflow detection
{$rangechecks on}                 // Runtime array bounds checking
{$modeswitch advancedrecords}     // Records with methods
{$modeswitch multihelpers}        // Multiple class helpers for same type
```

Overflow and range checks are enabled for safety. The `-O3` optimization flag in `config.cfg` ensures performance is still good despite the runtime checks.

## How `build.pas` Works

The build script is a FreePascal program that runs via the `instantfpc` shebang:

```pascal
#!/usr/bin/env instantfpc
```

It:

1. Creates the `build/` directory if it doesn't exist.
2. Parses command-line arguments to determine which components to build.
3. Calls `fpc` with `@config.cfg` and suppressed verbose output flags (`-vw-n-h-i-l-d-u-t-p-c-x-`).
4. If the `FPC_TARGET_CPU` environment variable is set, prepends `-P<arch>` to the compiler arguments (used by CI to target x86_64 on Windows where the FPC package defaults to i386).
5. For the `tests` target, auto-discovers all `*.Test.pas` files in `units/`.

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
│   ├── *.pas          # All unit source files
│   └── *.Test.pas     # Pascal unit test programs
└── build/             # All output (gitignored)
    ├── *.o            # Object files
    ├── *.ppu          # Compiled unit files
    └── (binaries)     # Executable output
```

## CI/CD

GitHub Actions CI is defined in `.github/workflows/ci.yml` with four jobs that form a pipeline:

```
build → test       → artifacts
      → benchmark  →
```

All jobs run across the full platform matrix:

| OS | Architecture |
|----|-------------|
| Ubuntu Latest | x64, ARM64 |
| macOS Latest | x64, ARM64 |
| Windows Latest | x64 |

### Pipeline Jobs

**`build`** — Installs FPC, compiles all binaries, uploads them as intermediate artifacts.

**`test`** (needs build) — Downloads binaries, runs JavaScript tests and Pascal unit tests.

**`benchmark`** (needs build) — Downloads binaries, runs all benchmarks on all platforms to detect platform-specific regressions.

**`artifacts`** (needs test + benchmark, push only) — Uploads release binaries after both test and benchmark pass.

FPC is only installed once per platform in the `build` job. Test and benchmark jobs run in parallel, using pre-built binaries.

## Direct FPC Compilation

If you need to bypass the build script:

```bash
fpc @config.cfg -vw-n-h-i-l-d-u-t-p-c-x- REPL.dpr
fpc @config.cfg -vw-n-h-i-l-d-u-t-p-c-x- ScriptLoader.dpr
fpc @config.cfg -vw-n-h-i-l-d-u-t-p-c-x- TestRunner.dpr
fpc @config.cfg -vw-n-h-i-l-d-u-t-p-c-x- BenchmarkRunner.dpr
```

The `-v` flags suppress verbose output to keep the build clean. Use `fpc @config.cfg REPL.dpr` for full verbose output during debugging.
