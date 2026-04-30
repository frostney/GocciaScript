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

Runs a clean (removes stale `.ppu`, `.o`, `.res` from `build/compiled/`), then builds all components in order: tests, loader, testrunner, benchmarkrunner, bundler, repl.

### Build Specific Components

```bash
./build.pas repl             # Interactive REPL
./build.pas loader           # Script file executor
./build.pas testrunner       # JavaScript test runner
./build.pas benchmarkrunner  # Performance benchmark runner
./build.pas bundler          # Bytecode bundler (compile to .gbc)
./build.pas tests            # Pascal unit tests
```

Multiple components can be specified:

```bash
./build.pas loader repl
```

### Clean Build Artifacts

```bash
./build.pas clean              # Remove stale .ppu, .o, .res from build/compiled/
./build.pas clean loader       # Clean then build loader
```

A full build (no specific targets) automatically cleans first.

### Compile and Run

```bash
./build.pas loader && ./build/GocciaScriptLoader ./example.js
printf "const x = 2 + 2; x;" | ./build/GocciaScriptLoader
printf 'suite("stdin", () => { bench("sum", { run: () => 1 + 1 }); });\n' | ./build/GocciaBenchmarkRunner
```

Leading Unix shebang lines such as `#!/usr/bin/env goccia` are treated as comments by the lexer, so executable scripts can be run directly without preprocessing.

### Compile and Test

```bash
./build.pas testrunner && ./build/GocciaTestRunner tests
```

### Bytecode Mode

All execution tools support `--mode=bytecode` to compile and run via the Goccia bytecode VM instead of the tree-walk interpreter:

```bash
# Execute via bytecode VM
./build/GocciaScriptLoader example.js --mode=bytecode
printf "const x = 2 + 2; x;" | ./build/GocciaScriptLoader --mode=bytecode

# Load and execute a pre-compiled .gbc file
./build/GocciaScriptLoader output.gbc

# Emit structured JSON for programmatic consumers
printf "console.log('hi'); 2 + 2;" | ./build/GocciaScriptLoader --output=json
# Emit a smaller JSON envelope without build, memory, stdout, or stderr.
# Console output remains in the normalized `output` array; errors stay in `error`.
printf "console.log('hi'); 2 + 2;" | ./build/GocciaScriptLoader --output=compact-json

# `compact-json` is recognised by every runner that emits JSON.
# - GocciaTestRunner: pass it as the value of --output. `--output=json` and
#   `--output=compact-json` emit JSON to stdout (suppressing the human-readable
#   summary); any other --output value is treated as a file path that receives
#   the full JSON envelope.
./build/GocciaTestRunner tests --output=compact-json
# - GocciaBenchmarkRunner: pass it as the value of --format alongside the
#   existing console/text/csv/json options. `--output=<path>` still selects the
#   destination file when provided.
./build/GocciaBenchmarkRunner benchmarks --format=compact-json --output=out.json

# Inject globals from the CLI
printf "x + y;" | ./build/GocciaScriptLoader --global x=10 --global y=20
printf "name;" | ./build/GocciaScriptLoader --globals=context.json --output=json
printf "name;" | ./build/GocciaScriptLoader --globals=context.json5 --output=json
printf "name;" | ./build/GocciaScriptLoader --globals=context.toml --output=json
# `--global name=value` parses inline values as JSON only; `--globals=file` accepts JSON, JSON5, TOML, or YAML by file extension.
# Injected globals can override earlier injected values, but not built-in globals like console

# Load an explicit import map
./build/GocciaScriptLoader app.js --import-map=imports.json

# Add one-off import-map-style aliases from the CLI
./build/GocciaScriptLoader app.js --alias @/=./src/ --alias config=./config/default.js

# The same module-resolution flags are available on GocciaTestRunner, GocciaBenchmarkRunner, and GocciaREPL.
./build/GocciaTestRunner tests --import-map=imports.json --alias @/=./tests/helpers/
./build/GocciaBenchmarkRunner benchmarks --import-map=imports.json
./build/GocciaREPL --import-map=imports.json

# REPL supports the same engine options as the script loader:
./build/GocciaREPL --log=repl.log           # Console log capture
./build/GocciaREPL --stack-size=5000         # Custom call stack depth limit
./build/GocciaREPL --max-memory=10485760     # 10 MB GC heap limit

# When --import-map is omitted, the CLI walks up from the entry file directory
# and uses the first goccia.json (or .json5 / .toml) it finds.
printf 'import { add } from "@/math"; add(1, 2);' | ./build/GocciaScriptLoader

# Abort long-running scripts
printf "const f = () => f(); f();" | ./build/GocciaScriptLoader --timeout=100

# Abort after a fixed number of bytecode instructions
printf "const f = () => f(); f();" | ./build/GocciaScriptLoader --max-instructions=1000000 --mode=bytecode

# Set call stack depth limit (default 3500; 0 = unlimited)
./build/GocciaScriptLoader example.js --stack-size=5000
./build/GocciaScriptLoader example.js --stack-size=0

# Write .map source map alongside execution
./build/GocciaScriptLoader example.jsx --source-map --mode=bytecode

# Run tests via bytecode VM
./build/GocciaTestRunner tests --mode=bytecode

# Control parallel worker threads (default: CPU count; --jobs=1 forces sequential)
./build/GocciaScriptLoader example.js --jobs=4
./build/GocciaTestRunner tests --jobs=4
./build/GocciaBenchmarkRunner benchmarks --jobs=1

# Run benchmarks via bytecode VM
./build/GocciaBenchmarkRunner benchmarks --mode=bytecode
printf 'suite("stdin", () => { bench("sum", { run: () => 1 + 1 }); });\n' | ./build/GocciaBenchmarkRunner - --mode=bytecode
```

### Configuration File (`goccia.json`)

All CLI options can also be set via a project configuration file. The CLI discovers the file by walking up from the **entry file's directory** and checking for (in priority order):

1. `goccia.toml`
2. `goccia.json5`
3. `goccia.json`

The first file found is loaded and applied as the **root config**. When running multiple files (e.g. via `GocciaTestRunner` or `GocciaBundler`), a **per-file config** is also discovered from each file's directory. The full precedence is:

1. **CLI arguments** (highest priority — always win)
2. **Per-file config** (`goccia.toml`, `goccia.json5`, or `goccia.json` nearest to the file being processed)
3. **Root config** (discovered from the entry path at startup, or supplied via `--config`)
4. **System default** (engine defaults)

**`--config=<path>`** — Override auto-discovery and load the root config from an explicit location. Available on every CLI tool.

The path may be either a **file** (any registered extension — `.json`, `.json5`, `.toml`; the parser is selected by extension), or a **directory**, in which case the CLI looks for `goccia.toml` → `goccia.json5` → `goccia.json` in that directory only (same priority as auto-discovery). The directory form does **not** walk upward — that's the point of the explicit override.

```bash
# File form (use this exact file)
./build/GocciaScriptLoader example.js --config=./configs/strict.toml
./build/GocciaTestRunner tests --config=./configs/ci.json

# Directory form (find goccia.{toml,json5,json} inside, no walk-up)
./build/GocciaScriptLoader example.js --config=./configs/
```

Relative paths are resolved against the current working directory. A missing file or a directory with no recognised `goccia.*` is a hard error so a typo is not silently ignored. CLI arguments still take precedence over values from the file, and per-file configs continue to be discovered normally for individual files.

```json
{
  "mode": "bytecode",
  "source-type": "module",
  "asi": true,
  "strict-types": true,
  "timeout": 5000,
  "max-memory": 10485760,
  "stack-size": 3500,
  "allowed-hosts": ["api.example.com", "cdn.example.com"],
  "imports": {
    "@/": "./src/"
  }
}
```

Config keys mirror CLI flag names (e.g. `--mode` → `"mode"`, `--max-memory` → `"max-memory"`). Boolean flags use `true`/`false`. Array values (like `alias` and `allowed-hosts`) use JSON arrays. The `imports` object is handled by the module resolver and coexists with CLI option keys.

**`extends`** — A config file can inherit from a base config using the `extends` key. The path is resolved relative to the config file's directory. Child values override parent values:

```json
{
  "extends": "../goccia.json",
  "mode": "bytecode"
}
```

This is useful for test subdirectories that need specific flags. For example, `tests/language/asi/goccia.json` can enable ASI for all tests in that folder:

```json
{
  "asi": true
}
```

TOML equivalent (`goccia.toml`):

```toml
asi = true
mode = "bytecode"
timeout = 5000
max-memory = 10485760
stack-size = 3500
```

**CLI vs. embedding** — Config file discovery is automatic for all CLI applications (`GocciaScriptLoader`, `GocciaTestRunner`, `GocciaBenchmarkRunner`, `GocciaBundler`, `GocciaREPL`) because they inherit from `TGocciaCLIApplication`. When embedding the engine directly, config file loading is not automatic. Use the shared `CLI.ConfigFile` unit to get the same behavior.

**Note:** `ApplyConfigFile` only handles `.json` out of the box. To support `.json5` and `.toml` config files, you must register their parsers first — the same way `TGocciaCLIApplication.Execute` does via `EnsureConfigParsersRegistered`. See `Goccia.CLI.Application.pas` for the registration pattern using `RegisterConfigParser`.

```pascal
uses CLI.ConfigFile, CLI.Options;

// Register parsers for JSON5 and TOML (required before discovery)
RegisterConfigParser('.json5', @ParseJSON5Config);
RegisterConfigParser('.toml', @ParseTOMLConfig);

// Discover a config file from a starting directory
ConfigPath := DiscoverConfigFile(EntryDir,
  ['goccia'], ['.toml', '.json5', '.json']);

// Parse it (with extends resolution) and apply to your options
if ConfigPath <> '' then
  ApplyConfigFile(ConfigPath, YourOptions);
```

For import-map resolution only (without CLI option loading), use the module resolver API directly:

```pascal
uses Goccia.Modules.Resolver;

ConfigPath := TGocciaModuleResolver.DiscoverProjectConfig(EntryDir);
if ConfigPath <> '' then
  Resolver.LoadImportMap(ConfigPath);
```

### GocciaBundler (Standalone Bytecode Compiler)

GocciaBundler is a dedicated tool for compiling source files to `.gbc` bytecode without executing them. It accepts the same input modes as GocciaScriptLoader (file, multiple files, directory, stdin):

```bash
# Compile a single file (output: example.gbc alongside the source)
./build/GocciaBundler example.js

# Custom output path
./build/GocciaBundler example.js --output=dist/example.gbc

# Compile all scripts in a directory (1:1 .gbc output alongside each source)
./build/GocciaBundler src/

# Compile a directory to a specific output directory
./build/GocciaBundler src/ --output=dist/

# Compile from stdin (--output required)
printf "const x = 2 + 2; x;" | ./build/GocciaBundler --output=out.gbc

# Multiple positional files
./build/GocciaBundler a.js b.js c.js

# Write source map alongside .gbc
./build/GocciaBundler example.jsx --source-map

# Explicit source map path (single file only)
./build/GocciaBundler example.jsx --source-map=out.map

# Enable automatic semicolon insertion during parsing
./build/GocciaBundler example.js --asi

# Parallel compilation (default: CPU count)
./build/GocciaBundler src/ --jobs=4
```

See [bytecode-vm.md](bytecode-vm.md) for the bytecode VM architecture and binary format.

## Build Output

All compiled binaries go to the `build/` directory:

| Binary | Source | Description |
|--------|--------|-------------|
| `build/GocciaREPL` | `source/app/GocciaREPL.dpr` | Interactive read-eval-print loop |
| `build/GocciaScriptLoader` | `source/app/GocciaScriptLoader.dpr` | Execute `.js` files or stdin input, with optional JSON output |
| `build/GocciaTestRunner` | `source/app/GocciaTestRunner.dpr` | JavaScript test runner |
| `build/GocciaBenchmarkRunner` | `source/app/GocciaBenchmarkRunner.dpr` | Performance benchmark runner for files or stdin input |
| `build/GocciaBundler` | `source/app/GocciaBundler.dpr` | Standalone bytecode compiler (source to `.gbc`) |
| `build/Goccia.Values.Primitives.Test` | `*.Test.pas` | Pascal unit test binaries |

Intermediate files (`.o`, `.ppu`) go to `build/compiled/` to keep the source tree clean.

## Compiler Configuration

### `config.cfg` (Shared Flags)

```text
-Fu./source/units    # Engine unit search path
-Fu./source/generated # Generated runtime data unit search path
-Fu./source/shared   # Shared infrastructure unit search path
-Fu./source/app      # CLI application unit search path
-Fi./source/units    # Engine include file search path
-Fi./source/shared   # Shared include file search path
-FUbuild/compiled         # Unit output directory
-FEbuild             # Executable output directory
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

### `Shared.inc` and `Goccia.inc`

Common compiler settings live in `source/shared/Shared.inc`, which is included by `source/units/Goccia.inc`. Engine units include `Goccia.inc` (which pulls in `Shared.inc` and adds Goccia-specific defines); shared infrastructure units include `Shared.inc` directly.

`Shared.inc` provides:

```pascal
{$mode delphi}                    // Delphi-compatible syntax
{$H+}                              // Long strings (`string` = `AnsiString`) by default
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
7. For the `tests` target, auto-discovers all `*.Test.pas` files in `source/units/` and `source/shared/`.

The GitHub Actions cross-compilation workflow uses a reduced cached FPC toolchain rather than a full target-side FCL install. It prebuilds the RTL, `rtl-objpas`, `rtl-generics`, and `fcl-process`, and also caches the official `fcl-base` and `regexpr` sources so cross builds can resolve units such as `Base64` and `RegExpr` on demand from the shipped FPC packages.

## Project Structure for Compilation

```text
GocciaScript/
├── build.pas          # Build script (entry point)
├── config.cfg         # Shared FPC configuration
├── source/
│   ├── app/              # CLI applications
│   │   ├── GocciaREPL.dpr              # REPL program source
│   │   ├── GocciaScriptLoader.dpr      # Script loader program source
│   │   ├── GocciaTestRunner.dpr        # Test runner program source
│   │   ├── GocciaBenchmarkRunner.dpr   # Benchmark runner program source
│   │   ├── GocciaBundler.dpr     # Standalone bytecode compiler
│   │   ├── Goccia.CLI.Application.pas  # CLI application base class
│   │   ├── Goccia.CLI.Help.pas         # CLI help generation
│   │   └── Goccia.CLI.EngineSetup.pas  # Engine setup utilities
│   ├── shared/           # Shared infrastructure
│   │   ├── Shared.inc    # Common compiler settings
│   │   ├── TestingPascalLibrary.pas  # Pascal test framework (TTestSuite)
│   │   ├── TimingUtils.pas   # Cross-platform timing and duration formatting
│   │   ├── HashMap.pas       # Purpose-built hash maps
│   │   ├── OrderedMap.pas    # Ordered hash maps
│   │   ├── OrderedStringMap.pas  # String-keyed ordered maps
│   │   ├── StringBuffer.pas  # String buffer
│   │   ├── FileUtils.pas     # File utilities
│   │   ├── BaseMap.pas       # Base map type
│   │   └── JSONParser.pas    # JSON parser
│   ├── generated/        # Generated runtime data
│   │   ├── Generated.TimeZoneData.pas # Timezone resource metadata/link unit
│   │   └── Generated.TimeZoneData.res # Embedded IANA TZif resource
│   └── units/            # Goccia engine units
│       ├── Goccia.inc    # Goccia compiler directives (includes Shared.inc)
│       ├── Goccia.Bytecode*.pas # Bytecode definitions, templates, modules, binary I/O
│       ├── Goccia.VM*.pas       # Bytecode VM, frames, closures, upvalues, exceptions
│       ├── *.pas         # All engine unit source files
│       └── *.Test.pas    # Pascal unit test programs
└── build/             # All output (gitignored)
    ├── compiled/      # Intermediate files
    │   ├── *.o        # Object files
    │   └── *.ppu      # Compiled unit files
    └── (binaries)     # Executable output
```

### Generated Timezone Data

`source/generated/Generated.TimeZoneData.pas` and `source/generated/Generated.TimeZoneData.res` are produced by `scripts/generate-timezone-data.js`. By default, the generator downloads the latest IANA `tzdata-latest.tar.gz`, compiles it with `zic`, packs the resulting TZif files into a single resource payload, and emits a small Pascal unit that links the resource. Pass a local zoneinfo directory, a local `tzdata` tarball, or an explicit URL to generate from a different source.

The generator requires `zic`, `tar`, and `fpcres`. `fpcres` writes the FreePascal resource consumed by `{$R Generated.TimeZoneData.res}`.

Temporal embeds this generated timezone resource by default through `source/units/Goccia.inc`. Define `GOCCIA_TEMPORAL_NO_EMBEDDED_TZDATA` to build without the resource fallback.

## CI/CD

GitHub Actions CI is split into two workflow files:

### `ci.yml` — Push to main + tags

```text
build → test (JS + native)   → artifacts (main only)
      → toml-compliance      →
      → json5-compliance     →
      → test262              →
      → benchmark            →
      → cli                  →
```

All matrix strategies use `fail-fast: false`, so one platform failing does not cancel other platforms. The post-build jobs (`test`, `toml-compliance`, `json5-compliance`, `test262`, `benchmark`, `cli`) are independent.

Runs on the full platform matrix:

| OS | Architecture |
|----|-------------|
| Ubuntu Latest | x64, ARM64 |
| macOS Latest | x64, ARM64 |
| Windows Latest | x86 (i386-win32) |

**`build`** — Installs FPC, compiles the `source/app/*.dpr` programs with `--prod`, stages those binaries plus Pascal test executables, and uploads that staged set as intermediate artifacts.

**`test`** (needs build) — Runs all JavaScript tests and Pascal unit tests on all platforms.

**`toml-compliance`** — Downloads the prebuilt `GocciaTOMLCheck` harness from each matrix build artifact, runs the official `toml-test` TOML 1.1.0 suite on every CI platform via `python3 scripts/run_toml_test_suite.py --harness=...`, validates that the JSON summary reports zero failures, and uploads the per-platform JSON report.

**`json5-compliance`** — Downloads the prebuilt `GocciaJSON5Check` harness and `GocciaTestRunner` binary from each matrix build artifact, runs `python3 scripts/run_json5_test_suite.py --harness=... --test-runner=...` on every CI platform, validates both the parser and stringify summaries, and uploads the per-platform JSON report.

**`test262`** (needs build, ubuntu-latest x64 only, **non-blocking**) — Downloads the `gocciascript-x86_64-linux` build, shallow-clones [`tc39/test262@main`](https://github.com/tc39/test262), runs `python3 scripts/run_test262_suite.py --suite-dir test262-suite --mode=bytecode --jobs=4 --output=test262-results.json`, and uploads the report as a 30-day workflow artifact. The run step uses `continue-on-error: true` because GocciaScript intentionally excludes parts of the language test262 covers — this lane is an indicator metric, not a regression gate. On main, the JSON is also stashed via `actions/cache/save` under `test262-baseline-<sha>` so the PR workflow can compute Δ vs main. See [docs/testing.md](testing.md#ci-integration) for the comment format.

**`benchmark`** (needs build) — Runs all benchmarks on all platforms. On main (ubuntu-latest x64), saves benchmark results as JSON to `actions/cache` for PR comparison.

**`cli`** (needs build) — Downloads pre-built binaries and runs CLI behavior smoke tests on all platforms via Bun: flags across all apps, lexer numeric-separator rejection, parser error display, config-file loading, and app-specific features. Windows runs additionally assert that the loader binary does not link OpenSSL DLLs (HTTPS must use the platform TLS stack statically).

**`artifacts`** (needs test + toml-compliance + json5-compliance + benchmark + cli, main only) — Uploads production binaries after all checks pass, deriving the executable names from the `source/app/*.dpr` entrypoints.

**`release`** (needs test + toml-compliance + json5-compliance + benchmark + cli, tags only) — Downloads all platform build artifacts, stages only the shipped binaries derived from the `source/app/*.dpr` entrypoints, bundles them with `tests/`, `benchmarks/`, and `examples/` into per-platform archives (`.tar.gz` for Linux/macOS, `.zip` for Windows), generates categorized release notes via [git-cliff](https://git-cliff.org/) (`cliff.toml`), and creates a GitHub release using `softprops/action-gh-release`.

### `pr.yml` — Pull requests

```text
build → test (JS + native)
      → benchmark → PR comment (comparison)
      → test262   → PR comment (conformance)
      → cli
```

Runs on **ubuntu-latest x64 only** (single runner, no matrix).

**`build`** — Installs FPC, compiles all binaries with `--prod`, stages the `source/app/*.dpr` binaries plus Pascal test executables, and uploads that staged set as `build-pr`.

**`test`** (needs build) — Runs all JavaScript tests and Pascal unit tests.

**`benchmark`** (needs build) — Restores the cached benchmark baseline JSON from main, runs all benchmarks with JSON output, and posts a collapsible comparison comment on the PR grouped by file. Each file section shows the cached baseline and PR `opsPerSec` point estimates side by side, with each point estimate carrying its min/max ops/sec range in brackets. Classification uses range overlap: fully above the baseline range is an improvement, fully below is a regression, and overlapping ranges are treated as unchanged noise. Percentage deltas are still shown as secondary context, and files with significant changes are auto-expanded. If no baseline exists, shows results without comparison.

**`test262`** (needs build, **non-blocking**) — Shallow-clones `tc39/test262@main`, runs `python3 scripts/run_test262_suite.py --suite-dir test262-suite --mode=bytecode --jobs=4 --output=test262-results.json`, and uploads the JSON report. Failing tests do not fail the job. The downstream `test262-comment` job (`if: always()`) restores the most recent `test262-baseline-` cache entry from main, computes a four-row metric table (Total / Eligible / Eligible passing / Total passing) and a top-three "Areas closest to 100%" table (≥ 25 attempted tests, below 100%, keyed by the first two test262 path components), and posts/updates a comment using marker `<!-- test262-results -->`. When a baseline is available, both tables include a Δ vs main column.

**`cli`** (needs build) — Runs CLI behavior smoke tests via Bun (`scripts/test-cli.ts`, `scripts/test-cli-lexer.ts`, `scripts/test-cli-parser.ts`, `scripts/test-cli-config.ts`, `scripts/test-cli-apps.ts`).

FPC is only installed once per platform in the `build` job. In `ci.yml`, the test, benchmark, cli, TOML, JSON5, and test262 conformance jobs reuse the pre-built binaries and artifacts from that job; in `pr.yml`, the test, benchmark, test262, and cli jobs do the same.

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
| Commits touching `website/` | 🌐 Website |

This table shows common prefixes. See `cliff.toml` for the complete list of patterns, which includes additional verb-specific and phrase-specific matchers. Website commits are detected by changed path before prefix grouping so site changes stay separate from language/runtime changes. Release sections are generated only for semver-style tags such as `0.7.0` or `v0.7.0`; moving tags such as `nightly` are ignored.

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
fpc @config.cfg -vw-n-h-i-l-d-u-t-p-c-x- source/app/GocciaREPL.dpr
fpc @config.cfg -vw-n-h-i-l-d-u-t-p-c-x- source/app/GocciaScriptLoader.dpr
fpc @config.cfg -vw-n-h-i-l-d-u-t-p-c-x- source/app/GocciaTestRunner.dpr
fpc @config.cfg -vw-n-h-i-l-d-u-t-p-c-x- source/app/GocciaBenchmarkRunner.dpr
fpc @config.cfg -vw-n-h-i-l-d-u-t-p-c-x- source/app/GocciaBundler.dpr
```

The `-v` flags suppress verbose output to keep the build clean. Use `fpc @config.cfg source/app/GocciaREPL.dpr` for full verbose output during debugging.

## Build System Design Decision

The build script (`build.pas`) is a FreePascal script executed via `instantfpc` — a cross-platform, out-of-the-box solution within the FreePascal ecosystem that requires no external build tools.
