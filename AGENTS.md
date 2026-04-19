# AGENTS.md

Instructions for **AI coding assistants** (Cursor, Claude Code, and similar) working in this repository.

## Contributing guide vs this file

| | Purpose |
|---|--------|
| **[CONTRIBUTING.md](CONTRIBUTING.md)** | **Contributing guide for all contributors**—humans and agents. Workflow, critical rules, testing, Pascal code style, formatting, build/run reference, documentation index. If it affects what may be merged, it belongs there. |
| **This file (`AGENTS.md` / `CLAUDE.md`)** | **Agent-only** context: how assistants should operate in this repo, where to read first, and what not to duplicate. It does **not** replace CONTRIBUTING. |

Assistants should treat CONTRIBUTING as authoritative for contribution requirements. Use this file for assistant-specific expectations and navigation, not a second copy of CONTRIBUTING.

## Expectations for assistants

- **Read [CONTRIBUTING.md](CONTRIBUTING.md)** before substantive edits—especially [Critical rules](CONTRIBUTING.md#critical-rules) and [Code style](docs/contributing/code-style.md).
- **Run verification yourself** when the environment allows (tests, format check); do not only tell the human what to run unless execution is impossible.
- **Match the project's workflow**: branch from `main`, focused diffs, tests and docs updated per CONTRIBUTING.
- **Do not paste large chunks of CONTRIBUTING into this file** when CONTRIBUTING changes—edit CONTRIBUTING instead, and keep AGENTS short.

## Quick checks

```bash
./build.pas testrunner && ./build/GocciaTestRunner tests --asi --unsafe-ffi  # after substantive changes
./build.pas bundler && ./build/GocciaBundler example.js  # build and run the bundler
./format.pas --check # before push / PR
```

## Quick reference

### Build commands

```bash
./build.pas # Clean + dev build of everything (default)
./build.pas --dev loader # Dev build of GocciaScriptLoader
./build.pas --prod # Clean + production build of everything
./build.pas --prod loader repl # Production build of specific components
./build.pas loader # Dev build (--dev is the default)
./build.pas testrunner # Dev build of GocciaTestRunner
./build.pas benchmarkrunner # Dev build of GocciaBenchmarkRunner
./build.pas bundler # Dev build of GocciaBundler
./build.pas tests # Dev build of Pascal unit tests
./build.pas clean # Clean stale artifacts only (no build)
./build.pas clean loader # Clean, then dev build of ScriptLoader
```

### Run commands

```bash
./build/GocciaScriptLoader example.js # Execute a script (interpreted)
./build/GocciaScriptLoader example.js --mode=bytecode # Execute via bytecode VM
./build/GocciaScriptLoader example.js --import-map=imports.json # Execute with an explicit import map
./build/GocciaScriptLoader example.js --alias @/=./src/ --alias config=./config/default.js # One-off import-map-style aliases
./build/GocciaScriptLoader out.gbc # Load and execute .gbc bytecode
printf "const x = 2 + 2; x;" | ./build/GocciaScriptLoader # Execute stdin source
./build/GocciaScriptLoader example.js --coverage # Execute with line and branch coverage
./build/GocciaScriptLoader example.js --coverage --coverage-format=lcov --coverage-output=coverage.lcov # Coverage with lcov output
./build/GocciaScriptLoader example.js --coverage --coverage-format=json --coverage-output=coverage.json # Coverage with JSON output
./build/GocciaScriptLoader example.js --asi # Execute with automatic semicolon insertion
./build/GocciaScriptLoader example.js --profile=opcodes # Opcode histogram, pair frequency, scalar hit rate (bytecode)
./build/GocciaScriptLoader example.js --profile=functions # Function self-time, allocations (bytecode)
./build/GocciaScriptLoader example.js --profile=all # All profiling data (bytecode)
./build/GocciaScriptLoader example.js --profile=all --profile-output=profile.json # Profile with JSON export
./build/GocciaScriptLoader example.js --profile=functions --profile-format=flamegraph --profile-output=flamegraph.txt # Flame graph export
./build/GocciaScriptLoader example.jsx --source-map --mode=bytecode # Write .map source map alongside execution
./build/GocciaScriptLoader example.js --max-instructions=1000000 --mode=bytecode # Abort after 1M bytecode instructions
./build/GocciaScriptLoader example.js --unsafe-ffi # Execute with FFI enabled
./build/GocciaScriptLoader example.js --log=console.log # Write console output to a log file (tee to stdout + file)
./build/GocciaREPL # Start interactive REPL (interpreted)
./build/GocciaREPL --mode=bytecode # Start the REPL via bytecode VM
./build/GocciaREPL --mode=bytecode --timing # Bytecode REPL with per-line timing
./build/GocciaREPL --import-map=imports.json # Start the REPL with an explicit import map
./build/GocciaREPL --asi # Start the REPL with automatic semicolon insertion
./build/GocciaREPL --unsafe-ffi # Start the REPL with FFI enabled
./build/GocciaTestRunner tests/ --asi # Run all JavaScript tests
./build/GocciaTestRunner tests --import-map=imports.json # Run tests with an explicit import map
./build/GocciaTestRunner tests/language/expressions/ # Run a test category
./build/GocciaTestRunner tests --no-progress --exit-on-first-failure # CI mode
./build/GocciaTestRunner tests --silent # Suppress all console output
./build/GocciaTestRunner tests --output=results.json # Write test results as JSON
./build/GocciaTestRunner tests --mode=bytecode # Run tests via the Goccia bytecode VM
./build/GocciaTestRunner tests/language/asi --asi # Run ASI tests with automatic semicolon insertion
./build/GocciaTestRunner tests --coverage # Run tests with line and branch coverage
./build/GocciaTestRunner tests --coverage --coverage-format=lcov --coverage-output=coverage.lcov # Coverage with lcov output
./build/GocciaTestRunner tests --coverage --coverage-format=json --coverage-output=coverage.json # Coverage with JSON output
./build/GocciaTestRunner tests --jobs=4 # Run tests with 4 parallel workers
./build/GocciaTestRunner tests -j 1 # Force sequential execution (no threading)
./build/GocciaTestRunner tests --asi --unsafe-ffi # Run all tests including FFI tests
./build/GocciaTestRunner tests --log=test-console.log # Capture console output to a log file
./build/GocciaBenchmarkRunner benchmarks/ # Run all benchmarks
./build/GocciaBenchmarkRunner benchmarks --import-map=imports.json # Run benchmarks with an explicit import map
./build/GocciaBenchmarkRunner benchmarks/fibonacci.js # Run a specific benchmark
printf 'suite("stdin", () => { bench("sum", { run: () => 1 + 1 }); });\n' | ./build/GocciaBenchmarkRunner # Run benchmark source from stdin
./build/GocciaBenchmarkRunner benchmarks --format=json --output=out.json # Export as JSON
./build/GocciaBenchmarkRunner benchmarks --format=console --format=json --output=out.json # Console + JSON
./build/GocciaBenchmarkRunner benchmarks --no-progress # Suppress progress (CI)
./build/GocciaBenchmarkRunner benchmarks --mode=bytecode # Benchmarks via the Goccia bytecode VM
./build/GocciaBenchmarkRunner benchmarks --jobs=4 # Run benchmarks with 4 parallel workers
./build/GocciaBundler example.js # Compile to .gbc (no execution)
./build/GocciaBundler example.js --output=out.gbc # Custom output path
./build/GocciaBundler src/ # Compile all scripts in a directory
./build/GocciaBundler src/ --output=dist/ # Compile directory to output directory
./build/GocciaBundler example.js --source-map # Write .map source map alongside .gbc
printf "const x = 2 + 2; x;" | ./build/GocciaBundler --output=out.gbc # Compile stdin to .gbc
./build/GocciaBundler src/ --jobs=4 # Compile with 4 parallel workers
./build/GocciaBundler example.js --asi # Compile with automatic semicolon insertion
```

### Compile and run (common workflows)

```bash
# Compile and run a script
./build.pas loader && ./build/GocciaScriptLoader ./example.js

# Compile and run all tests
./build.pas testrunner && ./build/GocciaTestRunner tests --asi --unsafe-ffi

# Compile and run a specific test
./build.pas testrunner && ./build/GocciaTestRunner tests/language/expressions/addition/basic-addition.js

# Check formatting without modifying files
./format.pas --check

# Auto-format all Pascal files
./format.pas
```

### Direct FPC compilation

```bash
fpc @config.cfg -vw-n-h-i-l-d-u-t-p-c-x- source/app/GocciaREPL.dpr
fpc @config.cfg -vw-n-h-i-l-d-u-t-p-c-x- source/app/GocciaScriptLoader.dpr
fpc @config.cfg -vw-n-h-i-l-d-u-t-p-c-x- source/app/GocciaTestRunner.dpr
fpc @config.cfg -vw-n-h-i-l-d-u-t-p-c-x- source/app/GocciaBenchmarkRunner.dpr
fpc @config.cfg -vw-n-h-i-l-d-u-t-p-c-x- source/app/GocciaBundler.dpr
```

## Where to go next

- **Contribution requirements:** [CONTRIBUTING.md](CONTRIBUTING.md)
- **Engine shape:** [docs/architecture.md](docs/architecture.md), [docs/interpreter.md](docs/interpreter.md), [docs/bytecode-vm.md](docs/bytecode-vm.md), [docs/core-patterns.md](docs/core-patterns.md)
- **Optional extended agent skills:** [.agents/skills/](.agents/skills/) (installable playbooks; not a substitute for CONTRIBUTING)
