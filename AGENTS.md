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
./build.pas testrunner && ./build/TestRunner tests   # after substantive changes
./format.pas --check # before push / PR
```

## Quick reference

### Build commands

```bash
./build.pas # Clean + dev build of everything (default)
./build.pas --dev loader # Dev build of ScriptLoader
./build.pas --prod # Clean + production build of everything
./build.pas --prod loader repl # Production build of specific components
./build.pas loader # Dev build (--dev is the default)
./build.pas testrunner # Dev build of TestRunner
./build.pas benchmarkrunner # Dev build of BenchmarkRunner
./build.pas tests # Dev build of Pascal unit tests
./build.pas clean # Clean stale artifacts only (no build)
./build.pas clean loader # Clean, then dev build of ScriptLoader
```

### Run commands

```bash
./build/ScriptLoader example.js # Execute a script (interpreted)
./build/ScriptLoader example.js --mode=bytecode # Execute via bytecode VM
./build/ScriptLoader example.js --import-map=imports.json # Execute with an explicit import map
./build/ScriptLoader example.js --alias @/=./src/ --alias config=./config/default.js # One-off import-map-style aliases
./build/ScriptLoader example.js --emit # Compile to .gbc (no execution)
./build/ScriptLoader example.js --emit=bytecode # Compile to .gbc (explicit)
./build/ScriptLoader example.js --emit --output=out.gbc # Custom output path
./build/ScriptLoader out.gbc # Load and execute .gbc bytecode
printf "const x = 2 + 2; x;" | ./build/ScriptLoader # Execute stdin source
./build/ScriptLoader example.js --coverage # Execute with line and branch coverage
./build/ScriptLoader example.js --coverage --coverage-format=lcov --coverage-output=coverage.lcov # Coverage with lcov output
./build/ScriptLoader example.js --coverage --coverage-format=json --coverage-output=coverage.json # Coverage with JSON output
./build/ScriptLoader example.js --asi # Execute with automatic semicolon insertion
./build/ScriptLoader example.js --profile=opcodes # Opcode histogram, pair frequency, scalar hit rate (bytecode)
./build/ScriptLoader example.js --profile=functions # Function self-time, allocations (bytecode)
./build/ScriptLoader example.js --profile=all # All profiling data (bytecode)
./build/ScriptLoader example.js --profile=all --profile-output=profile.json # Profile with JSON export
./build/ScriptLoader example.js --profile=functions --profile-format=flamegraph --profile-output=flamegraph.txt # Flame graph export
./build/ScriptLoader example.jsx --source-map --mode=bytecode # Write .map source map alongside execution
./build/ScriptLoader example.jsx --source-map=out.map --emit # Write source map to specific path with bytecode emit
./build/REPL # Start interactive REPL (interpreted)
./build/REPL --mode=bytecode # Start the REPL via bytecode VM
./build/REPL --mode=bytecode --timing # Bytecode REPL with per-line timing
./build/REPL --import-map=imports.json # Start the REPL with an explicit import map
./build/REPL --asi # Start the REPL with automatic semicolon insertion
./build/TestRunner tests/ # Run all JavaScript tests
./build/TestRunner tests --import-map=imports.json # Run tests with an explicit import map
./build/TestRunner tests/language/expressions/ # Run a test category
./build/TestRunner tests --no-progress --exit-on-first-failure # CI mode
./build/TestRunner tests --silent # Suppress all console output
./build/TestRunner tests --output=results.json # Write test results as JSON
./build/TestRunner tests --mode=bytecode # Run tests via the Goccia bytecode VM
./build/TestRunner tests/language/asi --asi # Run ASI tests with automatic semicolon insertion
./build/TestRunner tests --coverage # Run tests with line and branch coverage
./build/TestRunner tests --coverage --coverage-format=lcov --coverage-output=coverage.lcov # Coverage with lcov output
./build/TestRunner tests --coverage --coverage-format=json --coverage-output=coverage.json # Coverage with JSON output
./build/BenchmarkRunner benchmarks/ # Run all benchmarks
./build/BenchmarkRunner benchmarks --import-map=imports.json # Run benchmarks with an explicit import map
./build/BenchmarkRunner benchmarks/fibonacci.js # Run a specific benchmark
printf 'suite("stdin", () => { bench("sum", { run: () => 1 + 1 }); });\n' | ./build/BenchmarkRunner # Run benchmark source from stdin
./build/BenchmarkRunner benchmarks --format=json --output=out.json # Export as JSON
./build/BenchmarkRunner benchmarks --format=console --format=json --output=out.json # Console + JSON
./build/BenchmarkRunner benchmarks --no-progress # Suppress progress (CI)
./build/BenchmarkRunner benchmarks --mode=bytecode # Benchmarks via the Goccia bytecode VM
```

### Compile and run (common workflows)

```bash
# Compile and run a script
./build.pas loader && ./build/ScriptLoader ./example.js

# Compile and run all tests
./build.pas testrunner && ./build/TestRunner tests

# Compile and run a specific test
./build.pas testrunner && ./build/TestRunner tests/language/expressions/addition/basic-addition.js

# Check formatting without modifying files
./format.pas --check

# Auto-format all Pascal files
./format.pas
```

### Direct FPC compilation

```bash
fpc @config.cfg -vw-n-h-i-l-d-u-t-p-c-x- REPL.dpr
fpc @config.cfg -vw-n-h-i-l-d-u-t-p-c-x- ScriptLoader.dpr
fpc @config.cfg -vw-n-h-i-l-d-u-t-p-c-x- TestRunner.dpr
fpc @config.cfg -vw-n-h-i-l-d-u-t-p-c-x- BenchmarkRunner.dpr
```

## Where to go next

- **Contribution requirements:** [CONTRIBUTING.md](CONTRIBUTING.md)
- **Engine shape:** [docs/architecture.md](docs/architecture.md), [docs/interpreter.md](docs/interpreter.md), [docs/bytecode-vm.md](docs/bytecode-vm.md), [docs/core-patterns.md](docs/core-patterns.md)
- **Optional extended agent skills:** [.agents/skills/](.agents/skills/) (installable playbooks; not a substitute for CONTRIBUTING)
