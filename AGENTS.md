# AGENTS.md

Instructions for AI coding assistants working on GocciaScript. For deep-dive documentation, see the [docs/](docs/) folder.

## Project Overview

GocciaScript is a subset of ECMAScript 2020 implemented in FreePascal. It provides a JavaScript-like scripting language with intentional limitations for security and simplicity. See [docs/language-restrictions.md](docs/language-restrictions.md) for the full rationale.

## Quick Reference

### Build Commands

```bash
./build.pas                  # Build everything
./build.pas loader           # Build ScriptLoader only
./build.pas repl             # Build REPL only
./build.pas testrunner       # Build TestRunner only
./build.pas benchmarkrunner  # Build BenchmarkRunner only
./build.pas tests            # Build Pascal unit tests only
```

### Run Commands

```bash
./build/ScriptLoader example.js                  # Execute a script
./build/REPL                                      # Start interactive REPL
./build/TestRunner tests/                         # Run all JavaScript tests
./build/TestRunner tests/language/expressions/    # Run a test category
./build/BenchmarkRunner benchmarks/                                # Run all benchmarks
./build/BenchmarkRunner benchmarks/fibonacci.js                    # Run a specific benchmark
./build/BenchmarkRunner benchmarks --format=json --output=out.json # Export as JSON
./build/BenchmarkRunner benchmarks --format=csv --output=out.csv   # Export as CSV
```

### Compile and Run (Common Workflows)

```bash
# Compile and run a script
./build.pas loader && ./build/ScriptLoader ./example.js

# Compile and run all tests
./build.pas testrunner && ./build/TestRunner tests

# Compile and run a specific test
./build.pas testrunner && ./build/TestRunner tests/language/expressions/addition/basic-addition.js
```

### Direct FPC Compilation

```bash
fpc @config.cfg -vw-n-h-i-l-d-u-t-p-c-x- REPL.dpr
fpc @config.cfg -vw-n-h-i-l-d-u-t-p-c-x- ScriptLoader.dpr
fpc @config.cfg -vw-n-h-i-l-d-u-t-p-c-x- TestRunner.dpr
fpc @config.cfg -vw-n-h-i-l-d-u-t-p-c-x- BenchmarkRunner.dpr
```

## Architecture

See [docs/architecture.md](docs/architecture.md) for the full architecture deep-dive.

**Pipeline:** Source → Lexer → Parser → Interpreter → Evaluator → Result

**Key components:**

| Component | File | Role |
|-----------|------|------|
| Engine | `Goccia.Engine.pas` | Top-level orchestration, built-in registration |
| Lexer | `Goccia.Lexer.pas` | Source → tokens |
| Parser | `Goccia.Parser.pas` | Tokens → AST |
| Interpreter | `Goccia.Interpreter.pas` | AST execution, module loading, scope ownership |
| Evaluator | `Goccia.Evaluator.pas` | Pure AST evaluation (+ sub-modules) |
| Scope | `Goccia.Scope.pas` | Lexical scoping, variable bindings, TDZ |
| Garbage Collector | `Goccia.GarbageCollector.pas` | Mark-and-sweep memory management for runtime values |

## Critical Rules

These rules **must** be followed when modifying the codebase:

### 1. Evaluator Purity

Functions in `Goccia.Evaluator.pas` (and its sub-modules) are **pure functions**. They cannot have side effects and must return the same output for the same input. State changes happen through the scope and value objects passed via `TGocciaEvaluationContext`, never through evaluator-internal state.

### 2. Scope Creation

`TGocciaScope` must **never** be instantiated directly. Always use the `CreateChild` factory method on an existing scope:

```pascal
// Correct
ChildScope := ParentScope.CreateChild(skBlock);

// WRONG — do not do this
ChildScope := TGocciaScope.Create(ParentScope, skBlock);
```

### 3. Testing Requirements

JavaScript end-to-end tests are the **primary** way of testing GocciaScript. When implementing a new feature or fixing a bug:
- Add JavaScript tests under the `tests/` directory.
- Keep tests isolated and grouped by feature/filename.
- Follow the existing directory structure (`tests/language/` for language features, `tests/built-ins/` for built-in objects).
- Each test file should focus on a single concern.
- Always verify changes by running: `./build.pas testrunner && ./build/TestRunner tests`

### 4. Garbage Collector Awareness

GocciaScript uses a mark-and-sweep garbage collector (`Goccia.GarbageCollector.pas`). All `TGocciaValue` instances auto-register with the GC via `AfterConstruction`. Key rules:

- **AST literal values** are unregistered from the GC by `TGocciaLiteralExpression.Create` and owned by the AST node. The evaluator calls `Value.RuntimeCopy` to produce fresh GC-managed values when evaluating literals.
- **Singleton values** (e.g., `UndefinedValue`, `TrueValue`, `NaNValue`, `SmallInt` cache) are pinned via `TGocciaGC.Instance.PinValue` during engine initialization (consolidated in `PinSingletons`).
- **Shared prototype singletons** (String, Number, Array, Set, Map, Function) are pinned inside each type's `InitializePrototype` method. All prototype method callbacks must use `ThisValue` (not `Self`) to access instance data, since `Self` refers to the method host singleton.
- **Pinned values, temp roots, and root scopes** are stored in `TDictionary<T, Boolean>` for O(1) membership checks.
- **Values held only by Pascal code** (not in any GocciaScript scope) must be protected with `AddTempRoot`/`RemoveTempRoot` for the duration they are needed. Example: benchmark functions held in a `TObjectList`.
- **Scopes** register with the GC in their constructor. Active call scopes are tracked via `PushActiveScope`/`PopActiveScope` in `TGocciaFunctionValue.Call`.
- Each value type must override `GCMarkReferences` to mark all `TGocciaValue` references it holds (prototype, closure, elements, property values, etc.).

### 5. Language Restrictions

GocciaScript intentionally excludes these JavaScript features — do **not** add support for them:
- `var` declarations (use `let`/`const`)
- `function` keyword (use arrow functions or shorthand methods)
- `==` and `!=` loose equality (use `===`/`!==`)
- `eval()` and `arguments` object
- Automatic semicolon insertion (semicolons are required)
- `with` statement
- Global `parseInt`, `parseFloat`, `isNaN`, `isFinite` — use `Number.*` instead (intentional divergence; keeps these functions on the object they belong to)

See [docs/language-restrictions.md](docs/language-restrictions.md) for the full list and rationale.

### 6. `this` Binding Semantics

GocciaScript follows ECMAScript strict mode `this` binding. Two function forms exist with separate AST nodes and runtime types:

- **Arrow functions** (`(x) => x + 1`) — AST: `TGocciaArrowFunctionExpression`, Runtime: `TGocciaArrowFunctionValue`. Always inherit `this` from their lexical (closure) scope via `BindThis` override.
- **Shorthand methods** (`method() { ... }`) — AST: `TGocciaMethodExpression`, Runtime: `TGocciaFunctionValue`. Receive call-site `this` from the receiver.

`this` binding is resolved via virtual dispatch on `TGocciaFunctionValue.BindThis` — no boolean flags or runtime branches. Array prototype callbacks pass `undefined` as `ThisValue`, so arrow callbacks correctly inherit their enclosing scope's `this`.

See [docs/design-decisions.md](docs/design-decisions.md) for the full design rationale.

## Code Style

See [docs/code-style.md](docs/code-style.md) for the complete style guide.

### Key Conventions

- **Unit naming:** `Goccia.<Category>.<Name>.pas` (dot-separated hierarchy)
- **Class naming:** `TGoccia<Name>` prefix
- **Interface naming:** `I<Name>` prefix
- **Private fields:** `F` prefix
- **Parameters:** `A` prefix
- **Indentation:** 2 spaces (see `.editorconfig`)
- **Compiler directives:** All units include `{$I Goccia.inc}`

### Terminology

- **Define** = create a new variable binding (`DefineLexicalBinding`)
- **Assign** = re-assign an existing binding (`AssignLexicalBinding`)
- These are distinct operations — do not conflate them.

### Design Patterns in Use

- **Singleton** for special values (`undefined`, `null`, `true`, `false`, `NaN`, `Infinity`) and shared prototype singletons (String, Number, Array, Set, Map, Function — each type uses `class var` + `InitializePrototype` guarded by `if Assigned`)
- **Factory method** for scope creation (`CreateChild`, with optional capacity hint)
- **Context object** for evaluation state (`TGocciaEvaluationContext`)
- **Virtual dispatch** for property access (`GetProperty`/`SetProperty` on `TGocciaValue`)
- **Chain of responsibility** for scope lookup
- **Parser combinator** for binary expressions (`ParseBinaryExpression` shared helper)
- **Recursive descent** for parsing
- **Mark-and-sweep** for garbage collection (`TGocciaGC`)
- **Shared helpers** for evaluator deduplication (`EvaluateStatementsSafe`, `SpreadIterableInto`, `EvaluateSimpleNumericBinaryOp`)

## Value System

See [docs/value-system.md](docs/value-system.md) for the complete value system documentation.

All values inherit from `TGocciaValue`. Property access is unified through virtual methods on the base class:
- `GetProperty(Name)` — Returns `nil` by default; overridden by objects, arrays, classes, instances, and string values.
- `SetProperty(Name, Value)` — No-op by default; overridden by objects, arrays, classes, and instances.

The evaluator calls these directly (`Value.GetProperty(Name)`) without type-checking or interface queries.

Error construction is centralized in `Goccia.Values.ErrorHelper.pas` (`ThrowTypeError`, `ThrowRangeError`, `CreateErrorObject`, etc.). Built-in argument validation uses `TGocciaArgumentValidator` (`Goccia.Arguments.Validator.pas`).

## Built-in Objects

See [docs/built-ins.md](docs/built-ins.md) for documentation on all built-ins and how to add new ones.

Built-ins are registered by the engine via `TGocciaGlobalBuiltins` flags:

```pascal
DefaultGlobals = [ggConsole, ggMath, ggGlobalObject, ggGlobalArray,
                  ggGlobalNumber, ggPromise, ggJSON, ggSymbol, ggSet, ggMap];
```

The TestRunner adds `ggTestAssertions` for the test framework (`describe`, `test`, `expect`).
The BenchmarkRunner adds `ggBenchmark` for the benchmark framework (`suite`, `bench`, `runBenchmarks`). It supports `--format=console|text|csv|json` and `--output=file` for exporting results. Benchmark calibration parameters are configurable via environment variables (`GOCCIA_BENCH_CALIBRATION_MS`, `GOCCIA_BENCH_ROUNDS`, etc.).

## Testing

See [docs/testing.md](docs/testing.md) for the complete testing guide.

- **Primary:** JavaScript end-to-end tests in `tests/` directory — these are the source of truth for correctness
- **Secondary:** Pascal unit tests in `units/*.Test.pas` — only for internal implementation details
- **JS test framework:** built-in `describe`/`test`/`expect` (enabled via `ggTestAssertions`)
- **Pascal test framework:** `TestRunner.pas` provides generic `Expect<T>(...).ToBe(...)` assertions. `Expect<T>` is a **standalone function** (not a method on `TTestSuite`) to avoid FPC 3.2.2 AArch64 compiler crash with cross-unit generic method inheritance.
- **NaN checks:** In Pascal tests, use `Value.ToNumberLiteral.IsNaN` (not `Math.IsNaN`) — special values store `0.0` internally

## Build System

See [docs/build-system.md](docs/build-system.md) for build system details.

- Build script: `./build.pas` (FreePascal script via `instantfpc`)
- Compiler config: `config.cfg`
- Shared directives: `units/Goccia.inc`
- Output directory: `build/`
- CI: GitHub Actions on Linux, macOS, Windows (x64 + ARM)

## Documentation Index

| Document | Description |
|----------|-------------|
| [docs/architecture.md](docs/architecture.md) | Pipeline overview, component responsibilities, data flow |
| [docs/design-decisions.md](docs/design-decisions.md) | Rationale behind key technical choices |
| [docs/code-style.md](docs/code-style.md) | Naming conventions, patterns, file organization |
| [docs/value-system.md](docs/value-system.md) | Type hierarchy, virtual property access, primitives, objects |
| [docs/built-ins.md](docs/built-ins.md) | Available built-ins, registration system, adding new ones |
| [docs/testing.md](docs/testing.md) | Test organization, writing tests, running tests |
| [docs/build-system.md](docs/build-system.md) | Build commands, configuration, CI/CD |
| [docs/language-restrictions.md](docs/language-restrictions.md) | Supported/excluded features with rationale |
| [docs/embedding.md](docs/embedding.md) | Embedding the engine in FreePascal applications |
