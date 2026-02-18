# AGENTS.md

Instructions for AI coding assistants working on GocciaScript. For deep-dive documentation, see the [docs/](docs/) folder.

## Project Overview

GocciaScript is a subset of ECMAScript 2020 implemented in FreePascal. It provides a JavaScript-like scripting language with intentional limitations for security and simplicity. See [docs/language-restrictions.md](docs/language-restrictions.md) for the full rationale.

## Quick Reference

### Build Commands

```bash
./build.pas                       # Clean + dev build of everything (default)
./build.pas --dev loader          # Dev build of ScriptLoader
./build.pas --prod                # Clean + production build of everything
./build.pas --prod loader repl    # Production build of specific components
./build.pas loader                # Dev build (--dev is the default)
./build.pas testrunner            # Dev build of TestRunner
./build.pas benchmarkrunner       # Dev build of BenchmarkRunner
./build.pas tests                 # Dev build of Pascal unit tests
./build.pas clean                 # Clean stale artifacts only (no build)
./build.pas clean loader          # Clean, then dev build of ScriptLoader
```

### Run Commands

```bash
./build/ScriptLoader example.js                  # Execute a script
./build/REPL                                      # Start interactive REPL
./build/TestRunner tests/                         # Run all JavaScript tests
./build/TestRunner tests/language/expressions/    # Run a test category
./build/BenchmarkRunner benchmarks/                                               # Run all benchmarks
./build/BenchmarkRunner benchmarks/fibonacci.js                                   # Run a specific benchmark
./build/BenchmarkRunner benchmarks --format=json --output=out.json                # Export as JSON
./build/BenchmarkRunner benchmarks --format=console --format=json --output=out.json # Console + JSON
./build/BenchmarkRunner benchmarks --no-progress                                  # Suppress progress (CI)
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
| Scope | `Goccia.Scope.pas` | Lexical scoping, variable bindings, TDZ, VMT-based chain-walking |
| Keywords | `Goccia.Keywords.pas` | Centralized JavaScript keyword string constants |
| Timing Utilities | `TimingUtils.pas` | Cross-platform timing: monotonic (`GetNanoseconds`, `GetMilliseconds`), wall-clock (`GetEpochNanoseconds`), and duration formatting (`FormatDuration`) |
| Microtask Queue | `Goccia.MicrotaskQueue.pas` | Singleton FIFO queue for Promise reactions and `queueMicrotask` callbacks, drained after script execution, cleared on exception |
| Garbage Collector | `Goccia.GarbageCollector.pas` | Mark-and-sweep memory management for runtime values |
| Temporal Utilities | `Goccia.Temporal.Utils.pas` | ISO 8601 date math helpers, parsing, formatting |
| Temporal Built-in | `Goccia.Builtins.Temporal.pas` | Temporal namespace, constructors, static methods, Temporal.Now |

## Development Workflow

Every new feature or change **must** follow this workflow:

1. **Create a branch** — Create a new branch from `main` with a descriptive name (e.g., `feature/string-prototype-repeat`, `fix/nan-comparison`, `refactor/scope-chain`).
2. **Implement the feature** — Develop the feature on the branch. Follow the critical rules below (testing, evaluator purity, etc.).
3. **Add/update tests** — If adding a new language feature, create JavaScript test files following the existing patterns in `tests/`. If modifying AST logic, scope chain, evaluator, or value types, also build and run the native Pascal test suite and update `units/*.Test.pas` as needed.
4. **Update documentation** — Update all relevant documentation (`AGENTS.md`, `docs/*.md`, `README.md`) to reflect the change. Documentation is not optional.
5. **Commit** — Commit the implementation, tests, and documentation together with a clear, descriptive commit message.

```bash
# Example workflow
git checkout -b feature/array-prototype-flat
# ... implement the feature, add tests, update docs ...
git add .
git commit -m "Add Array.prototype.flat and flatMap"
```

Do **not** commit directly to `main`. All changes go through branches.

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

**When adding a new language feature:**
- Create test files for the feature following the existing directory and naming patterns in `tests/`.
- Tests should cover happy paths, edge cases, and error cases.
- Verify all tests pass before committing.

**When modifying AST logic, scope chain, evaluator, or value types:**
- Build and run the native Pascal test suite: `./build.pas clean tests && for t in build/Goccia.*.Test; do "$t"; done`
- Update the native tests in `units/*.Test.pas` to reflect any changes in behaviour (e.g. new parameters, changed return semantics).
- Both the JavaScript tests **and** the native Pascal tests must pass.

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

At the scope level, `this`, owning class, and super class are resolved via VMT-based chain-walking:
- `Scope.FindThisValue` — walks the parent chain calling `GetThisValue` (virtual) on each scope.
- `Scope.FindOwningClass` / `Scope.FindSuperClass` — same pattern for class resolution.
- `Scope.ResolveIdentifier(Name)` — unified identifier lookup that handles `this` (via `FindThisValue`) and keyword constants before falling back to the scope chain.

Use `Goccia.Keywords` constants (`KEYWORD_THIS`, `KEYWORD_SUPER`, etc.) instead of hardcoded string literals when referencing JavaScript keywords.

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

### Platform Pitfall: `Double(Int64)` on AArch64

On FPC 3.2.2 AArch64, `Double(Int64Var)` performs a bit reinterpretation, not a value conversion. Use implicit promotion instead: `Int64Var * 1.0` or `Int64Var * 1000000.0`. See [docs/code-style.md](docs/code-style.md) for details.

### Terminology

- **Define** = create a new variable binding (`DefineLexicalBinding`)
- **Assign** = re-assign an existing binding (`AssignLexicalBinding`)
- These are distinct operations — do not conflate them.

### Design Patterns in Use

- **Singleton** for special values (`undefined`, `null`, `true`, `false`, `NaN`, `Infinity`) and shared prototype singletons (String, Number, Array, Set, Map, Function — each type uses `class var` + `InitializePrototype` guarded by `if Assigned`)
- **Factory method** for scope creation (`CreateChild`, with optional capacity hint)
- **Context object** for evaluation state (`TGocciaEvaluationContext`)
- **Virtual dispatch** for property access (`GetProperty`/`SetProperty`), type discrimination (`IsPrimitive`/`IsCallable`), and scope chain resolution (`GetThisValue`/`GetOwningClass`/`GetSuperClass`) on the `TGocciaValue` and `TGocciaScope` hierarchies
- **Chain of responsibility** for scope lookup
- **Parser combinator** for binary expressions (`ParseBinaryExpression` shared helper)
- **Recursive descent** for parsing
- **Mark-and-sweep** for garbage collection (`TGocciaGC`)
- **Shared helpers** for evaluator deduplication (`EvaluateStatementsSafe`, `SpreadIterableInto`, `EvaluateSimpleNumericBinaryOp`)

## Value System

See [docs/value-system.md](docs/value-system.md) for the complete value system documentation.

All values inherit from `TGocciaValue`. Virtual methods on the base class eliminate type-checking at call sites:
- `GetProperty(Name)` / `SetProperty(Name, Value)` — Polymorphic property access. Returns `nil` / no-op by default.
- `IsPrimitive` — Returns `True` for null, undefined, boolean, number, and string types. Use `Value.IsPrimitive` instead of multi-`is` check chains.
- `IsCallable` — Returns `True` for functions and classes. Use `Value.IsCallable` instead of `(Value is TGocciaFunctionBase)` or `(Value is TGocciaFunctionValue) or (Value is TGocciaNativeFunctionValue)`.

The evaluator calls these directly (`Value.GetProperty(Name)`, `Value.IsPrimitive`, `Value.IsCallable`) without type-checking or interface queries. **Prefer these VMT methods over `is` type checks for fundamental type-system properties.** Do not add VMT methods for optional built-in types (e.g., Symbol, Set, Map) — these are toggled via `TGocciaGlobalBuiltins` flags and should use standard RTTI (`is`) checks instead.

Error construction is centralized in `Goccia.Values.ErrorHelper.pas` (`ThrowTypeError`, `ThrowRangeError`, `CreateErrorObject`, etc.). Built-in argument validation uses `TGocciaArgumentValidator` (`Goccia.Arguments.Validator.pas`).

**Symbol coercion:** `TGocciaSymbolValue.ToNumberLiteral` throws `TypeError` (symbols cannot convert to numbers). `ToStringLiteral` returns `"Symbol(description)"` for internal use (display, property keys), but implicit string coercion (template literals, `+` operator, `String.prototype.concat`) must check for symbols and throw `TypeError` at the operator level. See `Goccia.Evaluator.Arithmetic.pas` and `Goccia.Evaluator.pas` for the pattern.

## Built-in Objects

See [docs/built-ins.md](docs/built-ins.md) for documentation on all built-ins and how to add new ones.

Built-ins are registered by the engine via `TGocciaGlobalBuiltins` flags:

```pascal
DefaultGlobals = [ggConsole, ggMath, ggGlobalObject, ggGlobalArray,
 ggGlobalNumber, ggPromise, ggJSON, ggSymbol, ggSet, ggMap, ggTemporal];
```

The TestRunner adds `ggTestAssertions` for the test framework (`describe`, `test`, `expect`).
The BenchmarkRunner adds `ggBenchmark` for the benchmark framework (`suite`, `bench`, `runBenchmarks`). It supports multiple `--format=console|text|csv|json` flags in a single command (each optionally followed by `--output=file`), `--no-progress` for CI builds, and benchmark calibration via environment variables (`GOCCIA_BENCH_CALIBRATION_MS`, `GOCCIA_BENCH_ROUNDS`, etc.).

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
- Build modes: `--dev` (default, debug info, checks) / `--prod` (O4, stripped, smart-linked)
- Clean: `clean` target removes stale `.ppu`, `.o`, `.res` files from `build/` — runs automatically on full builds, or chain it explicitly (e.g. `./build.pas clean loader`)
- Shared path config: `config.cfg`
- Shared directives: `units/Goccia.inc` (overflow/range checks conditional on `PRODUCTION` define)
- Output directory: `build/`
- CI: Two workflow files — `ci.yml` (main + tags, full matrix, all checks + release) and `pr.yml` (PRs, ubuntu-latest x64 only, JS tests + benchmark comparison comment)

## Documentation Index

| Document | Description |
|----------|-------------|
| [docs/architecture.md](docs/architecture.md) | Pipeline overview, component responsibilities, data flow |
| [docs/design-decisions.md](docs/design-decisions.md) | Rationale behind key technical choices |
| [docs/code-style.md](docs/code-style.md) | Naming conventions, patterns, file organization |
| [docs/value-system.md](docs/value-system.md) | Type hierarchy, virtual property access, primitives, objects |
| [docs/built-ins.md](docs/built-ins.md) | Available built-ins, registration system, adding new ones |
| [docs/testing.md](docs/testing.md) | Test organization, writing tests, running tests |
| [docs/benchmarks.md](docs/benchmarks.md) | Benchmark runner, output formats, writing benchmarks, CI comparison |
| [docs/build-system.md](docs/build-system.md) | Build commands, configuration, CI/CD |
| [docs/language-restrictions.md](docs/language-restrictions.md) | Supported/excluded features with rationale |
| [docs/embedding.md](docs/embedding.md) | Embedding the engine in FreePascal applications |
