# AGENTS.md

Instructions for AI coding assistants working on GocciaScript. For deep-dive documentation, see the [docs/](docs/) folder.

## Project Overview

GocciaScript is a subset of ECMAScript implemented in FreePascal. It provides a JavaScript-like scripting language with intentional limitations for security and simplicity. See [docs/language-restrictions.md](docs/language-restrictions.md) for the full rationale.

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
./build/ScriptLoader example.js                  # Execute a script (interpreted)
./build/ScriptLoader example.js --mode=bytecode  # Execute via bytecode VM
./build/ScriptLoader example.js --import-map=imports.json  # Execute with an explicit import map
./build/ScriptLoader example.js --alias @/=./src/ --alias config=./config/default.js  # One-off import-map-style aliases
./build/ScriptLoader example.js --emit           # Compile to .gbc (no execution)
./build/ScriptLoader example.js --emit=bytecode  # Compile to .gbc (explicit)
./build/ScriptLoader example.js --emit --output=out.gbc   # Custom output path
./build/ScriptLoader out.gbc                     # Load and execute .gbc bytecode
printf "const x = 2 + 2; x;" | ./build/ScriptLoader        # Execute stdin source
./build/ScriptLoader example.js --coverage                 # Execute with line and branch coverage
./build/ScriptLoader example.js --coverage --coverage-format=lcov --coverage-output=coverage.lcov  # Coverage with lcov output
./build/ScriptLoader example.js --coverage --coverage-format=json --coverage-output=coverage.json  # Coverage with JSON output
./build/ScriptLoader example.js --asi                              # Execute with automatic semicolon insertion
./build/ScriptLoader example.js --profile=opcodes                  # Opcode histogram, pair frequency, scalar hit rate (bytecode)
./build/ScriptLoader example.js --profile=functions                # Function self-time, allocations (bytecode)
./build/ScriptLoader example.js --profile=all                      # All profiling data (bytecode)
./build/ScriptLoader example.js --profile=all --profile-output=profile.json  # Profile with JSON export
./build/ScriptLoader example.js --profile=functions --profile-format=flamegraph --profile-output=flamegraph.txt  # Flame graph export
./build/REPL                                      # Start interactive REPL (interpreted)
./build/REPL --mode=bytecode                      # Start the REPL via bytecode VM
./build/REPL --mode=bytecode --timing             # Bytecode REPL with per-line timing
./build/REPL --import-map=imports.json            # Start the REPL with an explicit import map
./build/REPL --asi                                # Start the REPL with automatic semicolon insertion
./build/TestRunner tests/                                                      # Run all JavaScript tests
./build/TestRunner tests --import-map=imports.json                             # Run tests with an explicit import map
./build/TestRunner tests/language/expressions/                                 # Run a test category
./build/TestRunner tests --no-progress --exit-on-first-failure                 # CI mode
./build/TestRunner tests --silent                                              # Suppress all console output
./build/TestRunner tests --output=results.json                                 # Write test results as JSON
./build/TestRunner tests --mode=bytecode                                       # Run tests via the Goccia bytecode VM
./build/TestRunner tests/language/asi --asi                                     # Run ASI tests with automatic semicolon insertion
./build/TestRunner tests --coverage                                            # Run tests with line and branch coverage
./build/TestRunner tests --coverage --coverage-format=lcov --coverage-output=coverage.lcov  # Coverage with lcov output
./build/TestRunner tests --coverage --coverage-format=json --coverage-output=coverage.json  # Coverage with JSON output
./build/BenchmarkRunner benchmarks/                                               # Run all benchmarks
./build/BenchmarkRunner benchmarks --import-map=imports.json                      # Run benchmarks with an explicit import map
./build/BenchmarkRunner benchmarks/fibonacci.js                                   # Run a specific benchmark
printf 'suite("stdin", () => { bench("sum", { run: () => 1 + 1 }); });\n' | ./build/BenchmarkRunner # Run benchmark source from stdin
./build/BenchmarkRunner benchmarks --format=json --output=out.json                # Export as JSON
./build/BenchmarkRunner benchmarks --format=console --format=json --output=out.json # Console + JSON
./build/BenchmarkRunner benchmarks --no-progress                                  # Suppress progress (CI)
./build/BenchmarkRunner benchmarks --mode=bytecode                                # Benchmarks via the Goccia bytecode VM
```

### Compile and Run (Common Workflows)

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

### Direct FPC Compilation

```bash
fpc @config.cfg -vw-n-h-i-l-d-u-t-p-c-x- REPL.dpr
fpc @config.cfg -vw-n-h-i-l-d-u-t-p-c-x- ScriptLoader.dpr
fpc @config.cfg -vw-n-h-i-l-d-u-t-p-c-x- TestRunner.dpr
fpc @config.cfg -vw-n-h-i-l-d-u-t-p-c-x- BenchmarkRunner.dpr
```

## Architecture

See [docs/architecture.md](docs/architecture.md) for the full architecture deep-dive and [docs/bytecode-vm.md](docs/bytecode-vm.md) for the bytecode backend.

**Interpreted pipeline:** Source → (JSX Transformer) → Lexer → Parser → Interpreter → Evaluator → Result

**Bytecode pipeline:** Source → Lexer → Parser → Compiler → Goccia Bytecode → Goccia VM → Result

**Key components:**

| Component | File | Role |
|-----------|------|------|
| Engine | `Goccia.Engine.pas` | Top-level orchestration, built-in registration |
| Lexer | `Goccia.Lexer.pas` | Source → tokens |
| Parser | `Goccia.Parser.pas` | Tokens → AST |
| Interpreter | `Goccia.Interpreter.pas` | AST execution, module loading, scope ownership |
| Evaluator | `Goccia.Evaluator.pas` | Pure AST evaluation |
| Compiler | `Goccia.Compiler*.pas` | AST → Goccia bytecode |
| Bytecode format | `Goccia.Bytecode*.pas` | Opcodes, templates, modules, binary I/O, debug info |
| VM | `Goccia.VM*.pas` | Register execution, closures, upvalues, exception handlers |
| Runtime bootstrap | `Goccia.Runtime.Bootstrap.pas` | Shared built-in and global initialization |
| Shared values | `Goccia.Values.*.pas` | Arrays, objects, classes, promises, iterators, primitives |
| Garbage collector | `Goccia.GarbageCollector.pas` | Unified mark-and-sweep GC for interpreter and bytecode execution |
| Profiler | `Goccia.Profiler*.pas` | Bytecode opcode/function profiling, pair tracking, allocation counting |
| Proxy | `Goccia.Values.ProxyValue.pas` | ES2026 Proxy with all 13 handler traps and invariant enforcement |
| Uint8Array encoding | `Goccia.Values.Uint8ArrayEncoding.pas` | Uint8Array Base64/Hex: `toBase64`, `fromBase64`, `toHex`, `fromHex`, `setFromBase64`, `setFromHex` |
| FFI | `Goccia.FFI*.pas`, `Goccia.Values.FFI*.pas` | Foreign Function Interface for native shared libraries |
| URI encoding | `Goccia.URI.pas` | Shared `encodeURI`/`decodeURI`/`encodeURIComponent`/`decodeURIComponent` and `PercentEncodePath` |
| import.meta | `Goccia.ImportMeta.pas` | Per-module metadata object with `url` and `resolve()` (ES2026 §13.3.12) |
| Dynamic import | `Goccia.AST.Expressions.pas` | `import(specifier)` expression returning a Promise with the module namespace (ES2026 §13.3.10) |
| Temporal types | `Goccia.Values.TemporalPlainYearMonth.pas`, `Goccia.Values.TemporalPlainMonthDay.pas`, `Goccia.Values.TemporalZonedDateTime.pas` | PlainYearMonth, PlainMonthDay, and ZonedDateTime Temporal types |
| Temporal timezone | `Goccia.Temporal.TimeZone.pas` | IANA timezone resolution, TZif parsing, UTC offset calculation |
| Temporal options | `Goccia.Temporal.Options.pas` | Shared options bag parsing: rounding modes, overflow, units, fractional digits |
| Spec | `Goccia.Spec.pas` | Spec/proposal feature data and factory functions |
| Preprocessors | `Goccia.Engine.pas` | `TGocciaPreprocessor = (ppJSX)` and `TGocciaPreprocessors` — pre-processing system |
| Compatibility | `Goccia.Engine.pas` | `TGocciaCompatibility = (cfASI)` and `TGocciaCompatibilityFlags` — compatibility layer |

**Bytecode design rules:**

- The VM register file uses tagged `TGocciaRegister` values internally. Hot scalar kinds (`undefined`, `null`, `hole`, booleans, integers, floats) stay unboxed in registers; object/runtime boundaries materialize `TGocciaValue` instances when needed.
- Bytecode mode uses the same runtime objects as the interpreter.
- Use `Goccia.Bytecode*` and `Goccia.VM*` names in new code.
- Use `.gbc`, not `.sbc`.
- Prefer Goccia-specific opcodes over generic extension layers when the semantics are language-owned.

## Development Workflow

### Local Setup

After cloning the repository, install [Lefthook](https://github.com/evilmartians/lefthook) to enable the pre-commit auto-formatter:

```bash
# macOS
brew install lefthook

# Linux (Snap)
sudo snap install lefthook

# Windows (Scoop)
scoop install lefthook

# Any platform with Go or npm
go install github.com/evilmartians/lefthook@latest
npm install -g lefthook
```

Then register the git hooks:

```bash
lefthook install
```

This ensures `./format.pas` runs automatically on staged `.pas`/`.dpr` files before each commit, auto-fixing uses clause ordering, PascalCase function names, and parameter `A` prefix naming.

### Feature Workflow

Every new feature or change **must** follow this workflow:

1. **Create a branch** — Create a new branch from `main` with a descriptive name (e.g., `feature/string-prototype-repeat`, `fix/nan-comparison`, `refactor/scope-chain`).
2. **Implement the feature** — Develop the feature on the branch. Follow the critical rules below (testing, evaluator purity, etc.).
3. **Annotate spec references** — If the feature implements ECMAScript-specified behavior, add `// ESYYYY §X.Y.Z` spec annotations above each function body and at key algorithm steps within the body (see [Code Style: ECMAScript Spec Annotations](#ecmascript-spec-annotations)).
4. **Add/update tests** — If adding a new language feature, create JavaScript test files following the existing patterns in `tests/`. If modifying AST logic, scope chain, evaluator, or value types, also build and run the native Pascal test suite and update `units/*.Test.pas` as needed.
5. **Update documentation** — Update all relevant documentation (`AGENTS.md`, `docs/*.md`, `README.md`) to reflect the change. Documentation is not optional.
6. **Commit** — Commit the implementation, tests, and documentation together with a clear, descriptive commit message.

```bash
# Example workflow
git checkout -b feature/array-prototype-flat
# ... implement the feature, add tests, update docs ...
git add .
git commit -m "Add Array.prototype.flat and flatMap"
```

Do **not** commit directly to `main`. All changes go through branches.

### Issues and Pull Requests

When creating GitHub issues, use the issue template (`.github/ISSUE_TEMPLATE/default.md`) which requires: Summary, Why, Current behavior, Expected behavior, and Scope notes.

When creating pull requests, use the PR template (`.github/pull_request_template.md`) which requires: a Summary section (describe the change, note constraints, link related issues) and a Testing checklist (JS/TS test verification, documentation updates, optional Pascal tests, optional benchmark verification).

## Critical Rules

These rules **must** be followed when modifying the codebase:

### 1. Evaluator Purity and VMT Dispatch

Evaluation is **pure** — same expression + context always produces the same result, with no hidden mutable state. State changes happen through the scope and value objects passed via `TGocciaEvaluationContext`, never through evaluator-internal state.

**Dispatch mechanism:** Expression and statement evaluation uses VMT dispatch. `TGocciaExpression` declares `function Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue; virtual; abstract;` and `TGocciaStatement` declares `function Execute(const AContext: TGocciaEvaluationContext): TGocciaControlFlow; virtual; abstract;`. Each AST subclass overrides the appropriate method. The evaluator's `EvaluateExpression` and `EvaluateStatement` are thin wrappers that call these virtual methods. This replaces the previous `if AExpression is TGocciaXxx then` dispatch chain, eliminating `TObject.InheritsFrom` overhead (18.4% of interpreted instructions in callgrind profiling). Helper functions (`EvaluateBinary`, `EvaluateCall`, `EvaluateBlock`, etc.) remain as standalone functions in `Goccia.Evaluator.pas`, called by the AST overrides via the implementation-section `uses Goccia.Evaluator` pattern (legal circular reference in FPC Delphi mode). See [docs/spikes/fpc-dispatch-performance.md](docs/spikes/fpc-dispatch-performance.md) for the benchmark analysis.

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
- Prefer tests that exercise the **public surface** that users actually call (JavaScript files, CLI commands, workflow smoke tests, documented entry points). Avoid adding tests that lock onto private helper functions or internal implementation details unless that internal API is itself a shared contract with no stable public entry point.
- Write test names, descriptions, and suite titles in terms that match the **layer under test**. End-to-end JavaScript and CLI tests should use JavaScript/runtime terminology and describe observable behavior at that layer, not Pascal implementation details or internal type names. Lower-level Pascal tests may use implementation terminology when that is the layer being exercised.
- **One method per file** — each test file focuses on a single method or operation. Never bundle multiple methods into one file (no `prototype-methods.js` or `static-methods.js`).
- **Prototype methods go in `prototype/`** — instance methods live in `BuiltIn/prototype/methodName.js`. Static methods and constructor tests live directly in the `BuiltIn/` folder. See `tests/built-ins/Array/prototype/` for the canonical example.
- **Edge cases are co-located** — edge case tests (NaN, Infinity, negative indices, clamping, empty collections, boundary conditions) belong in the **same file** as the happy-path tests for that method. Do **not** create separate `edge-cases.js` files.
- **Format-defined newline semantics are not host-dependent** — when a file format specifies newline normalization (for example TOML multiline strings or YAML folding/block-scalar rules), implement that behavior explicitly and do **not** emit `LineEnding`/`sLineBreak` just because the host OS is Windows. Add regression coverage with explicit `#13#10` / `\r\n` inputs and assert the format-defined canonical result so Windows CI cannot drift from Linux/macOS behavior.
- **Parser file inputs must preserve UTF-8 bytes on Windows** — when a parser consumes file-backed text (TOML/JSON/YAML/import maps/globals/source files), do not route UTF-8 through `string(UTF8String(...))`, plain `string` temporaries on the file-backed path, or bare `TStringList.LoadFromFile` and assume Windows will do the right thing. Read raw bytes with the shared UTF-8 helper, keep them as `UTF8String` until the parser consumes them, and add one regression that exercises a real file on disk instead of only in-memory strings.
- Always verify changes by running: `./build.pas testrunner && ./build/TestRunner tests`

**When adding a new language feature:**
- Create test files for the feature following the existing directory and naming patterns in `tests/`.
- Tests should cover happy paths, edge cases, and error cases — all in the same file for each method.
- Verify all tests pass before committing.

**When modifying AST logic, scope chain, evaluator, or value types:**
- Build and run the native Pascal test suite: `./build.pas clean tests && for t in build/Goccia.*.Test; do "$t"; done`
- Update the native tests in `units/*.Test.pas` to reflect any changes in behaviour (e.g. new parameters, changed return semantics).
- Keep native Pascal tests focused on visible public behavior where possible, and only drop to lower-level coverage when the behavior is genuinely internal or unreachable through a stable public API. Prefer stateless tests with repeatable inputs and outputs over tests that depend on ambient process state, timing, or incidental implementation details. If a feature is exposed through a CLI tool or other documented user-facing API, prefer adding or extending command-level/workflow-level coverage there instead of asserting internal helper behavior directly.
- Both the JavaScript tests **and** the native Pascal tests must pass.

### 4. Garbage Collector Awareness

GocciaScript uses a **unified mark-and-sweep garbage collector** shared by both the interpreter and bytecode VM. Key rules:

- **Generation-based marking** — `TGCManagedObject.AdvanceMark` provides O(1) mark-clear. All GC-managed objects inherit from `TGCManagedObject` and auto-register via `AfterConstruction`.
- **Singleton management** — `TGarbageCollector.Instance` manages all objects. `Initialize`/`Instance`/`Shutdown` lifecycle.
- **AST literal values** are unregistered from the GC by `TGocciaLiteralExpression.Create` and owned by the AST node. The evaluator calls `Value.RuntimeCopy` to produce fresh GC-managed values.
- **Singleton values** (`UndefinedValue`, `TrueValue`, `NaNValue`, etc.) are pinned via `PinObject` during engine initialization (consolidated in `PinPrimitiveSingletons`).
- **Shared prototype singletons** are pinned automatically by `TGocciaSharedPrototype.Create` — no manual pinning needed.
- **Values held only by Pascal code** (not in any GocciaScript scope) must be protected with `AddTempRoot`/`RemoveTempRoot`.
- **On-demand collection** — Use `CollectIfNeeded(AProtect)` when holding a `TGCManagedObject` on the stack. The no-arg `CollectIfNeeded` is only safe when all live values are already rooted.
- **Scopes** register/unregister with the GC in their constructor/destructor. Active call scopes tracked via `PushActiveRoot`/`PopActiveRoot`.
- **VM register rooting** only traverses object-bearing register slots.
- Each value type must override `MarkReferences` to mark all `TGocciaValue` references it holds.
- **Pinned values, temp roots, and root scopes** use `THashMap<TGCManagedObject, Boolean>` (`TGCObjectSet`) for O(1) membership.
- **Adaptive threshold** — after each collection, the threshold scales to `max(DEFAULT_GC_THRESHOLD, surviving_count)`, reducing collection frequency for large heaps.
- Automatic collection is disabled during bytecode execution; TestRunner and BenchmarkRunner call `Collect` after each file.

See [docs/value-system.md](docs/value-system.md#gc-integration) for the GC integration details.

### 5. Language Restrictions

GocciaScript intentionally excludes these JavaScript features — do **not** add support for them:
- `var` declarations (use `let`/`const`)
- `function` keyword (use arrow functions or shorthand methods)
- `==` and `!=` loose equality (use `===`/`!==`)
- `eval()` and `arguments` object
- Automatic semicolon insertion (semicolons required by default; ASI available as opt-in via `Engine.ASIEnabled := True`, `cfASI in Engine.Compatibility`, or `--asi` CLI flag)
- `with` statement
- Traditional loops (`for`, `while`, `do...while`) — use `for...of`, `for await...of`, or array methods instead. `for...of` and `for await...of` are supported.
- Default imports/exports — use named imports/exports
- Global `parseInt`, `parseFloat`, `isNaN`, `isFinite` — use `Number.*` instead (intentional divergence; keeps these functions on the object they belong to)

The parser accepts unsupported syntax (loops, `with`, `function`, `==`, `!=`, default imports/exports, side-effect imports, wildcard re-exports, labeled statements) but treats it as a no-op — the code parses successfully, the unsupported construct is skipped at runtime, and a warning is emitted to stdout with a suggestion. `function` declarations produce `TGocciaEmptyStatement`; `function` expressions evaluate to `undefined`. `==`/`!=` expressions evaluate to `undefined` (both operands are parsed and discarded). Labeled statements strip the label and execute the labeled statement normally. This design preserves the AST for a potential future compatibility mode.

See [docs/language-restrictions.md](docs/language-restrictions.md) for the full list and rationale.

### 6. Types as Comments

GocciaScript supports the TC39 Types as Comments proposal. Type annotations are parsed and preserved as raw strings on AST nodes. In interpreted mode, annotations are ignored. In bytecode mode, the compiler uses annotations and inferred types to emit `OP_CHECK_TYPE` guards. See [docs/language-restrictions.md](docs/language-restrictions.md#types-as-comments) for supported syntax.

**Key parser helpers:** `CollectTypeAnnotation(Terminators)` consumes type tokens with balanced bracket tracking. `CollectGenericParameters` consumes `<...>` generic parameter lists.

**AST nodes with type fields:** `TGocciaParameter` (`TypeAnnotation`, `IsOptional`), `TGocciaVariableInfo` (`TypeAnnotation`), `TGocciaDestructuringDeclaration` (`TypeAnnotation`), `TGocciaArrowFunctionExpression` (`ReturnType`), `TGocciaClassMethod` (`ReturnType`, `GenericParams`), `TGocciaClassDefinition` (`GenericParams`, `ImplementsClause`, `InstancePropertyTypes`), `TGocciaTryStatement` (`CatchParamType`).

`type`/`interface` declarations and `import type`/`export type` produce `TGocciaEmptyStatement`. Access modifiers in class bodies are consumed and discarded.

### 7. Decorators

GocciaScript supports TC39 Stage 3 decorators and decorator metadata. See [docs/language-restrictions.md](docs/language-restrictions.md#decorators) for supported syntax.

**Key implementation details for contributors:**

- **Parser:** `ParseDecorators` collects lists; `ParseDecoratorExpression` parses identifiers, member access, and calls. `TGocciaClassElement` record in `Goccia.AST.Statements.pas` stores decorators.
- **Evaluator:** Three-phase evaluation in `EvaluateClassDefinition`: (1) evaluate expressions in source order, (2) call decorators bottom-up with context objects, (3) apply results. Auto-accessors generate private backing fields.
- **GC:** `TGocciaClassValue.MarkReferences` must mark `FMethodInitializers`, `FFieldInitializers`, and `FDecoratorFieldInitializers[].Initializer`.
- **Constants:** Decorator context properties use `Goccia.Constants.PropertyNames` constants (`PROP_KIND`, `PROP_NAME`, `PROP_STATIC`, `PROP_PRIVATE`, `PROP_METADATA`, `PROP_ACCESS`, `PROP_ADD_INITIALIZER`).
- **`Symbol.metadata`:** `TGocciaSymbolValue.WellKnownMetadata` (lazily initialized, GC-pinned).
- **Contextual keyword:** `KEYWORD_ACCESSOR = 'accessor'` in `Goccia.Keywords.Contextual.pas`.
- **Not supported:** Parameter decorators.

### 8. `this` Binding Semantics

- **Arrow functions** (`TGocciaArrowFunctionExpression` / `TGocciaArrowFunctionValue`) — always inherit `this` from lexical scope via `BindThis` override
- **Shorthand methods** (`TGocciaMethodExpression` / `TGocciaFunctionValue`) — receive call-site `this` from the receiver

`this` binding is resolved via virtual dispatch on `TGocciaFunctionValue.BindThis`. Async variants inherit binding from their non-async superclasses. Scope-level resolution uses VMT-based chain-walking (`FindThisValue`, `FindOwningClass`, `FindSuperClass`, `ResolveIdentifier`). Use `Goccia.Keywords.Reserved` constants (`KEYWORD_THIS`, `KEYWORD_SUPER`) instead of hardcoded strings. See [docs/value-system.md](docs/value-system.md#functions) and [docs/design-decisions.md](docs/design-decisions.md).

## Code Style

See [docs/code-style.md](docs/code-style.md) for the complete style guide.

### Key Conventions

- **Function/procedure names:** PascalCase (e.g., `EvaluateBinary`, `GetProperty`). External C bindings are exempt. Auto-fixed by `./format.pas`.
- **Unit naming:** `Goccia.<Category>.<Name>.pas` (dot-separated hierarchy)
- **No abbreviations:** Use full words in class, function, method, and type names (e.g., `TGarbageCollector` not `TGC`). Exceptions: `AST`, `JSON`, `REPL`, `ISO`, `Utils`.
- **File extension constants:** Use `Goccia.FileExtensions` constants (`EXT_JS`, `EXT_JSX`, `EXT_TS`, `EXT_TSX`, `EXT_MJS`, `EXT_JSON`, `EXT_JSON5`, `EXT_JSONL`, `EXT_TOML`, `EXT_YAML`, `EXT_YML`, `EXT_TXT`, `EXT_MD`, `EXT_GBC`) instead of hardcoded string literals. Use the shared extension arrays/helpers (`ScriptExtensions`, `ModuleImportExtensions`, `IsScriptExtension`, `IsTextAssetExtension`, `IsJSXNativeExtension`, etc.) instead of duplicating extension lists or ad-hoc checks.
- **Runtime constants:** Use the split constant units instead of hardcoded string literals for property names, type names, error names, constructor names, and symbol names:
  - `Goccia.Constants.PropertyNames` — `PROP_LENGTH`, `PROP_NAME`, `PROP_CONSTRUCTOR`, `PROP_PROTOTYPE`, `PROP_GET`, `PROP_SET`, `PROP_KIND`, `PROP_STATIC`, `PROP_PRIVATE`, `PROP_METADATA`, `PROP_ACCESS`, `PROP_INIT`, `PROP_ADD_INITIALIZER`, `PROP_STRICT_TYPES`, etc.
  - `Goccia.Constants.TypeNames` — `OBJECT_TYPE_NAME`, `STRING_TYPE_NAME`, `FUNCTION_TYPE_NAME`, etc.
  - `Goccia.Constants.ErrorNames` — `ERROR_NAME`, `TYPE_ERROR_NAME`, `RANGE_ERROR_NAME`, etc.
  - `Goccia.Constants.ConstructorNames` — `CONSTRUCTOR_OBJECT`, `CONSTRUCTOR_ARRAY`, `CONSTRUCTOR_STRING`, `CONSTRUCTOR_MAP`, etc.
  - `Goccia.Constants.SymbolNames` — `SYMBOL_ITERATOR`, `SYMBOL_ASYNC_ITERATOR`, `SYMBOL_SPECIES`, `SYMBOL_HAS_INSTANCE`, `SYMBOL_TO_PRIMITIVE`, `SYMBOL_TO_STRING_TAG`, `SYMBOL_IS_CONCAT_SPREADABLE`, `SYMBOL_METADATA`
  - `Goccia.Constants` — `BOOLEAN_TRUE_LITERAL`, `NULL_LITERAL`, `NAN_LITERAL`, `ZERO_VALUE`, `EMPTY_STRING`, etc.
- **No magic numbers:** Extract bare numeric literals in `implementation` sections into named constants so the value is defined once and the name conveys intent. When a constant is used in both `interface` (e.g., default parameter) and `implementation` (e.g., fallback), declare it in `interface`. Trivial self-explanatory literals (`0`, `1`, `-1`, `''`, `True`, `False`) do not need extraction.
- **Generic lists for class types:** Prefer `TObjectList<T>` over `TList<T>` when `T` is a class. When a specialization is used across multiple units, define a **named type alias** in the unit that declares `T` (e.g., `TGocciaValueList = TObjectList<TGocciaValue>` in `Goccia.Values.Primitives.pas`, `TGocciaScopeList = TObjectList<TGocciaScope>` in `Goccia.Scope.pas`). All consumers must use the alias — never re-specialize locally. Use `Create(False)` for non-owning collections. This prevents FPC's per-unit generic VMT specialization from causing "Invalid type cast" failures with `{$OBJECTCHECKS ON}`.
- **Class naming:** `TGoccia<Name>` prefix
- **Interface naming:** `I<Name>` prefix
- **Private fields:** `F` prefix
- **Parameters:** `A` prefix for multi-letter names (e.g., `AScope`, `AValue`); single-letter names (`A`, `B`, `E`, `T`) are left as-is. Auto-fixed by `./format.pas`.
- **`const` parameters:** Prefer `const` for all parameters that are not modified in the function body
- **Uses clauses:** One unit per line, grouped (System > Third-party > Project > Relative), alphabetically sorted within each group, blank line between groups. Units with `in` paths go in Relative; `Goccia.*` in Project; FPC standard library in System; everything else in Third-party. Auto-fixed by `./format.pas`.
- **Indentation:** 2 spaces (see `.editorconfig`)
- **Compiler directives:** All units include `{$I Goccia.inc}`
- **Editor setup:** VSCode/Cursor auto-formats on save via the `runOnSave` extension (configured in `.vscode/settings.json`). Recommended extensions are in `.vscode/extensions.json`.
- **General rules:** Follow [Embarcadero's Pascal style guide](https://docwiki.embarcadero.com/RADStudio/Athens/en/General_Rules) for casing, keywords, and [type declarations](https://docwiki.embarcadero.com/RADStudio/Athens/en/Type_Declarations)

### ECMAScript Spec Annotations

When implementing ECMAScript-specified behavior, annotate each function or method with a comment referencing the relevant spec section. Use the format `// ESYYYY §X.Y.Z SpecMethodName(specParams)` where `YYYY` is the current edition year of the specification (e.g., `ES2026` for 2026). **The method name and parameter list must match the spec's pseudo-code exactly** — use `Array.prototype.map(callbackfn [, thisArg])`, not the Pascal implementation name `TGocciaArrayValue.Map(AArgs, AThisValue)`. The annotation is a spec cross-reference, not a Pascal signature. Place the annotation immediately above the function body in the `implementation` section. For multi-step algorithms, annotate individual steps inline:

```pascal
// ES2026 §23.1.3.18 Array.prototype.map(callbackfn [, thisArg])
function TGocciaArrayValue.Map(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Arr: TGocciaArrayValue;
begin
  Arr := TGocciaArrayValue(AThisValue);
  // ES2026 §23.1.3.18 step 4: ArraySpeciesCreate(O, len)
  ResultArray := ArraySpeciesCreate(Arr, Arr.Elements.Count);
```

This applies to:
- Built-in method implementations (Array, String, Number, Object, Set, Map, Promise, Symbol, etc.)
- Abstract operations — use the spec's operation name and parameters (e.g., `Await(value)`, `ToPrimitive(input [, preferredType])`, `IteratorNext(iteratorRecord)`)
- Evaluator operations that implement spec algorithms (property access, type coercion, operator semantics)

The section numbers reference [ECMA-262](https://tc39.es/ecma262/) (the living standard). Use the edition year matching the current year (e.g., `ES2026` for 2026, `ES2027` for 2027). For TC39 proposals not yet merged into the main spec (e.g., Temporal, Iterator Helpers), reference the proposal name instead:

```pascal
// TC39 Temporal §5.5.3 Temporal.Duration.prototype.add(other)
// TC39 Iterator Helpers §2.1.3.1 Iterator.prototype.map(mapper)
```

### Do Not Implement String Interning

Benchmarked at -4% across 172 benchmarks. See [docs/design-decisions.md](docs/design-decisions.md).

### Do Not Use TStringBuilder

Use `TStringBuffer` (`StringBuffer.pas`) instead — `TStringBuilder` triggers a 750x slowdown without preallocation. See [docs/spikes/fpc-string-performance.md](docs/spikes/fpc-string-performance.md).

### Do Not Use TDictionary on Hot Paths

Use purpose-built maps instead: `TOrderedStringMap<V>` for string-keyed ordered maps (4-6x faster), `THashMap<K,V>` for unordered maps, `TOrderedMap<K,V>` for generic ordered maps. `TDictionary` is acceptable on cold paths. **Never** use `TFPDataHashTable`. See [docs/code-style.md](docs/code-style.md#hash-map-selection) and [docs/spikes/fpc-hashmap-performance.md](docs/spikes/fpc-hashmap-performance.md).

**FPC 3.2.2 generic VMT pitfall:** FPC 3.2.2 creates per-unit VMTs for generic specializations. When a generic with a generic base class is specialized across multiple units, `{$OBJECTCHECKS ON}` can cause "Invalid type cast" failures if instances are type-cast across unit boundaries. All map types (`TOrderedStringMap`, `THashMap`, `TOrderedMap`) inherit from `TBaseMap`. This is safe because map instances are used as fields and local variables — they are never passed as base-class parameters or cross-unit type-cast. The VMT pitfall only applies when using `is`/`as` operators or `InheritsFrom` on generic instances across units. For `TObjectList<T>` (where cross-unit casts are common), named type aliases remain required. See [docs/spikes/fpc-generics-performance.md](docs/spikes/fpc-generics-performance.md) for the benchmark analysis confirming generics have zero runtime cost.

### Platform Pitfalls

Two FPC 3.2.2 bugs affect `Int64` → `Double` conversion: (1) `Double(Int64Var)` does bit reinterpretation in Delphi mode, (2) `Int64 * 1.0` gives wrong results near ±2³¹ on AArch64. **Safe:** use implicit assignment (`D := SomeInt64;`) or function parameter passing. For sign-bit checks on `Double`, use `Int64 absolute` overlay (`Bits < 0`), not byte indexing. See [docs/code-style.md](docs/code-style.md#platform-specific-pitfalls).

### Terminology

- **Define** = create a new variable binding (`DefineLexicalBinding`)
- **Assign** = re-assign an existing binding (`AssignLexicalBinding`)
- These are distinct operations — do not conflate them.

### Design Patterns in Use

See [docs/code-style.md](docs/code-style.md#design-patterns) for full descriptions with code examples.

- **Singleton** for special values and shared prototype singletons (pinned via `TGocciaSharedPrototype.Create`)
- **Factory method** for scope creation (`CreateChild`, with optional capacity hint)
- **Context object** for evaluation state (`TGocciaEvaluationContext`)
- **Virtual dispatch** for property access, type discrimination, and scope chain resolution
- **Chain of responsibility** for scope lookup
- **Parser combinator** for binary expressions (`ParseBinaryExpression` shared helper)
- **Recursive descent** for parsing
- **Mark-and-sweep** for garbage collection
- **Shared helpers** for evaluator deduplication (`EvaluateStatements`, `SpreadIterableInto`, `EvaluateSimpleNumericBinaryOp`)

## Value System

All values inherit from `TGocciaValue`. See [docs/value-system.md](docs/value-system.md) for the complete documentation.

**Key virtual methods on `TGocciaValue`:**
- `GetProperty(Name)` / `SetProperty(Name, Value)` — Polymorphic property access. Returns `nil` / no-op by default.
- `IsPrimitive` — `True` for null, undefined, boolean, number, string. Use instead of multi-`is` checks.
- `IsCallable` — `True` for functions and classes. **Exception:** When casting to `TGocciaFunctionBase` after the check, use `is TGocciaFunctionBase` instead (since `TGocciaClassValue` inherits from `TGocciaValue`, not `TGocciaFunctionBase`).

**Error construction:** Centralized in `Goccia.Values.ErrorHelper.pas` (`ThrowTypeError`, `ThrowRangeError`, `ThrowError`). All runtime errors must use these helpers so exceptions are catchable from JavaScript `try...catch`. Built-in argument validation uses `TGocciaArgumentValidator`. All error objects receive a `stack` property via `Goccia.CallStack.pas`.

**Symbol coercion:** `ToNumberLiteral` throws `TypeError`. Implicit string coercion (template literals, `+`) must check for symbols at the operator level. Symbols use a shared prototype singleton with `description` getter and `toString()` method.

**Well-known symbols:** `TGocciaSymbolValue.WellKnownIterator`, `.WellKnownAsyncIterator`, `.WellKnownSpecies`, `.WellKnownMetadata` — all lazily initialized and GC-pinned.

## Built-in Objects

See [docs/built-ins.md](docs/built-ins.md) for complete documentation on all built-ins and how to add new ones.

Standard built-ins (Console, Math, Object, Array, Number, Promise, JSON, JSON5, JSONL, TOML, YAML, Symbol, Set, Map, Performance, Temporal, ArrayBuffer, Proxy, Reflect, URL, etc.) are always registered by the engine. Only optional built-ins use the `TGocciaGlobalBuiltins` flag system:

```pascal
TGocciaGlobalBuiltin = (ggTestAssertions, ggBenchmark, ggFFI);
```

The TestRunner adds `ggTestAssertions`; the BenchmarkRunner adds `ggBenchmark`. FFI (`ggFFI`) is available but not enabled by default.

After all built-ins, two always-present `const` globals are created: `globalThis` (self-referential global object) and `Goccia` (engine metadata with `version`, `commit`, `strictTypes`, `semver`, `build`, `spec`, `proposal`, and `shims`). `Goccia.build` exposes compile-time platform information (`os` and `arch`), mirroring `Deno.build`. `Goccia.spec` and `Goccia.proposal` expose spec and proposal feature data; `Goccia.shims` lists active shims. `strictTypes` is configurable at engine creation.

**JSON/JSON5 source text access (ES2024):** `JSON.parse` and `JSON5.parse` revivers receive `(key, value, context)` where `context` is an object with a `source` property for primitive values (the raw JSON/JSON5 text as written). Objects and arrays get an empty context (no `source` property). Source text collection is implemented via `TAbstractJSONParser.OnValueStart` (position tracking hook in `JSONParser.pas`) and `TGocciaJSONVisitor.RecordSourceText` (captures the raw substring). `TGocciaJSONParser.ParseWithSources` returns both the value tree and a flat source text list consumed in depth-first order by `ApplyReviver`. See [docs/built-ins.md](docs/built-ins.md) for details.

**JSX:** Opt-in preprocessor (`ppJSX in Engine.Preprocessors`). The JSX transformer (`Goccia.JSX.Transformer.pas`) converts JSX to `createElement` calls as a pre-pass. Users provide their own `createElement`/`Fragment`. Custom factory via `@jsxFactory`/`@jsxFragment` pragmas. See [docs/language-restrictions.md](docs/language-restrictions.md#jsx-opt-in).

## Testing

See [docs/testing.md](docs/testing.md) for the complete testing guide.

- **Primary:** JavaScript end-to-end tests in `tests/` — source of truth for correctness
- **Secondary:** Pascal unit tests in `units/*.Test.pas`
- **JS test framework:** `describe`/`test`/`expect` with async support, `.resolves`/`.rejects`, mock functions (`mock()`/`spyOn()`), lifecycle hooks, and Vitest-compatible matchers
- **Key matchers:** `.toBe`, `.toEqual`, `.toContain`, `.toMatch`, `.toThrow(ErrorType)`, `.toHaveBeenCalledWith(...)`, plus `.not` negation
- **Test directories:** Organized by feature under `tests/language/` (includes `async-await/` with top-level await, `for-of/` with top-level for-await-of, `modules/` with import.meta tests) and `tests/built-ins/` (includes `Proxy/` with all 13 trap test files, `FFI/` with library loading and binding tests)
- **Pascal test framework:** Generic `Expect<T>(...).ToBe(...)` assertions. NaN checks: use `Value.ToNumberLiteral.IsNaN` (not `Math.IsNaN`)

## Build System

See [docs/build-system.md](docs/build-system.md) for build system details.

- Build script: `./build.pas` (FreePascal script via `instantfpc`)
- Build modes: `--dev` (default) / `--prod` (O4, stripped, smart-linked)
- Output directory: `build/`
- Auto-formatter: `./format.pas` — auto-fixes uses clause ordering, PascalCase naming, parameter prefixes
- Pre-commit hook: [Lefthook](https://github.com/evilmartians/lefthook) (`lefthook.yml`)
- CI: `ci.yml` (main + tags, full matrix) and `pr.yml` (PRs, ubuntu-latest x64 only, with benchmark comparison)
- Changelog: `CHANGELOG.md` is auto-generated via [git-cliff](https://git-cliff.org/) (`cliff.toml`). Run `git-cliff -o CHANGELOG.md` to regenerate. **Do not edit `CHANGELOG.md` manually — it will be overwritten.**

## Documentation Index

| Document | Description |
|----------|-------------|
| [docs/tutorial.md](docs/tutorial.md) | Your first GocciaScript program — a guided walkthrough for newcomers |
| [docs/architecture.md](docs/architecture.md) | Pipeline overview, component responsibilities, data flow |
| [docs/bytecode-vm.md](docs/bytecode-vm.md) | Bytecode VM architecture, binary format, and runtime model |
| [docs/design-decisions.md](docs/design-decisions.md) | Rationale behind key technical choices |
| [docs/code-style.md](docs/code-style.md) | Naming conventions, patterns, file organization |
| [docs/value-system.md](docs/value-system.md) | Type hierarchy, virtual property access, primitives, objects |
| [docs/built-ins.md](docs/built-ins.md) | Available built-ins, registration system, adding new ones |
| [docs/adding-built-in-types.md](docs/adding-built-in-types.md) | Step-by-step guide for adding new built-in types |
| [docs/testing.md](docs/testing.md) | Test organization, writing tests, running tests |
| [docs/profiling.md](docs/profiling.md) | Bytecode VM profiling: opcodes, pairs, scalar hit rate, function timing, allocations |
| [docs/benchmarks.md](docs/benchmarks.md) | Benchmark runner, output formats, writing benchmarks, CI comparison |
| [docs/build-system.md](docs/build-system.md) | Build commands, configuration, CI/CD |
| [docs/language-restrictions.md](docs/language-restrictions.md) | Supported/excluded features with rationale |
| [docs/decision-log.md](docs/decision-log.md) | Chronological record of key architectural decisions with links |
| [docs/embedding.md](docs/embedding.md) | Embedding the engine in FreePascal applications |
