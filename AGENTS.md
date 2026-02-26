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
./build/ScriptLoader example.js                  # Execute a script
./build/REPL                                      # Start interactive REPL
./build/TestRunner tests/                                                      # Run all JavaScript tests
./build/TestRunner tests/language/expressions/                                 # Run a test category
./build/TestRunner tests --no-progress --exit-on-first-failure                 # CI mode
./build/TestRunner tests --silent                                              # Suppress all console output
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

See [docs/architecture.md](docs/architecture.md) for the full architecture deep-dive.

**Pipeline:** Source → (JSX Transformer) → Lexer → Parser → Interpreter → Evaluator → Result

**Key components:**

| Component | File | Role |
|-----------|------|------|
| Engine | `Goccia.Engine.pas` | Top-level orchestration, built-in registration |
| Lexer | `Goccia.Lexer.pas` | Source → tokens |
| Parser | `Goccia.Parser.pas` | Tokens → AST (including `TGocciaForOfStatement`, `TGocciaForAwaitOfStatement` in `Goccia.AST.Statements.pas`) |
| Interpreter | `Goccia.Interpreter.pas` | AST execution, module loading, scope ownership |
| Evaluator | `Goccia.Evaluator.pas` | Pure AST evaluation (+ sub-modules: Arithmetic, Bitwise, Comparison, Assignment, TypeOperations) |
| Scope | `Goccia.Scope.pas` | Lexical scoping, variable bindings, TDZ, VMT-based chain-walking |
| Reserved Keywords | `Goccia.Keywords.Reserved.pas` | Reserved JavaScript keyword string constants (`break`, `class`, `const`, `this`, etc.) |
| Contextual Keywords | `Goccia.Keywords.Contextual.pas` | Contextual keyword string constants (`async`, `get`, `set`, `type`, `interface`, `implements`, etc.) |
| Timing Utilities | `TimingUtils.pas` | Cross-platform timing: monotonic (`GetNanoseconds`, `GetMilliseconds`), wall-clock (`GetEpochNanoseconds`), and duration formatting (`FormatDuration`) |
| Microtask Queue | `Goccia.MicrotaskQueue.pas` | Singleton FIFO queue for Promise reactions and `queueMicrotask` callbacks, drained after script execution, cleared on exception |
| Call Stack | `Goccia.CallStack.pas` | Singleton call frame stack for `Error.stack` traces — pushed/popped in `EvaluateCall`/`EvaluateNewExpression`, captured at error construction |
| Garbage Collector | `Goccia.GarbageCollector.pas` | Mark-and-sweep memory management for runtime values |
| Iterator Base | `Goccia.Values.IteratorValue.pas` | Iterator protocol base class, shared prototype with helper methods, `Iterator.from()`, `CreateGlobalObject` |
| Concrete Iterators | `Goccia.Values.Iterator.Concrete.pas` | Array/String/Map/Set iterator subclasses with virtual `AdvanceNext` |
| Lazy Iterators | `Goccia.Values.Iterator.Lazy.pas` | Lazy `map`/`filter`/`take`/`drop`/`flatMap` iterator wrappers |
| Generic Iterator | `Goccia.Values.Iterator.Generic.pas` | Wraps user-defined `{next()}` objects as proper iterators |
| ArrayBuffer Value | `Goccia.Values.ArrayBufferValue.pas` | `TGocciaArrayBufferValue` — fixed-length raw binary data buffer backed by `TBytes` |
| SharedArrayBuffer Value | `Goccia.Values.SharedArrayBufferValue.pas` | `TGocciaSharedArrayBufferValue` — shared-memory binary data buffer backed by `TBytes` |
| ArrayBuffer Built-in | `Goccia.Builtins.GlobalArrayBuffer.pas` | ArrayBuffer/SharedArrayBuffer constructors, `isView` static method |
| Enum Value | `Goccia.Values.EnumValue.pas` | TC39 proposal-enum: `TGocciaEnumValue` (null-prototype, non-extensible, iterable via `Symbol.iterator`) |
| Auto-Accessor Helpers | `Goccia.Values.AutoAccessor.pas` | `TGocciaAutoAccessorGetter`, `TGocciaAutoAccessorSetter` — getter/setter methods for auto-accessor backing fields |
| Async Function Values | `Goccia.Values.AsyncFunctionValue.pas` | `TGocciaAsyncFunctionValue`, `TGocciaAsyncArrowFunctionValue`, `TGocciaAsyncMethodValue` — async variants that wrap body execution in Promise |
| Evaluator Decorator Helpers | `Goccia.Evaluator.Decorators.pas` | `TGocciaInitializerCollector`, `TGocciaAccessGetter`, `TGocciaAccessSetter` — helper classes for decorator runtime (FPC closure workaround) |
| JSON Utilities | `Goccia.JSON.pas` | Standalone JSON ↔ `TGocciaValue` parser and stringifier |
| Version | `Goccia.Version.pas` | Git-derived version and commit hash, resolved once at startup via `RunCommand` |
| Temporal Utilities | `Goccia.Temporal.Utils.pas` | ISO 8601 date math helpers, parsing, formatting |
| Temporal Built-in | `Goccia.Builtins.Temporal.pas` | Temporal namespace, constructors, static methods, Temporal.Now |
| File Extensions | `Goccia.FileExtensions.pas` | Centralized file extension constants (`EXT_JS`, `EXT_MJS`, etc.), `ScriptExtensions` array, `IsScriptExtension`/`IsJSXNativeExtension` helpers |
| Module Resolver | `Goccia.Modules.Resolver.pas` | Extensionless imports, path aliases, virtual `Resolve` for custom resolvers |
| JSX Source Map | `Goccia.JSX.SourceMap.pas` | Lightweight internal position mapping for JSX-transformed source |
| JSX Transformer | `Goccia.JSX.Transformer.pas` | Standalone pre-pass that converts JSX to `createElement` calls |
| Logger | `Goccia.Logger.pas` | Configurable logging with levels and output formats |
| Benchmark Reporter | `Goccia.Benchmark.Reporter.pas` | Multi-format benchmark output (console, text, CSV, JSON) with setup/teardown timing |
| REPL Line Editor | `Goccia.REPL.LineEditor.pas` | Interactive line editing with history for the REPL |
| REPL Formatter | `Goccia.REPL.Formatter.pas` | Color-formatted value output for the REPL |
| Shared Prototype | `Goccia.SharedPrototype.pas` | Shared prototype singleton utilities; `Create` auto-pins both prototype and method host with the GC |
| Constants | `Goccia.Constants.pas` | Literal value strings (`'true'`, `'NaN'`, etc.) and numeric constants |
| Constants: Type Names | `Goccia.Constants.TypeNames.pas` | `typeof` result string constants (`'object'`, `'string'`, etc.) |
| Constants: Property Names | `Goccia.Constants.PropertyNames.pas` | Common property name constants (`'length'`, `'constructor'`, etc.) |
| Constants: Error Names | `Goccia.Constants.ErrorNames.pas` | Error type name constants (`'TypeError'`, `'RangeError'`, etc.) |
| Constants: Constructor Names | `Goccia.Constants.ConstructorNames.pas` | Built-in constructor name constants (`'Object'`, `'Array'`, etc.) |
| ToPrimitive | `Goccia.Values.ToPrimitive.pas` | ECMAScript `ToPrimitive` abstract operation |
| Error Helper | `Goccia.Values.ErrorHelper.pas` | `ThrowTypeError`, `ThrowRangeError`, `ThrowDataCloneError` (creates DOMException with code 25), centralized error construction with proper prototype chain |
| Argument Validator | `Goccia.Arguments.Validator.pas` | `RequireExactly`, `RequireAtLeast` — standardized argument count/type validation |
| Argument Callbacks | `Goccia.Arguments.Callbacks.pas` | Pre-typed callback argument collections for array prototype methods |
| Ordered Map | `OrderedMap.pas` | Generic insertion-order-preserving string-keyed map (`TOrderedMap<T>`) |
| Binding Map | `Goccia.Scope.BindingMap.pas` | Ordered map specialized for lexical bindings (`TOrderedMap<TLexicalBinding>`) |
| Array Utils | `Goccia.Utils.Array.pas` | `ArrayCreateDataProperty` helper for spec-compliant array operations |
| TypedArray Value | `Goccia.Values.TypedArrayValue.pas` | `TGocciaTypedArrayValue` — view over ArrayBuffer with fixed element type (Int8, Uint8, Uint8Clamped, Int16, Uint16, Int32, Uint32, Float32, Float64), `TGocciaTypedArrayClassValue`, `TGocciaTypedArrayStaticFrom` |
| Test Console | `Goccia.Builtins.TestConsole.pas` | Silent console override for `--silent` mode in TestRunner |

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
- **One method per file** — each test file focuses on a single method or operation. Never bundle multiple methods into one file (no `prototype-methods.js` or `static-methods.js`).
- **Prototype methods go in `prototype/`** — instance methods live in `BuiltIn/prototype/methodName.js`. Static methods and constructor tests live directly in the `BuiltIn/` folder. See `tests/built-ins/Array/prototype/` for the canonical example.
- **Edge cases are co-located** — edge case tests (NaN, Infinity, negative indices, clamping, empty collections, boundary conditions) belong in the **same file** as the happy-path tests for that method. Do **not** create separate `edge-cases.js` files.
- Always verify changes by running: `./build.pas testrunner && ./build/TestRunner tests`

**When adding a new language feature:**
- Create test files for the feature following the existing directory and naming patterns in `tests/`.
- Tests should cover happy paths, edge cases, and error cases — all in the same file for each method.
- Verify all tests pass before committing.

**When modifying AST logic, scope chain, evaluator, or value types:**
- Build and run the native Pascal test suite: `./build.pas clean tests && for t in build/Goccia.*.Test; do "$t"; done`
- Update the native tests in `units/*.Test.pas` to reflect any changes in behaviour (e.g. new parameters, changed return semantics).
- Both the JavaScript tests **and** the native Pascal tests must pass.

### 4. Garbage Collector Awareness

GocciaScript uses a mark-and-sweep garbage collector (`Goccia.GarbageCollector.pas`). All `TGocciaValue` instances auto-register with the GC via `AfterConstruction`. Key rules:

- **AST literal values** are unregistered from the GC by `TGocciaLiteralExpression.Create` and owned by the AST node. The evaluator calls `Value.RuntimeCopy` to produce fresh GC-managed values when evaluating literals.
- **Singleton values** (e.g., `UndefinedValue`, `TrueValue`, `NaNValue`, `SmallInt` cache) are pinned via `TGocciaGarbageCollector.Instance.PinValue` during engine initialization (consolidated in `PinSingletons`).
- **Shared prototype singletons** (String, Number, Array, Set, Map, Function, Symbol, ArrayBuffer, SharedArrayBuffer, TypedArray) are pinned inside each type's `InitializePrototype` method. `TGocciaSharedPrototype.Create` automatically pins both the prototype object and the method host via `TGocciaGarbageCollector.Instance.PinValue` — no manual pinning is needed after calling `TGocciaSharedPrototype.Create`. All prototype method callbacks must use `ThisValue` (not `Self`) to access instance data, since `Self` refers to the method host singleton. **Object.prototype** is the `ObjectConstructor.Prototype` created in `RegisterBuiltinConstructors` — it hosts `toString()` (ES2026 §20.1.3.6) and is stored in `TGocciaObjectValue.SharedObjectPrototype` so the evaluator can assign it as the prototype of object literals.
- **Pinned values, temp roots, and root scopes** are stored in `TDictionary<T, Boolean>` for O(1) membership checks.
- **Values held only by Pascal code** (not in any GocciaScript scope) must be protected with `AddTempRoot`/`RemoveTempRoot` for the duration they are needed. Example: benchmark functions held in a `TObjectList`.
- **Scopes** register with the GC in their constructor. Active call scopes are tracked via `PushActiveScope`/`PopActiveScope` in `TGocciaFunctionValue.Call`.
- Each value type must override `MarkReferences` to mark all `TGocciaValue` references it holds (prototype, closure, elements, property values, etc.).

### 5. Language Restrictions

GocciaScript intentionally excludes these JavaScript features — do **not** add support for them:
- `var` declarations (use `let`/`const`)
- `function` keyword (use arrow functions or shorthand methods)
- `==` and `!=` loose equality (use `===`/`!==`)
- `eval()` and `arguments` object
- Automatic semicolon insertion (semicolons are required)
- `with` statement
- Traditional loops (`for`, `while`, `do...while`) — use `for...of`, `for await...of`, or array methods instead. `for...of` and `for await...of` are supported.
- Default imports/exports — use named imports/exports
- Global `parseInt`, `parseFloat`, `isNaN`, `isFinite` — use `Number.*` instead (intentional divergence; keeps these functions on the object they belong to)

The parser accepts unsupported syntax (loops, `with`, `function`, `==`, `!=`, default imports/exports, namespace imports, side-effect imports, wildcard re-exports, labeled statements) but treats it as a no-op — the code parses successfully, the unsupported construct is skipped at runtime, and a warning is emitted to stdout with a suggestion. `function` declarations produce `TGocciaEmptyStatement`; `function` expressions evaluate to `undefined`. `==`/`!=` expressions evaluate to `undefined` (both operands are parsed and discarded). Labeled statements strip the label and execute the labeled statement normally. This design preserves the AST for a potential future compatibility mode.

See [docs/language-restrictions.md](docs/language-restrictions.md) for the full list and rationale.

### 6. Types as Comments

GocciaScript supports the TC39 Types as Comments proposal. Type annotations are **parsed but ignored at runtime**. Raw type strings are preserved on AST nodes for potential future optimization. Key parser helpers:

- **`CollectTypeAnnotation(Terminators)`** — Consumes type tokens with balanced bracket tracking, returning the raw text. Stops at any terminator token at depth 0.
- **`CollectGenericParameters`** — Consumes `<...>` generic parameter lists, returning the raw text.

AST nodes with type fields: `TGocciaParameter` (`TypeAnnotation`, `IsOptional`), `TGocciaVariableInfo` (`TypeAnnotation`), `TGocciaDestructuringDeclaration` (`TypeAnnotation`), `TGocciaArrowFunctionExpression` (`ReturnType`), `TGocciaClassMethod` (`ReturnType`, `GenericParams`), `TGocciaClassDefinition` (`GenericParams`, `ImplementsClause`, `InstancePropertyTypes`), `TGocciaTryStatement` (`CatchParamType`).

`type`/`interface` declarations and `import type`/`export type` produce `TGocciaEmptyStatement` (no-op at runtime). Access modifiers (`public`, `protected`, `private`, `readonly`, `override`, `abstract`) in class bodies are consumed and discarded.

### 7. Decorators

GocciaScript supports TC39 Stage 3 decorators ([proposal-decorators](https://github.com/tc39/proposal-decorators)) and decorator metadata ([proposal-decorator-metadata](https://github.com/tc39/proposal-decorator-metadata)).

**Lexer:** `@` is tokenized as `gttAt` in `Goccia.Lexer.pas`.

**Parser:** `ParseDecorators` collects decorator lists; `ParseDecoratorExpression` parses restricted expressions (identifier, member access, call — no private member access or computed access). `ParseClassBody` stores decorators on the unified `TGocciaClassElement` record. Class-level decorators are stored on `TGocciaClassDefinition.FDecorators`.

**AST:** `TGocciaDecoratorList = array of TGocciaExpression` in `Goccia.AST.Expressions.pas`. `TGocciaClassElementKind` (method, getter, setter, field, accessor) and `TGocciaClassElement` record in `Goccia.AST.Statements.pas`. The `FElements` array preserves source order.

**Evaluator:** `EvaluateClassDefinition` in `Goccia.Evaluator.pas` implements three-phase decorator evaluation: (1) evaluate all decorator expressions in source order, (2) call decorators bottom-up with context objects, (3) apply results. Auto-accessors generate private backing fields with getter/setter pairs; the field initializer is registered on the backing field name (e.g. `__accessor_x`), not the public accessor name. The entire decorator pipeline is wrapped in `try..finally` to ensure `MetadataObject` temp root removal and collector cleanup even if a decorator throws. Decorator context properties use `Goccia.Constants.PropertyNames` constants (`PROP_KIND`, `PROP_NAME`, `PROP_STATIC`, `PROP_PRIVATE`, `PROP_METADATA`, `PROP_ACCESS`, `PROP_ADD_INITIALIZER`).

**Helper classes** in `Goccia.Evaluator.Decorators.pas` and `Goccia.Values.AutoAccessor.pas` work around FPC's lack of anonymous closures by encapsulating captured state as class instances whose methods serve as native function callbacks.

**GC awareness:** `TGocciaClassValue.MarkReferences` must mark `FMethodInitializers`, `FFieldInitializers`, and `FDecoratorFieldInitializers[].Initializer` — these hold `TGocciaValue` references to decorator initializer functions that would otherwise be collected.

**`Symbol.metadata`:** `TGocciaSymbolValue.WellKnownMetadata` (lazily initialized, GC-pinned). Registered on the `Symbol` constructor in `Goccia.Builtins.GlobalSymbol.pas`.

**Contextual keyword:** `KEYWORD_ACCESSOR = 'accessor'` in `Goccia.Keywords.Contextual.pas`.

**Not supported:** Parameter decorators.

### 8. `this` Binding Semantics

Two function forms exist with separate AST nodes and runtime types:

- **Arrow functions** (`(x) => x + 1`) — AST: `TGocciaArrowFunctionExpression`, Runtime: `TGocciaArrowFunctionValue`. Always inherit `this` from their lexical (closure) scope via `BindThis` override.
- **Shorthand methods** (`method() { ... }`) — AST: `TGocciaMethodExpression`, Runtime: `TGocciaFunctionValue`. Receive call-site `this` from the receiver.

`this` binding is resolved via virtual dispatch on `TGocciaFunctionValue.BindThis` — no boolean flags or runtime branches. Async variants (`TGocciaAsyncFunctionValue`, `TGocciaAsyncArrowFunctionValue`, `TGocciaAsyncMethodValue`) inherit `this` binding from their non-async superclasses. Array prototype callbacks pass `undefined` as `ThisValue`, so arrow callbacks correctly inherit their enclosing scope's `this`.

At the scope level, `this`, owning class, and super class are resolved via VMT-based chain-walking:
- `Scope.FindThisValue` — walks the parent chain calling `GetThisValue` (virtual) on each scope.
- `Scope.FindOwningClass` / `Scope.FindSuperClass` — same pattern for class resolution.
- `Scope.ResolveIdentifier(Name)` — unified identifier lookup that handles `this` (via `FindThisValue`) and keyword constants before falling back to the scope chain.

Use `Goccia.Keywords.Reserved` and `Goccia.Keywords.Contextual` constants (`KEYWORD_THIS`, `KEYWORD_SUPER`, `KEYWORD_GET`, `KEYWORD_TYPE`, etc.) instead of hardcoded string literals when referencing JavaScript keywords.

See [docs/design-decisions.md](docs/design-decisions.md) for the full design rationale.

## Code Style

See [docs/code-style.md](docs/code-style.md) for the complete style guide.

### Key Conventions

- **Function/procedure names:** PascalCase (e.g., `EvaluateBinary`, `GetProperty`). External C bindings are exempt. Auto-fixed by `./format.pas`.
- **Unit naming:** `Goccia.<Category>.<Name>.pas` (dot-separated hierarchy)
- **No abbreviations:** Use full words in class, function, method, and type names (e.g., `TGocciaGarbageCollector` not `TGocciaGC`). Exceptions: `AST`, `JSON`, `REPL`, `ISO`, `Utils`.
- **File extension constants:** Use `Goccia.FileExtensions` constants (`EXT_JS`, `EXT_JSX`, `EXT_TS`, `EXT_TSX`, `EXT_MJS`, `EXT_JSON`) instead of hardcoded string literals. Use the `ScriptExtensions` array, `IsScriptExtension`, and `IsJSXNativeExtension` helpers instead of duplicating extension lists or ad-hoc checks.
- **Runtime constants:** Use the split constant units instead of hardcoded string literals for property names, type names, error names, constructor names, and symbol names:
  - `Goccia.Constants.PropertyNames` — `PROP_LENGTH`, `PROP_NAME`, `PROP_CONSTRUCTOR`, `PROP_PROTOTYPE`, `PROP_GET`, `PROP_SET`, `PROP_KIND`, `PROP_STATIC`, `PROP_PRIVATE`, `PROP_METADATA`, `PROP_ACCESS`, `PROP_INIT`, `PROP_ADD_INITIALIZER`, etc.
  - `Goccia.Constants.TypeNames` — `OBJECT_TYPE_NAME`, `STRING_TYPE_NAME`, `FUNCTION_TYPE_NAME`, etc.
  - `Goccia.Constants.ErrorNames` — `ERROR_NAME`, `TYPE_ERROR_NAME`, `RANGE_ERROR_NAME`, etc.
  - `Goccia.Constants.ConstructorNames` — `CONSTRUCTOR_OBJECT`, `CONSTRUCTOR_ARRAY`, `CONSTRUCTOR_STRING`, `CONSTRUCTOR_MAP`, etc.
  - `Goccia.Constants.SymbolNames` — `SYMBOL_ITERATOR`, `SYMBOL_ASYNC_ITERATOR`, `SYMBOL_SPECIES`, `SYMBOL_HAS_INSTANCE`, `SYMBOL_TO_PRIMITIVE`, `SYMBOL_TO_STRING_TAG`, `SYMBOL_IS_CONCAT_SPREADABLE`, `SYMBOL_METADATA`
  - `Goccia.Constants` — `BOOLEAN_TRUE_LITERAL`, `NULL_LITERAL`, `NAN_LITERAL`, `ZERO_VALUE`, `EMPTY_STRING`, etc.
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

Dictionary-based string interning (`TDictionary<string, TGocciaStringLiteralValue>` cache) was attempted and **benchmarked at -4% across 172 benchmarks** (49 regressions, 3 improvements). The hash + lookup cost per string exceeds FreePascal's allocation cost. See [docs/design-decisions.md](docs/design-decisions.md) for the full analysis and alternative approaches.

### Platform Pitfall: `Double(Int64)` on AArch64

On FPC 3.2.2 AArch64, `Double(Int64Var)` performs a bit reinterpretation, not a value conversion. Use implicit promotion instead: `Int64Var * 1.0` or `Int64Var * 1000000.0`. See [docs/code-style.md](docs/code-style.md) for details.

### Terminology

- **Define** = create a new variable binding (`DefineLexicalBinding`)
- **Assign** = re-assign an existing binding (`AssignLexicalBinding`)
- These are distinct operations — do not conflate them.

### Design Patterns in Use

- **Singleton** for special values (`undefined`, `null`, `true`, `false`, `NaN`, `Infinity`) and shared prototype singletons (String, Number, Array, Set, Map, Function, Symbol, ArrayBuffer, SharedArrayBuffer, TypedArray — each type uses `class var FShared: TGocciaSharedPrototype` + `InitializePrototype` guarded by `if Assigned`; `TGocciaSharedPrototype.Create` handles GC pinning automatically)
- **Factory method** for scope creation (`CreateChild`, with optional capacity hint)
- **Context object** for evaluation state (`TGocciaEvaluationContext`)
- **Virtual dispatch** for property access (`GetProperty`/`SetProperty`), type discrimination (`IsPrimitive`/`IsCallable`), and scope chain resolution (`GetThisValue`/`GetOwningClass`/`GetSuperClass`) on the `TGocciaValue` and `TGocciaScope` hierarchies
- **Chain of responsibility** for scope lookup
- **Parser combinator** for binary expressions (`ParseBinaryExpression` shared helper)
- **Recursive descent** for parsing
- **Mark-and-sweep** for garbage collection (`TGocciaGarbageCollector`)
- **Shared helpers** for evaluator deduplication (`EvaluateStatementsSafe`, `SpreadIterableInto`, `EvaluateSimpleNumericBinaryOp`)

## Value System

See [docs/value-system.md](docs/value-system.md) for the complete value system documentation.

All values inherit from `TGocciaValue`. Virtual methods on the base class eliminate type-checking at call sites:
- `GetProperty(Name)` / `SetProperty(Name, Value)` — Polymorphic property access. Returns `nil` / no-op by default.
- `IsPrimitive` — Returns `True` for null, undefined, boolean, number, and string types. Use `Value.IsPrimitive` instead of multi-`is` check chains.
- `IsCallable` — Returns `True` for functions and classes. Use `Value.IsCallable` instead of `(Value is TGocciaFunctionBase)` or `(Value is TGocciaFunctionValue) or (Value is TGocciaNativeFunctionValue)`.

The evaluator calls these directly (`Value.GetProperty(Name)`, `Value.IsPrimitive`, `Value.IsCallable`) without type-checking or interface queries. **Prefer these VMT methods over `is` type checks for fundamental type-system properties.** Do not add VMT methods for optional built-in types (e.g., Symbol, Set, Map) — these are toggled via `TGocciaGlobalBuiltins` flags and should use standard RTTI (`is`) checks instead.

Error construction is centralized in `Goccia.Values.ErrorHelper.pas` (`ThrowTypeError`, `ThrowRangeError`, `CreateErrorObject`, etc.). All error objects — both user-created via `new Error()` and runtime-thrown via `ThrowTypeError` etc. — are linked to the correct error type prototype (e.g., `GTypeErrorProto`, `GRangeErrorProto`). This ensures `instanceof TypeError`, `instanceof RangeError`, etc. work correctly for both user-constructed and internally-thrown errors. The global error prototypes are exposed from `Goccia.Builtins.Globals.pas` and follow the ES2026 prototype hierarchy: `TypeError.prototype` → `Error.prototype` → `Object.prototype`. All error objects receive a `stack` property containing a formatted stack trace captured at the point of construction. The call stack is maintained by `Goccia.CallStack.pas`, a singleton that tracks function name, file path, and line/column for each active call frame. Built-in argument validation uses `TGocciaArgumentValidator` (`Goccia.Arguments.Validator.pas`).

**Symbol coercion:** `TGocciaSymbolValue.ToNumberLiteral` throws `TypeError` (symbols cannot convert to numbers). `ToStringLiteral` returns `"Symbol(description)"` for internal use (display, property keys), but implicit string coercion (template literals, `+` operator, `String.prototype.concat`) must check for symbols and throw `TypeError` at the operator level. See `Goccia.Evaluator.Arithmetic.pas` and `Goccia.Evaluator.pas` for the pattern. Symbols use a shared prototype singleton (like String, Number, Array) with `description` as an accessor getter and `toString()` as a method. `Symbol.prototype` is exposed on the Symbol constructor function.

**Well-known symbols:** `Symbol.iterator` is a well-known symbol singleton accessed via `TGocciaSymbolValue.WellKnownIterator`. `Symbol.species` is accessed via `TGocciaSymbolValue.WellKnownSpecies`. `Symbol.metadata` is accessed via `TGocciaSymbolValue.WellKnownMetadata`. All are lazily initialized and GC-pinned. The `TGocciaGlobalSymbol` built-in uses these same instances.

**`Symbol.species` semantics:** The `[Symbol.species]` static getter is registered on `Array`, `Map`, and `Set` constructors in `Goccia.Engine.pas`. The default getter returns `this`, so subclasses inherit the correct constructor. Array prototype methods (`map`, `filter`, `slice`, `concat`, `flat`, `flatMap`, `splice`) use the `ArraySpeciesCreate` helper (`Goccia.Values.ArrayValue.pas`) to create result arrays via the species constructor, enabling subclass-aware array derivation. User-defined classes can override `static get [Symbol.species]()` to control which constructor is used for derived arrays. `TGocciaClassValue` supports symbol-keyed static properties via `FStaticSymbolDescriptors`, `DefineSymbolProperty`, and `GetSymbolPropertyWithReceiver` (which preserves the original receiver when traversing the superclass chain for getter invocation).

**Iterator protocol:** `TGocciaIteratorValue` (`Goccia.Values.IteratorValue.pas`) is the abstract base class for all iterators, providing the shared prototype with helper methods and a virtual `AdvanceNext` method. Iterators are organized into a class hierarchy across four files:

1. **Concrete** (`Goccia.Values.Iterator.Concrete.pas`) — `TGocciaArrayIteratorValue`, `TGocciaStringIteratorValue`, `TGocciaMapIteratorValue`, `TGocciaSetIteratorValue`. Each overrides `AdvanceNext` for its collection type and uses sub-kind enums (`TGocciaArrayIteratorKind`, `TGocciaMapIteratorKind`, `TGocciaSetIteratorKind`) for values/keys/entries variants.
2. **Lazy** (`Goccia.Values.Iterator.Lazy.pas`) — `TGocciaLazyMapIteratorValue`, `TGocciaLazyFilterIteratorValue`, `TGocciaLazyTakeIteratorValue`, `TGocciaLazyDropIteratorValue`, `TGocciaLazyFlatMapIteratorValue`. Each wraps a source iterator and advances on-demand (one element per `next()` call).
3. **Generic** (`Goccia.Values.Iterator.Generic.pas`) — `TGocciaGenericIteratorValue` wraps user-defined iterator objects (plain objects with `next()` method), enabling user-defined iterables to work with spread, destructuring, and `Array.from()`.

All subclasses inherit from `TGocciaIteratorValue`, so existing `is TGocciaIteratorValue` checks in the evaluator work with zero changes. The evaluator's `GetIteratorFromValue` helper resolves iterators by checking for `[Symbol.iterator]` symbol properties on objects (including boxing primitives), and wraps plain `{next()}` objects as generic iterators. A global `Iterator` object with `Iterator.from()` and `Iterator.prototype` is always registered.

**Async functions:** `TGocciaAsyncFunctionValue`, `TGocciaAsyncArrowFunctionValue`, and `TGocciaAsyncMethodValue` (`Goccia.Values.AsyncFunctionValue.pas`) are subclasses of their non-async counterparts. Their `Call` method wraps `ExecuteBody` in a try/except, creating a `TGocciaPromiseValue` that resolves on success or rejects on `TGocciaThrowValue`. Arrow async functions inherit lexical `this` via virtual dispatch on `BindThis`.

## Built-in Objects

See [docs/built-ins.md](docs/built-ins.md) for documentation on all built-ins and how to add new ones.

Built-ins are registered by the engine via `TGocciaGlobalBuiltins` flags:

```pascal
DefaultGlobals = [ggConsole, ggMath, ggGlobalObject, ggGlobalArray,
 ggGlobalNumber, ggPromise, ggJSON, ggSymbol, ggSet, ggMap, ggTemporal, ggJSX, ggArrayBuffer];
```

The TestRunner adds `ggTestAssertions` for the test framework (`describe`, `test`, `expect`).
The BenchmarkRunner adds `ggBenchmark` for the benchmark framework (`suite`, `bench`, `runBenchmarks`). The `bench()` API takes a name and an options object: `bench(name, { setup?, run, teardown? })`. The `setup` function runs once before warmup and its return value is passed to `run` and `teardown`. All three callbacks may be `async` — the resolved value of an `async setup` is passed to `run` and `teardown`, and each phase awaits completion before proceeding. The `run` phase is measured as `opsPerSec` and `meanMs` (per-iteration). Setup and teardown are additionally timed and reported as `setupMs` and `teardownMs` (one-shot, not per-iteration). It supports multiple `--format=console|text|csv|json` flags in a single command (each optionally followed by `--output=file`), `--no-progress` for CI builds, and benchmark calibration via environment variables (`GOCCIA_BENCH_CALIBRATION_MS`, `GOCCIA_BENCH_ROUNDS`, etc.).

`Array.fromAsync(asyncItems [, mapfn [, thisArg]])` creates an array from an async iterable, sync iterable, or array-like, returning a `Promise<Array>`. It tries `[Symbol.asyncIterator]` first, falls back to `[Symbol.iterator]`, then array-like. Each element value is awaited (Promises resolved via synchronous microtask drain).

**`Symbol.asyncIterator`:** `TGocciaSymbolValue.WellKnownAsyncIterator` (lazily initialized, GC-pinned). Registered on the `Symbol` constructor in `Goccia.Builtins.GlobalSymbol.pas`. Used by `for await...of` and `Array.fromAsync` to obtain async iterators.

### JSX Support (Opt-in)

JSX is handled by a **standalone pre-pass transformer** (`Goccia.JSX.Transformer.pas`) that runs before the lexer/parser pipeline when `ggJSX` is enabled. It converts JSX syntax into `createElement` function calls:

```javascript
// Input (JSX)
const el = <div className="active">Hello {name}</div>;

// Output (after transformation)
const el = createElement("div", { className: "active" }, "Hello ", name);
```

The transformer generates an internal source map (`Goccia.JSX.SourceMap.pas`) so that errors in transformed code report correct original line/column numbers. When `ggJSX` is not in the globals set (the default), the transformer is skipped entirely — zero overhead.

**Runtime contract:** Users must provide their own `createElement` function (and `Fragment` for fragments) in scope:

```javascript
const createElement = (tag, props, ...children) => ({ tag, props, children });
const Fragment = Symbol("Fragment");
```

**Custom factory via pragmas:** The factory and fragment names can be overridden per-file using pragma comments at the top of the file:

```javascript
/* @jsxFactory h */
/* @jsxFragment Frag */

const h = (tag, props, ...children) => ({ tag, props, children });
const Frag = Symbol("Frag");
const el = <div>hello</div>; // → h("div", null, "hello")
```

Both `//` and `/* */` comment styles are supported. The pragma must be the first non-whitespace content of the comment.

**Supported file extensions:** `.js`, `.jsx`, `.ts`, `.tsx`, `.mjs`. The ScriptLoader, TestRunner, and BenchmarkRunner all discover files with these extensions when scanning directories. A warning is emitted when JSX syntax is found in `.js`, `.ts`, or `.mjs` files, suggesting the use of `.jsx`/`.tsx` instead.

**Disabling JSX:**

```pascal
// Without JSX — zero overhead
Result := TGocciaEngine.RunScript(Source, FileName, DefaultGlobals - [ggJSX]);
```

**Supported JSX features:** elements, self-closing tags, fragments (`<>...</>`), string/expression/boolean attributes, spread attributes (`{...props}`), shorthand props (`<div {value} />` → `value={value}`), expression children (`{expr}`), nested JSX, dotted component names (`<Foo.Bar />`), uppercase tags as identifier references, `@jsxFactory`/`@jsxFragment` pragmas.

After all flag-gated built-ins are registered, the engine also creates two always-present `const` globals:
- **`globalThis`** — A `const` plain object containing all global scope bindings, with a self-referential `globalThis` property.
- **`GocciaScript`** — Engine metadata object with `version` (semver from git tag, or tag + `-dev`), `commit` (short git hash), and `builtIns` (array of enabled `TGocciaGlobalBuiltin` flag names via RTTI).

## Testing

See [docs/testing.md](docs/testing.md) for the complete testing guide.

- **Primary:** JavaScript end-to-end tests in `tests/` directory — these are the source of truth for correctness
- **Secondary:** Pascal unit tests in `units/*.Test.pas` — only for internal implementation details
- **Test directories:** `tests/language/async-await/` — async functions, await expressions, async class/object methods, error handling; `tests/language/for-of/` — for...of loops, destructuring, break, iterators, for-await-of; `tests/built-ins/Array/fromAsync.js` — Array.fromAsync with async/sync iterables; `tests/built-ins/Symbol/asyncIterator.js` — Symbol.asyncIterator well-known symbol; `tests/built-ins/ArrayBuffer/` — ArrayBuffer constructor, `isView`, `slice`, `Symbol.toStringTag`; `tests/built-ins/SharedArrayBuffer/` — SharedArrayBuffer constructor, `slice`, `Symbol.toStringTag`; `tests/built-ins/TypedArray/` — constructors, element access, prototype methods, static methods, iterators, buffer sharing, edge cases (NaN/Infinity handling, clamping, negative indices); `tests/built-ins/constructors/` — `new` requirement for built-in constructors; `tests/built-ins/structuredClone/arraybuffer.js` — structuredClone of ArrayBuffer/SharedArrayBuffer
- **JS test framework:** built-in `describe`/`test`/`expect` (enabled via `ggTestAssertions`). Supports nested `describe` blocks (suite names are composed with ` > ` separators), `test.skip`/`describe.skip` for unconditional skipping, and `skipIf(condition)`/`runIf(condition)` on both `describe` and `test` for conditional execution. Skip state is inherited by nested describes. Test callbacks can be `async` — `await` works directly in the test body and inside `expect()` calls (e.g., `expect(await somePromise).toBe(42)`). Returning a Promise from a non-async test callback is supported for backward compatibility. Lifecycle hooks (`beforeEach`/`afterEach`) and benchmark callbacks (`bench()`) also support `async` functions with `await`.
- **`.resolves` / `.rejects`:** Vitest/Jest-compatible Promise unwrapping on the `expect()` object. `await expect(promise).resolves.toBe(42)` unwraps a fulfilled Promise; `await expect(promise).rejects.toThrow(TypeError)` unwraps a rejected Promise. Both are getter properties (like `.not`) that drain the microtask queue and return a new expectation with the unwrapped value. `.rejects.toThrow(ErrorType)` checks the rejection reason's error name. Both require an actual Promise — call async functions explicitly: `expect(fn())` not `expect(fn)`.
- **`.toThrow()` best practice:** Always pass an explicit error constructor (`TypeError`, `RangeError`, `Error`, etc.) to `.toThrow()` — e.g. `expect(() => null.foo).toThrow(TypeError)`. Bare `.toThrow()` only asserts *something* throws; the constructor form also verifies the error type.
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
- Auto-formatter: `./format.pas` (instantfpc script, no build step) — auto-fixes uses clause ordering, PascalCase function names, and parameter `A` prefix naming
- Pre-commit hook: [Lefthook](https://github.com/evilmartians/lefthook) (`lefthook.yml`) — requires `lefthook install` after cloning
- Editor setup: `.vscode/settings.json` (format-on-save) + `.vscode/extensions.json` (recommended extensions)

## Documentation Index

| Document | Description |
|----------|-------------|
| [docs/architecture.md](docs/architecture.md) | Pipeline overview, component responsibilities, data flow |
| [docs/design-decisions.md](docs/design-decisions.md) | Rationale behind key technical choices |
| [docs/code-style.md](docs/code-style.md) | Naming conventions, patterns, file organization |
| [docs/value-system.md](docs/value-system.md) | Type hierarchy, virtual property access, primitives, objects |
| [docs/built-ins.md](docs/built-ins.md) | Available built-ins, registration system, adding new ones |
| [docs/adding-built-in-types.md](docs/adding-built-in-types.md) | Step-by-step guide for adding new built-in types |
| [docs/testing.md](docs/testing.md) | Test organization, writing tests, running tests |
| [docs/benchmarks.md](docs/benchmarks.md) | Benchmark runner, output formats, writing benchmarks, CI comparison |
| [docs/build-system.md](docs/build-system.md) | Build commands, configuration, CI/CD |
| [docs/language-restrictions.md](docs/language-restrictions.md) | Supported/excluded features with rationale |
| [docs/embedding.md](docs/embedding.md) | Embedding the engine in FreePascal applications |
