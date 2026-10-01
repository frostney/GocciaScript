# Interpreter

*Tree-walk execution over the AST: `Goccia.Interpreter` and `Goccia.Evaluator.*`, sharing the same value model as bytecode mode but not the same control path.*

## Executive Summary

- **Pipeline** — Source → JSX Transformer → Lexer → Parser → Interpreter → Evaluator → `TGocciaValue`
- **Pure evaluator** — Same expression + context always produces the same result; state changes happen through scope and value objects
- **VMT dispatch** — Expression/statement evaluation dispatches through virtual method tables on AST nodes
- **Scope chain** — Lexical scoping: plain child scopes come from `TGocciaScope.CreateChild`; specialized scopes (call, catch, class-init, `with`, …) are constructed directly with their parent
- **Shared value model** — Both interpreter mode and bytecode mode produce `TGocciaValue` results on the same object model

For **pipelines and the layer diagram**, see [Architecture](architecture.md). For **register VM execution and `.gbc`**, see [Bytecode VM](bytecode-vm.md).

## Pipeline

```text
Source -> JSX Transformer (optional) -> Lexer -> Parser -> Interpreter -> Evaluator -> TGocciaValue
```

Bytecode mode branches after the parser to the compiler and VM instead; both paths end with `TGocciaValue` results on the shared object model.

## Evaluator model (interpreter mode)

`Goccia.Interpreter` drives module loading and top-level execution; `Goccia.Evaluator.*` implements expression and statement evaluation over the AST. Bytecode mode uses `Goccia.Compiler*` / `Goccia.VM*` instead, but the same `TGocciaValue`, scopes, and built-ins.

### Evaluation context and purity

The evaluator threads state through a `TGocciaEvaluationContext` record rather than using instance variables or globals (`Goccia.Evaluator.Context`):

```pascal
TGocciaEvaluationContext = record
  Realm: TGocciaRealm;
  Scope: TGocciaScope;
  OnError: TGocciaThrowErrorCallback;
  LoadModule: TLoadModuleCallback;
  LoadModuleSource: TLoadModuleSourceCallback;
  LoadDeferredModule: TLoadDeferredModuleCallback;
  ResolveModuleURL: TResolveModuleURLCallback;
  CurrentFilePath: string;
  CoverageEnabled: Boolean;
  StrictTypes: Boolean;
  NonStrictMode: Boolean;
  CompatibilityNonStrictMode: Boolean;
  HideFunctionSourceText: Boolean;
  InEvalCode: Boolean;
  EvalVarScope: TGocciaScope;
  RejectArgumentsVarDeclarationInEval: Boolean;
  RejectVarDeclarationNamesInEval: TGocciaEvalRejectNameArray;
  DisposalTracker: TObject; // TGocciaDisposalTracker or nil
  CurrentModule: TGocciaModule;
  ModuleEnvironmentInitialized: Boolean;
end;
```

This keeps evaluator functions pure — all dependencies are explicit parameters. The `OnError` callback is also stored on `TGocciaScope` and propagated to child scopes, so closures always have access to the error handler without global mutable state.

### VMT dispatch on AST nodes

Expression and statement evaluation use **VMT dispatch** on AST nodes: `TGocciaExpression.Evaluate` and `TGocciaStatement.Execute` are abstract virtual methods; each AST subclass overrides the appropriate entry point. The wrappers in `Goccia.Evaluator.pas` record coverage when enabled, then delegate:

```pascal
function EvaluateExpression(const AExpression: TGocciaExpression;
  const AContext: TGocciaEvaluationContext): TGocciaValue;
begin
  // coverage ...
  Result := AExpression.Evaluate(AContext);
end;
```

The top-level `Evaluate` helper still distinguishes **expressions vs statements** with a small `is TGocciaExpression` / `is TGocciaStatement` split before routing; that is not the per-node hot path.

See [spikes/fpc-dispatch-performance.md](spikes/fpc-dispatch-performance.md) for the benchmark analysis comparing virtual, interface, and manual VMT dispatch.

### Shared evaluator helpers

- **`EvaluateStatements`** — Evaluates a list of AST nodes in sequence, returning `TGocciaControlFlow`. Exits early when `Result.Kind <> cfkNormal` to propagate `return` and `break` signals. `TGocciaThrowValue` exceptions propagate naturally without interception.

- **`SpreadIterableInto` / `SpreadIterableIntoArgs`** — Unified spread expansion for arrays, strings, sets, and maps. Used by `EvaluateCall`, `EvaluateArray`, and `EvaluateObject`.

## Design Rationale

### Pure Evaluator Functions

The evaluator (`Goccia.Evaluator.pas`) is designed around pure functions — given the same AST node and evaluation context, the evaluator always produces the same result with no side effects.

**Why this matters:**

- **Testability** — Pure functions are trivially testable in isolation.
- **Reasoning** — No hidden state mutations make the evaluation logic easier to understand and debug.
- **Parallelism potential** — Pure evaluation is inherently safe for concurrent execution.
- **Composability** — Evaluator helper units (`Goccia.Arithmetic` for arithmetic, bitwise, equality, and relational operators; `Goccia.Evaluator.TypeOperations` for `typeof`, `instanceof`, and `in`; `Goccia.Evaluator.Assignment`; …) compose cleanly because they don't share mutable state.
- **ECMAScript conformance** — `ToPrimitive` (`Goccia.Values.ToPrimitive.pas`) is a standalone abstract operation (trying `valueOf` then `toString` on objects) used by the `+` operator and available to any module. `Goccia.Arithmetic` computes `%` as a floating-point remainder (not integer modulo) with NaN/Infinity propagation, and implements the relational operators through the `IsLessThan` abstract operation with type coercion.

State changes (variable bindings, object mutations) happen through the scope and value objects passed in the `TGocciaEvaluationContext`, not through evaluator-internal state.

**Performance-aware evaluation:** Template literal evaluation and `Array.ToStringLiteral` use `TStringBuffer` for O(n) string assembly instead of O(n^2) repeated concatenation. `Boolean.ToNumberLiteral` returns the existing `ZeroValue`/`OneValue` singletons rather than allocating, avoiding an allocation on every boolean-to-number coercion. `Function.prototype.apply` uses a fast path for `TGocciaArrayValue` arguments (direct `Elements[I]` access) instead of per-element `IntToStr` + `GetProperty`. Binary arithmetic lives in `Goccia.Arithmetic` (`EvaluateSubtraction`, `EvaluateMultiplication`, …), where each operator shares the `ToNumericOperands` / `ToNumberPair` coercion helpers.

**IEEE-754 correctness:** Arithmetic operations handle special number values (`NaN`, `Infinity`, `-Infinity`, `-0`) via property accessors on `TGocciaNumberLiteralValue` (`IsNaN`, `IsInfinite`, `IsNegativeZero`). Division uses explicit `IsNegativeZero` checks to compute correct signed results (e.g., `1 / -Infinity` → `-0`, `-1 / 0` → `-Infinity`). Exponentiation delegates to `NumberExponentiation` (`Goccia.NumberExponentiation`), which follows ES2026 §6.1.6.1.3 Number::exponentiate. The sort comparator (`CallCompareFunc`) maps a `NaN` comparison result to `0` and `±Infinity` to `±1` before the stable merge sort (`StableSortElements`) uses it. Negative zero detection (`IsNegativeZero`) calls `NumberBits.IsNegativeZero`, which compares the double's bit pattern with the sign-bit-only pattern, so it does not depend on byte order.

### Scope Chain Design

Scopes form a tree with parent pointers, implementing lexical scoping:

- **`CreateChild` factory method** — Plain block, module, and function scopes come from `CreateChild(AScopeKind, ACustomLabel, ACapacity)`, which creates a `TGocciaScope` with this scope as parent and copies its `this` value. Specialized scopes are constructed directly with their parent — for example `TGocciaCallScope.Create(FClosure, FName, Length(FParameters) + 2)` for a function call, and `TGocciaCatchScope`, `TGocciaClassInitScope`, `TGocciaFunctionNameScope`, and `TGocciaWithScope` during evaluation. Either way the `TGocciaScope` constructor links the parent and inherits its `OnError` callback, module callbacks, and strict-types and non-strict flags. The optional capacity pre-sizes the binding dictionary (function calls pass their parameter count).
- **`OnError` on scopes** — Each scope carries a reference to the error handler callback, inherited from its parent. This allows closures and callbacks to always find the correct error handler without global state.
- **Temporal Dead Zone** — `let`/`const` bindings are registered before initialization, enforcing TDZ semantics (accessing before `=` throws `ReferenceError`).
- **Module scope isolation** — Modules execute in `skModule` scopes (children of the global scope), preventing module-internal variables from leaking into the global scope.
- **Module path resolution** — `TGocciaModuleResolver` handles alias expansion via its inherited `Resolve` method using import-map semantics (exact match for keys without `/`, prefix match for keys with `/`, longest matching key wins), resolves `./` and `../` paths relative to the importing file's directory, tries the engine's module import extensions (`.js`, `.jsx`, `.ts`, `.tsx`, `.mjs`, `.mts`, `.json`, `.txt`, `.md`) followed by the structured-data extensions any installed runtime extension contributes (`.json5`, `.jsonc`, `.jsonl`, `.toml`, `.yaml`, `.yml`, `.csv`, `.tsv`), then index files for extensionless imports, then expands to an absolute path. `TGocciaModuleResolver.LoadImportMap` resolves import-map values relative to the map file and `DiscoverProjectConfig` walks parent directories looking for `goccia.json`. CLI applications also discover a project-level `goccia.toml`, `goccia.json5`, or `goccia.json` (in that priority order) starting from the entry file's directory via `CLI.ConfigFile.DiscoverConfigFile`. Config keys mirror CLI option names (for example, `mode`, `compat-asi`, `timeout`), and CLI options override config values. Config files support `"extends"` to inherit from a base config, enabling per-directory overrides (e.g. `tests/language/asi/goccia.json` enables ASI for that subtree). Absolute paths are used as cache keys to prevent loading the same file via different relative paths. Loader-owned virtual modules are checked before filesystem and import-map resolution; host modules registered with `Engine.RegisterHostModule` are checked before the resolver. Custom resolvers can be injected by subclassing `TGocciaModuleResolver` and overriding the `Resolve` method.
- **Live import bindings** — Named imports are initialized as immutable indirect bindings in the importing module scope. The first successful access retains the resolved target module and binding name, while every access still reads that binding's current value. This avoids repeating `ResolveExport` and namespace lookup without caching the value itself, so mutations, imported-name exports, re-exports, and transitive function captures remain live in interpreter mode and bytecode mode.
- **Circular dependency handling** — Modules are added to the cache (`FModules`) and register their local export bindings before requested modules are evaluated. If a circular import encounters a partially linked module, its import binding can refer to that module without reading the target early; an actual read still observes the target binding's temporal dead zone until its declaration is evaluated.
- **Namespace imports** — `import * as ns from "./module.js"` binds the module's reusable namespace exotic object. The object has a null prototype, sorted enumerable read-only export properties, and live `[[Get]]` behavior. It caches each successfully resolved export identity, not its value, so repeated reads avoid re-walking forwarding chains while JavaScript bindings stay live; structured-data module values remain stable by construction.
- **JSON module imports** — Files ending in `.json` are handled by `TGocciaModuleLoader.LoadJSONModule`, which parses the file via `TGocciaJSONParser` (`Goccia.JSON` unit) and exposes the parsed root as the `default` export and, for object roots, each own key as a named export (a root key named `default` takes that slot). JSON modules bypass the lexer/parser/evaluator pipeline entirely, keeping the import path unified (`import { key } from "./file.json"`). Array roots are objects too, so they also export their indices and `length`; primitive roots export only `default`. JSON modules participate in the same caching and path resolution as JS modules.
- **Standalone JSON utilities** — `Goccia.JSON` provides `TGocciaJSONParser` and `TGocciaJSONStringifier` as dependency-free utility classes that convert between JSON text and `TGocciaValue` types. `Goccia.Builtins.JSON` (the `JSON.parse`/`JSON.stringify` built-in) delegates to these, keeping the built-in a thin adapter. This separation allows the interpreter and any other component to parse JSON without instantiating a built-in.
- **Capability-driven JSON parsing** — `JSONParser.pas` now owns one event-driven parser core plus a `TJSONParserCapabilities` set. Strict JSON uses the empty capability set, while JSON5 opts into comments, trailing commas, single-quoted strings, identifier keys, hexadecimal numbers, signed numbers, `Infinity` / `NaN`, line continuations, and ECMAScript whitespace extensions. This keeps JSON and JSON5 behavior aligned on the shared grammar machinery instead of maintaining two diverging parser implementations.
- **JSON5 parser/stringifier split** — `Goccia.JSON5` provides standalone `TGocciaJSON5Parser` and `TGocciaJSON5Stringifier` utilities. The parser reuses the same core capability-driven parser engine as strict JSON but enables the JSON5 capability set, while the stringifier reuses the shared JSON serialization engine in JSON5 mode instead of maintaining a second formatter. `Goccia.Builtins.JSON5` backs the named exports of `goccia:json5` (`parse`, `stringify`), the module loader reuses the parser for `.json5` imports, and globals injection reuses it for `--globals=file.json5` and embedding helpers.
- **JSON5 compatibility target** — The project goal for JSON5 is full parser compatibility with the reference `json5/json5` implementation plus upstream-aligned stringify behavior. The pinned upstream parser cases are generated as an ordinary JavaScript suite and run directly through `GocciaTestRunner` with the local stringify suite. A rerun on 2026-07-21 against upstream commit `b935d4a280eafa8835e6182551b63809e61243b0` matched 84 of 84 extracted parser cases; the local stringify suite passed all 42 upstream-aligned tests covering special numeric values, quote handling, replacers, boxed primitives, options objects, and pretty-print trailing commas.
- **JSONL parser split** — `Goccia.JSONL` provides a standalone `TGocciaJSONLParser` utility that builds on `TGocciaJSONParser` one line at a time, preserving JSONL source line numbers in parse errors and supporting Bun-style chunked parsing through `ParseChunk(...)`. `Goccia.Builtins.JSONL` backs the named exports of `goccia:jsonl` (`parse`, `parseChunk`), while the module loader reuses the same parser for `.jsonl` imports.
- **JSONL module imports** — `.jsonl` modules intentionally expose each non-empty line as a zero-based string-indexed named export (`"0"`, `"1"`, ...). This keeps the structured-data import surface consistent with the existing string-literal named import/export work and means namespace imports can reuse the same export table without introducing a JSONL-specific synthetic wrapper object just for modules.
- **Text asset module imports** — `.txt` and `.md` modules bypass the script parser and expose a small named-export surface: `content` is the UTF-8 file text with source newlines canonicalized to LF (`\n`), and `metadata` is a frozen object containing `kind`, `path`, `fileName`, `extension`, and `byteLength`. The `default` export is the same string as `content`. Canonicalizing newlines keeps imported text stable across Windows and non-Windows hosts.
- **TOML parser split** — `Goccia.TOML` provides a standalone `TGocciaTOMLParser` utility that converts TOML 1.1.0 text into `TGocciaValue` trees. `Goccia.Builtins.TOML` backs the `parse` named export of `goccia:toml`, and the module loader reuses the same utility for `.toml` imports and TOML-backed globals injection. TOML module imports expose each root-table key as a named export, and namespace imports project that same export table into a module namespace object (non-extensible, with read-only exports). TOML date/time values currently map to validated string scalars rather than Temporal values. For compliance work, the parser also exposes `ParseDocument(...)`, which preserves TOML scalar kinds and canonical values in a recursive TOML node tree without changing the public TOML runtime API.
- **TOML compatibility target** — The project goal for TOML is full TOML 1.1.0 compatibility. The official `toml-test` TOML 1.1.0 suite is part of CI and is rerun across the supported platform matrix through the native `GocciaTOMLComplianceRunner`.
- **YAML parser split** — `Goccia.YAML` provides a standalone `TGocciaYAMLParser` utility that converts YAML text into `TGocciaValue` trees. `Goccia.Builtins.YAML` backs the named exports of `goccia:yaml`: `parse`, which follows Bun-style stream semantics by returning an array whenever explicit `---` document markers are present, and `parseDocuments` for callers that always want an array. The module loader reuses the same utility for `.yaml` and `.yml` imports: a single top-level mapping still exports its keys directly, while multi-document streams expose each document as a string-indexed named export (`"0"`, `"1"`, ...). Namespace imports for YAML file modules project that same export table into a module namespace object (non-extensible, with read-only exports).
- **YAML anchor handling** — Anchors are tracked per document during parsing, aliases resolve to the anchored node, and `<<:` merge keys fill only missing keys so explicit mapping entries always win and earlier entries in merge sequences keep precedence over later ones.
- **YAML block scalars** — Literal (`|`) and folded (`>`) block scalars are parsed directly in `Goccia.YAML`, including chomping modifiers and indentation indicators, so common multi-line configuration text works the same through the `goccia:yaml` `parse` export and `.yaml`/`.yml` module imports.
- **YAML folded inline scalars** — Multi-line plain, single-quoted, and double-quoted scalars are folded directly in `Goccia.YAML`, with blank continuation lines becoming line breaks and non-blank continuation lines folding to spaces. Single-line scalars still pass through the normal implicit typing path, so booleans, nulls, and numbers are not accidentally stringified just because a blank separator follows them.
- **YAML quoted escapes** — Double-quoted YAML scalars decode the YAML 1.2 escape surface directly in `Goccia.YAML`, including `\x`, `\u`, `\U`, YAML-specific escapes like `\N` / `\_` / `\L` / `\P`, and escaped line continuations. Quote scanning now tracks odd vs. even backslash runs so escaped quotes do not corrupt comment stripping, flow parsing, or multiline quoted scalar termination.
- **YAML numeric resolution** — Implicit and tagged numeric scalars use explicit YAML-oriented validation before numeric coercion. Base-prefixed integers (`0x`, `0o`, `0b`), decimal floats, exponent forms, `.inf`, and `.nan` are supported, while malformed underscore placement falls back to plain strings for implicit scalars and remains a parse error for `!!int` / `!!float`.
- **YAML alias graphs** — Anchored mappings, sequences, and flow collections are registered before their children are fully parsed, so aliases can refer back to the container being constructed. That preserves object identity for repeated aliases and enables self-referential structures like `self: *root` and `- *loop` without introducing a second YAML-specific object model.
- **YAML flow collection validation** — Flow-style parsing accepts common YAML shorthands like `[foo: bar]` and trailing commas, but it now rejects malformed empty interior entries such as `[1,,2]` or `{, a: 1}` instead of silently skipping them. This keeps the parser permissive where YAML allows it and explicit where the input is structurally broken.
- **YAML tags and directives** — `%YAML` and `%TAG` directives are parsed at the document preamble, tag handles are expanded per document (including the primary `!` handle), and the standard tags `!!str`, `!!int`, `!!float`, `!!bool`, `!!null`, `!!seq`, `!!map`, `!!timestamp`, and `!!binary` perform explicit coercion, validation, or shape checks. Tagged values preserve metadata through lightweight wrappers that expose `.tagName` and `.value`, while still delegating normal behavior to the wrapped runtime value. The parser now keeps directives tied to the document preamble instead of silently treating mid-document directives as implicit stream splits.
- **YAML complex keys** — Explicit key syntax (`? key`) is supported, including omitted explicit values and zero-indented sequence values, and non-scalar keys are canonicalized into stable JSON-like strings when inserted into `TGocciaObjectValue`. Anchored mapping keys also parse now instead of being rejected. This is a deliberate runtime adaptation: it preserves the ability to parse complex YAML keys without changing GocciaScript's core string-keyed object model.
- **YAML compatibility target** — The project goal for YAML is full YAML 1.2 compatibility plus Bun-compatible user-facing parsing semantics where that does not conflict with GocciaScript's module model. The implementation is intentionally landing in increments rather than claiming full conformance before the parser reaches it. As a concrete snapshot, a parse-validity rerun on 2026-08-19 (engine `0508da44`) against `yaml-test-suite` commit `6ad3d2c62885d82fc349026c136ef560838fdf3d` matched the expected parse/fail result for 336 of 402 cases (83.6%), with 38 false accepts, 28 false rejects, and no timeouts. The latest reruns removed the previously observed parser hangs and improved multiline quoted scalars, flow mappings, document-marker handling, and trailing-content cases materially, but the main remaining gap clusters are still invalid documents that are accepted, remaining tab edge cases, some tag/property composition cases, a small trailing-content cluster, and a small number of remaining flow cases.
- **Specialized scope hierarchy** — `TGocciaGlobalScope` (root), `TGocciaCallScope` (function calls), `TGocciaArrowCallScope` (arrow function calls), `TGocciaMethodCallScope` (class method calls with `SuperClass`/`OwningClass`), `TGocciaClassInitScope` (instance property initialization), `TGocciaCatchScope` (catch parameter scoping), and `TGocciaWithScope` (object environment records for `with`). Each specialized scope overrides virtual methods (`GetThisValue`, `GetOwningClass`, `GetSuperClass`, `IsFunctionBoundary`) to participate in VMT-based chain-walking.
- **VMT-based chain-walking** — `FindOwningClass`, `FindSuperClass`, and `FindNewTarget` walk the parent chain calling the corresponding virtual `Get*` method on each scope, stopping at the first non-`nil` result or when `IsFunctionBoundary` returns `True` (ordinary functions do not inherit `super`, `new.target`, or owning class). `TGocciaArrowCallScope` overrides `IsFunctionBoundary` to return `False`, keeping arrow functions transparent to these walks.
- **Unified identifier resolution** — `ResolveIdentifier(Name)` on `TGocciaScope` handles `this` (via `FindThisValue`) and keyword constants (via `Goccia.Keywords.Reserved`) before falling back to the standard scope chain walk, avoiding scattered special-case checks in the evaluator. `ResolveIdentifierReference(Name, Value, ThisValue)` is used for identifier-call sites so `with`-provided methods receive the object environment as `this` while normal lexical/global calls still receive `undefined`.
- **Compatibility call bindings** — With `--compat-arguments-object` enabled, ordinary functions, shorthand methods, accessors, and generators create an `arguments` object in their call scope unless a parameter list already binds `arguments`; arrow call scopes deliberately skip it so `arguments` remains lexical. `--compat-non-strict-mode` does not enable the object by itself. Strict functions, modules, and non-simple parameter lists create unmapped objects; sloppy simple parameter lists create mapped arguments exotic objects whose indexed properties alias parameter bindings until the mapping is broken by deletion or descriptor changes. Ordinary function calls also coerce nullish `this` to `globalThis` only with script source `--compat-non-strict-mode`; arrows keep lexical `this`.
- **`with` object environments** — With `--compat-non-strict-mode` enabled for script source, `TGocciaWithScope` wraps the object produced by `ToObject`, checks `HasProperty`, honors `Symbol.unscopables`, and forwards reads/writes to that object before falling back to the parent scope. Writes use receiver-aware object assignment; a failed object `[[Set]]` raises `TypeError` by default and is ignored in non-strict compatibility mode. The scope is GC-managed rather than stack-owned because closures created inside the `with` body can retain it after the body exits.
- **Non-strict assignment and `delete`** — With `--compat-non-strict-mode` enabled for script source, failed ordinary object/global writes are ignored while assignment expressions still return the assigned value. `delete identifier` uses scope `DeleteBinding` semantics (`false` for declared bindings, deletes configurable global object properties, `true` for unresolvable names), and property deletion returns `false` for non-configurable properties instead of raising the strict-mode `TypeError`.

### Error Handling Strategy

GocciaScript uses a layered error approach (see [Errors](errors.md) for the full error type reference and user-facing display format):

1. **Compile-time errors** (lexer/parser) use Pascal exceptions (`TGocciaSyntaxError` and its subclass `TGocciaLexerError`) — these terminate parsing immediately.
2. **Runtime errors** use a callback pattern (`OnError` in `TGocciaEvaluationContext`) — this keeps evaluator functions pure.
3. **JavaScript-level errors** use `TGocciaThrowValue` for `throw` statements and `try/catch` — these flow through the evaluator's return path.
4. **`try-finally` without `catch`** — The evaluator wraps the Pascal `try...except` in a Pascal `try...finally` to guarantee the JS `finally` block runs before exceptions propagate, even when no `catch` clause exists.
5. **`break` and `return`** — Use `TGocciaControlFlow` result records (`cfkBreak`, `cfkReturn`) instead of Pascal exceptions. Statement-level evaluator functions return `TGocciaControlFlow`, and callers check `Result.Kind` to propagate signals. This eliminates `FPC_SETJMP` overhead from the interpreter's hot path (function calls, loop iterations, switch statements). `EvaluateSwitch` checks `CF.Kind = cfkBreak` after each case statement to implement JavaScript's fall-through-until-break semantics.

**Centralized error construction** — `Goccia.Values.ErrorHelper.pas` provides `ThrowTypeError`, `ThrowRangeError`, `ThrowReferenceError`, and `CreateErrorObject` helpers. All error throw sites across the codebase use these helpers instead of manually building error objects, reducing duplication and ensuring consistent error formatting.

**Why not exceptions everywhere?** Pascal exceptions disrupt the pure-function model of the evaluator. The callback pattern allows the evaluator to signal errors without unwinding the call stack, making control flow explicit. `TGocciaThrowValue` is the only exception used for non-local exits — it propagates naturally through the call stack to `EvaluateTry` (JS `try...catch`) or the top-level handler. `return` and `break` use lightweight `TGocciaControlFlow` records instead of exceptions, avoiding `setjmp`/`longjmp` overhead on every function call and loop iteration.

### Synchronous Microtask Queue

GocciaScript implements ECMAScript Promises with a synchronous microtask queue (`Goccia.MicrotaskQueue.pas`) that drains after each top-level script execution.

**The problem:** Promise `.then()` callbacks must be deferred (never synchronous), but GocciaScript is a synchronous engine with no event loop.

**The solution:** A singleton FIFO queue. When a Promise settles or `.then()` is called on an already-settled Promise, the reaction is enqueued rather than executed immediately. The engine drains the queue after the executor finishes the program (`TGocciaEngine.ExecuteProgram` → `WaitForRuntimeIdle`), in both execution modes.

**Nested engines:** The singleton is per thread, and engines nest on a thread: a sandbox `runScript` child executes inside its caller's statement, and so does an `Execute` an embedder calls from a native callback. An `Execute` that starts while another engine run (`Execute`, `ExecuteProgram`, or `RunModule`) is in progress on the thread therefore runs in a microtask scope of its own (`TGocciaMicrotaskQueue.EnterScope` / `LeaveScope`):

- Its drain runs only the jobs enqueued in that scope, so it returns before any of the enclosing engine's pending jobs run. They run afterwards, in the order they were enqueued, once the enclosing engine reaches its own drain.
- Its cleanup discards only the jobs of that scope. A nested `Execute` that throws leaves the enclosing engine's callbacks and `async` continuations queued.
- A Promise's jobs belong to the scope the Promise was created in, and a `FinalizationRegistry` cleanup job to the scope that created the registry. When a nested engine's drain settles an enclosing engine's Promise — a fetch that completed in the meantime — its reactions wait in the enclosing engine's scope instead of running inside the nested one. A job whose scope has already ended is enqueued into the scope that is current when it comes due.

This is the ECMAScript job rule ([ES2026 §9.5](https://tc39.es/ecma262/#sec-jobs)): a job runs only when its agent's execution context stack is empty. A ShadowRealm is a second realm of the same agent rather than a separate execution, so it shares its creator's scope. The outermost `Execute` keeps the thread's own scope, so jobs the host queued before it — by evaluating a globals module ahead of the entry script — drain with it. Only `Execute` isolates: a nested `ExecuteProgram` or `RunModule` drains the scope it was called in. [ADR 0123](adr/0123-per-execution-microtask-scopes.md) records the decision.

Fetch uses a separate fetch-specific completion pump: blocking HTTP work runs off-thread, the owning runtime thread settles the fetch Promise when a response or error is ready, and the resulting Promise reactions still run through this same microtask queue. The microtask queue itself is not used as an I/O queue.

**Why drain after script execution (not during)?**

In the ECMAScript specification, the entire script is one macrotask. Microtasks drain after the current macrotask completes, not interleaved with synchronous code. This means:

1. All synchronous code runs to completion first.
2. All `.then()` callbacks fire in FIFO order.
3. New microtasks enqueued during draining (e.g., chained `.then()` handlers) are processed in the same drain cycle.

This follows the ECMAScript specification's microtask ordering semantics. Thenable adoption (resolving a Promise with another Promise) is deferred by one microtask tick, matching the spec's PromiseResolveThenableJob. When `Resolve(innerPromise)` is called, instead of synchronously calling `SubscribeTo`, a `prtThenableResolve` microtask is enqueued. When this microtask drains, it calls `SubscribeTo` to adopt the inner Promise's state — resulting in a 2-tick deferral (one for the thenable resolve job, one for the settlement reaction). This ensures correct ordering relative to other microtasks.

There is one macrotask source, and it is deliberately not an event loop: `Goccia.Timers.pas` holds a [virtual timer queue](adr/0113-deterministic-virtual-timer-queue.md) behind `setTimeout` and `setInterval`, installed in the test-runner profile. Nothing there waits on wall time. A timer runs only when a test advances the virtual clock (`vi.advanceTimersByTime` and friends) or, without fake timers, when the engine would otherwise have nothing left to do — an `await` on a promise a timer will settle, or the end-of-run idle drain. Each such step runs one timer and then drains this microtask queue, which is the macrotask-then-microtask ordering an event loop provides, minus the loop and minus real elapsed time. Other macrotask sources — I/O callbacks, event handlers — remain unimplemented.

**Integration points:**

| Context | When microtasks drain |
|---------|----------------------|
| `TGocciaEngine.Execute` | After the script program (through `ExecuteProgram`) or the module body finishes (`WaitForRuntimeIdle`) |
| `TGocciaEngine.ExecuteProgram` | After `FExecutor.ExecuteProgram` returns, in either execution mode (`WaitForRuntimeIdle`) |
| `TGocciaEngine.RunModule` / `RunModuleInScope` | After the executor runs a precompiled `TGocciaCompiledModule` (`WaitForRuntimeIdle`) |
| Test framework | After each test callback |
| Benchmark runner | After warmup, calibration batches, and each measurement round |

For fetch-backed Promises, these integration points also pump fetch completions before treating a pending Promise as permanently unsettled.

**`queueMicrotask`:** The global `queueMicrotask(callback)` function enqueues a user-provided callback into the same microtask queue used by Promise reactions. This matches the [HTML spec](https://html.spec.whatwg.org/multipage/timers-and-user-prompts.html#microtask-queuing). If a `queueMicrotask` callback throws, the error is surfaced as an uncaught host callback error instead of being converted into a Promise rejection. Promise reaction handler errors still reject their result promises.

**Error safety:** `TGocciaEngine.Execute` wraps the whole source pipeline and execution path in a `try..finally` that calls `ClearQueue` — a nested run leaves its microtask scope instead, which discards only its own jobs — and discards pending fetch completions. If the interpreter throws, stale microtasks and fetch callbacks are discarded rather than leaking into subsequent executions; outstanding fetch workers are detached so cleanup does not wait on network I/O that can no longer affect the script. Lower-level callers that bypass `Execute` and call `ExecuteProgram` directly still get the idle drain, but they own any surrounding runtime cleanup.

**GC safety:** During `DrainQueue`, each microtask's handler, value, and result promise are temp-rooted to prevent collection mid-callback. Queued microtasks and FinalizationRegistry cleanup jobs are also registered as queued GC roots until they run.

## Related documents

- [Architecture](architecture.md) — Shared source pipeline, both execution modes, main layers, design direction
- [Bytecode VM](bytecode-vm.md) — Compiler output, opcodes, `TGocciaVM`
- [Core patterns](core-patterns.md) — Recurring implementation patterns
- [GocciaScript Context](../CONTEXT.md) — Canonical project terminology
- [Value system](value-system.md) — `TGocciaValue` hierarchy
- [Contributing](../CONTRIBUTING.md) — Workflow and code style
