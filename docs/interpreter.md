# Interpreter

*Tree-walk execution over the AST: `Goccia.Interpreter` and `Goccia.Evaluator.*`, sharing the same runtime values as bytecode mode but not the same control path.*

## Executive Summary

- **Pipeline** — Source → JSX Transformer → Lexer → Parser → Interpreter → Evaluator → `TGocciaValue`
- **Pure evaluator** — Same expression + context always produces the same result; state changes happen through scope and value objects
- **VMT dispatch** — Expression/statement evaluation dispatches through virtual method tables on AST nodes
- **Scope chain** — Lexical scoping via `TGocciaScope.CreateChild`; no direct instantiation
- **Shared runtime** — Both interpreter and bytecode modes produce `TGocciaValue` results on the same object model

For **pipelines and the layer diagram**, see [Architecture](architecture.md). For **register VM execution and `.gbc`**, see [Bytecode VM](bytecode-vm.md).

## Pipeline

```text
Source -> JSX Transformer (optional) -> Lexer -> Parser -> Interpreter -> Evaluator -> TGocciaValue
```

Bytecode mode branches after the parser to the compiler and VM instead; both paths end with `TGocciaValue` results on the shared object model.

## Evaluator model (interpreted backend)

`Goccia.Interpreter` drives module loading and top-level execution; `Goccia.Evaluator.*` implements expression and statement evaluation over the AST. Bytecode mode uses `Goccia.Compiler*` / `Goccia.VM*` instead, but the same `TGocciaValue`, scopes, and built-ins.

### Evaluation context and purity

The evaluator threads state through a `TGocciaEvaluationContext` record rather than using instance variables or globals:

```pascal
TGocciaEvaluationContext = record
  Scope: TGocciaScope;
  OnError: TGocciaThrowErrorCallback;
  LoadModule: TLoadModuleCallback;
  CurrentFilePath: string;
  CoverageEnabled: Boolean;
  DisposalTracker: TObject; // TGocciaDisposalTracker or nil
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

- **`EvaluateSimpleNumericBinaryOp`** — Shared helper for subtraction, multiplication, and exponentiation, which all share the same pattern of numeric coercion, NaN propagation, and a single-operation callback.

## Design Rationale

### Pure Evaluator Functions

The evaluator (`Goccia.Evaluator.pas`) is designed around pure functions — given the same AST node and evaluation context, the evaluator always produces the same result with no side effects.

**Why this matters:**

- **Testability** — Pure functions are trivially testable in isolation.
- **Reasoning** — No hidden state mutations make the evaluation logic easier to understand and debug.
- **Parallelism potential** — Pure evaluation is inherently safe for concurrent execution.
- **Composability** — Evaluator sub-modules (`Arithmetic`, `Bitwise`, `Comparison`, etc.) compose cleanly because they don't share mutable state.
- **ECMAScript conformance** — `ToPrimitive` (`Goccia.Values.ToPrimitive.pas`) is a standalone abstract operation (trying `valueOf` then `toString` on objects) used by the `+` operator and available to any module. The arithmetic module uses floating-point modulo (not integer) with proper NaN/Infinity propagation. The comparison module implements the Abstract Relational Comparison algorithm with type coercion.

State changes (variable bindings, object mutations) happen through the scope and value objects passed in the `TGocciaEvaluationContext`, not through evaluator-internal state.

**Performance-aware evaluation:** Template literal evaluation and `Array.ToStringLiteral` use `TStringBuffer` for O(n) string assembly instead of O(n^2) repeated concatenation. `Boolean.ToNumberLiteral` returns the existing `ZeroValue`/`OneValue` singletons rather than allocating, avoiding an allocation on every boolean-to-number coercion. `Function.prototype.apply` uses a fast path for `TGocciaArrayValue` arguments (direct `Elements[I]` access) instead of per-element `IntToStr` + `GetProperty`. Numeric binary operations that share a common pattern (subtraction, multiplication, exponentiation) are consolidated through `EvaluateSimpleNumericBinaryOp` to avoid code duplication while maintaining clear semantics.

**IEEE-754 correctness:** Arithmetic operations handle special number values (`NaN`, `Infinity`, `-Infinity`, `-0`) via property accessors on `TGocciaNumberLiteralValue` (`IsNaN`, `IsInfinite`, `IsNegativeZero`). Division uses explicit `IsNegativeZero` checks to compute correct signed results (e.g., `1 / -Infinity` → `-0`, `-1 / 0` → `-Infinity`). Exponentiation uses an `IsActualZero` guard to distinguish true zero exponents, and checks `RightNum.IsInfinite` before `RightNum.Value = 0` to correctly handle infinite exponents. The sort comparator (`CallCompareFunc`) maps infinite comparison results to `±1` to avoid passing raw `Double` values to the quicksort partitioning logic. Negative zero detection (`IsNegativeZero`) uses an endian-neutral `Int64 absolute` overlay to check the sign bit (`Bits < 0`) instead of byte-indexed checks that assume little-endian layout.

### Scope Chain Design

Scopes form a tree with parent pointers, implementing lexical scoping:

- **`CreateChild` factory method** — Scopes are never instantiated directly. `CreateChild` ensures proper parent linkage, scope kind propagation, and `OnError` callback inheritance. An optional `ACapacity` parameter allows callers to pre-size the binding dictionary (used by function calls that know their parameter count).
- **`OnError` on scopes** — Each scope carries a reference to the error handler callback, inherited from its parent. This allows closures and callbacks to always find the correct error handler without global state.
- **Temporal Dead Zone** — `let`/`const` bindings are registered before initialization, enforcing TDZ semantics (accessing before `=` throws `ReferenceError`).
- **Module scope isolation** — Modules execute in `skModule` scopes (children of the global scope), preventing module-internal variables from leaking into the global scope.
- **Module path resolution** — `TGocciaModuleResolver` handles alias expansion via its inherited `Resolve` method using import-map semantics (exact match for keys without `/`, prefix match for keys with `/`, longest matching key wins), resolves `./` and `../` paths relative to the importing file's directory, tries the shared module import extensions (`.js`, `.jsx`, `.ts`, `.tsx`, `.mjs`, `.json`, `.json5`, `.jsonl`, `.toml`, `.yaml`, `.yml`, `.txt`, `.md`) and index files for extensionless imports, then expands to an absolute path. `TGocciaModuleResolver.LoadImportMap` resolves import-map values relative to the map file and `DiscoverProjectConfig` walks parent directories looking for `goccia.json`. CLI applications also discover a project-level `goccia.toml`, `goccia.json5`, or `goccia.json` (in that priority order) starting from the entry file's directory via `CLI.ConfigFile.DiscoverConfigFile`. The config file sets defaults for any CLI option (e.g. `mode`, `asi`, `timeout`); CLI arguments override config values. Config files support `"extends"` to inherit from a base config, enabling per-directory overrides (e.g. `tests/language/asi/goccia.json` enables ASI for that subtree). Absolute paths are used as cache keys to prevent loading the same file via different relative paths. Global modules (bare specifiers registered via `Engine.RegisterGlobalModule`) are checked before the resolver runs. Custom resolvers can be injected by subclassing `TGocciaModuleResolver` and overriding the `Resolve` method.
- **Circular dependency handling** — Modules are added to the cache (`FModules`) before execution and to a loading set (`FLoadingModules`) for the duration. If a circular import is encountered, the partially-populated module is returned from cache. Inline exports (`export const`) that execute before the circular import point are available; `export { x }` declarations processed post-execution are not. This matches the observable behavior of ES module live bindings for the common case.
- **Namespace imports** — `import * as ns from "./module.js"` now materializes a namespace object from `TGocciaModule.ExportsTable` instead of being skipped by the parser. The namespace object is created with a null prototype, populated with enumerable read-only data properties for each export, then frozen before binding. The result is an import-time snapshot rather than a live export mirror, which keeps namespace imports lightweight while matching the expected read-only shape for both script modules and structured-data modules.
- **JSON module imports** — Files ending in `.json` are handled by `LoadJsonModule`, which parses the file via `TGocciaJSONParser` (`Goccia.JSON` unit) and exposes each top-level object key as a named export. JSON modules bypass the lexer/parser/evaluator pipeline entirely, keeping the import path unified (`import { key } from "./file.json"`). Non-object JSON root values (arrays, primitives) produce a module with no exports. JSON modules participate in the same caching and path resolution as JS modules.
- **Standalone JSON utilities** — `Goccia.JSON` provides `TGocciaJSONParser` and `TGocciaJSONStringifier` as dependency-free utility classes that convert between JSON text and `TGocciaValue` types. `Goccia.Builtins.JSON` (the `JSON.parse`/`JSON.stringify` built-in) delegates to these, keeping the built-in a thin adapter. This separation allows the interpreter and any other component to parse JSON without instantiating a built-in.
- **Capability-driven JSON parsing** — `JSONParser.pas` now owns one event-driven parser core plus a `TJSONParserCapabilities` set. Strict JSON uses the empty capability set, while JSON5 opts into comments, trailing commas, single-quoted strings, identifier keys, hexadecimal numbers, signed numbers, `Infinity` / `NaN`, line continuations, and ECMAScript whitespace extensions. This keeps JSON and JSON5 behavior aligned on the shared grammar machinery instead of maintaining two diverging parser implementations.
- **JSON5 parser/stringifier split** — `Goccia.JSON5` provides standalone `TGocciaJSON5Parser` and `TGocciaJSON5Stringifier` utilities. The parser reuses the same core capability-driven parser engine as strict JSON but enables the JSON5 capability preset, while the stringifier reuses the shared JSON serialization engine in JSON5 mode instead of maintaining a second formatter. `Goccia.Builtins.JSON5` exposes these as `JSON5.parse(...)` and `JSON5.stringify(...)`, the module loader reuses the parser for `.json5` imports, and globals injection reuses it for `--globals=file.json5` and embedding helpers.
- **JSON5 compatibility target** — The project goal for JSON5 is full parser compatibility with the reference `json5/json5` implementation plus upstream-aligned stringify behavior. The local rerun command is `python3 scripts/run_json5_test_suite.py`, or `python3 scripts/run_json5_test_suite.py --harness=./build/GocciaJSON5Check` if you already built the parser decoder harness. That command runs both the upstream parser corpus and the local upstream-aligned stringify suite. A rerun on 2026-04-05 matched 84 of 84 extracted upstream parser cases, and the stringify half covers special numeric values, quote handling, replacers, boxed primitives, options objects, and pretty-print trailing commas.
- **JSONL parser split** — `Goccia.JSONL` provides a standalone `TGocciaJSONLParser` utility that builds on `TGocciaJSONParser` one line at a time, preserving JSONL source line numbers in parse errors and supporting Bun-style chunked parsing through `ParseChunk(...)`. `Goccia.Builtins.JSONL` exposes that utility as `JSONL.parse(...)` and `JSONL.parseChunk(...)`, while the module loader reuses the same parser for `.jsonl` imports.
- **JSONL module imports** — `.jsonl` modules intentionally expose each non-empty line as a zero-based string-indexed named export (`"0"`, `"1"`, ...). This keeps the structured-data import surface consistent with the existing string-literal named import/export work and means namespace imports can reuse the same export table without introducing a JSONL-specific synthetic wrapper object just for modules.
- **Text asset module imports** — `.txt` and `.md` modules bypass the script parser and expose a small named-export surface instead: `content` is the UTF-8 file text with source newlines canonicalized to LF (`\n`), and `metadata` is a frozen object containing `kind`, `path`, `fileName`, `extension`, and `byteLength`. This keeps asset imports aligned with the named-export-only module philosophy without inventing a default export wrapper just for plain text files, while also making imported text stable across Windows and non-Windows hosts.
- **TOML parser split** — `Goccia.TOML` provides a standalone `TGocciaTOMLParser` utility that converts TOML 1.1.0 text into `TGocciaValue` trees. `Goccia.Builtins.TOML` exposes that parser as `TOML.parse(...)`, and the module loader reuses the same utility for `.toml` imports and TOML-backed globals injection. TOML module imports expose each root-table key as a named export, and namespace imports project that same export table into a frozen namespace object. TOML date/time values currently map to validated string scalars rather than Temporal values. For compliance work, the parser also exposes `ParseDocument(...)`, which preserves TOML scalar kinds and canonical values in a recursive TOML node tree without changing the public runtime surface.
- **TOML compatibility target** — The project goal for TOML is full TOML 1.1.0 compatibility. The official `toml-test` TOML 1.1.0 suite is part of CI and is rerun across the supported platform matrix. The local rerun command is `python3 scripts/run_toml_test_suite.py`, or `python3 scripts/run_toml_test_suite.py --harness=./build/GocciaTOMLCheck` if you already built the decoder harness.
- **YAML parser split** — `Goccia.YAML` provides a standalone `TGocciaYAMLParser` utility that converts YAML text into `TGocciaValue` trees. `Goccia.Builtins.YAML` exposes that parser as `YAML.parse(...)`, which follows Bun-style stream semantics by returning an array whenever explicit `---` document markers are present, and `YAML.parseDocuments(...)` for callers that always want an array. The module loader reuses the same utility for `.yaml` and `.yml` imports: a single top-level mapping still exports its keys directly, while multi-document streams expose each document as a string-indexed named export (`"0"`, `"1"`, ...). Namespace imports for YAML simply project that same export table into a frozen namespace object.
- **YAML anchor handling** — Anchors are tracked per document during parsing, aliases resolve to the anchored node, and `<<:` merge keys fill only missing keys so explicit mapping entries always win and earlier entries in merge sequences keep precedence over later ones.
- **YAML block scalars** — Literal (`|`) and folded (`>`) block scalars are parsed directly in `Goccia.YAML`, including chomping modifiers and indentation indicators, so common multi-line configuration text works the same through `YAML.parse(...)` and `.yaml`/`.yml` module imports.
- **YAML folded inline scalars** — Multi-line plain, single-quoted, and double-quoted scalars are folded directly in `Goccia.YAML`, with blank continuation lines becoming line breaks and non-blank continuation lines folding to spaces. Single-line scalars still pass through the normal implicit typing path, so booleans, nulls, and numbers are not accidentally stringified just because a blank separator follows them.
- **YAML quoted escapes** — Double-quoted YAML scalars decode the YAML 1.2 escape surface directly in `Goccia.YAML`, including `\x`, `\u`, `\U`, YAML-specific escapes like `\N` / `\_` / `\L` / `\P`, and escaped line continuations. Quote scanning now tracks odd vs. even backslash runs so escaped quotes do not corrupt comment stripping, flow parsing, or multiline quoted scalar termination.
- **YAML numeric resolution** — Implicit and tagged numeric scalars use explicit YAML-oriented validation before numeric coercion. Base-prefixed integers (`0x`, `0o`, `0b`), decimal floats, exponent forms, `.inf`, and `.nan` are supported, while malformed underscore placement falls back to plain strings for implicit scalars and remains a parse error for `!!int` / `!!float`.
- **YAML alias graphs** — Anchored mappings, sequences, and flow collections are registered before their children are fully parsed, so aliases can refer back to the container being constructed. That preserves object identity for repeated aliases and enables self-referential structures like `self: *root` and `- *loop` without introducing a second YAML-specific object model.
- **YAML flow collection validation** — Flow-style parsing accepts common YAML shorthands like `[foo: bar]` and trailing commas, but it now rejects malformed empty interior entries such as `[1,,2]` or `{, a: 1}` instead of silently skipping them. This keeps the parser permissive where YAML allows it and explicit where the input is structurally broken.
- **YAML tags and directives** — `%YAML` and `%TAG` directives are parsed at the document preamble, tag handles are expanded per document (including the primary `!` handle), and the standard tags `!!str`, `!!int`, `!!float`, `!!bool`, `!!null`, `!!seq`, `!!map`, `!!timestamp`, and `!!binary` perform explicit coercion, validation, or shape checks. Tagged values preserve metadata through lightweight wrappers that expose `.tagName` and `.value`, while still delegating normal behavior to the wrapped runtime value. The parser now keeps directives tied to the document preamble instead of silently treating mid-document directives as implicit stream splits.
- **YAML complex keys** — Explicit key syntax (`? key`) is supported, including omitted explicit values and zero-indented sequence values, and non-scalar keys are canonicalized into stable JSON-like strings when inserted into `TGocciaObjectValue`. Anchored mapping keys also parse now instead of being rejected. This is a deliberate runtime adaptation: it preserves the ability to parse complex YAML keys without changing GocciaScript's core string-keyed object model.
- **YAML compatibility target** — The project goal for YAML is full YAML 1.2 compatibility plus Bun-compatible user-facing parsing semantics where that does not conflict with GocciaScript's module model. The implementation is intentionally landing in increments rather than claiming full conformance before the parser reaches it. As a concrete snapshot, a parse-validity rerun against the official `yaml-test-suite` `data` branch on 2026-04-02 matched the expected parse/fail result for 336 of 402 cases (83.6%). The latest reruns removed the previously observed parser hangs and improved multiline quoted scalars, flow mappings, document-marker handling, and trailing-content cases materially, but the main remaining gap clusters are still invalid documents that are accepted, remaining tab edge cases, some tag/property composition cases, a small trailing-content cluster, and a small number of remaining flow cases.
- **Specialized scope hierarchy** — `TGocciaGlobalScope` (root), `TGocciaCallScope` (function calls), `TGocciaMethodCallScope` (class method calls with `SuperClass`/`OwningClass`), `TGocciaClassInitScope` (instance property initialization), and `TGocciaCatchScope` (catch parameter scoping). Each specialized scope overrides virtual methods (`GetThisValue`, `GetOwningClass`, `GetSuperClass`) to participate in VMT-based chain-walking.
- **VMT-based chain-walking** — `FindThisValue`, `FindOwningClass`, and `FindSuperClass` walk the parent chain calling the corresponding virtual `Get*` method on each scope, stopping at the first non-`nil` result. This eliminates `is` type checks in the evaluator and centralizes resolution logic in the scope hierarchy.
- **Unified identifier resolution** — `ResolveIdentifier(Name)` on `TGocciaScope` handles `this` (via `FindThisValue`) and keyword constants (via `Goccia.Keywords.Reserved`) before falling back to the standard scope chain walk, avoiding scattered special-case checks in the evaluator.

### Error Handling Strategy

GocciaScript uses a layered error approach (see [Errors](errors.md) for the full error type reference and user-facing display format):

1. **Compile-time errors** (lexer/parser) use Pascal exceptions (`TGocciaLexerError`, `TGocciaSyntaxError`) — these terminate parsing immediately.
2. **Runtime errors** use a callback pattern (`OnError` in `TGocciaEvaluationContext`) — this keeps evaluator functions pure.
3. **JavaScript-level errors** use `TGocciaThrowValue` for `throw` statements and `try/catch` — these flow through the evaluator's return path.
4. **`try-finally` without `catch`** — The evaluator wraps the Pascal `try...except` in a Pascal `try...finally` to guarantee the JS `finally` block runs before exceptions propagate, even when no `catch` clause exists.
5. **`break` and `return`** — Use `TGocciaControlFlow` result records (`cfkBreak`, `cfkReturn`) instead of Pascal exceptions. Statement-level evaluator functions return `TGocciaControlFlow`, and callers check `Result.Kind` to propagate signals. This eliminates `FPC_SETJMP` overhead from the interpreter's hot path (function calls, loop iterations, switch statements). `EvaluateSwitch` checks `CF.Kind = cfkBreak` after each case statement to implement JavaScript's fall-through-until-break semantics.

**Centralized error construction** — `Goccia.Values.ErrorHelper.pas` provides `ThrowTypeError`, `ThrowRangeError`, `ThrowReferenceError`, and `CreateErrorObject` helpers. All error throw sites across the codebase use these helpers instead of manually building error objects, reducing duplication and ensuring consistent error formatting.

**Why not exceptions everywhere?** Pascal exceptions disrupt the pure-function model of the evaluator. The callback pattern allows the evaluator to signal errors without unwinding the call stack, making control flow explicit. `TGocciaThrowValue` is the only exception used for non-local exits — it propagates naturally through the call stack to `EvaluateTry` (JS `try...catch`) or the top-level handler. `return` and `break` use lightweight `TGocciaControlFlow` records instead of exceptions, avoiding `setjmp`/`longjmp` overhead on every function call and loop iteration.

### Synchronous Microtask Queue

GocciaScript implements ECMAScript Promises with a synchronous microtask queue (`Goccia.MicrotaskQueue.pas`) that drains after each top-level script execution.

**The problem:** Promise `.then()` callbacks must be deferred (never synchronous), but GocciaScript is a synchronous engine with no event loop.

**The solution:** A singleton FIFO queue. When a Promise settles or `.then()` is called on an already-settled Promise, the reaction is enqueued rather than executed immediately. The engine drains the queue after `Interpreter.Execute` completes.

**Why drain after script execution (not during)?**

In the ECMAScript specification, the entire script is one macrotask. Microtasks drain after the current macrotask completes, not interleaved with synchronous code. This means:

1. All synchronous code runs to completion first.
2. All `.then()` callbacks fire in FIFO order.
3. New microtasks enqueued during draining (e.g., chained `.then()` handlers) are processed in the same drain cycle.

This follows the ECMAScript specification's microtask ordering semantics. Thenable adoption (resolving a Promise with another Promise) is deferred by one microtask tick, matching the spec's PromiseResolveThenableJob. When `Resolve(innerPromise)` is called, instead of synchronously calling `SubscribeTo`, a `prtThenableResolve` microtask is enqueued. When this microtask drains, it calls `SubscribeTo` to adopt the inner Promise's state — resulting in a 2-tick deferral (one for the thenable resolve job, one for the settlement reaction). This ensures correct ordering relative to other microtasks. The only scenario where timing would differ from a full engine is with multiple macrotask sources (`setTimeout`, I/O callbacks, event handlers), which GocciaScript does not implement. If these are added in the future, they would require an event loop that repeatedly: (1) dequeues one macrotask, (2) drains the microtask queue, (3) repeats.

**Integration points:**

| Context | When microtasks drain |
|---------|----------------------|
| `TGocciaEngine.Execute` | After `Interpreter.Execute` completes |
| `TGocciaEngine.ExecuteProgram` | After interpreter execution |
| Test framework | After each test callback |
| Benchmark runner | After warmup, calibration batches, and each measurement round |

**`queueMicrotask`:** The global `queueMicrotask(callback)` function enqueues a user-provided callback into the same microtask queue used by Promise reactions. This matches the [HTML spec](https://html.spec.whatwg.org/multipage/timers-and-user-prompts.html#microtask-queuing). If a `queueMicrotask` callback throws, the error is silently discarded and the queue keeps draining — remaining microtasks and Promise reactions still run. This matches the observable behavior in Node.js/browsers where uncaught microtask errors don't prevent other microtasks from executing.

**Error safety:** Both `Execute` and `ExecuteProgram` wrap the drain in a `try..finally` that calls `ClearQueue`. If the interpreter throws, stale microtasks are discarded rather than leaking into subsequent executions. After a successful `DrainQueue` the queue is already empty, so `ClearQueue` is a no-op.

**GC safety:** During `DrainQueue`, each microtask's handler, value, and result promise are temp-rooted to prevent collection mid-callback.

## Related documents

- [Architecture](architecture.md) — Shared frontend, both backends, main layers, design direction
- [Bytecode VM](bytecode-vm.md) — Compiler output, opcodes, `TGocciaVM`
- [Core patterns](core-patterns.md) — Recurring implementation patterns and internal terminology
- [Value system](value-system.md) — `TGocciaValue` hierarchy
- [Contributing](../CONTRIBUTING.md) — Workflow and code style
