# Architecture

*How source flows through the engine: the source pipeline, bytecode compilation and the VM, and the main Pascal layers.*

## Executive Summary

- **Engine/runtime split** — `TGocciaEngine` orchestrates parsing, execution, core language built-ins, and source text execution; `Goccia.Runtime` attaches runtime globals, import-only runtime modules, file helpers, and extensions
- **One engine, one VM** — `TGocciaEngine` compiles every program, module body, shim and eval to bytecode and runs it on a `TGocciaVM` it owns; there is no executor to choose since the tree-walk interpreter was removed ([#875](https://github.com/frostney/GocciaScript/issues/875))
- **Source pipeline** — Preprocessors, lexer, parser, warning data, source maps, and AST artifacts feed the compiler and the tools that need parse artifacts
- **One execution substrate** — The VM, the built-ins and runtime extensions share one value model, one scope model for global and module bindings, and one mark-and-sweep GC
- **Goccia-specific** — The bytecode VM operates directly on `TGocciaValue`, not a generic VM abstraction
- **Compiler and VM** — The engine drives the compiler and VM directly (it shares the `Goccia.EvaluationContext` record type with the module loader). See [Bytecode VM § Overview](bytecode-vm.md#overview)

## Overview

GocciaScript executes source one way: the engine parses it, compiles the AST to Goccia bytecode, and runs the bytecode on a `TGocciaVM`. A single `TGocciaEngine` class orchestrates this and owns the VM. GocciaScript 0.15.0 removed the tree-walk interpreter and the `--mode` option that selected it ([ADR 0138](adr/0138-bytecode-is-the-only-way-gocciascript-executes.md)). Runtime globals and import-only runtime modules are attached through runtime extensions; see [Main Layers](#main-layers) for the `Goccia.Engine` / `Goccia.Runtime` split. See [Bytecode VM](bytecode-vm.md) for the compiler and VM.

The source pipeline public API is `TGocciaSourcePipeline` in `Goccia.SourcePipeline`. `Parse` accepts source text plus `TGocciaSourcePipelineOptions` (preprocessors, compatibility flag set, unsupported-feature warning policy, and source type) and returns an owning `TGocciaSourcePipelineResult` containing the AST, generated-source lines, source map, timings, and warnings as data. Narrow source-pipeline entry points cover module source, dynamic `Function` validation/wrapper parsing, and expression fragments so hosts do not construct or configure `TGocciaParser` directly. Embedders that want to run source should continue using `TGocciaEngine.Execute` or the `RunScript*` helpers; direct source-pipeline use is for hosts that need parse artifacts, such as bytecode compilation paths.

The parser owns ECMAScript lexical-goal choices during source-pipeline parsing. `TGocciaLexer.ScanNextToken` scans one token at a time with an explicit `TGocciaLexicalGoal` such as `InputElementRegExp` or `InputElementDiv`; parser lookahead requests the goal required by the syntactic context before ambiguous input is classified. This follows the ES2026 lexical grammar split between `InputElementDiv`, `InputElementRegExp`, `InputElementRegExpOrTemplateTail`, `InputElementTemplateTail`, and `InputElementHashbangOrRegExp`, and keeps lexical-goal selection in parser context rather than lexer heuristics. Template literals use the same model: the lexer emits `TemplateHead`, `TemplateMiddle`, and `TemplateTail` span tokens, while `${...}` expressions are parsed in the main parser stream before the parser requests the next template-tail span. The source pipeline, expression parsing, and dynamic `Function` parsing all use this parser-owned lexer path.

## Pipeline

```text
Source -> Preprocessors (optional, e.g. JSX) -> Lexer -> Parser -> Compiler -> Goccia Bytecode -> TGocciaVM -> TGocciaValue
```

## Main Layers

| Layer | Units | Responsibility |
|-------|-------|----------------|
| Engine | `Goccia.Engine`, `Goccia.HostEnvironment`, `Goccia.Capabilities` | Core language built-ins, language configuration, the engine's immutable capability set, host-controlled script time/randomness, source text execution, bytecode compilation and the engine's `TGocciaVM` |
| Runtime | `Goccia.Runtime`, `Goccia.RuntimeExtensions.*`, `Goccia.RuntimeProfiles.*` | Runtime integration layer, runtime extensions such as console/fetch/data modules/SemVer/testing/benchmarks/FFI, loader/test/benchmark profiles, and file-backed helpers |
| Source pipeline | `Goccia.SourcePipeline`, `Goccia.JSX.Transformer`, `Goccia.Lexer`, `Goccia.Parser`, `Goccia.AST.*` | Source text to AST, including parser policy, preprocessor dispatch, source maps, warning data, and generated-source lines |
| Bytecode compiler | `Goccia.Compiler*` | AST to bytecode templates/modules |
| Bytecode format | `Goccia.Bytecode*` | Opcodes, templates, modules, binary I/O, debug info |
| Bytecode VM | `Goccia.VM*` | Register execution, closures, upvalues, handlers |
| Shared value system | `Goccia.Values.*`, `Goccia.Scope`, `Goccia.Modules` | Objects, classes, arrays, promises, scopes, live module import/export bindings, and shared value behavior |
| Temporal semantics | `Goccia.Temporal.AbstractOperations`, `Goccia.Temporal.Options`, `Goccia.Temporal.Utils`, `Goccia.Temporal.TimeZone` | Shared specification operations for calendar coercion, options, rounding, RFC 9557 parsing, and time-zone offsets; Temporal built-ins and values delegate here instead of duplicating policy |
| Realm | `Goccia.Realm` | Per-engine container for mutable intrinsic prototypes |
| GC | `Goccia.GarbageCollector` | Mark-and-sweep garbage collection |

For **bytecode execution**, see [Bytecode VM](bytecode-vm.md). For **canonical terminology**, see [GocciaScript Context](../CONTEXT.md). For **recurring implementation patterns** and Define vs Assign implementation details, see [Core patterns](core-patterns.md).

The engine also owns what source may reach outside the process: a `TGocciaCapabilities` value fixed at construction (`None` when the host passes none). The runtime, module loader, resolver, fetch, and FFI all consult that one set, and nested contexts only narrow it; see [Permissions](permissions.md).

Source type belongs to the `SourceType` property on `TGocciaEngine`, because script source and module source change language execution (`this`, import metadata, and top-level scope lifetime). File names ending in `.mjs` or `.mts` infer module source unless an explicit source type is provided; otherwise the default `stDetect` lets the source pipeline settle the goal from a top-level `import` or `export` declaration while it parses ([ADR 0140](adr/0140-an-entry-with-module-syntax-runs-as-a-module.md)). `TGocciaRuntimeCore` may be attached to an engine, but it does not decide the entry file's source type. File-backed convenience APIs and the default filesystem module content provider live in `Goccia.Runtime`; runtime globals and import-only runtime modules are added by installing concrete `TGocciaRuntimeExtension` classes or by applying a profile such as `ApplyLoaderRuntimeProfile`. Engine APIs accept source text or caller-provided `TStringList` instances. CLI hosts may still read their entry file or stdin before constructing the engine, as `GocciaScriptLoaderBare` does, but that file read is outside the engine API and does not attach runtime globals or runtime modules.

## Design Direction

- Bytecode execution is Goccia-specific, not a generic VM layer.
- The VM register file uses tagged `TGocciaRegister` values internally; hot scalar kinds stay unboxed until they cross an object/runtime boundary.
- Arrays, objects, classes, promises, and functions are ordinary `TGocciaValue` objects that the VM, the built-ins and host code all use.
- Sparse arrays use a dedicated hole sentinel.
- Precompiled bytecode uses the `.gbc` format.

## CLI Library

The CLI tools share a two-level application class hierarchy and a declarative option parsing system.

**Application classes:**

- `TGocciaApplication` (`Goccia.Application.pas`) — embeddable base for any GocciaScript host. Provides `Run`/`RunApplication`, which turn a `TCLIUsageError` into exit code 2 (`EXIT_CODE_USAGE`) and any other exception into exit code 1 after unified error handling (`HandleError` virtual). It does not depend on the CLI application layer; it uses only those two symbols from the shared `CLI.Options` unit. It has no GC code: `TGocciaEngine`'s constructor initializes the garbage collector.
- `TGocciaCLIApplication` (`Goccia.CLI.Application.pas`) — extends `TGocciaApplication` with CLI concerns: argument parsing, help generation, option registration, and coverage/profiler singleton lifecycle. Tools override `Configure` (register options) and `ExecuteWithPaths` (business logic).

**Option class hierarchy**:

- `CLI.Options.pas` provides generic primitives: `TOptionBase` → `TFlagOption`, `TStringOption` (→ `TOptionalStringOption`), `TIntegerOption`, `TInt64Option` (→ `TByteSizeOption`, `TDurationOption`, `TCountOption` for unit-aware limits), `TScopeListOption` (`--allow-<cap>[=scope,...]` grant lists), `TRemovedOption` (a removed flag that stays parseable so its use fails with a message naming the replacement), `TRepeatableOption`, `TEnumOption<T>`
- The parser calls `Option.Apply(Value)` via virtual dispatch — no pointer arithmetic
- `TEnumOption<T>` uses RTTI (`GetEnumName` + prefix stripping) to auto-discover valid values
- `Goccia.CLI.Options.pas` owns Goccia-specific groups (`TGocciaEngineOptions`, which owns `TGocciaCapabilityOptions` for the `--allow-*`/`--deny-*` flags; `TGocciaCoverageOptions`; `TGocciaProfilerOptions`; `TGocciaSandboxOptions`) and the compatibility flag registry used by each CLI host

**CLI lifecycle** (`TGocciaCLIApplication.Execute`):

1. `Configure` — register option groups and tool-specific flags
2. `ParseCommandLine` — parse `ParamStr` via virtual `Apply` dispatch
3. `Validate` — post-parse semantic checks (e.g., conflicting flags)
4. `InitializeSingletons` — coverage tracker, profiler
5. `ExecuteWithPaths` — tool business logic
6. `AfterExecute` — reporting hooks
7. `ShutdownSingletons` — cleanup in reverse order

CLI bytecode paths that need parse artifacts use `TGocciaCLISourcePipelineResult` (`Goccia.CLI.SourcePipelineResult.pas`) around the shared `TGocciaSourcePipeline.Parse` result. The helper stays in `source/app/`: it applies CLI warning display, transfers AST/source-map/generated-line ownership for compilation and coverage, and leaves bytecode compilation to each caller. `Goccia.CLI.SourceMaps.pas` owns the shared CLI source-map file output policy used by the Runner and Bundler.

**Tool mapping:**

| Tool | Base Class | Overrides |
|------|-----------|-----------|
| GocciaREPL | `TGocciaCLIApplication` | `Configure`, `ConfigureCreatedEngine`, `ExecuteWithPaths` |
| GocciaRunner | `TGocciaCLIApplication` | `Configure`, `ConfigureCreatedEngine`, `Validate`, `ExecuteWithPaths`, `HandleError`, `AfterExecute` |
| GocciaTestRunner | `TGocciaCLIApplication` | `Configure`, `ConfigureCreatedEngine`, `Validate`, `ExecuteWithPaths` |
| GocciaBenchmarkRunner | `TGocciaCLIApplication` | `Configure`, `ConfigureCreatedEngine`, `Validate`, `ExecuteWithPaths`, `AfterExecute` |
| GocciaBundler | `TGocciaCLIApplication` | `Configure`, `Validate`, `ExecuteWithPaths` |

`GocciaRunner` has a host mode and a [sandbox mode](permissions.md#sandbox-mode). In sandbox mode it copies the host inputs into a `TSandboxVirtualFileSystem` and captures that as the seed baseline before creating an engine, then installs `TGocciaSandboxRuntimeExtension` so source can import `"fs"`, `"goccia"` and `"goccia:search"` inside that sandbox. Both modes run on the bytecode VM.

Nested execution runs inside the sandbox-mode host: `runScript` and shell `goccia` dispatch through `TGocciaSandboxContext.RunScriptCallback`. Shared-VFS execution remains the default, while `{ sandbox: true }` / `goccia --sandbox` creates a child `TGocciaSandboxContext` whose `copy` entries come from parent-VFS paths. Either way the nested run gets an engine of its own.

## Execution Architecture

The engine bootstraps the realm itself: it creates the global scope (`Engine.GlobalScope`), binds it to the module loader and registers the built-ins. It also creates the `TGocciaVM` it runs everything on and frees it in its destructor. It compiles each program, module body, `Function` constructor body and eval with `TGocciaCompiler` and runs the result on that VM, gives the module loader its callbacks for instantiating and evaluating module bodies, and loads the bootstrap shims when they are first used (`TGocciaEngine.ShimLoader`, see [Core Patterns § Shimmed Legacy Globals](core-patterns.md#shimmed-legacy-globals)).

Nothing besides the engine needs to be created or freed: settings that shape compilation are engine properties, namely `StrictTypes`, `Compatibility` and `GlobalBackedTopLevel`, which binds a program's top-level declarations in the global environment as a Script's are ([Embedding § Engine Constructor](embedding.md#engine-constructor)).

### Evaluation Context

The engine, the module loader and declaration instantiation pass host-side state to each other in a `TGocciaEvaluationContext` record (`Goccia.EvaluationContext`) rather than through instance variables or globals: the realm, the scope, the error callback, the module-loading callbacks, the current file path, the non-strict flags, the disposal tracker, and the module being evaluated. It is not the ECMA-262 execution context; that is `TGocciaExecutionContext` in `Goccia.ExecutionContext`. `CurrentFilePath` is what relative imports resolve against: the entry file for the main program and the resolved module path for each module.

### Scope Chain Design

Scopes (`TGocciaScope`, `Goccia.Scope`) hold the bindings that outlive one compiled function's registers: the realm's global scope (`TGocciaGlobalScope`, `Engine.GlobalScope`), one module scope per module, and the module environment each bootstrap shim runs in. A function's parameters and locals live in VM registers, and the variables its closures capture live in upvalue cells; see [Bytecode VM](bytecode-vm.md).

- **`CreateChild` factory method** — A child scope comes from `CreateChild(AScopeKind, ACustomLabel, ACapacity)`, which creates a `TGocciaScope` with this scope as parent and copies its `this` value. The `TGocciaScope` constructor links the parent and inherits its `OnError` callback, module callbacks, and strict-types, non-strict and `arguments`-object flags. The optional capacity pre-sizes the binding dictionary.
- **`OnError` on scopes** — Each scope carries a reference to the error callback, inherited from its parent. The engine seeds it on the global scope, so code reached through any scope can raise a structured host error without global state.
- **Temporal Dead Zone** — `let`/`const` bindings are registered before initialization, enforcing TDZ semantics (accessing before `=` throws `ReferenceError`).
- **Var bindings** — With `--compat-var`, a scope keeps `var` bindings in a binding map of their own, distinct from lexical bindings, and a script's top-level vars are `globalThis` properties ([Language § `var` Declarations](language.md#var-declarations)). A function's vars are registers ([Bytecode VM § Compatibility Scope Helpers](bytecode-vm.md#compatibility-scope-helpers)).
- **Module scope isolation** — Modules execute in `skModule` scopes (children of the global scope), preventing module-internal variables from leaking into the global scope.
- **Compatibility semantics** — The `arguments` object, `with`, non-strict `this`, assignment and `delete` compile to explicit VM state and opcodes; see [Bytecode VM § Compatibility Scope Helpers](bytecode-vm.md#compatibility-scope-helpers) and [Language](language.md) for the behavior.

### Modules and Data Formats

- **Module path resolution** — `TGocciaModuleResolver` handles alias expansion via its inherited `Resolve` method using import-map semantics (exact match for keys without `/`, prefix match for keys with `/`, longest matching key wins), resolves `./` and `../` paths relative to the importing file's directory, tries the engine's module import extensions (`.js`, `.jsx`, `.ts`, `.tsx`, `.mjs`, `.mts`, `.json`, `.txt`, `.md`) followed by the structured-data extensions any installed runtime extension contributes (`.json5`, `.jsonc`, `.jsonl`, `.toml`, `.yaml`, `.yml`, `.csv`, `.tsv`), then index files for extensionless imports, then expands to an absolute path. `TGocciaModuleResolver.LoadImportMap` resolves import-map values relative to the map file and `DiscoverProjectConfig` walks parent directories looking for `goccia.json`. CLI applications also discover a project-level `goccia.toml`, `goccia.json5`, or `goccia.json` (in that priority order) starting from the entry file's directory via `CLI.ConfigFile.DiscoverConfigFile`. Config keys mirror CLI option names (for example, `source-type`, `compat-asi`, `timeout`), and CLI options override config values. Config files support `"extends"` to inherit from a base config, enabling per-directory overrides (e.g. `tests/language/asi/goccia.json` enables ASI for that subtree). Absolute paths are used as cache keys to prevent loading the same file via different relative paths. Loader-owned virtual modules are checked before filesystem and import-map resolution; host modules registered with `Engine.RegisterHostModule` are checked before the resolver. Custom resolvers can be injected by subclassing `TGocciaModuleResolver` and overriding the `Resolve` method.
- **Live import bindings** — Named imports are initialized as immutable indirect bindings in the importing module scope. The first successful access retains the resolved target module and binding name, while every access still reads that binding's current value. This avoids repeating `ResolveExport` and namespace lookup without caching the value itself, so mutations, imported-name exports, re-exports, and transitive function captures remain live.
- **Circular dependency handling** — Modules are added to the cache (`FModules`) and register their local export bindings before requested modules are evaluated. If a circular import encounters a partially linked module, its import binding can refer to that module without reading the target early; an actual read still observes the target binding's temporal dead zone until its declaration is evaluated.
- **Namespace imports** — `import * as ns from "./module.js"` binds the module's reusable namespace exotic object. The object has a null prototype, sorted enumerable read-only export properties, and live `[[Get]]` behavior. It caches each successfully resolved export identity, not its value, so repeated reads avoid re-walking forwarding chains while JavaScript bindings stay live; structured-data module values remain stable by construction.
- **JSON module imports** — Files ending in `.json` are handled by `TGocciaModuleLoader.LoadJSONModule`, which parses the file via `TGocciaJSONParser` (`Goccia.JSON` unit) and exposes the parsed root as the `default` export and, for object roots, each own key as a named export (a root key named `default` takes that slot). JSON modules bypass the lexer, parser and compiler entirely, keeping the import path unified (`import { key } from "./file.json"`). Array roots are objects too, so they also export their indices and `length`; primitive roots export only `default`. JSON modules participate in the same caching and path resolution as JS modules.
- **Standalone JSON utilities** — `Goccia.JSON` provides `TGocciaJSONParser` and `TGocciaJSONStringifier` as dependency-free utility classes that convert between JSON text and `TGocciaValue` types. `Goccia.Builtins.JSON` (the `JSON.parse`/`JSON.stringify` built-in) delegates to these, keeping the built-in a thin adapter. This separation allows the module loader, globals injection and any other component to parse JSON without instantiating a built-in.
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
- **YAML compatibility target** — The project goal for YAML is full YAML 1.2 compatibility plus Bun-compatible user-facing parsing semantics where that does not conflict with GocciaScript's module model. The implementation is intentionally landing in increments rather than claiming full conformance before the parser reaches it. As a concrete snapshot, a parse-validity rerun for 0.15.0 (engine `70b83cb2`, 2026-10-22) against `yaml-test-suite` commit `6ad3d2c62885d82fc349026c136ef560838fdf3d` matched the expected parse/fail result for 336 of 402 cases (83.6%), with 38 false accepts, 28 false rejects, and no timeouts. The latest reruns removed the previously observed parser hangs and improved multiline quoted scalars, flow mappings, document-marker handling, and trailing-content cases materially, but the main remaining gap clusters are still invalid documents that are accepted, remaining tab edge cases, some tag/property composition cases, a small trailing-content cluster, and a small number of remaining flow cases.

### Error Handling Strategy

GocciaScript uses a layered error approach (see [Errors](errors.md) for the full error type reference and user-facing display format):

1. **Compile-time errors** (lexer/parser) use Pascal exceptions (`TGocciaSyntaxError` and its subclass `TGocciaLexerError`). These terminate parsing immediately.
2. **JavaScript-level errors** are thrown values. Native code, such as a built-in, raises a `TGocciaThrowValue`. A `throw` in compiled code, or a native throw the VM catches, unwinds to the innermost `try` handler that the current VM entry installed on its handler stack (`TGocciaBytecodeHandlerStack`). A throw with no such handler leaves that VM entry as `EGocciaBytecodeThrow` and reaches the native caller or the host.
3. **`break`, `continue` and `return`** compile to jumps and returns, not exceptions. When one leaves a `try` that has a `finally`, the compiler emits the `finally` block's code at that exit.
4. **Host errors** that are not JavaScript values, such as malformed `--globals` input, go through the engine's error callback (`OnError`, `TGocciaEngine.ThrowError`) and raise `TGocciaRuntimeError`.

**Centralized error construction** — `Goccia.Values.ErrorHelper.pas` provides `ThrowTypeError`, `ThrowRangeError`, `ThrowReferenceError`, and `CreateErrorObject` helpers. All error throw sites across the codebase use these helpers instead of manually building error objects, reducing duplication and ensuring consistent error formatting.

### Synchronous Microtask Queue

GocciaScript implements ECMAScript Promises with a synchronous microtask queue (`Goccia.MicrotaskQueue.pas`) that drains after each top-level script execution.

**The problem:** Promise `.then()` callbacks must be deferred (never synchronous), but GocciaScript is a synchronous engine with no event loop.

**The solution:** A singleton FIFO queue. When a Promise settles or `.then()` is called on an already-settled Promise, the reaction is enqueued rather than executed immediately. The engine drains the queue after the program finishes (`TGocciaEngine.ExecuteProgram` → `WaitForRuntimeIdle`).

**Nested engines:** The singleton is per thread, and engines nest on a thread: a sandbox `runScript` child executes inside its caller's statement, and so does another engine's `Execute` that an embedder calls from a native callback. An `Execute` that starts while a different engine's run (`Execute`, `ExecuteProgram`, or `RunModule`) is in progress on the thread therefore runs in a microtask scope of its own (`TGocciaMicrotaskQueue.EnterScope` / `LeaveScope`):

- Its drain runs only the jobs enqueued in that scope, so it returns before any of the enclosing engine's pending jobs run. They run afterwards, in the order they were enqueued, once the enclosing engine reaches its own drain.
- Its cleanup discards only the jobs of that scope. A nested `Execute` that throws leaves the enclosing engine's callbacks and `async` continuations queued.
- A Promise reaction belongs to the scope that registered it, the job that calls a thenable's `then` to the scope the Promise being resolved was created in, and a `FinalizationRegistry` cleanup job to the scope that created the registry. When a nested engine's drain settles an enclosing engine's Promise — a fetch that completed in the meantime — its reactions wait in the enclosing engine's scope instead of running inside the nested one. A job whose scope has already ended is enqueued into the scope that is current when it comes due.

This is the ECMAScript job rule ([ES2026 §9.5](https://tc39.es/ecma262/#sec-jobs)): a job runs only when its agent's execution context stack is empty. A ShadowRealm is a second realm of the same agent rather than a separate execution, so it shares its creator's scope. The outermost `Execute` keeps the thread's own scope, so jobs the host queued before it — by evaluating a globals module ahead of the entry script — drain with it, and an engine re-entered from its own native callback shares the scope it is already running in. Only `Execute` isolates: a nested `ExecuteProgram` or `RunModule` drains the scope it was called in. [ADR 0123](adr/0123-per-execution-microtask-scopes.md) records the decision.

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
| `TGocciaEngine.ExecuteProgram` | After the compiled program returns (`WaitForRuntimeIdle`) |
| `TGocciaEngine.RunModule` / `RunModuleInScope` | After a precompiled `TGocciaBytecodeModule` returns (`WaitForRuntimeIdle`) |
| Test framework | After each test callback |
| Benchmark runner | After warmup, calibration batches, and each measurement round |

For fetch-backed Promises, these integration points also pump fetch completions before treating a pending Promise as permanently unsettled. `WaitForRuntimeIdle` repeats its round — every runtime extension's `WaitForIdle`, then the microtask drain — until a round leaves no extension with pending work (`HasPendingWork`: a fetch in flight, a real-mode timeout the host drain would run), because a job can start that work after the extension's pass. The tail of a module whose first top-level `await` settles at once is such a job. The drain also waits out the engine's own `Atomics.waitAsync` timeouts, after any extension work a job started; a waiter without one settles only through `Atomics.notify`. `Execute` then fails the run if the entry module's evaluation promise is still pending: with the oldest unhandled rejection when there is one and the unhandled-rejection policy throws, otherwise with the `TypeError` a synchronous `await` reports for a promise nothing can settle.

Every public entry that compiles or runs script (`Execute`, `ExecuteProgram`, `CompileModule`, `RunModule`, `RunModuleInScope`, `CallAsRun`, `EvaluateGlobalEval`, `WaitForRuntimeIdle`, `InjectGlobalsFromModule` and `InjectModulesFromModule`) establishes the engine's floating-point mode with `EnterGocciaFloatingPointScope` and restores the caller's on the way out. NaN and infinity arithmetic then follows IEEE 754, as ECMAScript requires, and does not raise `EInvalidOp`, even when an embedder calls the entry outside a CLI run. The scopes nest, so an entry called from inside a run changes nothing. A helper that only defines or reads bindings (`RegisterGlobal`, `RefreshGlobalThis`) is not an entry: if a property it reads is an accessor the script defined, the accessor runs in the caller's mode.

**`queueMicrotask`:** The global `queueMicrotask(callback)` function enqueues a user-provided callback into the same microtask queue used by Promise reactions. This matches the [HTML spec](https://html.spec.whatwg.org/multipage/timers-and-user-prompts.html#microtask-queuing). If a `queueMicrotask` callback throws, the error is surfaced as an uncaught host callback error instead of being converted into a Promise rejection. Promise reaction handler errors still reject their result promises.

**Error safety:** `TGocciaEngine.Execute` wraps the execution path in a `try..finally` that calls `ClearQueue` — a nested run leaves its microtask scope instead, which discards only its own jobs — and discards pending fetch completions. If the run throws, stale microtasks and fetch callbacks are discarded rather than leaking into subsequent executions, and so are the promises the queue was tracking as [rejected with no handler](errors.md#unhandled-promise-rejections); outstanding fetch workers are detached so cleanup does not wait on network I/O that can no longer affect the script. Lower-level callers that bypass `Execute` and call `ExecuteProgram` or `RunModule` directly get the idle drain, and the same cleanup when the run fails: an outermost run that exits by exception clears the queue and discards its pending fetches and timers, so the next run on the thread does not execute its jobs. After a run that returns normally the microtask queue is empty. Timers scheduled under a fake clock are not part of the idle drain; they stay until the caller advances or clears them.

**GC safety:** During `DrainQueue`, each microtask's handler, value, and result promise are temp-rooted to prevent collection mid-callback. Queued microtasks and FinalizationRegistry cleanup jobs are also registered as queued GC roots until they run.

### Shared Semantic Helpers

ECMAScript abstract operations that more than one caller needs live in units of their own rather than in the VM. `ToPrimitive` (`Goccia.Values.ToPrimitive.pas`) is a standalone abstract operation (trying `valueOf` then `toString` on objects) used by the `+` operator and available to any module. Binary arithmetic lives in `Goccia.Arithmetic` (`EvaluateSubtraction`, `EvaluateMultiplication`, …), where each operator shares the `ToNumericOperands` / `ToNumberPair` coercion helpers; it computes `%` as a floating-point remainder (not integer modulo) with NaN/Infinity propagation, and implements the relational operators through the `IsLessThan` abstract operation with type coercion.

**IEEE-754 correctness:** Arithmetic operations handle special number values (`NaN`, `Infinity`, `-Infinity`, `-0`) via property accessors on `TGocciaNumberLiteralValue` (`IsNaN`, `IsInfinite`, `IsNegativeZero`). Division uses explicit `IsNegativeZero` checks to compute correct signed results (e.g., `1 / -Infinity` → `-0`, `-1 / 0` → `-Infinity`). Exponentiation delegates to `NumberExponentiation` (`Goccia.NumberExponentiation`), which follows ES2026 §6.1.6.1.3 Number::exponentiate; its finite case is a port of Arm optimized-routines' `pow` ([Built-ins — Math](built-ins.md#math-gocciabuiltinsmathpas)). The sort comparator (`CallCompareFunc`) maps a `NaN` comparison result to `0` and `±Infinity` to `±1` before the stable merge sort (`StableSortElements`) uses it. Negative zero detection (`IsNegativeZero`) calls `NumberBits.IsNegativeZero`, which compares the double's bit pattern with the sign-bit-only pattern, so it does not depend on byte order.

## Realm Isolation

`TGocciaRealm` (`Goccia.Realm.pas`) is the engine's ECMA-262 Realm Record. It owns mutable intrinsic state — every built-in prototype object whose properties JS code can rewrite (`Array.prototype`, `Object.prototype`, `Map.prototype`, every error prototype, every Temporal prototype, and so on) — plus the realm links for `[[AgentSignifier]]`, `[[Intrinsics]]`, `[[GlobalObject]]`, `[[GlobalEnv]]`, `[[TemplateMap]]`, `[[LoadedModules]]`, and `[[HostDefined]]`. Each `TGocciaEngine` constructs its initial host-defined realm and frees it in its destructor; tear-down unpins every prototype and cached template object the realm owns, so the next engine on the same worker thread starts from pristine intrinsics.

```text
TGocciaEngine
  └── owns TGocciaRealm
        ├── [[GlobalObject]] / [[GlobalEnv]]
        ├── [[TemplateMap]]          -> cached tagged-template objects
        ├── [[LoadedModules]]        -> bound module-loader state
        ├── Slot[Array.prototype]    -> TGCManagedObject (pinned, unpinned at tear-down)
        ├── Slot[Object.prototype]   -> TGCManagedObject
        ├── ...
        ├── OwnedSlot[Map.shared]    -> TGocciaSharedPrototype (Free-d at tear-down)
        └── OwnedSlot[Set.shared]    -> TGocciaSharedPrototype
```

The realm exposes two slot kinds:

- **`TGocciaRealmSlotId`** — for `TGCManagedObject` prototypes. `SetSlot` pins the object via the GC; tear-down unpins everything ever stored.
- **`TGocciaRealmOwnedSlotId`** — for plain-`TObject` helpers like `TGocciaSharedPrototype`. The realm calls `Free` on the stored object at tear-down, before the pinned-slot release pass, so destructors that need to unpin owned GC objects still see a working GC.

Value units register a slot id at unit `initialization` time via `RegisterRealmSlot` / `RegisterRealmOwnedSlot` (process-wide monotonic counters), and read/write through `CurrentRealm.GetSlot(SlotId)` / `.SetSlot(SlotId, Value)` at runtime. `CurrentRealm` is maintained by `TGocciaExecutionContextStack` (`Goccia.ExecutionContext.pas`): the engine's and the VM's entry points push a `TGocciaExecutionContext` whose `Realm` is the active realm. The old thread-local pointer remains as a compatibility facade for value units, but execution contexts are the source of truth.

This replaces a previous `threadvar`-cache approach where intrinsic prototypes survived engine destruction and contaminated subsequent engines on the same thread, and a JS-level harness (`prototypeIsolation.js`) that tried to undo mutations from script (and could not reverse non-configurable property additions). See [ADR 0032](adr/0032-per-engine-realm-isolation.md) for the rationale, [Core patterns § Realm Ownership & Slot Registration](core-patterns.md#realm-ownership--slot-registration) for the registration recipe, and [Embedding § Engine Lifecycle & Realm Isolation](embedding.md#engine-lifecycle--realm-isolation) for embedder-facing implications.

## Duplication Boundaries (beneficial vs harmful)

The compiler and the VM are separate layers with separate representations of the same specification: the compiler works on the AST, the VM on opcodes and registers. Sharing the **same** `TGocciaValue` model and virtual property access between the VM, the built-ins and host code is the architectural consolidation point.

- **Beneficial separation** — Different layers solving different problems: the lexer/parser/AST source pipeline vs `Goccia.Compiler.*` vs `Goccia.VM.*`; standalone format parsers (`Goccia.JSON`, `Goccia.TOML`, …) vs thin `Goccia.Builtins.*` adapters. Duplication *across* those boundaries is often *different representations of the same spec* (AST vs opcodes vs byte streams), not copy-paste to delete blindly.

- **Harmful duplication** — The **same rule** maintained twice without a seam: e.g. identical helper functions in two compiler units, or compile-time type compatibility (`TypesAreCompatible`) drifting from runtime `OP_CHECK_TYPE` behavior. That class of duplication should be centralized (shared helpers, single runtime check implementation) so policy stays consistent.

When in doubt: preserve pipeline separation; consolidate **policy and mechanical helpers**.

## Related Documents

- [Bytecode VM](bytecode-vm.md) — Compiler, opcodes, register VM
- [Core patterns](core-patterns.md) — Recurring implementation patterns
- [GocciaScript Context](../CONTEXT.md) — Canonical project terminology
- [Build System](build-system.md)
- [Architecture Decision Records](adr/)
- [Contributing](../CONTRIBUTING.md) — Single contribution standard (workflow, mandatory rules, testing, Pascal style, quick reference)
