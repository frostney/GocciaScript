# Architecture

*How source flows through the engine: shared source pipeline, two execution modes, and the main Pascal layers.*

## Executive Summary

- **Engine/runtime split** — `TGocciaEngine` orchestrates parsing, execution, core language built-ins, and source text execution; `Goccia.Runtime` attaches runtime globals, file helpers, and extensions
- **Execution mode abstraction** — `TGocciaExecutor` is the abstract class; `TGocciaInterpreterExecutor` and `TGocciaBytecodeExecutor` implement it independently
- **Shared source pipeline** — Preprocessors, lexer, parser, warning data, source maps, and AST artifacts are shared between execution modes
- **Shared execution substrate** — Both execution modes share the same value types, core scope model, runtime extension mechanism, and mark-and-sweep GC
- **Goccia-specific** — The bytecode VM operates directly on `TGocciaValue`, not a generic VM abstraction
- **No cross-dependency** — The bytecode executor has no dependency on the interpreter or evaluator units

## Overview

GocciaScript has two execution modes: interpreter mode (tree-walk over the AST) and bytecode mode (`TGocciaVM`). A single `TGocciaEngine` class orchestrates both, delegating execution to a pluggable `TGocciaExecutor`. Both execution modes share the same source pipeline, value system, core language built-ins, and garbage collector. Runtime globals are attached through runtime extensions; see [Main Layers](#main-layers) for the `Goccia.Engine` / `Goccia.Runtime` split. See [Bytecode VM](bytecode-vm.md) for the bytecode executor's architecture.

The source pipeline public API is `TGocciaSourcePipeline` in `Goccia.SourcePipeline`. `Parse` accepts source text plus `TGocciaSourcePipelineOptions` (preprocessors, compatibility flag set, and source type) and returns an owning `TGocciaSourcePipelineResult` containing the AST, generated-source lines, source map, timings, and warnings as data. Narrow source-pipeline entry points cover module source, dynamic `Function` validation/wrapper parsing, and expression fragments so hosts do not construct or configure `TGocciaParser` directly. Embedders that want to run source should continue using `TGocciaEngine.Execute` or the `RunScript*` helpers; direct source-pipeline use is for hosts that need parse artifacts, such as bytecode compilation paths.

The parser owns ECMAScript lexical-goal choices during source-pipeline parsing. `TGocciaLexer.ScanNextToken` scans one token at a time with an explicit `TGocciaLexicalGoal` such as `InputElementRegExp` or `InputElementDiv`; parser lookahead requests the goal required by the syntactic context before ambiguous input is classified. This follows the ES2026 lexical grammar split between `InputElementDiv`, `InputElementRegExp`, `InputElementRegExpOrTemplateTail`, `InputElementTemplateTail`, and `InputElementHashbangOrRegExp`, and keeps lexical-goal selection in parser context rather than lexer heuristics. Template literals use the same model: the lexer emits `TemplateHead`, `TemplateMiddle`, and `TemplateTail` span tokens, while `${...}` expressions are parsed in the main parser stream before the parser requests the next template-tail span. The source pipeline, expression parsing, and dynamic `Function` parsing all use this parser-owned lexer path.

## Pipelines

### Interpreter

```text
Source -> Preprocessors (optional, e.g. JSX) -> Lexer -> Parser -> Interpreter -> Evaluator -> TGocciaValue
```

### Bytecode

```text
Source -> Preprocessors (optional, e.g. JSX) -> Lexer -> Parser -> Compiler -> Goccia Bytecode -> TGocciaVM -> TGocciaValue
```

## Main Layers

| Layer | Units | Responsibility |
|-------|-------|----------------|
| Engine | `Goccia.Engine` | Core language built-ins, language configuration, source text execution, executor dispatch |
| Runtime | `Goccia.Runtime`, `Goccia.RuntimeExtensions.*`, `Goccia.RuntimeProfiles.*` | Runtime integration layer, runtime extensions such as console/fetch/data modules/SemVer/testing/benchmarks/FFI, loader/test/benchmark profiles, and file-backed helpers |
| Executor abstraction | `Goccia.Executor` | Abstract `TGocciaExecutor` base class |
| Interpreter executor | `Goccia.Executor.Interpreter` (`TGocciaInterpreterExecutor`) | Tree-walk execution via `TGocciaInterpreter` |
| Bytecode executor | `Goccia.Executor.Bytecode` (`TGocciaBytecodeExecutor`) | Bytecode compile + VM execution; no interpreter dependency |
| Source pipeline | `Goccia.SourcePipeline`, `Goccia.JSX.Transformer`, `Goccia.Lexer`, `Goccia.Parser`, `Goccia.AST.*` | Source text to AST, including parser policy, preprocessor dispatch, source maps, warning data, and generated-source lines |
| Interpreter | `Goccia.Interpreter`, `Goccia.Evaluator.*` | Tree-walk execution |
| Bytecode compiler | `Goccia.Compiler*` | AST to bytecode templates/modules |
| Bytecode format | `Goccia.Bytecode*` | Opcodes, templates, modules, binary I/O, debug info |
| Bytecode VM | `Goccia.VM*` | Register execution, closures, upvalues, handlers |
| Shared value system | `Goccia.Values.*`, `Goccia.Scope` | Objects, classes, arrays, promises, scopes, and shared value behavior |
| Realm | `Goccia.Realm` | Per-engine container for mutable intrinsic prototypes |
| GC | `Goccia.GarbageCollector` | Mark-and-sweep garbage collection |

For **tree-walk execution**, see [Interpreter](interpreter.md); for **bytecode execution**, see [Bytecode VM](bytecode-vm.md). For **canonical terminology**, see [GocciaScript Context](../CONTEXT.md). For **recurring implementation patterns** and Define vs Assign implementation details, see [Core patterns](core-patterns.md).

Source type belongs to the `SourceType` property on `TGocciaEngine`, because script source and module source change language execution (`this`, import metadata, and top-level scope lifetime). File names ending in `.mjs` infer module source unless an explicit source type is provided. `TGocciaRuntimeCore` may be attached to an engine, but it does not decide the entry file's source type. File-backed convenience APIs and the default filesystem module content provider live in `Goccia.Runtime`; runtime globals are added by installing concrete `TGocciaRuntimeExtension` classes or by applying a profile such as `ApplyLoaderRuntimeProfile`. Engine APIs accept source text or caller-provided `TStringList` instances. CLI hosts may still read their entry file or stdin before constructing the engine, as `GocciaScriptLoaderBare` does, but that file read is outside the engine API and does not attach runtime globals.

## Design Direction

- Bytecode execution is Goccia-specific, not a generic VM layer.
- The VM register file uses tagged `TGocciaRegister` values internally; hot scalar kinds stay unboxed until they cross an object/runtime boundary.
- Arrays, objects, classes, promises, and functions are shared between interpreter mode and bytecode mode.
- Sparse arrays use a dedicated hole sentinel.
- Precompiled bytecode uses the `.gbc` format.

## CLI Library

The CLI tools share a two-level application class hierarchy and a declarative option parsing system.

**Application classes:**

- `TGocciaApplication` (`Goccia.Application.pas`) — embeddable base for any GocciaScript host. Manages GC lifecycle (`Initialize`/`Shutdown`) and unified error handling (`HandleError` virtual). No CLI dependency.
- `TGocciaCLIApplication` (`Goccia.CLI.Application.pas`) — extends `TGocciaApplication` with CLI concerns: argument parsing, help generation, option registration, and coverage/profiler singleton lifecycle. Tools override `Configure` (register options) and `ExecuteWithPaths` (business logic).

**Option class hierarchy**:

- `CLI.Options.pas` provides generic primitives: `TOptionBase` → `TFlagOption`, `TStringOption`, `TIntegerOption`, `TRepeatableOption`, `TEnumOption<T>`
- The parser calls `Option.Apply(Value)` via virtual dispatch — no pointer arithmetic
- `TEnumOption<T>` uses RTTI (`GetEnumName` + prefix stripping) to auto-discover valid values
- `Goccia.CLI.Options.pas` owns Goccia-specific groups (`TGocciaEngineOptions`, `TGocciaCoverageOptions`, `TGocciaProfilerOptions`) and the compatibility flag registry used by each CLI host

**CLI lifecycle** (`TGocciaCLIApplication.Execute`):

1. `Configure` — register option groups and tool-specific flags
2. `ParseCommandLine` — parse `ParamStr` via virtual `Apply` dispatch
3. `Validate` — post-parse semantic checks (e.g., conflicting flags)
4. `InitializeSingletons` — coverage tracker, profiler
5. `ExecuteWithPaths` — tool business logic
6. `AfterExecute` — reporting hooks
7. `ShutdownSingletons` — cleanup in reverse order

CLI bytecode paths that need parse artifacts use `TGocciaCLISourcePipelineResult` (`Goccia.CLI.SourcePipelineResult.pas`) around the shared `TGocciaSourcePipeline.Parse` result. The helper stays in `source/app/`: it applies CLI warning display, transfers AST/source-map/generated-line ownership for compilation and coverage, and leaves bytecode compilation to each caller. `Goccia.CLI.SourceMaps.pas` owns the shared CLI source-map file output policy used by the Script Loader and Bundler.

**Tool mapping:**

| Tool | Base Class | Overrides |
|------|-----------|-----------|
| GocciaREPL | `TGocciaCLIApplication` | `Configure`, `ConfigureCreatedEngine`, `ExecuteWithPaths` |
| GocciaScriptLoader | `TGocciaCLIApplication` | `Configure`, `ConfigureCreatedEngine`, `Validate`, `ExecuteWithPaths`, `HandleError`, `AfterExecute` |
| GocciaSandboxRunner | `TGocciaCLIApplication` | `Configure`, `Validate`, `ExecuteWithPaths` |
| GocciaTestRunner | `TGocciaCLIApplication` | `Configure`, `ConfigureCreatedEngine`, `ExecuteWithPaths` |
| GocciaBenchmarkRunner | `TGocciaCLIApplication` | `Configure`, `ConfigureCreatedEngine`, `ExecuteWithPaths` |
| GocciaBundler | `TGocciaCLIApplication` | `Configure`, `Validate`, `ExecuteWithPaths` |

`GocciaSandboxRunner` is a separate CLI host for virtual-filesystem execution. It seeds a `TSandboxVirtualFileSystem` from explicit import baselines before creating an engine, then installs `TGocciaSandboxRuntimeExtension` so source can import `"fs"` and `"goccia"` inside that sandbox. The sandbox runner still uses the same executor abstraction as the script loader: `--mode=interpreted` uses `TGocciaInterpreterExecutor`, while `--mode=bytecode` uses `TGocciaBytecodeExecutor`.

Nested sandbox execution is still hosted by `GocciaSandboxRunner`: `runScript` and shell `goccia` dispatch through `TGocciaSandboxContext.RunScriptCallback`. Shared-VFS execution remains the default, while `{ sandbox: true }` / `goccia --sandbox` creates a child `TGocciaSandboxContext` seeded from parent-VFS paths and runs it through the same interpreter or bytecode executor mode.

## Executor Architecture

The engine uses a **strategy pattern** for execution. `TGocciaExecutor` is the abstract base; two concrete implementations exist:

```text
TGocciaExecutor (abstract — Goccia.Executor.pas)
├── TGocciaInterpreterExecutor (Goccia.Executor.Interpreter.pas)
│     Wraps TGocciaInterpreter for tree-walk execution
└── TGocciaBytecodeExecutor (Goccia.Executor.Bytecode.pas)
      Compiles to bytecode and runs on TGocciaVM
      No dependency on Goccia.Interpreter or Goccia.Evaluator
```

The engine always creates a `TGocciaInterpreter` for bootstrapping (global scope creation, built-in registration, shim loading). The executor receives the bootstrapped global scope and module loader via `Initialize`, then handles all program and module body execution independently.

Callers must pass an explicit executor to the engine constructor — there is no implicit default. The engine never frees the executor; the caller (or a wrapping layer such as `TGocciaRuntime`) owns it and frees it after the engine.

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

Value units register a slot id at unit `initialization` time via `RegisterRealmSlot` / `RegisterRealmOwnedSlot` (process-wide monotonic counters), and read/write through `CurrentRealm.GetSlot(SlotId)` / `.SetSlot(SlotId, Value)` at runtime. `CurrentRealm` is maintained by `TGocciaExecutionContextStack` (`Goccia.ExecutionContext.pas`): interpreter and bytecode entry points push a `TGocciaExecutionContext` whose `Realm` is the active realm. The old thread-local pointer remains as a compatibility facade for value units, but execution contexts are the source of truth.

This replaces a previous `threadvar`-cache approach where intrinsic prototypes survived engine destruction and contaminated subsequent engines on the same thread, and a JS-level harness (`prototypeIsolation.js`) that tried to undo mutations from script (and could not reverse non-configurable property additions). See [ADR 0032](adr/0032-per-engine-realm-isolation.md) for the rationale, [Core patterns § Realm Ownership & Slot Registration](core-patterns.md#realm-ownership--slot-registration) for the registration recipe, and [Embedding § Engine Lifecycle & Realm Isolation](embedding.md#engine-lifecycle--realm-isolation) for embedder-facing implications.

## Duplication Boundaries (beneficial vs harmful)

The interpreter and bytecode executors are **intentionally separate control-flow mechanisms** (tree-walk vs register VM). Sharing the **same** `TGocciaValue` model and virtual property access is the architectural consolidation point; you should not try to merge those executors into one execution path.

- **Beneficial separation** — Different layers solving different problems: the lexer/parser/AST source pipeline vs `Goccia.Evaluator.*` vs `Goccia.Compiler.*` vs `Goccia.VM.*`; standalone format parsers (`Goccia.JSON`, `Goccia.TOML`, …) vs thin `Goccia.Builtins.*` adapters. Duplication *across* those boundaries is often *different representations of the same spec* (AST vs opcodes vs byte streams), not copy-paste to delete blindly.

- **Harmful duplication** — The **same rule** maintained twice without a seam: e.g. identical helper functions in two compiler units, or compile-time type compatibility (`TypesAreCompatible`) drifting from runtime `OP_CHECK_TYPE` behavior. That class of duplication should be centralized (shared helpers, single runtime check implementation) so policy stays consistent.

When in doubt: preserve pipeline separation; consolidate **policy and mechanical helpers**.

## Related Documents

- [Interpreter](interpreter.md) — Tree-walk pipeline and evaluator model
- [Bytecode VM](bytecode-vm.md) — Compiler, opcodes, register VM
- [Core patterns](core-patterns.md) — Recurring implementation patterns
- [GocciaScript Context](../CONTEXT.md) — Canonical project terminology
- [Build System](build-system.md)
- [Architecture Decision Records](adr/)
- [Contributing](../CONTRIBUTING.md) — Single contribution standard (workflow, mandatory rules, testing, Pascal style, quick reference)
