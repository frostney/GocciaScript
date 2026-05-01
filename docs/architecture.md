# Architecture

*How source flows through the engine: shared frontend, two backends, and the main Pascal layers.*

## Executive Summary

- **Engine/runtime split** — `TGocciaEngine` orchestrates parsing, execution, core language built-ins, and source-list/string execution; `Goccia.Runtime` attaches runtime-provided built-ins, host globals, file helpers, and extensions
- **Execution mode abstraction** — `TGocciaExecutor` is the abstract class; `TGocciaInterpreterExecutor` and `TGocciaBytecodeExecutor` implement it independently
- **Shared frontend** — Lexer, Parser, and AST are shared between execution modes
- **Shared execution substrate** — Both modes share the same value types, core scope model, runtime extension mechanism, and mark-and-sweep GC
- **Goccia-specific** — The bytecode VM operates directly on `TGocciaValue`, not a generic VM abstraction
- **No cross-dependency** — The bytecode executor has no dependency on the interpreter or evaluator units

## Overview

GocciaScript has two execution modes — interpreter (tree-walk over the AST) and bytecode (`TGocciaVM`). A single `TGocciaEngine` class orchestrates both, delegating execution to a pluggable `TGocciaExecutor`. Both modes share the same frontend, value system, core language built-ins, and garbage collector. Host/runtime globals are attached through runtime extensions; see [Main Layers](#main-layers) for the `Goccia.Engine` / `Goccia.Runtime` split. See [Bytecode VM](bytecode-vm.md) for the bytecode backend's architecture.

## Pipelines

### Interpreter

```text
Source -> JSX Transformer (optional) -> Lexer -> Parser -> Interpreter -> Evaluator -> TGocciaValue
```

### Bytecode

```text
Source -> JSX Transformer (optional) -> Lexer -> Parser -> Compiler -> Goccia Bytecode -> TGocciaVM -> TGocciaValue
```

## Main Layers

| Layer | Units | Responsibility |
|-------|-------|----------------|
| Engine | `Goccia.Engine` | Core language globals, language configuration, source-string/source-list execution, executor dispatch |
| Runtime | `Goccia.Runtime` | Optional host/runtime globals such as console, fetch, JSON5, TOML, YAML, CSV/TSV, text assets, SemVer, test assertions, benchmarks, FFI, and file-backed helpers |
| Executor abstraction | `Goccia.Executor` | Abstract `TGocciaExecutor` base class |
| Interpreter executor | `Goccia.Engine` (`TGocciaInterpreterExecutor`) | Tree-walk execution via `TGocciaInterpreter` |
| Bytecode executor | `Goccia.Engine.Backend` (`TGocciaBytecodeExecutor`) | Bytecode compile + VM execution; no interpreter dependency |
| JSX | `Goccia.JSX.Transformer` | Optional pre-pass converting JSX to `createElement` calls |
| Frontend | `Goccia.Lexer`, `Goccia.Parser`, `Goccia.AST.*` | Source to AST |
| Interpreter | `Goccia.Interpreter`, `Goccia.Evaluator.*` | Tree-walk execution |
| Bytecode compiler | `Goccia.Compiler*` | AST to bytecode templates/modules |
| Bytecode format | `Goccia.Bytecode*` | Opcodes, templates, modules, binary I/O, debug info |
| Bytecode VM | `Goccia.VM*` | Register execution, closures, upvalues, handlers |
| Shared value system | `Goccia.Values.*`, `Goccia.Scope` | Objects, classes, arrays, promises, scopes, and shared value behavior |
| Realm | `Goccia.Realm` | Per-engine container for mutable intrinsic prototypes |
| GC | `Goccia.GarbageCollector` | Mark-and-sweep garbage collection |

For **tree-walk execution**, see [Interpreter](interpreter.md); for **bytecode execution**, see [Bytecode VM](bytecode-vm.md). For **recurring implementation patterns** and **terminology** (Define vs Assign, bindings, …), see [Core patterns](core-patterns.md).

## Design Direction

- Bytecode execution is Goccia-specific, not a generic VM layer.
- The VM register file uses tagged `TGocciaRegister` values internally; hot scalar kinds stay unboxed until they cross an object/runtime boundary.
- Arrays, objects, classes, promises, and functions are shared between interpreter and bytecode mode.
- Sparse arrays use a dedicated hole sentinel.
- Precompiled bytecode uses the `.gbc` format.

## CLI Library

The CLI tools share a two-level application class hierarchy and a declarative option parsing system.

**Application classes:**

- `TGocciaApplication` (`Goccia.Application.pas`) — embeddable base for any GocciaScript host. Manages GC lifecycle (`Initialize`/`Shutdown`) and unified error handling (`HandleError` virtual). No CLI dependency.
- `TGocciaCLIApplication` (`Goccia.CLI.Application.pas`) — extends `TGocciaApplication` with CLI concerns: argument parsing, help generation, option registration, and coverage/profiler singleton lifecycle. Tools override `Configure` (register options) and `ExecuteWithPaths` (business logic).

**Option class hierarchy** (`CLI.Options.pas`):

- `TGocciaOptionBase` → `TGocciaFlagOption`, `TGocciaStringOption`, `TGocciaIntegerOption`, `TGocciaRepeatableOption`, `TGocciaEnumOption<T>`
- The parser calls `Option.Apply(Value)` via virtual dispatch — no pointer arithmetic
- `TGocciaEnumOption<T>` uses RTTI (`GetEnumName` + prefix stripping) to auto-discover valid values
- Predefined option groups (`TGocciaEngineOptions`, `TGocciaCoverageOptions`, `TGocciaProfilerOptions`) bundle related options with owning lifecycle

**CLI lifecycle** (`TGocciaCLIApplication.Execute`):

1. `Configure` — register option groups and tool-specific flags
2. `ParseCommandLine` — parse `ParamStr` via virtual `Apply` dispatch
3. `Validate` — post-parse semantic checks (e.g., conflicting flags)
4. `InitializeSingletons` — coverage tracker, profiler
5. `ExecuteWithPaths` — tool business logic
6. `AfterExecute` — reporting hooks
7. `ShutdownSingletons` — cleanup in reverse order

**Tool mapping:**

| Tool | Base Class | Overrides |
|------|-----------|-----------|
| GocciaREPL | `TGocciaCLIApplication` | `Configure`, `ConfigureCreatedEngine`, `ExecuteWithPaths` |
| GocciaScriptLoader | `TGocciaCLIApplication` | `Configure`, `ConfigureCreatedEngine`, `Validate`, `ExecuteWithPaths`, `HandleError`, `AfterExecute` |
| GocciaTestRunner | `TGocciaCLIApplication` | `Configure`, `ConfigureCreatedEngine`, `ExecuteWithPaths` |
| GocciaBenchmarkRunner | `TGocciaCLIApplication` | `Configure`, `ConfigureCreatedEngine`, `ExecuteWithPaths` |
| GocciaBundler | `TGocciaCLIApplication` | `Configure`, `Validate`, `ExecuteWithPaths` |

## Executor Architecture

The engine uses a **strategy pattern** for execution. `TGocciaExecutor` is the abstract base; two concrete implementations exist:

```text
TGocciaExecutor (abstract — Goccia.Executor.pas)
├── TGocciaInterpreterExecutor (Goccia.Engine.pas)
│     Wraps TGocciaInterpreter for tree-walk execution
└── TGocciaBytecodeExecutor (Goccia.Engine.Backend.pas)
      Compiles to bytecode and runs on TGocciaVM
      No dependency on Goccia.Interpreter or Goccia.Evaluator
```

The engine always creates a `TGocciaInterpreter` for bootstrapping (global scope creation, built-in registration, shim loading). The executor receives the bootstrapped global scope and module loader via `Initialize`, then handles all program and module body execution independently.

Callers pass an executor to the engine constructor. When none is provided, the engine defaults to `TGocciaInterpreterExecutor`.

## Realm Isolation

`TGocciaRealm` (`Goccia.Realm.pas`) is the per-engine container for mutable intrinsic state — every built-in prototype object whose properties JS code can rewrite (`Array.prototype`, `Object.prototype`, `Map.prototype`, every error prototype, every Temporal prototype, and so on). Each `TGocciaEngine` constructs its own realm and frees it in its destructor; tear-down unpins every prototype the realm owns, so the next engine on the same worker thread starts from pristine intrinsics.

```text
TGocciaEngine
  └── owns TGocciaRealm
        ├── Slot[Array.prototype]    -> TGCManagedObject (pinned, unpinned at tear-down)
        ├── Slot[Object.prototype]   -> TGCManagedObject
        ├── ...
        ├── OwnedSlot[Map.shared]    -> TGocciaSharedPrototype (Free-d at tear-down)
        └── OwnedSlot[Set.shared]    -> TGocciaSharedPrototype
```

The realm exposes two slot kinds:

- **`TGocciaRealmSlotId`** — for `TGCManagedObject` prototypes. `SetSlot` pins the object via the GC; tear-down unpins everything ever stored.
- **`TGocciaRealmOwnedSlotId`** — for plain-`TObject` helpers like `TGocciaSharedPrototype`. The realm calls `Free` on the stored object at tear-down, before the pinned-slot release pass, so destructors that need to unpin owned GC objects still see a working GC.

Value units register a slot id at unit `initialization` time via `RegisterRealmSlot` / `RegisterRealmOwnedSlot` (process-wide monotonic counters), and read/write through `CurrentRealm.GetSlot(SlotId)` / `.SetSlot(SlotId, Value)` at runtime. `CurrentRealm` is a `threadvar` — each worker thread sees the realm of whichever engine that thread last constructed.

This replaces a previous `threadvar`-cache approach where intrinsic prototypes survived engine destruction and contaminated subsequent engines on the same thread, and a JS-level harness (`prototypeIsolation.js`) that tried to undo mutations from script (and could not reverse non-configurable property additions). See [Decision Log](decision-log.md) for the rationale, [Core patterns § Realm Ownership & Slot Registration](core-patterns.md#realm-ownership--slot-registration) for the registration recipe, and [Embedding § Engine Lifecycle & Realm Isolation](embedding.md#engine-lifecycle--realm-isolation) for embedder-facing implications.

## Duplication Boundaries (beneficial vs harmful)

The interpreter and bytecode executors are **intentionally separate control-flow mechanisms** (tree-walk vs register VM). Sharing the **same** `TGocciaValue` model and virtual property access is the architectural consolidation point; you should not try to merge those executors into one execution path.

- **Beneficial separation** — Different layers solving different problems: the lexer/parser/AST frontend vs `Goccia.Evaluator.*` vs `Goccia.Compiler.*` vs `Goccia.VM.*`; standalone format parsers (`Goccia.JSON`, `Goccia.TOML`, …) vs thin `Goccia.Builtins.*` adapters. Duplication *across* those boundaries is often *different representations of the same spec* (AST vs opcodes vs byte streams), not copy-paste to delete blindly.

- **Harmful duplication** — The **same rule** maintained twice without a seam: e.g. identical helper functions in two compiler units, or compile-time type compatibility (`TypesAreCompatible`) drifting from runtime `OP_CHECK_TYPE` behavior. That class of duplication should be centralized (shared helpers, single runtime check implementation) so policy stays consistent.

When in doubt: preserve pipeline separation; consolidate **policy and mechanical helpers**.

## Related Documents

- [Interpreter](interpreter.md) — Tree-walk pipeline and evaluator model
- [Bytecode VM](bytecode-vm.md) — Compiler, opcodes, register VM
- [Core patterns](core-patterns.md) — Recurring implementation patterns, internal terminology
- [Build System](build-system.md)
- [Decision Log](decision-log.md)
- [Contributing](../CONTRIBUTING.md) — Single contribution standard (workflow, mandatory rules, testing, Pascal style, quick reference)
