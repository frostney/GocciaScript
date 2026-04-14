# Architecture

*How source flows through the engine: shared frontend, two backends, and the main Pascal layers.*

## Executive Summary

- **Shared frontend** — Lexer, Parser, and AST are shared between interpreter and bytecode backends
- **Two backends** — tree-walk interpreter (default) and register-based bytecode VM (`--mode=bytecode`)
- **Unified runtime** — Both backends share the same value types, built-ins, scope chain, and mark-and-sweep GC
- **Goccia-specific** — The bytecode VM operates directly on `TGocciaValue`, not a generic VM abstraction

## Overview

GocciaScript has two execution backends — interpreter (tree-walk over the AST) and bytecode (`TGocciaVM`). Both share the same frontend, runtime objects, built-ins, and garbage collector. See [Bytecode VM](bytecode-vm.md) for the bytecode backend's architecture.

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
| Engine | `Goccia.Engine`, `Goccia.Engine.Backend` | Top-level orchestration, built-in registration, backend selection |
| JSX | `Goccia.JSX.Transformer` | Optional pre-pass converting JSX to `createElement` calls |
| Frontend | `Goccia.Lexer`, `Goccia.Parser`, `Goccia.AST.*` | Source to AST |
| Interpreter | `Goccia.Interpreter`, `Goccia.Evaluator.*` | Tree-walk execution |
| Bytecode compiler | `Goccia.Compiler*` | AST to bytecode templates/modules |
| Bytecode format | `Goccia.Bytecode*` | Opcodes, templates, modules, binary I/O, debug info |
| Bytecode VM | `Goccia.VM*` | Register execution, closures, upvalues, handlers |
| Shared runtime | `Goccia.Values.*`, `Goccia.Scope`, `Goccia.Runtime.Bootstrap` | Built-ins, objects, classes, arrays, promises, globals |
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

**Option class hierarchy** (`Goccia.CLI.Options.pas`):

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
| REPL | `TGocciaApplication` | `Execute` (interactive loop) |
| ScriptLoader | `TGocciaCLIApplication` | `Configure`, `Validate`, `ExecuteWithPaths`, `HandleError`, `AfterExecute` |
| TestRunner | `TGocciaCLIApplication` | `Configure`, `ExecuteWithPaths`, `GlobalBuiltins` |
| BenchmarkRunner | `TGocciaCLIApplication` | `Configure`, `ExecuteWithPaths`, `GlobalBuiltins` |

## Duplication Boundaries (beneficial vs harmful)

The interpreter and bytecode backend are **intentionally separate control-flow mechanisms** (tree-walk vs register VM). Sharing the **same** `TGocciaValue` model and virtual property access is the architectural consolidation point; you should not try to merge those backends into one execution path.

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
