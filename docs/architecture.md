# Architecture

*For contributors who need to understand how source code flows through the system.*

## Executive Summary

- **Shared frontend** — Lexer, Parser, and AST are shared between interpreter and bytecode backends
- **Two backends** — tree-walk interpreter (default) and register-based bytecode VM (`--mode=bytecode`)
- **Unified runtime** — Both backends share the same value types, built-ins, scope chain, and mark-and-sweep GC
- **Goccia-specific** — The bytecode VM operates directly on `TGocciaValue`, not a generic VM abstraction

## Overview

GocciaScript has two execution backends:

- **Interpreter mode**: tree-walk execution over the AST
- **Bytecode mode**: compilation to Goccia bytecode and execution on `TGocciaVM`

Both backends share the same frontend, runtime objects, built-ins, and garbage collector.

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
| Frontend | `Goccia.Lexer`, `Goccia.Parser`, `Goccia.AST.*` | Source to AST |
| Interpreter | `Goccia.Interpreter`, `Goccia.Evaluator.*` | Tree-walk execution |
| Bytecode compiler | `Goccia.Compiler*` | AST to bytecode templates/modules |
| Bytecode format | `Goccia.Bytecode*` | Opcodes, templates, modules, binary I/O, debug info |
| Bytecode VM | `Goccia.VM*` | Register execution, closures, upvalues, handlers |
| Shared runtime | `Goccia.Values.*`, `Goccia.Scope`, `Goccia.Runtime.Bootstrap` | Built-ins, objects, classes, arrays, promises, globals |

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

## Related Documents

- [Bytecode VM](bytecode-vm.md)
- [Build System](build-system.md)
- [Decision Log](decision-log.md)
