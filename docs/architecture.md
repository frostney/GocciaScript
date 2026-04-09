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

## Related Documents

- [Bytecode VM](bytecode-vm.md)
- [Build System](build-system.md)
- [Decision Log](decision-log.md)
