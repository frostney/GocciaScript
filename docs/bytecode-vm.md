# Bytecode VM

*For contributors working on the bytecode backend.*

## Executive Summary

- **Two execution modes** — tree-walk interpreter (default) and bytecode VM (`--mode=bytecode`), sharing the same frontend, runtime objects, and GC
- **Goccia-owned VM** — executes directly on `TGocciaValue` with tagged `TGocciaRegister` values; not a generic VM layer
- **Opcode space** — core instructions (0-127) for hot paths, non-core generic ops (128-166), and semantic/helper instructions (167-255) for colder operations like imports/exports
- **Binary format** — `.gbc` files with little-endian encoding, `GBC\0` magic, and version constant

## Overview

GocciaScript has two execution modes:

- **Interpreter mode**: tree-walk execution over the AST
- **Bytecode mode**: AST compilation to Goccia bytecode, then execution on `TGocciaVM`

The bytecode backend is no longer a language-agnostic subsystem. It is a Goccia-owned VM that executes directly on `TGocciaValue` and shares the same runtime object model as the interpreter.

## Pipeline

```text
Source -> JSX Transformer (optional) -> Lexer -> Parser -> Compiler -> Goccia Bytecode -> TGocciaVM -> TGocciaValue
```

Public bytecode artifacts use the `.gbc` extension.

## Main Units

| Area | Units |
|------|-------|
| Opcode definitions | `Goccia.Bytecode.pas` |
| Function templates / constants | `Goccia.Bytecode.Chunk.pas` |
| Module format | `Goccia.Bytecode.Module.pas` |
| Binary I/O | `Goccia.Bytecode.Binary.pas` |
| Debug metadata | `Goccia.Bytecode.Debug.pas` |
| VM execution | `Goccia.VM.pas` |
| Frames / closures / upvalues | `Goccia.VM.CallFrame.pas`, `Goccia.VM.Closure.pas`, `Goccia.VM.Upvalue.pas` |
| Backend entry point | `Goccia.Engine.Backend.pas` |
| Opcode name lookup | `Goccia.Bytecode.OpCodeNames.pas` |
| Profiler | `Goccia.Profiler.pas`, `Goccia.Profiler.Report.pas` |

## Core Design

- The register file uses tagged values that keep scalars unboxed until they cross a runtime boundary (see [Design Direction](architecture.md#design-direction)).
- The VM uses the same value classes as the interpreter: arrays, objects, classes, promises, functions, symbols, enums, and built-ins.
- `undefined`, `null`, booleans, and hole values use shared singleton objects.
- Sparse arrays use `TGocciaHoleValue.HoleValue`, not raw `nil`.
- The VM is integrated with the shared garbage collector and shared call stack.
- Bytecode mode enables strict type enforcement through compiler-emitted checks and typed opcodes.

## Opcode Layout

The opcode space is split into three tiers:

- `0..127`: core VM instructions
- `128..166`: non-core generic arithmetic/bitwise operations
- `167..255`: semantic helper/orchestration operations

In the current VM:

- core instructions cover hot execution paths such as locals, arithmetic, comparisons, property/index access, calls, construction, iteration, and class/object setup
- semantic instructions already include generic arithmetic and bitwise operations in `128..140`
- module and async orchestration currently starts at `167` (`IMPORT`, `EXPORT`, `AWAIT`, `IMPORT_META`)

The current encoding helpers are defined in `Goccia.Bytecode.pas`:

- `EncodeABC`
- `EncodeABx`
- `EncodeAsBx`
- `EncodeAx`
- `DecodeOp`
- `DecodeA`
- `DecodeB`
- `DecodeC`
- `DecodeBx`
- `DecodesBx`
- `DecodeAx`

Current instruction families:

- load and move: constants, literals, locals, upvalues
- control flow: jumps, handlers, throw, return
- closures: function templates and captured upvalues
- typed arithmetic and comparison
- object and array operations
- class construction and member definition
- calls, construction, iteration, globals, and string coercion
- semantic-only imports/exports, dynamic import, import.meta, and await

Some opcode families intentionally use flags or mode operands instead of one opcode per syntax form. For example:

- accessor definition uses constant-key and dynamic-key instructions plus getter/setter and static/instance flags
- collection helpers use a shared opcode (`OP_COLLECTION_OP`) for object spread, object rest, and iterable-to-array spread
- validation uses a shared opcode for require-object and require-iterable checks

Current opcode design rules:

- add explicit Goccia opcodes for stable, hot, language-owned behaviour
- do not introduce generic VM naming into new instructions
- do not add an opcode for something already reachable through existing call dispatch — if a built-in (e.g. `Object.freeze`) already goes through `OP_CALL_METHOD`, emit that call sequence from the compiler rather than adding a new opcode or sub-mode
- prefer mode operands on shared opcodes over proliferating single-purpose opcodes for cold or infrequent operations

## Performance Direction

Recent VM cleanup and optimization work has focused on reducing per-instruction overhead without reintroducing old abstraction layers:

- cache and reuse shared primitive values directly in registers
- avoid eager allocation of closure cells for uncaptured locals
- pre-size argument collections for calls and construction
- use unchecked template access in the dispatch loop where bounds are already guaranteed
- keep fast register access limited to proven hot/simple paths; local-slot and complex property paths should only move to fast access when they stay correct and measurably improve throughput

The current optimization target is reducing bytecode-mode suite time further without diverging interpreter and bytecode semantics.

## Profiling

The `--profile` flag on ScriptLoader enables language-level profiling of the bytecode VM. See [profiling.md](profiling.md) for the full guide.

- `--profile=opcodes` — opcode frequency histogram, opcode pair frequency (superinstruction candidates), and scalar fast-path hit rate for generic arithmetic/comparison opcodes
- `--profile=functions` — per-function self-time, total-time, call count, and heap allocation count
- `--profile=all` — both
- `--profile-output=path.json` — JSON export

The profiler follows the same singleton-tracker pattern as coverage (`Goccia.Coverage.pas`). Zero overhead when disabled. Opcode counting adds ~1% overhead; function timing adds ~3%.

## Binary Format

- Magic: `GBC\0`
- Version constant: `GOCCIA_FORMAT_VERSION`
- Endianness: little-endian
- File extension: `.gbc`

## Current Status

- `--mode=bytecode` runs the Goccia VM directly.
- The full JavaScript suite passes in bytecode mode.
- The old generic VM/runtime bridge has been removed from the active build.

## Design Rationale

GocciaScript includes a bytecode execution backend built specifically for GocciaScript. The current VM is not a language-agnostic subsystem: it executes directly on `TGocciaValue`, shares the same runtime objects as the interpreter, and uses a Goccia-owned opcode surface.

### Why a Bytecode VM?

The tree-walk interpreter directly evaluates AST nodes via recursive function calls. This is simple and debuggable, but carries overhead from VMT dispatch on every AST node, deep call stacks for nested expressions, and no opportunity for instruction-level optimization. A bytecode VM trades compilation cost for faster execution: flat instruction dispatch, register-based operands, and a compact in-memory representation.

### Why Register-Based?

Stack-based VMs (like the JVM and WASM) are simpler to compile to and have smaller instruction encoding. Register-based VMs (like Lua 5, LuaJIT, and Dalvik) need fewer instructions per operation and avoid redundant stack manipulations. Register-based was chosen for execution performance.

### Why Three Tiers?

The solution is a split opcode space with three tiers:

- **Core range (0–127):** register, control-flow, closure, literal, and other hot/stable VM operations.
- **Non-core generic range (128–166):** generic arithmetic and bitwise operations that are still explicit bytecode but handle mixed or untyped operands.
- **Semantic helper range (167–255):** colder language-level orchestration operations such as imports/exports, dynamic import, `import.meta`, await, and resource disposal.

This split keeps the dispatch surface organized while still allowing the backend to be explicitly Goccia-specific.

### Why Shared Runtime Values?

The VM shares the `TGocciaValue` object model with the interpreter rather than maintaining a second value representation. Registers use `TGocciaRegister` — a tagged variant record (`Goccia.VM.Registers.pas`) that keeps booleans, integers, and floats unboxed as scalars. Values only cross into `TGocciaValue` when they leave the register file (e.g., property access, function calls, GC marking).

That choice removes:

- conversion layers between interpreter values and VM values
- duplicate object models for arrays, objects, classes, and promises
- bridge-only GC root management
- bytecode/runtime disagreement over `undefined`, `null`, and sparse array holes

The trade-off is that arithmetic fast paths are split between scalar register operations (typed opcodes like `OP_ADD_INT` / `OP_ADD_FLOAT`) and generic `TGocciaValue` fallbacks (like `OP_ADD`).

### Compiler-Side Desugaring

Language features are compiled into compact bytecode instruction sequences rather than expanding the opcode surface unnecessarily:

- **Nullish coalescing (`??`) and nullish coalescing assignment (`??=`)** — The compiler emits `OP_JUMP_IF_NOT_NULLISH` in its nullish-match mode, so `undefined`, `null`, and internal hole values all follow the same short-circuit path without extra comparison instructions.
- **Template literals** — The compiler parses interpolations at compile time, emits string constants and `OP_TO_STRING` for expression parts, then chains `OP_CONCAT` instructions.
- **Object spread** — The compiler emits dedicated Goccia bytecode rather than routing through a generic extension dispatcher.

This keeps the emitted bytecode compact and makes opcode additions deliberate instead of reactive.

### How Opcode Additions Work

New opcodes should be added only when an operation is both common enough and semantically stable enough to justify a dedicated instruction.

Prefer:

- explicit Goccia opcodes for core language/runtime behaviour
- compiler lowering to existing instructions for syntactic sugar
- flags or operands when an operation is a mode of an existing instruction rather than a new concept

### Tier 1 Property Flags vs Tier 2 Visibility

Property **mutability** (writable/configurable) is still a VM concern. Bulk operations like freeze and seal remain derived from the lower-level property-flag operations:

- `SetEntryFlags(key, flags)` — modify flags on a single property
- `PutWithFlags(key, value, flags)` — create a property with specific flags
- `PreventExtensions` — stop new properties from being added
- `Freeze` = iterate all entries, set flags to 0, prevent extensions (a convenience, not a primitive)

Property **visibility** and **accessor semantics** remain part of the higher-level object/class model rather than low-level property-flag storage.

### Spread Calling Consolidation

Spread-based calls use the flags byte on `OP_CALL` and `OP_CALL_METHOD`. Spread is treated as a mode of the call instruction rather than as a separate opcode family.

### Rejected Findings

During code review, the following findings were investigated and determined to be non-issues:

- **`SBIAS_24` (`Goccia.Bytecode.pas`)** — The 24-bit signed bias constant 8388607 is correct. The 24-bit unsigned range 0..16777215 centered at 8388607 gives a signed range of −8388607..+8388608. This is standard Lua-style bias encoding.
- **Token list leak in `Goccia.Compiler.Test.pas`** — `Lexer.ScanTokens` returns the lexer's own `FTokens` list (freed in the lexer's destructor). Adding manual `Tokens.Free` causes a double-free crash.

## Related documents

- [Architecture](architecture.md) — Shared frontend and both backends at a glance
- [Interpreter](interpreter.md) — Tree-walk backend (`Goccia.Interpreter`, `Goccia.Evaluator.*`)
- [Core patterns](core-patterns.md) — Recurring Pascal conventions and terminology

## Contributor Notes

- Do not add new bytecode/runtime concepts under old generic naming.
- Prefer `TGoccia*` bytecode and VM types in new code.
- Keep interpreter and bytecode semantics aligned through shared runtime objects, not conversion layers.
