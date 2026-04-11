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

- The VM register file uses tagged `TGocciaRegister` values internally; hot scalar kinds stay unboxed until they cross an object/runtime boundary.
- The VM uses the same value classes as the interpreter: arrays, objects, classes, promises, functions, symbols, enums, and built-ins.
- `undefined`, `null`, booleans, and hole values use shared singleton objects.
- Sparse arrays use `TGocciaHoleValue.HoleValue`, not raw `nil`.
- The VM is integrated with the shared garbage collector and shared call stack.
- Bytecode mode enables strict type enforcement through compiler-emitted checks and typed opcodes.

## Opcode Layout

The opcode space is split into two ranges:

- `0..127`: core VM instructions
- `128..255`: semantic / non-core space

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
- semantic-only imports/exports, import.meta, and await

Some opcode families intentionally use flags or mode operands instead of one opcode per syntax form. For example:

- accessor definition uses constant-key and dynamic-key instructions plus getter/setter and static/instance flags
- collection helpers use a shared opcode for object spread, object rest, and iterable-to-array spread
- validation uses a shared opcode for require-object and require-iterable checks

Current opcode design rules:

- add explicit Goccia opcodes for stable language/runtime behaviour
- do not introduce old generic VM naming into new instructions
- prefer direct bytecode operations over extension multiplexers when semantics are first-class in GocciaScript

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

## Contributor Notes

- Do not add new bytecode/runtime concepts under old generic naming.
- Prefer `TGoccia*` bytecode and VM types in new code.
- Keep interpreter and bytecode semantics aligned through shared runtime objects, not conversion layers.
