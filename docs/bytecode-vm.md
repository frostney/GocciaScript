# Bytecode VM

*For contributors working on the bytecode backend.*

## Overview

GocciaScript has two execution modes:

- **Interpreter mode**: tree-walk execution over the AST
- **Bytecode mode**: AST compilation to Goccia bytecode, then execution on `TGocciaVM`

The bytecode backend is no longer a language-agnostic subsystem. It is a Goccia-owned VM that executes directly on `TGocciaValue` and shares the same runtime object model as the interpreter.

## Pipeline

```text
Source -> Lexer -> Parser -> Compiler -> Goccia Bytecode -> TGocciaVM -> TGocciaValue
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

## Core Design

- Registers hold `TGocciaValue` directly.
- The VM uses the same value classes as the interpreter: arrays, objects, classes, promises, functions, symbols, enums, and built-ins.
- `undefined`, `null`, booleans, and hole values use shared singleton objects.
- Sparse arrays use `TGocciaHoleValue.HoleValue`, not raw `nil`.
- The VM is integrated with the shared garbage collector and shared call stack.
- Bytecode mode enables strict type enforcement through compiler-emitted checks and typed opcodes.

## Opcode Layout

The opcode space is split into two ranges:

- `0..127`: core VM instructions
- `128..255`: semantic instructions

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
- semantic-only imports/exports and await

Some opcode families intentionally use flags or mode operands instead of one opcode per syntax form. For example:

- accessor definition uses constant-key and dynamic-key instructions plus getter/setter and static/instance flags
- collection helpers use a shared opcode for object spread, object rest, and iterable-to-array spread
- validation uses a shared opcode for require-object and require-iterable checks

Current opcode design rules:

- add explicit Goccia opcodes for stable language/runtime behaviour
- do not introduce old generic VM naming into new instructions
- prefer direct bytecode operations over extension multiplexers when semantics are first-class in GocciaScript

## Binary Format

- Magic: `GBC\0`
- Version constant: `GOCCIA_FORMAT_VERSION`
- Endianness: little-endian
- File extension: `.gbc`

## Current Status

- `--mode=bytecode` runs the Goccia VM directly.
- The full JavaScript suite passes in bytecode mode.
- The old generic VM/runtime bridge has been removed from the active build.
- WASM emission has been removed pre-1.0.

## Contributor Notes

- Do not add new bytecode/runtime concepts under old generic naming.
- Prefer `TGoccia*` bytecode and VM types in new code.
- Keep interpreter and bytecode semantics aligned through shared runtime objects, not conversion layers.
