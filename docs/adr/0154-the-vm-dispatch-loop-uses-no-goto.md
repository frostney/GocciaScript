# 0154 - The VM dispatch loop uses no goto

**Date:** 2026-10-16
**Area:** `bytecode VM`, `build`
**Related:** [ADR 0003](0003-opcode-layout-by-hotness.md), [ADR 0023](0023-bytecode-vm-profiling.md)

## Context

`TGocciaVM.ExecuteClosureRegistersInternal` runs every bytecode instruction.
Since [#1216](https://github.com/frostney/GocciaScript/pull/1216) it had two
loop heads, a production head without the coverage, opcode-profiling, stop-IP
and instruction-limit checks and an instrumented head with them, and jumped
between them, the shared opcode case in `Goccia.VM.DispatchCase.inc` and the
loop end with `goto`. The case itself left early with 23 `goto LDispatchNext`
and entered `OP_GET_PROP_CONST` from `OP_GET_LOCAL_PROP_CONST` through a label.

FPC's wasm32 code generator has no arbitrary jumps. It lowers a function that
uses `goto` into a state machine over the whole function body: every label
becomes a state, and the function re-dispatches on the state at each jump.
With a patched trunk compiler that dispatches the states through one
`br_table`, the `goto` version of Richards still executed 3.07 times the
instructions of a structured one under wasmtime. A wasm32 build therefore
needed a second, structured loop behind a define, which the wasm32 port
prototype carried as `GOCCIA_VM_STRUCTURED_DISPATCH`, and the targets would
have run different dispatch code.

## Decision

The dispatch loop is one `while` loop with no labels. Its body fetches and
decodes the next instruction through the production or the instrumented path,
chosen once per entry into the `try` block as before, then runs the opcode
case and the memory-pressure check. In the case, `Continue` replaces
`goto LDispatchNext` and skips the memory-pressure check, as the jump did.
`OP_GET_LOCAL_PROP_CONST` shares `OP_GET_PROP_CONST`'s case arm and loads its
local before the shared property read. Every target compiles this one loop;
there is no define for another.

## Consequences

- Native cost does not rise. With FPC 3.2.2 and the production build, callgrind
  counts 0.1% to 0.9% fewer instructions for each of 13 AWFY workloads and 4
  micro workloads than the `goto` version at 02e2dc1a did (AWFY geomean
  0.997x). An `OP_GET_PROP_CONST` now tests the opcode once before its
  property read.
- A wasm32 build compiles the same dispatch loop as a native one.
- `Continue` and `Break` in `Goccia.VM.DispatchCase.inc` act on the dispatch
  loop only outside an inner loop; inside a `for` or `while` in a case arm they
  act on that inner loop. A `Break` out of the dispatch loop does not end
  execution: the enclosing loop around the exception handlers enters it again
  at the same instruction. An opcode ends execution with `Exit` or by raising.
- `goto` stays out of the VM (see the contributor notes in
  [Bytecode VM](../bytecode-vm.md#contributor-notes)).
