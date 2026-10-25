# 0159 - A frame whose function never reads its arguments as passed gets no argument window

**Date:** 2026-10-25
**Area:** `bytecode runtime`
**Related:** [ADR 0126](0126-bytecode-call-path-arena-fills-and-thread-binding.md), [ADR 0089](0089-defer-fixed-arity-call-specialization.md), [Bytecode VM § Performance Direction](../bytecode-vm.md#performance-direction)

## Context

`SetupNewFrame` stored every argument of a bytecode call twice: into the
callee's argument window on the argument arena, and then from there into the
callee's registers `1..n`. Before the second copy it cleared the whole register
window through `FillChar`, a call into the RTL, and then wrote `this` and the
arguments over the start of it again. In AWFY Richards almost every call is a
method call on a class instance, about 539,000 bytecode calls per iteration,
and the frame set-up was the largest single cost around them: about 370 machine
instructions per call, the `FillChar` call included, of about 640 between a
call instruction and the return to its caller.

Most functions never read the argument window. Their parameters are registers,
and the window is read only by the instructions that need the arguments as
passed: `OP_ARG_COUNT`, `OP_LOAD_ARGUMENT` (a parameter preamble with defaults
or patterns), `OP_PACK_ARGS` (a rest parameter), `OP_CREATE_ARGUMENTS` (an
`arguments` object), and the continuation of an async function or a generator,
which keeps it. A direct eval's code reads the caller's arguments only through
an `arguments` object the caller creates.

## Decision

**A function template records whether its frames read the argument window.**
`TGocciaFunctionTemplate.ArgumentWindowUnread` scans the template's code on the
first call and keeps the answer in a runtime-only field that is never
serialised and is reset by `EmitInstruction` and `PatchInstruction`. Every code
word is read as an instruction, so a word that is not one (an `OP_WIDE` prefix)
can only make the answer "read". A template with none of the instructions above
and no `OP_DIRECT_EVAL`, that is neither async nor a generator, is "unread".

**A frame of an unread template gets an empty argument window**, as long as no
arena has to grow for it: `FArgCount` is 0, so the collector, which marks the
argument arena up to the current window's end, marks none of it for that frame.
`SetupNewFrame` then stores `this` and the arguments straight into the new
register window and clears only the registers above them, before anything can
collect. When an arena would have to grow, or
the template reads its window, the frame is set up as before.

The direct copy relies on the register arena not moving between the read of an
argument and its store, which holds because nothing grows in between. The
arguments can sit in the same arena: in the caller's window, below the new one,
or, on a proper tail call, in the frame being replaced, whose window the new one
starts at. There each argument `i` is read from the old frame's register
`A + 1 + i` and stored into register `1 + i`, at or below it, so the arguments
are copied in ascending order and `this`, which no argument overlaps, first.

## Considered options

- **Clear only the registers above the arguments, but keep the argument
  window.** Half of the saving: each argument would still be stored twice.
- **Decide in the compiler and serialise the flag in `.gbc`.** It would make
  the bytecode format carry a fact the VM can find from the code itself, and a
  loaded template would have to be trusted.
- **Clear the registers above the arguments with inline stores instead of
  `FillChar`.** A `for` loop of two `Int64` stores per register ran fewer
  instructions, but made the `array-callbacks` probe (native `map` and
  `reduce` callbacks) about 3% slower than `FillChar` in interleaved A/Bs; a
  pointer-bounded `while` loop also cost Richards 3.9%.
- **Merge `TeardownCurrentFrame` and `PopFrame` into one return routine, or
  inline them into the dispatch loop.** Both executed more instructions on
  Richards (FPC 3.2.2 spills more in the larger routine), and were dropped.

## Consequences

- An instruction added later that reads `FArguments` or `FArgCount` in the
  frame that executes it must be added to `ScanArgumentWindowUse`; otherwise
  it reads an empty window in a function that has none of the others.
- A frame's `FArgCount` is no longer always its argument count. Nothing but
  the instructions above, `CreateArgumentsObjectFromCurrentFrame` and the
  continuations of async functions and generators read it.
- ADR 0126's argument-window rule still holds: `SetupNewFrame` stores every
  slot of any window it acquires before anything can collect, and an empty
  window has none.
