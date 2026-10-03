# 0126 - A bytecode call clears only what a new frame could read stale, binds thread state per outermost entry, and identifies its callee by exact class

**Date:** 2026-10-02
**Area:** `bytecode runtime`

## Context

A call from one bytecode function to another cost about 1,950 machine
instructions around the callee's own code in AWFY Richards, 27% of the run.
Most of it was bookkeeping that repeated work whose answer could not have
changed since the previous call. Three parts of the fix change rules the VM
used to follow, and each would look like a mistake to a reader who did not
know why.

[bytecode-vm.md](../bytecode-vm.md#performance-direction) said that the
register, local-cell and argument window fills are critical for the collector
and "deliberately retained rather than trimmed". Every call read the thread's
call stack and execution-context stack through thread variables, four and
four times. `OP_CALL` and `OP_CALL_METHOD` found a bytecode callee only after
`is` tests for native and bound functions had walked its class chain and
failed.

## Decision

**Two of the three window fills are trimmed; the register window is still
filled in full.**

- *Local cells.* `FLocalCellStaleTop` is a mark in the local-cell arena:
  every slot at or above it is nil. Only two places store a cell,
  `GetLocalCell` when a closure first captures a local and
  `RestoreContinuation` when a generator resumes, and both raise the mark
  (`NoteLocalCells`). Taking a window for a new frame, or growing one, clears
  only the slots below the mark, and `ClearStaleLocalCells` is the only place
  that lowers it, to the start of the range it cleared, and only when that
  range reached the mark. A live window therefore holds exactly what it held
  when every window was cleared.
- *Arguments.* The argument window is not cleared. `SetupNewFrame` stores
  every slot of it straight after acquiring it, before anything that can
  allocate a collected object runs, and every reader of the window is bounded
  by `FArgCount`. The call instruction hands `SetupNewFrame` a pointer to its
  argument registers; they are copied before any register is acquired,
  because acquiring registers can move the register arena or, on a tail call,
  clear the slots the arguments sit in.
- *Registers.* A callee reads a register before it writes it, the collector
  marks the whole window, and every frame leaves its registers dirty, so
  there is no mark to keep and the fill stays.

**The VM binds the thread's call stack and execution-context stack when
native code enters it while it is running nothing, and on every such entry.**
`BindToCurrentThread` runs when `FNativeExecutionDepth` is zero. Every nested
entry happens inside the outermost one, on its thread, and so does every
frame pushed or popped until it returns, so calls inside it push and pop
through the cached objects without a thread-local lookup. Binding once per VM
would be wrong: between two outermost entries the VM can be entered from
another thread, and the thread's call stack can be destroyed and created
again. The interned source-path reference the VM reuses between calls is
dropped at the same point, because it belongs to the thread that interned it.

**`TGocciaBytecodeFunctionValue` is sealed, and the call opcodes identify a
bytecode callee by comparing its class exactly.** The comparison is one load;
it stands in for `is` only while nothing derives from the class. It lets a
bytecode callee skip the `is` tests for native and bound functions, and any
other callee skip the one for a bytecode function.

## Considered options

- **Clear a frame's cells when it is torn down instead of when the next one
  is set up.** Rejected: it moves the same fill to the other end of the call,
  and exception unwinding would have to do it for every frame it drops.
- **A mark for the register arena.** Rejected: sibling calls reuse the same
  slots and every frame writes its registers, so the mark would sit above
  each new window and nothing would be skipped.
- **Bind the thread state on every native entry, nested ones included.** It
  needs no depth test, but a callback from `Array.prototype.map` is a native
  entry and would pay two thread-local lookups for it.
- **Bind once per VM.** Rejected for the reason given above.
- **Keep `is` as a fallback behind the exact class comparison.** This was the
  first form. A native callee then paid for the comparison and for the
  failing `is` as well, 13 instructions more per built-in call than before.

## Consequences

- A new store of a non-nil local cell must raise the mark. Development builds
  assert that every new or grown local-cell window is clear, which catches a
  missed one at the next call. That assertion is a local development check:
  CI builds every binary with production flags, where assertions are off.
  What fails in CI is the JavaScript suite's closure and generator tests: with
  the assertions removed, never clearing a window and a missed mark in
  `GetLocalCell` both crash the bytecode run, a missed mark in
  `RestoreContinuation` fails 22 tests, and lowering the mark too far fails
  127.
- A new caller of `AcquireArgumentWindow` must store every slot before
  anything can collect. `SetupNewFrame` is the only caller.
- A new subclass of `TGocciaBytecodeFunctionValue` does not compile. Removing
  `sealed` to allow one silently sends its instances down the generic call
  path, so the exact comparisons in `OP_CALL` and `OP_CALL_METHOD` have to
  become `is` tests again at the same time.
- Code that replaces a thread's call stack while a VM on that thread is in
  the middle of an entry leaves that VM pushing frames to the old one until
  the entry returns. Nothing does this: `TGocciaCallStack.Shutdown` runs only
  when a thread's runtime shuts down.
- AWFY Richards runs in 21.1% fewer instructions, and `SetupNewFrame` costs
  382 instructions per call instead of 1,152. The three decisions recorded
  here account for about half of that; the rest is described in
  [bytecode-vm.md](../bytecode-vm.md#performance-direction).
