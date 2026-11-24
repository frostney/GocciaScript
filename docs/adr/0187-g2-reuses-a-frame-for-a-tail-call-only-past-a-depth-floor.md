# 0187 - G2 reuses a frame for a tail call only past a depth floor

**Date:** 2026-11-24
**Area:** `g2`, `compiler`, `jit`
**Related:** [ADR 0160](0160-concise-arrow-bodies-make-proper-tail-calls.md)

## Context

ECMAScript requires proper tail calls: a call in tail position (ES2026
§15.10.1 IsInTailPosition, §15.10.2 HasCallInTailPosition) runs after
PrepareForTailCall has released the caller's resources, so recursion through
such calls runs in bounded space. GocciaScript's old engine reuses the frame on
every tail call, and its tests recurse through tail calls far past any frame
bound. G2 had no tail calls: its 200,000-frame bound let the 100,000-deep
test262 cases pass, while deeper recursion, and any under `--max-stack`,
overflowed.

G2 runs a function in three ways: the interpreter (Execute, with FastLoop for
common instructions), the baseline JIT, and the optimizing tier, which inlines
callees. Compiled code calls a compiled callee on the native stack, one
activation per call, and inlined calls push no frame at all. Reusing the frame
on every tail call would make compiled code leave to the interpreter at each
`return f(x)`, which strict code is full of.

## Decision

The compiler marks a call in tail position with bit 41 of its `opCall` or
`opCallV`, for a `return`'s value and a concise arrow body, through the forms
§15.10.2 lists. It leaves the bit off in non-strict code, generators, async
functions, and while a try block, a `using` declaration or a `for-of` loop is
still open; a catch block without a finally, and a finally block, are tail
positions.

The VM acts on the bit only once the frames are deep: past 1,024, or half of a
smaller `--max-stack` bound. There Execute replaces the calling frame with the
callee's (XTailCall: the frame's upvalues closed, its this and arguments moved
to its base, its return index and kind kept), FastLoop hands the call to
Execute, and the baseline JIT's call exits to the interpreter. Below the floor
the call is an ordinary call in every tier. A construction's frame (whose
result the VM checks), a callee that is not a bytecode function, a generator or
async callee, and another realm's function keep their frames.

## Consequences

A chain of tail calls holds at most about 1,024 frames however long it runs,
so recursion through tail calls is bounded by neither the stack limit nor the
frame bound, in every tier. Code that stays below the floor runs as before:
compiled code keeps its native calls, the optimizing tier its inlining, and an
error's stack trace every caller. Past the floor, the callers a tail call
replaced are gone from the trace, as in an engine that reuses every frame.

Code that recurses past the floor through tail calls from compiled code pays
an exit to the interpreter per call. The bytecode format keeps version 200: a
file with tail bits is refused by an older G2's verifier at its call flag.
