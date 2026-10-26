# 0160 - Concise arrow bodies make proper tail calls, and closed numeric frames give way to them

**Date:** 2026-10-26
**Area:** `compiler`, `bytecode VM`
**Related:** [ADR 0101](0101-closed-numeric-scalar-self-call-frames.md), [ADR 0127](0127-collector-skips-closed-numeric-frames.md), [Bytecode VM](../bytecode-vm.md)

## Context

The compiler marked a call in tail position with `CALL_FLAG_TAIL` only in a
`return` statement's value. A concise arrow body compiled its expression as an
ordinary value, so `(n) => n === 0 ? x : f(n - 1)` stacked a frame per call
and failed with `RangeError` after the default 2,200 nested calls, while the
same function with `{ return …; }` ran at any depth. ES2026 §15.10.1
IsInTailPosition treats a ConciseBody like a FunctionBody, and §15.10.2
HasCallInTailPosition reaches its expression through the same rules as a
`return`.

[ADR 0127](0127-collector-skips-closed-numeric-frames.md) lets the collector
skip the registers of closed numeric frames because no other frame runs above
one: a closed numeric arrow calls only itself, and its concise body compiled
those self-calls as `OP_CALL_SELF_NUM`. It noted that proper tail calls in
concise bodies would break that, and would have to bring the marking back. A
self-call in tail position becomes a generic tail call, which sets up a frame;
from inside a closed numeric frame it set one up above it.

## Decision

A concise arrow body compiles its expression with the tail-position rules a
`return` uses, under the same conditions: strict code, not async. A
conditional in tail position compiles its condition as any conditional does,
so the fused compare-and-jump instructions are kept.

An arrow whose concise body has a self-call in tail position gets no closed
numeric frames. Its numeric proof still holds, so its parameters keep their
Number type and the numeric immediate instructions, but its non-tail
self-calls are ordinary calls. The collector keeps skipping closed numeric
frames, and the invariant ADR 0127 rests on still holds.

## Consequences

Recursion through a tail call in a concise arrow body no longer depends on the
stack limit, as in a block body. Infinite recursion written that way, such as
`const f = () => f()`, now runs until a timeout or instruction limit rather
than throwing `RangeError`; tests that need the overflow call from a block
body.

A numeric self-recursion that mixes tail and non-tail self-calls loses the
scalar frame for its non-tail calls. One with only non-tail self-calls, such
as Fibonacci, keeps it.
