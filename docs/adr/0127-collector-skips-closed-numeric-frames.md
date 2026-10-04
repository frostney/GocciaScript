# 0127 - The collector does not mark the registers of closed numeric frames

**Date:** 2026-10-04
**Area:** `bytecode runtime`

A closed numeric frame ([ADR 0101](0101-closed-numeric-scalar-self-call-frames.md))
took its register window without clearing the slots below a watermark that
was local to one native VM entry and only ever rose. Ordinary frames, native
callbacks and resumed generators use the same slots between two scalar
recursions and leave object references there, and the collector marks the
whole live register arena, so a collection during the next scalar recursion
followed references to freed objects (#1364). The collector now marks the
register arena only up to the first closed numeric frame's window while one
is live, and a closed numeric frame clears nothing. That loses nothing,
because a closed numeric frame stores only scalars and the pinned NaN,
infinity and -0 values, and writes each register before reading it. It rests
on closed numeric frames being the innermost frames whenever one is live:
they call only themselves, since the proof admits no other call and a concise
arrow body compiles its self-calls as `OP_CALL_SELF_NUM`, not as tail calls.
Development builds assert in `SetupNewFrame` that no frame is set up while a
closed numeric frame is live; a change that lets one call out, such as proper
tail calls in concise arrow bodies, has to bring the marking back for them.

## Considered Options

Keeping the clear on push and tracking, VM-wide, the slots that only closed
numeric frames had written since any other window used them also fixes the
bug, without depending on which frames can run above closed numeric frames.
It costs every ordinary call a compare in `AcquireRegisters`. Measured with
callgrind on the production runner, it added 0.13% to a recursive Fibonacci
on ordinary frames and saved 0.21% on the closed numeric one; the chosen
design costs ordinary calls nothing and saves 0.73% on the closed numeric
Fibonacci, because the push no longer tests a watermark.
