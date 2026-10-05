# 0130 - The bytecode VM's stacks are charged to the memory budget, and a refusal is a catchable RangeError at the next instruction boundary

**Date:** 2026-10-04
**Area:** `gc`, `bytecode runtime`, `sandbox`
**Related:** [ADR 0027](0027-heap-trampoline-call-stack-limit.md), [ADR 0106](0106-sandbox-hardening-scope.md), [ADR 0110](0110-growth-gate-collects-before-refusing.md), [ADR 0126](0126-bytecode-call-path-arena-fills-and-thread-binding.md)

## Context

The bytecode VM keeps a call's state in five arenas it grows by doubling:
the register, local-cell and argument stacks, the frame stack, and the
closed-numeric frame stack. Nothing checked that growth against
`--max-memory`. Neither was the snapshot a generator or async function takes
of its frame when it suspends.

`--max-stack` used to bound the arenas indirectly. With `--max-stack=0`,
nothing did. A recursion costs about 520 bytes of real memory per frame but
only about 24 bytes of `BytesAllocated`, so the memory budget never noticed.
On 2026-10-04 a production `GocciaRunner` running
`const f = () => { d++; f(); }; f()` with `--max-stack=0` reached 86 GB before
the kernel's OOM killer stopped it, together with the agent host around it
(#1472). Under `--max-memory=256MiB` the same script held 834 MB at 1.6
million frames, and nothing refused it.

The budget refuses in one of two ways (ADR 0106, ADR 0110). A **charged**
allocation has an owner that releases the bytes again, and a refused charge is
a catchable `RangeError`. A **gated** growth point belongs to a container with
no release hook, so it is checked before it allocates and never charged, and a
refused gate is the script-opaque `MemoryLimitError`. The VM stacks needed one
of the two. Where a refusal could happen was a separate question.

## Decision

**The stacks are charged, not gated.** They have an owner that releases the
bytes: the VM, which now shrinks a stack once deep recursion has returned and
frees everything when it is destroyed. ADR 0106 gates only storage whose
owner cannot release it, because a charge there "would leak budget the engine
could never give back". That reason does not hold here. A charge also bounds
something a gate cannot. A gate tests each allocation alone. A charge puts the
stacks into `BytesAllocated`, so the heap and the stacks share one ceiling,
and a deep recursion leaves less room for the heap.

Growth is charged for everything a stack holds past its initial capacity
(64 entries for the register, local-cell and argument stacks, 8 for the
frame stack and 64 for the closed-numeric frame stack, as set by PR #1483). The stacks may grow up to the memory-pressure line: the
reserve below the ceiling (`MaxBytes / 8`, clamped to 16 KiB–16 MiB, now
exposed as `TGarbageCollector.MemoryPressureReserve`) in which the VM collects
at every pressure check.

- **Deep recursion stops at the line.** Inside the reserve, every check would
  be a full collection, and each collection marks every frame. In a
  development build, a deep recursion that grew into the reserve took 30 s
  to reach its refusal at 64 MiB, against 0.8 s when it stops at the line. Stopping there also leaves
  the reserve free for the `RangeError` and for the handler that catches it.
- **Small stacks may enter the reserve.** Stacks no bigger than the reserve
  may grow into it, up to the ceiling itself. A program whose heap fills the
  ceiling can therefore still make calls. A first version applied the line to
  every growth. Its frame stack could then not grow past 64 entries once the
  heap passed the line, so a 100-deep recursion beside a 50 MiB heap under
  `--max-memory=64MiB` failed with this error, where `main` ran it.
- **A stack doubles only while it can afford to.** It doubles while doubling
  leaves as much of its allowance free as it takes. Past that it takes half
  of what is left. At the very end it takes only the entries its frame needs.

**A refusal is a catchable `RangeError: Maximum call stack size exceeded`.**
This follows from the charged contract. It is also the error the depth limit,
the native re-entry cap and the delegation caps already throw, and the one V8
throws when its stack runs out. ADR 0110 keeps the gate's refusal opaque
because "a ceiling the guest can catch is a ceiling the guest can ignore in a
loop". That does not apply to a stack: catching the error unwinds the frames.
A script that catches it and recurses again reaches the same ceiling at the
same depth, and the stacks do not grow past it (below).

**A growth is never refused where it happens.** Stacks grow inside call setup.
That point can neither collect nor throw:

- It cannot collect. When native code calls into bytecode, the callee, the
  receiver and the arguments are still held only in Pascal locals there.
  `SetupNewFrame` copies them into the argument window "before anything can
  collect" (ADR 0126). A collection at that point is the use-after-free that
  ADR 0110 found on the property-store paths.
- It cannot throw. `PushFrame` has saved the caller's frame, but the callee's
  frame is not set up yet. Unwinding from between the two would tear down a
  frame that does not exist and pop the caller's call-stack entry.

So the growth site charges without collecting, through the new
`TGarbageCollector.TryChargeExternalBytes`. If the charge does not fit, the
stack takes only the entries its frame needs, uncharged. The VM then sets its
memory-pressure countdown to zero, so the next instruction boundary handles
the debt. There, at the safe point where the VM already collects for memory
pressure, `SettleStackGrowth` calls `TryCollectForLimitedBytes`. That call
collects if a collection could help. If room appears, the growth is charged.
Otherwise the boundary throws. The ADR 0110 floor keeps retries at constant
cost.

Two details keep this from degrading:

- **A settle needs room for a quarter more stack.** It succeeds only if the
  allowance also has room for the stacks to grow by a quarter. Without this rule,
  a collection that freed space for just a few frames would be repeated every
  few frames, and the recursion would slow quadratically on its way to the
  same refusal.
- **Unsettled or refused growth takes only what its frame needs.** While a
  growth is unsettled or was refused, any further growth takes only the
  entries it needs. A script that catches the error and recurses again
  therefore adds one frame of uncharged memory per attempt. It cannot double
  the stacks past the ceiling.

**A stack shrinks back once it is idle.** The same instruction boundary checks
for this whenever the stacks hold a charge. A stack holding four times what it
uses shrinks to twice that, but never below its initial capacity. The bytes
freed pay off uncharged growth first, then release the charge. Every live
window lies below the current top of its stack, so no live entry is dropped.

**A suspended frame is charged to its generator.** Its registers, local
cells, arguments and handler entries are charged when the frame is captured,
only above the largest frame that generator has already held, and released
when the generator finishes or is destroyed. A generator that yields in a loop
therefore takes the accounting lock once rather than at every yield. The
capture happens at a yield or an await,
where the value being yielded can be held only in a Pascal local, so this
charge does not collect either. If it does not fit, the yield or await throws
the charged `RangeError` before anything has changed. This is how a value
allocation is refused.

**A charge is released against the collector that took it.** The VM charges
the collector its stack root is registered with, bound when the VM is created,
even when it later runs on another thread. A generator records the collector
that took its first charge and releases its charge there. A VM moved between
threads therefore neither strands a charge on one collector nor releases it
against another.

**`TryChargeExternalBytes` does not latch memory pressure.**
`TryReserveExternalBytes` does: a reservation near the ceiling arms a
collection at the next poll. An async function charges its frame at every
await. With a latch, an async chain near the ceiling would collect at every
await. Measured at `--max-memory=8MiB`, that took a 0.46 s run to 47 s. These
charges are counted like value allocations instead, and the periodic pressure
check sees them.

## Consequences

The issue's script now stops with a catchable `RangeError` under any ceiling.
Production build, Linux x86-64, `--mode=bytecode --max-stack=0`:

| `--max-memory` | Refused at depth | Peak RSS | Time |
|---|---:|---:|---:|
| 16 MiB | 74,581 | 46 MB | 0.19 s |
| 64 MiB | 299,071 | 147 MB | 0.68 s |
| 256 MiB | 1,297,363 | 581 MB | 8.2 s |

After the refusal, `Goccia.gc.bytesAllocated` drops back to its level before
the recursion.

**The ceiling still does not bound resident memory.** Peak RSS is about 2.2
to 2.9 times the ceiling, for three reasons that are outside this decision:

- Every frame also adds an entry to the thread's call stack
  (`TGocciaCallStack`) and to its execution-context stack. Both are shared
  with the interpreter, and neither is charged. A frame inside a `try` also
  adds an entry to the VM's exception-handler stack, which is not charged
  either. Each of these costs a few dozen bytes per frame, beside a charged
  frame of a few hundred, so they raise the ratio but cannot unbound it.
- The `RangeError` lists every frame in its `stack` property.
  `TGocciaCallStack.CaptureStackTrace` formats every frame and appends each
  line to one growing string. At a million frames that string is the largest
  single allocation, and building it is most of the run time. This, not the
  stack growth, is why the run time grows faster than the depth (0.68 s, then
  8.2 s, for 4.3x the depth).
- `SetLength` holds the old block and the new block together while it copies
  one to the other.

A host that needs a hard ceiling must still impose one outside the process,
as ADR 0106 Amendment 1 says.

The default ceiling is half of physical memory, capped at 8 GB, so a runaway
recursion with `--max-stack=0` and no `--max-memory` now stops instead of
running until the OS kills it. It can still reach tens of gigabytes first.

Interpreted mode is not changed. It recurses on the native stack, and
`--max-stack=0` there still ends in a native stack overflow (#1274). The
interpreter is being removed (#825).

`scripts/test-cli-apps.ts` asserts:

- the refusal is catchable;
- it happens at the ceiling rather than earlier;
- the budget is free again after the unwind;
- peak RSS stays bounded;
- a 1000-deep recursion still runs beside a heap that fills most of the
  ceiling;
- finished generators give back their frame charge.

`tests/language/functions/stack-memory-release.js` asserts that a recursion
which returns gives its stack memory back, including while a native callback
is running and across a numeric self-recursion that grows the stacks again.
`Goccia.GarbageCollector.Test` asserts that `TryChargeExternalBytes` neither
collects nor latches pressure.

PR #1483 lowered the initial capacities from 4,096 and 64 entries to 64 and
8 while this change was in review, so more calls take the growth path. The
suite and the instruction-count comparison were run with both sets of
capacities.

## Amendment 1 — the call-stack records and the stack copies are charged

**Date:** 2026-10-05
**Related:** [#1504](https://github.com/frostney/GocciaScript/issues/1504), [#1503](https://github.com/frostney/GocciaScript/issues/1503), [ADR 0129](0129-heap-triggered-collection.md)

Decision records here are immutable. This amendment is an exception on the
same ground as ADR 0106's: the Consequences above say the ceiling does not
bound resident memory, and that is no longer true of the costs it lists. The
decision itself is unchanged. The stacks are charged, a refusal is the
catchable `RangeError` at the next instruction boundary, a growth that cannot
be decided where it happens is settled there after a collection, and a charge
is released against the collector that took it. This amendment widens what is
charged and adds one rule to how a stack may grow.

### What was not charged

At a refusal, the process held about 2.2 times the ceiling, and three costs
outside the charge made up the gap:

- **The per-frame records the VM pushes for the thread.** Every call pushes a
  frame record onto the thread's call stack (`TGocciaCallStack`, 40 bytes)
  and a context onto its execution-context stack (48 bytes). A frame inside
  a `try` also pushes a handler entry (16 bytes). Beside the 175 bytes the
  issue's script charges a frame, these added half as much again, and none
  of them shrank.
- **The copy.** `SetLength` allocates the grown block, copies the old one into
  it and only then frees it, so for a moment a stack holds its old and its new
  capacity. A growth of the largest stack briefly added that stack's whole
  size again, uncharged.
- **The `RangeError`'s stack text**, which lists every frame. #1503 caps it.

### Decision

- **The VM grows the thread's call stack and execution-context stack, and its
  own handler stack, through the same charged growth path as its other
  stacks.** The push paths no longer grow: `TGocciaCallStack.PushTemplate`,
  `TGocciaExecutionContextStack.PushFunctionContext` and
  `TGocciaBytecodeHandlerStack.Push` expect room, and the VM checks for it
  before each push, where those pushes used to check. Each of the three grows
  to 64 entries uncharged, like the VM's own initial capacities. Past that it
  is charged, and it shrinks back with the other stacks at the instruction
  boundary.
- **The thread's stacks give back no more than the VM charged for them.** The
  call stack and the execution-context stack belong to the thread, not to the
  VM, and other code (the interpreter, a native constructor call) can grow
  them uncharged. The VM records how many bytes of each it accounted for, and
  for which thread's instance. A shrink releases at most that, so it never
  releases the charge of another stack. If the VM later runs on another
  thread, what it charged for the first thread's stacks stays charged until
  the VM is destroyed, because that memory is still allocated.
- **Every stack is charged for its capacity.** This was already true of the
  VM's own stacks, whose charge is the capacity each growth adds; it now holds
  for the three above.
- **A growth must leave room under the ceiling to copy the largest stack once
  more.** A growth of G bytes leaves the stacks G bytes bigger and the largest
  stack at most G bytes bigger, so it is taken only while
  `2G + largest <= MaxBytes - BytesAllocated`. That covers the copy this
  growth makes and the next one, charged or not: a growth that does not fit
  still takes the entries its frame needs, uncharged, and copies the stack to
  do so. The settle at the instruction boundary applies the same rule to the
  quarter of growth room it requires. The copy room is checked against the
  ceiling itself, not the memory-pressure line, because the copy is transient
  and nothing else is allocated while it is held.

### Consequences

The issue's script, `const f = () => { d++; f(); }`, production build,
Linux x86-64, `--mode=bytecode --max-stack=0`. Peak RSS is from
`/usr/bin/time`, and an idle run of the same binary holds 11.5 MiB. Both
builds here cap the `RangeError`'s stack text at 64 frames, as #1503 will, so
that the comparison measures the stacks:

| `--max-memory` | Before: depth | Before: peak RSS | After: depth | After: peak RSS |
|---|---:|---:|---:|---:|
| 64 MiB | 331,783 | 143.0 MiB (2.23x) | 175,510 | 77.8 MiB (1.22x) |
| 256 MiB | 1,401,400 | 563.8 MiB (2.20x) | 703,530 | 281.1 MiB (1.10x) |
| 1 GiB | 6,143,728 | 2,266.1 MiB (2.21x) | 2,815,609 | 1,083.9 MiB (1.06x) |

Above the idle run, the peak is now 1.04 to 1.05 times the ceiling. The
recursion stops at about half the depth: each frame is now charged about 260
bytes rather than 175, and the stacks keep room to copy the largest of them.

What is left above the ceiling is garbage. `BytesAllocated` counts a dead
value at its `InstanceSize`, and the heap manager holds more for it (ADR 0129
measures 2.4 to 3.6 times). In this script each frame drops a number, and
between collections those take a few percent of the ceiling more than they are
counted for. This is the gap #1467 is about, and it is outside this decision.

`scripts/test-cli-apps.ts` now bounds the recursion's peak RSS at the idle
run's plus 1.2 times the 64 MiB ceiling, which fails on the code before this
amendment, and `tests/language/functions/stack-memory-release.js` asserts that
a recursion through `try` blocks gives its handler entries back.

Callgrind instruction counts on the AWFY rows and the `perf/probes` set move
by at most 0.04%, except `typed-array-update-loop`, 0.13% lower, and
`fib-recursive`, 0.13% higher. There a numeric self-call costs about two more
instructions now that the VM, rather than `PushTemplate`, checks the call
stack for room.
