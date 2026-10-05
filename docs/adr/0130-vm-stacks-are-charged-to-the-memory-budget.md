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
(4,096 entries for the register, local-cell and argument stacks, 64 for the
two frame stacks). The stacks may grow up to the memory-pressure line: the
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

PR #1483 lowers the initial capacities to 64 entries for the register,
local-cell and argument stacks and 8 for the frame stack, so more calls take
the growth path. The suite and the instruction-count comparison were also run
with those capacities applied.
