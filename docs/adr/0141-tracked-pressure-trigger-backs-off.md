# 0141 - The tracked-bytes pressure trigger backs off above a live set that sits inside the reserve

**Date:** 2026-10-14
**Area:** `gc`
**Related:** [ADR 0129](0129-heap-triggered-collection.md), [ADR 0130](0130-vm-stacks-are-charged-to-the-memory-budget.md), [ADR 0110](0110-growth-gate-collects-before-refusing.md), [#1673](https://github.com/frostney/GocciaScript/issues/1673), [#1508](https://github.com/frostney/GocciaScript/issues/1508)

## Context

While a script runs, the collector runs only at the pressure checkpoints (the
VM every 1,024 dispatched instructions, the interpreter after every
expression) and at the forced collection a refused request takes. A
checkpoint collects when a trigger has fired. The tracked-bytes trigger fired
whenever `BytesAllocated` was at or above `MaxBytes - Reserve`, where the
reserve is `MaxBytes / 8` clamped to 16 KiB…16 MiB. A charged reservation
(string payloads, `ArrayBuffer` backing stores) that left the total there
also latched a collection for the next instruction.

The trigger kept no record of the last collection. A script whose working set
fits under the ceiling but sits inside the reserve stays at or above the line
after every collection, so every checkpoint collected and every charged
reservation latched another collection. Nothing leaked and each collection
freed the garbage, but the cost of a collection grows with the live set, so
allocation slowed by one to three orders of magnitude.
[#1673](https://github.com/frostney/GocciaScript/issues/1673) measured it in
bytecode mode on a production build, at `--max-memory=64MiB` with 20,000
live objects:

| Allocation | Live before the loop | N | µs per allocation | Collections |
| --- | --- | ---: | ---: | ---: |
| `{ a: 1, b: 2 }` | 26.6 MiB, below the line | 100,000 | 2.98 | 2 |
| `{ a: 1, b: 2 }` | 58.1 MiB, inside the reserve | 100,000 | 101.1 | 1,562 |
| `new ArrayBuffer(64)` | 26.6 MiB, below the line | 2,000 | 3.19 | 0 |
| `new ArrayBuffer(64)` | 58.1 MiB, inside the reserve | 2,000 | 7,050.8 | 2,096 |

[#1508](https://github.com/frostney/GocciaScript/issues/1508) reports the same
trigger in interpreted mode, where the checkpoint follows every expression.

ADR 0129 gave the heap-manager trigger a back-off for the same shape: after a
full collection it records the heap still in use and raises the trigger above
it. The tracked trigger next to it had none.

## Decision

**The tracked trigger backs off above what the last full collection left
live.** `Collect` records `BytesAllocated` once it has swept. While that
record sits below the line, the trigger is the line, as before. When it is at
or above the line, a collection could not get the live set under it, and
another one before the script allocates more would mark the same live set to
free little or nothing. The trigger
then moves halfway from the recorded level to the ceiling. Collections follow
allocated bytes: one for each half of the remaining headroom that the script
allocates, rather than one per checkpoint.

**The trigger comes back down on its own.** A collection that gets the live
set under the line records a level below it, and the trigger is the line
again. Between collections, a release that takes the live total below the
recorded level (a destructor's release, a resize that shrinks a buffer) lowers
the record to that total, so freeing memory lowers the trigger without waiting
for a collection, and the lower record holds when the script grows back before
any checkpoint runs.
A young collection (`CollectYoung`, used only between benchmark rounds) keeps
every older object, so it does not move the record, as ADR 0129 decided for
the heap record.

**The trigger never rises above the ceiling.** Charged reservations and gated
growth do not rely on the trigger: both force a collection and re-test before
they refuse (ADR 0110), so they never pass the ceiling and still find garbage
the back-off left on the heap. A plain value allocation that would pass the
ceiling is refused with the catchable `RangeError` without collecting first
(`TGocciaValue.AfterConstruction`), so only a checkpoint collection clears
the garbage in front of it. The backed-off trigger is therefore at most
`MaxBytes`, and a script whose live set has reached the ceiling collects at
every checkpoint, as before.

**A live set inside the reserve gives up burst headroom.** Before this
change every checkpoint inside the reserve collected, so the plain
allocations between two checkpoints could use all of the headroom `h` above
the live set. With the back-off, up to `h / 2` of garbage can already sit in
that headroom when a burst starts. A burst between two checkpoints that fits
in `h` but not in what is left is refused with the `RangeError` where the
earlier trigger ran it. The ceiling does not move, nothing allocates past it,
a script whose live set is below the line is unaffected, and catching the
error, calling `Goccia.gc()` and retrying succeeds. This is accepted in
exchange for removing the collection at every checkpoint.

**One predicate serves every caller.** `NeedsMemoryPressureCollection` tests
the backed-off trigger, so the VM's checkpoint, the interpreter's checkpoint
and the latch in `TryChargeExternalBytesLocked` all change together. The
non-collecting charge path (`TryChargeExternalBytes`, used for VM stacks and
suspended generator frames) still never latches. The forced collections
(`TryCollectForLimitedBytes`, the GC-stress checkpoint) pass `AForce` and do
not consult the trigger. The 256 MiB external-allocation trigger is
unchanged.

ADR 0130 stops a deep recursion's stacks at the line partly because "inside
the reserve, every check would be a full collection". With the back-off, the
checks inside the reserve collect once per half of the remaining headroom
instead. That still grows more frequent as the headroom closes, each
collection still marks every frame, and the reserve is still kept for the
`RangeError` and its handler, so the stack line stays where it is.

## Considered options

- **A fixed budget, such as a fraction of the reserve.** Simple, but a budget
  larger than the remaining headroom would put the trigger above the ceiling,
  and one that is small next to the headroom collects more often than needed.
  Halving the headroom adapts to both.
- **Double the budget after each collection that fails to get under the
  line.** It amortises well when there is room to grow, but the ceiling caps
  the trigger, so the doubling ends at the ceiling after a few steps, and from
  there it is the next option.
- **Move the trigger to the ceiling itself.** Collections would be half as
  frequent, but plain objects would run past the ceiling for up to a
  checkpoint interval before a collection, and most collections would move
  from the checkpoints into the forced collections of charged and gated
  requests, which run with more of the engine's state held in Pascal locals.
- **Collect before refusing a plain value allocation**, as charged
  reservations do. It would keep the full headroom, but a collection could
  then run inside any allocation, including in the middle of a built-in that
  holds values only in Pascal locals, the bug class
  [#1679](https://github.com/frostney/GocciaScript/issues/1679) tracks. The
  GC-stress build collects at instruction boundaries, not at allocation
  sites, so it could not find such cases.
- **Back off by half the survivors, as the heap trigger does.** The heap
  trigger may sit above the ceiling because refusal does not use it. The
  tracked trigger may not, for the reason above.

## Consequences

Measured on Linux x86_64 with production builds of `main` and of this change,
running the #1673 reproduction (`--max-memory=64MiB`, 20,000 live objects,
bytecode mode), median of 3 runs. "Below the line" holds about 26.6 MiB live,
"inside the reserve" about 58 MiB:

| Allocation | Live set | N | Before: µs each | Before: collections | After: µs each | After: collections |
| --- | --- | ---: | ---: | ---: | ---: | ---: |
| `{ a: 1, b: 2 }` | below the line | 100,000 | 1.93 | 2 | 1.93 | 2 |
| `{ a: 1, b: 2 }` | inside the reserve | 100,000 | 65.9 | 1,562 | 1.89 | 4 |
| `new ArrayBuffer(64)` | below the line | 2,000 | 1.79 | 0 | 1.80 | 0 |
| `new ArrayBuffer(64)` | inside the reserve | 2,000 | 4,177 | 2,096 | 1.80 | 1 |

Inside the reserve, allocation now costs what it costs below the line. In
interpreted mode the same in-reserve object loop, cut to 2,000 allocations,
went from 64.5 s and 51,091 collections to 9 ms and 2. That is the collection
half of #1508; its `--timeout` overshoot is a separate fix.

Without a ceiling in reach nothing changes. Callgrind instruction counts on
eight `perf/probes` workloads (string append and builder, typed arrays,
property access, calls) and an object, string and `ArrayBuffer` allocation
loop, with no `--max-memory`, differ from `main` by at most 314 instructions
per run, under 0.0001%. The predicate tests the line before it looks at the
back-off, and the back-off arithmetic is inlined, so the charge path that
every string payload takes stays a leaf function. A first version that called
the back-off out of line cost up to 0.25% on the allocation loop.

The invariants hold, and the tests pin them. `Goccia.GarbageCollector.Test`
checks that a checkpoint inside the reserve does not collect again right
after a collection that could not get below the line, and that a charged
reservation there latches nothing. It checks that the backed-off trigger
still fires once the script allocates half of the remaining headroom, and
that a reservation that does not fit still collects first, finds the garbage
made in between, and fits. It also checks that what no collection can make
room for is still refused under the ceiling, and that the trigger returns to
the line once a collection gets back under it. `scripts/test-cli.ts` counts
`memory.gc.collections` for a run that parks its live set 3 MiB under a 64 MiB
ceiling and then allocates 20,000 objects and 500 small buffers: 779
collections before, 2 after, against a bound of 40.

The cost is the burst headroom described above. Two reproductions from the
review of this change, at `--max-memory=64MiB`, production builds, bytecode
mode:

- A live set parked 128 KiB under the ceiling makes two `Object.entries`
  bursts of about 40 KiB and 88 KiB per iteration, separated by loops longer
  than a checkpoint interval that allocate nothing. The earlier trigger
  finishes 200 iterations; this change refuses the second.
- A live set parked 12 KiB under the ceiling allocates 100,000 small objects
  that are garbage at once. The earlier trigger finishes; this change refuses
  after 2,041.

Both refusals are the catchable `RangeError`, and no run passed the ceiling.
`scripts/test-cli.ts` pins the shape: with the live set parked 1.5 bursts
under the ceiling and 0.6 of a burst of garbage made, an `Object.entries`
burst is refused, and after `Goccia.gc()` the same burst fits.
