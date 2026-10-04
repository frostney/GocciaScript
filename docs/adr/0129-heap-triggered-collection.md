# 0129 - The collector also triggers on the heap manager's in-use total, and refusal stays on tracked bytes

**Date:** 2026-10-04
**Area:** `gc`
**Related:** [ADR 0106](0106-sandbox-hardening-scope.md), [ADR 0110](0110-growth-gate-collects-before-refusing.md), [#1442](https://github.com/frostney/GocciaScript/issues/1442)

## Context

`--max-memory` (and the default ceiling, half of physical memory capped at
8 GB on 64-bit) is compared against `BytesAllocated`. That counter charges each
managed value its `InstanceSize` plus the payloads that have an owner able to
release them (strings, `ArrayBuffer` backing stores). Most of what the heap
manager hands out for a value is not in it.

Measured on Linux x64 with the FPC heap manager wrapped to count every block,
on a loop that allocates and drops `{ x: 1 }` and the array
`Reflect.ownKeys` returns 400,000 times:

| Per iteration (interpreted) | Bytes |
| --- | ---: |
| Charged to `BytesAllocated` (a scope, two objects, an array, a string, two numbers) | 520 |
| Held by the heap manager | about 3,440 |

The difference, in heap blocks:

- **Symbol-keyed storage nobody used.** Every object created a 16-slot hash
  table (a 416-byte block plus its 32-byte instance) and an insertion-order
  list (64 bytes) in its constructor. Across objects, arrays, iterator results,
  promises and native functions this alone was 36 to 41% of the heap.
- **Property storage.** The shaped property map instance (96 bytes), its bucket
  and entry arrays once a property exists (96 bytes each), and a descriptor per
  property (32 bytes).
- **Scope bindings.** A scope's binding map and its arrays, about 256 bytes.
- **Allocator rounding.** On 64-bit the FPC heap manager stores
  `InstanceSize + 8` in a block rounded up to 32 bytes: a 72-byte object takes
  96 and a 24-byte number 32.

Across that loop and a Promise micro-benchmark, in both execution modes, the
heap held 4.8 to 6.7 bytes for every tracked byte. Automatic collection while a
script runs happens only at the pressure checkpoints (the interpreter after
every expression, the VM every 1,024 instructions), and they fired only as
`BytesAllocated` neared the ceiling. So a script holding almost nothing alive
reached five to six times the ceiling in resident memory before its first
collection: about 400 MB at `--max-memory=64MiB`, and at the 8 GB default an
estimated 50 GB, past the point where the kernel kills the process. Two agent
hosts were OOM-killed this way, at 27 GB and 6 GB.

The heap manager does return memory: after an explicit collection the same
loop's resident set fell from 1.39 GB to 168 MB, and the Promise benchmark's
from 1.62 GB to 117 MB. Peak resident memory is therefore a meaningful thing to
bound.

## Decision

**Collection also triggers on the heap manager's in-use total.** Every 1,024
object registrations, `TGarbageCollector.SampleHeap` reads
`GetFPCHeapStatus.CurrHeapUsed`. At or above the heap trigger it latches the
pending flag an external reservation already uses, and zeroes the VM's
pressure countdown, so the next checkpoint collects. The interpreter's
per-expression checkpoint still tests a single flag.

**No collection site is added.** The trigger only changes *when* the existing
checkpoints collect. It never collects from inside an allocation; an
allocation-time collection would sweep temporaries held only in Pascal locals,
which is what [#1143](https://github.com/frostney/GocciaScript/issues/1143) and
[#1157](https://github.com/frostney/GocciaScript/issues/1157) are about.

**The heap trigger backs off above what a collection cannot reclaim.** The
trigger sits one pressure reserve (`MaxBytes / 8`, clamped to 16 KiB…16 MiB)
below the ceiling, like the tracked trigger. After each collection the
collector records the heap still in use: survivors plus everything the
collector does not own, such as source text, ASTs, bytecode and its own object
list. When that level already sits within a reserve of the trigger, the
trigger moves to the larger of a reserve and half that level above it. Without this, a heap that collecting cannot bring under
the ceiling would collect at every growth of one reserve. The heap therefore
peaks at about `max(ceiling, 1.5 × what survives a collection)`.

**Refusal stays on `BytesAllocated`.** Charged allocations still raise the
catchable `RangeError` and gated growth points still raise `MemoryLimitError`,
both against `BytesAllocated`, as [ADR 0110](0110-growth-gate-collects-before-refusing.md)
describes. A script whose live data costs more heap than the ceiling, while
its tracked bytes stay under it, runs on and collects once per growth step
instead of being refused.

**Object symbol storage is created on the first symbol key.**
`TGocciaObjectValue.EnsureSymbolStorage` creates the hash table and the
insertion-order list when a symbol-keyed property is first defined, and every
reader treats their absence as "no symbol properties". This removed about
512 heap bytes from every object and brought the heap-to-tracked ratio on the
same workloads from 4.8–6.7 down to 2.4–3.6, which also makes heap-triggered
collections less frequent.

**FPC only.** The heap manager keeps its status per thread, which matches the
thread-local collector. A Delphi build reports no heap total, so its heap
trigger never fires and it keeps the tracked trigger alone.

## Considered options

- **Charge more allocations to `BytesAllocated`.** A per-class static
  overhead and the allocator's rounding could be charged at registration, but
  the dynamic storage — property maps, element lists, scope bindings — would
  then need charging on every growth and release, which reopens ADR 0106's
  decision to gate rather than charge that storage. The ratio also varies with
  the workload (strings and buffers are charged nearly exactly, small objects
  several times off), so no fixed correction is right, and every exact byte
  count that tests and ADRs pin would move.
- **Lower the default ceiling.** One constant, but a single divisor is wrong for
  some workload: dividing by the object ratio would refuse string-heavy scripts
  far below half of physical memory, and it would leave `--max-memory` exactly
  as unbounded as before.
- **Collect on the allocation-count threshold during execution.** The tightest
  bound, but every ordinary run would then collect constantly at every
  checkpoint, exposing every remaining rooting gap in default runs. It stays
  behind #1143 and #1157.
- **Refuse on the heap total after a collection.** It would make
  `--max-memory` bound live data too, but it changes both refusal contracts and
  counts memory the guest does not control (source text, ASTs). It is left as a
  follow-up decision.

## Consequences

Measured on Linux x64 (`GocciaRunner`, production build), peak resident memory
from `/usr/bin/time`:

| Workload | Ceiling | Before | After |
| --- | --- | ---: | ---: |
| Flat allocate-and-drop loop, interpreted | 64 MiB | 398 MB | 69 MB |
| Flat allocate-and-drop loop, bytecode | 64 MiB | 400 MB | 70 MB |
| Flat allocate-and-drop loop, either mode | 256 MiB | 798–1,356 MB (no collection) | 258 MB |
| Promise micro-benchmark, both modes | 64 MiB | 309–310 MB | 74–77 MB |
| Promise micro-benchmark, both modes | 256 MiB | 1,104–1,188 MB | 266 MB |
| Flat loop, both modes | default (8 GB) | 798–1,356 MB | 397–755 MB |

At the default ceiling no heap-triggered collection runs on these workloads; the
lower peak there comes from the lazy symbol storage. With 300,000 objects kept
alive and garbage churned on top at 64 MiB, the run completes with 9
collections in bytecode mode (24 interpreted) at 230–242 MB resident, about 1.5
times the live heap. Measured before the symbol storage became lazy, the same
run took 11 (25) collections with the back-off and 125 (232) without it.

Under a tight ceiling collections become several times more frequent — 48
instead of 6 for the Promise benchmark at 64 MiB — which costs about 7.6% more
instructions than tracked-only triggering. The lazy symbol storage more than
pays for that: the same run uses 12.3% fewer instructions than before either
change, and with no ceiling pressure the sampling itself costs under 0.1%.

More frequent collection under a low ceiling also reaches rooting defects
sooner. The full JavaScript suite run with a very low ceiling already faults
before this change, and reaches the same faults at a somewhat higher ceiling
after it. The default ceiling is unaffected, because the heap trigger fires only
near it.

Coverage: `Goccia.GarbageCollector.Test` checks that heap growth triggers a
collection before garbage outgrows the ceiling, and that the trigger backs off
above heap the collector cannot reclaim. `scripts/test-cli.ts` checks that a
flat allocation loop and the Promise micro-benchmark stay under twice a 64 MiB
ceiling in resident memory, in both modes.
