# 0145 - The heap trigger follows growth since the last collection, and gated storage growth samples the heap

**Date:** 2026-10-17
**Area:** `gc`
**Related:** [ADR 0129](0129-heap-triggered-collection.md), [ADR 0141](0141-tracked-pressure-trigger-backs-off.md), [ADR 0106](0106-sandbox-hardening-scope.md), [ADR 0110](0110-growth-gate-collects-before-refusing.md), [#1747](https://github.com/frostney/GocciaScript/issues/1747), [#1749](https://github.com/frostney/GocciaScript/issues/1749)

## Context

ADR 0129 added a heap trigger: every 1,024 object registrations the collector
reads the heap manager's in-use total, and at or above the trigger the next VM
checkpoint collects. Two gaps remained.

**The trigger sat only near the ceiling.** Both the tracked trigger and the
heap trigger fire one pressure reserve (`MaxBytes / 8`, clamped to
16 KiB…16 MiB) below `--max-memory`. The default ceiling is half of physical
memory, up to 8 GB. A script that keeps little alive therefore grew towards
the ceiling before its first collection ([#1749](https://github.com/frostney/GocciaScript/issues/1749)):

| Run (1,000 small objects live, 2,000,000 allocated) | Collections | Peak RSS |
| --- | ---: | ---: |
| `--max-memory=64MiB` | 26 | 73 MiB |
| `--max-memory=512MiB` | 2 | 548 MiB |
| `--max-memory=2GiB` | 0 | 1,621 MiB |
| default ceiling | 0 | 1,621 MiB |

**The heap was sampled per registration, not per byte.** Element storage and
large property storage are gated, not charged (ADR 0106): `RequireNativeBytes`
tests the request against the ceiling and charges nothing. `new Array(100000)`
registers about two objects for 800 KB of element storage, so up to about
400 MB of element storage could be allocated between two samples, and `BytesAllocated` never saw
it ([#1747](https://github.com/frostney/GocciaScript/issues/1747)): 300 such
arrays dropped in a loop at `--max-memory=64MiB` peaked at 340 MiB resident
with no collection, 2,000 at 565 MiB with 4.

## Decision

**The heap trigger also fires on growth since the last full collection.** The
collector already records the heap in use after each full collection
(`FHeapAfterLastCollect`: survivors plus everything it does not own). The heap
trigger is now the lower of the ceiling trigger and that record times
`HEAP_GROWTH_TRIGGER_FACTOR` (4), and at least `HEAP_GROWTH_TRIGGER_MIN_BYTES`
(64 MiB) above the record. It latches the same pending flag as before, so the
next checkpoint collects. No collection site is added.

- The growth trigger only ever lowers the trigger. Near the ceiling the ceiling
  trigger and its back-off (ADR 0129) decide, as before.
- It always sits at least 64 MiB, or three times the record, above what the
  last collection left, so a heap that a collection cannot shrink does not
  collect more often. The live set inside the tracked reserve
  ([#1673](https://github.com/frostney/GocciaScript/issues/1673), ADR 0141)
  is unaffected: the tracked trigger is separate, and the heap trigger cannot
  fire again before the heap grows by that step.
- A sample below the record lowers it, as ADR 0129 decided, so the growth
  trigger comes down with it.
- With `--max-memory=0` there is no ceiling and, as before, no heap sampling.

**Gated growth samples the heap.** `RequireNativeBytes` reports every granted
request to `TGarbageCollector.NoteNativeGrowth`. Once the reported growth
since the last sample reaches 1 MiB, or a pressure reserve if that is smaller,
the collector samples the heap. It still charges nothing and never collects:
gate-not-charge stays as ADR 0106 decided. Element storage that grows by
appending (`push`, literal construction) does not pass the gate, so it is
still sampled only through registrations; the measured push-built loop already
collects that way.

**Refusal is unchanged.** Both refusal paths still compare `BytesAllocated`
with the ceiling (ADR 0110). This decision changes only when the existing
checkpoints collect.

## Considered options

- **A factor of 2 and a 32 MiB floor**, as #1749 suggested. It holds the
  smallest heap, but it doubled the collection work: the AWFY Havlak benchmark,
  which reaches 3,958 MiB with no collection at the default ceiling, took 36%
  longer than before, against 16% with a factor of 4 (table below).
- **A factor of 2 and a 128 MiB floor.** Havlak cost about the same as a
  factor of 4 (13%), but a small live set peaked at 154 MiB, above the
  150 MB #1749 asked for, and a large live set collects three times as
  often.
- **A factor of 3 and a 64 MiB floor.** Havlak 17% slower, for the same
  peak for a small live set as a factor of 4 (the floor decides it).
- **Collect on the allocation count.** ADR 0129 left it behind #1143 and
  #1157: it would collect constantly in every run. The growth trigger collects
  only at the same checkpoints and only after the heap has grown by at least
  the floor.
- **Charge element and property storage.** ADR 0129 and ADR 0106 rejected it:
  the containers have no hook to release a charge when they shrink or are
  freed. Sampling the heap needs no release, because the heap manager's total
  already falls when they do.
- **Report element growth where the list reallocates.** FPC's generic list
  grows inside `Add` through `SetLength` on its private array, with no virtual
  hook, so the append path cannot be reported without wrapping every append
  site. The gate covers the shapes #1747 measured.

## Consequences

Measured on Linux x86_64 (16 cores, FPC 3.2.2), production builds of `main`
and of this change, bytecode, peak RSS from `/usr/bin/time`, collections from
`--output=json`:

| Workload | Ceiling | Before: collections | Before: peak RSS | After: collections | After: peak RSS |
| --- | --- | ---: | ---: | ---: | ---: |
| 300 × `new Array(100000)` dropped | 64 MiB | 0 | 340 MiB | 6 | 74 MiB |
| 300 × `a.length = 100000` dropped | 64 MiB | 0 | 340 MiB | 6 | 74 MiB |
| 2,000 × `new Array(100000)` dropped | 64 MiB | 4 | 565 MiB | 41 | 74 MiB |
| 300 push-built arrays of 100,000 | 64 MiB | 63 | 93 MiB | 63 | 93 MiB |
| 1,000 small objects live, 2,000,000 allocated | 64 MiB | 26 | 73 MiB | 26 | 73 MiB |
| same | 512 MiB | 2 | 548 MiB | 22 | 83 MiB |
| same | 2 GiB | 0 | 1,621 MiB | 22 | 83 MiB |
| same | default (8 GB) | 0 | 1,621 MiB | 22 | 83 MiB |

The AWFY suite at the default ceiling, interleaved A/B through
`scripts/awfy-ci-report.js` (five samples each, median of the in-script time):
Havlak took 18.7% longer, and every other benchmark was within 5% either
way (Bounce, CD, DeltaBlue,
Json, List, Mandelbrot, NBody, Permute, Queens, Richards, Sieve, Storage,
Towers). Havlak run whole, three runs each:

| Havlak, default ceiling | Collections | Peak RSS | Wall time (median) |
| --- | ---: | ---: | ---: |
| Before | 0 | 3,958 MiB | 25.4 s |
| Factor 2, 32 MiB floor | 78 | 129 MiB | 34.6 s |
| Factor 2, 128 MiB floor | 27 | 229 MiB | 28.7 s |
| Factor 3, 64 MiB floor | 39 | 211 MiB | 29.8 s |
| Factor 4, 64 MiB floor (this decision) | 28 | 261 MiB | 29.4 s |

The rows other than the first and last were measured as candidates in the
same session, from builds that differed only in the two constants.

Collecting costs time that a run which never collected did not spend. The
small-object run above takes about 18% longer at the default ceiling (5.8 s
against 4.9 s), and the same as before at 64 MiB, where it already
collected. A run whose heap never grows by the floor over what the last
collection left collects no more often than before.

More collections also reach rooting gaps sooner (#1143, #1679). The growth
trigger collects only at the VM checkpoint, the safe point every pressure
collection already uses and the GC-stress build collects at every 8
instructions.

Coverage: `Goccia.GarbageCollector.Test` checks that heap growth triggers a
collection under a far ceiling and keeps the heap within the growth step, that
the growth trigger does not repeat over heap a collection cannot reclaim, and
that reported native growth samples the heap with no registration.
`scripts/test-cli.ts` checks that 300 dropped `new Array(100000)` stay under
twice a 64 MiB ceiling in resident memory on Linux, and that a small live set
under a 2 GiB ceiling collects (everywhere) and stays under 150 MB resident
(Linux).
