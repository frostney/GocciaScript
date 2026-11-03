# 0173 - A request the floor refused is collected for again, with a backoff

**Date:** 2026-11-02
**Area:** `gc`, `memory-limit`
**Related:** [ADR 0110](0110-growth-gate-collects-before-refusing.md), [#1990](https://github.com/frostney/GocciaScript/issues/1990)

## Context

[ADR 0110](0110-growth-gate-collects-before-refusing.md) makes a request that
does not fit force one collection before it is refused. When that collection
still leaves no room, the collector records `FForcedCollectFloor`, the number
of bytes live after it, and answers a later request that does not fit beside
that level without walking the heap. The record keeps a guest that catches the
`RangeError` and retries at constant cost per attempt.

The record is a statement about a counter, and a program drops what it held
without moving any counter: an unreachable object stays in `BytesAllocated`
until a collection frees it. Routine threshold collection is off while
bytecode runs. So a program that fills the heap, catches the refusal, drops
what it held and retries the same size is refused until something calls
`Goccia.gc()`, whose call ADR 0110 names as the way out:

```js
fill();                       // refused after 33 buffers of 8 MB, then returns
new ArrayBuffer(8000000);     // refused: nothing is reachable, the floor says no
new ArrayBuffer(100);         // fits
new ArrayBuffer(8000000);     // refused again
Goccia.gc();
new ArrayBuffer(8000000);     // fits
```

A fetch response body is reserved through the same path and behaves the same.
Re-arming forcing when the event loop takes its next task would not help this
shape, which is one synchronous run, and re-arming only for fetch would leave
every other reservation as it was.

## Decision

**The floor still answers a repeat of a refused request, but every
`FForcedCollectRetryAfter`-th request it answers collects after all.**

- The counter `FForcedCollectRefusals` counts the requests the floor has
  answered since the last forced collection. When it reaches
  `FForcedCollectRetryAfter`, the request forces a collection as if no floor
  were on record.
- `FForcedCollectRetryAfter` starts at 1, so the first retry after a refusal
  always collects: that is the retry after a drop. A retry collection that
  makes no room doubles it, up to 65,536 (`FORCED_COLLECT_MAX_RETRY_AFTER`).
  One that makes room, any collection (`Collect`, `CollectYoung`) and a drop of
  the counters below the floor reset it to 1.
- A retry loop that drops nothing therefore walks the heap on attempts 1, 2, 4,
  8 and so on, a logarithmic number of times, and then once per 65,536
  attempts, not once per attempt. The initial refusal's collection does not
  count against the interval, so a program that fills, drops and retries in a
  cycle gets its collection on every first retry.
- The state is written under `FAccountingLock` with the floor, and the floor
  stays per request size.

## Consequences

- The sequence above now reads `fits, fits, fits, fits` without `Goccia.gc()`.
- Only the first retry after the initial refusal is guaranteed to collect. If that
  collection makes no room, the interval becomes 2, and a retry that follows a
  drop but not a counter change can be refused without a collection; `Goccia.gc()`
  remains the fallback. A retry collection that frees some of what the program
  held counts as progress and keeps the interval at 1.
- A refused retry loop pays more than the O(1) it paid before. On a heap of
  20,000 small objects and about 250 MB of buffers under `--max-memory=256MiB`, one
  forced collection takes about 14 ms, and 200,000 refused attempts took 4.0 s
  before and 6.5 s after. The extra walks come from the restarts after each
  real collection (the VM's pressure checkpoints also collect while the heap is
  near the ceiling and reset the interval), about a dozen per restart. The cost
  stays proportional to the number of real collections, not to attempts.
- `Goccia.gc()` remains the way to force the issue: it collects and clears the
  record. After more than 65,536 refused attempts that dropped nothing, a drop
  can again meet a refusal until the interval ends or the program collects.
- ADR 0110's third refusal shape (a request larger than the budget) and the
  re-entrancy guard are unchanged; so is the gate's uncatchable refusal.

## Alternatives rejected

- **Re-arm at the next event-loop task.** It leaves a synchronous retry
  refused, which is the reported shape, and a loop that awaits between attempts
  would walk the heap once per turn.
- **Re-arm for fetch bodies only.** It fixes one caller of a rule that every
  reservation shares.
- **Measure time or instructions between collections.** Wall time makes
  whether a request fits depend on timing, which `--deterministic` rules out,
  and the instruction counter is armed only under an instruction limit.
- **Document `Goccia.gc()` as the required step.** ADR 0110 already does; the
  pattern it leaves, refused, fits, refused, is what the report was about.
