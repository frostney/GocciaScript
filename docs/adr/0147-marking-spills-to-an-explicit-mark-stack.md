# 0147 - Marking nests at most 64 objects deep and spills the rest to an explicit mark stack

**Date:** 2026-10-18
**Area:** `gc`
**Related:** [ADR 0129](0129-heap-triggered-collection.md), [ADR 0141](0141-tracked-pressure-trigger-backs-off.md), [#1665](https://github.com/frostney/GocciaScript/issues/1665), [#1772](https://github.com/frostney/GocciaScript/pull/1772)

## Context

`TGCManagedObject.MarkReferences` was virtual, and every override marked its
children by calling their `MarkReferences`, after an `if GCMarked then Exit;`
guard. Marking therefore took one native frame, often several, per link of
the object graph. A collection taken while a deep chain was alive ran out of
native stack: a stack overflow in a development build, a segmentation fault
(exit 139) in a production build, on Linux x86-64 and AArch64.

Nothing in the language bounds those chains. On `main`, a production build
crashed at the first collection over a chain of 30,000 classes, or 100,000
nested objects, prototypes, suspended generators or pending promise
reactions, or 300,000 nested arrays, closures, Maps or Sets. Construction of
such chains already avoids native recursion (#1665), so they were reachable;
they only crashed when a collection happened to run. #1772 makes collections
run where they did not, so
`tests/language/classes/deep-implicit-constructor-chain.js` started to crash.
`main` crashed the same way when `--max-memory` forced a collection.

## Decision

`MarkReferences` is no longer virtual. It marks the object, returns at once
if it was already marked, and otherwise has the object traced. The overrides
become `TraceReferences`, a protected virtual that the collector calls once
per marked object per collection (and again in an overflow rescan, so it must
be idempotent). It calls `MarkReferences` on each child and has no `GCMarked`
guard, since the object is already marked when it runs. All 126 overrides were
renamed and the 125 that had the guard lost it, and the call sites stayed
`Child.MarkReferences`. A branch that still declares
`MarkReferences; override;` therefore fails to compile instead of recursing
silently.

While a trace runs, `MarkReferences` traces the object in place as long as
fewer than 64 `TraceReferences` calls are nested (`MARK_RECURSION_LIMIT`), and
otherwise pushes it on the mark stack, which the trace drains before it
returns. Native stack use is bounded by 64 levels whatever the depth of the
graph, except in the out-of-memory fallback below.

- **The stack.** One process-wide array, used only under the collect lock, as
  the mark epoch is. An object is marked when it is pushed, so it is pushed at
  most once per collection, and the stack holds at most one pointer per object
  the collection marks. That is the same order as the collector's object list,
  and like that list it is not charged to `BytesAllocated`. It keeps 4,096
  entries for the life of the process and gives back what a collection grew it
  to.
- **When it cannot grow.** The object is marked but untraced, so dropping it
  would free what only it references. If the thread's collector manages it,
  the trace rescans: it calls `TraceReferences` again on every marked object
  in that collector's list, draining after each, and repeats while a pass
  overflowed. Each pass marks at least what overflowed in the one before, so
  the passes end. Objects outside that list are traced at the failed push,
  so native depth follows the graph again along chains of such objects, but
  only in that out-of-memory case.
- **Calls outside a trace.** A root, a root source or a weak container's
  `TraceWeakReferences` hook starts a trace, and gets back a fully marked
  subgraph, as before. The stack and its flags are process-wide like the mark
  epoch, so marking runs only inside a collection on the collecting thread, or
  while no other thread collects (a unit test); outside a collection the trace
  takes the collect lock itself.

## Considered options

- **Push every object, with no in-place tracing.** The first version did this.
  Leaves (numbers, strings, small objects) then paid a push, a pop and an empty
  virtual call where the recursive marker paid one call. A full collection took
  37% longer over a 200,000-entry Map, 12% longer over 500,000 small objects in
  an array and 8% longer over a 2^20-node tree. Bounded recursion is faster than
  `main` on all three (below).
- **Mark at pop instead of at push**, so that overrides could keep their guard
  unchanged. Then an object is pushed once per incoming edge until it is popped:
  `new Array(n).fill(o)` pushes `o` n times. Marking at push bounds the stack by
  objects, not edges.
- **Keep the virtual name `MarkReferences` and add a new push entry point.**
  Rejected. Every existing call `Child.MarkReferences` would then trace the
  child without marking it, and the sweep would free it while it is still
  referenced. An unconverted call site on another branch would compile and be
  wrong. Renaming the virtual makes an unconverted override a compile error
  instead.
- **Fixed-size stack with no growth.** Rescans would then be routine for wide
  arrays rather than an out-of-memory fallback.

## Consequences

Measured on Linux x86-64 (32 cores, FPC 3.2.2), production builds of `main`
at eea3db7f and of this change. Full-collection pause, median of 11
`Goccia.gc()` calls over a live heap, two interleaved runs each:

| Live heap | `main` | This change |
| --- | ---: | ---: |
| binary tree, 2^20 nodes | 288-307 ms | 260-263 ms |
| array of 500,000 small objects | 112-113 ms | 95-96 ms |
| Map, 200,000 entries | 61 ms | 54-57 ms |
| 200,000 closures | 46-47 ms | 29-38 ms |
| chain of 5,000 objects | 0.98 ms | 0.77-0.86 ms |

Throughput with collections in the loop, a live tree of 2^17 nodes and
3,000,000 short-lived objects under `--max-memory=64MiB` (50 collections in
both builds), three interleaved runs: 9.82-9.96 s on `main`, 9.31-9.54 s with
this change. On `benchmarks/`, `main` and this change ran side by side per
file, three rounds: the geometric mean of the 441 ratios is 1.000 (+0.03%),
with a median run-to-run spread of 3.6%. The largest file-level difference,
Map and Set `forEach` at about -10%, runs no collector code; a standalone loop
of it with no collection differs by 2-4%, which is code placement rather than
marking.

A cycle ends at the mark, so no override needs a guard. An override that
tests `GCMarked` on itself is now a bug, because it skips its whole body.
`docs/garbage-collector.md` states the new contract. Tools that walk the
object graph through `MarkReferences`, such as a heap snapshot, see the same
reachability as before, but in a different order, and must not assume a
child is traced before its parent's call returns. Only the outermost call
guarantees that.
