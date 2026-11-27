# Garbage Collector

*The engine's generational collector: a moving nursery and a mark-and-sweep old generation, one heap per engine thread.*

## Executive Summary

- **Two generations** — New cells are bump-allocated in a nursery; a minor collection copies the live ones into old space, promoting each on its first survival. Old space is mark-and-sweep over size-class pages and is never moved (`Goccia.Core`: `AllocCell`, `CollectMinor`, `CollectMajor`)
- **The nursery moves cells** — Any allocation can run a minor collection, so a `TValue` or cell pointer a native holds in a Pascal local is stale after it. Natives root what they keep with `PushRoot` and read it back from the root (`RootAt`) or from the value stack; see [Contributor Rules](#contributor-rules)
- **A write barrier and a remembered set** — Storing a young pointer into an old cell remembers the old cell, so a minor collection scans the remembered cells instead of old space
- **Weak references and finalization** — WeakMap and WeakSet are ephemeron tables outside the heap; WeakRef and FinalizationRegistry targets sit in fields the collector does not trace and are cleared or updated after marking (`Goccia.Weak`)
- **The memory limit refuses at the next safe point** — A full collection that leaves the heap over `--max-memory` throws a catchable `RangeError` at the next back edge or call; eight refusals in a row that free nothing end the run ([ADR 0185](adr/0185-g2-refuses-memory-at-the-next-safe-point.md)). An explicit reservation, such as an `ArrayBuffer`'s bytes, is refused with a `RangeError` at once
- **Stress tooling** — `--gc-stress` collects at every nursery allocation, `--gc-verify` checks roots, the barrier and the heap around every collection, `GOCCIA_NURSERY_KB` shrinks the nursery, and `scripts/engine/gcstress.sh` runs the benchmark bundles, the regression scripts and `tests/` that way ([GC Stress and Verification](#gc-stress-and-verification))

## Heap Layout

Every heap thing is a **cell** with an 8-byte header (`TCellHeader`): its size in bytes (a multiple of 8), its kind
(string, object, value array, upvalue, accessor, symbol, BigInt, bytes and so on), its flags and 16 bits of
kind-specific data. A `TValue` is NaN-boxed: numbers, `undefined`, `null`, the booleans and the array hole are
immediates, and only a value tagged as a pointer refers to a cell ([Engine design — Values](engine/design.md#values)).

| Region | What lives there | How it is collected |
|---|---|---|
| Nursery | Cells up to 1/16 of the nursery's size, bump-allocated (`NurseryTop` up to `NurseryLimit`) | Copied out by every minor collection, then reused from its start |
| Old space, small | Cells of up to 16,384 bytes, rounded up to one of 22 size classes, in 256 KiB pages per class | Marked and swept by a major collection; a freed cell goes on its class's free list |
| Old space, large | Cells over 16,384 bytes, each its own block | Marked and swept; a dead one is freed at once |
| Permanent | Atoms (interned property keys), key atoms (symbol keys, private names, engine slots) and a few engine-owned cells (`cfPermanent`) | Never collected; freed with the heap |
| Outside the heap | Shapes, function templates and bytecode, the atom table, WeakMap and WeakSet tables, SharedArrayBuffer blocks | Owned by their units; shapes are retired by a major collection ([below](#prototypes-and-shapes)) |

The default nursery is 8 MiB (`EngineNurseryBytes`, used by `EngineInit` and so by `TGocciaEngine`). A cell larger than
1/16 of the nursery is allocated directly in old space (`AllocOld`): a large temporary such as a long concatenation
result would otherwise fill the nursery. A value array or object born old starts remembered, because the stores that
fill it are made without a write barrier, as into a young cell.

Old cells never move. Atoms and other permanent cells never move or die while the engine lives. Only young cells move,
and only during a minor collection.

## Collections

`Goccia.Core` has two collection routines. A minor collection can run alone; a major one always runs right after a minor
one, so it finds the nursery empty.

- **Minor** (`CollectMinor`) — Visits the roots, the temporary roots, every shape's prototype and the remembered cells,
  evacuating each young cell it reaches into old space (`Evacuate` turns the young cell into a forwarding record
  that points at its old copy), then scans the evacuated cells through an explicit gray stack until it drains (a Cheney-style copy, without
  recursion). The VM then relinks its open upvalues, the weak hooks run, and the nursery starts over.
- **Major** (`CollectMajor`) — Marks from the same roots through the gray stack, runs the ephemeron and weak hooks,
  retires the shapes whose prototype died, and sweeps old space (`SweepPages`): unmarked small cells go to their
  class's free list and unmarked large cells are freed. Pages stay allocated for the engine's lifetime.

Collections run only on the engine's own thread, at these points (and where engine code asks for a collection
itself, as the heap snapshot code runs a minor one before it walks the built-ins):

| Trigger | Collection |
|---|---|
| A nursery allocation that does not fit | Minor |
| After that minor collection, old space has grown since the last major one by more than 64 MiB or by its size after that collection, whichever is larger (`GOCCIA_MAJOR_FLOOR_MB` and `GOCCIA_MAJOR_GROWTH_PCT` override the two) | Major as well |
| After that minor collection, old space and the external bytes are over the memory limit (`OverCeiling`) | Major as well |
| After that minor collection, the shapes made since the last major collection outnumber those it left, and at least 65,536 (`ShapesDueMajor`) | Major as well |
| A cell allocated directly in old space that trips the growth or limit test | Minor and major, before it is allocated |
| An explicit reservation that does not fit (`HeapReserve`) | Minor and major, then the test again |
| `Goccia.gc()` | Minor and major |

The roots (`ScanRoots` in `Goccia.VM`) are the value stack up to its top, each frame's closure and `new.target`, the open
upvalues, the global lexical bindings of every realm, each realm's intrinsics, every slot registered with `AddRoot`
or `AddRootRange`, the global object, the pending exception, the current and constructing `new.target`, and whatever
the host's `HostRootScanner` visits. `Goccia.Core` adds the temporary roots natives push (`TempRoots`) and, in a minor
collection, the remembered cells and every shape's prototype.

`GCMinorCount` and `GCMajorCount` count collections; `GCEpochNow` changes at every collection of either kind, so a
native that reads the same epoch before and after a call knows that nothing moved or was freed meanwhile.

### Write Barrier

An old cell that points into the nursery must be in the remembered set, or a minor collection misses the pointer and
the young cell dies or moves under it. `WriteBarrier(Container, V)` remembers `Container` when it is old, not yet
remembered, and `V` points into the nursery. `SetSlot`, `ValueArrayStore`, `AddProperty` and the VM's own stores call
it; a native that writes a field directly (an object's `Aux1` or `Aux2`, an upvalue's value, a value array's data)
calls it after the store. A minor collection scans and then empties the remembered set; a major one empties it too,
since the nursery is empty afterwards.

### Prototypes and Shapes

Shapes live outside the heap and name their prototype. A minor collection keeps every shape's prototype alive and
moves the young ones. A major collection marks a prototype only through the objects whose shapes name it, then
retires the shapes whose prototype nothing reached (`RetireDeadShapes`): their prototype becomes a value nothing
equals, `GCShapeSweepHook` clears the inline caches, constructor caches and compiled code caches that name them, and
their records are freed, except those that machine code compares with. Because shape records are not heap bytes,
old-space growth alone would not start a major collection however many dead prototypes' shapes pile up; the shape
count trigger above covers that. [Engine design — Heap and GC](engine/design.md#heap-and-gc) has the full rules.

### Weak References and Finalization

`Goccia.Weak` implements WeakRef, FinalizationRegistry, WeakMap and WeakSet (ES2026 §9.9, §24.3, §24.4, §26.1, §26.2)
through hooks `Goccia.Core` calls after each collection's marking or evacuation, before old space is swept:

- **WeakMap and WeakSet** keep their entries in an ephemeron table outside the heap, owned by an internal box. After
  marking drains, `GCEphemeronHook` visits the value of every entry whose key is alive in a live table, and flags the
  keys not reached yet (`cfEphKey`); marking notes a flagged cell when it reaches it, and the next call visits what
  waited on it, so a chain of keys is followed in linear time. A minor collection looks only at the entries set with a
  young key or value since the last one.
- **WeakRef and FinalizationRegistry** hold their target in a box field the collector does not trace.
  `GCWeakRefHook` clears a target that died, updates one that moved out of the nursery, and drops table entries whose
  key died. A FinalizationRegistry cell whose target died marks the cleanup pending, and the event loop runs the
  registries' cleanup one job at a time (`RunWeakCleanup`), each followed by the microtasks it queued.
- **Kept objects** — A target `new WeakRef(target)` or `deref()` returned stays alive until the current job ends
  (ES2026 §9.11 AddToKeptObjects); the microtask loop and the host event loop empty the list before each job
  (ES2026 §9.10 ClearKeptObjects).

`GCSurvives` is what a hook asks: whether a cell lives on, updating the pointer when the cell moved. A hook must not
allocate.

## Contributor Rules

These hold for every native, built-in and host routine in `source/engine` and for embedders that touch `TValue`s.

- **A value in a Pascal local is stale after any allocation.** An allocation can run a minor collection, which moves
  every young cell it reaches and leaves the old address pointing at a forwarding record. That includes everything
  that can allocate: `NewString`, `StrFromUnicode`, `NewObject`, `NewPlainObject`, `NewArray`, `AddProperty` (it can
  grow the overflow slots), `ArrayPush`, and every call into JavaScript. Root a value before the allocation with
  `PushRoot`, read it back with `RootAt(TempTop - K)` after, and `PopRoots(N)` on every exit path. `PopRoots` stops the
  process (`RunError(204)`) on more pops than pushes.
- **Re-derive interior pointers too.** A `PObj`, `PStr` or `PValueArray` taken from a value is as stale as the value.
  Take it again from the rooted value after each allocation, as `Goccia.HeapSnapshot`'s reader does with
  `ObjOf(TableAt(I))` before every store.
- **A native's arguments are roots; its local copies are not.** The VM calls a native as
  `function(Args: PValue; Argc: Integer; var Res: TValue): Boolean` with `Args` pointing at value stack slots
  (`Args[0]` the receiver, `Args[1]` to `Args[Argc]` the arguments), which a collection updates. Read `Args[I]` again
  after an allocation instead of keeping it in a local. `Res` is the caller's variable, not a root in general: set it
  after the native's last allocation.
- **Use the store helpers, or call the barrier.** `SetSlot`, `ValueArrayStore` and `AddProperty` apply the write
  barrier. A direct store into a cell field needs `WriteBarrier` afterwards; without it a minor collection frees or
  moves a young value an old cell still holds.
- **State a unit keeps across runs is registered once per engine.** A threadvar or global `TValue` that must outlive
  one native call is registered with `AddRoot` (or `AddRootRange` for consecutive slots), guarded by
  `FirstInEngine`, so that a thread's next engine registers it again (`Goccia.Weak` and `Goccia.HeapSnapshot` do this).
- **Anything held outside the heap by address is updated in a hook.** A table or record that keeps a cell pointer the
  collector does not trace (a weak target, an ephemeron key) must check it with `GCSurvives` in a collection hook,
  or the pointer dangles after the cell moves or dies. Hooks run inside the collection and must not allocate.
- **Stress covers compiled code too.** Under `--gc-stress` the baseline JIT builds array literals through its helper
  and the optimizing tier does not allocate objects inline, so those allocations go through `AllocCell` and its
  collection as well.
- **Keep the stress run green.** A change near allocation, rooting or the barrier runs `scripts/engine/gcstress.sh`
  ([below](#gc-stress-and-verification)); a missed root usually shows there long before the default nursery shows it.

## Memory Ceiling

`HeapCeiling` (per engine thread, in `Goccia.Core`) bounds the heap. It is set from `--max-memory=<bytes>`, the
`max-memory` key of `goccia.json`, or `TGocciaEngine.MaxMemory`; without one it is `DefaultHeapCeiling` (`Goccia.HostAPI`):
half of `GetAvailableMemoryBytes` (`MemoryDetection`: a Linux cgroup's memory limit when one is set, else physical
memory), at most 8 GB on a 64-bit build or 700 MB on a 32-bit one. Where neither can be read, the default is that cap.

What counts against the ceiling is old space (`GCOldBytes`) plus the external bytes charged to it. The shared
counter covers SharedArrayBuffer blocks and a sandbox search's native working memory. Each engine also tracks
its own `THeapExternalCharge` reservations, including retained RegExp programs and matcher storage.

The nursery never holds more than the ceiling leaves free: `NurseryFitCeiling` sets the nursery's bound to the room
between old space plus external bytes and the ceiling, at least 64 KiB (`NURSERY_MIN_ROOM`) and at most the nursery's
size. So the minor collection, and the major collection that tests the limit after it, run before the heap can pass
the ceiling.

### Refusal at the next safe point

The engine's allocator cannot fail: every native and VM operation that allocates assumes it succeeds. The limit therefore
refuses at the next safe point, not at the allocation
([ADR 0185](adr/0185-g2-refuses-memory-at-the-next-safe-point.md)):

1. A major collection that leaves old space and the external bytes over the ceiling calls `GCLimitHook`, which is the
   VM's `MemoryLimitHit`. So does a cell allocated directly in old space when the collection before it could not make
   room. The hook marks a refusal pending, sets the thread's `Interrupt` and drops the frame bound to 0, and the JIT is
   blocked until the limits are disarmed.
2. The next back edge or call sees the interrupt and throws a catchable
   `RangeError: Allocation failed: GC heap size exceeds the configured memory limit`, with the suggestion to call
   `Goccia.gc()` or raise `--max-memory` (`DeliverMemoryRefusal`). Compiled loops poll the same `Interrupt` at every
   back edge. A refusal met at a call with at least 1,024 frames on the stack is the call stack's:
   `RangeError: Maximum call stack size exceeded`, with the stack-memory suggestion.
3. A deadline already passed wins: the timeout aborts the run instead.
4. Each refusal records what its collection left. Eight refusals in a row (`MEM_STRIKES_MAX`), each leaving no less
   than the one before, end the run past every handler with `Live heap of N bytes exceeds the memory limit of M bytes`
   (`AbortKind` 3; GocciaRunner's JSON report names it `MemoryLimitError`). A collection that ends within the limit
   resets the count (`GCLimitClearHook`).

The error therefore lands at a loop head or a call rather than at the allocating expression, and an allocation-heavy
native finishes before the refusal. Making the refusal's own error does not refuse again, and `LimitsDisarm` drops a
refusal the run never reached a safe point for.

### Gated growth points

The engine has no growth gate. Array elements, overflow property slots and string buffers are ordinary heap cells, counted
and collected like any other, and the [refusal above](#refusal-at-the-next-safe-point) is what stops them.

What the engine has instead are **explicit reservations**: before an allocation whose size the script chose and that the
engine can refuse cleanly, a native calls `HeapReserve(Bytes)`. It succeeds when the bytes fit under the ceiling;
otherwise it runs a full collection and tests again. A reservation that still does not fit is a catchable
`RangeError` at once. The callers are `ArrayBuffer` and typed array construction (the bytes, or a resizable buffer's
maximum), `SharedArrayBuffer` blocks (`Goccia.Atomics`), a `Response` body (`Goccia.Fetch`) and the sandbox's file reads and
search memory (`Goccia.Sandbox`). `HeapReserve` collects, so the caller's values must be rooted, as for any allocation.

For storage outside the collector, `THeapExternalCharge.TryGrow` reserves and records bytes against the owning
engine. Release the charge when freeing the storage. This includes failed construction and replacement of a buffer.
RegExp uses preflight checks for compiler and optimizer workspace and keeps charges for the resulting program and
matcher while the compiled pattern is cached. Matcher growth uses the non-collecting check while it reads the
subject through a raw pointer.

**What the ceiling does not bound.** It bounds the collected heap and the charged external bytes, not resident
memory. Shapes, function templates, bytecode, atoms, the WeakMap and WeakSet tables, the parser's AST and the heap's
free space are outside it, and old-space pages are kept until the engine ends, so a heap that shrinks after a
collection keeps its pages for reuse. A host that needs a hard ceiling on the process imposes one outside it, with an
operating-system limit.

### What `bytesAllocated` tracks

`Goccia.gc.bytesAllocated` (`HeapUsedBytes`) is old space plus the nursery's used part: the size-class bytes of every
old cell, whole large cells, and every byte bump-allocated in the nursery since the last minor collection, garbage
included until a collection frees it. It does not include charged external memory, which counts against the ceiling all
the same, nor anything [outside the heap](#heap-layout).

`TGocciaEngine.Memory` reports the same figures to a host (`TGocciaMemoryStats`): `LiveBytes` (`HeapUsedBytes`),
`PeakLiveBytes` (the most the heap held at a collection or a direct old-space allocation, `HeapPeakBytes`),
`AllocatedBytes` (every byte allocated since the engine was made, `HeapAllocatedBytes`), `LimitBytes` and
`Collections`. A CLI's JSON report shows them under `memory.gc` ([Engine README](engine/README.md)).

### Teardown

`EngineDone` (`Goccia.Engine`) frees the thread's engine: `VMDone` releases the VM's stacks and roots, then `HeapDone`
frees every page, large cell, the nursery, the atoms, the key atoms and every shape, without running anything per
cell. Function templates and lazy sources go after it. Nothing from the engine may be used afterwards, and the
thread's next `EngineInit` builds a new heap (`FirstInEngine` tells once-per-engine set-up to run again).

### Threading model

Every piece of engine state, the heap included, is a threadvar, so a thread that calls `EngineInit` has an engine and
a heap of its own, and values never cross threads ([Engine README](engine/README.md)). Each thread has its own `HeapCeiling`:
GocciaTestRunner's workers each set the limit for the file they run, and an agent thread (`Goccia.Agent`) takes its
starter's. The limit is per thread, so N workers may hold up to N times it.

`HeapExternalBytes` is the exception: it is one process-wide counter, updated with interlocked adds, because a
SharedArrayBuffer block is shared between threads and released when its last holder lets go. Every thread's limit
test counts every thread's external bytes.

## JavaScript API

| Property | Type | Description |
|---|---|---|
| `Goccia.gc()` | `function` | A full collection, the nursery first; returns `undefined` |
| `Goccia.gc.bytesAllocated` | `number` | Old space plus the nursery's used part, garbage included (read-only getter) |
| `Goccia.gc.maxBytes` | `number` | The ceiling in force (read-only getter) |
| `Goccia.gc.suggestedMaxBytes` | `number` | The default ceiling, `DefaultHeapCeiling` (read-only getter) |

The functions come from `Goccia.HostAPI`'s natives and the getters from the host prelude (`source/engine/prelude/host.js`).
The ceiling changes only from the host: `--max-memory`, `goccia.json` or `TGocciaEngine.MaxMemory`.
`Goccia.build.gcStress` stays `false`: the engine has no stress build, and `./build.pas --gc-stress` now stops with a pointer to
the run-time flags below.

## GC Stress and Verification

A rooting bug shows only when a collection lands between the allocation and the stale read, which an 8 MiB nursery
makes rare. The engine's binaries make it certain at run time instead of in a separate build. The flags belong to the `goccia`
command-line host (`source/engine/goccia.dpr`, built by `./build.pas engine` into `build/engine/goccia`); GocciaRunner and the other
applications do not take them.

| Setting | Effect |
|---|---|
| `--gc-stress` | Every nursery allocation runs a minor collection, and every eighth minor collection a major one too (`GCStress`) |
| `--gc-verify` | Checks every root for a valid pointer as each collection scans it, that every old cell pointing into the nursery is remembered before each minor collection, and after each collection that no live cell points into the nursery or at a freed cell; after a minor collection it also fills the emptied nursery with `$DB`, so a pointer that was not updated meets a header no cell has (`GCVerify`). A failure prints the cell and field to stderr and exits with status 9 |
| `GOCCIA_NURSERY_KB=<n>` | A nursery of n KiB, so minor collections come every few kilobytes. `goccia` applies it to a script's heap and to the engine `goccia --test` makes; `GOCCIA_NURSERY_MB` sets it in MiB for a script. The test262 host (`Goccia.Test262`) reads it too, with `GOCCIA_GC_VERIFY` for `--gc-verify` |
| `GOCCIA_HOST=1` | Installs the hidden `GocciaHost` object, whose `gc()` collects at once, for the regression scripts |

`scripts/engine/gcstress.sh [KB]` runs the full stress pass and writes `build/engine/gcstress.txt`, printing the count of
`OK` lines and every `BAD` or `TIMEOUT` line:

1. Every Web Tooling digest bundle (`GOCCIA_WT_DIR`) and JetStream bundle (`GOCCIA_JS_DIR`) with `--gc-verify` under a nursery
   of KB KiB (default 256), plus the AWFY bundles (`GOCCIA_AWFY_DIR`, optional) without `--gc-verify`, whose heap walks
   would take hours. A Web Tooling bundle passes when its `DIGEST` line matches `scripts/engine/digests.txt`.
2. Each script in `scripts/engine/regress` with `--gc-verify` under 16, 64 and 128 KiB nurseries, with `GOCCIA_HOST=1`; a first
   line `// goccia-flags: ...` adds flags (`--gc-stress` among them). Each prints `ok` last.
3. `scripts/engine/gcstress-tests.py`, which runs every test file under `tests/` through `goccia --test` twice: as is, and
   with a 16 KiB nursery and `--gc-verify`. A file whose totals differ, or whose stressed run crashes, is `BAD`; a
   stressed run past 600 seconds is `TIMEOUT`, not a failure (the deep-chain tests under `tests/built-ins/Goccia/gc`
   verify a large heap at every collection).

To reproduce one finding:

```bash
./build.pas engine
GOCCIA_HOST=1 GOCCIA_NURSERY_KB=16 ./build/engine/goccia --gc-verify scripts/engine/regress/closure-alloc.js
GOCCIA_NURSERY_KB=16 ./build/engine/goccia --gc-verify --test tests/built-ins/Goccia/gc/deep-object-chains.js
```

The gate that runs this, and when to run it, is described in [Engine README — Gate](engine/README.md#gate).

## Related Documents

- [Engine design](engine/design.md) — values, cells, the heap and shapes
- [Engine README](engine/README.md) — limits, reports and the gate
- [ADR 0185](adr/0185-g2-refuses-memory-at-the-next-safe-point.md) — the memory limit's refusal at the next safe point
- [Heap Snapshots](heap-snapshots.md) — writing what a value reaches and restoring it in a fresh engine
- [Embedding](embedding.md) — `TGocciaEngine` and its limits
