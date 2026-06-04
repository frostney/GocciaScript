# Garbage Collector

*Unified mark-and-sweep garbage collector shared by both the interpreter and bytecode VM.*

## Executive Summary

- **Mark-and-sweep** — Two-phase tracing GC (`Goccia.GarbageCollector.pas`) shared by both execution modes
- **Auto-registration** — Every `TGocciaValue` registers with the GC via `AfterConstruction`; subclasses override `MarkReferences` to mark owned references
- **Weak reference phase** — WeakMap, WeakSet, WeakRef, and FinalizationRegistry use post-mark weak tracing/sweeping hooks so weak targets do not become strong roots
- **Kept objects** — WeakRef construction and `deref()` keep targets stable until the next host job checkpoint
- **Finalization cleanup** — FinalizationRegistry cleanup jobs are enqueued by GC and run after the normal microtask queue
- **Generation-counter marking** — O(1) mark-clear via `AdvanceMark` instead of O(n) flag reset per collection; explicit collections are serialized across worker threads because some intrinsic objects are shared
- **Pinned singletons** — `undefined`, `null`, `true`, `false`, `NaN`, `Infinity` are pinned once at engine startup; built-in prototypes are pinned per-engine via [realm slots](core-patterns.md#realm-ownership--slot-registration) and released atomically when the realm is destroyed
- **Adaptive threshold** — Collection frequency scales with surviving object count to amortize cost on large heaps

## Value Integration

Every `TGocciaValue` participates in the garbage collector:

```pascal
var
  GCCurrentMark: Cardinal;  // Shared generation counter

TGCManagedObject = class
private
  FGCMark: Cardinal;           // Per-object mark — matches GCCurrentMark when alive
  FGCIndex: Integer;           // Index in FManagedObjects for O(1) unregistration
public
  procedure BeforeDestruction; override;
  class procedure AdvanceMark; static; inline;
  procedure MarkReferences; virtual;
  function TraceWeakReferences: Boolean; virtual;
  procedure SweepWeakReferences; virtual;
  property GCMarked: Boolean read GetGCMarked write SetGCMarked;
  property GCIndex: Integer read FGCIndex write FGCIndex;
end;

TGocciaValue = class(TGCManagedObject)
  procedure AfterConstruction; override;  // Auto-registers with GC
  function RuntimeCopy: TGocciaValue; virtual;  // Create a GC-managed copy
end;
```

- **`AfterConstruction` / `BeforeDestruction`** — Every value auto-registers with the thread-local `TGarbageCollector.Instance` upon creation and unregisters before destruction so root sets cannot retain stale object pointers.
- **`MarkReferences`** — Base implementation sets `FGCMark := GCCurrentMark` (marking the object as alive for the current collection). `AdvanceMark` increments the shared `GCCurrentMark` while the collector lock is held, and `TGarbageCollector.Instance` uses that mark while traversing objects. Subclasses override `MarkReferences` to also mark values they reference (e.g., `TGocciaObjectValue` marks its prototype and property values, `TGocciaFunctionValue` marks its closure scope, `TGocciaArrayValue` marks its elements). The `if GCMarked then Exit;` guard at the top of each override prevents re-visiting objects in cyclic reference graphs.
- **`TraceWeakReferences` / `SweepWeakReferences`** — Optional hooks for weak containers and weak references. The default implementations do nothing. WeakMap uses `TraceWeakReferences` as an ephemeron pass: if a key is already marked by normal roots, its value is marked, but the key is never marked by the map. WeakMap and WeakSet use `SweepWeakReferences` to remove entries whose keys/values remain unmarked. WeakRef clears an unmarked target, and FinalizationRegistry removes dead cells while enqueueing cleanup jobs for their held values.
- **`RuntimeCopy`** — Creates a fresh GC-managed copy of the value. Used by the evaluator when evaluating literal expressions: AST-owned literal values are not tracked by the GC, so `RuntimeCopy` produces a runtime value that is. The default implementation returns `Self` (for singletons and complex values). Primitives override this: numbers use the `SmallInt` cache for 0-255, booleans return singletons, strings create new instances (cheap due to copy-on-write).

## Contributor Rules

When working with the GC, follow these rules:

- **Override `MarkReferences`** in every value type that holds `TGocciaValue` references. Call `inherited` first, then mark each owned reference.
- **Pin singletons** — `UndefinedValue`, `TrueValue`, `NaNValue`, etc. are pinned via `PinObject` during engine initialization (consolidated in `PinPrimitiveSingletons`). Built-in prototype singletons are stored per-engine in realm slots (see next bullet); the realm pins them on `SetSlot` and releases them on `Destroy`, so contributors do not need to call `PinObject` from `InitializePrototype` themselves.
- **Realm-owned pinning** — Built-in prototypes are stored in per-engine [realm slots](core-patterns.md#realm-ownership--slot-registration). `TGocciaRealm.SetSlot` pins the stored object via `PinObject`; the realm tracks every pin it took and releases all of them in `Destroy` via `UnpinObject`. Owned-slot helpers (`TGocciaSharedPrototype` instances) are `Free`d before the pin-release pass, so their destructors can still call `UnpinObject` on objects they own. This means engine tear-down releases the entire intrinsic prototype graph atomically — embedders should not pin or unpin built-in prototypes manually.
- **Protect stack-held values** — Values held only by Pascal code (not in any GocciaScript scope) must be protected. Use `TGocciaActiveRootFrame` for stack-local groups of roots, especially nested evaluator paths where the same object may be rooted more than once; use `AddTempRoot`/`RemoveTempRoot` for simple one-off temporary ownership.
- **Use `CollectIfNeeded(AProtect)` or `CollectForMemoryPressure(AProtect)`** when holding a `TGCManagedObject` on the stack. The no-arg `CollectIfNeeded` is only safe when all live values are already rooted.
- **Weak containers and weak references must not mark weak targets during `MarkReferences`**. Put weak-value propagation in `TraceWeakReferences` and dead-target pruning or cleanup scheduling in `SweepWeakReferences`; otherwise weak semantics collapse into strong references.
- **Queued jobs must root their callback payloads**. Promise reactions, `queueMicrotask` callbacks, and FinalizationRegistry cleanup jobs use queued roots so callback functions, held values, and result promises survive collections until the job runs.
- **Clear kept objects at host job boundaries**. Engine idle checkpoints and the shared microtask/fetch drain helper clear the kept-objects set before and after draining; individual microtask/finalization jobs clear it after they complete.
- **Scopes** register with the GC in their constructor and unregister through `BeforeDestruction`. Active call scopes are tracked via `PushActiveRoot`/`PopActiveRoot`.
- **VM register rooting** uses a bytecode VM stack root and only traverses object-bearing register slots.
- Automatic collection is disabled during bytecode execution. CLI hosts may still call `Collect` explicitly between files; the benchmark runner does this after each benchmark file, while parallel test workers reclaim their thread-local GC heap at worker shutdown. Explicit `Goccia.gc()` is still available in worker threads and is serialized by the collector lock.

## Design Rationale

### Why Not Manual Memory Management?

- **Aliased references** — A value assigned to multiple variables, captured in a closure, and stored in an array has no single owner. Determining when to free it requires tracking all references.
- **Shared prototype singletons** — String, Number, Array, Set, Map, Function, Symbol, and other built-in prototype objects are per-engine singletons stored in [realm slots](core-patterns.md#realm-ownership--slot-registration) and shared across all instances of their type within the same engine. Each type's `InitializePrototype` creates the singleton once (guarded by checking the realm slot) and stores it via `TGocciaRealm.SetSlot` / `SetOwnedSlot`, which pins it with the GC. Manual lifetime tracking would be fragile; the realm releases all of its pins atomically in `Destroy`.
- **Closure captures** — Arrow functions capture their enclosing scope, creating non-obvious reference chains between scopes and values.

### Why Not Reference Counting?

`TGocciaValue` inherits from `TGCManagedObject`, which is a plain `TObject` descendant — there is no reference counting. Values are stored as class references (`TGocciaValue`), and lifetime is managed entirely by the mark-and-sweep GC. Using interface-based reference counting would require a large-scale refactor and introduce circular reference issues (objects referencing their prototypes and vice versa).

### Why Mark-and-Sweep?

- **Simplicity** — Two phases (mark reachable, sweep unreachable) with straightforward implementation.
- **Handles cycles** — Circular references between objects, closures, and scopes are collected correctly.
- **O(1) membership checks** — Pinned objects, temp roots, and root objects are stored in `THashMap<TGCManagedObject, Boolean>` (`TGCObjectSet`) for O(1) `PinObject`, `AddRootObject`, `AddTempRoot`, and `RemoveTempRoot` operations, avoiding O(n) linear scans on every allocation.
- **Generation-counter mark tracking** — Instead of clearing the `GCMarked` flag on every object at the start of each collection (an O(n) pass), the GC uses a generation counter. `AdvanceMark` increments the counter in O(1), and an object is considered "marked" when its `FGCMark` matches the current generation. This eliminates a full pass over the managed objects list per collection. The counter is shared across threads, and full/young collection holds a global collector lock so shared intrinsic objects cannot race on mark state.
- **O(1) `UnregisterObject`** — Each managed object stores its index in the managed objects list (`GCIndex`). Unregistration nils the slot at the known index instead of performing an O(n) linear scan. The sweep phase compacts nil slots during its existing pass.
- **Adaptive threshold** — After each collection, the threshold scales to `max(DEFAULT_GC_THRESHOLD, surviving_count)`, so large heaps collect proportionally less often, amortizing collection cost to O(1) per allocation.
- **Weak fixed-point tracing** — After normal root marking, the collector repeatedly visits marked objects' weak hooks until no hook marks anything new. This handles ephemeron chains such as a live WeakMap key exposing a value that then keeps another WeakMap key alive. After the fixed point, marked weak containers sweep entries whose weak keys or values are still unmarked, WeakRefs clear dead targets, FinalizationRegistries enqueue cleanup jobs for dead cells, and then the normal object sweep frees unreachable objects.
- **`Recycle` virtual method** — Sweep calls `Obj.Recycle` instead of `Obj.Free`. The default calls `Free`, but subclasses can override to return objects to a pool.
- **Measurable impact** — Both the GocciaBenchmarkRunner and GocciaTestRunner call `Collect` after each file to reclaim memory between script executions.

## Memory Ceiling

The GC tracks approximate heap usage via `InstanceSize` per registered object. A byte ceiling is always active:

- **Default:** half of physical memory, capped at 8 GB on 64-bit or 700 MB on 32-bit. Falls back to 512 MB when OS detection fails.
- **Override:** `--max-memory=<bytes>` sets an explicit limit.

Any allocation that pushes `BytesAllocated` above `MaxBytes` raises a JavaScript `RangeError`. The error is catchable with `try/catch`; after catching, the script can call `Goccia.gc()` to free unreachable objects and retry.

The interpreter also has safe checkpoints that call `CollectForMemoryPressure` as live bytes approach the ceiling. These checkpoints protect the current expression result and active Pascal-local temporaries, allowing transient-heavy programs to reclaim unreachable values before the hard allocation guard fires. If a collection cannot bring usage below the ceiling, the next allocation still raises `RangeError`.

From JavaScript, `Goccia.gc.bytesAllocated` and `Goccia.gc.maxBytes` are read-only getters. The ceiling can only be changed from the engine level (CLI option or Pascal API: `TGarbageCollector.Instance.MaxBytes`).

### Physical memory detection

| Platform | API | Notes |
|----------|-----|-------|
| macOS/Darwin | `sysconf(_SC_PHYS_PAGES) * sysconf(_SC_PAGESIZE)` | Declared as `external 'c'` inline |
| Linux | `sysconf(_SC_PHYS_PAGES) * sysconf(_SC_PAGESIZE)` | Same API, different constant values |
| Windows | `GlobalMemoryStatusEx` (kernel32.dll) | Declared inline because the standard FPC 3.2.2 `Windows` unit only provides the older `GlobalMemoryStatus`, which [Microsoft documents](https://learn.microsoft.com/en-us/windows/win32/api/winbase/nf-winbase-globalmemorystatus) as capping `dwTotalPhys` at 2 GB on x86 systems with 2-4 GB of RAM. `GlobalMemoryStatusEx` uses 64-bit `DWORDLONG` fields (`ullTotalPhys`) that report correctly on all systems. |

### Scaling constants

| Constant | Value | Purpose |
|----------|-------|---------|
| `DEFAULT_MAX_BYTES` | 512 MB | Fallback when OS memory detection fails |
| `MAX_BYTES_CAP_64BIT` | 8 GB | Upper bound on 64-bit targets |
| `MAX_BYTES_CAP_32BIT` | 700 MB | Upper bound on 32-bit targets |

The formula is `min(physicalMemory / 2, platformCap)`.

### What `BytesAllocated` tracks

`BytesAllocated` sums `InstanceSize` of each `TGCManagedObject` registered with the GC. This covers the Delphi/FPC object instance (vtable, fields, padding) but **not** backing storage allocated separately by the object (e.g., dynamic array buffers in `TGocciaArrayValue`, string heap allocations in `TGocciaStringLiteralValue`). The ceiling is therefore an approximate safety net, not a precise memory accounting system.

### Threading model

Each worker thread creates its own `TGarbageCollector` instance via `threadvar`. The `--max-memory` ceiling is propagated from the main thread's GC to each worker via `TGocciaThreadPool.MaxBytes` → `InitThreadRuntime(AMaxBytes)`.

Key behavior on worker threads:

- **Automatic GC collection is disabled** (`Enabled := False`) so worker execution does not collect between ordinary allocations. Explicit `Collect` calls still run under the global collector lock; `Goccia.gc()` therefore has the same observable behavior in worker threads as on the main thread. `GocciaTestRunner` still lets worker shutdown reclaim each thread-local GC heap instead of collecting after every file.
- **`BytesAllocated` still increments** on every allocation, even with automatic collection disabled. Without explicit host collection, the counter grows across all files a worker processes.
- **The memory ceiling check still fires.** The limit check in `TGocciaValue.AfterConstruction` does not depend on `GC.Enabled` — it checks `MaxBytes > 0` and `BytesAllocated > MaxBytes` regardless. This is the sole protection against unbounded memory growth on workers.
- **No pre-allocation.** `MaxBytes` is a threshold, not a reservation. Memory is allocated on demand by the FPC heap manager; the GC only checks whether the running total exceeds the ceiling.
- **Each worker gets the same ceiling as the main thread.** The limit is per-thread, not divided across workers. With N workers, the theoretical maximum total allocation is `N × MaxBytes`, though in practice worker allocations are far below the ceiling.

CLI JSON reports aggregate worker GC memory once per worker thread. Live and peak values are summed across worker thread-local GC instances, while the limit is the per-worker ceiling. The report deliberately avoids summing per-file live snapshots, because a worker may process many files using the same GC instance.

The separate `memory.heap` JSON object comes from FreePascal's `GetHeapStatus`, not from the GocciaScript GC. It describes allocator state for the process/thread scope being measured. Free-space deltas can be negative when the allocator has less reusable free space at the end of a run; this is not itself evidence of a GocciaScript GC leak.

## JavaScript API

`Goccia.gc()` manually triggers a full mark-and-sweep collection, bypassing the automatic collection threshold. Active interpreter calls and bytecode VM registers are treated as roots while collection runs. Collections are serialized by a global collector lock so explicit calls are safe in parallel test workers even though intrinsic prototype objects can be shared. It is safe to call repeatedly and returns `undefined`.

| Property | Type | Description |
|----------|------|-------------|
| `Goccia.gc()` | `function` | Force a full garbage collection |
| `Goccia.gc.bytesAllocated` | `number` | Approximate bytes currently tracked by the GC (read-only) |
| `Goccia.gc.maxBytes` | `number` | Active byte ceiling (read-only; set via `--max-memory` or auto-detected from OS memory) |

## AST Literal Ownership

The parser creates `TGocciaValue` instances (numbers, strings, booleans) and stores them inside `TGocciaLiteralExpression` AST nodes. These values are owned by the AST, not the GC. `TGocciaLiteralExpression.Create` calls `TGarbageCollector.Instance.UnregisterObject` to remove the value from GC tracking, and `TGocciaLiteralExpression.Destroy` frees the value (unless it is a singleton like `UndefinedValue`, `TrueValue`, or `FalseValue`).

When the evaluator encounters a literal expression, it calls `Value.RuntimeCopy` to produce a fresh GC-managed runtime value. This cleanly separates compile-time constants (owned by the AST) from runtime values (managed by the GC). The overhead is minimal: integers 0-255 hit the `SmallInt` cache (zero allocation), booleans return singletons, and strings benefit from FreePascal's copy-on-write semantics.

## Related Documents

- [Value System](value-system.md) — `TGocciaValue` hierarchy and property access
- [Core patterns](core-patterns.md) — Singleton pattern, shared prototypes
- [Architecture](architecture.md) — GC layer in the main layers table
