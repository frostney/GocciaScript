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
- **Two refusal contracts** — the `--max-memory` ceiling refuses a *charged* allocation with a catchable `RangeError`, after forcing a collection and re-testing, and a [gated growth point](#gated-growth-points) with the script-opaque `MemoryLimitError`, without collecting at all

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
- **`RuntimeCopy`** — Creates a fresh GC-managed copy of the value. Used by the evaluator when evaluating literal expressions: AST-owned literal values are not tracked by the GC, so `RuntimeCopy` produces a runtime value that is. The default implementation returns `Self` (for singletons and complex values). Primitives override this: numbers reuse the special-value singletons (`0`, `1`, `NaN`, `±Infinity`, `-0`) and otherwise create a fresh instance, booleans return singletons, strings create new instances (cheap due to copy-on-write).

## Contributor Rules

When working with the GC, follow these rules:

- **Override `MarkReferences`** in every value type that holds `TGocciaValue` references. Call `inherited` first, then mark each owned reference. This includes host objects a builtin defines for its own bookkeeping — a matcher, a per-call state record, a handler cell — whenever they stash a value in a native field instead of a property. The inherited walk only sees properties, so an unmarked native field is invisible to the collector even while the object holding it is plainly alive, and the value dies mid-operation as soon as the builtin re-enters user code.
- **Pin singletons** — `UndefinedValue`, `TrueValue`, `NaNValue`, etc. are pinned via `PinObject` during engine initialization (consolidated in `PinPrimitiveSingletons`). Built-in prototype singletons are stored per-engine in realm slots (see next bullet); the realm pins them on `SetSlot` and releases them on `Destroy`, so contributors do not need to call `PinObject` from `InitializePrototype` themselves.
- **Realm-owned pinning** — Built-in prototypes are stored in per-engine [realm slots](core-patterns.md#realm-ownership--slot-registration). `TGocciaRealm.SetSlot` pins the stored object via `PinObject`; the realm tracks every pin it took and releases all of them in `Destroy` via `UnpinObject`. Owned-slot helpers (`TGocciaSharedPrototype` instances) are `Free`d before the pin-release pass, so their destructors can still call `UnpinObject` on objects they own. This means engine tear-down releases the entire intrinsic prototype graph atomically — embedders should not pin or unpin built-in prototypes manually.
- **Protect stack-held values** — Values held only by Pascal code (not in any GocciaScript scope) must be protected. Use `TGocciaActiveRootFrame` for stack-local groups of roots, especially nested evaluator paths where the same object may be rooted more than once; use `AddTempRoot`/`RemoveTempRoot` for simple one-off temporary ownership.
- **Use `CollectIfNeeded(AProtect)` or `CollectForMemoryPressure(AProtect)`** when holding a `TGCManagedObject` on the stack. The no-arg `CollectIfNeeded` is only safe when all live values are already rooted.
- **A live `TGocciaArgumentsCollection` roots its elements.** It descends from `TGCRootSource`, registers with the thread-local collector in `AfterConstruction`, unregisters in `BeforeDestruction`, and `MarkRoots` marks every registered source. Builtins therefore do not root their own `AArgs` elements. The contract stops at the container: anything a builtin copies into its own native state — a path intermediate, a traversal stack, a partially built result — is still unrooted across any call that can run user code, and still needs `AddTempRootIfNeeded` or `TGocciaActiveRootFrame`. A subclass that hands out values not present in `FArgs` must override `MarkRootReferences` to mark them (see `Goccia.Arguments.Callbacks`). See [ADR 0105](adr/0105-argument-collections-root-their-elements.md).
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
- **Weak fixed-point tracing** — After normal root marking, the collector repeatedly visits marked objects' weak hooks until no hook marks anything new. This handles ephemeron chains such as a live WeakMap key exposing a value that then keeps another WeakMap key alive. After the fixed point, marked weak containers sweep entries whose weak keys or values are still unmarked, WeakRefs clear dead targets, FinalizationRegistries enqueue cleanup jobs for dead cells, and then the normal object sweep frees unreachable objects. Because the weak hooks are inert on every object except a live weak container, the collector tracks live weak containers and skips both weak passes entirely while none are tracked — the common case for code that uses no weak collections. A container joins the set on its **first** weak-data insertion and leaves only when it is itself collected; it is *not* removed when it merely becomes empty again (via `delete`, `unregister`, or having its weak keys/targets swept). So the set means "live containers that have ever held weak data": an emptied-but-still-live container keeps both full-heap passes running. That residual cost is bounded — such code already opted into weak semantics — and removing it would mean iterating the tracked set itself (O(live weak containers)) rather than the whole managed-objects list.
- **`Recycle` virtual method** — Sweep calls `Obj.Recycle` instead of `Obj.Free`. The default calls `Free`, but subclasses can override to return objects to a pool.
- **Measurable impact** — Both the GocciaBenchmarkRunner and GocciaTestRunner call `Collect` after each file to reclaim memory between script executions.

## Memory Ceiling

The GC tracks approximate heap usage via `InstanceSize` per registered object. A byte ceiling is always active:

- **Default:** half of physical memory, capped at 8 GB on 64-bit or 700 MB on 32-bit. Falls back to 512 MB when OS detection fails.
- **Override:** `--max-memory=<bytes>` sets an explicit limit.

An allocation that pushes `BytesAllocated` above `MaxBytes` is refused, and *how* it is refused depends on which path it took. A **charged** allocation — one with an owner that can release it again — raises a JavaScript `RangeError`. That error is catchable with `try/catch`; after catching, the script can call `Goccia.gc()` to free unreachable objects and retry. A **gated** growth point ([below](#gated-growth-points)) instead refuses without collecting at all and raises `TGocciaMemoryLimitError`, which is opaque to the script and ends the run. The rest of this section describes the charged path; the gate's contract is stated where it is defined.

The interpreter also has safe checkpoints that call `CollectForMemoryPressure` as live bytes approach the ceiling. These checkpoints protect the current expression result and active Pascal-local temporaries, allowing transient-heavy programs to reclaim unreachable values before the hard allocation guard fires. If a collection cannot bring usage below the ceiling, the next allocation still raises `RangeError`.

Charged reservations do not depend on those checkpoints having fired. When `TryReserveExternalBytes` finds that a request would push `BytesAllocated` past the ceiling, it forces a collection — the pressure heuristic alone would decline, because it only triggers once the live set is within a fixed reserve (`MaxBytes / 8`, clamped to 16 KiB…16 MiB) of the ceiling — and then re-tests the request once. A reservation is therefore refused only when the post-collection heap genuinely cannot fit it. The forcing does not apply while a collection is already running or while the memory limit is firing, since both mean the caller is already inside the collector's own re-entrant path.

Two shapes are still refused without walking the heap, because for them no collection could change the answer: a request larger than `MaxBytes` itself, and a repeat of a request a forced collection has already refused at the current live level. The second is what keeps a script that catches the `RangeError` and retries at constant cost per attempt rather than paying a full mark-and-sweep each time; the record is per request size, so a smaller request the record does not rule out still forces its collection. The record is cleared by any collection — threshold, young-generation, pressure checkpoint, or an explicit `Goccia.gc()` — and by the heap dropping below the recorded level, not by allocation as such; between collections, a retry of the refused size keeps the recorded answer.

The [gated growth points](#gated-growth-points) below behave differently on purpose, and the difference is observable: the gate refuses without ever collecting, and its `TGocciaMemoryLimitError` is opaque to the script rather than catchable. The same byte count can therefore succeed on a charged path — a string payload, an `ArrayBuffer` — while ending the run on the element-storage or property-storage path, because only the charged path has an owner whose release makes a collection worth attempting.

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

`BytesAllocated` sums `InstanceSize` of each `TGCManagedObject` registered with the GC. This covers the Delphi/FPC object instance (vtable, fields, padding). Backing storage allocated separately by an object splits into two cases. Storage with a clear owner and a release hook is **charged** to `BytesAllocated` through the `TryReserveExternalBytes` contract — string payloads (`TGocciaStringLiteralValue`) and `ArrayBuffer`/`SharedArrayBuffer` backing stores reserve on allocation and release when the owner is destroyed; `ArrayBuffer`/`SharedArrayBuffer` additionally release the freed bytes when a resize shrinks the backing store (`SetData`/`SetDataLength` release `-Delta` whenever `Delta < 0`), so the charge tracks the live payload rather than the peak. Either way they do count. Storage that is only **gated** (checked before allocation, never charged) does not — dynamic array element buffers in `TGocciaArrayValue` are bounded per-allocation by the growth gate below, not summed into `BytesAllocated`. The ceiling is therefore an approximate safety net, not a precise memory accounting system.

### Gated growth points

Backing storage sized by the running script is checked against the ceiling *before* it is allocated, without being charged to it (`Goccia.MemoryLimit`: `CanAllocateNativeBytes` / `RequireNativeBytes`, raising the host-catchable `TGocciaMemoryLimitError`). Two growth points are gated: array element extension (`ExtendElementsWithHoles`) and object property storage, where the map's entry and bucket arrays grow past a small-block threshold. Both belong to containers with no hook to release a reservation, so a charge would leak budget the engine could never give back; a gate bounds the peak instead. What is reported to the gate is the *transient* footprint — the block being allocated plus the block still live while it is — because `SetLength` may allocate and copy rather than extend in place, and compaction holds both entry arrays at once by construction.

**What the gate does not bound.** It bounds the size of any one gated allocation; it does not bound resident memory. Two limits stack up. Property storage below `GATED_GROWTH_MIN_BYTES` is not reported at all, so an object that never exceeds 62 properties (64-bit, `SizeOf(TEntry) = 24`) is never checked. More importantly, `BytesAllocated` omits the dominant per-property cost — `TGocciaPropertyDescriptor` is a plain class, not a `TGCManagedObject`, and the shape's property-key strings are likewise unmanaged — so the remaining budget the gate compares against barely moves as properties accumulate, and no number of these uncharged small objects ever sums to a refusal. (Charged allocations — string payloads, `ArrayBuffer` backing stores — still count against `BytesAllocated` and can exhaust it; the aggregate gap is specific to the uncharged descriptors and keys, and closing it would narrow the gap rather than turn the budget into a resident-memory ceiling.) Measured at a 64 MiB budget: 40,000 objects of 120 properties each completes at 978 MB resident, while the same total properties on a *single* object is refused at 324 MB. [ADR 0106, Amendment 1](adr/0106-sandbox-hardening-scope.md#amendment-1--the-aggregate-caveat-measured) records the full measurements. A host that needs a real ceiling must impose one outside the process.

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

When the evaluator encounters a literal expression, it calls `Value.RuntimeCopy` to produce a fresh GC-managed runtime value. This cleanly separates compile-time constants (owned by the AST) from runtime values (managed by the GC). The overhead is minimal: `0`, `1`, and the special values (`NaN`, `±Infinity`, `-0`) reuse singletons (zero allocation), other numbers allocate cheaply, booleans return singletons, and strings benefit from FreePascal's copy-on-write semantics.

## Related Documents

- [Value System](value-system.md) — `TGocciaValue` hierarchy and property access
- [Core patterns](core-patterns.md) — Singleton pattern, shared prototypes
- [Architecture](architecture.md) — GC layer in the main layers table
