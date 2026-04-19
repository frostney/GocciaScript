# Garbage Collector

*Unified mark-and-sweep garbage collector shared by both the interpreter and bytecode VM.*

## Executive Summary

- **Mark-and-sweep** — Two-phase tracing GC (`Goccia.GarbageCollector.pas`) shared by both execution backends
- **Auto-registration** — Every `TGocciaValue` registers with the GC via `AfterConstruction`; subclasses override `MarkReferences` to mark owned references
- **Generation-counter marking** — O(1) mark-clear via `AdvanceMark` instead of O(n) flag reset per collection
- **Pinned singletons** — `undefined`, `null`, `true`, `false`, `NaN`, `Infinity`, and shared prototype objects are pinned during engine startup
- **Adaptive threshold** — Collection frequency scales with surviving object count to amortize cost on large heaps

## Value Integration

Every `TGocciaValue` participates in the garbage collector:

```pascal
TGCManagedObject = class
private class var
  FCurrentMark: Cardinal;      // Generation counter — incremented each collection
private
  FGCMark: Cardinal;           // Per-object mark — matches FCurrentMark when alive
  FGCIndex: Integer;           // Index in FManagedObjects for O(1) unregistration
public
  class procedure AdvanceMark; static; inline;
  procedure MarkReferences; virtual;
  property GCMarked: Boolean read GetGCMarked write SetGCMarked;
  property GCIndex: Integer read FGCIndex write FGCIndex;
end;

TGocciaValue = class(TGCManagedObject)
  procedure AfterConstruction; override;  // Auto-registers with GC
  function RuntimeCopy: TGocciaValue; virtual;  // Create a GC-managed copy
end;
```

- **`AfterConstruction`** — Every value auto-registers with `TGarbageCollector.Instance` upon creation.
- **`MarkReferences`** — Base implementation sets `FGCMark := FCurrentMark` (marking the object as alive for the current collection). Subclasses override this to also mark values they reference (e.g., `TGocciaObjectValue` marks its prototype and property values, `TGocciaFunctionValue` marks its closure scope, `TGocciaArrayValue` marks its elements). The `if GCMarked then Exit;` guard at the top of each override prevents re-visiting objects in cyclic reference graphs.
- **`RuntimeCopy`** — Creates a fresh GC-managed copy of the value. Used by the evaluator when evaluating literal expressions: AST-owned literal values are not tracked by the GC, so `RuntimeCopy` produces a runtime value that is. The default implementation returns `Self` (for singletons and complex values). Primitives override this: numbers use the `SmallInt` cache for 0-255, booleans return singletons, strings create new instances (cheap due to copy-on-write).

## Contributor Rules

When working with the GC, follow these rules:

- **Override `MarkReferences`** in every value type that holds `TGocciaValue` references. Call `inherited` first, then mark each owned reference.
- **Pin singletons** — `UndefinedValue`, `TrueValue`, `NaNValue`, etc. are pinned via `PinObject` during engine initialization (consolidated in `PinPrimitiveSingletons`). Shared prototype singletons are pinned automatically by `TGocciaSharedPrototype.Create`.
- **Protect stack-held values** — Values held only by Pascal code (not in any GocciaScript scope) must be protected with `AddTempRoot`/`RemoveTempRoot`.
- **Use `CollectIfNeeded(AProtect)`** when holding a `TGCManagedObject` on the stack. The no-arg `CollectIfNeeded` is only safe when all live values are already rooted.
- **Scopes** register/unregister with the GC in their constructor/destructor. Active call scopes are tracked via `PushActiveRoot`/`PopActiveRoot`.
- **VM register rooting** only traverses object-bearing register slots.
- Automatic collection is disabled during bytecode execution; GocciaTestRunner and GocciaBenchmarkRunner call `Collect` after each file.

## Design Rationale

### Why Not Manual Memory Management?

- **Aliased references** — A value assigned to multiple variables, captured in a closure, and stored in an array has no single owner. Determining when to free it requires tracking all references.
- **Shared prototype singletons** — String, Number, Array, Set, Map, Function, and Symbol prototype objects are class-level singletons shared across all instances of their type. Each type's `InitializePrototype` creates the singleton once (guarded by `if Assigned`) and pins it with `TGarbageCollector.Instance.PinObject`. Manual lifetime tracking of these shared singletons would be fragile.
- **Closure captures** — Arrow functions capture their enclosing scope, creating non-obvious reference chains between scopes and values.

### Why Not Reference Counting?

`TGocciaValue` inherits from `TGCManagedObject`, which is a plain `TObject` descendant — there is no reference counting. Values are stored as class references (`TGocciaValue`), and lifetime is managed entirely by the mark-and-sweep GC. Using interface-based reference counting would require a large-scale refactor and introduce circular reference issues (objects referencing their prototypes and vice versa).

### Why Mark-and-Sweep?

- **Simplicity** — Two phases (mark reachable, sweep unreachable) with straightforward implementation.
- **Handles cycles** — Circular references between objects, closures, and scopes are collected correctly.
- **O(1) membership checks** — Pinned objects, temp roots, and root objects are stored in `THashMap<TGCManagedObject, Boolean>` (`TGCObjectSet`) for O(1) `PinObject`, `AddRootObject`, `AddTempRoot`, and `RemoveTempRoot` operations, avoiding O(n) linear scans on every allocation.
- **Generation-counter mark tracking** — Instead of clearing the `GCMarked` flag on every object at the start of each collection (an O(n) pass), the GC uses a generation counter (`TGCManagedObject.FCurrentMark`). `AdvanceMark` increments the counter in O(1), and an object is considered "marked" when its `FGCMark` matches `FCurrentMark`. This eliminates a full pass over the managed objects list per collection.
- **O(1) `UnregisterObject`** — Each managed object stores its index in the managed objects list (`GCIndex`). Unregistration nils the slot at the known index instead of performing an O(n) linear scan. The sweep phase compacts nil slots during its existing pass.
- **Adaptive threshold** — After each collection, the threshold scales to `max(DEFAULT_GC_THRESHOLD, surviving_count)`, so large heaps collect proportionally less often, amortizing collection cost to O(1) per allocation.
- **`Recycle` virtual method** — Sweep calls `Obj.Recycle` instead of `Obj.Free`. The default calls `Free`, but subclasses can override to return objects to a pool.
- **Measurable impact** — Both the GocciaBenchmarkRunner and GocciaTestRunner call `Collect` after each file to reclaim memory between script executions.

## Memory Ceiling

The GC tracks approximate heap usage via `InstanceSize` per registered object. A byte ceiling is always active:

- **Default:** half of physical memory, capped at 8 GB on 64-bit or 700 MB on 32-bit. Falls back to 512 MB when OS detection fails.
- **Override:** `--max-memory=<bytes>` sets an explicit limit.

Any allocation that pushes `BytesAllocated` above `MaxBytes` raises a JavaScript `RangeError`. The error is catchable with `try/catch`; after catching, the script can call `Goccia.gc()` to free unreachable objects and retry.

From JavaScript, `Goccia.gc.bytesAllocated` and `Goccia.gc.maxBytes` are read-only getters. The ceiling can only be changed from the engine level (CLI flag or Pascal API: `TGarbageCollector.Instance.MaxBytes`).

## JavaScript API

`Goccia.gc()` triggers a full mark-and-sweep collection. It bypasses the automatic collection threshold but respects `GC.Enabled`: on worker threads (where `Enabled` is `False` because shared immutable objects are not thread-safe for concurrent marking) the call is a no-op. It is safe to call repeatedly and returns `undefined`.

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
