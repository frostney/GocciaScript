# Profiling Report: GocciaScript Benchmark Suite

**Date:** 2026-03-08
**Tool:** Valgrind callgrind 3.22.0
**Platform:** Linux x86_64, FPC 3.2.2
**Benchmarks:** Full suite (254 benchmarks across 17 files)
**Configuration:** `GOCCIA_BENCH_CALIBRATION_MS=50 GOCCIA_BENCH_ROUNDS=1`

## Executive Summary

Callgrind profiling reveals that **FreePascal runtime overhead dominates execution time in both modes**. In interpreted mode, the `is` type-check chain in the evaluator is the single largest bottleneck (18.4% of all instructions). In bytecode mode, exception handling setup (`try`/`except` via `setjmp`/`longjmp`) is the top FPC-runtime cost at 11.4%.

| Mode | Dev Build | Prod Build | Dev→Prod Savings |
|------|-----------|------------|------------------|
| Interpreted | 29.5B Ir | 24.2B Ir | 18% |
| Bytecode | 17.3B Ir | 16.0B Ir | 8% |

The bytecode VM executes **34% fewer instructions** than the interpreter in production builds (16.0B vs 24.2B). Most of the interpreted mode's extra cost comes from type-checking dispatch, scope chain hash lookups, and per-call object allocation that the VM avoids through register-based execution.

## Profiling Results (Production Builds)

### Interpreted Mode — Top Hotspots

| Rank | Function | Ir (millions) | % | Category |
|------|----------|:---:|:---:|----------|
| 1 | `TObject.InheritsFrom` | 3,215 | 13.30 | Type checking |
| 2 | `fpc_do_is` | 1,243 | 5.14 | Type checking |
| 3 | `fpc_pushexceptaddr` | 1,106 | 4.58 | Exception handling |
| 4 | `FillChar` | 961 | 3.97 | Memory zeroing |
| 5 | `SysGetMem_Fixed` | 952 | 3.94 | Memory allocation |
| 6 | `fpc_dynarray_setlength` | 839 | 3.47 | Dynamic arrays |
| 7 | `RecordRTTI` | 707 | 2.92 | RTTI traversal |
| 8 | **`EvaluateExpression`** | 704 | 2.91 | Application code |
| 9 | `fpc_popaddrstack` | 685 | 2.83 | Exception handling |
| 10 | `fpc_setjmp` | 632 | 2.62 | Exception handling |
| 11 | `CleanupInstance` | 585 | 2.42 | Object destruction |
| 12 | `SysFreeMem` | 541 | 2.24 | Memory deallocation |
| 13 | `SysFreeMem_Fixed` | 524 | 2.17 | Memory deallocation |
| 14 | `fpc_finalize` | 500 | 2.07 | RTTI finalization |
| 15 | **`OrderedMap.TryGetValue`** | 415 | 1.72 | Scope lookup |
| 16 | **`OrderedMap.FindBucket`** | 387 | 1.60 | Scope lookup |
| 17 | `NewInstance` | 355 | 1.47 | Object allocation |
| 18 | `SysGetMem` | 339 | 1.40 | Memory allocation |
| 19 | `fpc_dynarray_clear` | 333 | 1.38 | Dynamic arrays |
| 20 | `fpc_ansistr_compare_equal` | 255 | 1.05 | String comparison |

### Bytecode Mode — Top Hotspots

| Rank | Function | Ir (millions) | % | Category |
|------|----------|:---:|:---:|----------|
| 1 | **`ExecuteLoop`** | 1,239 | 7.76 | VM dispatch |
| 2 | `fpc_pushexceptaddr` | 833 | 5.22 | Exception handling |
| 3 | `RecordRTTI` | 765 | 4.79 | RTTI traversal |
| 4 | `SysGetMem_Fixed` | 537 | 3.36 | Memory allocation |
| 5 | `fpc_finalize` | 527 | 3.31 | RTTI finalization |
| 6 | `fpc_dynarray_setlength` | 525 | 3.29 | Dynamic arrays |
| 7 | `TObject.InheritsFrom` | 523 | 3.28 | Type checking |
| 8 | `fpc_popaddrstack` | 516 | 3.23 | Exception handling |
| 9 | `fpc_setjmp` | 476 | 2.98 | Exception handling |
| 10 | `FillChar` | 417 | 2.61 | Memory zeroing |
| 11 | **`ExecuteRuntimeOp`** | 375 | 2.35 | VM runtime bridge |
| 12 | `fpc_initialize` | 371 | 2.33 | RTTI initialization |
| 13 | `CleanupInstance` | 358 | 2.24 | Object destruction |
| 14 | **`TList.GetItem`** | 355 | 2.22 | List access |
| 15 | **`TList.IndexOf`** | 282 | 1.77 | List search |
| 16 | **`CallClosure`** | 256 | 1.60 | VM function call |
| 17 | `SysFreeMem` | 254 | 1.59 | Memory deallocation |
| 18 | `SysFreeMem_Fixed` | 245 | 1.54 | Memory deallocation |
| 19 | `fpc_do_is` | 242 | 1.52 | Type checking |
| 20 | **`ExecuteCoreOp`** | 232 | 1.45 | VM core dispatch |

### Cost by Category (Production Builds)

| Category | Interpreted | % | Bytecode | % |
|----------|:-----------:|:---:|:--------:|:---:|
| **Type checking** (`is` / `InheritsFrom`) | 4,458M | 18.4 | 765M | 4.8 |
| **Exception handling** (`setjmp` / `push`/`pop`) | 2,423M | 10.0 | 1,825M | 11.4 |
| **Memory management** (alloc/free/FillChar/Move) | 3,497M | 14.5 | 2,397M | 15.0 |
| **RTTI** (RecordRTTI/finalize/initialize) | 1,414M | 5.9 | 1,856M | 11.6 |
| **Dynamic arrays** (setlength/clear) | 1,172M | 4.8 | 712M | 4.5 |
| **Object lifecycle** (NewInstance/CleanupInstance) | 940M | 3.9 | 580M | 3.6 |
| **Scope/hash lookup** (OrderedMap) | 802M | 3.3 | 73M | 0.5 |
| **String operations** (compare/assign/decr_ref) | 629M | 2.6 | 328M | 2.1 |
| **VM execution** (ExecuteLoop/Core/Runtime) | — | — | 2,101M | 13.2 |
| **Application evaluator** | 1,020M | 4.2 | — | — |

## Actionable Optimizations

### 1. [CRITICAL] Replace `is` Type-Check Chain with VMT Dispatch

**Impact:** Up to **18.4%** of interpreted mode, **4.8%** of bytecode mode (compile phase)

**Problem:** `EvaluateExpression` dispatches on expression type using a chain of ~31 `if AExpression is TGocciaXxx then` checks. In FPC, each `is` check calls `TObject.InheritsFrom`, which walks the `ClassParent` pointer chain in a loop. With 31 expression types, the average expression evaluates ~15 `InheritsFrom` calls before matching. `EvaluateStatement` adds another ~19 checks.

**Root cause in FPC:** `TObject.InheritsFrom` is O(depth) — it walks the class hierarchy linearly. For a flat hierarchy (all expression types inherit directly from `TGocciaExpression`), this is O(1) per check but still O(n) total for n branches. The cumulative cost of 50 `is` checks across two dispatch functions is 18.4% of all interpreted instructions.

**Solution:** Add virtual `Evaluate` / `EvaluateStatement` methods to the AST node hierarchy and dispatch through the VMT. In FPC, a virtual method call is a single indexed pointer dereference through the vtable — O(1) regardless of hierarchy depth or number of subclasses. This is the same pattern already used throughout the value hierarchy (`TGocciaValue.GetProperty`, `IsPrimitive`, `IsCallable`, `BindThis`, `MarkReferences`, `RuntimeCopy`, etc.).

```pascal
// Goccia.AST.Node.pas
TGocciaExpression = class(TGocciaASTNode)
  function Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue; virtual; abstract;
end;

TGocciaStatement = class(TGocciaASTNode)
  function Execute(const AContext: TGocciaEvaluationContext): TGocciaControlFlow; virtual; abstract;
end;
```

```pascal
// Each expression subclass overrides:
TGocciaLiteralExpression = class(TGocciaExpression)
  function Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue; override;
end;

TGocciaBinaryExpression = class(TGocciaExpression)
  function Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue; override;
end;
```

```pascal
// Before (O(n) dispatch chain, 18.4% overhead):
function EvaluateExpression(const AExpression: TGocciaExpression;
  const AContext: TGocciaEvaluationContext): TGocciaValue;
begin
  if AExpression is TGocciaLiteralExpression then
    // ...
  else if AExpression is TGocciaIdentifierExpression then
    // ...
  else if AExpression is TGocciaBinaryExpression then
    // ... (~31 branches)
end;

// After (O(1) VMT dispatch):
function EvaluateExpression(const AExpression: TGocciaExpression;
  const AContext: TGocciaEvaluationContext): TGocciaValue;
begin
  Result := AExpression.Evaluate(AContext);
end;
```

**Evaluator purity:** This does not violate Critical Rule #1. Evaluator purity means evaluation functions are deterministic with no hidden mutable state — the same expression + context always produces the same result. Moving the dispatch target into the AST class changes *where* the code lives, not *how* it behaves. The `AContext` record is still the sole carrier of evaluation state. The value hierarchy already follows this exact pattern: `TGocciaValue.GetProperty`, `ToStringLiteral`, `ToBooleanLiteral` are all virtual methods on value objects that the evaluator calls directly — evaluation logic that lives on the class hierarchy, dispatched via VMT. The AST hierarchy should follow the same design.

**Why VMT, not an enum tag:** A `Kind` enum field + `case` statement would also achieve O(1) dispatch via a jump table, but introduces a parallel type system that must be kept in sync with the class hierarchy. The VMT approach:
- Preserves the existing class hierarchy — no new enum type, no constructor changes
- Is enforced at compile time — forgetting to implement `Evaluate` on a new expression subclass is a compiler error (`abstract method not overridden`), whereas a missing `case` branch is a silent runtime bug
- Is idiomatic FPC — the codebase already uses VMT dispatch on `TGocciaValue` for the same purpose
- Has the same O(1) cost — one pointer dereference through the vtable

**Implementation note:** The evaluation logic currently in `EvaluateExpression`'s `if`/`is` branches should move into each subclass's `Evaluate` override. Helper functions like `EvaluateBinary`, `EvaluateCall`, `EvaluateArrowFunction` etc. that already exist as standalone functions in `Goccia.Evaluator.pas` can remain as-is — each subclass's `Evaluate` simply calls the corresponding helper. This keeps the refactor incremental: move the dispatch, not the implementation.

### 2. [CRITICAL] Reduce `try`/`except` Blocks in Evaluator

**Impact:** Up to **10-11%** of instructions in both modes

**Problem:** The evaluator has **64** `try` blocks in `Goccia.Evaluator.pas` alone. FPC implements `try`/`except` using `setjmp`/`longjmp`, which means every `try` block incurs:
- `fpc_pushexceptaddr` — registers the exception frame
- `fpc_setjmp` — saves CPU registers to a jump buffer
- `fpc_popaddrstack` — pops the frame on normal exit

This is a fixed per-`try`-block cost even when no exception is thrown.

**Solution:** Audit each `try` block and eliminate those that exist only for cleanup patterns. The goal is to reduce the total count of `try` blocks, not to replace them with equally expensive mechanisms.

- **Eliminate `TGocciaArgumentsCollection` cleanup blocks**: If item 3 is implemented (replacing `TGocciaArgumentsCollection` with open array parameters), the `try`/`finally` blocks around argument collection create/free become unnecessary — stack-allocated open arrays need no cleanup. This alone removes a significant portion of the 64 `try` blocks.

- **GC temp root patterns** (`AddTempRoot` + `try`/`finally` + `RemoveTempRoot`): These are the most numerous `try`/`finally` blocks in the evaluator. The fundamental issue is that values held only in local variables need GC protection during evaluation. Two FPC-specific approaches:

  1. **Consolidate temp roots per evaluation frame**: Instead of individual `AddTempRoot`/`RemoveTempRoot` pairs (each requiring its own `try`/`finally`), maintain a per-call-frame temp root list that is bulk-cleared on scope exit. This replaces N `try`/`finally` blocks with one.

  2. **Extend the scope's binding map**: If a temp value is being created and immediately used (e.g., an iterator for `for...of`), bind it in the scope under an internal name. The GC already marks scope bindings — no `AddTempRoot`/`try`/`finally` needed.

- **Call stack push/pop**: `TGocciaCallStack.Push` + `try`/`finally` + `Pop` runs on every function call. Combine the call stack push/pop into the scope creation/destruction lifecycle (scope constructor pushes, destructor pops) to eliminate one `try`/`finally` per call.

### 3. [HIGH] Eliminate Allocations on the Call Path

**Impact:** **14-15%** of instructions in both modes

**Problem:** Every function call allocates:
- 1× `TGocciaCallScope` (with internal `TGocciaScopeBindingMap` / `TOrderedMap`)
- 1× `TGocciaArgumentsCollection` (with internal `TGocciaValueList`)
- N× `TGocciaNumberLiteralValue` via `RuntimeCopy` for each numeric operand

`FillChar` alone (zeroing newly allocated objects) accounts for 2.6-4.0% of instructions.

**Historical note:** Object pooling and string interning have been tried in this project and did not yield improvements. String interning benchmarked at **-4% across 172 benchmarks** — FPC's fixed-size allocator (`SysGetMem_Fixed`) is already essentially a pool, so adding a lookup layer on top only adds overhead. The right FPC-specific approach is to **eliminate allocations entirely** rather than trying to recycle them.

**Solutions:**

- **Replace `TGocciaArgumentsCollection` with open array parameters**: FPC's `array of TGocciaValue` open array parameters are stack-allocated by the caller — zero heap cost. The current pattern creates a heap-allocated `TGocciaValueList` inside a `TInterfacedObject` for every call, then immediately destroys it. Replace with:

  ```pascal
  // Before: heap-allocate per call
  Args := TGocciaArgumentsCollection.Create;
  Args.Add(Value1);
  Args.Add(Value2);
  FunctionValue.Call(Args, ThisValue);
  Args.Free;

  // After: stack-allocated open array, zero heap cost
  FunctionValue.Call([Value1, Value2], ThisValue);
  ```

  In FPC, `const AArgs: array of TGocciaValue` is passed as a pointer + length pair on the stack. No `TInterfacedObject`, no `TGocciaValueList`, no ref-counting, no `FillChar`, no `SysGetMem`/`SysFreeMem`. This eliminates 2 heap allocations + 1 interface ref-count cycle per call.

- **Pre-size `OrderedMap` in scope creation**: `CreateChild` already accepts a capacity hint. Ensure hot paths (arrow function calls with known parameter count) pass the correct hint to avoid `SetLength` resizing during parameter binding.

- **Consider tagged value representation for numbers**: Every numeric expression currently calls `RuntimeCopy`, which heap-allocates a new `TGocciaNumberLiteralValue`. In FPC, a tagged pointer or NaN-boxed value representation could encode numbers as immediate values without heap allocation. This is a larger architectural change but would eliminate the single biggest source of per-expression allocations. The Souffle VM's `TSouffleValue` already uses this approach (tagged union with inline `Float` and `Integer` kinds) — the interpreter could adopt a similar strategy.

### 4. [HIGH] Eliminate RTTI Overhead on Hot-Path Records

**Impact:** **6-12%** of instructions (higher in bytecode due to more record-passing)

**Problem:** `RecordRTTI`, `fpc_finalize`, and `fpc_initialize` together consume 6-12% of all instructions. In FPC, any record containing a managed type (string, interface, dynamic array) triggers automatic RTTI-based initialization and finalization every time it is created, copied, or goes out of scope.

**Root cause:** `TGocciaEvaluationContext` contains `CurrentFilePath: string` — a managed type. This record is passed by value to every evaluator function, triggering `fpc_initialize` + `fpc_finalize` on each call.

**Solutions:**

- **Remove managed types from `TGocciaEvaluationContext`**: Replace `CurrentFilePath: string` with `CurrentFilePath: PAnsiChar` (or an integer index into a string table). This makes the record fully unmanaged, eliminating all RTTI overhead for the most-passed record in the system.

  ```pascal
  TGocciaEvaluationContext = record
    Scope: TGocciaScope;           // pointer — unmanaged
    OnError: TGocciaThrowErrorCallback; // method pointer — unmanaged
    LoadModule: TLoadModuleCallback;    // method pointer — unmanaged
    CurrentFilePath: PAnsiChar;         // raw pointer — unmanaged
  end;
  ```

  Since `CurrentFilePath` is set once per script execution and read only for error reporting, using a `PAnsiChar` pointing to the long-lived string in the interpreter is safe.

- **Audit all records on hot paths**: Check `TGocciaControlFlow` (already optimized — `PtrUInt` only, no managed fields ✓), `TSouffleVMCallFrame` (already optimized — class pointers + integers only ✓), `TLexicalBinding` (check if managed fields exist).

- **Check callback types**: If `TGocciaThrowErrorCallback` or `TLoadModuleCallback` are `reference to` function types (anonymous function references), they are managed types in FPC and trigger RTTI. Use `procedure of object` or plain function pointers instead.

### 5. [MEDIUM] Optimize Scope Chain Lookup

**Impact:** **3.3%** in interpreted mode, **0.5%** in bytecode

**Problem:** Each variable lookup walks the scope chain calling `OrderedMap.TryGetValue` → `FindBucket` (hash + linear probe) at each level. For closures with deep scope chains, this is proportionally expensive. `OrderedMap.Create` itself accounts for 0.98% of interpreted instructions — a new hash table is allocated for every scope, even trivial ones with 1-2 bindings.

**Solutions:**

- **Use a flat array for small scopes**: For scopes with ≤8 bindings, linear search over a contiguous `array[0..7] of record Name: PAnsiChar; Binding: TLexicalBinding end` is faster than hash table lookup due to cache locality. The `OrderedMap` hash table has per-entry overhead (hash computation, bucket array, collision chains) that exceeds the cost of scanning 4-8 entries sequentially. FPC's `fpc_ansistr_compare_equal` is fast for short strings — it does a pointer equality check first, then length check, before comparing characters. Most scopes in tight loops have only a few bindings (function parameters + 1-2 locals), making linear scan the better fit.

- **Defer `OrderedMap` allocation**: Instead of creating the hash table in the scope constructor, start with the flat array representation and only promote to `OrderedMap` when the binding count exceeds the threshold. This avoids `OrderedMap.Create` (0.98% of interpreted instructions) for the majority of scopes.

- **Avoid `Keys`/`Values`/`ToArray` on OrderedMap in hot paths**: These allocate new dynamic arrays on every call. The `Destroy` path and `MarkReferences` iteration should use direct bucket traversal without intermediate allocations.

### 6. [MEDIUM] Reduce Dynamic Array Resizing

**Impact:** **4-5%** in both modes

**Problem:** `fpc_dynarray_setlength` and `fpc_dynarray_clear` account for ~4-5% of all instructions. Dynamic array growth triggers reallocation + `Move` + RTTI init/finalize of elements.

**Solutions:**

- **Pre-size arrays**: Where the final size is known (e.g., `Array.from({length: N})`), allocate once at the correct size instead of growing incrementally.

- **Use geometric growth for `TGocciaValueList`**: Ensure `TObjectList<T>` uses 2× growth factor. FPC's `TList` already does this, but verify `PrepareAddingItem` uses the correct growth strategy.

- **Avoid creating empty dynamic arrays**: Many code paths create an array and then immediately set its length. Use `SetLength` directly to the known capacity.

### 7. [MEDIUM] Eliminate `TList.IndexOf` in Bytecode Bridge

**Impact:** **1.77%** of bytecode mode

**Problem:** `TList<TGocciaValue>.IndexOf` performs a linear scan. At 282M instructions (1.77%), it is being called frequently in the bytecode bridge layer — likely from `TObjectList` notification callbacks during GC sweep or array bridge sync.

**Root cause in FPC:** `TObjectList<T>.Notify` is called on every `Add`/`Delete` operation. When the list owns its objects (`OwnsObjects = True`), `Notify` with `cnRemoved` frees the object. But `TCustomList.DoRemove` calls `IndexOf` internally to find the element before removal. Additionally, `DeleteRange` in sweep compaction may trigger per-element `Notify` calls.

**Solutions:**

- **Audit `TObjectList` ownership**: If `SweepPhase` manages object lifetimes explicitly (freeing dead values directly), the `TObjectList` should use `OwnsObjects = False` to avoid redundant `Notify` → `IndexOf` chains on removal. The project already uses `Create(False)` for non-owning collections — verify this is the case for GC-managed lists.

- **Replace `DeleteRange` with in-place compaction**: Instead of using `TObjectList.DeleteRange` (which triggers per-element notifications), compact the live values array manually by moving live pointers forward and adjusting the count. This is already the pattern in `SweepPhase` — verify that no `TObjectList` method calls are reintroducing `IndexOf` overhead underneath.

- **Use `TFPList` or raw pointer arrays for GC internals**: FPC's `TFPList` (non-generic, untyped pointer list) has no `Notify` mechanism and no `IndexOf` in its removal path. For the GC's managed values list where ownership and type safety are handled manually anyway, `TFPList` avoids the generic collection overhead entirely.

### 8. [LOW] Optimize Number Comparison Checks

**Impact:** ~1.5% in interpreted mode

**Problem:** `IsNaN` (0.57%), `IsInfinite` (0.54%), `GetIsInfinity` (0.09%), and `GetIsNegativeInfinity` (0.09%) collectively consume ~1.3% of interpreted instructions. Every arithmetic operation checks for NaN and Infinity.

**Solutions:**

- **Fast-path common case**: Check if both operands are "normal" numbers (not NaN, not Infinity) first with a single branch, then proceed with the fast arithmetic path. Only fall back to special-value handling when needed.

- **Use raw IEEE 754 bit checks**: The Souffle VM already uses `FloatBitsAreNaN` (raw bit-pattern check). Apply the same approach in the interpreter's number operations to avoid FPC's `Math.IsNaN`/`Math.IsInfinite` library calls.

### 9. [LOW] Reduce `fpc_intf_assign` / Interface Ref-Counting

**Impact:** ~0.6% in both modes

**Problem:** `fpc_intf_assign` + `fpc_intf_decr_ref` account for ~0.5-0.6% of instructions. This comes from `TGocciaArgumentsCollection` being a `TInterfacedObject` — every assignment triggers atomic ref-count operations (`IncLocked`/`DecLocked`).

**Solutions:**

- **Use `class` instead of `TInterfacedObject`**: If `TGocciaArgumentsCollection` doesn't need to be passed as an interface, make it a plain class with explicit `Free` calls. This eliminates ref-counting overhead entirely.

- **Use `{$INTERFACES CORBA}`**: If interface dispatch is desired without ref-counting, CORBA-style interfaces avoid the overhead. However, this requires manual lifetime management.

## Raw Data

The annotated callgrind output for both modes is available in:
- `docs/profiling-report/callgrind_interpreted_prod.txt`
- `docs/profiling-report/callgrind_bytecode_prod.txt`

## Priority Matrix

| # | Optimization | Impact | Effort | Mode |
|---|-------------|--------|--------|------|
| 1 | VMT dispatch for expression/statement | 18.4% | Medium | Interpreted |
| 2 | Reduce `try`/`except` blocks | 10-11% | High | Both |
| 3 | Eliminate call-path allocations | 14-15% | Medium | Both |
| 4 | Eliminate RTTI on `TGocciaEvaluationContext` | 6-12% | Low | Both |
| 5 | Optimize scope chain lookup | 3.3% | Medium | Interpreted |
| 6 | Reduce dynamic array resizing | 4-5% | Low | Both |
| 7 | Eliminate `TList.IndexOf` in bridge | 1.8% | Low | Bytecode |
| 8 | Fast-path number comparisons | 1.5% | Low | Interpreted |
| 9 | Remove interface ref-counting | 0.6% | Low | Both |

**Recommended order:** Items 4, 1, 6, 3, 2 — starting with the lowest-effort highest-impact changes. Item 4 (`TGocciaEvaluationContext` RTTI removal) is a single-line change with 6-12% impact. Item 1 (VMT dispatch) is a structural refactor but with the highest single-item impact at 18.4%, and the pattern is already established in the value hierarchy.
