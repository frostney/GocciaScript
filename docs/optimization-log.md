# Souffle VM Optimization Log

This document records every optimization experiment attempted on the Souffle VM and bytecode pipeline since PR #73 — what worked, what didn't, and why. It is intended to give a fresh contributor full context for continuing optimization work without repeating past mistakes.

## Overview

GocciaScript has two execution modes:

| Pipeline | Flow | Status |
|----------|------|--------|
| Interpreted | Source → Lexer → Parser → Interpreter (tree-walk) → Result | Stable, full feature coverage |
| Bytecode | Source → Lexer → Parser → Compiler → Souffle Bytecode → Souffle VM → Result | 3,501 tests passing |

The Souffle VM is a register-based bytecode VM with a two-tier ISA (Tier 1 core + Tier 2 runtime). Performance parity with the interpreter is the baseline goal; the VM should eventually be faster due to type-specialized opcodes and trusted call optimizations.

The bytecode pipeline still bridges to the interpreter for two features: module imports and async/await (microtask queue). Eliminating these bridge dependencies is the highest-impact remaining work.

**Key source files:**

| File | Role |
|------|------|
| `Souffle.VM.pas` | VM execution loop, two-tier dispatch |
| `Souffle.Value.pas` | `TSouffleValue` tagged union (16 bytes) |
| `Souffle.Bytecode.pas` | Opcode definitions, instruction encoding |
| `Souffle.Heap.pas` | `TSouffleHeapObject`, `TSouffleHeapString` |
| `Goccia.Runtime.Operations.pas` | GocciaScript ↔ Souffle VM bridge |
| `Goccia.Compiler.pas` | AST → Souffle bytecode |
| `Goccia.Engine.Backend.pas` | Backend orchestration, GC integration |
| `GarbageCollector.Generic.pas` | Mark-and-sweep GC singleton |
| `OrderedStringMap.pas` | String-keyed ordered map (hot-path replacement for `TDictionary`) |
| `HashMap.pas` | Open-addressed hash map with backshift deletion |

## Completed Optimizations

### Inline SouffleIsTrue (PR #73)

Marked `SouffleIsTrue` as `inline` in `Souffle.Value.pas`. This function is called on every `OP_JUMP_IF_FALSE` and `OP_JUMP_IF_TRUE` — every conditional branch in the VM. Without `inline`, every branch paid function call overhead for a simple `case` switch on `TSouffleValueKind`.

### Remove Unused Typed Local Opcodes (PR #74)

Removed 10 typed local opcodes (`OP_GET_LOCAL_INT` through `OP_SET_LOCAL_REF`) that had identical implementations to `OP_GET_LOCAL`/`OP_SET_LOCAL` — plain register copies with no type-specific behavior. The optimization decision (choosing `OP_ADD_FLOAT` vs `OP_RT_ADD`) comes from `ExpressionType` at compile time, not from which load opcode was used. Simplified the VM dispatch surface by 10 fewer case arms.

### Remove Dead Decorator Class Bridge Code (PR #75)

Removed the entire `GOCCIA_EXT_EVAL_CLASS` / `EvaluateClassByIndex` code path (~220 lines across 5 files), which was dead code since `IsSimpleClass` always returned `True`. All decorated classes are already compiled natively via `GOCCIA_EXT_BEGIN_DECORATORS` / `APPLY_ELEMENT_DECORATOR` / `FINISH_DECORATORS`.

### Tier 1 OP_NOT and OP_TO_BOOL (PR #76)

Added `OP_NOT` and `OP_TO_BOOL` as Tier 1 core opcodes in the main VM dispatch loop. The `!` operator now emits `OP_NOT` instead of `OP_RT_NOT`, eliminating a procedure call + virtual dispatch on every boolean negation. Both opcodes use `SouffleIsTrue` (inline) + `SouffleBoolean` directly — ~5 instructions of straight-line code vs a function call chain. Old `OP_RT_NOT` / `OP_RT_TO_BOOLEAN` retained for `.sbc` backward compatibility.

### Replace Double-Negation Unary + (PR #80)

Added `OP_RT_TO_NUMBER` to replace the compiler's double-negation pattern for unary `+` (ToNumber coercion). The compiler now emits one instruction instead of two. The VM handler resolves numeric types as identity, boolean/nil inline, and only falls back to `FRuntimeOps.Negate` for strings and references. WASM translator updated with dedicated `rt_to_number` import.

### Promote 10 Hot Opcodes to Main Dispatch Loop (PR #81)

Moved 10 high-frequency opcodes from `ExecuteCoreOp` (separate procedure) into the main `ExecuteLoop` case block: `RECORD_GET`, `RECORD_SET`, `ARRAY_GET`, `ARRAY_SET`, `ARRAY_PUSH`, `NEW_ARRAY`, `NEW_RECORD`, `CLOSURE`, `RETURN_NIL`, `GET_LENGTH`. Each promoted opcode saves one procedure call per execution. These opcodes are on the hot path for property access, array operations, function closures, and object creation.

### Pre-Materialize Constant Pool (PR #82)

Pre-materializes the constant pool into a `TSouffleValue` array when a module is loaded, before VM execution begins. `OP_LOAD_CONST` becomes a single array index lookup with no branching on constant kind or heap string allocation per instruction. `MaterializeConstants` is recursive — covers all sub-templates (closures, methods) in one pass. String heap objects are allocated once during materialization and reused. 

**Bug found (PR #92):** Heap-allocated string constants (`TSouffleHeapString`) created during materialization were not reachable from GC roots and were prematurely collected. Fixed by pinning heap strings in the function template's destructor/materialization lifecycle.

### Record/Delegate Fast Path for GET_PROP (PR #83)

Added a record/delegate fast path for `OP_RT_GET_PROP` in `ExecuteRuntimeOp`. When the receiver is a `TSouffleRecord`, the property is looked up directly on the record and its delegate chain before falling through to `FRuntimeOps.GetProperty`. Most dynamic property accesses hit records, so this avoids the virtual call + full runtime property resolution for the common case.

### Extend Constant Folding (PR #85)

Extended the compiler's constant folding pass with: boolean NOT (`!true` → `false`), typeof on literals (`typeof "s"` → `"string"`), and logical AND/OR short-circuit (`false && x` → `false`, `true || x` → `true`). Reduces instruction count for common patterns.

### Eliminate Redundant OP_MOVE (PR #86)

Optimized plain function call compilation to eliminate redundant `OP_MOVE` instructions. When `ADest` is at the top of the register stack and not a local variable slot, the call is compiled directly into `ADest` instead of a temp register. Method calls, super calls, and private member calls left unchanged (different register layout makes this unsafe).

### Bypass Interpreter Bridge for Class Construction (PR #87)

Bypassed the interpreter bridge for blueprint-backed class construction. Previously, all `new ClassName()` calls in bytecode mode would: (1) convert blueprint to `TGocciaClassValue`, (2) call `InstantiateClass` through the interpreter, (3) convert back. Now, simple classes are constructed natively using the blueprint's constructor closure.

**Results:** Measurable improvement in ArrayBuffer/TypedArray creation and class-heavy operations.

**Bugs found:**
- **Blueprint bridge cache inconsistency (issue #99, fixed in PR #100).** `UnwrapToGocciaValue` cached a less-complete class value that `Construct` later reused. Fixed by unifying on `ConvertBlueprintToClassValue` everywhere.
- **`instance.constructor !== Class` (issue #101, fixed in PR #102).** The native blueprint fast path didn't set the `constructor` property on the prototype.

### Optimize Bridge Cache (PR #89)

Added `FArrayBridgeDirty` flag so `SyncCachedGocciaToSouffle` skips the O(n) array cache scan when no Goccia-side mutations have occurred. Replaced double dictionary lookup (`ContainsKey` + indexer) in `ToSouffleValue` with a single `TryGetValue` call for the array reverse bridge cache.

### TSouffleValue Record Size — 16 bytes (PR #96)

Reduced `TSouffleValue` from 26 bytes (`string[23]`) to 16 bytes (`string[13]`). The 13-char inline string still covers the vast majority of property names and short literals without forcing heap allocation.

**Results:** Positive overall. Tight loops and property-access-heavy code improved from better cache utilization.

**Bug found during testing:** `>=` and `<=` operators on strings always returned `false`. String operands were coerced to numbers (producing `NaN`) before the string comparison path was reached. Fixed in both `Goccia.Evaluator.Comparison.pas` and `Goccia.Runtime.Operations.pas`.

**FPC compiler bug (PR #98):** FPC 3.2.2 has a temp register allocator bug triggered when a packed variant record ≤16 bytes is passed via an inline open array constructor at `-O2` or higher. Replaced the inline open array with a stack-allocated variable.

**Related experiments:**

| PR | Record size | Inline string max | Outcome |
|----|-------------|-------------------|---------|
| #95 | 10 bytes | 7 chars | Closed — too many strings forced to heap |
| **#96** | **16 bytes** | **13 chars** | **Merged — best balance of cache efficiency vs heap pressure** |
| #97 | 8 bytes | N/A (reference pools) | Closed — pool indirection exceeded cache savings |

### Custom Hash Map Hierarchy (PR #66)

Replaced `TDictionary` with purpose-built hash maps across the runtime:

| Map type | Key type | Features | Speedup vs TDictionary |
|----------|----------|----------|----------------------|
| `TOrderedStringMap<V>` | `string` | Static inline DJB2 hash, insertion-order preserving | 4–6× faster inserts at N=20–100 |
| `THashMap<K,V>` | Any | Open-addressed, backshift deletion (no tombstones), multiplicative hash for pointer keys | 2× faster inserts for pointer keys |
| `TOrderedMap<K,V>` | Any | Virtual hash/equality, insertion-order preserving | Parity with TDictionary |

**Applied to:**
- `FGlobals`, `FConstGlobals`, `FExports` → `TOrderedStringMap`
- `FClosureBridgeCache`, `FArrayBridgeCache`, `FArrayBridgeReverse`, `FRecordBridgeCache`, `FBlueprintBridgeCache`, `FBlueprintSuperValues`, `FFormalParameterCounts`, `FClassDefinitionScopes` → `THashMap`

**Critical regression found and fixed:** An initial `TScopeMap<V>` variant used linear scan for scope bindings. macOS `sample` profiling revealed `CreateBridgedContext` consuming 51% of CPU samples vs 24% on `main` — a 2.7× slowdown. Root cause: linear scan O(n) per scope level instead of hash-based O(1). Fixed by reverting scope bindings to `TOrderedStringMap<TLexicalBinding>`.

**Additional optimization:** `THashMap.HashKey`/`KeysEqual` specialized for pointer-sized keys using multiplicative hash and direct integer comparison. All `mod` operations replaced with bitwise AND.

### Infrastructure (PRs #90, #92, #98)

- **PR #90:** Made BenchmarkRunner exit with non-zero code when benchmarks fail, so CI catches broken benchmarks.
- **PR #92:** Pinned heap-allocated string constants during bytecode execution to prevent premature GC (bug from PR #82).
- **PR #98:** Worked around FPC 3.2.2 compiler bug with packed records ≤16 bytes and inline open arrays at `-O2+` (triggered by PR #96).

## Experiments That Didn't Work

### Integer Fast Paths for Bitwise Opcodes (PR #78)

**Hypothesis:** Adding integer fast paths for all 7 bitwise opcodes (`OP_RT_BAND`, `BOR`, `BXOR`, `SHL`, `SHR`, `USHR`, `BNOT`) directly in the VM would avoid virtual method calls when operands are `svkInteger`.

**Result:** Overall regression. The additional code in the VM dispatch loop increased instruction cache pressure, outweighing the dispatch savings. Same root cause as PR #81's successful promotion — the difference is that bitwise operations are far less frequent than property access and array operations, so the cache cost exceeds the benefit.

**Status:** Closed.

### Primitive Fast Path for typeof (PR #77)

**Hypothesis:** Inlining `typeof` for primitive types directly in `ExecuteRuntimeOp` would avoid the virtual method call for common cases.

**Result:** Minimal improvement. `typeof` is not frequent enough on hot paths to justify the code size increase.

**Status:** Closed.

### Integer Fast Path for OP_RT_POW (PR #79)

**Hypothesis:** Calling `Math.Power` directly when both operands are `svkInteger` would speed up exponentiation.

**Result:** Not merged. The edge case handling for ES2026-compliant exponentiation (Infinity, NaN, negative zero) meant the fast path was rarely taken in practice.

**Status:** Closed.

### Peephole Optimization Pass (PR #84)

**Hypothesis:** A post-compilation peephole pass eliminating redundant instructions (self-moves, zero-offset jumps, consecutive LINE merges) would reduce instruction count.

**Result:** Mixed benchmarks — some improved, some regressed. Load+move fusion was deferred as unsafe for destructuring/default parameter patterns. The safe transformations alone didn't produce consistent wins.

**Status:** Closed (deferred until register liveness analysis is available).

### Eliminate Evaluator Bridge for Async/Await (PR #88)

**Hypothesis:** A native `AwaitGocciaValue` helper handling Promises, thenables, and microtask queue draining directly in the runtime layer would eliminate the interpreter bridge for async/await.

**Result:** Regression in CI benchmarks. The microtask queue integration with the VM execution model was complex and the overhead of the native path exceeded the bridge cost for typical async patterns.

**Status:** Closed.

### Cached typeof Strings (PR #94)

**Hypothesis:** Pre-allocating `TSouffleValue` constants for `typeof` result strings would avoid repeated string construction.

**Result:** No measurable improvement. `TSouffleValue` with inline strings is already on the stack (not the heap), making string construction effectively free. FPC's string interning doesn't help (tested 6 times, typically 2–3% regression).

**Status:** Closed.

### 10-byte TSouffleValue (PR #95)

**Hypothesis:** Reducing inline string max from 23 to 7 characters (10-byte record) would improve cache utilization.

**Result:** Mixed. Some tight-loop improvements, but regressions in string-heavy operations due to more heap-allocated strings. Too many common property names exceed 7 characters.

**Status:** Closed. The 16-byte / 13-char variant (PR #96) was the winner.

### 8-byte TSouffleValue with Reference Pools (PR #97)

**Hypothesis:** Storing `Int64`, `Double`, and `TSouffleHeapObject` in external global arrays and holding only a `UInt32` index in `TSouffleValue` would bring the record to 8 bytes.

**Result:** Significant regressions. Pool indirection added latency to every value access. GC integration was complex (pool entries needed marking). Pool reset timing was tricky (premature reset invalidated global built-ins).

**Status:** Closed.

## Remaining Work

### Native Module Loading

**Priority:** High

The runtime bridge is still used for all module imports. This is one of the last major bridge dependencies. Native module loading would load `.sbc` files directly, resolve imports/exports at the bytecode level, and eliminate the interpreter bridge for module operations.

**Relevant files:** `Goccia.Modules.Resolver.pas`, `Souffle.Bytecode.Module.pas`, `Goccia.Runtime.Operations.pas`.

### Async/Await Native Support

**Priority:** High

Async functions and `await` expressions currently delegate to the interpreter via the bridge. PR #88 attempted this but regressed. A fresh approach may be needed — potentially compiling async functions as state machines rather than wrapping interpreter calls.

**Relevant files:** `Goccia.MicrotaskQueue.pas`, `Goccia.Values.AsyncFunctionValue.pas`, `Goccia.Runtime.Operations.pas`.

### Garbage Collection

**Priority:** Separate track

The GC may benefit from revisiting, but this requires a fresh analysis — the assumptions from the original design may no longer hold after recent refactors. The current behavior of running `Collect` after each test/benchmark file is a deliberate design choice. Any GC changes should be validated against the current codebase state, not against prior assumptions.

**Relevant files:** `GarbageCollector.Generic.pas`, `GarbageCollector.Managed.pas`, `Goccia.Engine.Backend.pas`.

### Minor Items

- **Tombstone compaction for TOrderedStringMap** — Tracked in [issue #103](https://github.com/frostney/GocciaScript/issues/103). Low priority; `Remove` is only called during bridge-cache teardown, not in tight loops.
- **Naming convention for shared utility units** — Tracked in [issue #104](https://github.com/frostney/GocciaScript/issues/104). Shared units (`BaseMap.pas`, `HashMap.pas`, etc.) should follow a naming convention distinct from the `Goccia.*` prefix, since they are extractable infrastructure not specific to GocciaScript.

## Key Insights

1. **Profile before guessing.** The `TScopeMap` regression (2.7× slowdown) was only found via macOS `sample` profiling. CI benchmarks showed the symptom (−30% to −42%) but not the cause. Always profile with `sample` (macOS) or `callgrind` (Linux) before making optimization decisions.

2. **Cache is king.** The 16-byte `TSouffleValue` (down from 26) improved performance because more values fit in cache lines. But going to 8 bytes with reference pools was worse because the pool indirection added memory accesses that negated the cache benefit.

3. **String operations are already fast in FPC.** String interning, `typeof` caching, and similar strategies don't help because FPC's COW (copy-on-write) semantics and inline short strings are already efficient. Do not attempt string interning or string caching again. See [spikes/fpc-string-performance.md](spikes/fpc-string-performance.md).

4. **Code size matters.** Inlining Tier 2 ops into the VM loop (PR #78) and adding the async/await native path (PR #88) both caused regressions from instruction cache pressure. Keep the VM loop compact. Only promote opcodes that are genuinely high-frequency (like the 10 in PR #81).

5. **Bridge elimination is the biggest win.** Native class construction (PR #87) showed clear improvements. The remaining bridge dependencies (modules, async/await) are the most impactful optimization targets.

6. **Hash map specialization matters for hot paths.** Generic DJB2 over raw bytes was 2× slower than multiplicative hash for pointer keys (see [spikes/fpc-hashmap-performance.md](spikes/fpc-hashmap-performance.md)). But avoid linear-scan alternatives — the `TScopeMap` incident proved that even small N doesn't justify O(n) lookup on hot paths.

7. **FPC generics have zero runtime cost.** Type aliases, subclasses, and multi-level generic inheritance all produce byte-identical machine code. Design map hierarchies for API clarity, not performance. See [spikes/fpc-generics-performance.md](spikes/fpc-generics-performance.md).

8. **Virtual dispatch is constant-time regardless of hierarchy depth.** A class 5 levels deep dispatches in the same ~2.5ns as a direct child. Avoid `inherited` chains on hot paths (linear cost scaling), but hierarchy depth itself is free. See [spikes/fpc-dispatch-performance.md](spikes/fpc-dispatch-performance.md).

9. **Run benchmarks on CI, not locally.** Local results have high variance. The workflow is: implement → push → let CI run benchmarks → analyze CI results.
