# Souffle VM Optimization Log

This document records every optimization experiment attempted on the Souffle VM and bytecode pipeline — what worked, what didn't, and why. It is intended to give a fresh contributor full context for continuing optimization work without repeating past mistakes.

## Overview

GocciaScript has two execution modes:

| Pipeline | Flow | Status |
|----------|------|--------|
| Interpreted | Source → Lexer → Parser → Interpreter (tree-walk) → Result | Stable, full feature coverage |
| Bytecode | Source → Lexer → Parser → Compiler → Souffle Bytecode → Souffle VM → Result | 3,501 tests passing |

The Souffle VM is a register-based bytecode VM with a two-tier ISA (Tier 1 core + Tier 2 runtime). Performance parity with the interpreter is the baseline goal; the VM should eventually be faster due to type-specialized opcodes and trusted call optimizations.

The bytecode pipeline still bridges to the interpreter for three features: module imports, async/await (microtask queue), and decorator evaluation. Eliminating these bridge dependencies is the highest-impact remaining work.

**Key source files:**

| File | Role |
|------|------|
| `Souffle.VM.pas` | VM execution loop, two-tier dispatch |
| `Souffle.Value.pas` | `TSouffleValue` tagged union |
| `Souffle.Bytecode.pas` | Opcode definitions, instruction encoding |
| `Souffle.Heap.pas` | `TSouffleHeapObject`, `TSouffleString` |
| `Goccia.Runtime.Operations.pas` | GocciaScript ↔ Souffle VM bridge |
| `Goccia.Compiler.pas` | AST → Souffle bytecode |
| `Goccia.Engine.Backend.pas` | Backend orchestration, GC integration |
| `GarbageCollector.Generic.pas` | Mark-and-sweep GC singleton |
| `OrderedStringMap.pas` | String-keyed ordered map (hot-path replacement for `TDictionary`) |
| `HashMap.pas` | Open-addressed hash map with backshift deletion |

## Completed Optimizations

### TSouffleValue Record Size (PR #96)

Reduced `TSouffleValue` from 26 bytes (`string[23]`) to 16 bytes (`string[13]`).

**Rationale:** Smaller records mean more values per cache line. The 13-char inline string still covers the vast majority of property names and short literals without forcing heap allocation.

**Results:** Positive overall. Tight loops and property-access-heavy code improved from better cache utilization. String-heavy operations were unaffected because most runtime strings remain under 13 characters.

**Bug found during testing:** `>=` and `<=` operators on strings always returned `false`. String operands were coerced to numbers (producing `NaN`) before the string comparison path was reached. Fixed in both `Goccia.Evaluator.Comparison.pas` and `Goccia.Runtime.Operations.pas`.

**Related experiments:**

| PR | Record size | Inline string max | Outcome |
|----|-------------|-------------------|---------|
| #95 | 10 bytes | 7 chars | Closed — too many strings forced to heap |
| **#96** | **16 bytes** | **13 chars** | **Merged — best balance of cache efficiency vs heap pressure** |
| #97 | 8 bytes | N/A (reference pools) | Closed — pool indirection exceeded cache savings |

See [TSouffleValue Record Size Experiments](#8-byte-tsoufflevalue-with-reference-pools-pr-97) in the failed experiments section for details on #95 and #97.

### Native Class Construction (PR #87)

Bypassed the interpreter bridge for blueprint-backed class construction. Previously, all `new ClassName()` calls in bytecode mode would:

1. Convert the blueprint to a `TGocciaClassValue` (bridge)
2. Call `InstantiateClass` through the interpreter
3. Convert the result back to `TSouffleValue`

Now, simple classes (no decorators) are constructed natively in the VM using the blueprint's constructor closure directly.

**Results:** Measurable improvement in ArrayBuffer/TypedArray creation benchmarks and class-heavy operations.

**Bugs found:**
- **Blueprint bridge cache inconsistency (issue #99, fixed in PR #100).** When a blueprint was first seen via `UnwrapToGocciaValue` (simple bridge), a less-complete class value was cached and later reused by `Construct` (which needs full fidelity with getters/setters/private members). Fixed by unifying on `ConvertBlueprintToClassValue` everywhere.
- **`instance.constructor !== Class` (issue #101, fixed in PR #102).** The native blueprint fast path didn't set the `constructor` property on the prototype. Fixed by ensuring the prototype's `constructor` property is set during blueprint instantiation.

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

**Critical regression found and fixed:** An initial `TScopeMap<V>` variant used linear scan (no hashing) for scope bindings. macOS `sample` profiling revealed `CreateBridgedContext` consuming 51% of CPU samples vs 24% on `main` — a 2.7× slowdown. Root cause: linear scan O(n) per scope level instead of hash-based O(1). Fixed by reverting scope bindings to `TOrderedStringMap<TLexicalBinding>`.

**Additional optimization:** `THashMap.HashKey` and `KeysEqual` were specialized for pointer-sized keys using multiplicative hash (golden-ratio/Fibonacci constant) instead of byte-by-byte DJB2, and direct integer comparison instead of `CompareMem`. All `mod` operations replaced with bitwise AND (`and (FCapacity - 1)`). FPC constant-folds `SizeOf(TKey)` after specialization.

### Minor Improvements

**Computed Property Source Order (PR #86)** — Fixed `ConvertToPattern` in the parser to preserve interleaved static/computed property order in destructuring patterns. Added `PropertySourceOrder` tracking.

**Continue Opcode Removal (PR #89)** — Removed dead `OP_RT_CONTINUE` enum entry and bumped `SOUFFLE_FORMAT_VERSION`. The Souffle VM handles `continue` through `OP_JUMP` — no dedicated opcode needed.

## Experiments That Didn't Work

### Inline Runtime Ops (PR #78)

**Hypothesis:** Inlining common Tier 2 runtime operations directly into the VM's `ExecuteLoop` would avoid the virtual method call overhead on `TSouffleRuntimeOperations`.

**Result:** −18% overall bytecode regression. The code size increase caused instruction cache pressure that outweighed the dispatch savings. The VM loop must stay compact.

**Status:** Closed.

### 8-byte TSouffleValue with Reference Pools (PR #97)

**Hypothesis:** Storing `Int64`, `Double`, and `TSouffleHeapObject` values in external global arrays (`GIntPool`, `GFloatPool`, `GRefPool`) and holding only a `UInt32` index in `TSouffleValue` would bring the record down to 8 bytes — one cache line per 8 values.

**Result:** Significant regressions. Pool indirection added latency to every value access. GC integration was complex (pool entries needed marking). Pool reset timing was tricky (premature reset invalidated global built-ins).

**Status:** Closed. The indirection overhead exceeded cache savings.

### 10-byte TSouffleValue (PR #95)

**Hypothesis:** Reducing inline string max from 23 to 7 characters (10-byte record) would improve cache utilization.

**Result:** Mixed. Some improvements in tight loops, but regressions in string-heavy operations due to more heap-allocated strings. Too many common property names exceed 7 characters.

**Status:** Closed. Not enough benefit to justify the tradeoff. The 16-byte / 13-char variant (PR #96) was the winner.

### Cached typeof Strings (PR #94)

**Hypothesis:** Pre-allocating `TSouffleValue` constants for `typeof` result strings (`"string"`, `"number"`, `"object"`, etc.) would avoid repeated string construction.

**Result:** No measurable improvement across 6 measurement runs, with a typical 2–3% regression. `TSouffleValue` with inline strings is already on the stack (not the heap), making string construction effectively free.

**Status:** Closed. Expected null result confirmed.

### GC Compaction (PR #84)

**Hypothesis:** Adding memory compaction to the garbage collector would improve cache locality for long-running scripts.

**Result:** Mixed — some benchmarks improved, others regressed. The complexity was significant and the GC subsystem needs a dedicated optimization pass.

**Status:** Closed (deferred, not abandoned). Will be revisited when GC optimization is prioritized.

### Tier 1 Boolean/Not Promotion (PR #88)

**Hypothesis:** Promoting `OP_RT_NOT` and `OP_RT_TO_BOOLEAN` from Tier 2 (runtime dispatch) to Tier 1 (inline in VM loop) would speed up boolean-heavy control flow.

**Result:** Performance regression in CI benchmarks. Likely the same instruction cache pressure issue as PR #78.

**Status:** Closed.

### Opcode Consolidation (PR #74)

**Hypothesis:** Consolidating redundant opcodes would simplify the VM and reduce dispatch overhead.

**Result:** Light regression. PR review identified that the "redundant" opcodes actually provided useful specialization that the consolidation removed.

**Status:** Closed.

## Remaining Work

### Native Module Loading

**Priority:** High

The runtime bridge is still used for all module imports. This is one of the last major bridge dependencies. Native module loading would:

- Load `.sbc` files directly in the VM
- Resolve imports/exports at the bytecode level
- Eliminate the interpreter bridge for module operations

**Relevant files:** `Goccia.Modules.Resolver.pas` (current resolver), `Souffle.Bytecode.Module.pas` (module structure), `Goccia.Runtime.Operations.pas` (bridge code).

### Async/Await Native Support

**Priority:** High

Async functions and `await` expressions currently delegate to the interpreter via the bridge. Native support requires:

- Microtask queue integration with the VM loop
- Promise resolution within VM execution
- Async function state machine compilation (suspend/resume points)

**Relevant files:** `Goccia.MicrotaskQueue.pas` (singleton FIFO queue), `Goccia.Values.AsyncFunctionValue.pas` (interpreter async functions), `Goccia.Runtime.Operations.pas` (`ResolveAsyncThrow`).

### Decorator Evaluation

**Priority:** Medium

Classes with decorators are deferred to the interpreter via `FPendingClasses` / `GOCCIA_EXT_EVAL_CLASS`. Native decorator evaluation requires:

- Compiling decorator expressions to bytecode
- Implementing the three-phase decorator protocol (evaluate → call → apply) in the VM
- Auto-accessor compilation (getter/setter pairs for backing fields)

**Relevant files:** `Goccia.Evaluator.pas` (`EvaluateClassDefinition`), `Goccia.Evaluator.Decorators.pas` (helper classes), `Goccia.Compiler.Statements.pas` (`CompileClassDeclaration`, `IsSimpleClass`).

### GC Optimization

**Priority:** Separate track (not blocked by VM work)

The garbage collector has known limitations during VM execution:

- Automatic collection is disabled during VM execution to avoid sweeping stack-held objects not yet in VM registers
- Both BenchmarkRunner and TestRunner call `Collect` after each file as a workaround
- Incremental or generational collection could allow safe collection during VM execution
- Memory compaction was attempted (PR #84) and deferred — worth revisiting

**Relevant files:** `GarbageCollector.Generic.pas`, `GarbageCollector.Managed.pas`, `Goccia.Engine.Backend.pas` (disables/restores `Enabled` flag).

### Minor Items

**Tombstone compaction for TOrderedStringMap** (Low priority) — The `Remove` method leaves `DELETED_SLOT` tombstones. `Compact` exists and runs during `Grow`, but tracking `FDeletedCount` with threshold-triggered compaction would keep probe chains bounded for delete-heavy workloads. Relevant file: `OrderedStringMap.pas`.

**Map unit naming** (Low priority) — `BaseMap.pas`, `OrderedMap.pas`, `HashMap.pas`, `OrderedStringMap.pas` should be renamed to `Goccia.Map.Base.pas`, `Goccia.Map.Ordered.pas`, `Goccia.Map.Hash.pas`, `Goccia.Map.OrderedString.pas` to follow the project's dot-separated unit naming convention. Pure refactoring.

## Key Insights

1. **Profile before guessing.** The `TScopeMap` regression (2.7× slowdown) was only found via macOS `sample` profiling. CI benchmarks showed the symptom (−30% to −42%) but not the cause. Always profile with `sample` (macOS) or `callgrind` (Linux) before making optimization decisions.

2. **Cache is king.** The 16-byte `TSouffleValue` (down from 26) improved performance because more values fit in cache lines. But going to 8 bytes with reference pools was worse because the pool indirection added memory accesses that negated the cache benefit.

3. **String operations are already fast in FPC.** String interning, `typeof` caching, and similar strategies don't help because FPC's COW (copy-on-write) semantics and inline short strings are already efficient. Do not attempt string interning or string caching again.

4. **Code size matters.** Inlining Tier 2 ops into the VM loop (PR #78, −18%) demonstrated that instruction cache pressure can outweigh dispatch savings. Keep the VM loop compact. Promoting individual opcodes to Tier 1 (PR #88) showed the same effect.

5. **Bridge elimination is the biggest win.** Native class construction (PR #87) showed clear improvements. The remaining bridge dependencies (modules, async/await, decorators) are the most impactful optimization targets.

6. **Hash map specialization matters for hot paths.** Generic DJB2 over raw bytes was 2× slower than multiplicative hash for pointer keys. Specialize data structures for their actual key types. But avoid linear-scan alternatives — the `TScopeMap` incident proved that even small N doesn't justify O(n) lookup on hot paths.

7. **Run benchmarks on CI, not locally.** Local benchmark results have high variance. CI provides more consistent environments. The workflow is: implement → push → let CI run benchmarks → analyze the CI results.
