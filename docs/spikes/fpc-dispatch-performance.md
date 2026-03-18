# FreePascal Method Dispatch Performance

Comparing virtual, interface, and manual VMT dispatch across inheritance depths

**Test Environment:** FPC 3.2.2, -O3, x86_64 Linux | 100M iterations per test | Best of 5 runs

## 1. Mode Comparison: ObjFPC vs Delphi

Assembly diff between binaries compiled with `-Mobjfpc` and `-Mdelphi` shows zero divergence in generated code (4,119 lines of instructions compared). The only difference is a string literal containing the mode name. Both modes produce byte-identical virtual dispatch sequences, interface thunks, and call-site codegen. Mode selection affects parsing and syntax rules only — not performance. Choose whichever fits your codebase conventions.

## 2. Leaf Override Dispatch — Depth Has No Cost

When the override simply replaces the parent method (no `inherited` call), dispatch cost is constant regardless of hierarchy depth. The VMT stores a direct pointer to the leaf method; the runtime never walks the class chain.

| Mechanism | Depth 1 | Depth 2 | Depth 5 | Scaling d1→d5 |
|---|---|---|---|---|
| Virtual (override) | 2.50 ns | 2.50 ns | 2.53 ns | +1.0% |
| Interface | 2.53 ns | 2.42 ns | 2.49 ns | −1.6% |
| Manual VMT (flat) | 2.53 ns | — | — | — |

All three mechanisms converge to ~2.5 ns per call. Depth-related variation is within measurement noise (±3%).

## 3. Inherited Chains — Linear Cost Scaling

When each level calls `inherited` (i.e. the child invokes the parent's method before doing its own work), cost scales linearly with depth. Each level adds one static call plus one return instruction.

| Mechanism | Depth 1 | Depth 2 | Depth 5 | Multiplier d5 |
|---|---|---|---|---|
| Virtual + inherited | 2.50 ns | 7.24 ns | 14.31 ns | 5.7x |
| Manual VMT (direct chain) | 2.53 ns | 2.54 ns | 14.28 ns | 5.6x |
| Manual VMT (indirect walk) | — | 7.32 ns | 14.44 ns | 5.7x |

Note: Manual VMT depth=2 direct chain is fast (2.54 ns) because it inlines both levels into a single function body, avoiding the call chain entirely.

## 4. Assembly Analysis

Inspecting the generated x86_64 assembly for the hot loops reveals why these mechanisms perform identically at a given depth:

| Mechanism | Hot Loop Instructions | Indirection |
|---|---|---|
| Static (non-virtual) | `call <direct_address>` | None (link-time resolved) |
| Virtual | `movq (%r14),%rax ; call *200(%rax)` | 1 load + indirect call at VMT offset |
| Manual VMT | `movq (%rsp),%rax ; call *(%rax)` | 1 load + indirect call at offset 0 |
| Interface | Thunk → virtual dispatch | Thunk adjusts Self, then virtual call |

Virtual and manual VMT emit structurally identical code: one pointer load followed by an indirect call. FPC's virtual method uses a larger VMT (with RTTI, parent references, and interface tables) accessed at offset 200, while the manual VMT is a minimal record accessed at offset 0. In practice this makes no measurable difference because the VMT remains in L1 cache for monomorphic call sites.

The `inherited` keyword compiles to a direct static call to the parent class's method — it does not re-enter the VMT. This means each `inherited` level contributes a fixed ~2.5 ns cost (one call + one ret) regardless of whether the hierarchy uses virtual methods, interfaces, or manual VMTs.

## 5. Practical Recommendations

**Hierarchy depth is free for dispatch.** Whether your class sits 1 or 5 levels deep, a virtual or interface method call costs the same ~2.5 ns. The VMT offset is a compile-time constant; the runtime never traverses the inheritance chain. Design your hierarchies for clarity, not for dispatch performance.

**Avoid inherited chains in hot paths.** Each `inherited` call adds ~2.5 ns of linear overhead. At depth 5 this means a 5.7x cost multiplier versus a flat override. If a deep subclass needs parent behaviour, consider composing the logic within a single method body rather than chaining `inherited` calls up the hierarchy.

**Interfaces are as fast as virtual methods.** FPC's interface dispatch adds negligible overhead (±2% vs virtual). Use interfaces freely for abstraction without worrying about a dispatch penalty.

**Manual VMTs offer layout control, not speed.** A hand-rolled function pointer table generates the same indirect call pattern as FPC's built-in virtual dispatch. Its advantage lies in controlling memory layout (stack-allocatable records, no TObject overhead, cache-friendly field placement) rather than reducing call overhead.

**ObjFPC vs Delphi mode: zero performance impact.** Both modes produce byte-identical machine code. Choose based on syntax preference and codebase conventions.

## Methodology

Each test calls a trivial method (accumulator increment) in a tight loop of 100 million iterations. Timing uses `clock_gettime(CLOCK_MONOTONIC)`. Five runs are executed per test and the best time is reported to minimise OS scheduling noise. The accumulator result is printed to prevent dead-code elimination. All tests compiled with FPC 3.2.2 at -O3 optimisation level targeting x86_64 Linux. Source code available in `bench_dispatch.pas` and `bench_dispatch2.pas`.
