# Bytecode Profiling

*For contributors analyzing bytecode VM performance.*

## Executive Summary

- **Language-level profiling** — Operates inside the VM dispatch loop, providing data external profilers cannot see
- **Three modes** — `--profile=opcodes` (histogram + pair frequency + scalar hit rate), `--profile=functions` (per-function timing + allocations), `--profile=all` (both)
- **Export formats** — JSON (`--profile-output=path.json`) and collapsed flame graph (`--profile-format=flamegraph`)
- **Near-zero overhead** — Boolean guard on dispatch loop; ~1% for opcode counting, ~3% for function timing

## Overview

The `--profile` flag on GocciaScriptLoader enables language-level profiling of the bytecode VM. It operates inside the dispatch loop, providing data that external profilers (like `sample` or `callgrind`) cannot see — which opcodes execute, which JS functions are hot, and where the VM allocates.

Profiling implies `--mode=bytecode` automatically. Near-zero overhead when disabled (boolean guard on the dispatch loop, same pattern as `--coverage`). The guard branches are consistently not-taken and well-predicted, but they are present in the compiled binary.

## CLI Usage

```bash
# Opcode profiling: histogram, pair frequency, scalar hit rate
./build/GocciaScriptLoader script.js --profile=opcodes

# Function profiling: self-time, total-time, call count, allocations
./build/GocciaScriptLoader script.js --profile=functions

# Both
./build/GocciaScriptLoader script.js --profile=all

# JSON export (includes all sections regardless of console mode)
./build/GocciaScriptLoader script.js --profile=all --profile-output=profile.json

# Deterministic benchmark profile capture for CI comparisons
./build/GocciaBenchmarkRunner benchmarks/numbers.js --profile-deterministic --profile-output=numbers-profile.json

# Stdin works too
echo 'const x = 1 + 2; x;' | ./build/GocciaScriptLoader --profile=all
```

## Report Sections

### Opcode Histogram (`--profile=opcodes`)

Counts how many times each bytecode opcode executes. Sorted by frequency descending.

```text
Opcode Profile:
  Opcode                                             Count        %
  OP_GET_LOCAL                                       54728    19.2%
  OP_LOAD_INT                                        43782    15.4%
  OP_RETURN                                          21892     7.7%
  ...
  Total                                             284582
```

**How to read it:** The percentages show where the VM spends its instruction budget. A large share of `OP_CALL`/`OP_RETURN` indicates call-overhead-bound code. Elevated `OP_TO_PRIMITIVE` counts suggest the compiler is emitting generic opcodes where typed opcodes would suffice. Frequent `OP_GET_PROP_CONST` points to property-access-heavy code that might benefit from inline caching.

### Opcode Pair Frequency (`--profile=opcodes`)

Counts how often each two-instruction sequence executes. Shows the top 20 pairs.

```text
Opcode Pairs (top 20):
  Previous                                      Current                                            Count        %
  OP_GET_LOCAL                              -> OP_LOAD_INT                                     87918017    15.4%
  OP_LOAD_INT                               -> OP_LTE                                          43959011     7.7%
  OP_LTE                                    -> OP_JUMP_IF_FALSE                                43959011     7.7%
  ...
```

**How to read it:** Hot pairs are superinstruction candidates — two opcodes that could be fused into one to eliminate a dispatch. A pair at 7%+ of total is worth fusing. Cross-frame pairs (e.g., `RETURN → MOVE`) cannot be fused because they span call boundaries.

### Scalar Fast-Path Hit Rate (`--profile=opcodes`)

For the generic arithmetic and comparison opcodes (`OP_ADD`, `OP_SUB`, `OP_MUL`, `OP_DIV`, `OP_MOD`, `OP_POW`, `OP_LT`, `OP_GT`, `OP_LTE`, `OP_GTE`), tracks how often the `RegisterIsNumericScalar` fast path hits versus falls through to the slow (boxing/polymorphic) path.

```text
Scalar Fast-Path:
  Hits:      109897525 (100.0%)
  Misses:            0 (  0.0%)
  Total:     109897525
```

**How to read it:** A high hit rate (>95%) means the compiler should be emitting typed opcodes (`OP_ADD_INT`, `OP_LTE_INT`, etc.) instead of generic ones, since the runtime polymorphism is never exercised. A lower hit rate indicates genuinely mixed-type arithmetic where generic opcodes are necessary.

### Function Profile (`--profile=functions`)

Per-function breakdown: self-time (exclusive — time in the function minus time in callees), total-time (inclusive), call count, and allocation count (heap-allocated `TGocciaValue` instances created during that function's execution).

```text
Function Profile:
  Self Time    Total Time      Calls     Allocs  Function                       Location
  19.78s       443.42s      43959011   13584073  fib                            script.js:1
  91.00µs     19.78s              1         22  <module>                       script.js:1
  7.00µs      19.78s              5          8  <arrow>                        script.js:2
```

**How to read it:** Sort by self-time to find the functions where the VM spends the most time. The `Allocs` column shows GC pressure per function — high allocation counts relative to call counts indicate boxing overhead (register values being unnecessarily converted to heap objects). Functions named `<module>` are top-level script code; `<arrow>` and `<anonymous>` are unnamed callbacks.

## JSON Export

`--profile-output=path.json` writes all profiling data as JSON:

```json
{
  "opcodes": [
    {"opcode": "OP_GET_LOCAL", "count": 54728, "percentage": 19.2},
    ...
  ],
  "opcodePairs": [
    {"prev": "OP_GET_LOCAL", "cur": "OP_LOAD_INT", "count": 87918017},
    ...
  ],
  "scalarFastPath": {"hits": 109897525, "misses": 0, "total": 109897525, "hitRate": 100.0},
  "functions": [
    {"name": "fib", "sourceFile": "script.js", "line": 1, "calls": 43959011,
     "selfTimeNs": 19780000000, "totalTimeNs": 443420000000, "allocations": 13584073},
    ...
  ]
}
```

## Flame Graph Export

`--profile-format=flamegraph --profile-output=flamegraph.txt` writes collapsed stack traces, viewable in [speedscope](https://speedscope.app) (drag and drop) or renderable to SVG via [FlameGraph](https://github.com/brendangregg/FlameGraph):

```bash
./build/GocciaScriptLoader script.js --profile=functions --profile-format=flamegraph --profile-output=flamegraph.txt

# View in browser
open https://speedscope.app  # drag flamegraph.txt into the page

# Or render to SVG
flamegraph.pl flamegraph.txt > flamegraph.svg
```

Each line is a semicolon-separated call stack with a self-time weight in microseconds:

```text
<module>;fib;fib;fib 4170
<module>;fib;fib 1
<module>;fib 1
```

This shows the JS function call hierarchy that external profilers like `sample` cannot see — `sample` would show all these as the same recursive `ExecuteClosureRegistersInternal` frame.

## Combining with `sample` (macOS)

The profiler and `sample` are complementary. The profiler sees inside the dispatch loop (opcodes, JS functions, allocations). `sample` sees outside it (FPC runtime overhead, memory management, exception frames).

Run both on the same production binary for the full picture:

```bash
# Build production
./build.pas --prod loader

# Profiler run
./build/GocciaScriptLoader script.js --profile=all

# sample run (no --profile, to avoid measuring profiling overhead)
./build/GocciaScriptLoader script.js --mode=bytecode &
PID=$!
sleep 0.2
sample "$PID" 10 1 -file sample-output.txt -mayDie
wait $PID
```

Look at `sample`'s "Sort by top of stack" section. Typical categories:

| `sample` function | Category |
|-------------------|----------|
| `ExecuteClosureRegistersInternal` | Dispatch loop itself |
| `FPC_DYNARRAY_*` | Register frame allocation/deallocation |
| `FPC_PUSHEXCEPTADDR`, `FPC_POPADDRSTACK` | try/except per call frame |
| `TObject.InheritsFrom`, `FPC_DO_IS` | RTTI type checks |
| `Math.IsNaN`, `Math.IsInfinite` | Number creation guards |
| `RegisterToValue` | Register→value boxing |
| `SysGetMem`, `SysFreeMem` | Heap allocator |

The profiler's allocation count per function tells you *which JS function* drives the allocations that `sample` attributes to `SysGetMem`/`RegisterToValue`. The opcode histogram tells you *which operations* are inside the 30% of time `sample` attributes to `ExecuteClosureRegistersInternal`.

## Architecture

| Unit | Role |
|------|------|
| `Goccia.Profiler.pas` | Singleton tracker: opcode counts, pair matrix, scalar hit/miss, function profiles with timing stack |
| `Goccia.Profiler.Report.pas` | Console output, JSON export, and collapsed stack export |
| `Goccia.Bytecode.OpCodeNames.pas` | Opcode ordinal to human-readable name (cold path, report generation only) |

The profiler follows the same singleton pattern as `Goccia.Coverage.pas`: `Initialize`/`Instance`/`Shutdown`, boolean `Enabled` flag, zero overhead when disabled. Opcode counting and pair tracking use static arrays (no heap allocation in the hot path). Function profiling uses a timing stack for correct self-time calculation across recursive calls. Allocation tracking hooks into `TGocciaValue.AfterConstruction` via a global `GProfilingAllocations` boolean, attributing each allocation to the function on top of the profiler's timing stack.
