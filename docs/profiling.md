# Bytecode Profiling

*For contributors analyzing bytecode VM performance.*

## Executive Summary

- **Language-level profiling** — Operates inside the VM dispatch loop, providing data external profilers cannot see
- **Three modes** — `--profile=opcodes` (histogram + pair frequency + scalar hit rate), `--profile=functions` (per-function timing + allocations), `--profile=all` (both)
- **Export formats** — JSON (`--profile-output=path.json`) and collapsed flame graph (`--profile-format=flamegraph`)
- **Disabled-path cost** — Production dispatch omits the opcode profiler; the dispatch loop's instrumented fetch path (and its boolean guards) is used when opcode profiling is on. Measure enabled-mode overhead on the workload being profiled.
- **Corpus profile review** — Main CI publishes aggregate and detailed test262
  profile reports for trend review; see [test262 profile report contract](test262.md#profile-report-contract)

## Overview

The `--profile` option on GocciaRunner enables language-level profiling of the bytecode VM. It operates inside the dispatch loop, providing data that external profilers (like `sample` or `callgrind`) cannot see — which opcodes execute, which JS functions are hot, and where the VM allocates.

Production dispatch does not record opcodes. `--profile=opcodes` and `--profile=all` select the dispatch loop's instrumented fetch path, which carries the per-instruction profiler guard; `--profile=functions` alone keeps the production path and records on calls, returns and allocations. Coverage, a stop-IP or an instruction limit selects the instrumented path on its own.

## CLI Usage

```bash
# Opcode profiling: histogram, pair frequency, scalar hit rate
./build/GocciaRunner script.js --profile=opcodes

# Function profiling: self-time, total-time, call count, allocations
./build/GocciaRunner script.js --profile=functions

# Both
./build/GocciaRunner script.js --profile=all

# JSON export (every section key is present; only the modes --profile enables are populated)
./build/GocciaRunner script.js --profile=all --profile-output=profile.json

# Deterministic benchmark profile capture for CI comparisons
./build/GocciaBenchmarkRunner benchmarks/numbers.js --profile-deterministic --profile-output=numbers-profile.json

# Stdin works too
echo 'const x = 1 + 2; x;' | ./build/GocciaRunner --profile=all
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

### Shape Saturation (`--profile=opcodes`)

Counts the times an object's layout could not be fully interned in the shape system: `Depth-limit prefixes` when a layout reached the transition depth limit (64 properties), and `Table-capacity events` when the realm's shape table was full. The object keeps the longest interned prefix of its layout, so property reads past that prefix lose the inline-cache fast path. An event is counted each time such a layout is interned again, which a cache site does when the shape it holds does not match; a site that keeps hitting on the prefix it already holds does not intern and does not count. The section, headed `Shape Saturation (degraded property-read caching):`, is printed only when either count is non-zero.

### Function Profile (`--profile=functions`)

Per-function breakdown: self-time (exclusive — time in the function minus time in callees), total-time (inclusive — time in the function and its callees), call count, and allocation count (heap-allocated `TGocciaValue` instances created during that function's execution).

Total time counts each outermost activation once. A call that starts while the same function already has a frame on the profiling stack (direct or mutual recursion, or a re-entry through a callback such as `arr.map(f)` inside `f`) adds nothing to the function's total, because the outer frame's time already contains it. The self times add up to the profiled time, and no function's total exceeds it; when all the work runs inside `<module>`, no total exceeds `<module>`'s. Work that runs after `<module>` returns, such as a promise job or a host callback, is outside `<module>`'s total. Calls counts every frame, including each resumption of a generator or async function after a `yield` or `await`. A proper tail call ends the caller's frame before the callee's starts, so a tail-calling function's time is in its callee's row, and the flame graph shows the callee in its place.

Allocation counts belong to the active profiled function, including allocations
made by native built-ins on its behalf. Collection follows each native entry
into the VM, so host-invoked benchmark callbacks and resumed generators or async
functions are included after module execution finishes. Nested VM entries
restore their caller's allocation-profiling state on return or exception;
ordinary bytecode calls use the existing function profiling stack.

With `--profile-deterministic`, BenchmarkRunner resets the counters after module
registration and executes each registered benchmark once. The resulting
function allocation counts describe that benchmark execution, including its
setup and teardown, rather than the discarded registration phase.

For this script:

```js
const fib = (n) => n < 2 ? n : fib(n - 1) + fib(n - 2);
[26, 27, 28, 29, 30].forEach((n) => { fib(n); });
```

```text
Function Profile:
  Self Time    Total Time      Calls     Allocs  Function                       Location
  4782.60ms    4782.60ms     6413529          0  fib                            script.js:1
  64.14µs     4782.68ms           1         13  <module>                       script.js:1
  10.35µs     4782.61ms           5          0  <arrow>                        script.js:2
```

**How to read it:** Sort by self-time to find the functions where the VM spends the most time. `fib` calls only itself, so its self and total times are equal; both stay within `<module>`'s total. The `Allocs` column shows GC pressure per function — high allocation counts relative to call counts indicate boxing overhead (register values being unnecessarily converted to heap objects). Functions named `<module>` are top-level script code; `<arrow>` and `<anonymous>` are unnamed callbacks.

## JSON Export

`--profile-output=path.json` writes the profiling data as JSON. Every key is always present, but only the enabled modes collect data: `opcodes`, `opcodePairs`, and `scalarFastPath` stay empty or zero without `--profile=opcodes` or `all`, and `functions` stays empty without `--profile=functions` or `all`. `scalarFastPath` leaves out `hitRate` when its `total` is 0. A function's `selfTimeNs` and `totalTimeNs` have the console report's meaning, so `totalTimeNs` counts each outermost activation once:

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
  "shapeSaturation": {"depthLimitPrefixes": 0, "tableCapacityEvents": 0},
  "functions": [
    {"name": "fib", "sourceFile": "script.js", "line": 1, "calls": 6413529,
     "selfTimeNs": 4782600948, "totalTimeNs": 4782600948, "allocations": 0},
    ...
  ]
}
```

## test262 corpus profiles

Full-corpus test262 profile reports reuse the bytecode profiler data described
above, but the report contract lives in [test262 Harness Contract](test262.md#profile-report-contract).
Like the benchmark profile aggregate ([Benchmarks — CI Integration](benchmarks.md#ci-integration)),
it adds up each function's calls, self time, total time and allocations over
the runs it covers; each run's total counts every outermost activation once.
Review the aggregate report before opening detailed profiles, and use the
trend to propose compiler, bytecode, AST, parser, allocation, property-access,
or call-frame improvements.

## Flame Graph Export

`--profile-format=flamegraph --profile-output=flamegraph.txt` (with `--profile=functions` or `all`) writes collapsed stack traces. Opcode-only profiling has no stacks: with `--profile=opcodes` the file is written empty and the run still exits 0 ([#1279](https://github.com/frostney/GocciaScript/issues/1279)). The output is viewable in [speedscope](https://speedscope.app) (drag and drop) or renderable to SVG via [FlameGraph](https://github.com/brendangregg/FlameGraph):

```bash
./build/GocciaRunner script.js --profile=functions --profile-format=flamegraph --profile-output=flamegraph.txt

# View in browser
open https://speedscope.app  # drag flamegraph.txt into the page

# Or render to SVG
flamegraph.pl flamegraph.txt > flamegraph.svg
```

Each line is a semicolon-separated call stack with a self-time weight in whole microseconds (a stack with less than 1 µs is left out). The weights are self times, never total times, so a recursive function's time is counted once however deep it recurses. For the script in [Function Profile](#function-profile---profilefunctions), the three shallowest stacks are:

```text
<module>;<arrow>;fib 4
<module>;<arrow>;fib;fib 8
<module>;<arrow>;fib;fib;fib 14
```

This shows the JS function call hierarchy that external profilers like `sample` cannot see — `sample` would show all these as the same recursive `ExecuteClosureRegistersInternal` frame.

## Combining with `sample` (macOS)

The profiler and `sample` are complementary. The profiler sees inside the dispatch loop (opcodes, JS functions, allocations). `sample` sees outside it (FPC runtime overhead, memory management, exception frames).

Run both on the same production binary for the full picture:

```bash
# Build production
./build.pas --prod runner

# Profiler run
./build/GocciaRunner script.js --profile=all

# sample run (no --profile, to avoid measuring profiling overhead)
./build/GocciaRunner script.js &
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

The profiler follows the same singleton pattern as `Goccia.Coverage.pas`: `Initialize`/`Instance`/`Shutdown`, boolean `Enabled` flag. The instance is per-thread, so parallel workers never share profiler state. Opcode counting and pair tracking use static arrays (no heap allocation in the hot path). Function profiling uses a timing stack for correct self-time calculation across recursive calls, and counts each function's frames on that stack so that only its outermost frame adds to its total time. Allocation tracking hooks into `TGocciaValue.AfterConstruction` via `GThreadPolls.ProfilingAllocations`, a flag in the per-thread poll word ([Thread Variables on Hot Paths](core-patterns.md#thread-variables-on-hot-paths)), attributing each allocation to the function on top of the profiler's timing stack.

When profiling is disabled, the remaining cost is small but not zero: each allocation reads the per-thread poll word `GThreadPolls.Any`, which it reads anyway for the timeout and the instruction limit, and each native VM entry (`ExecuteClosureRegistersInternal`) saves that flag, sets it from the VM's function-profiling state, and restores it on exit. A callback-heavy A/B run (6M `map`/`reduce` callbacks) put that save/restore within run-to-run noise.
