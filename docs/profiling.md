# Bytecode Profiling

*For contributors analyzing bytecode VM performance.*

## Executive Summary

- **Language-level profiling** — Operates inside the VM dispatch loop, providing data external profilers cannot see
- **Four modes** — `--profile=opcodes` (histogram + pair frequency + scalar hit rate), `--profile=functions` (per-function timing + allocations), `--profile=all` (both), and `--profile=compile` (lex, parse and compile time and the compiler's work per source function, before execution)
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

# The parse and compile phase: time and compiler work per source function
./build/GocciaRunner script.js --profile=compile

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

Total time counts each outermost activation once. A call that starts while the same function already has a frame on the profiling stack (direct or mutual recursion, or a re-entry through a callback such as `arr.map(f)` inside `f`) adds nothing to the function's total, because the outer frame's time already contains it. The self times add up to the profiled time, and no function's total exceeds it; when all the work runs inside `<module>`, no total exceeds `<module>`'s. Work that runs after `<module>` returns, such as a promise job or a host callback, is outside `<module>`'s total. Calls counts every frame, including each resumption of a generator or async function after a `yield` or `await`. A proper tail call ends the caller's frame before the callee's starts: the caller's row keeps the time up to the call, the callee's work is not in the caller's total, and the flame graph shows the callee in the caller's place.

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

## Compile Profile (`--profile=compile`)

The execution modes start counting when the first instruction runs. The compile profile covers what comes before: lexing, parsing and compiling each file, including imported modules, and leaving out the engine's own bootstrap code. Code compiled at run time is reported as files of its own, one per distinct source: `<eval>#N` for `eval` (direct and indirect) and `<Function>#N` for the `Function` constructor, numbered in the order the sources were first seen, its functions keyed by their position in that code. The `Function` constructor first parses its parameters and its body on their own, to validate them; those parses are `<Function:params>#N` and `<Function:body>#N`, with the same number. The same source compiled again adds to its own records. It answers which function of a file is slow to parse or compile, and which compiler operation makes it slow, without an external profiler. `all` does not include it: `--profile-deterministic` captures `all` for the CI benchmark comparisons, and wall-clock parse and compile times would put run-to-run noise into those artifacts ([ADR 0164](adr/0164-compile-phase-profile.md)).

### What is recorded

One record per source function, keyed by its file and the line and column of its node, so its parse and its compile meet in one record; the top level of a file is `<module>`. A class's field initializers, static field initializers and static blocks have no node of their own: they share one record at the class, and their instructions add up. The compiler keeps a column in 16 bits, so on a line longer than 65,535 characters two functions exactly 65,536 columns apart share a record. Each record holds:

- **Lex, Parse, Compile** — self time: the lexer's scan time, the rest of the parse, and the compile, minus the time spent in nested functions.
- **Tokens, Nodes, Instrs** — the tokens the lexer scanned while the function was open, the expressions and statements compiled, and the instructions of its finished bytecode: the size the counters are compared with. Tokens a lookahead scans before the function opens (an arrow function's parameters, read while the parser decides it is one) count for the enclosing function, with their lex time.
- **Counters** — the compiler's and the parser's expensive operations, counted while the function was being parsed or compiled:

| Counter | JSON key | Counts |
|---|---|---|
| Nodes compiled | `nodesCompiled` | Expressions and statements compiled (`CompileExpressionNode`, `CompileStatementNode`) |
| Constant-emit attempts | `constantEmitAttempts` | Constant-folding attempts: the one `CompileExpressionNode` makes for an expression, unless an earlier attempt already showed it is not constant, and those of `TryEmitConstantExpression` (the unary fold, strict-type simplification) |
| Constant-evaluation visits | `constantEvaluationVisits` | Steps of the constant-evaluation walk over an operand subtree (`TryEvaluateConstantExpression`) |
| Expression-type visits | `expressionTypeVisits` | Steps of type inference over an expression subtree (`ExpressionType`) |
| Number-proof visits | `numberProofVisits` | `HasExactNumberProof` checks of an operand |
| Local lookups, probes | `localLookups`, `localLookupProbes` | Name lookups among a function scope's locals, and the locals each examined (1 through the hashed index, the entries scanned without it, plus each shadowed local stepped over) |
| Upvalue resolutions, list probes | `upvalueResolutions`, `upvalueListProbes` | `ResolveUpvalue`, once per function scope it climbs, and the upvalue entries scanned for an existing capture |
| Local-list scans | `localListScans` | Locals visited by walks over a scope's list of locals: the case-clause resets, relinking names after a rename, suspending bindings for a `finally` block |
| Parameter-name probes | `parameterNameProbes` | Lookups in a function's hashed set of parameter names: one per name it records and one per test of a name |
| Binding-name lookups, probes | `bindingNameLookups`, `bindingNameProbes` | Lookups in a list of unique binding names (a pattern's or a declaration's names in the compiler, declaration checks and the duplicate-parameter check in the parser), and the names each compared: 1 once the list is hashed (16 names or more), the names scanned below that |
| Direct-eval record probes | `directEvalRecordProbes` | Name lookups in the direct-eval binding record index |
| Type-argument probe tokens | `typeArgumentProbeTokens` | Tokens visited by constructor lookahead and failed-probe indexing |

AST nodes are also counted by kind: for each function and each file in the JSON, and for the whole run in the text report.

### Reading the report

```text
Compile Profile Functions (self time, top 2 of 2):
  Lex          Parse        Compile         Tokens     Nodes    Instrs  Function                 Location
  6.23ms       7.49ms       158.96ms         20008      8002     25242  g                        switch-2000.js:2:11
  13.58µs     77.36µs     0.73ms              17         6        12  <module>                 switch-2000.js

Compile Profile Work (per node: count / nodes compiled; function: the one with the highest count):
  Counter                               Total   Per node     In function   Per node  Function
  Local lookups                         16032        2.0           16008        2.0  g switch-2000.js:2:11
  Depth-reset local scans             8004000      999.5         8004000     1000.2  g switch-2000.js:2:11
```

A linear compile does a bounded amount of each kind of work per node, whatever the size of the function. Over the 1,301 functions of 20 nodes or more in 676 KB of this repository's `tests/built-ins`, every counter stayed at or below 3 per node except local lookups (at most 12) and local lookup probes (at most 35). A counter whose **per node** figure is in the hundreds, or grows when the same shape gets bigger, is the superlinear spot, and the function next to it is where it happens. Profile two sizes of the input and compare: a figure that doubles when the input doubles is quadratic work. The excerpt above is [#1921](https://github.com/frostney/GocciaScript/issues/1921), a `switch` of 2,000 case clauses that each declare a `const`, profiled before its fix: every clause walked every local of the function, and with 4,000 clauses the figure was 2,000 per node. The fix removed that walk; its successor is counted as `Local-list scans`, which reads 0.5 per node on the same input.

Lookups are the leaves of the walks. When a walk counter (expression-type or constant-evaluation visits) is high, the lookups it makes rise with it, and the largest absolute count can be a lookup counter: in [#1937](https://github.com/frostney/GocciaScript/issues/1937), long operator chains summing captured bindings, upvalue list probes had the largest count, while upvalue resolutions grew in step with the constant-evaluation visits (61 per node each) and expression-type visits stood at 120 per node. The walk is the cause and the lookups are its leaves: fix the walk first. A function whose parse or compile time per token or per node is high while every counter is low is spending its time in work the counters do not cover.

The profile reads the clock at each switch between functions. With it on, the 676 KB source above parsed about 6% and compiled about 9% slower. The counters are exact.

## JSON Export

`--profile-output=path.json` writes the profiling data as JSON. Every key is always present, but only the enabled modes collect data: `opcodes`, `opcodePairs`, and `scalarFastPath` stay empty or zero without `--profile=opcodes` or `all`, `functions` stays empty without `--profile=functions` or `all`, and `compile` has empty lists and zero counters without `--profile=compile`. `scalarFastPath` leaves out `hitRate` when its `total` is 0. A function's `selfTimeNs` and `totalTimeNs` have the console report's meaning, so `totalTimeNs` counts each outermost activation once:

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
  ],
  "compile": {
    "files": [
      {"sourceFile": "script.js", "lexTimeNs": 6240000, "parseTimeNs": 7570000,
       "compileTimeNs": 159690000, "functions": 2, "tokens": 20025,
       "instructions": 25254, "counters": {"nodesCompiled": 8008, ...},
       "nodeKinds": {"SwitchStatement": 1, "VariableDeclaration": 2001, ...}}
    ],
    "functions": [
      {"name": "g", "sourceFile": "script.js", "line": 2, "column": 11,
       "lexTimeNs": 6230000, "parseTimeNs": 7490000, "compileTimeNs": 158960000,
       "tokens": 20008, "instructions": 25242,
       "counters": {"nodesCompiled": 8002, "localLookups": 16008, ...},
       "nodeKinds": {"SwitchStatement": 1, "VariableDeclaration": 2000, ...}},
      ...
    ],
    "nodeKinds": [{"kind": "VariableDeclaration", "count": 2001}, ...],
    "counters": {"nodesCompiled": 8008, ...}
  }
}
```

`compile.functions` is sorted by self time (lex + parse + compile), `counters` holds every counter of the [table above](#what-is-recorded) under its JSON key, `nodeKinds` the nodes compiled by kind (kinds with none left out), and `compile.counters` and `compile.nodeKinds` cover every file reported.

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

`--profile-format=flamegraph --profile-output=flamegraph.txt` (with `--profile=functions` or `all`) writes collapsed stack traces. Opcode-only and compile profiling have no stacks: with `--profile=opcodes` or `--profile=compile` the file is written empty and the run still exits 0 ([#1279](https://github.com/frostney/GocciaScript/issues/1279)). The output is viewable in [speedscope](https://speedscope.app) (drag and drop) or renderable to SVG via [FlameGraph](https://github.com/brendangregg/FlameGraph):

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
| `Goccia.Profiler.Compile.pas` | The compile profile: per-function records, counters, nodes by kind, and its console output and JSON section |
| `Goccia.Bytecode.OpCodeNames.pas` | Opcode ordinal to human-readable name (cold path, report generation only) |

The profiler follows the same singleton pattern as `Goccia.Coverage.pas`: `Initialize`/`Instance`/`Shutdown`, boolean `Enabled` flag. The instance is per-thread, so parallel workers never share profiler state. Opcode counting and pair tracking use static arrays (no heap allocation in the hot path). Function profiling uses a timing stack for correct self-time calculation across recursive calls, and counts each function's frames on that stack so that only its outermost frame adds to its total time. Allocation tracking hooks into `TGocciaValue.AfterConstruction` via `GThreadPolls.ProfilingAllocations`, a flag in the per-thread poll word ([Thread Variables on Hot Paths](core-patterns.md#thread-variables-on-hot-paths)), attributing each allocation to the function on top of the profiler's timing stack.

When profiling is disabled, the remaining cost is small but not zero: each allocation reads the per-thread poll word `GThreadPolls.Any`, which it reads anyway for the timeout and the instruction limit, and each native VM entry (`ExecuteClosureRegistersInternal`) saves that flag, sets it from the VM's function-profiling state, and restores it on exit. A callback-heavy A/B run (6M `map`/`reduce` callbacks) put that save/restore within run-to-run noise.

The compile profile costs nothing measurable when it is off. The parser and the compiler read its per-thread instance once, when a parser is created or a compile starts, and keep it in a field that every function scope inherits, so each counting site is a nil test of a field already at hand, with no thread-variable lookup ([Thread Variables on Hot Paths](core-patterns.md#thread-variables-on-hot-paths)). Each function the parser opens adds two tests, and each template switch in the compiler one. An A/B of production builds over a 676 KB source (three copies of each binary, alternating order, 30 runs each) put the sum of lex, parse and compile at the same median as the build without the profile: 211.1 ms against 211.6 ms, within the 209.8 to 213.3 ms spread between identical copies.
