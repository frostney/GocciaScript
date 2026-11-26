# Bytecode Profiling

*For contributors analyzing bytecode VM performance.*

## Executive Summary

- **Language-level profiling** — Runs every instruction through the interpreter's counted loop (`Goccia.Profile`), providing data external profilers cannot see; the fast loop and compiled code stay off while it runs, so the times are interpreter times
- **Four modes** — `--profile=opcodes` (histogram + pair frequency + scalar hit rate), `--profile=functions` (per-function timing + allocations), `--profile=all` (both), and `--profile=compile` (lex, parse and compile time and the compiler's work per source function, before execution)
- **Export formats** — JSON (`--profile-output=path.json`) and collapsed flame graph (`--profile-format=flamegraph`)
- **Disabled-path cost** — With profiling off, the fast loop and the JIT run as usual; what remains is a flag test at each frame push and at each object made. Measure enabled-mode overhead on the workload being profiled.
- **Corpus profile review** — Main CI publishes aggregate and detailed test262
  profile reports for trend review; see [test262 profile report contract](test262.md#profile-report-contract)

## Overview

The `--profile` option on GocciaRunner and GocciaBenchmarkRunner enables language-level profiling of the engine. It records inside the interpreter, providing data that external profilers (like `sample` or `callgrind`) cannot see — which opcodes execute, which JS functions are hot, and which functions make objects.

The engine normally runs hot instructions in its fast loop (`FastLoop` in `Goccia.VM`) and, on Linux x86-64, in compiled code. Under `--profile=opcodes`, `functions` or `all`, every instruction runs through `Execute`'s counted loop instead, which calls the profiler's step hook, and every frame push calls its push hook (`Goccia.Profile`). Coverage and an instruction limit select the counted loop the same way. A profile therefore describes the interpreter: compare its times with each other, not with an unprofiled run, which compiles what gets hot.

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

## Directories and deterministic captures

`GocciaRunner directory --profile=all --profile-output=profile.json` writes
one report covering every entry file, including their imported modules. With
profiling enabled, the runner executes entries one at a time, each on a thread
that records into the main thread's profilers (`TGocciaProfiler.Adopt`), as
BenchmarkRunner does; `--jobs` still controls parallelism when profiling is
off. Directory discovery is sorted. Counts accumulate across
entries, but opcode pairs never join the last instruction of one entry to the
first of another. Runtime function rows retain each template's file and line;
compile records at the same source position accumulate. Flame graphs combine
identical displayed stack paths, as they do for a single file. A script failure
still exits nonzero and retains the profiles of the entries that ran.

For repeatable **Runner** captures, add `--profile-deterministic`. It implies
`--profile=all` unless a mode was specified, installs the deterministic guest
clock and random source, and reports every function and compile timing field
as zero while retaining counts, locations and all JSON keys. It works with
`--profile=compile` too. Its flame graph is empty because zero-time stacks have
no weight. Use normal profiling to measure time. External inputs and granted
host services must also be held constant for repeatable results. Runner's
flag does not change BenchmarkRunner's existing once-per-benchmark capture
behavior described below.

```bash
./build/GocciaRunner scripts/ --profile-deterministic --profile-output=counts.json
./build/GocciaRunner scripts/ --profile=compile --profile-deterministic --profile-output=compile-counts.json
```

## Report Sections

### Opcode Histogram (`--profile=opcodes`)

Counts how many times each bytecode opcode executes, under G2's own opcode names (`TOp` in `Goccia.Instructions` without its `op` prefix). Sorted by frequency descending. The examples in this section come from this script:

```js
const fib = (n) => n < 2 ? n : fib(n - 1) + fib(n - 2);
[20, 21, 22].forEach((n) => { fib(n); });
```

```text
Opcode Profile:
  Opcode                                             Count        %
  LoadI                                             114628    11.8%
  Call                                              114626    11.8%
  Ret                                               114626    11.8%
  GetUpv                                            114625    11.8%
  JNLt                                              114625    11.8%
  SubI                                              114622    11.8%
  CheckHole                                         114622    11.8%
  ...
  Total                                             974328
```

**How to read it:** The percentages show where the interpreter spends its instructions. A large share of `Call`/`Ret` indicates call-overhead-bound code. Frequent `GetProp`/`SetProp` points to property-access-heavy code, which the inline caches serve; frequent `GetGlobal` points to global lookups. `GetUpv` followed by `CheckHole` is a read of a captured `let` or `const` that still needs its TDZ check. The host's own code (the JavaScript preludes) is not counted.

### Opcode Pair Frequency (`--profile=opcodes`)

Counts how often each two-instruction sequence executes. Shows the top 20 pairs.

```text
Opcode Pairs (top 20):
  Previous                                      Current                                            Count        %
  Call                                      -> LoadI                                             114625    11.8%
  LoadI                                     -> JNLt                                              114625    11.8%
  SubI                                      -> Call                                              114622    11.8%
  ...
```

**How to read it:** Hot pairs are fusion candidates — two opcodes that could become one instruction and save a dispatch, as the compare-and-jump forms (`JNLt` and the like) and the immediate forms (`SubI`, `AddI`) already do. Cross-frame pairs (e.g., `Ret → Add`) cannot be fused because they span call boundaries.

### Scalar Fast-Path Hit Rate (`--profile=opcodes`)

For the generic binary arithmetic, bitwise and comparison instructions (`Add`, `Sub`, `Mul`, `Div`, `Mod`, `Pow`, `BitAnd`, `BitOr`, `BitXor`, `Shl`, `Shr`, `UShr`, `Eq`, `Ne`, `SEq`, `SNe`, `Lt`, `Le`, `Gt`, `Ge`), tracks how often both operands are Numbers (a hit) and how often one is not (a miss). The immediate forms and the fused compare-and-jumps are not counted.

```text
Scalar Fast-Path:
  Hits:          57311 (100.0%)
  Misses:            0 (  0.0%)
  Total:         57311
```

**How to read it:** A high hit rate means the arithmetic is monomorphic over Numbers. Misses are strings, BigInts or objects reaching the instruction: string concatenation through `Add`, or operands that need `ToPrimitive`.

### Shape Saturation (`--profile=opcodes`)

The report keeps a section for object layouts that could not be fully interned in a shape system, headed `Shape Saturation (degraded property-read caching):` and printed only when one of its counts is non-zero. G2 records no such event: an object past 64 properties gets a private dictionary shape instead ([G2 design — Objects](engine/design.md#objects)). The section is not printed, and the JSON export's `shapeSaturation` counts are 0.

### Function Profile (`--profile=functions`)

Per-function breakdown: self-time (exclusive — time in the function minus time in callees), total-time (inclusive — time in the function and its callees), call count, and allocation count (the objects made during that function's execution, counted by `NewObject` in `Goccia.Core`; strings and other cells are not counted).

Total time counts each outermost activation once. A call that starts while the same function already has a frame on the profiling stack (direct or mutual recursion, or a re-entry through a callback such as `arr.map(f)` inside `f`) adds nothing to the function's total, because the outer frame's time already contains it. The self times add up to the profiled time, and no function's total exceeds it; when all the work runs inside `<module>`, no total exceeds `<module>`'s. Work that runs after `<module>` returns, such as a promise job or a host callback, is outside `<module>`'s total. Calls counts every function frame, including each resumption of a generator or async function after a `yield` or `await`; a call that only creates a generator is not one, and its first resumption is its first call. A module top level counts once per evaluation, even when top-level `await` suspends and resumes it. Its timing includes each active segment; promise jobs that run while it is suspended are outside its total. A frame ends when a later push reuses its slot or an instruction runs in a frame below it, so a throw that unwinds several frames ends them all. A tail call that replaces its caller's frame (past 1,024 frames, see [G2 — Proper tail calls](engine/README.md#proper-tail-calls)) ends the caller's frame before the callee's starts: the caller's row keeps the time up to the call, the callee's work is not in the caller's total, and the flame graph shows the callee in the caller's place.

Function timing excludes the measured time spent registering templates and
maintaining the profiling stack, including flame-path interning. Returns update
an interned path node in constant time instead of walking every ancestor.
Expanded paths are constructed only for a flame-graph export. Instrumentation
still has a cost: clock reads, enabled-mode dispatch and allocation counters
can perturb execution, so compare elapsed time with an unprofiled run when
measuring profiler overhead. These times are not an estimate of completely
uninstrumented execution.

Allocation counts belong to the innermost profiled function, including objects
made by native built-ins on its behalf; the objects made since the last charge
are charged at each frame push and each frame end. The host's own code (the
JavaScript preludes, whose source names look like `<goccia:console>`) is not
profiled: its time and allocations are its caller's.

With `--profile-deterministic`, BenchmarkRunner resets the counters after module
registration and executes each registered benchmark once. The resulting
function allocation counts describe that benchmark execution, including its
setup and teardown, rather than the discarded registration phase.

For the script in [Opcode Histogram](#opcode-histogram---profileopcodes):

```text
Function Profile:
  Self Time    Total Time      Calls     Allocs  Function                       Location
  17.24ms      17.24ms        114625          0  fib                            script.js:1
  29.40µs     17.27ms             1          3  <module>                       script.js
  9.40µs      17.24ms             3          0  <anonymous>                    script.js:2
```

**How to read it:** Sort by self-time to find the functions where the interpreter spends the most time. `fib` calls only itself, so its self and total times are equal; both stay within `<module>`'s total. The `Allocs` column shows GC pressure per function: Numbers never allocate, so a high count relative to calls means the function makes objects or arrays on each call. A function named `<module>` is top-level script code, located by its file alone; `<anonymous>` is an unnamed function, the arrow callback here.

## Compile Profile (`--profile=compile`)

The execution modes start counting when the first instruction runs. The compile profile covers what comes before: lexing, parsing and compiling each file, including imported modules, and leaving out the engine's own bootstrap code. Code compiled at run time is reported as files of its own. The `Function` constructor's sources are `<Function>#N`, one per distinct source, numbered in the order the sources were first seen, its functions keyed by their position in that code; it first parses its parameters and its body on their own, to validate them, and those parses are `<Function:params>#N` and `<Function:body>#N`, with the same number. Code that `eval` compiles (direct and indirect) is reported under the one file `<eval>`. The same source compiled again adds to its own records. It answers which function of a file is slow to parse or compile, and which compiler operation makes it slow, without an external profiler. `all` does not include it: `--profile-deterministic` captures `all` for the CI benchmark comparisons, and wall-clock parse and compile times would put run-to-run noise into those artifacts ([ADR 0164](adr/0164-compile-phase-profile.md)).

### What is recorded

One record per source function, keyed by its file and the line and column of its node, so its parse and its compile meet in one record; the top level of a file is `<module>`. A class's field initializers, static field initializers and static blocks have no node of their own: they share one record at the class, and their instructions add up. The compiler keeps a column in 16 bits, so on a line longer than 65,535 characters two functions exactly 65,536 columns apart share a record. Each record holds:

- **Lex, Parse, Compile** — self time: the lexer's scan time, the rest of the parse, and the compile, minus the time spent in nested functions.
- **Tokens, Nodes, Instrs** — the tokens the lexer scanned while the function was open, the expressions and statements compiled, and the instructions of its finished bytecode: the size the counters are compared with. Tokens a lookahead scans before the function opens (an arrow function's parameters, read while the parser decides it is one) count for the enclosing function, with their lex time.
- **Counters** — the compiler's (`Goccia.Compiler`) and the parser's expensive operations, counted while the function was being parsed or compiled:

| Counter | JSON key | Counts |
|---|---|---|
| Nodes compiled | `nodesCompiled` | Expressions and statements compiled |
| Constant-emit attempts | `constantEmitAttempts` | Each expression's literal check |
| Constant-evaluation visits | `constantEvaluationVisits` | Each binary operator's right operand checked for an immediate |
| Expression-type visits | `expressionTypeVisits` | Each expression's kind classification |
| Number-proof visits | `numberProofVisits` | Each number immediate checked for a small integer |
| Local lookups, probes | `localLookups`, `localLookupProbes` | Scope lookups, and the bindings (or hash buckets) each examined |
| Upvalue resolutions, list probes | `upvalueResolutions`, `upvalueListProbes` | Each function an upvalue lookup climbs, and the names it scans for an existing capture (one bucket once hashed) |
| Local-list scans | `localListScans` | The bindings each closed scope walks |
| Parameter-name probes | `parameterNameProbes` | Each parameter name's duplicate test and its record |
| Binding-name lookups, probes | `bindingNameLookups`, `bindingNameProbes` | The parser's lookups in a list of unique binding names (declaration checks and the duplicate-parameter and match-pattern checks), and the names each compared: 1 once the list is hashed (16 names or more), the names scanned below that; match-pattern maps count actual bucket probes |
| Parameter-initializer visits | `parameterInitializerVisits` | Each parameter the check that a default initializer creates no closure visits |
| Parameter-initializer source checks | `parameterInitializerSourceChecks` | Each initializer or pattern checked for a closure |
| Direct-eval record probes | `directEvalRecordProbes` | Each entry of a direct eval's scope description |
| Type-argument probe tokens | `typeArgumentProbeTokens` | Tokens the parser visits in constructor lookahead and failed-probe indexing |

The binding-name and type-argument counters are the parser's (`Goccia.Parser`); the rest are `Goccia.Compiler`'s.

Match-pattern nodes are not compiled expression/statement nodes. Compare parser binding work against pattern bindings or tokens; the compiled-node denominator can be very small for a large pattern.

AST nodes are also counted by kind: for each function and each file in the JSON, and for the whole run in the text report.

### Reading the report

Before the per-function table the report prints one row per file. For a function `g` that holds a `switch` of 2,000 case clauses, each declaring a `const`:

```text
Compile Profile Functions (self time, top 2 of 2):
  Lex          Parse        Compile         Tokens     Nodes    Instrs  Function                 Location
  2.06ms       6.30ms       2.35ms           26008     12002      8003  g                        switch-2000.js:1:11
  17.76µs     34.27µs     43.68µs            14         5         9  <module>                 switch-2000.js

Compile Profile Work (per node: count / nodes compiled; function: the one with the highest count):
  Counter                               Total   Per node     In function   Per node  Function
  Constant-emit attempts                 6004        0.5            6001        0.5  g switch-2000.js:1:11
  Local lookups                         10016        0.8           10006        0.8  g switch-2000.js:1:11
  Local-list scans                       2000        0.2            2000        0.2  g switch-2000.js:1:11
  Binding-name lookups                   4003        0.3            4001        0.3  g switch-2000.js:1:11
```

A linear compile does a bounded amount of each kind of work per node, whatever the size of the function, as here, where no counter reaches 1 per node. A counter whose **per node** figure is in the hundreds, or grows when the same shape gets bigger, is the superlinear spot, and the function next to it is where it happens. Profile two sizes of the input and compare: a figure that doubles when the input doubles is quadratic work. [#1921](https://github.com/frostney/GocciaScript/issues/1921) was this shape in the previous compiler, where every clause walked every local of the function.

Lookups are the leaves of the walks. When a walk counter (expression-type or constant-evaluation visits) is high, the lookups it makes rise with it, and the largest absolute count can be a lookup counter. The walk is the cause and the lookups are its leaves: fix the walk first. A function whose parse or compile time per token or per node is high while every counter is low is spending its time in work the counters do not cover.

The profile reads the clock at each switch between functions, so its times include that cost. The counters are exact.

## JSON Export

`--profile-output=path.json` writes the profiling data as JSON. Every key is always present, but only the enabled modes collect data: `opcodes`, `opcodePairs`, and `scalarFastPath` stay empty or zero without `--profile=opcodes` or `all`, `functions` stays empty without `--profile=functions` or `all`, and `compile` has empty lists and zero counters without `--profile=compile`. `scalarFastPath` leaves out `hitRate` when its `total` is 0. `opcodes` and `opcodePairs` are in opcode order and `functions` in the order the functions were first called; sort them yourself. A function's `selfTimeNs` and `totalTimeNs` have the console report's meaning, so `totalTimeNs` counts each outermost activation once:

```json
{
  "opcodes": [
    {"opcode": "Mov", "count": 57318, "percentage": 5.9},
    ...
  ],
  "opcodePairs": [
    {"prev": "Mov", "cur": "Jmp", "count": 57314},
    ...
  ],
  "scalarFastPath": {"hits": 57311, "misses": 0, "total": 57311, "hitRate": 100.0},
  "shapeSaturation": {"depthLimitPrefixes": 0, "tableCapacityEvents": 0},
  "functions": [
    {"name": "fib", "sourceFile": "script.js", "line": 1, "calls": 114625,
     "selfTimeNs": 23625142, "totalTimeNs": 23625142, "allocations": 0},
    ...
  ],
  "compile": {
    "files": [
      {"sourceFile": "switch-2000.js", "lexTimeNs": 2027139, "parseTimeNs": 6450276,
       "compileTimeNs": 1985313, "functions": 2, "tokens": 26022,
       "instructions": 8012, "counters": {"nodesCompiled": 12007, ...},
       "nodeKinds": {"VariableDeclaration": 2001, "SwitchStatement": 1, ...}}
    ],
    "functions": [
      {"name": "g", "sourceFile": "switch-2000.js", "line": 1, "column": 11,
       "lexTimeNs": 2006640, "parseTimeNs": 6413457, "compileTimeNs": 1940319,
       "tokens": 26008, "instructions": 8003,
       "counters": {"nodesCompiled": 12002, "localLookups": 10006, ...},
       "nodeKinds": {"VariableDeclaration": 2000, "SwitchStatement": 1, ...}},
      ...
    ],
    "nodeKinds": [{"kind": "VariableDeclaration", "count": 2001}, ...],
    "counters": {"nodesCompiled": 12007, ...}
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

Each line is a semicolon-separated call stack with a self-time weight in whole microseconds (a stack with less than 1 µs is left out). The weights are self times, never total times, so a recursive function's time is counted once however deep it recurses. For the script in [Function Profile](#function-profile---profilefunctions), the shallowest stacks are:

```text
<module> 48
<module>;<anonymous> 23
<module>;<anonymous>;fib 1
<module>;<anonymous>;fib;fib 1
```

This shows the JS function call hierarchy that external profilers like `sample` cannot see — `sample` shows all these as the same recursive `Execute` frames.

## Combining with `sample` (macOS)

The profiler and `sample` are complementary. The profiler sees inside the interpreter (opcodes, JS functions, objects made). `sample` sees outside it (the collector, the FPC runtime, exception frames), and in an unprofiled run it sees the fast loop, which the profiler turns off. macOS builds have no JIT, so all of it is interpreter time.

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
| `FastLoop` | The hot instructions and plain bytecode calls (`Goccia.VM`) |
| `Execute` | Every other instruction, and every instruction under a profile, coverage or an instruction limit (`Goccia.VM`) |
| `CollectMinor`, `CollectMajor` | Minor (copying) and major (mark and sweep) collections (`Goccia.Core`) |
| `NewObject` | Object allocation (`Goccia.Core`) |
| `fpc_pushexceptaddr`, `fpc_popaddrstack` | An exception frame in a routine with managed locals; see G2's [working rules](engine/README.md#working-rules) |
| `SysGetMem`, `SysFreeMem` | FPC's heap: Pascal strings and dynamic arrays, not engine cells |

The profiler's allocation count per function tells you *which JS function* makes the objects behind the collections `sample` reports. The opcode histogram tells you *which operations* run inside the time `sample` attributes to `Execute` and `FastLoop`.

## Architecture

| Unit | Role |
|------|------|
| `Goccia.Profile` | Feeds the profiler from G2's interpreter: the push hook times frames and charges objects made, the step hook counts opcodes, pairs and the scalar fast path, and `GocciaOpcodeName` names G2's opcodes |
| `Goccia.Profiler.pas` | Per-thread tracker: opcode counts, pair matrix, scalar hit/miss, function profiles with timing stack |
| `Goccia.Profiler.Report.pas` | Console output, JSON export, and collapsed stack export |
| `Goccia.Profiler.Compile.pas` | The compile profile: per-function records, counters, nodes by kind, and its console output and JSON section; `Goccia.Compiler`, `Goccia.Eval` and `Goccia.Parser` feed it |

The profiler follows the same singleton pattern as `Goccia.Coverage.pas`: `Initialize`/`Instance`/`Shutdown`, boolean `Enabled` flag. The instance is per-thread; a runner that runs each file on a thread of its own makes that thread record into the main thread's profiler with `Adopt`, and runs profiled files one at a time. Opcode counting and pair tracking use static arrays (no heap allocation in the hot path). Function profiling uses a timing stack and a clock adjusted for registration and frame bookkeeping, with parent-linked interned flame paths, and counts each function's frames on that stack so that only its outermost frame adds to its total time. `Goccia.Profile` keeps a shadow stack of the frames it has seen; allocation tracking counts the objects `NewObject` makes while `CountObjects` is set (`ObjectsMade` in `Goccia.Core`) and charges the difference to the innermost profiled function at each frame push and end.

When profiling is disabled, the remaining cost is small but not zero: each frame push tests `ProfilingAny` (`Goccia.VM`), each object made tests `CountObjects` (`Goccia.Core`), and the counted loop is not entered for profiling. Both flags stay set in a process once any thread has profiled.

When the compile profile is off, each counting site is a nil test. The parser reads the profiler's per-thread instance once, when a parser is created, and `Goccia.Compiler` once per compile, keeping it in a field (`Prof`) the function scopes share, so a counting site does no thread-variable lookup. A source the profile excludes (the host's preludes) gets a nil field as well.
