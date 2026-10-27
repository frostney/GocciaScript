# 0164 - A compile profile reports lex, parse and compile time and the compiler's work per source function

**Date:** 2026-10-27
**Area:** `profiler`, `compiler`, `parser`
**Related:** [ADR 0023](0023-bytecode-vm-profiling.md), [Profiling](../profiling.md#compile-profile---profilecompile), [#1942](https://github.com/frostney/GocciaScript/issues/1942)

## Context

The profiler of [ADR 0023](0023-bytecode-vm-profiling.md) starts counting at
the first instruction. Before that, the runner reports one lex, parse and
compile total per file. Four parse and compile costs that grew
quadratically with the source were found in a few days
([#1921](https://github.com/frostney/GocciaScript/issues/1921),
[#1933](https://github.com/frostney/GocciaScript/issues/1933),
[#1934](https://github.com/frostney/GocciaScript/issues/1934),
[#1937](https://github.com/frostney/GocciaScript/issues/1937)), and each
needed callgrind or perf to locate. For #1937 a flat profile pointed at
`AddUpvalue` and `ResolveUpvalue`, the leaves of the walks that were the
cause.

What would have named each cause is a count of the compiler's work per
function, beside the function's size: the count that grows faster than the
source is the superlinear spot.

## Decision

**`--profile=compile` records, per source function and per file, lex, parse
and compile self time, the function's size (tokens, nodes compiled,
instructions) and counters of the operations a compile repeats.** The
counters were chosen by reading the compiler for work done per node whose
amount depends on something other than the node: the constant-folding and
type-inference walks over operand subtrees, local and upvalue lookups and
the entries each examines, walks over a scope's whole list of locals,
parameter-name and binding-name list lookups and the names each examines,
and nodes compiled by kind.

### One record per function, by switch

A function is identified by its file and the line and column of its node,
which the parser and the compiler see alike, so its parse and its compile
meet in one record. Work and time are charged by switch: what happens
between two switches belongs to the function open at the time. The parser
switches where it enters and leaves a function (the sites that already
track `FFunctionDepth`, in their `finally` blocks); the compiler switches in
`DoSwapState` and where it creates the top-level template. A template with
no position of its own (class field initializers) takes the position of the
node being compiled when it is first entered, so a class's field
initializers and static blocks share the class's record and their
instructions add up. The key wraps the column to 16 bits, as the compiler's
debug information stores it, and the record keeps the full column the parser
saw. Names are read from the finished templates, since a function named by
its binding gets that name after its body compiled.

Code compiled at run time is a unit of its own per source: the parse and
compile of an `eval` are recorded under `<eval>#` and those of the
`Function` constructor under `<Function>#`, each followed by a hash of the
source, not under the caller's file, where its positions, which count from
the start of the eval source, would merge with the file's functions. A
direct eval's rebuilt caller scopes take the profile too, so lookups that
climb into them count.

### Nothing on the path when it is off

Every counting site is `if Assigned(<field>) then ...`. The parser keeps the
profile in a field set when it is created; the compiler sets its field when
a compile starts, and every compiler scope takes its parent's. A thread
variable is read once per parser and once per compile, never per site
([Thread Variables on Hot Paths](../core-patterns.md#thread-variables-on-hot-paths)).
The binding-name helpers of `Goccia.AST.BindingPatterns` take the profile as
an optional last parameter, passed where a caller keeps names unique. An A/B
of production builds, three copies of each binary in alternating order,
found no difference beyond the spread between identical copies.

### `all` keeps its meaning

`all` stays opcodes and functions. `--profile-deterministic` captures `all`
for the benchmark comparisons in CI, and compile times are wall clock: in
`all` they would add run-to-run noise to artifacts meant to be compared.
The JSON export always carries a `compile` section, empty unless
`--profile=compile` is on, beside the existing keys.

### Rejected

- **A build-time switch** (`{$IFDEF}`) around the counters: zero cost by
  construction, but the mode would exist only in special builds, and the
  cost it avoids is a nil test that measures as nothing.
- **A thread-variable read at each site**: on POSIX targets each read calls
  `pthread_getspecific`, on paths that run once per lookup.
- **Flame graphs of the compile**: the records already nest by function; the
  collapsed-stack file stays empty for this mode, as for `opcodes`.

## Consequences

- On the generated sources of the four issues, before their fixes, the
  counter that named each cause stood far above its level in ordinary code
  (at most 3 per node for all but the local lookups, over 1,301 functions of
  the test suite), from 61 to millions per node, and doubled or quadrupled
  when the input doubled: local-list scans for #1921,
  binding-name and parameter-name probes for #1933 and #1934, and
  expression-type and constant-evaluation visits for #1937, with the upvalue
  resolutions in step with the constant-evaluation visits that make them.
- A new expensive operation is visible only once it is counted: work the
  counters do not cover shows as a high time per node with low counters.
  The whole-function pre-passes (`Goccia.Compiler.NumericBindings`,
  `OperandSafety`, `NumericProof`) are covered only by their share of a
  function's compile time.
- A change that replaces a counted operation keeps its counter: a hashed
  lookup counts one probe, so the counter still reads as work per lookup.
