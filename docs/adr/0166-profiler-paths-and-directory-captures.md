# 0166 - Profile stack paths incrementally and aggregate directory captures on one thread

**Date:** 2026-10-28
**Area:** `profiler`, `cli`
**Related:** [ADR 0023](0023-bytecode-vm-profiling.md), [ADR 0164](0164-compile-phase-profile.md), [#1953](https://github.com/frostney/GocciaScript/issues/1953), [#1954](https://github.com/frostney/GocciaScript/issues/1954)

## Context

Returning from a profiled function rebuilt its full collapsed stack string.
That work grew with depth and happened after the return timestamp, charging it
to the caller's self time. Outermost-frame totals were consistent but included
that cost. Directory workers had no thread-local profiler, so the reporting
thread exported empty data.

## Decision

Intern a flame path by its parent node and function profile index on entry.
The frame retains that node; return updates its self weight without an
ancestor walk, allocation, or string lookup. A linked list records the order
nodes first receive positive weight. Expand paths only on export, merging
identical displayed paths and preserving the existing names, order and weights.
The JSON schema is unchanged. A cumulative excluded-time clock removes the
measured registration and frame-bookkeeping intervals from all active frames;
explicit-timestamp entry points retain their accounting contract.

Run directory entries serially when profiling is requested, accumulating one
report on the reporting thread. This follows BenchmarkRunner and avoids a
second set of merge rules for runtime templates, compile positions and dynamic
source identities. Reset the previous opcode at each independent entry so
there is no fabricated cross-file pair. Unprofiled execution stays parallel.

Runner's `--profile-deterministic` selects deterministic guest time/randomness
and zeroes report timing fields, retaining counters and the schema. It implies
`all` when no mode was selected, supports compile counters, and emits an empty
zero-weight flame graph. BenchmarkRunner keeps its existing capture contract.

## Consequences

Function returns have depth-independent profiler cost; path creation is
amortized constant work per entry plus the name length. Flame export work and
storage remain proportional to expanded output size, which can be quadratic
in maximum depth when every prefix is emitted. JSON captures need no expanded
paths. Instrumentation and clock reads can still perturb execution.

A profiled directory takes the sum of its entry runtimes rather than their
parallel wall time. The report covers each file instead of silently losing the
worker data. Deterministic counts require the same external inputs; zero-time
captures are unsuitable for identifying time hotspots.
