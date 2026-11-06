# 0178 - AWFY is timed warm, engines alternate their order, and interpreter-class comparators run beside QuickJS

**Date:** 2026-11-05
**Area:** `benchmarking`, `ci`, `website`
**Issue:** [#2066](https://github.com/frostney/GocciaScript/issues/2066)
**Supersedes:** in part [ADR 0087](0087-awfy-cross-engine-benchmark-methodology.md) and [ADR 0091](0091-unified-performance-barometer.md)
**Related:** [ADR 0076](0076-same-runner-benchmark-comparison.md), refines [ADR 0176](0176-the-interpreter-beats-quickjs-before-gcbundle-a-jit-and-a-compiled-executable.md), [#2102](https://github.com/frostney/GocciaScript/issues/2102)

## Context

ADR 0176 judges the first performance target on the AWFY and JetStream QuickJS
ratios of the Performance Barometer, and leaves two questions of the
measurement protocol to #2066: how AWFY is timed, and in what order the engines
run.

Under driver version 1 every AWFY sample timed one cold call of the benchmark's
`innerBenchmarkLoop` in a fresh process (two for `CD`). Several rows were below
ADR 0087's 0.5 ms floor: in CI run 41824992451 Mandelbrot took 0.02 ms in
Goccia and 0.01 ms in QuickJS. A cold sample mostly measures startup for the
short benchmarks, and JIT-enabled Node.js before its JIT has warmed up, so its
Node.js column was slower than Goccia on three rows. Both lanes also ran
Goccia before QuickJS in every repetition, so any effect of running first or
second fell on one engine. The only comparators were QuickJS and JIT-enabled
Node.js, which left the lane with no second interpreter-class point.

## Decision

**AWFY is timed warm (driver version 2).** Each process creates one benchmark
instance and runs `warmupIterations` iterations, each timed on its own, then
`measuredIterations` iterations timed as one window. An iteration is one
`innerBenchmarkLoop(innerIterations)` call, verified as in upstream AWFY's
harness. A sample's headline `durationMicros` is the window divided by
`measuredIterations`: the steady-state time of one iteration. Because the
window has two clock reads however many iterations it holds, a short iteration
adds no timer cost, and the 0.5 ms floor applies to the window, not to one
iteration. The first warm-up iteration is kept as `firstIterationMicros`, the
cold figure, and each engine's stats carry its median as
`medianFirstIterationMicros`; no ratio uses it. The headline over processes
is the median of the samples, six in CI.

The counts live per benchmark under `warmProtocol` in `perf/awfy/manifest.json`.
They keep each benchmark's problem size (`innerIterations` 1, `CD` 2), five
warm-up iterations, and enough measured iterations for QuickJS's window to
last about 50 ms or more on a 2022 laptop core (100 times the floor). `Havlak`,
whose single iteration takes over 20 s in Goccia, runs one warm-up and one
measured iteration. The problem sizes are unchanged because `Mandelbrot`,
`NBody` and `Havlak` verify only at the sizes upstream lists, and the next
listed sizes (`Mandelbrot` 500, `NBody` 250000) cost Goccia 1.2 s and 8.3 s per
iteration. A change of any count changes the barometer's protocol key and
breaks the trend line.

**Engines alternate their order (both lanes, driver version 2).** The first
round runs the `--engines` order, and every next round runs the reverse of the
one before (unrecorded `--warmup` rounds count as rounds; CI runs none), so every pair of engines swaps places from one repetition to the
next. The order is deterministic, so a report can be rerun in the same order,
and over an even number of repetitions each engine of a pair runs first
equally often. CI runs six repetitions in both lanes, so the `--engines` order
and its reverse run three times each; with an odd number of engines the middle
one keeps its place, but still swaps order with every other engine. Random order was rejected: it balances only in expectation and
needs a recorded seed to be reproducible. Every sample records `engine`,
`repetition` and `position` (its place in that repetition's order), each
target records `executionOrder`, and the report's metadata names the order
mode and the base order. Target-1 evidence cites the order from those fields.

**Interpreter-class comparators run beside QuickJS.**

- `node-jitless` is Node.js with `--jitless`, V8's Ignition interpreter with no
  JIT tier, from the same Node.js binary as `node`. It runs by default in both
  drivers and in CI, and the barometer publishes its ratio.
- `jsc` is the JavaScriptCore shell with `--useJIT=false` (the same option as
  `JSC_useJIT=false`), which leaves the LLInt interpreter. CI runs it in both
  lanes: every AWFY and JetStream job on `ubuntu-latest` installs the shell
  from the Ubuntu archive's `libjavascriptcoregtk-bin` package
  (`.github/scripts/install-jsc.sh`). The archive drops a package version once
  an update supersedes it, so the version is recorded rather than pinned: the
  driver writes the package and version into the report's engine metadata, the
  CI reports fail when a report that measured JSC records none, and the
  barometer breaks the trend line when it changes. The drivers' defaults leave
  `jsc` out, because a local machine may have no shell; each manifest's
  `ciReport.engines` adds it for CI, and a local run opts in with
  `--engines ...,jsc --jsc <path>`.
- The JIT flags are arguments, not environment variables, so every sample's
  recorded command shows them.

Goccia gets a separate ratio against each comparator: `geomeanRatios` has one
pair per engine, and the PR comments and the barometer show a column per
comparator. QuickJS stays the only comparator target 1 counts. The
interpreter-class engines are context, and JIT-enabled Node.js stays on the
barometer as an outside reference.

## Consequences

- Driver version 2 and the protocol key break the AWFY and JetStream trend
  lines, as ADR 0091 requires for a measurement change. A version 1 result does
  not count toward target 1 under version 2.
- The warm figures differ from the cold ones by benchmark. On one machine
  (Ryzen 9 6900HX, QuickJS 2026-06-04), the QuickJS geomean ratio was 4.78
  cold and 5.27 warm; `NBody` went from 5.83 to 9.18 and `Json` from 6.92 to
  8.85, because the cold rows had mostly measured startup.
- The AWFY job takes longer. On the same machine the CI report took 292 s
  with four engines, against 139 s for the cold protocol with three, most of
  it `Havlak`'s two Goccia iterations per process. The PR lane runs two Goccia
  binaries, so its share of `Havlak` doubles.
- CI runs six repetitions instead of five, and JSC joins `node-jitless` as a
  new engine, so the AWFY lane takes six samples per target and engine where
  it took five, from five engines (six in the PR lane), and the JetStream lane
  goes from three engines (four in the PR lane) to five (six). Both new
  engines are interpreters, but each costs far less than Goccia, whose
  samples dominate both lanes. Every AWFY and JetStream job also installs
  JSC.
- Per-platform measurement and JavaScriptCore on macOS runners stay with
  #2102.
- What stays: ADR 0076's same-runner rule, ADR 0087's target, then repetition,
  then engine sampling and its raw samples, verification and metadata, and
  ADR 0091's lanes, frozen JetStream subset, ratio convention and segmentation.
  The JetStream lane's in-process protocol (three iterations with
  First/Worst/Average sub-scores) is unchanged.
