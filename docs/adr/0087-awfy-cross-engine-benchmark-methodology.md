# AWFY cross-engine benchmark methodology

**Date:** 2026-07-05
**Area:** `benchmarking`, `tooling`
**Issue:** [#856](https://github.com/frostney/GocciaScript/issues/856)
**Related:** [#821](https://github.com/frostney/GocciaScript/issues/821), [#862](https://github.com/frostney/GocciaScript/issues/862), [ADR 0076](0076-same-runner-benchmark-comparison.md), [ADR 0081](0081-reject-value-caches-for-allocation-reduction.md)

GocciaScript will use a repo-local AWFY adapter and cross-engine driver instead
of pointing GocciaScript or QuickJS at upstream AWFY's Node-shaped JavaScript
harness directly. Upstream AWFY remains the source of benchmark semantics and is
recorded by repository SHA in `perf/awfy/manifest.json`, with weekly/manual
pin-refresh PRs handled by `.github/workflows/awfy-bump.yml`. Each selected
benchmark is bundled into a per-benchmark shell-portable entrypoint with a tiny
`require` adapter and a `performance.now()` timer with a `Date.now()` fallback.
This keeps the benchmark source comparable across Node, QuickJS, and GocciaScript
while avoiding Node-only globals such as `require`, `process.argv`,
`process.hrtime`, and `process.stdout`.

The adapter deliberately builds one benchmark entrypoint at a time. A monolithic
bundle lets one parser or syntax issue in an unrelated benchmark block every
other result, which is not useful when #856's job is to quantify the runnable
surface and report first-class failures for the rest. Per-benchmark bundles make
each timeout, crash, OOM, parse failure, or checksum failure an explicit result
instead of a reason the whole corpus disappears.

Cross-engine timing claims from this lane must follow the benchmark protocol:
production Goccia bytecode binaries, raw samples retained in JSON, medians plus
IQR/min/max/CV, checksum/verification agreement across engines, and environment
metadata for Goccia commit, FPC version, platform, architecture, reference-engine
versions, AWFY corpus SHA, and driver version. When comparing two Goccia
binaries, the driver interleaves baseline and candidate samples per benchmark
and repetition. Sequential baseline-then-candidate batches are not accepted for
runtime claims, matching ADR 0081's drift guardrail. Profiler-backed Goccia runs
remain separate from timing runs; selected AWFY outliers and diagnostic probes
may be rerun with `--profile=opcodes`, `--profile=functions`, or `--profile=all`
for explanation.

Pull-request CI runs the AWFY report set from `perf/awfy/manifest.json`: all
pinned AWFY benchmarks under GocciaScript, QuickJS, and the latest Node Current
release resolved by `actions/setup-node` at workflow time, with five raw samples
per engine. Samples are collected in target -> repetition -> engine order, so
reference engines and optional baseline/candidate Goccia binaries are
interleaved within each target rather than measured in sequential engine-sized
batches. CI surfaces the median timings as an `AWFY Results` PR comment and
keeps min/max/CV plus raw samples in the JSON artifact. Full CI runs the same
report on the ubuntu-latest x64 build and publishes the compressed report JSON
plus a daily pointer to the `awfy/` Vercel Blob namespace on `main` when
`BLOB_READ_WRITE_TOKEN` is configured.

Rows with sub-0.5ms medians are timer-floor sensitive. They can prove that a
benchmark verifies across engines, but they should be rerun with higher
inner-iteration counts before being used for broad runtime claims.

AWFY and web-tooling are roadmap-level proof, not the only gate for optimization
work. The repo also keeps a small Goccia-owned diagnostic probe corpus under
`perf/probes/` for targeted engine surfaces: dispatch preamble and loop floor,
scalar `+` and `OP_TO_PRIMITIVE` pressure, strict-types typed emission probes
where syntax is Goccia-only, call-path probes, string/RegExp cliffs, and the
later typed-array boxing / inline-cache levers (#860/#861). Those probes use
the same normalized driver/report schema but are not upstream AWFY benchmarks.
They should be run as focused optimization gates or profiler-backed diagnostic
passes, not folded into the public AWFY PR summary.

Consequences:

- Issue #856 is complete only when AWFY JavaScript benchmarks run under
  GocciaScript bytecode, QuickJS, and the latest Node Current release from the
  same driver with raw samples, verification results, first-class failure
  outcomes, and reproducible metadata.
- Issue #862 should use diagnostic probes as implementation gates and AWFY /
  web-tooling as transfer proof for the worst measured hotspots.
- Existing `GocciaBenchmarkRunner` remains the in-repo benchmark runner and CI PR
  comparison surface. The AWFY driver is an external-shell comparison lane for
  reference engines that do not provide Goccia's `"goccia:microbench"` API,
  and its raw probes stay outside `benchmarks/` so CI does not mistake them for
  runner-managed benchmark files.
- Strict-types probes are Goccia-only because type annotations are not portable
  JavaScript syntax for Node or QuickJS; they are paired with untyped probes for
  cross-engine context.
