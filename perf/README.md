# External Performance Tooling

## Executive Summary

- `perf/awfy/manifest.json` pins the upstream AWFY JavaScript corpus version and
  lists the Goccia-owned diagnostic probes.
- `perf/web-tooling/manifest.json` pins the upstream V8 Web Tooling Benchmark
  corpus and lists every upstream workload run by the Goccia-only lane.
- `perf/probes/` contains plain portable probe scripts, not
  `GocciaBenchmarkRunner` `suite()` / `bench()` files.
- `scripts/awfy-driver.js` owns timing, interleaving, reference-engine commands,
  checksum comparison, first-class failure outcomes, and normalized JSON output.
- `scripts/web-tooling-driver.js` owns workload-specific Web Tooling bundles,
  direct Goccia-only workload timing, first-class failure outcomes, and
  normalized JSON output.
- `scripts/awfy-bump-pin.ts` and `.github/workflows/awfy-bump.yml` keep the
  AWFY corpus SHA refreshable through the same automated bump-PR pattern used
  by the other pinned upstream suites.
- Timing runs and Goccia profiler-backed diagnostic runs are separate by design.

Use this directory for shell-driven performance work outside the
`GocciaBenchmarkRunner` corpus: cross-engine AWFY, Goccia-owned diagnostic
probes, and the Goccia-only Web Tooling lane. Use `benchmarks/` for the in-repo
benchmark runner corpus.

See [Benchmarks](../docs/benchmarks.md#awfy-cross-engine-lane) and
[ADR 0087](../docs/adr/0087-awfy-cross-engine-benchmark-methodology.md) for the
AWFY methodology. See
[Web Tooling Goccia Lane](../docs/benchmarks.md#web-tooling-goccia-lane) and
[ADR 0089](../docs/adr/0089-web-tooling-goccia-benchmark-lane.md) for the
Goccia-only Web Tooling lane.
