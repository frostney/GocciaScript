# Cross-Engine Performance Tooling

## Executive Summary

- `perf/awfy/manifest.json` pins the upstream AWFY JavaScript corpus version and
  lists the Goccia-owned diagnostic probes.
- `perf/probes/` contains plain portable probe scripts, not
  `GocciaBenchmarkRunner` `suite()` / `bench()` files.
- `scripts/awfy-driver.js` owns timing, interleaving, reference-engine commands,
  checksum comparison, first-class failure outcomes, and normalized JSON output.
- `scripts/awfy-bump-pin.ts` and `.github/workflows/awfy-bump.yml` keep the
  AWFY corpus SHA refreshable through the same automated bump-PR pattern used
  by the other pinned upstream suites.
- Timing runs and Goccia profiler-backed diagnostic runs are separate by design.

Use this directory for cross-engine performance work that needs Node, QuickJS,
and GocciaScript to run the same script entrypoints. Use `benchmarks/` for the
in-repo benchmark runner corpus.

See [Benchmarks](../docs/benchmarks.md#awfy-cross-engine-lane) and
[ADR 0087](../docs/adr/0087-awfy-cross-engine-benchmark-methodology.md) for the
methodology and acceptance criteria.
