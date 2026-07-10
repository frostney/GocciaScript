# Web Tooling Goccia benchmark lane

**Date:** 2026-07-06
**Area:** `benchmarking`, `tooling`, `ci`
**Issue:** [#857](https://github.com/frostney/GocciaScript/issues/857)
**Related:** [#821](https://github.com/frostney/GocciaScript/issues/821), [ADR 0087](0087-awfy-cross-engine-benchmark-methodology.md)

GocciaScript will integrate the V8 Web Tooling Benchmark as a Goccia-only
external workload lane, not as a Node/QuickJS comparison lane. The suite answers
whether GocciaScript can build and execute real JavaScript tooling bundles, so
reference engines are unnecessary signal and would make CI spend time measuring
the wrong thing. Node remains in the workflow only as the upstream package/build
runtime for Webpack.

The upstream corpus is pinned by repository SHA in
`perf/web-tooling/manifest.json`. CI runs every pinned upstream workload, not a
curated subset: the current report set is `acorn`, `babel`, `babel-minify`,
`babylon`, `buble`, `chai`, `coffeescript`, `espree`, `esprima`, `jshint`,
`lebab`, `postcss`, `prepack`, `prettier`, `source-map`, `terser`,
`typescript`, and `uglify-js`.

The driver prepares upstream's generated Terser/UglifyJS self-bundles, then
generates a static entry and payload-only `fs.readFileSync` adapter for each
workload. The entry imports exactly one upstream `*-benchmark` module and times
one direct call to its exported `fn()`; process repetitions are the raw samples.
This keeps the upstream workload and `third_party` inputs intact while excluding
Benchmark.js, Lodash, `virtualfs`, and unrelated workloads from the measured
bundle. It also avoids making those measurement dependencies compatibility
requirements for GocciaScript.

Each workload runs with `GocciaScriptLoader` in bytecode mode. The runner passes
the broad ECMAScript compatibility flags needed by legacy tooling bundles,
including `var`, `function`, ASI, non-strict Script semantics, legacy loops,
loose equality, labels, `arguments`, and the explicit
`--unsafe-function-constructor` gate. Those flags are part of the measurement
contract: Web Tooling is a compatibility and viability probe, not a
recommendation for default-style GocciaScript code.

The report is normalized JSON with one target entry per workload. A build
failure, timeout, crash, OOM, or missing `runs/s` line is recorded as a
first-class target outcome instead of failing the whole report. The job fails
only when the driver cannot produce a structurally complete report for every
manifest workload. CI runs one workload per matrix job, then validates and
merges those shards into one report. PR CI uploads the `web-tooling-report`
artifact and posts a Goccia-only comment. Main CI uploads the same artifact and, when
`BLOB_READ_WRITE_TOKEN` is configured, publishes the compressed report plus a
UTC daily pointer to Vercel Blob under `web-tooling/`.

Consequences:

- Issue #857 is complete when every pinned Web Tooling workload has a Goccia
  report entry with build/execution outcome, raw sample data, and reproducible
  metadata.
- Web Tooling results are retained like test262 and performance artifacts:
  workflow artifacts for every CI run, Vercel Blob run/daily records on `main`.
- AWFY remains the cross-engine external benchmark lane. Web Tooling remains
  Goccia-only unless a future issue asks a different question.
