# Native Test262 runner with isolated worker runtimes

**Date:** 2026-07-24
**Area:** `testing`, `engine`
**Issue:** [#768](https://github.com/frostney/GocciaScript/issues/768)
**Related:** [ADR 0042](0042-test262-loaderbare-harness.md)

GocciaScript promotes Test262 execution from a TypeScript orchestrator plus one
`GocciaScriptLoaderBare` subprocess per test into the dedicated native
`GocciaTest262Runner` binary. The binary owns corpus discovery, frontmatter
parsing, harness composition, Test262 host installation, execution,
classification, worker dispatch, progress, and per-shard JSON. LoaderBare
returns to its general-purpose core-engine surface and no longer accepts
`--test262-host`.

Each CI shard starts one runner process. Inside it, a bounded native worker pool
pulls tests dynamically. A worker creates a fresh thread-local runtime, heap,
engine, and realm for a test, records only the Pascal result data needed by the
aggregate, then tears the complete runtime down before accepting another test.
The worker operating-system thread remains long-lived; JavaScript state and
garbage-collector state do not. Test262 agent threads remain children of the
individual test host.

Sharding is content-stable: FNV-1a of the normalized Test262 ID modulo the
runtime shard count. The GitHub Actions matrix determines only how many jobs
exist. `strategy.job-index` and `strategy.job-total` supply the binary
arguments and artifact names. The external TypeScript utility remains
responsible for cross-process concerns only: validating and merging shard
reports, aggregating profile files, and rendering the pull-request comment.

The native-runner prototype established that removing process startup alone is
not the wall-clock multiplier: on its fixed representative corpus it improved
measured execution time by only 5–20%, and a retained worker heap was unsafe at
full-corpus scale. In the final isolated-runtime design, a 5,000-test sample had
identical classifications and reported 32.1 seconds of native execution versus
40.0 seconds through per-test subprocesses, but both commands took about 40.2
seconds end to end because final runtime teardown falls outside the native
report timer. Four balanced CI shards supplied the measured 3.64x runner-wall
improvement. This decision therefore keeps sharding as the performance
mechanism and uses the native binary to establish the correct ownership
boundary. Per-test runtime recreation addresses the prototype's retained-heap
finding without relying on forced collection, which had exposed a rooting
defect.

Alternatives considered:

- **Keep the TypeScript runner and only make shard arguments dynamic.**
  Rejected because discovery, host behavior, engine configuration, and verdict
  classification would remain split across languages, while LoaderBare would
  retain a Test262-only API.
- **Let the binary spawn copies of itself or LoaderBare.** Rejected because the
  desired unit is one runner process per CI shard with worker-owned engines,
  not a process supervisor disguised as an engine runner.
- **Keep one collector heap for the lifetime of each worker.** Rejected because
  the prototype retained roughly the full allocation volume until worker
  shutdown and exceeded a safe full-corpus memory envelope.
- **Claim native workers replace CI sharding.** Rejected by the measured
  prototype. Native execution improves ownership and removes process overhead;
  independent shards provide the required wall-clock capacity.

Consequences:

- Test262 host hooks live under `Goccia.Test262.Host` and are not a LoaderBare
  option.
- The runner report remains compatible with existing baseline, dashboard,
  profile, and PR-comment consumers.
- Changing CI shard cardinality requires changing the matrix only; merge
  validation infers the expected count from report metadata.
- A native signal can terminate one shard process. The merge job rejects a
  missing or incomplete shard set instead of publishing partial conformance
  data.
- Worker runtime recreation has a measurable cost and is retained as the
  correctness and memory-isolation policy unless a future GC design proves an
  equivalent bounded-lifetime guarantee.
