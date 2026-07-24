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

Each CI shard starts one runner invocation. On the POSIX systems used by the
Test262 CI lane, that invocation is a supervisor which uses `fork`, never
`exec`, to run one test in each short-lived worker process. The program does not
start another copy of its executable: the child continues from the already
initialized runner image, creates one runtime, heap, engine, and realm, writes
its Pascal result to the parent, and exits. `--jobs` bounds the number of child
processes alive concurrently. A native signal or watchdog expiry is therefore
contained to one test, and the same supervisor continues dispatching the
remaining corpus. Test262 agent threads remain children of the individual test
worker.

Sharding is content-stable: FNV-1a of the normalized Test262 ID modulo the
runtime shard count. The GitHub Actions matrix determines only how many jobs
exist. `strategy.job-index` and `strategy.job-total` supply the binary
arguments and artifact names. The external TypeScript utility remains
responsible for cross-process concerns only: validating and merging shard
reports, aggregating profile files, and rendering the pull-request comment.

The native-runner prototype established that removing process startup alone is
not the wall-clock multiplier: on its fixed representative corpus it improved
measured execution time by only 5–20%. Four balanced CI shards supplied the
measured 3.64x runner-wall improvement, so sharding remains the performance
mechanism and the native binary establishes the correct ownership boundary.

A full 13,051-test shard then showed why long-lived worker threads were not a
safe final boundary. Although every engine and thread-local runtime was
destroyed between tests, Free Pascal's allocator retained released storage in
the runner process and reached 14.5 GB RSS. Recreating the thread pool did not
return that storage to the operating system, while forced heap reclamation was
unsafe for process-shared engine caches. Fork-per-test workers produced exactly
the same 13,022 pass, 23 fail, 2 wrapper-infrastructure, and 4 timeout
classifications, completed locally in 183.9 seconds wall time, and bounded peak
combined RSS to 3.23 GB. Process exit is therefore both the native-crash
containment boundary and the allocator reclamation boundary.

Alternatives considered:

- **Keep the TypeScript runner and only make shard arguments dynamic.**
  Rejected because discovery, host behavior, engine configuration, and verdict
  classification would remain split across languages, while LoaderBare would
  retain a Test262-only API.
- **Let the binary spawn/exec copies of itself or LoaderBare.** Rejected because
  that recreates executable startup and splits the worker protocol across CLI
  entry points. POSIX `fork` continues the one runner invocation without
  starting another binary.
- **Keep one collector heap for the lifetime of each worker.** Rejected because
  the prototype retained roughly the full allocation volume until worker
  shutdown and exceeded a safe full-corpus memory envelope.
- **Reuse worker processes for multiple tests.** Rejected because the Pascal
  allocator retained released storage across engines, making process exit the
  only proven bounded-memory reclamation point.
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
- On POSIX, a native signal or watchdog expiry is classified for the affected
  test and the supervisor launches the next worker. The shard remains complete.
- Fork-per-test workers have a measurable cost and are retained as the
  correctness, crash-isolation, and allocator-reclamation policy unless a
  future allocator or GC design proves an equivalent bounded-lifetime
  guarantee.
