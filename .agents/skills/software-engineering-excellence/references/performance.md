# Performance and feedback time

Read this when making a performance claim or changing a frequent,
latency-sensitive or resource-intensive path, tooling, hooks, CI, startup,
concurrency or external operations, or when diagnosing a slow, flaky or
redundant feedback loop.

## Measure before claiming

Measure representative before/after behavior. Include workload, environment,
warm/cold state, samples, units and comparison statistic. For a PR, compare the
target branch baseline with the PR candidate under matching conditions and
identify both revisions. Do not claim gains where measurements overlap or
extrapolate a microbenchmark beyond its workload.

An unstable benchmark blocks accepting a speedup while permitting authorized
diagnosis and repair.

## Keep feedback fast without losing coverage

Reduce duplicate work with correct incremental checks, caching and shared
results. Preserve coverage and hooks. Diagnose material regressions, including
flakiness, retries, queue delay and redundant validation.
