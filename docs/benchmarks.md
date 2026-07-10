# Benchmarks

*For contributors measuring performance or adding new benchmarks.*

## Executive Summary

- **Module-only benchmark API** — Import `bench`, `group`, `run`, `summary`, and `boxplot` from `"goccia:microbench"`; no benchmark helpers are ambient globals
- **Five output formats** — `console` (default), `text`, `csv`, `json`, `compact-json`; configurable via `--format` and `--output`
- **Profiler-backed runs** — Bytecode benchmark runs can emit opcode/function profiles with `--profile`, including deterministic single-run capture
- **CI integration** — PR workflow posts benchmark comparison comments with range-overlap classification; main CI retains deterministic profile reports
- **Environment tuning** — Calibration time, warmup iterations, and measurement rounds configurable via environment variables
- **Cross-engine AWFY lane** — `scripts/awfy-driver.js` runs pinned AWFY and `perf/probes/` diagnostics under GocciaScript, QuickJS, and Node without mixing them into the benchmark-runner corpus

GocciaScript includes a benchmark runner for measuring execution performance. Benchmarks live in the `benchmarks/` directory and import the benchmark API from `"goccia:microbench"`.

## Running Benchmarks

```bash
# Build the GocciaBenchmarkRunner
./build.pas benchmarkrunner

# Run all benchmarks
./build/GocciaBenchmarkRunner benchmarks

# Run a specific benchmark
./build/GocciaBenchmarkRunner benchmarks/fibonacci.js

# Run a benchmark from stdin
printf 'import { bench, group } from "goccia:microbench"; group("stdin", () => { bench("sum", () => 1 + 1); });\n' | ./build/GocciaBenchmarkRunner --source-type=module
printf 'import { bench, group } from "goccia:microbench"; group("stdin", () => { bench("sum", () => 1 + 1); });\n' | ./build/GocciaBenchmarkRunner - --source-type=module --mode=bytecode

# Run benchmarks with 2 parallel workers (default: CPU count)
./build/GocciaBenchmarkRunner benchmarks --jobs=2

# Run bytecode benchmarks with VM profiler data
./build/GocciaBenchmarkRunner benchmarks --profile=all --profile-output=bench-profile.json --jobs=1

# Capture a deterministic opcode/allocation profile for CI-style diffing
./build/GocciaBenchmarkRunner benchmarks/numbers.js --profile-deterministic --profile-output=profile.json

# Export results in different formats
./build/GocciaBenchmarkRunner benchmarks --format=json --output=results.json
./build/GocciaBenchmarkRunner benchmarks --format=compact-json --output=compact-results.json
./build/GocciaBenchmarkRunner benchmarks --format=csv --output=results.csv
./build/GocciaBenchmarkRunner benchmarks --format=text
```

When no path is provided, `GocciaBenchmarkRunner` reads benchmark source from stdin. Use `-` explicitly when you want stdin alongside other CLI options and still make the input source obvious.

## Output Formats

The GocciaBenchmarkRunner supports five output formats via the `--format` option:

| Format | Description |
|--------|-------------|
| `console` (default) | Pretty-printed columnar output with group headers, variance, setup/teardown times, and summary |
| `text` | Compact one-line-per-benchmark format with optional `setup=Xms teardown=Xms` suffixes |
| `csv` | Standard CSV with header row (`file,suite,name,ops_per_sec,variance_percentage,mean_ms,iterations,setup_ms,teardown_ms,error`) |
| `json` | Structured JSON with the common CLI envelope (`build`, aggregate `output`, `timing`, `memory`, `workers`) and a `files[]` array containing each `fileName`, per-file timing, and nested `benchmarks[]`, including `opsPerSec`, `variancePercentage`, `minOpsPerSec`, `maxOpsPerSec`, `setupMs`, and `teardownMs` |
| `compact-json` | Same structured JSON shape, omitting `build`, `memory`, `stdout`, and `stderr` for smaller machine-readable output |

Use `--output=<file>` to write results to a file instead of stdout.

## Profiling Benchmark Runs

`GocciaBenchmarkRunner` accepts the same VM profiling options as `GocciaScriptLoader`:
`--profile=opcodes|functions|all`, `--profile-output=<path>`, and
`--profile-format=flamegraph`. Profiling forces bytecode mode and serial execution
because profiler state is per thread.

Use profiler-backed benchmark runs when a benchmark ratio is ambiguous and you need
to see the opcode mix, scalar fast-path hit rate, JS function self-time, or
allocation attribution:

```bash
./build/GocciaBenchmarkRunner benchmarks/numbers.js \
  --profile=all \
  --profile-output=tmp/numbers-profile.json \
  --format=json \
  --output=tmp/numbers-bench.json \
  --jobs=1
```

For deterministic CI signals, add `--profile-deterministic`. This skips warmup,
calibration, and repeated measurement rounds, then runs each registered benchmark
once through the same setup/measurement/teardown lifecycle used by normal
benchmark runs. If `--profile` is not provided, it defaults to `--profile=all`.
The benchmark report remains structurally valid, but throughput fields are
placeholders; use the profile JSON for deterministic comparisons.

```bash
./build/GocciaBenchmarkRunner benchmarks/numbers.js \
  --profile-deterministic \
  --profile-output=tmp/numbers-deterministic-profile.json \
  --format=compact-json \
  --output=tmp/numbers-deterministic-report.json
```

## Configuring Benchmark Parameters

Benchmark calibration and measurement parameters can be configured via environment variables:

| Environment Variable | Default | Description |
|---------------------|---------|-------------|
| `GOCCIA_BENCH_WARMUP` | 5 | Number of warmup iterations before calibration |
| `GOCCIA_BENCH_CALIBRATION_MS` | 200 | Target calibration time in milliseconds |
| `GOCCIA_BENCH_CALIBRATION_BATCH` | 5 | Initial batch size for calibration |
| `GOCCIA_BENCH_ROUNDS` | 7 | Number of measurement rounds (1–50; median of IQR-filtered data is reported) |

Example:

```bash
# Fast local run with shorter calibration and fewer rounds
GOCCIA_BENCH_CALIBRATION_MS=50 GOCCIA_BENCH_ROUNDS=3 ./build/GocciaBenchmarkRunner benchmarks

# Thorough serial run with longer calibration and more rounds
GOCCIA_BENCH_CALIBRATION_MS=500 GOCCIA_BENCH_ROUNDS=15 ./build/GocciaBenchmarkRunner benchmarks
```

## Writing Benchmarks

Import benchmark helpers from `"goccia:microbench"`:

```javascript
import { bench, group, summary, boxplot } from "goccia:microbench";

group("collections", () => {
  const setIteration = {
    *run() {
      const set = new Set(Array.from({ length: 50 }, (_, i) => i));
      try {
        yield () => {
          let sum = 0;
          set.forEach((v) => { sum = sum + v; });
        };
      } finally {
        set.clear();
      }
    },
  }.run;

  bench("Set iteration", setIteration);

  bench("simple computation", () => {
    const result = 1 + 2 + 3;
  });
});

summary(() => {
  boxplot(() => {
    group("numbers", () => {
      bench("addition", () => 1 + 1);
    });
  });
});
```

### API

The primary benchmark form is `bench(name, fn)`, where `fn` is called many times during measurement. Generator callbacks provide setup and teardown without including that setup cost in the measured body:

```javascript
import { bench, group } from "goccia:microbench";

group("collections", () => {
  const setIteration = {
    *run() {
      const set = new Set(Array.from({ length: 50 }, (_, i) => i));
      try {
        yield () => {
          let sum = 0;
          set.forEach((v) => { sum = sum + v; });
        };
      } finally {
        set.clear();
      }
    },
  }.run;

  bench("Set iteration", setIteration);

  bench("simple computation", () => {
    const result = 1 + 2 + 3;
  });
});
```

- Code before the generator's first `yield` is setup.
- The yielded function is the timed benchmark body.
- Cleanup that must run during generator close belongs in a `finally` block around the `yield`.
- Setup and teardown are independently timed and reported as `setupMs`, `teardownMs`, and the main `opsPerSec`/`meanMs` metrics.
- `summary(fn)` and `boxplot(fn)` are accepted output-shaping wrappers. Rich percentile and chart rendering belongs to issue #855, so today they should be treated as registration wrappers rather than a promise of additional report fields.
- `run(opts?)` executes the currently registered benchmarks from script code. `GocciaBenchmarkRunner` also auto-runs registered benchmarks after loading a file, but a script-callable `run()` consumes the pending registry so the runner does not measure the same benchmarks twice.

### Data flow

```text
setup before yield → yielded run function → warmup/calibrate/measure → close generator
        ↓                         ↓                                      ↓
     setupMs             opsPerSec, meanMs, variance                 teardownMs
```

### Guidelines

- Use generator setup to create data structures that the yielded benchmark function operates on. This isolates allocation cost from the operation being measured.
- Put cleanup in a generator `finally` block when setup creates resources that should be explicitly released.
- When the benchmark IS the creation (e.g., measuring `Array.from` speed), use a plain benchmark function with no setup.
- The benchmark function can be `async` for benchmarking async/await operations.

## How the GocciaBenchmarkRunner Works

The `GocciaBenchmarkRunner` program:

1. Parses CLI inputs (`--format`, `--output`, and the benchmark path or stdin marker).
2. Scans the provided path for `.js` files.
3. For each file, creates a `TGocciaEngine`, attaches `TGocciaRuntimeCore`, applies the loader runtime profile, and installs the benchmark runtime extension.
4. Loads and executes the source so benchmark files import `"goccia:microbench"` and register groups and benchmarks.
5. Measures lex, parse, compile (bytecode mode), script execution, and benchmark execution phases separately with nanosecond precision via `TimingUtils.GetNanoseconds`.
6. `group()` calls execute immediately, registering nested `bench()` entries.
7. After the script finishes, the runner auto-runs any benchmarks that were registered but not already consumed by a script-callable `run()`:
   - **Setup:** Advances generator callbacks to their first `yield` (timed) when a benchmark uses generator-style setup.
   - **Warmup:** Configurable iterations to stabilize (default 5). The yielded function or plain benchmark function is called for each iteration.
   - **Calibrate:** Scales batch size until it runs for at least the target calibration time (default 200ms). Uses nanosecond-resolution timing via `TimingUtils` (`clock_gettime(CLOCK_MONOTONIC)` on Unix/macOS, `QueryPerformanceCounter` on Windows).
   - **Measure:** Runs multiple measurement rounds (default 7). GC is disabled during measurement for identical behavior in both interpreter and bytecode modes. Between rounds, `CollectYoung` reclaims measurement garbage efficiently (pre-marks old objects, only traverses new allocations). After all rounds, IQR-based outlier filtering removes noise spikes before computing the coefficient of variation (CV%) and median values.
   - **Teardown:** Closes generator callbacks after measurement (timed), running `finally` cleanup when present.
8. After each file completes, `GC.Collect` runs to reclaim memory between script executions.
9. Collects all results into a `TBenchmarkReporter`, which renders the chosen output format.
10. After rendering, checks for failures via `TBenchmarkReporter.HasFailures`. If any benchmark entry has a non-empty `Error` field or zero `OpsPerSec`/`MeanMs`, the process exits with code 1.

### Exit Codes

| Exit Code | Meaning |
|-----------|---------|
| `0` | All benchmarks completed successfully with non-zero measurements |
| `1` | One or more benchmarks failed — either an error occurred (access violation, exception) or a benchmark produced zero ops/sec or zero mean ms |

The non-zero exit code ensures CI pipelines fail when benchmarks crash or produce empty results.

## Script API

| Function | Description |
|----------|-------------|
| `bench(name, fn)` | Register a benchmark function. `fn` is called many times during measurement; generator callbacks use pre-yield setup, a yielded measured function, and `finally` cleanup on close. |
| `group(name, fn)` | Group benchmarks (like `describe` in tests). Executes `fn` immediately. |
| `run(opts?)` | Execute registered benchmarks from script code and return results. Calling `run()` prevents the runner's post-load auto-run from measuring the same registry again. |
| `summary(fn)` | Accepted registration wrapper for summary-shaped output. Richer summary reporting is tracked in #855. |
| `boxplot(fn)` | Accepted registration wrapper for boxplot-shaped output. Richer chart output is tracked in #855. |

## Available Benchmarks

| File | Covers |
|------|--------|
| `benchmarks/fibonacci.js` | Recursive vs iterative computation |
| `benchmarks/atomics.js` | Atomics load/store, read-modify-write operations, compareExchange, wait/notify, waitAsync synchronous path |
| `benchmarks/arrays.js` | Array.from, map, filter, reduce, forEach, find, sort, flat, flatMap |
| `benchmarks/objects.js` | Object creation, property access, Object.keys/entries, spread |
| `benchmarks/strings.js` | Concatenation, template literals, split/join, indexOf, trim, replace, pad |
| `benchmarks/regexp.js` | RegExp construction, prototype methods, and string integration hooks |
| `benchmarks/intl.js` | Intl.NumberFormat, Intl.DateTimeFormat, and Intl.Collator construction-free hot formatting/comparison paths |
| `benchmarks/temporal.js` | Temporal PlainDate arithmetic, Duration balancing, ZonedDateTime construction, and DST-aware difference paths |
| `benchmarks/classes.js` | Instantiation, method dispatch, inheritance, private fields, getters/setters, decorators (class, method, field, getter/setter, static, private, auto-accessor, metadata) |
| `benchmarks/closures.js` | Closure capture, higher-order functions, call/apply/bind, recursion |
| `benchmarks/collections.js` | Set add/has/delete/forEach, Map set/get/has/delete/forEach/keys/values |
| `benchmarks/weak-collections.js` | WeakMap/WeakSet construction, mutation, lookup, non-registered symbol keys/values, upsert methods, and GC smoke cases |
| `benchmarks/json.js` | JSON.parse, JSON.stringify, roundtrip with nested and mixed data |
| `benchmarks/csv.js` | CSV parse/stringify named module imports, delimiter options, revivers, and roundtrips |
| `benchmarks/tsv.js` | TSV parse/stringify named module imports, escaped fields, and roundtrips |
| `benchmarks/destructuring.js` | Array/object/parameter/callback destructuring, rest, defaults, nesting |
| `benchmarks/promises.js` | Promise.resolve/reject, then chains, catch/finally, all/race/allSettled/any |
| `benchmarks/numbers.js` | Integer/float arithmetic, coercion, prototype methods, static methods |
| `benchmarks/iterators.js` | Iterator.from, user-defined iterables, lazy iterator helpers, built-in iterator chaining |
| `benchmarks/for-of.js` | for...of with arrays, strings, Sets, Maps, destructuring, for-await-of |
| `benchmarks/async-await.js` | Single/multiple awaits, await non-Promise, try/catch, Promise.all, nested async |
| `benchmarks/generators.js` | Manual next, for...of, yield delegation, object/class generator methods |
| `benchmarks/async-generators.js` | for-await-of over async generators and await inside async generator bodies |

## Sample Output

Console format (default):

```text
  Lex: 287μs | Parse: 0.58ms | Execute: 7207.31ms | Total: 7208.18ms

  fibonacci
    recursive fib(15)                        282 ops/sec  ± 0.87%      3.5467 ms/op  (30 iterations)
                                    range: 279 .. 286 ops/sec
    recursive fib(20)                         25 ops/sec  ± 0.88%     39.9814 ms/op  (10 iterations)
                                    range: 25 .. 25 ops/sec
    iterative fib(20) via reduce          12,762 ops/sec  ± 1.43%      0.0804 ms/op  (2500 iterations)
                                    range: 12,430 .. 12,879 ops/sec

  collections
    Set iteration                         50,366 ops/sec  ± 1.23%      0.0199 ms/op  (5000 iterations)
                                    range: 49,800 .. 50,950 ops/sec
                                    setup: 0.0120ms  teardown: 0.0010ms

Benchmark Summary
  Total benchmarks: 4
  Total duration: 7.21s
```

Durations are auto-formatted by `FormatDuration` from `TimingUtils`: values below 0.5μs display as `ns`, values below 0.5ms as `μs`, values up to 10s as `ms` with two decimal places, and larger values as `s`.

The `±X.XX%` column shows the coefficient of variation across measurement rounds. It is omitted when variance is zero (e.g., with a single measurement round).

When a benchmark has a `setup` or `teardown` function, a second line displays their durations (e.g., `setup: 0.0120ms  teardown: 0.0010ms`).

## CI Integration

Benchmarks run as part of the CI pipeline in both **interpreter mode** and
**bytecode mode**. CI uses `GOCCIA_BENCH_CALIBRATION_MS=100` and
`GOCCIA_BENCH_ROUNDS=7` for stable measurements with IQR outlier filtering. On
pushes to `main`, the ubuntu-latest x64 runner emits JSON and validates the
report shape. PR comparison builds and benchmarks `main` on the PR's own runner
rather than reading a cached baseline ([ADR 0076](adr/0076-same-runner-benchmark-comparison.md)).
See [testing.md](testing.md#ci-integration) for the full pipeline overview.

Main bytecode benchmark runs also retain deterministic VM profile reports. The
GitHub Actions artifact is named `benchmark-profile` and contains:

- `benchmark-profile-aggregate.json`: aggregate opcode, opcode-pair, scalar
  fast-path, function, allocation, and benchmark-file hotspot data.
- `benchmark-profile-aggregate.md`: a human-readable summary with the same
  provenance and ranked tables.
- `profile-baseline/`: the detailed per-benchmark-file profile JSON directory
  used by PR profile diffs and deeper investigation.

When `BLOB_READ_WRITE_TOKEN` is configured, main CI publishes the same payloads
to Vercel Blob under the separate `benchmark-profiles/` namespace. The default
paths are `benchmark-profiles/runs/<artifactId>/aggregate.json.gz`,
`benchmark-profiles/runs/<artifactId>/summary.md`,
`benchmark-profiles/runs/<artifactId>/details.tar.gz`, and
`benchmark-profiles/daily/<YYYY-MM-DD>.json`.

### PR Benchmark Comparison

The PR workflow (`.github/workflows/pr.yml`) builds the PR's base commit (`main`) in a `build-main` job and benchmarks that `main` build and the PR build **back-to-back on the same runner** (after a discarded warm-up), so deltas reflect the diff rather than cross-runner variance (issue #815, [ADR 0076](adr/0076-same-runner-benchmark-comparison.md)). The comparison logic lives in [`scripts/benchmark-compare.js`](../scripts/benchmark-compare.js) (unit-tested by `scripts/test-benchmark-compare.ts`). It posts a collapsible comparison comment on the PR; each benchmark file gets a **unified table** with both execution modes side by side:

- Each table row shows `| Benchmark | Interpreted (main → PR) | Δ | Bytecode (main → PR) | Δ |` with the point estimate and same-runner `main`/PR min-max range in the form `10,000 ops/sec [9,500..10,500] → 9,200 ops/sec [8,700..9,700]`
- Classification uses **range overlap** instead of a single point value: if the PR run sits fully above the `main` range it is improved, if it sits fully below it is regressed, and overlapping ranges are treated as unchanged noise
- Percentage deltas remain in the `Δ` column as secondary context, even when the classifier marks a benchmark as `~ overlap`
- The overall summary reports the measured **median per-run variance** as a noise floor, so sub-noise deltas are read as unchanged
- Results are **grouped by file**, each in a collapsible `<details>` section
- Files with significant changes (improvements or regressions) are auto-expanded
- Each file summary shows per-mode counts (e.g., `Interp: 🟢 1, 7 unch. · Bytecode: 🟢 2, 6 unch.`)
- The **overall PR summary** shows per-mode totals on separate lines with average percentage deltas
- The comparison is **advisory** (comment-only, never merge-blocking)
- 🟢 marks non-overlapping improvements, 🔴 marks non-overlapping regressions, `~ overlap` marks overlapping ranges, and 🆕 marks new benchmarks absent from the `main` build

## AWFY Cross-Engine Lane

The `benchmarks/` directory is reserved for `GocciaBenchmarkRunner` inputs that
import `"goccia:microbench"` and register `group()` / `bench()` cases. Cross-engine AWFY and diagnostic
probes live under `perf/` because they are plain shell-portable scripts driven by
Node tooling, not benchmark-runner files. This keeps CI's recursive benchmark
scan from treating diagnostic probes as missing benchmark-runner inputs.

Use `scripts/awfy-driver.js` for #856/#862 investigation:

```bash
# List pinned AWFY benchmark names and Goccia-owned probes
node scripts/awfy-driver.js --list

# Run one AWFY benchmark from a local upstream checkout
node scripts/awfy-driver.js \
  --awfy-dir /path/to/are-we-fast-yet/benchmarks/JavaScript \
  --benchmark NBody \
  --inner-iterations 1 \
  --repetitions 5 \
  --engines goccia,qjs,node \
  --output tmp/awfy-report.json

# Run one diagnostic probe through the same normalized report schema
node scripts/awfy-driver.js \
  --probe generic-plus-scalars \
  --inner-iterations 1000 \
  --repetitions 1 \
  --engines goccia,qjs,node \
  --output tmp/probe-report.json
```

`perf/awfy/manifest.json` records the upstream AWFY repository, pinned commit,
JavaScript corpus path, driver version, and diagnostic probe catalog. The driver
generates per-benchmark portable AWFY bundles from the selected benchmark's
CommonJS dependency graph instead of using upstream `harness.js` directly. That
avoids Node-only globals (`require`, `process.argv`, `process.hrtime`,
`process.stdout`) and prevents one unrelated benchmark parse failure from
blocking every other benchmark.

### Updating the AWFY Pin

The AWFY corpus SHA is pinned in `perf/awfy/manifest.json`. A weekly workflow
(`.github/workflows/awfy-bump.yml`) fetches the latest
`smarr/are-we-fast-yet` `master` SHA, runs `bun scripts/awfy-bump-pin.ts`, and
opens an automated PR when the manifest changes. The workflow no-ops when the
pin is already current, matching the test262 and TOML suite bump pattern.

Manual bump:

```bash
bun scripts/awfy-bump-pin.ts <40-hex-sha>
```

The normalized report records:

- raw samples for every engine/target/repetition
- medians, IQR-filtered medians, min/max, coefficient of variation, and
  geomean pairwise ratios
- checksum/verification results and cross-engine checksum agreement
- timeout, crash, OOM, missing-result, and verification-failed outcomes
- Goccia commit, FPC version, platform, architecture, reference-engine versions,
  AWFY corpus SHA, and driver version

Pull requests run an AWFY report lane from `.github/workflows/pr.yml`.
The target set is recorded in `perf/awfy/manifest.json` under `ciReport`: all
pinned AWFY benchmarks under the production-built PR Goccia bytecode loader, the
production-built PR `main` baseline loader, QuickJS, and the latest Node Current
release resolved by `actions/setup-node` at workflow time, with five raw samples
per engine.
Sampling is interleaved as target -> repetition -> engine, so every repetition
runs the selected engines next to each other instead of collecting engine-sized
batches. The workflow uploads the normalized JSON report and posts an
`AWFY Results` PR comment with main/PR Goccia medians, the per-target PR-vs-main
delta, and reference-engine medians; min/max/CV and raw samples stay in the
`awfy-report` artifact. Diagnostic probes remain available through the same
driver for focused engine work, but they are not mixed into the PR AWFY summary.

AWFY rows with sub-0.5ms medians are useful for verifying that the benchmark
runs consistently, but they are timer-floor sensitive. Before using one of those
rows as evidence for a broad runtime claim, rerun that target with a higher
`--inner-iterations` value and inspect the artifact's min/max/CV.

Full CI runs the same AWFY report on the ubuntu-latest x64 build. It uploads the
`awfy-report` artifact on every full CI run. On `main`, when
`BLOB_READ_WRITE_TOKEN` is configured, the workflow also publishes the compressed
report JSON to Vercel Blob under the separate `awfy/` namespace. The default
paths are `awfy/runs/<artifactId>/report.json.gz` and
`awfy/daily/<YYYY-MM-DD>.json`.

When comparing two Goccia binaries, pass `--goccia-baseline` and
`--goccia-candidate`; the driver interleaves baseline and candidate samples per
target and repetition. Runtime claims for #862 should use this interleaved
shape, not sequential baseline-then-candidate batches.

Profiler-backed runs stay separate from timing. For selected Goccia outliers or
diagnostic probes, add `--profile=opcodes|functions|all --profile-dir=<dir>` and
run a dedicated diagnostic pass; do not mix profiled timings into ratio claims.

### Issue Acceptance Criteria

Issue #856 is complete when AWFY JavaScript benchmarks run under GocciaScript
bytecode, QuickJS, and the latest Node Current release from the same driver;
reports retain raw samples and derived statistics; verification/checksum results
agree or fail explicitly; timeout, crash, and OOM are first-class outcomes;
metadata is sufficient to reproduce the run; and at least one Goccia AWFY outlier
can be rerun with opcode/function profiles attached.

Issue #862 should use `perf/probes/` as implementation gates for dispatch,
primitive lowering, call path, string/RegExp cliffs, typed-array call/return
boxing, and inline-cache shape behavior. AWFY and web-tooling remain
roadmap-level transfer proof. Allocation reductions are supporting evidence
only; they do not prove a runtime win without interleaved timing movement and
profile evidence for the mechanism.
