# Benchmarks

GocciaScript includes a benchmark runner for measuring execution performance. Benchmarks live in the `benchmarks/` directory and use a `suite`/`bench` API.

## Running Benchmarks

```bash
# Build the BenchmarkRunner
./build.pas benchmarkrunner

# Run all benchmarks
./build/BenchmarkRunner benchmarks

# Run a specific benchmark
./build/BenchmarkRunner benchmarks/fibonacci.js

# Export results in different formats
./build/BenchmarkRunner benchmarks --format=json --output=results.json
./build/BenchmarkRunner benchmarks --format=csv --output=results.csv
./build/BenchmarkRunner benchmarks --format=text
```

## Output Formats

The BenchmarkRunner supports four output formats via the `--format` flag:

| Format | Description |
|--------|-------------|
| `console` (default) | Pretty-printed columnar output with suite headers, variance, and summary |
| `text` | Compact one-line-per-benchmark format, suitable for piping or logging |
| `csv` | Standard CSV with header row (`file,suite,name,ops_per_sec,variance_percentage,mean_ms,iterations,error`) |
| `json` | Structured JSON with `files[]` array containing nested `benchmarks[]` |

Use `--output=<file>` to write results to a file instead of stdout.

## Configuring Benchmark Parameters

Benchmark calibration and measurement parameters can be configured via environment variables:

| Environment Variable | Default | Description |
|---------------------|---------|-------------|
| `GOCCIA_BENCH_WARMUP` | 3 | Number of warmup iterations before calibration |
| `GOCCIA_BENCH_CALIBRATION_MS` | 300 | Target calibration time in milliseconds |
| `GOCCIA_BENCH_CALIBRATION_BATCH` | 10 | Initial batch size for calibration |
| `GOCCIA_BENCH_ROUNDS` | 3 | Number of measurement rounds (1–5; median is reported) |

Example:

```bash
# Fast run with shorter calibration and fewer rounds
GOCCIA_BENCH_CALIBRATION_MS=100 GOCCIA_BENCH_ROUNDS=1 ./build/BenchmarkRunner benchmarks

# Thorough run with longer calibration and more rounds
GOCCIA_BENCH_CALIBRATION_MS=1000 GOCCIA_BENCH_ROUNDS=5 ./build/BenchmarkRunner benchmarks
```

## Writing Benchmarks

```javascript
suite("fibonacci", () => {
  bench("recursive fib(20)", () => {
    const fib = (n) => n <= 1 ? n : fib(n - 1) + fib(n - 2);
    fib(20);
  });

  bench("iterative fib(20) via reduce", () => {
    const fib = (n) => Array.from({ length: n }).reduce(
      (acc) => [acc[1], acc[0] + acc[1]],
      [0, 1]
    )[0];
    fib(20);
  });
});
```

## How the BenchmarkRunner Works

The `BenchmarkRunner` program:

1. Parses CLI arguments (`--format`, `--output`, and the benchmark path).
2. Scans the provided path for `.js` files.
3. For each file, creates a `TGocciaEngine` with `DefaultGlobals + [ggBenchmark]`.
4. Loads the source and appends a `runBenchmarks()` call.
5. Executes the script — the engine measures lex, parse, and execute phases separately with nanosecond precision via `TimingUtils.GetNanoseconds`.
6. `suite()` calls execute immediately, registering `bench()` entries.
7. `runBenchmarks()` runs each registered benchmark:
   - **Warmup:** Configurable iterations to stabilize (default 3).
   - **Calibrate:** Scales batch size until it runs for at least the target calibration time (default 300ms). Uses nanosecond-resolution timing via `TimingUtils` (`clock_gettime(CLOCK_MONOTONIC)` on Unix/macOS, `QueryPerformanceCounter` on Windows).
   - **Measure:** Runs multiple measurement rounds (default 3), computes the coefficient of variation (CV%) from the unsorted ops/sec data, then reports the median values.
8. Collects all results into a `TBenchmarkReporter`, which renders the chosen output format.

## Script API

| Function | Description |
|----------|-------------|
| `suite(name, fn)` | Group benchmarks (like `describe` in tests). Executes `fn` immediately. |
| `bench(name, fn)` | Register a benchmark. `fn` is called many times during measurement. |
| `runBenchmarks()` | Execute all registered benchmarks and return results. Injected automatically by BenchmarkRunner. |

## Available Benchmarks

| File | Covers |
|------|--------|
| `benchmarks/fibonacci.js` | Recursive vs iterative computation |
| `benchmarks/arrays.js` | Array.from, map, filter, reduce, forEach, find, sort, flat, flatMap |
| `benchmarks/objects.js` | Object creation, property access, Object.keys/entries, spread |
| `benchmarks/strings.js` | Concatenation, template literals, split/join, indexOf, trim, replace, pad |
| `benchmarks/classes.js` | Instantiation, method dispatch, inheritance, private fields, getters/setters, decorators (class, method, field, getter/setter, static, private, auto-accessor, metadata) |
| `benchmarks/closures.js` | Closure capture, higher-order functions, call/apply/bind, recursion |
| `benchmarks/collections.js` | Set add/has/delete/forEach, Map set/get/has/delete/forEach/keys/values |
| `benchmarks/json.js` | JSON.parse, JSON.stringify, roundtrip with nested and mixed data |
| `benchmarks/destructuring.js` | Array/object/parameter/callback destructuring, rest, defaults, nesting |
| `benchmarks/promises.js` | Promise.resolve/reject, then chains, catch/finally, all/race/allSettled/any |

## Sample Output

Console format (default):

```text
  Lex: 287μs | Parse: 0.58ms | Execute: 7207.31ms | Total: 7208.18ms

  fibonacci
    recursive fib(15)                        201 ops/sec  ± 0.42%      4.9843 ms/op  (60 iterations)
    recursive fib(20)                         18 ops/sec  ± 2.40%     55.6868 ms/op  (10 iterations)
    iterative fib(20) via reduce         111,301 ops/sec  ± 0.43%      0.0090 ms/op  (40960 iterations)

Benchmark Summary
  Total benchmarks: 3
  Total duration: 7.21s
```

Durations are auto-formatted by `FormatDuration` from `TimingUtils`: values below 0.5μs display as `ns`, values below 0.5ms as `μs`, values up to 10s as `ms` with two decimal places, and larger values as `s`.

The `±X.XX%` column shows the coefficient of variation across measurement rounds. It is omitted when variance is zero (e.g., with a single measurement round).

## CI Integration

Benchmarks run as part of the CI pipeline. See [testing.md](testing.md#ci-integration) for the full pipeline overview.

### PR Benchmark Comparison

The PR workflow (`.github/workflows/pr.yml`) restores the cached benchmark baseline from main, runs all benchmarks with JSON output, and posts a collapsible comparison comment on the PR:

- Results are **grouped by file**, each in a collapsible `<details>` section
- Files with significant changes (improvements or regressions) are auto-expanded
- Each file summary shows the count of improved/regressed/unchanged benchmarks and an **average percentage change**
- The **overall PR summary** at the top shows totals across all files with an average percentage
- Changes within **±7%** are considered insignificant (shown without color indicators) — this threshold accounts for CI environment noise during active development
- 🟢 marks improvements > 7%, 🔴 marks regressions > 7%, 🆕 marks new benchmarks with no baseline
