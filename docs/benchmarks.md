# Benchmarks

*For contributors measuring performance or adding new benchmarks.*

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
| `console` (default) | Pretty-printed columnar output with suite headers, variance, setup/teardown times, and summary |
| `text` | Compact one-line-per-benchmark format with optional `setup=Xms teardown=Xms` suffixes |
| `csv` | Standard CSV with header row (`file,suite,name,ops_per_sec,variance_percentage,mean_ms,iterations,setup_ms,teardown_ms,error`) |
| `json` | Structured JSON with `files[]` array containing nested `benchmarks[]`, each with `setupMs` and `teardownMs` fields |

Use `--output=<file>` to write results to a file instead of stdout.

## Configuring Benchmark Parameters

Benchmark calibration and measurement parameters can be configured via environment variables:

| Environment Variable | Default | Description |
|---------------------|---------|-------------|
| `GOCCIA_BENCH_WARMUP` | 3 | Number of warmup iterations before calibration |
| `GOCCIA_BENCH_CALIBRATION_MS` | 100 | Target calibration time in milliseconds |
| `GOCCIA_BENCH_CALIBRATION_BATCH` | 5 | Initial batch size for calibration |
| `GOCCIA_BENCH_ROUNDS` | 5 | Number of measurement rounds (1–5; median is reported) |

Example:

```bash
# Fast run with shorter calibration and fewer rounds
GOCCIA_BENCH_CALIBRATION_MS=50 GOCCIA_BENCH_ROUNDS=1 ./build/BenchmarkRunner benchmarks

# Thorough run with longer calibration and more rounds
GOCCIA_BENCH_CALIBRATION_MS=300 GOCCIA_BENCH_ROUNDS=5 ./build/BenchmarkRunner benchmarks
```

## Writing Benchmarks

The `bench()` function takes a name and an options object with `setup`, `run`, and `teardown` fields:

```javascript
suite("collections", () => {
  bench("Set iteration", {
    setup: () => new Set(Array.from({ length: 50 }, (_, i) => i)),
    run: (s) => {
      let sum = 0;
      s.forEach((v) => { sum = sum + v; });
    },
    teardown: (s) => { s.clear(); },
  });

  bench("simple computation", {
    run: () => {
      const result = 1 + 2 + 3;
    },
  });
});
```

### API

- **`setup`** (optional): Called once before warmup. Its return value is passed as the first argument to `run` and `teardown`.
- **`run`** (required): The timed benchmark function. Called many times during warmup, calibration, and measurement.
- **`teardown`** (optional): Called once after all measurement rounds complete. Receives the setup return value.
- All three phases are independently timed and reported as `setupMs`, `teardownMs`, and the main `opsPerSec`/`meanMs` metrics.

### Data flow

```text
setup() → [return value] → warmup(run × N) → calibrate(run × N) → measure(run × N × rounds) → teardown()
   ↓                                                                    ↓                          ↓
 setupMs                                                        opsPerSec, meanMs, variance   teardownMs
```

### Guidelines

- Use `setup` to create data structures that the `run` function operates on. This isolates allocation cost from the operation being measured.
- Use `teardown` for cleanup when the setup creates resources that should be explicitly released.
- When the benchmark IS the creation (e.g., measuring `Array.from` speed), put everything in `run` with no `setup`.
- The `run` function can be `async` for benchmarking async/await operations.

## How the BenchmarkRunner Works

The `BenchmarkRunner` program:

1. Parses CLI arguments (`--format`, `--output`, and the benchmark path).
2. Scans the provided path for `.js` files.
3. For each file, creates a `TGocciaEngine` with `DefaultGlobals + [ggBenchmark]`.
4. Loads the source and appends a `runBenchmarks()` call.
5. Executes the script — the engine measures lex, parse, and execute phases separately with nanosecond precision via `TimingUtils.GetNanoseconds`.
6. `suite()` calls execute immediately, registering `bench()` entries.
7. `runBenchmarks()` runs each registered benchmark:
   - **Setup:** Calls the `setup` function once (timed), caches the return value.
   - **Warmup:** Configurable iterations to stabilize (default 3). The setup return value is passed to each call.
   - **Calibrate:** Scales batch size until it runs for at least the target calibration time (default 100ms). Uses nanosecond-resolution timing via `TimingUtils` (`clock_gettime(CLOCK_MONOTONIC)` on Unix/macOS, `QueryPerformanceCounter` on Windows).
   - **Measure:** Runs multiple measurement rounds (default 5), computes the coefficient of variation (CV%) from the unsorted ops/sec data, then reports the median values.
   - **Teardown:** Calls the `teardown` function once (timed) after measurement completes.
8. Collects all results into a `TBenchmarkReporter`, which renders the chosen output format.

## Script API

| Function | Description |
|----------|-------------|
| `suite(name, fn)` | Group benchmarks (like `describe` in tests). Executes `fn` immediately. |
| `bench(name, { setup?, run, teardown? })` | Register a benchmark with optional setup/teardown. `run` is called many times during measurement; `setup` and `teardown` run once each and are independently timed. |
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
| `benchmarks/numbers.js` | Integer/float arithmetic, coercion, prototype methods, static methods |
| `benchmarks/iterators.js` | Iterator.from, user-defined iterables, lazy iterator helpers, built-in iterator chaining |
| `benchmarks/for-of.js` | for...of with arrays, strings, Sets, Maps, destructuring, for-await-of |
| `benchmarks/async-await.js` | Single/multiple awaits, await non-Promise, try/catch, Promise.all, nested async |

## Sample Output

Console format (default):

```text
  Lex: 287μs | Parse: 0.58ms | Execute: 7207.31ms | Total: 7208.18ms

  fibonacci
    recursive fib(15)                        201 ops/sec  ± 0.42%      4.9843 ms/op  (60 iterations)
    recursive fib(20)                         18 ops/sec  ± 2.40%     55.6868 ms/op  (10 iterations)
    iterative fib(20) via reduce         111,301 ops/sec  ± 0.43%      0.0090 ms/op  (40960 iterations)

  collections
    Set iteration                         50,366 ops/sec  ± 1.23%      0.0199 ms/op  (5000 iterations)
                                    setup: 0.0120ms  teardown: 0.0010ms

Benchmark Summary
  Total benchmarks: 4
  Total duration: 7.21s
```

Durations are auto-formatted by `FormatDuration` from `TimingUtils`: values below 0.5μs display as `ns`, values below 0.5ms as `μs`, values up to 10s as `ms` with two decimal places, and larger values as `s`.

The `±X.XX%` column shows the coefficient of variation across measurement rounds. It is omitted when variance is zero (e.g., with a single measurement round).

When a benchmark has a `setup` or `teardown` function, a second line displays their durations (e.g., `setup: 0.0120ms  teardown: 0.0010ms`).

## CI Integration

Benchmarks run as part of the CI pipeline in both **interpreted** and **bytecode** modes. Interpreted and bytecode benchmarks run in **parallel** via a matrix strategy. CI uses reduced calibration settings (`GOCCIA_BENCH_CALIBRATION_MS=50`, `GOCCIA_BENCH_ROUNDS=3`) for faster runs. On pushes to `main`, the ubuntu-latest x64 runner saves JSON baselines for each mode (`benchmark-interpreted-results.json` and `benchmark-bytecode-results.json`) to the GitHub Actions cache. See [testing.md](testing.md#ci-integration) for the full pipeline overview.

### PR Benchmark Comparison

The PR workflow (`.github/workflows/pr.yml`) runs interpreted and bytecode benchmarks in **parallel** on separate runners, restores the cached baselines from main, and posts a collapsible comparison comment on the PR with separate **Interpreted** and **Bytecode** sections:

- Results are **grouped by file**, each in a collapsible `<details>` section
- Files with significant changes (improvements or regressions) are auto-expanded
- Each file summary shows the count of improved/regressed/unchanged benchmarks and an **average percentage change**
- The **overall PR summary** at the top shows totals across all files with an average percentage
- Changes within **±7%** are considered insignificant (shown without color indicators) — this threshold accounts for CI environment noise during active development
- 🟢 marks improvements > 7%, 🔴 marks regressions > 7%, 🆕 marks new benchmarks with no baseline
