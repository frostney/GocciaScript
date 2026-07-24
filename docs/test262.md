# test262 Harness Contract

How GocciaScript runs the official TC39 test262 conformance suite, the
contract the runner guarantees, and the boundary between
conformance failures, wrapper-infrastructure failures, and runner-level
errors.

## What test262 is, and why we run it

[test262](https://github.com/tc39/test262) is the TC39-maintained
conformance suite for ECMAScript. ~50K tests cover every observable
behavior in the language and built-ins, plus Intl, staging proposals,
and the harness itself. We run it as an indicator metric and regression
signal to track which spec corners GocciaScript implements, where the
engine diverges, and how each PR moves those numbers. The generated
reports are the source of truth for ECMAScript compatibility status.

For the original LoaderBare-plus-stock-harness decision, see
[ADR 0042](adr/0042-test262-loaderbare-harness.md). The native-runner
replacement is recorded in
[ADR 0103](adr/0103-native-test262-runner.md).

## Executive summary

- Test262 runs via the dedicated native `GocciaTest262Runner`, never via
  `GocciaTestRunner` or `GocciaScriptLoaderBare`. Wrapper bodies execute inside
  a neutral engine without `expect`, `describe`, `test`, lifecycle hooks,
  mocks, or `runTests`.
- Stock tc39/test262 harness files are loaded from the pinned test262
  checkout's `harness/` directory. The only bundled harness file is
  `scripts/test262_harness/$262.js`, the host-provided hook object.
- `Goccia.Test262.Host` installs the host hooks only in engines owned by the
  Test262 runner.
- Test feature metadata may add explicit engine options when test262 splits
  proposal layers more narrowly than the base engine flag set.
- The native runner catches engine outcomes directly. Stock async markers and
  the negative-runtime wrapper markers remain the JavaScript-to-host verdict
  protocol.
- Wrapper-infrastructure failures are classified separately from
  conformance failures and gated to zero in CI.
- CI assigns every normalized test ID to a deterministic shard, runs the
  configured shard matrix concurrently, then validates and merges it into the
  same canonical report and profile artifacts used by unsharded consumers.
- CI uploads `test262-results.json` on every PR and main run. Main runs also
  publish the report to Vercel Blob when `BLOB_READ_WRITE_TOKEN` is configured,
  and the website compatibility dashboard reads those durable reports at
  request time with CDN caching.
- Main CI also publishes full-corpus test262 profile reports for performance
  review. These profiles are retained separately from compatibility JSON and are
  reviewed from the aggregate first, with detailed profiles reserved for
  investigation.

## Website dashboard

The public compatibility dashboard lives at `/compatibility`. It reads the
latest available main-branch test262 report for each UTC day, renders pass-rate
and runtime timelines, shows the top-level test262 category split, and ranks the
five least-covered path groups from the latest report. The "JSON result" link
on the dashboard points back to the exact report used for the latest view.

Dashboard data is built from Vercel Blob at request time and served with CDN
caching. Main CI publishes future dashboard points directly to Blob. To seed
historical data, run the one-off backfill command: it first copies any
still-retained main-branch GitHub artifact reports to Blob, then reruns
historical main commits for days whose artifacts have expired.

```bash
cd website
BLOB_READ_WRITE_TOKEN=<vercel-blob-token> \
GITHUB_ARTIFACT_TOKEN="$(gh auth token)" \
bun run backfill-test262
```

The backfill defaults to `--since=2026-05-01` and today's UTC date; pass
`--since=YYYY-MM-DD` or `--until=YYYY-MM-DD` only when intentionally narrowing
the range. It stores immutable run reports under `test262/runs/` and writes one
daily pointer under `test262/daily/YYYY-MM-DD.json` for the latest published main
run on each UTC day. Website builds do not download GitHub Actions artifact
ZIPs, read Blob, or bake test262 data into the deployment output. The
`/compatibility` page and `/api/test262/*` routes read the daily pointers and
per-run reports from Blob on request, using short CDN/server caching because
new results arrive only a few times per day.

Configure the Vercel project so both Preview and Production deployments have
access to the Blob store at runtime. CI and one-off backfills still need the
GitHub repository secret `BLOB_READ_WRITE_TOKEN` because they publish new
reports from GitHub Actions rather than from inside the Vercel project.

## Profile report contract

Main CI publishes a full-corpus bytecode profile for the existing `test262` job
without changing PR CI. The profile is a performance-review artifact, not the
compatibility dashboard input and not a conformance gate.

The GitHub Actions artifact is named `test262-profile`. It contains:

- `test262-profile-aggregate.json`: the review entry point, with run
  provenance, corpus settings, summary counts, runtime totals, path-group
  rollups, top opcode histograms, hot opcode pairs, scalar fast-path rates,
  function self-time/allocation summaries, and links or identifiers for the
  detailed profiles that explain each aggregate row.
- `test262-profile-aggregate.md`: a human-readable review summary with the
  same provenance header and ranked tables.
- `test262-profile-details/`: detailed profile JSON files used only after the
  aggregate points to a hotspot or regression worth investigating.

Main runs also publish the same profile data to Vercel Blob under the separate
`test262-profiles/` namespace. That namespace is intentionally distinct from the
compatibility dashboard's `test262/` namespace: `test262/` stores conformance
reports and daily pointers, while `test262-profiles/` stores profile aggregate,
Markdown summaries, compressed detail archives, and profile-specific daily
pointers for retained main-run review. The default paths are
`test262-profiles/runs/<artifactId>/aggregate.json.gz`,
`test262-profiles/runs/<artifactId>/summary.md`,
`test262-profiles/runs/<artifactId>/details.tar.gz`, and
`test262-profiles/daily/<YYYY-MM-DD>.json`.

Reviewers should compare the latest aggregate with the previous weekly profile
and nearby main-run profiles before opening detail files. Detailed profiles are
for explaining a ranked finding, such as a newly hot opcode pair, unexpectedly
low scalar fast-path hit rate, high allocation path group, or call-frame-heavy
feature area. Findings should turn the observed corpus behavior into concrete
compiler, bytecode, AST, parser, value-boxing, property-access, or call-frame
recommendations.

## Architecture

```text
GocciaTest262Runner
  → discover tests under suite/test/{built-ins,harness,intl402,language,staging}
  → optionally select one deterministic --shard-index/--shard-count partition
  → dispatch tests to native workers (--jobs=N)
  → for each test on its owning worker:
      → create a fresh thread-local runtime, heap, engine, and realm
      → read frontmatter, classify by phase (parse / runtime / positive)
      → build source = (stock harness includes) + body, with a tiny
        marker-emitting wrapper for most negative-runtime tests
      → install the Test262-only host and execute in process
      → classify into PASS / FAIL / WRAPPER_INFRA / TIMEOUT
      → destroy the engine and thread-local runtime before the next test
  → aggregate per top-level category
  → emit JSON and a console summary
scripts/run_test262_suite.ts --merge-shards
  → validate all shard indexes and test IDs, then emit one
    canonical report (and one aggregate profile from the merged details)
  → render the GitHub Step Summary and PR comment outside the native binary
```

## CI sharding

PR and main workflows run a matrix of independent Test262 shards. Membership is the
FNV-1a hash of the normalized test ID modulo the shard count, so it is stable
across platforms, independent of filesystem discovery order, and insensitive
to unrelated directory insertions. The workflow matrix is the only place that
sets parallelism. GitHub's `strategy.job-index` and `strategy.job-total`
provide the runner arguments and artifact names, so changing the matrix does
not require synchronised numeric edits elsewhere.

Both `--shard-index` and `--shard-count` are required, and the index is
zero-based:

```bash
./build/GocciaTest262Runner \
  --suite-dir=<checkout> \
  --shard-index 0 \
  --shard-count N \
  --output shard-0.json
```

After all shards finish, merge mode rejects missing or duplicate shard indexes,
run-metadata mismatches, duplicate tests, tests assigned to the wrong shard,
and incomplete corpus coverage before writing the canonical report:

```bash
bun scripts/run_test262_suite.ts \
  --merge-shards \
  --output test262-results.json \
  shard-*.json
```

The merged duration is the slowest shard duration, matching the effective CI
wall-clock. On main, shard jobs upload disjoint per-test profile details; merge
mode rebuilds the single aggregate JSON and Markdown reports from their union.
Downstream baseline caching, regression comments, dashboard publishing, and
profile publishing therefore continue to consume their existing canonical
artifact names.

## Wire protocol

| Test kind         | Pass signal                                                          | Fail signal                                                              |
|-------------------|----------------------------------------------------------------------|--------------------------------------------------------------------------|
| Sync positive     | engine returns normally                                              | an engine throw escapes                                                  |
| Async positive    | captured output contains `Test262:AsyncTestComplete`                 | `Test262:AsyncTestFailure:<name>: <msg>`, no marker, or a throw before `$DONE` |
| Negative runtime  | captured output contains `Test262:NegativeTestError:<expected-type>` | `Test262:NegativeTestNoError` or another error type                       |
| Top-level negative runtime | escaped throw has the expected error type                   | no throw or another error type                                           |
| Negative parse    | parsing or linking throws as expected                                | source executes cleanly                                                  |

Async markers are emitted by stock test262 `doneprintHandle.js` via
`$DONE`; the runner captures `print` output in the worker and scans it for
those strings. The
negative-runtime markers
(`Test262:NegativeTestError:...` / `Test262:NegativeTestNoError`) are
the only Goccia-specific marker addition; see "Wrapper templates"
below. Runtime-negative Script tests that depend on global declaration
instantiation run as top-level source instead, because wrapping them in
`try { ... }` would introduce a block scope and change the declaration
semantics under test.

## Wrapper templates

All four template kinds are produced by `TTest262App.BuildSource` in
`GocciaTest262Runner`. Bodies for positive sync, positive async,
empty, and script-scope tests are all `harness + body` — identical
shape, no special wrapping. Tests flagged `onlyStrict` receive a
`"use strict"` directive prefix before the harness so the runtime uses
strict directive semantics while the runner can still enable
compatibility-gated parser support for Script tests.

### Positive (sync, async, empty, script-scope)

```text
{harness_source}
{body}
```

### Negative runtime

```js
{harness_source}
try {
{body}
  print("Test262:NegativeTestNoError");
} catch (__gocciaT262_e) {
  var __gocciaT262_n = "unknown";
  if (__gocciaT262_e && typeof __gocciaT262_e === "object") {
    if (__gocciaT262_e.constructor && __gocciaT262_e.constructor.name) {
      __gocciaT262_n = __gocciaT262_e.constructor.name;
    }
  }
  print("Test262:NegativeTestError:" + __gocciaT262_n);
}
```

Top-level global-code runtime negatives that expect `SyntaxError` use the
positive template shape instead:

```text
{harness_source}
{body}
```

The runner then checks the escaped engine error type directly.

The error-class identification uses `e.constructor.name`, matching the
spec-visible constructor/prototype path. If this cannot identify the
thrown error class, the test is treated as an engine failure rather than
papered over by the harness.

The bindings `__gocciaT262_e` (catch parameter) and `__gocciaT262_n`
(local var inside catch) are the only Goccia-specific identifiers in
the generated source. Both are catch-block-scoped and cannot collide
with body-level vars.

### Negative parse

```text
{body}
```

Body alone. The parser runs, fails (or doesn't), and the runner
reads the exit code.

## Failure classification

| Source                                                | Surface                                  | Counts as          |
|-------------------------------------------------------|------------------------------------------|--------------------|
| Body assertion fails (`Test262Error` thrown)          | JS throw escapes the engine              | conformance fail   |
| Body throws another value                             | JS throw escapes the engine              | conformance fail   |
| Async body never calls `$DONE`                        | no captured `Test262:Async*` marker      | conformance fail   |
| Cooperative deadline expires                          | `TGocciaTimeoutError`                    | timeout            |
| Pascal-side exception                                 | non-Goccia exception at the test boundary | wrapper infra     |
| Native worker stalls beyond the watchdog              | worker produces a timeout result          | timeout            |
| Negative-runtime catch path itself fails              | no marker emitted                         | wrapper infra      |

`ClassifyResult` receives the structured engine outcome, captured output, and
formatted diagnostic. A native signal terminates that shard process; the merge
job then rejects the incomplete shard set instead of publishing partial
conformance numbers.

`wrapper_infra_failures` is gated to zero in CI. Any non-zero count
fails the run because the conformance numbers are not trustworthy when
the wrapper itself is broken.

## Visibility invariants

Bodies see only the identifiers stock test262 expects:

- `Test262Error` (from `sta.js`)
- `assert` and its methods (from `assert.js`)
- `$DONE`, `$DONOTEVALUATE` (from `sta.js` / `doneprintHandle.js` when included)
- `print` (Goccia engine global; stock `doneprintHandle.js` uses it for async markers)
- `$262` helpers implemented by Goccia's bundled host object:
  `detachArrayBuffer`, `evalScript`, `gc`, `global`, `createRealm`,
  `AbstractModuleSource`, and `IsHTMLDDA`
- `$262.agent` helpers used by Atomics tests: `start`, `broadcast`,
  `receiveBroadcast`, `report`, `getReport`, `sleep`, `monotonicNow`, and
  `leaving`
- `test262Host`, a private marker on the `Goccia` namespace exposed only by
  `Goccia.Test262.Host` so `$262.js` can reject accidental
  use outside the conformance host.
- Anything declared in test-included harness files (e.g. `compareArray`,
  `propertyHelper`)

Bodies do NOT see:

- `expect`, `describe`, `test`, `it`, `beforeAll`, `beforeEach`,
  `afterEach`, `afterAll`, `onTestFinished`, `runTests`, `mock`,
  `spyOn` — none of these exist on the Bare engine.
- `console`, `fetch`, `URL`, `performance` — the Test262 engine doesn't register
  Goccia's runtime extension.
- Optional `$262` hooks outside the bundled implementation. Tests that
  depend on unavailable host hooks fail honestly.

## Bundled harness adaptations

The runner loads stock tc39/test262 harness files from the pinned
checkout's `harness/` directory. `BUNDLED_INCLUDES` contains only
`$262.js`, because `$262` is not a stock harness helper: it is the
host-provided object test262 expects engines to supply.

Bundling rule: do not add compatibility copies of stock harness files.
If a stock helper fails, fix the language/runtime behavior or classify the
test as a genuine conformance failure. `$262.js` may grow only by adding
test262 host hooks or harness-local `$262` behavior.

`$262.evalScript()` and `$262.createRealm()` delegate to
`Goccia.Test262.Host`. `evalScript` parses and executes its source
as script code in the current realm. `createRealm` creates a fresh Goccia
engine/realm, returns a host record with the child realm's `globalThis`, its
own `evalScript`, and `createRealm`, and keeps the child engine alive for the
duration of the test run so cross-realm intrinsics remain valid.

`eval` is also host-gated. The Test262 runner installs the official host eval;
default Bare execution does not expose it. Bytecode direct calls preserve
the caller realm and caller lexical bindings, while shadowed or indirect eval
calls use ordinary function-call semantics.

`$262.agent` is host-gated in the same way. Agent `start` runs the supplied
source in a Test262-owned thread with the bundled `$262`
object installed; `broadcast`/`receiveBroadcast` share the provided value with
agent threads, and `report`/`getReport` provide the report queue used by
Atomics wait/notify tests.

## Strict mode

GocciaScript recognizes strict directive prologues at execution time for
the compatibility behaviors that otherwise depend on Script non-strict
mode. Independently, the engine's curated default semantics enforce most
strict-mode behaviors statically:

- Implicit globals throw `ReferenceError` (sloppy would create a global)
- `delete <identifier>` and non-configurable property deletion throw by default

The runner enables non-strict compatibility per test, not globally: Script
tests receive it, while module tests stay strict. `onlyStrict`
Script tests also receive the flag, but the injected directive keeps
`with`, non-strict assignment failures, legacy `delete` return values, and
regular-function nullish `this` coercion on the strict path. Remaining
`noStrict` tests rely on sloppy-only behaviors that
GocciaScript still does not provide and fail naturally as ordinary
conformance failures, not as wrapper-infra failures.

The runner enables syntax compatibility for traditional and `for...in` loops,
while loops, labels, and implicit arguments objects because test262 uses those
forms across both harness helpers and test bodies. The test's source type and
strictness still decide strict-mode semantics; `--compat-arguments-object`
only enables the implicit `arguments` binding. Strictness and parameter-list
shape decide whether that binding is unmapped or mapped.

## Source-phase import feature flags

The runner reads the test262 `features` frontmatter when a feature maps to a
Goccia engine option. `source-phase-imports` tests run with the base test262
flag set only. Tests that also declare `source-phase-imports-module-source`
enable the experimental JavaScript ModuleSource option, which provides
`ModuleSource` objects for the separate ESM Phase Imports proposal. This is not
an eligibility filter: every discovered test still runs, and feature metadata
only changes the host options used for that test.

## Path normalization

Test IDs are stored as POSIX-style relative paths under `suite/test/`.
On Windows the filesystem returns backslashes; the runner
normalizes to forward slashes via `NormalizeId` at every site
that uses an ID (glob-match, reporting, baseline lookup) so the same
test produces the same ID on both platforms.

## Test corpus

Default categories: `built-ins, harness, intl402, language, staging`
(everything except `annexB`).

- `annexB` (legacy/deprecated browser-only behavior) is excluded from
  the default run and is not a pre-1.0 release target. Some Annex B
  surfaces are implemented anyway (e.g. `String.prototype.substr`,
  `__proto__`, `__defineGetter__`) because they already support current
  compatibility work or layer cleanly as shims. Treat Annex B results as
  informational unless a future Web API/browser-compatibility profile
  deliberately re-opens the policy. See [ADR 0085](adr/0085-defer-annex-b-before-1-0.md).
- `intl402` covers Intl APIs (ECMA-402); tests exercise
  `Intl.getCanonicalLocales`, constructors, and formatting operations.
- `staging` is forward-looking proposals — engine-readiness signal.
- `harness` verifies test262's own harness functions work under our
  engine.

There is no eligibility filter. Every discovered test runs. Tests
that depend on missing features fail with a real diagnostic, not an
invisible skip. Each worker recreates its thread-local runtime after every
test, so the heap, roots, queues, executor, engine, and realm cannot leak into
the next case. Cooperative timeouts, per-test memory ceilings, and the native
worker watchdog bound ordinary hangs and memory growth.

## Known engine crashes

The native runner has no crash skip list and no generic eligibility filter.
A native signal terminates its shard, which prevents the merge job from
publishing an incomplete report. Any future quarantine must name the exact
test and link an engine issue; it must not become a feature-level filter.

## Updating the contract

Changes to `TTest262App.BuildSource` in `GocciaTest262Runner` are
verified by the full conformance run itself (no separate regression
suite). After any wrapper-template change:

1. Run locally:

   ```bash
   ./build.pas test262runner
   ./build/GocciaTest262Runner --suite-dir=<checkout> \
     --output=local-results.json
   ```

2. Confirm `wrapper_infra_failures: 0` in the summary.
3. Diff `local-results.json` against the prior baseline; investigate
   any sign change in pass/fail counts before opening the PR.

## Updating the SHA pin

The test262 SHA is pinned in `scripts/test262-suite-sha.txt`. Both
`.github/workflows/ci.yml` and `.github/workflows/pr.yml` read that file
before checking out `tc39/test262`, so the cached `main` baseline and a PR
run measure the same upstream corpus without weekly bump PRs needing to
modify workflow files. The weekly cron at `.github/workflows/test262-bump.yml`
opens a PR every Monday with the latest tc39/test262 main SHA; the PR's
standard CI run posts the per-category delta vs. the previous main baseline.
Merge once the delta is acceptable.

Manual bump: `bun scripts/test262-bump-pin.ts <40-hex-sha>`.
