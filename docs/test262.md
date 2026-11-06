# test262 Harness Contract

How GocciaScript runs the official TC39 test262 conformance suite, the
contract the runner guarantees, and the boundary between
conformance failures, wrapper-infrastructure failures, and runner-level
errors.

## What test262 is, and why we run it

[test262](https://github.com/tc39/test262) is the TC39-maintained
conformance suite for ECMAScript. ~52K tests cover every observable
behavior in the language and built-ins, plus Intl, staging proposals,
and the harness itself. We run it as an indicator metric and regression
signal to track which spec corners GocciaScript implements, where the
engine diverges, and how each PR moves those numbers. The generated
reports are the source of truth for ECMAScript compatibility status.

For the original LoaderBare-plus-stock-harness decision, see
[ADR 0042](adr/0042-test262-loaderbare-harness.md). The native-runner
replacement is recorded in
[ADR 0120](adr/0120-native-test262-runner.md).

## Executive summary

- Test262 runs via the dedicated native `GocciaTest262Runner`, never via
  `GocciaTestRunner` or `GocciaScriptLoaderBare`. Tests execute inside a
  neutral engine without `expect`, `describe`, `test`, lifecycle hooks,
  mocks, or `runTests`.
- Stock tc39/test262 harness files are loaded from the pinned test262
  checkout's `harness/` directory. The runner bundles one file,
  `scripts/test262_harness/$262.js`, the host-provided hook object.
- `Goccia.Test262.Host` installs the host hooks only in engines owned by the
  Test262 runner.
- Test feature metadata may add explicit engine options when test262 splits
  proposal layers more narrowly than the base engine flag set.
- Each test file runs once per mode test262 requires: an unflagged Script
  test runs twice, non-strict and strict, and each result records its
  `mode`. Each harness file runs as a non-strict global Script of its own
  before the test, and a strict run adds `"use strict";` to the test's own
  source only. The runner catches engine outcomes directly; the stock async markers
  are the only JavaScript-to-host verdict protocol.
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
five least-covered path groups from the latest report. Its figures count runs,
not files (see "Strict mode"), and it shows the strict and non-strict runs
separately for reports that record each run's mode; reports graded before
contract 4 ran each file once and show no per-mode figures, so the pass-rate
timeline steps where the two-mode runs begin. The "JSON result" link
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
new results arrive only a few times per day. History reads go through immutable
monthly snapshots of the daily pointers, stored under
`test262/history/v1/<YYYY-MM>/`, so a request does not fetch every pointer
serially; see
[ADR 0115](adr/0115-immutable-blob-history-snapshots.md).

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
  function self-time, total-time and allocation summaries (each function's
  figures added up over the tests, where a test's total time counts every
  outermost activation once; see
  [Function Profile](profiling.md#function-profile---profilefunctions)), and
  links or identifiers for the detailed profiles that explain each aggregate
  row.
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
  → read each selected file's flags and expand it into its runs, one per mode
    (see "Strict mode")
  → dispatch runs to native workers (--jobs=N)
  → on POSIX, fork one short-lived child per run without starting a new binary
  → for each run in its owning worker process:
      → create one runtime, heap, engine, and realm
      → read frontmatter, classify by phase (parse / runtime / positive)
      → install the Test262-only host, evaluate each harness file as its
        own Script, then evaluate the test file (prefixed with "use strict";
        in a strict run) as a Script or Module, in process
      → classify into PASS / FAIL / WRAPPER_INFRA / TIMEOUT
      → return the Pascal result and exit the worker process
  → aggregate per top-level category and per mode
  → emit JSON and a console summary
scripts/run_test262_suite.ts --merge-shards
  → validate all shard indexes and runs, keyed by (test ID, mode), then emit one
    canonical report (and one aggregate profile from the merged details)
  → render the GitHub Step Summary and PR comment outside the native binary
```

## CI sharding

PR and main workflows run a matrix of independent Test262 shards. Membership is the
FNV-1a hash of the normalized test ID modulo the shard count, so both runs of a
file land in the same shard, and membership is stable
across platforms, independent of filesystem discovery order, and insensitive
to unrelated directory insertions. The workflow matrix is the only place that
sets parallelism. GitHub's `strategy.job-index` and `strategy.job-total`
provide the runner arguments and artifact names, so changing the matrix does
not require synchronised numeric edits elsewhere.

Both `--shard-index` and `--shard-count` are required, and the index is
zero-based. Run the runner from the repository root: the bundled `$262.js`
is resolved relative to the working directory, and every test reports
`WRAPPER_INFRA` otherwise.

```bash
./build/GocciaTest262Runner \
  --suite-dir=<checkout> \
  --shard-index 0 \
  --shard-count N \
  --output shard-0.json
```

After all shards finish, merge mode rejects missing or duplicate shard indexes,
run-metadata mismatches, duplicate runs (the same test ID and mode twice),
tests assigned to the wrong shard, and incomplete corpus coverage (a
discovered file with no run) before writing the canonical report:

```bash
bun scripts/run_test262_suite.ts \
  --merge-shards \
  --output test262-results.json \
  shard-*.json
```

Sharding is by file, so a shard's run count is about twice its file count;
the shard count and job timeouts were checked against that two-mode load
when it began (#2068).

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
| Negative runtime  | the test's escaped throw has the expected error type                 | no throw or another error type                                           |
| Negative parse / resolution | parsing, early errors, or linking raise `negative.type` before evaluation | clean run, `$DONOTEVALUATE` reached, a JS-level throw, or another error type |

Async markers are emitted by stock test262 `doneprintHandle.js` via
`$DONE`; the runner captures `print` output in the worker and scans it for
those strings. Every other verdict comes from how the test's own Script or
Module ended; see "Harness evaluation" below.

## Harness evaluation

`TTest262App.ExecuteOne` in `GocciaTest262Runner` evaluates each test as a
sequence of separate Scripts in one realm, as test262's `INTERPRETING.md`
describes ("Test262-Defined Bindings", the `async` flag, `includes`):

1. `$262.js`, then `assert.js` and `sta.js`;
2. `doneprintHandle.js` for an `async` test;
3. the test's `includes`, in the order given;
4. the test file itself: a Script, or a Module for a `module` test.

Each harness file is its own global Script, evaluated through
`EvaluateTest262Script` (`Goccia.Test262.Host`), the same path as
`$262.evalScript`. Its own directive prologue alone decides its
strictness, so harness files are non-strict, in a strict run and before a
module test too. Their top-level declarations are global bindings that the
test, and any later Script, sees and is checked against, as
GlobalDeclarationInstantiation requires; a `var` or function declaration is
also a property of `globalThis`. A test file a test imports, such as one that
`import()`s itself, is loaded as its own source alone: the harness has
already run in the realm.

The test file is evaluated whole, frontmatter comment included, so
diagnostics report its own lines. A strict Script run inserts
`"use strict";` and a newline as the initial characters of the test file,
and of nothing else; its diagnostics are therefore one line past the file's.
A `raw` test runs alone: no harness Script, no prefix, the file exactly as
written. Which runs a test gets is described under "Strict mode".

No test source is wrapped. A negative-runtime test is graded on the
completion of its own Script or Module: the error that escapes it must have
`negative.type` as its `constructor.name`, read from data properties only,
so no accessor getter runs. Because nothing wraps the test, its top-level
declarations keep their global (or module) scope. If the thrown value's
class cannot be identified, the test fails rather than being papered over
by the harness.

A harness Script that fails to parse or throws means the test never ran:
that is a wrapper-infrastructure result naming the include, not a
conformance result. A harness Script that exceeds the timeout is a timeout.

### Negative parse

Used for `negative.phase` `parse` and `resolution` (and `early`, should it
reappear). Like every other non-raw test, it gets the stock harness first,
as the test262 interpretation rules require, so `sta.js` defines
`$DONOTEVALUATE()`. Each non-raw parse or resolution test calls it as its
first statement. `raw` tests get no harness and no `"use strict";` prefix,
and keep their original source, frontmatter included; the evaluated ones
throw the same string literal themselves. The test passes only when all of
these hold:

1. execution of the test's own Script or Module raised;
2. the body was not evaluated: the escaped value is not the string
   `$DONOTEVALUATE()` throws (`Test262: This statement should not be
   evaluated.`);
3. the error was raised before evaluation, as a host-level engine error
   from the parser, early-error checks, or module linking (of these, only
   `TGocciaSyntaxError` is named `SyntaxError`). A `SyntaxError` thrown as a
   JS value during evaluation does not count;
4. the error name equals `negative.type`.

An engine that misses an early error therefore runs the body, reaches
`$DONOTEVALUATE()`, and fails with `body was evaluated ($DONOTEVALUATE
reached)` instead of passing on the resulting throw (see #1571). Any
`SyntaxError` raised before evaluation passes, whichever rule raised it; this
is the same grading the test262 interpretation rules and other runners apply.

## Failure classification

| Source                                                | Surface                                  | Counts as          |
|-------------------------------------------------------|------------------------------------------|--------------------|
| Body assertion fails (`Test262Error` thrown)          | JS throw escapes the engine              | conformance fail   |
| Body throws another value                             | JS throw escapes the engine              | conformance fail   |
| Async body never calls `$DONE`                        | no captured `Test262:Async*` marker      | conformance fail   |
| Body leaves a promise rejected with no handler        | none: the runner's engines set `UnhandledRejections := urIgnore` ([Errors](errors.md#unhandled-promise-rejections)) | not a failure |
| Cooperative deadline expires                          | `TGocciaTimeoutError`                    | timeout            |
| Pascal-side exception during engine execution         | structured engine diagnostic              | conformance fail |
| Runner/harness protocol exception                     | outside engine execution                   | wrapper infra     |
| Native worker stalls beyond the watchdog              | supervisor kills only that worker         | timeout            |
| A harness Script fails to parse or throws             | `harness include <name> failed`           | wrapper infra      |

`ClassifyResult` receives the structured engine outcome, captured output, and
formatted diagnostic. On POSIX, a native signal terminates only the current
worker process. The supervisor records a wrapper-infrastructure result for that
test and continues the shard; a watchdog expiry is recorded as a timeout.

`summary.wrapperInfraFailures` is gated to zero in CI. Any non-zero count
fails the run because the conformance numbers are not trustworthy when
the wrapper itself is broken.

## Visibility invariants

Bodies see only the identifiers stock test262 expects:

- `Test262Error` (from `sta.js`)
- `assert` and its methods (from `assert.js`)
- `$DONE`, `$DONOTEVALUATE` (from `sta.js` / `doneprintHandle.js` when included)
- `print` (installed by the Test262 runner, which captures its output; stock
  `doneprintHandle.js` uses it for async markers)
- `$262` helpers implemented by Goccia's bundled host object:
  `detachArrayBuffer`, `evalScript`, `gc`, `global`, `createRealm`,
  `AbstractModuleSource`, and `IsHTMLDDA`
- `$262.agent` helpers used by Atomics tests: `start`, `broadcast`,
  `receiveBroadcast`, `report`, `getReport`, `sleep`, `monotonicNow`, and
  `leaving`
- Two properties on the `Goccia` namespace, both installed only by
  `Goccia.Test262.Host`: the `test262Host` marker, which `$262.js` checks to
  reject accidental use outside the conformance host, and the `test262` object
  holding the host hooks `$262.js` delegates to.
- Anything declared in test-included harness files (e.g. `compareArray`,
  `propertyHelper`)

Bodies do NOT see:

- `expect`, `describe`, `test`, `it`, `beforeAll`, `beforeEach`,
  `afterEach`, `afterAll`, `onTestFinished`, `runTests`, `mock`,
  `spyOn` — none of these exist in the Test262 runner's engines.
- `console`, `fetch`, `URL`, `performance` — the Test262 engine doesn't register
  Goccia's runtime extension.
- Optional `$262` hooks outside the bundled implementation. Tests that
  depend on unavailable host hooks fail honestly.

## Bundled harness adaptations

The runner loads stock tc39/test262 harness files from the pinned
checkout's `harness/` directory; "Harness evaluation" above gives their
order and how they run. Only `$262.js` is bundled: `$262` is not a stock
harness helper but the host-provided object test262 expects engines to
supply.

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

`eval` is also host-gated. The Test262 runner installs it through the
engine's `EnableEval` option, the one embedders use; default Bare execution
does not expose it. Bytecode direct calls preserve
the caller realm and caller lexical bindings, while shadowed or indirect eval
calls use ordinary function-call semantics.

`$262.agent` is host-gated in the same way. Agent `start` runs the supplied
source in a Test262-owned thread with the bundled `$262`
object installed; `broadcast`/`receiveBroadcast` share the provided value with
agent threads, and `report`/`getReport` provide the report queue used by
Atomics wait/notify tests.

## Strict mode

test262's `INTERPRETING.md` ("Strict Mode" and the `flags` list) requires each
test to run in both modes unless a flag says otherwise. The runner reads the
flags from the frontmatter, written inline (`flags: [noStrict]`) or as a YAML
block list (`flags:` followed by indented `- noStrict` lines, as `staging/sm` writes
them), and gives each file these runs, in this order of precedence:

| Flag | Runs |
|------|------|
| `module` | once, as module code, recorded as `strict` (module code is strict); no prefix |
| `raw` | once, `non-strict`, the file exactly as written |
| `onlyStrict` | once, `strict` |
| `noStrict` | once, `non-strict` |
| none of these | twice: `non-strict`, then `strict` |

A strict Script run inserts `"use strict";` and a newline in front of the
test file only (see "Harness evaluation"). `TTest262App.ExpandRunModes` turns
each selected file into its runs after sharding, and `ExecuteOne` takes the
strictness from the run, the one place it is decided.

Each result carries `"mode": "non-strict" | "strict"` beside `id`, and a
run is identified by the pair. The report counts runs: `summary.totalRun`
and every pass, fail, timeout and wrapper-infra count, overall and per
`byCategory` entry, are run counts, and `summary.byMode` and each category's
`byMode` break them down per mode, always listing both modes.
`summary.totalDiscovered` still counts test files.

A shard report also records `summary.expectedRuns`, the runs the runner
planned per mode from the flag rules above. The merge rejects a shard whose
results do not match it, and a contract-4 result without `mode`, so a missing
strict run of a file that still has its non-strict run is an error and the
merge never re-implements the frontmatter rules. A merged report leaves the
field out. The merge and the PR comparison key results by (id, mode), so a
run that regresses only in strict mode blocks a merge like any other; a run
with no counterpart in the baseline, such as a file's first strict run, is new
rather than a regression. A strict run's profile is written to
`per-test/<id>.strict.profile.json`, beside the non-strict run's
`per-test/<id>.profile.json`.

GocciaScript recognizes strict directive prologues at execution time for
the compatibility behaviors that otherwise depend on Script non-strict
mode. Independently, the engine's curated default semantics enforce most
strict-mode behaviors statically:

- Implicit globals throw `ReferenceError` (sloppy would create a global)
- `delete <identifier>` and non-configurable property deletion throw by default

The runner enables non-strict compatibility for every test. Module code
stays strict whatever the flag says, so for a module test the flag reaches
only Script code: its harness Scripts, `$262.evalScript`, indirect `eval`
and the `Function` constructor. `onlyStrict` tests also receive the flag,
but the directive inserted in front of the test file keeps `with`,
non-strict assignment failures, legacy `delete` return values, and
regular-function nullish `this` coercion on the strict path for the test's
own code, while the harness Scripts stay non-strict. A strict run of an
unflagged test is set up the same way. Remaining
`noStrict` tests rely on sloppy-only behaviors that
GocciaScript still does not provide and fail naturally as ordinary
conformance failures, not as wrapper-infra failures.

Every test runs with ASI, `var`, `function` declarations, traditional `for`,
`while`/`do...while`, loose equality, labels, `for...in`, and implicit
`arguments` objects enabled, because test262 uses those forms across both
harness helpers and test bodies. The test's source type and
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

- `annexB` (legacy browser behavior) is part of the conformance target
  and a release gate from 0.18.0. It is not in the default run yet:
  [#2069](https://github.com/frostney/GocciaScript/issues/2069) adds it
  once the runner's `IsHTMLDDA` harness clash is fixed. Until then, run it with
  `--categories=annexB`. New Annex B surfaces will go behind a
  compatibility flag, added by the 0.18.0 Annex B work, that the runner's
  engine profile turns on. See
  [ADR 0177](adr/0177-annex-b-is-part-of-the-test262-conformance-target.md).
- `intl402` covers Intl APIs (ECMA-402); tests exercise
  `Intl.getCanonicalLocales`, constructors, and formatting operations.
- `staging` is forward-looking proposals — engine-readiness signal.
- `harness` verifies test262's own harness functions work under our
  engine.

There is no eligibility filter. Every discovered test runs. Tests
that depend on missing features fail with a real diagnostic, not an
invisible skip. On POSIX, each worker process owns exactly one run, so its
heap, roots, queues, VM, engine, and realm cannot leak into the next case.
Process exit also returns allocator-retained storage to the operating system.
Cooperative timeouts (`--timeout`, default `20s`), per-test memory ceilings
(`--max-memory`, a size such as `512MiB`), and the native worker watchdog bound
ordinary hangs and memory growth. The runner accepts no capability flags: it
rejects `--allow-*` and accepts `--deny-*` as a no-op (see
[Permissions](permissions.md#what-each-binary-honors)). Internally each test
engine may read only the `--suite-dir` checkout, which test262 fixtures need
for computed `import()` specifiers.

## Known engine crashes

The native runner has no crash skip list and no generic eligibility filter.
On POSIX, a native signal is contained to one worker and classified as wrapper
infrastructure while the supervisor continues the shard. Any future quarantine
must name the exact test and link an engine issue; it must not become a
feature-level filter.

## Updating the contract

The contract has a version, `GRADING_CONTRACT_VERSION` in
`GocciaTest262Runner`, published in every report as `run.gradingContract`
(reports without the field were graded under contract 1). Bump it when a
grading or template change moves tests between PASS and FAIL without an engine
change. When the cached `main` baseline was graded under a different contract,
the PR comment still lists every PASS → FAIL transition but does not fail the
regression gate; the next `main` run republishes the baseline under the new
contract. Contract 2 (#1571) made negative parse and resolution tests require a
pre-evaluation error of `negative.type`. Contract 3 (#2067) evaluates each
harness file as a separate non-strict Script, applies `"use strict";` to the
test file only, and grades every negative-runtime test on its own escaped
throw, with no `try` wrapper. Contract 4 (#2068) runs each unflagged Script
test in both modes and records each result's `mode`; a contract-3 baseline
has one run per file and no modes, so each of its results pairs with the
file's only run, or with its non-strict run when the file now runs in both
modes, and the gate does not block on them.

`scripts/test-cli-apps.ts` covers the negative grading paths with a small
synthetic suite. After any grading or harness-evaluation change, also verify against the
full conformance run:

1. Run locally, from the repository root:

   ```bash
   ./build.pas test262runner
   ./build/GocciaTest262Runner --suite-dir=<checkout> \
     --output=local-results.json
   ```

2. Confirm `Wrapper infra: 0` in the console summary
   (`summary.wrapperInfraFailures` in `local-results.json`).
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
