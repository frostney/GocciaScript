# test262 Harness Contract

How GocciaScript runs the official TC39 test262 conformance suite, the
contract the runner guarantees, and the boundary between
conformance failures, wrapper-infrastructure failures, and runner-level
errors.

## What test262 is, and why we run it

[test262](https://github.com/tc39/test262) is the TC39-maintained
conformance suite for ECMAScript. Over 50,000 test files, most of them
run twice (non-strict and strict), cover every observable behavior in
the language and built-ins, plus Annex B, Intl, staging proposals, and
the harness itself. We run it as an indicator metric and regression
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
  checkout's `harness/` directory. The runner bundles one file, `$262.js`,
  the host-provided hook object: the engine's version is
  `source/engine/prelude/test262host.js`, compiled into the binary.
- `Goccia.Test262` installs the host hooks only in engines owned by the
  Test262 runner.
- Every test runs on the same engine options; test feature metadata does
  not change them.
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
  → discover tests under suite/test/{annexB,built-ins,harness,intl402,language,staging}
  → optionally select one deterministic --shard-index/--shard-count partition
  → read each selected file's flags and expand it into its runs, one per mode
    (see "Strict mode")
  → run up to --jobs=N runs at once
  → on POSIX, fork one short-lived child per run without starting a new binary;
    without fork (Windows), run each on a thread of its own
  → for each run in its owning worker:
      → create one engine (InitTest262Engine in Goccia.Test262): heap and realm
      → read frontmatter, classify by phase (parse / runtime / positive)
      → install the Test262-only host, evaluate each harness file as its
        own Script, then evaluate the test file (prefixed with "use strict";
        in a strict run) as a Script or Module, in process
      → classify into PASS / FAIL / WRAPPER_INFRA / TIMEOUT
      → hand the result back (a result file from a forked child) and exit
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
zero-based. The bundled `$262.js` is compiled into the runner, so it runs
from any working directory.

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

Each harness file is its own global Script, compiled and run in the
realm by `RunTest262Case` (`Goccia.Test262`), as `$262.evalScript` runs its
source. Its own directive prologue alone decides its
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
   from the parser, early-error checks, or module linking, named by the
   error's constructor. A `SyntaxError` thrown as a
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
| Body leaves a promise rejected with no handler        | none: the test262 host does not track unhandled rejections | not a failure |
| Run exceeds `--timeout`                               | POSIX: the runner kills the worker; without fork: the engine's deadline | timeout |
| Pascal-side exception during engine execution         | `crash` outcome naming the exception class | conformance fail |
| The compiler refuses the parsed test                  | `compile` outcome                         | conformance fail   |
| Runner/harness protocol exception                     | outside engine execution                   | wrapper infra     |
| A harness Script fails to parse or throws             | `harness include <name> failed`           | wrapper infra      |

`ClassifyResult` receives the structured engine outcome (`TGocciaT262Outcome`)
and the captured output. On POSIX, a native signal terminates only the current
worker process. The runner records a wrapper-infrastructure result for that
test and continues the shard; a worker it killed at the timeout is recorded as
a timeout.

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
  `AbstractModuleSource`, and `IsHTMLDDA`. `$262` is the only global
  binding `$262.js` declares, so a test may declare
  any other name at its top level. The `annexB` emulates-undefined tests
  declare `IsHTMLDDA` there, most as `var IsHTMLDDA = $262.IsHTMLDDA;`,
  which a global lexical `IsHTMLDDA` would turn into a `SyntaxError`
  (ES2026 §16.1.7 GlobalDeclarationInstantiation, steps 3.a and 4.a).
  `$262.IsHTMLDDA` is callable and returns `null`, as `INTERPRETING.md`
  requires.
- `$262.agent` helpers used by Atomics tests: `start`, `broadcast`,
  `receiveBroadcast`, `report`, `getReport`, `sleep`, `monotonicNow`, and
  `leaving`
- `GocciaHost`, a non-enumerable global installed only by `Goccia.Test262`
  (`InstallHostHooks`): the host hooks `$262.js` delegates to. `$262.js`
  throws when it is missing, which rejects accidental use outside the
  conformance host.
- `setTimeout`, which `$262.js` defines for the harness's polling loops
  (`atomicsHelper.js`): its callback runs from a microtask after a short
  sleep, so it involves no timer queue.
- The engine core's own `console` (`log`, `error`, `warn`), `performance.now()`
  and `eval`
- Anything declared in test-included harness files (e.g. `compareArray`,
  `propertyHelper`)

Bodies do NOT see:

- `expect`, `describe`, `test`, `it`, `beforeAll`, `beforeEach`,
  `afterEach`, `afterAll`, `onTestFinished`, `runTests`, `mock`,
  `spyOn` — none of these exist in the Test262 runner's engines.
- `fetch`, `URL`, the `Goccia` namespace and the rest of the CLI host
  (`Goccia.HostAPI`) — the Test262 engine does not install it.
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

`$262.evalScript()` and `$262.createRealm()` delegate to `GocciaHost`
(`Goccia.Test262`). `evalScript` parses and executes its source as script code
in the current realm and returns its completion value. `createRealm` creates
a new realm in the same engine, installs the host hooks and `$262.js` in it,
and returns that realm's `$262`.

`eval` is part of the engine's core realm. The CLI hosts remove it from the global
object (`Goccia.HostAPI`); the Test262 engine installs no CLI host and keeps it.
A direct `eval` call sees the caller's realm and lexical bindings, while a
shadowed or indirect call is an indirect eval.

`$262.agent` is host-gated in the same way (`Goccia.Agent`). Agent `start` runs
the supplied source in an engine of its own on a thread of its own, with
`$262.agent` installed; `broadcast`/`receiveBroadcast` share the provided value with
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

The test262 engine compiles every test, harness Script and `--eval-host`
probe under the engine's unrestricted language profile (`TLangProfile` with
`Restricted` off): the whole language, including everything the CLIs'
compatibility flags enable — ASI, `var`, function declarations, traditional
`for`, `while`/`do...while`, loose equality, labels, `for...in`, the
`arguments` object, and non-strict Script semantics — because test262 uses
those forms across both harness helpers and test bodies
([ADR 0175](adr/0175-test262-harness-includes-are-separate-non-strict-scripts.md)).
Non-strict code gets sloppy-mode semantics: an assignment to an undeclared
name creates a global, `delete` of a non-configurable property returns
`false`, and `with` is available. The directive inserted in front of a strict
run's test file, and module code, give the test's own code strict semantics,
while the harness Scripts stay non-strict.

## Source-phase import feature flags

The runner does not map `features` frontmatter to engine options: every test
runs on the same engine. The engine's experimental JavaScript `ModuleSource` objects
(`TGocciaEngine.JSModuleSources`, the ESM Phase Imports proposal) stay off in the
test262 engine, so `source-phase-imports-module-source` tests run without them
and fail where they need them. This is not an eligibility filter: every
discovered test still runs.

## Path normalization

Test IDs are stored as POSIX-style relative paths under `suite/test/`.
On Windows the filesystem returns backslashes; the runner
normalizes to forward slashes via `NormalizeId` at every site
that uses an ID (glob-match, reporting, baseline lookup) so the same
test produces the same ID on both platforms.

## Test corpus

Default categories: `annexB, built-ins, harness, intl402, language,
staging`, every top-level category in `suite/test`.

- `annexB` (legacy browser behavior) is part of the conformance target
  and a release gate from 0.18.0. It runs by default and counts toward
  the published pass rate and the PR comparison like any other category.
  New Annex B surfaces go behind `--compat-annex-b`; the test262 engine's
  unrestricted profile includes every such form (see
  [Strict mode](#strict-mode)). See
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
Without fork, each run makes its engine on its own thread and ends it after
the run (`DoneTest262Engine`). Timeouts (`--timeout`, default `20s`) and
per-test heap ceilings (`--max-memory`, a size such as `512MiB`, default
2 GiB) bound ordinary hangs and memory growth. The runner accepts no capability flags: it
rejects `--allow-*` and accepts `--deny-*` as a no-op (see
[Permissions](permissions.md#what-each-binary-honors)). The test engines do
not apply the CLI hosts' read policy (`SetReadPolicy` in `Goccia.HostAPI`), so
test262 fixtures' computed `import()` specifiers load from the checkout.

## Known engine crashes

The native runner has no crash skip list and no generic eligibility filter.
On POSIX, a native signal is contained to one worker and classified as wrapper
infrastructure while the runner continues the shard. Any future quarantine
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

### Corrections the pin lacks

When an upstream test contradicts the current specification and a
tc39/test262 pull request corrects it, `scripts/test262-patches/` holds that
pull request's diff, named after it, and CI applies it to the checkout with
`scripts/apply-test262-patches.sh test262-suite` before the run. Apply it to
a local checkout the same way. The script fails when a patch no longer
applies, which happens once the pin includes the pull request: delete the
patch in the bump PR. The current patch is tc39/test262#5112, which brings
`annexB/language/function-code/block-decl-func-skip-arguments.js` in line with
ES2026 §10.2.11 FunctionDeclarationInstantiation: its Annex B step checks
`parameterNames`, not `argumentsObjectNeeded`, so a block function named
`arguments` is still hoisted.
