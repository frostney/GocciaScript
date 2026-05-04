# test262 Harness Contract

How GocciaScript runs the official TC39 test262 conformance suite, the
contract the orchestrator guarantees, and the boundary between
conformance failures, wrapper-infrastructure failures, and runner-level
errors.

## Executive summary

- Test262 runs via `GocciaScriptLoaderBare`, never via `GocciaTestRunner`.
- Wrapper bodies execute inside a neutral engine — `expect`, `describe`,
  `test`, lifecycle hooks, mocks, `runTests` are not registered.
- Stock tc39/test262 harness files (`sta.js`, `assert.js`,
  `doneprintHandle.js`, etc.) are loaded directly from the pinned
  test262 checkout's `harness/` directory. No bundled custom copies.
- The orchestrator drives via process exit code + stdout markers, the
  same convention `test262-harness`/`eshost`/test262.fyi use.
- Wrapper-infrastructure failures are classified separately from
  conformance failures and gated to zero in CI.

## Why

PR #491 exposed that the previous adapter ran wrapped tests *inside*
`GocciaTestRunner`, which registers test-library globals (`expect`,
`describe`, `test`, hooks, mocks). The wrapper had to capture, hide,
and selectively restore those globals so test bodies wouldn't see
them; failure capture used `undefined` as a sentinel; `globalThis.runTests`
got clobbered. Any wrapper drift inflated or deflated conformance
numbers, and a chunked-runner crash marked thousands of tests ERROR
indistinguishably from real engine regressions.

The fix is structural: run each test in a neutral binary that simply
doesn't register those globals, and report results via the exit code +
stdout pattern stock test262 already uses. There's nothing to leak
because nothing is registered.

## Architecture

```text
scripts/run_test262_suite.ts
  → discover tests under suite/test/{built-ins,harness,intl402,language,staging}
  → for each test (parallel pool, --jobs=N):
      → read frontmatter, classify by phase (parse / runtime / positive)
      → build source = (stock harness includes) + body, with a tiny
        marker-emitting wrapper for negative-runtime
      → spawn ./build/GocciaScriptLoaderBare with stdin = source
      → capture (exitCode, stdout, stderr)
      → classify into PASS / FAIL / WRAPPER_INFRA / TIMEOUT
  → aggregate per top-level category
  → emit JSON, console summary, GitHub Step Summary table
```

## Wire protocol

| Test kind         | Pass signal                                                          | Fail signal                                                              |
|-------------------|----------------------------------------------------------------------|--------------------------------------------------------------------------|
| Sync positive     | exit 0                                                                | exit non-zero (stderr is the diagnostic)                                 |
| Async positive    | stdout contains `Test262:AsyncTestComplete`                           | stdout contains `Test262:AsyncTestFailure:<name>: <msg>`, OR no marker before timeout, OR engine exits before $DONE |
| Negative runtime  | stdout contains `Test262:NegativeTestError:<expected-type>`           | `Test262:NegativeTestNoError`, OR `Test262:NegativeTestError:<other>`    |
| Negative parse    | exit non-zero (parse failed as expected)                              | exit 0 (parse succeeded)                                                 |

The async markers come from stock tc39/test262 `doneprintHandle.js` —
its `$DONE` function calls `print("Test262:AsyncTestComplete")` (or the
failure variant). The negative-runtime markers are the only
Goccia-specific addition; see "Wrapper templates" below.

## Wrapper templates

All four template kinds are produced by `buildTestSource` in
`scripts/run_test262_suite.ts`. Bodies for positive sync, positive async,
empty, and script-scope tests are all `harness + body` — identical
shape, no special wrapping. There is no per-template strict-directive
injection (GocciaScript's parser ignores `"use strict"`; its curated
semantics enforce strict-equivalent behaviors statically — see
"Strict mode" below).

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
  if (__gocciaT262_e && __gocciaT262_e.constructor && __gocciaT262_e.constructor.name) {
    print("Test262:NegativeTestError:" + __gocciaT262_e.constructor.name);
  } else {
    print("Test262:NegativeTestError:unknown");
  }
}
```

The single binding `__gocciaT262_e` is catch-param-scoped and cannot
collide with body-level vars. This is the only Goccia-specific
identifier in the generated source.

### Negative parse

```text
{body}
```

Body alone. The parser runs, fails (or doesn't), and the orchestrator
reads the exit code.

## Failure classification

| Source                                                | Surface                                  | Counts as          |
|-------------------------------------------------------|------------------------------------------|--------------------|
| Body assertion fails (`Test262Error` thrown)          | exit 1, stderr has formatted error       | conformance fail   |
| Body throws other Error                               | exit 1, stderr captures the error        | conformance fail   |
| Body throws non-Error (`undefined`, `null`, etc.)     | exit 1, stderr carries the formatted value | conformance fail |
| Async body never calls `$DONE`                        | no `Test262:Async*` marker, exit 0        | conformance fail   |
| Engine killed by signal (SIGSEGV, OOM)                | `signalCode != null` or exit > 1          | wrapper infra      |
| Pascal-side error (`EAccessViolation`, `ESocket`, …)  | stderr starts with Pascal class name      | wrapper infra      |
| Per-test wall-clock timeout                            | `AbortSignal.timeout` fired               | timeout            |
| Negative-runtime catch path itself crashes            | no marker emitted at all                  | wrapper infra      |

The classifier (`classifyRunResult` in `run_test262_suite.ts`) reads
exit code, signal, stdout, and stderr to pick the bucket. The
`PASCAL_INFRA_RE` regex deliberately excludes the bare `Error:` prefix
because Goccia's bytecode mode uses it for legitimate JS errors — the
prefix alone is not a wrapper-infra signal.

`wrapper_infra_failures` is gated to zero in CI. Any non-zero count
fails the run because the conformance numbers are not trustworthy when
the wrapper itself is broken.

## Visibility invariants

Bodies see only the identifiers stock test262 expects:

- `Test262Error` (from `sta.js`)
- `assert` and its methods (from `assert.js`)
- `$DONE`, `$DONOTEVALUATE` (from `sta.js` / `doneprintHandle.js` when included)
- `print` (Goccia engine global; stock `doneprintHandle.js` calls it)
- Anything declared in test-included harness files (e.g. `compareArray`,
  `propertyHelper`)

Bodies do NOT see:

- `expect`, `describe`, `test`, `it`, `beforeAll`, `beforeEach`,
  `afterEach`, `afterAll`, `onTestFinished`, `runTests`, `mock`,
  `spyOn` — none of these exist on the Bare engine.
- `console`, `fetch`, `URL`, `performance` — Bare doesn't register
  Goccia's runtime extension.
- `$262` — Goccia doesn't implement test262's optional host hooks
  object. Tests that depend on it fail honestly.

## Strict mode

GocciaScript's parser does not process `"use strict"` directives — it
neither recognizes nor enforces strict-mode toggling at parse time.
Independently, the engine's curated semantics enforce most strict-mode
behaviors statically:

- Implicit globals throw `ReferenceError` (sloppy would create a global)
- `delete <identifier>` always throws (sloppy is silent)
- `arguments` and `with` are excluded by language design
- `eval` is not implemented

The orchestrator therefore does not inject `"use strict"` for `onlyStrict`
tests — the body's own directive (if present) is parsed and ignored,
which is correct because the engine's behavior is already strict-equivalent
for the things `onlyStrict` tests assert on.

`noStrict` tests rely on sloppy-only behaviors that GocciaScript doesn't
provide and fail naturally. They are documented in
`scripts/test262_compatibility_roadmap.json` as
`excluded-by-language-design` and counted as expected failures, not as
wrapper-infra failures.

## Path normalization

Test IDs are stored as POSIX-style relative paths under `suite/test/`.
On Windows the filesystem returns backslashes; the orchestrator
normalizes to forward slashes via `normalizeTestId(id)` at every site
that uses an ID (glob-match, reporting, baseline lookup) so the same
test produces the same ID on both platforms.

## Test corpus

Default categories: `built-ins, harness, intl402, language, staging`
(everything except `annexB`).

- `annexB` is legacy/deprecated browser-only behavior we don't intend
  to support.
- `intl402` covers Intl APIs Goccia largely lacks; those tests fail
  honestly and surface in the per-category breakdown so the gap is
  visible.
- `staging` is forward-looking proposals — engine-readiness signal.
- `harness` verifies test262's own harness functions work under our
  engine.

There is no eligibility filter. Every discovered test runs. Tests
that depend on missing features fail with a real diagnostic, not an
invisible skip. Per-test subprocess + `--timeout` + `--max-memory`
bound the blast radius of any individual hang or OOM.

## Updating the contract

Changes to `buildTestSource` in `scripts/run_test262_suite.ts` are
verified by the full conformance run itself (no separate regression
suite). After any wrapper-template change:

1. Run locally:

   ```bash
   ./build.pas loaderbare
   bun scripts/run_test262_suite.ts --suite-dir <checkout> \
     --output local-results.json
   ```

2. Confirm `wrapper_infra_failures: 0` in the summary.
3. Diff `local-results.json` against the prior baseline; investigate
   any sign change in pass/fail counts before opening the PR.

## Updating the SHA pin

The test262 SHA is pinned in `.github/workflows/ci.yml` and `pr.yml` so
the cached `main` baseline and a PR run measure the same upstream
corpus. The weekly cron at `.github/workflows/test262-bump.yml` opens
a PR every Monday with the latest tc39/test262 main SHA; the PR's
standard CI run posts the per-category delta vs. the previous main
baseline. Merge once the delta is acceptable.

Manual bump: `bun scripts/test262-bump-pin.ts <40-hex-sha>`.
