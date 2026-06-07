# test262 Harness Contract

How GocciaScript runs the official TC39 test262 conformance suite, the
contract the orchestrator guarantees, and the boundary between
conformance failures, wrapper-infrastructure failures, and runner-level
errors.

## What test262 is, and why we run it

[test262](https://github.com/tc39/test262) is the TC39-maintained
conformance suite for ECMAScript. ~50K tests cover every observable
behavior in the language and built-ins, plus Intl, staging proposals,
and the harness itself. We run it as an indicator metric — not a
release gate — to track which spec corners GocciaScript implements,
where the engine diverges, and how each PR moves those numbers.

For the architectural rationale behind the current
LoaderBare-plus-stock-harness setup, see the
[Decision log entry](decision-log.md) dated 2026-05-04.

## Executive summary

- Test262 runs via `GocciaScriptLoaderBare`, never via `GocciaTestRunner`.
- Wrapper bodies execute inside a neutral engine — `expect`, `describe`,
  `test`, lifecycle hooks, mocks, `runTests` are not registered.
- Stock tc39/test262 harness files are loaded from the pinned test262
  checkout's `harness/` directory. The only bundled harness file is
  `scripts/test262_harness/$262.js`, the host-provided hook object.
- The orchestrator passes `GocciaScriptLoaderBare --test262-host` so those
  host hooks are available only during conformance runs.
- The orchestrator drives via process exit code + stdout markers, the
  same convention `test262-harness`/`eshost`/test262.fyi use.
- Wrapper-infrastructure failures are classified separately from
  conformance failures and gated to zero in CI.

## Architecture

```text
scripts/run_test262_suite.ts
  → discover tests under suite/test/{built-ins,harness,intl402,language,staging}
  → for each test (parallel pool, --jobs=N):
      → read frontmatter, classify by phase (parse / runtime / positive)
      → build source = (stock harness includes) + body, with a tiny
        marker-emitting wrapper for negative-runtime
      → spawn ./build/GocciaScriptLoaderBare --test262-host with stdin = source
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

Async markers are emitted by stock test262 `doneprintHandle.js` via
`$DONE`; the runner only scans stdout for those strings. The
negative-runtime markers
(`Test262:NegativeTestError:...` / `Test262:NegativeTestNoError`) are
the only Goccia-specific marker addition; see "Wrapper templates"
below.

## Wrapper templates

All four template kinds are produced by `buildTestSource` in
`scripts/run_test262_suite.ts`. Bodies for positive sync, positive async,
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
| Per-test wall-clock timeout                            | `setTimeout(() => ac.abort(), wallClockMs)` fired (signalCode SIGTERM/SIGKILL) | timeout            |
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
- `print` (Goccia engine global; stock `doneprintHandle.js` uses it for async markers)
- `$262` helpers implemented by Goccia's bundled host object:
  `detachArrayBuffer`, `evalScript`, `gc`, `global`, and `createRealm`
- `test262Host`, a private marker on the `Goccia` namespace exposed only by
  `GocciaScriptLoaderBare --test262-host` so `$262.js` can reject accidental
  use outside the conformance host.
- Anything declared in test-included harness files (e.g. `compareArray`,
  `propertyHelper`)

Bodies do NOT see:

- `expect`, `describe`, `test`, `it`, `beforeAll`, `beforeEach`,
  `afterEach`, `afterAll`, `onTestFinished`, `runTests`, `mock`,
  `spyOn` — none of these exist on the Bare engine.
- `console`, `fetch`, `URL`, `performance` — Bare doesn't register
  Goccia's runtime extension.
- Optional `$262` hooks outside the bundled implementation. Tests that
  depend on unavailable host hooks fail honestly.

## Bundled harness adaptations

The orchestrator loads stock tc39/test262 harness files from the pinned
checkout's `harness/` directory. `BUNDLED_INCLUDES` contains only
`$262.js`, because `$262` is not a stock harness helper: it is the
host-provided object test262 expects engines to supply.

Bundling rule: do not add compatibility copies of stock harness files.
If a stock helper fails, fix the language/runtime behavior or classify the
test as a genuine conformance failure. `$262.js` may grow only by adding
test262 host hooks or harness-local `$262` behavior.

`$262.evalScript()` and `$262.createRealm()` delegate to flagged test262 host
hooks exposed by the bare loader. `evalScript` parses and executes its source
as script code in the current realm. `createRealm` creates a fresh Goccia
engine/realm, returns a host record with the child realm's `globalThis`, its
own `evalScript`, and `createRealm`, and keeps the child engine alive for the
duration of the test run so cross-realm intrinsics remain valid. The hooks are
only exposed when `GocciaScriptLoaderBare --test262-host` is enabled.

`eval` is also host-gated. `GocciaScriptLoaderBare --test262-host` installs
the official test262 host eval only for conformance runs; default Bare
execution does not expose it. Bytecode direct calls to that host eval preserve
the caller realm and caller lexical bindings, while shadowed or indirect eval
calls use ordinary function-call semantics.

## Strict mode

GocciaScript recognizes strict directive prologues at execution time for
the compatibility behaviors that otherwise depend on Script non-strict
mode. Independently, the engine's curated default semantics enforce most
strict-mode behaviors statically:

- Implicit globals throw `ReferenceError` (sloppy would create a global)
- `delete <identifier>` and non-configurable property deletion throw by default

The orchestrator enables `--compat-non-strict-mode` per test, not globally:
Script tests receive it, while module tests stay strict. `onlyStrict`
Script tests also receive the flag, but the injected directive keeps
`arguments`, `with`, non-strict assignment failures, legacy `delete`
return values, and regular-function nullish `this` coercion on the strict
path. Remaining `noStrict` tests rely on sloppy-only behaviors that
GocciaScript still does not provide and fail naturally as ordinary
conformance failures, not as wrapper-infra failures.

The runner passes syntax compatibility flags such as
`--compat-traditional-for-loop`, `--compat-for-in-loop`,
`--compat-while-loops`, and `--compat-label` unconditionally because
test262 uses those forms across both harness helpers and test bodies; the
test's source type and strictness still decide runtime strict-mode
semantics.

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
- `intl402` covers Intl APIs (ECMA-402); tests exercise
  `Intl.getCanonicalLocales`, constructors, and formatting operations.
- `staging` is forward-looking proposals — engine-readiness signal.
- `harness` verifies test262's own harness functions work under our
  engine.

There is no eligibility filter. Every discovered test runs. Tests
that depend on missing features fail with a real diagnostic, not an
invisible skip. Per-test subprocess + `--timeout` + `--max-memory`
bound the blast radius of any individual hang or OOM.

## Known engine crashes

A small `KNOWN_ENGINE_CRASHES` set in `scripts/run_test262_suite.ts`
skips tests that are known to crash the engine at the native level
(SIGSEGV / SIGBUS) — not catchable by the per-test timeout, not
representative of conformance failures, and would otherwise inflate
`wrapper_infra_failures` indefinitely. Each entry is paired with a
GitHub issue tracking the underlying engine bug; remove the entry
once the bug is fixed.

This list is the only allowed form of test-skipping in the harness.
Do not rebuild a generic eligibility filter (the structural blast-radius
control is per-test subprocess + `--timeout` + `--max-memory`, not
pre-execution exclusion).

Current entries:

- `built-ins/Iterator/concat/throws-typeerror-when-generator-is-running-next.js`
  — [#514](https://github.com/frostney/GocciaScript/issues/514)
- `staging/sm/RegExp/test-trailing.js`
  — [#515](https://github.com/frostney/GocciaScript/issues/515)

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

The test262 SHA is pinned in `.github/workflows/ci.yml` and
`.github/workflows/pr.yml` so the cached `main` baseline and a PR run
measure the same upstream corpus. The weekly cron at `.github/workflows/test262-bump.yml` opens
a PR every Monday with the latest tc39/test262 main SHA; the PR's
standard CI run posts the per-category delta vs. the previous main
baseline. Merge once the delta is acceptable.

Manual bump: `bun scripts/test262-bump-pin.ts <40-hex-sha>`.
