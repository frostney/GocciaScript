<!-- doc-length-limit: 1000 -->
# Testing

*For contributors writing, running, or debugging tests.*

## Executive Summary

- **Three testing layers** — JavaScript end-to-end tests (primary), CLI behavior tests (secondary), Pascal unit tests (tertiary)
- **Valid code → `tests/`; rejected code → `scripts/test-cli-*.ts`** — JavaScript tests assert valid code runs and returns the right result; parser/lexer **rejection** (SyntaxError, caret, error envelope) belongs in `scripts/test-cli-parser.ts` / `test-cli-lexer.ts`, because `eval` is not installed and the `Function` constructor is off by default (`--unsafe-function-constructor`), so a parse error cannot be asserted from inside a JS test
- **Built-in test framework** — `describe`/`test`/`expect` with async support, mock functions, lifecycle hooks, and Vitest-compatible matchers
- **One method per file** — Each test file focuses on a single method; edge cases are co-located with happy-path tests
- **Cover the contract, not only the example** — Include boundaries, invalid inputs and receivers, coercion/order, state transitions, and descriptors when those perspectives apply
- **Structure is checked in CI** — Run `bun run scripts/check-test-structure.ts` before submitting test-suite organization changes
- **Run with**: `./build.pas testrunner` and `./build/GocciaTestRunner -P tests` (see [Testing requirements](../CONTRIBUTING.md#3-testing-requirements) for `-P` and the one-time `--trust` alternative)

GocciaScript uses three testing layers in priority order:

1. **JavaScript end-to-end tests (primary)** -- `.js` tests in `tests/` that exercise the full pipeline through the same public surface that users call. Every new feature or bug fix should include tests at this layer.
2. **CLI behavior tests (CI integration)** -- Standalone bun scripts under `scripts/test-cli-*.ts` (`test-cli.ts`, `test-cli-lexer.ts`, `test-cli-parser.ts`, `test-cli-config.ts`, `test-cli-apps.ts`, `test-cli-permissions.ts`, `test-cli-embedded-resources.ts`, `test-cli-intl-taint.ts`, `test-cli-ffi-callback-slots.ts`, `test-cli-differential.ts`) that the PR and main workflows run via `bun run` in the `cli` job. They invoke `GocciaRunner`, `GocciaTestRunner`, and `GocciaBenchmarkRunner` as subprocesses and assert on exit codes, output structure, and error envelopes — above all **parser/lexer rejection** that a JS test cannot express (malformed source must fail with a `SyntaxError`, caret, suggestion, and JSON `error` envelope), plus JSON output structure, coverage CLI, source maps, numeric separator rejection, timeout handling, global injection, and config loading. The matchers these scripts share live in `scripts/test-cli/assertions.ts` and are themselves unit-tested by `scripts/test-cli-assertions.ts`, which spawns no binaries — a defect in a shared matcher silently weakens every harness that uses it, so its contract is locked by its own test.
3. **Pascal unit tests (tertiary)** -- Native `*.Test.pas` coverage for low-level front-end, data-format and host internals that are not reachable through a stable public API.

When choosing where to add coverage, prefer the most public entry point — and match the **kind** of check to the layer that can actually express it:

| What you are verifying | Where it goes | Why there |
|---|---|---|
| Valid code runs and produces the correct result — semantics, conformance, edge cases | JavaScript tests under `tests/` (run by `GocciaTestRunner`) | Exercises the full lexer → parser → compiler → VM pipeline through the public surface |
| Malformed source is **rejected** — a `SyntaxError`/early error, and its message, caret, suggestion, exit code, and JSON `error` envelope | `scripts/test-cli-parser.ts` (parser) or `scripts/test-cli-lexer.ts` (lexer), run by the CI `cli` job | A JS test **cannot** assert rejection: `eval` is not installed and the `Function` constructor is off by default, so a parse or lex error simply fails to load the whole test file. These scripts run `GocciaRunner` as a subprocess and assert on its exit code and error output |
| CLI tool contract — output formats (`--output=json`, `--format`), `--coverage`, `--source-map`, `--timeout`, global injection, config loading, `--allow-*`/`--deny-*` permissions and `--max-*` limits | the matching `scripts/test-cli-*.ts` (`test-cli.ts`, `test-cli-apps.ts`, `test-cli-config.ts`, `test-cli-permissions.ts`, `test-cli-embedded-resources.ts`), run by the CI `cli` job | Command-level behaviour over the real binaries |
| Real-network fetch behavior requiring a live listener | JavaScript requests in `scripts/fetch-e2e.sh` with `scripts/fetch_test_server.py`, run by the PR and non-Windows main CI `cli` jobs | The ordinary `tests/` runner has no server fixture; this still exercises the public `fetch` API through `GocciaRunner` |
| Low-level front-end, data-format or host internals not reachable from JavaScript | Pascal `*.Test.pas` | Internal-only behaviour |

A JavaScript test that merely *parses and runs* a construct — e.g. `(a / b)` evaluating to `1`, or `(/x/).test(...)` returning `true` — is a positive **execution/conformance** test. It confirms the construct works, but it does **not** prove that a *different*, malformed construct is rejected, and it is not the lexer/parser gate. When a change is about what the parser or lexer accepts or rejects (disambiguation, early errors, token classification), put the rejection cases in `scripts/test-cli-parser.ts` / `test-cli-lexer.ts`.

Do not use dynamic source execution (`Function(...)`, `eval(...)`, or equivalent runtime compilation) inside the JavaScript suite merely to smuggle parser failures into a runtime test. Keep dynamic-code assertions in `tests/` only when the dynamic-code API itself is the feature under test, such as `Function` constructor behavior. Direct `eval` is host-gated, so its private `GocciaTest262Runner` coverage belongs in `scripts/test-cli-apps.ts`, outside the JavaScript suite.

Avoid tests that lock onto private helper functions or transient implementation structure when the same behavior can be validated through a documented user-facing command or script entry point. Where native Pascal tests are still appropriate, keep them as stateless and repeatable as possible so they behave like pure input/output checks rather than process-sensitive probes.

Keep suite titles, test names, and failure messages aligned with the layer under test. End-to-end JavaScript and CLI tests should use JavaScript/runtime terminology and describe behavior at that layer, while lower-level Pascal tests may use implementation terms when that is the behavior being exercised.

## Test Organization

```text
tests/
├── built-ins/              # Built-in object tests
│   ├── Array/              # Array constructor and prototype methods
│   │   ├── constructor.js
│   │   ├── from.js         # Array.from
│   │   ├── of.js           # Array.of
│   │   └── prototype/
│   │       ├── map.js, filter.js, reduce.js, forEach.js
│   │       ├── find.js, findIndex.js, indexOf.js, lastIndexOf.js
│   │       ├── sort.js, splice.js, shift.js, unshift.js
│   │       ├── fill.js, at.js, includes.js, concat.js, reverse.js
│   │       └── ...
│   ├── ArrayBuffer/        # ArrayBuffer constructor, static/prototype methods
│   │   ├── constructor.js, isView.js, toString-tag.js
│   │   └── prototype/
│   │       └── slice.js
│   ├── Error/
│   ├── JSON/
│   ├── Map/
│   ├── Math/
│   ├── Number/             # Number methods and constants
│   │   ├── parseInt.js, parseFloat.js, isNaN.js, isFinite.js, isInteger.js
│   │   ├── isSafeInteger.js, epsilon.js, maxSafeInteger.js
│   │   └── maxValue.js, minValue.js, nan.js
│   ├── Object/             # Object static and prototype methods
│   │   ├── keys.js, values.js, entries.js, assign.js, create.js, is.js
│   │   ├── defineProperty.js, defineProperties.js, getOwnPropertyDescriptor.js
│   │   ├── freeze.js, isFrozen.js
│   │   ├── getPrototypeOf.js, setPrototypeOf.js
│   │   ├── fromEntries.js, groupBy.js
│   │   ├── prototype/      # Object.prototype instance methods
│   │   │   ├── toString.js, hasOwnProperty.js, isPrototypeOf.js
│   │   │   ├── propertyIsEnumerable.js, toLocaleString.js, valueOf.js
│   │   └── ...
│   ├── Promise/             # Promise constructor, static methods, microtask ordering
│   │   ├── constructor.js, resolve.js, reject.js
│   │   ├── all.js, all-settled.js, race.js, any.js
│   │   ├── microtask-ordering.js, thenable-adoption.js
│   │   └── prototype/
│   │       ├── then.js, catch.js, finally.js
│   ├── Set/
│   ├── SharedArrayBuffer/  # SharedArrayBuffer constructor, prototype methods
│   │   ├── constructor.js, toString-tag.js
│   │   └── prototype/
│   │       └── slice.js
│   ├── String/
│   │   └── prototype/
│   ├── structuredClone/    # structuredClone tests (primitives, objects, collections, errors, arraybuffer)
│   ├── Symbol/
│   └── TypedArray/         # TypedArray constructors, prototype methods
│       ├── constructors.js, element-access.js, buffer-sharing.js
│       ├── from.js, of.js  # Static methods at top level
│       └── prototype/      # Instance methods (one file per method, edge cases included)
│           ├── fill.js, map.js, filter.js, reduce.js, reduceRight.js
│           ├── indexOf.js, lastIndexOf.js, includes.js
│           ├── find.js, findIndex.js, findLast.js, findLastIndex.js
│           ├── sort.js, reverse.js, slice.js, subarray.js
│           ├── set.js, copyWithin.js, at.js, with.js
│           ├── every.js, some.js, forEach.js, join.js
│           ├── toReversed.js, toSorted.js
│           └── values.js, keys.js, entries.js
│
└── language/               # Core language feature tests
    ├── classes/            # Class declarations, inheritance, private fields/methods/getters/setters
    ├── declarations/       # let, const
    ├── decorators/         # TC39 Stage 2.7 decorators and decorator metadata
    ├── expressions/        # Arithmetic, comparison, logical, destructuring, trailing commas, etc.
    │   ├── addition/       # Addition with ToPrimitive
    │   ├── arithmetic/     # Division (IEEE-754 signed zeros, Infinity), exponentiation (Infinity edge cases)
    │   ├── bitwise/        # Bitwise OR, AND, XOR, NOT, shifts
    │   ├── modulo/         # Floating-point modulo
    │   ├── conditional/    # Ternary precedence
    │   ├── optional-chaining/  # Optional chaining (?.) edge cases
    │   ├── trailing-commas.js  # Trailing commas in calls, parameters, constructors
    │   └── ...
    ├── functions/          # Arrow functions, closures, higher-order, recursion
    │   └── function-length-name.js  # Function.length and Function.name
    ├── identifiers/        # Unicode support, reserved words
    ├── modules/            # Import/export
    ├── objects/            # Object literals, methods, computed properties
    ├── statements/         # if/else, switch/case/break, try/catch/finally
    │   ├── try-catch/      # Try-catch-finally edge cases
    │   └── unsupported-features/    # Warning-mode recovery tests for disabled compatibility syntax
    └── unary-operators.js
```

Constructor requirements belong with the constructor they constrain. For
example, the `Map()`-without-`new` rejection belongs in
`tests/built-ins/Map/constructor.js`, not in a shared
`tests/built-ins/constructors/require-new.js` file.

### File Naming and Layout Conventions

Follow these rules when creating or organizing test files:

**1. One method per file** — Each built-in method or static function gets its own test file. Never bundle multiple methods into a single file like `prototype-methods.js` or `static-methods.js`.

```text
# Correct — each method is its own file
tests/built-ins/TypedArray/prototype/fill.js
tests/built-ins/TypedArray/prototype/map.js
tests/built-ins/TypedArray/prototype/indexOf.js

# Wrong — multiple methods in one file
tests/built-ins/TypedArray/prototype-methods.js
```

**2. Prototype methods go in a `prototype/` subfolder** — Instance methods (e.g., `Array.prototype.map`) live in `BuiltIn/prototype/methodName.js`. Static methods and constructor tests live directly in the `BuiltIn/` folder (no separate `static/` subfolder).

```text
tests/built-ins/TypedArray/
├── constructors.js              # new TypedArray(...) constructor variants
├── buffer-sharing.js            # Buffer sharing behavior across views
├── from.js, of.js               # Static methods at top level
└── prototype/                   # Instance methods
    ├── at.js
    ├── fill.js
    ├── indexOf.js
    ├── map.js
    └── ...
```

**3. Edge cases are co-located, not separate** — Edge cases (NaN handling, Infinity, negative indices, empty arrays, clamping, etc.) belong **in the same file** as the happy-path tests for that method. Do **not** create standalone `edge-cases.js` files — they become disconnected from the feature they test and make it unclear which method the edge case validates.

```javascript
// tests/built-ins/TypedArray/prototype/fill.js — CORRECT
describe("TypedArray.prototype.fill", () => {
  test("fills entire array", () => { ... });
  test("fills with start and end", () => { ... });

  // Edge cases are part of the same file
  test("negative start index counts from end", () => { ... });
  test("NaN fill value becomes 0 for integer types", () => { ... });
  test("Infinity clamps to 255 for Uint8ClampedArray", () => { ... });
});
```

```text
# Wrong — edge cases in a separate catch-all file
tests/built-ins/TypedArray/edge-cases.js
```

**4. Object prototype methods use `prototype/` too** — `Object.prototype.toString`, `Object.prototype.hasOwnProperty`, etc. follow the same convention as Array. Use `Object/prototype/toString.js`, not `Object/prototype-toString.js`.

```text
tests/built-ins/Object/
├── keys.js                      # Object.keys (static method)
├── assign.js                    # Object.assign (static method)
├── prototype/                   # Instance methods on Object.prototype
│   ├── toString.js
│   ├── hasOwnProperty.js
│   ├── isPrototypeOf.js
│   ├── propertyIsEnumerable.js
│   ├── toLocaleString.js
│   └── valueOf.js
```

**5. Test titles should match the test layer** — `describe(...)` and `test(...)` strings should use the vocabulary of the layer they exercise. End-to-end JavaScript tests should avoid leaking Pascal implementation terms; lower-level Pascal tests do not need that restriction.

```javascript
// Correct — end-to-end JavaScript test uses JavaScript/runtime terminology
test("JSON.stringify preserves round-trip precision for large fractional floating-point numbers", () => {
  // ...
});

// Wrong — end-to-end JavaScript test leaks Pascal implementation terms
test("JSON.stringify preserves round-trip precision for large fractional doubles", () => {
  // ...
});
```

**6. Cross-surface files are narrow exceptions** — A file may span several
methods only when the behavior itself is one indivisible cross-surface
contract, not merely because the methods are related or share an
implementation. Examples include a proposal-version absence contract such as
`Temporal/removed-methods.js`, or a constructor-family invariant such as
`Error/error-prototype-constructor.js`. A shared receiver-branding matrix such
as `Date/prototype/receiver-branding.js` is also appropriate when it verifies
the same internal-slot requirement across methods without duplicating that
matrix in every method file.

These exceptions must be:

- named after the single contract rather than `methods.js`, `misc.js`, or
  `error-cases.js`;
- explicitly allowlisted in `scripts/check-test-structure.ts` with a concrete
  reason;
- kept small enough that a failure still identifies the contract that broke.

`tests/built-ins/prelude-intrinsics` is one such contract by directory: a file per prelude (or per group of small
ones) checks that its built-ins look up nothing a script can replace, across every method and getter it defines,
with `helpers/tripwire.js`. `scripts/check-test-structure.ts` requires a file for each prelude rather than an
allowlist entry for each file ([Captured intrinsics](adding-built-in-types.md#captured-intrinsics)).

Shared setup or table data is not by itself a reason to combine operations.
Prefer a helper imported by focused files when several operations need the same
fixtures.

### Coverage Perspectives

A passing happy-path example proves only that one route works. For every method
or operation, inspect its public contract and implementation branches, then add
the perspectives that can change its observable result:

- ordinary behavior, including more than one representative input where the
  operation has distinct modes;
- boundaries and sentinels such as empty input, zero and signed zero, `NaN`,
  infinities, minimum/maximum values, and first/last valid indexes;
- invalid arguments and incompatible receivers, including the exact error type;
- argument coercion, throwing coercions, and observable evaluation order;
- repeated calls and state transitions for mutable or stateful APIs;
- property metadata and descriptors for exposed built-in properties;
- independent expected values for encoders/decoders and getters/setters rather
  than round trips that can let two matching defects cancel out.

Not every bullet applies to every operation. Each added test must protect an
observable contract or a meaningful implementation branch; do not add
combinatorial duplicates merely to satisfy a checklist.

### Structural Check

Run the test-suite structure check after adding or reorganizing JavaScript
tests:

```bash
bun run scripts/check-test-structure.ts
```

The check rejects catch-all filenames, known multi-operation files, reliably
detectable prototype-method suites outside `prototype/`, and parser-rejection
assertions hidden behind `Function(...)` or `eval(...)`. Dynamic-code rejection
tests are allowed only where the dynamic-code API itself is under test.

### Cross-Platform Newline Rules for Data Format Parsers

When a parser implements a format with spec-defined newline semantics, test the format semantics directly instead of inheriting the host OS newline convention.

- Do not treat `LineEnding` / `sLineBreak` as the expected runtime result for parsed data just because the test is running on Windows.
- Prefer explicit `\r\n` or `#13#10` fixtures when adding regression tests for multiline parsing, folding, or block-scalar behavior.
- Assert the format-defined canonical result. Example: TOML multiline strings normalize recognized newlines to LF (`\n`) even when the source text uses CRLF.
- For parser inputs that come from files, use `Goccia.TextFiles.ReadUTF8FileText` for file I/O and `TextSemantics` helpers (`CreateFileTextLines`, `StringListToLFText`, newline normalization) for parser-facing text. Avoid `TStringList.LoadFromFile` and `string(UTF8String(...))`. Keep file text as `UTF8String` until parser entry, canonicalize parser-facing source text to LF through the shared text semantics, and add at least one regression that hits the real file-loading path with non-ASCII data.
- When code needs a "10 characters" or "first identifier code point" style rule on UTF-8 text, do not use raw `Length`, `Copy`, or byte indexing on `string`/`UTF8String`; those operate on bytes under our FPC settings.
- Keep one public-surface regression in `tests/` and, when the parser exposes a reusable native utility like `TGocciaTOMLParser`, add a focused Pascal regression alongside it as well.

### Test Metadata (Optional)

Test files can include JSDoc-style metadata inspired by test262:

```javascript
/*---
description: Tests for basic addition operations
features: [addition, arithmetic]
---*/
```

## Running Tests

### Build the GocciaTestRunner

```bash
./build.pas testrunner
```

This also builds the native FFI fixture library from `fixtures/ffi/fixture.c`,
so the full JavaScript suite can run the folder-configured FFI tests locally.

### Run All Tests

```bash
./build/GocciaTestRunner -P tests
```

The whole suite must pass. Test subtrees that require opt-in parser or runtime behavior declare it with a local `goccia.json` (for example, `tests/language/asi/goccia.json` enables ASI). Subtrees that reach outside their own directory declare the grant in a `permissions` block, with scopes relative to that config file: `tests/built-ins/FFI` (`allow-ffi` for the repo's `fixtures/ffi`), `tests/built-ins/fetch` (`allow-net` for the loopback test server and `example.com`), `tests/built-ins/ShadowRealm`, `tests/language/modules/**`, `tests/language/source-type`, and `tests/language/source-type-detection/**` (`allow-read` for shared fixtures and helpers), and `tests/language/modules/node-modules` (also `allow-import` for its committed `node_modules`). Declaring the grant beside the tests that need it, rather than passing `--allow-*` to the whole run, keeps every other folder under the default profile; see [Permissions](permissions.md#config-files).

Those permission blocks, and the `unsafe-*` keys some folders set, only take effect once the configs are accepted; without that, the runner refuses the whole run and lists each untrusted config. [Testing requirements](../CONTRIBUTING.md#3-testing-requirements) covers `-P` and `--trust`.

### Run a Specific Test File

```bash
./build/GocciaTestRunner tests/language/expressions/addition/basic-addition.js
```

### Run a Test Category

```bash
./build/GocciaTestRunner tests/built-ins/Math/
```

### GocciaTestRunner Options

| Flag | Description |
|------|-------------|
| `--no-progress` | Suppress per-file progress output |
| `--no-results` | Suppress test results summary |
| `--exit-on-first-failure` | Stop on the first test failure **or suite error** (a throwing `describe`, a failed `beforeAll`/`afterAll`). Under `--jobs` the worker queue is cancelled too; files that never ran are omitted from the report rather than counted as failures. It stops the queue, not the run — see [Stopping behaviour](#--exit-on-first-failure-stopping-behaviour) |
| `--silent` | Suppress all console output from test scripts |
| `--heap-snapshots` | Check [heap snapshot round trips](heap-snapshots.md#round-tripping-the-javascript-suite) for objects observed by `expect()` |
| `--jobs=N` / `-j N` | Number of parallel worker threads (default: CPU count) |
| `--update-snapshots` / `-u` | Create, update, and prune snapshots |
| `--update` | Vitest-compatible alias for `--update-snapshots` |
| `--test-timeout=N` | Per-test timeout in ms (0 disables); the test is marked `TIMEOUT` and the run continues |
| `--describe-timeout=N` | Per-`describe` timeout in ms (0 disables). It times only the `describe` callback, not the tests it registers; when it fires, the rest of the file does not run ([#1278](https://github.com/frostney/GocciaScript/issues/1278)) |
| `--output=json\|compact-json\|<file>` | JSON envelope to stdout (`compact-json` omits `build`, `memory`, `stdout`, `stderr`), or the full envelope to a file |
| `--no-vitest-compat` | Leave the bare `vitest` specifier unresolved; see [the `vitest` shim](testing-api.md#the-vitest-compatibility-shim) |

Run `./build/GocciaTestRunner --help` for the full list, including engine, permission, limit, coverage, and config-trust options.

#### `--exit-on-first-failure` stopping behaviour

The flag stops the **file queue**, not the run in progress:

- **Sequential (`--jobs=1`)** — execution stops at the failing file; no file
  after it runs.
- **Parallel (`--jobs=N`, `N > 1`)** — the queue is cancelled, but files a
  worker already picked up run to completion, and a worker reaching for its
  next file before the cancellation is visible starts that one too. Files after
  the failure can therefore still execute: expect roughly the failing file's
  position plus about one file per worker. The exact count depends on
  scheduling and is **not** reproducible between runs — do not assert on it.

Files the queue never reached are omitted from the report rather than counted
as failures, so reported totals shrink as workers are added. For a
deterministic "everything up to the failure and nothing after" run — bisecting,
or a reproducible failure list — use `--jobs=1`.

```bash
# CI-friendly: no progress, stop on first failure
./build/GocciaTestRunner tests --no-progress --exit-on-first-failure

# Deterministic stop: nothing after the failing file runs
./build/GocciaTestRunner tests --no-progress --exit-on-first-failure --jobs=1

# Silent mode: only show results, suppress script console output
./build/GocciaTestRunner tests --silent

# Run tests with 4 parallel workers; --jobs=1 forces sequential execution
./build/GocciaTestRunner tests --jobs=4
```

#### A file that crashes fails on its own

A Pascal exception that escapes a file's run, an access violation included, is
caught on the thread that ran the file: the file is recorded as one failed test
with the exception's message, its engine is freed, and the run goes on with the
other files. Each file runs under its own `--timeout` (30 seconds by default;
`--timeout=0` disables it), which the VM checks at every backward jump and call
and built-in loops poll, so a file stuck in a loop ends at its deadline as a
failed file instead of holding up the run.

### Snapshot Testing

`toMatchSnapshot()` writes external snapshots to
`__snapshots__/<test-file>.snap`. `toMatchInlineSnapshot()` inserts or updates
the snapshot in the test source. Both use Vitest-compatible formatting. The runner refuses a `.snap` file
or a `__snapshots__` directory that is a symbolic link rather than read, write,
or delete through it, so a link committed to a repository cannot redirect a
snapshot update outside the project; a snapshot file is replaced by writing
beside it and renaming.

```javascript
test("user", () => {
  const user = { id: 42, name: "Ada" };

  expect(user).toMatchSnapshot("public user");
  expect(user).toMatchInlineSnapshot({
    id: expect.any(Number),
  });
});
```

The first object argument is a snapshot property shape. Matching happens before
any write, and the stored value replaces shaped fields with their asymmetric
matcher descriptions. Snapshot shapes support `expect.anything`, `expect.any`,
`expect.closeTo`, `expect.arrayContaining`, `expect.objectContaining`,
`expect.stringContaining`, `expect.stringMatching`, and
`expect.schemaMatching`, plus `expect.toBeOneOf`, `expect.toSatisfy`,
`expect.toBeFasterThan`, and `expect.toBeSlowerThan`, including their supported
`expect.not` forms. Snapshot
assertions themselves do not support `.not`.

Local runs create missing snapshots. Existing mismatches fail until the runner
is invoked with `-u`, `--update`, or `--update-snapshots`; update mode also
prunes obsolete entries. Unless an explicit update flag is supplied,
environments detected as CI by Vitest's `std-env` provider conventions—including
GitHub Actions, GitLab, TeamCity, Buildkite, CircleCI, `CI`, and
`CONTINUOUS_INTEGRATION`—do not write snapshots and fail on missing, mismatched,
or obsolete entries. External snapshots are unavailable for stdin input.
Existing inline snapshots can be compared from stdin, but they cannot be
created or updated there.

Register a Vitest/pretty-format-compatible serializer when a test value needs a
domain-specific representation. The most recently registered serializer runs
first and must provide `test` plus either `serialize` or legacy `print`:

```javascript
expect.addSnapshotSerializer({
  test(value) {
    return value?.kind === "point";
  },
  serialize(value) {
    return `Point(${value.x}, ${value.y})`;
  },
});
```

Native embedders can replace snapshot persistence and source editing through
`IGocciaSnapshotHost`. See
[ADR 0096](adr/0096-host-injected-snapshot-testing.md) for the boundary.

**Official YAML Suite**

For YAML parse-validity checks against the official `yaml-test-suite` (pinned to a specific SHA in the script), run:

```bash
python3 scripts/run_yaml_test_suite.py
python3 scripts/run_yaml_test_suite.py --output=tmp/yaml-suite-results.json
python3 scripts/run_yaml_test_suite.py --suite-dir=/path/to/yaml-test-suite
```

This check is intentionally parse-only: it compares whether each suite case should parse or fail, not whether Goccia's runtime values exactly match the suite's JSON fixtures. That distinction matters because Goccia intentionally canonicalizes complex YAML mapping keys into strings.

**Official TOML Suite**

For TOML 1.1.0 checks against a prepared checkout of the official `toml-test` corpus, run:

```bash
./build.pas tomlcompliancerunner
./build/GocciaTOMLComplianceRunner --suite-dir=/path/to/toml-test --output=tmp/toml-suite-results.json
```

The runner verifies the checkout against `tests/compliance/toml-test.pin` without fetching or cloning. It launches its private worker mode once per case, applies a bounded `--jobs` limit (defaulting to the online processor count through the same detector as TestRunner), and classifies mismatches, false accepts, false rejects, timeouts, crashes, and infrastructure failures. Valid cases are compared with the official tagged JSON fixtures through `TGocciaTOMLParser.ParseDocument(...)`, preserving scalar distinctions such as `integer`, `float`, `datetime`, `datetime-local`, `date-local`, and `time-local`.

The runner prints a human summary, optionally writes the shared compliance JSON envelope with `--output`, and exits non-zero for compliance or infrastructure failures.

The harness and any file-backed parser regression must read source text as
UTF-8 bytes first and decode it strictly before handing UTF-16 text to the
parser. Do not place encoded bytes in a Pascal string or rely on compiler code
pages. The shared `TextEncoding` and `FileUtils` boundaries keep behavior
identical on Windows and non-Windows hosts and prevent mojibake such as
`José -> JosÃ©`.

**Official JSON5 Suite**

The upstream JSON5 parser cases are committed as an ordinary generated JavaScript test suite. Run parser and stringify compliance directly through TestRunner:

```bash
./build.pas testrunner
./build/GocciaTestRunner tests/built-ins/JSON5/upstream-parse.js tests/built-ins/JSON5/stringify.js --output=tmp/json5-suite-results.json
```

The generated suite records the revision in `tests/compliance/json5.pin`. TestRunner owns concurrency, per-file timeouts, aggregation, exit status, and the JSON report. Invalid JSON5 remains input to `JSON5.parse(...)` inside valid outer JavaScript. Normal compliance execution requires neither an upstream checkout, Python, nor Node.js.

Maintainers regenerate the JavaScript suite from an already-prepared checkout:

```bash
node scripts/regenerate-json5-tests.js /path/to/json5 <pinned-sha> tests/built-ins/JSON5/upstream-parse.js
```

Regeneration uses Node.js to execute the upstream reference tests, verifies and records the checkout commit, and is intentionally separate from normal compliance runs.

### Pinned es-toolkit library probes

The bounded es-toolkit lane runs named public-package probes and emits classified JSON without making a whole-suite percentage claim.
See [es-toolkit Validation](es-toolkit-validation.md) for the pinned artifact,
command, failure taxonomy, and sandbox/host-surface boundaries.

### Run Pascal Unit Tests

```bash
./build.pas tests
for t in build/Goccia.*.Test; do "$t"; done
```

## Writing Tests

See [Test Framework API](testing-api.md) for the complete test authoring reference including assertions, mocks, lifecycle hooks, async patterns, and cross-runtime compatibility.

## Test Principles

1. **JavaScript tests are the source of truth** — The JavaScript test suite is the primary mechanism for verifying correctness and ECMAScript compatibility. If a behavior isn't covered by a JavaScript test, it isn't guaranteed.

2. **Isolation** — Each test file is independent. No shared state between files. The GocciaTestRunner runs each file on a thread of its own in a fresh `TGocciaEngine` (`Goccia.Embed`); an engine's heap, realms and intrinsics belong to its thread and are freed with the engine when the file ends, so userland mutations in one file — including non-configurable property additions to `Array.prototype` or `Object.prototype` — cannot leak into the next file. When multiple explicit input paths are passed, the runner also keeps folder configs per-file; use `--config` for a deliberately shared root config. Native state that is process-wide is the exception: parallel workers are threads of one process, so they share the 64 FFI callback slots. FFI tests close every persistent callback in a `finally` block, and a check that needs the whole slot table runs in a process of its own (`scripts/test-cli-ffi-callback-slots.ts`).

3. **Grouped by feature** — Tests are organized by the feature they validate, mirroring the structure of the language specification. Prototype methods live in `prototype/` subfolders; static methods live in the built-in's root folder.

4. **One method per file** — Each test file focuses on a single method, operation, or language feature. `fill.js` tests `TypedArray.prototype.fill`; `map.js` tests `Array.prototype.map`. Never bundle multiple methods into a single file.

5. **Edge cases are co-located** — Edge cases are co-located with happy-path tests (see [File Naming and Layout Conventions](#file-naming-and-layout-conventions)).

6. **One scenario per test** — Keep each `test(...)` focused on a single behavior branch or input class. If `undefined`, `null`, and missing input go through different validation paths, they should be three tests, not one combined test. Combining many assertions into one block makes failures harder to localize and silently reduces test-count granularity during refactors.

7. **Readable as specification** — Test names should describe the expected behavior. A passing test suite serves as living documentation of what GocciaScript supports.

8. **Always pass an error constructor to `.toThrow()`** — When testing that code throws, pass the expected error constructor (`TypeError`, `RangeError`, `SyntaxError`, `Error`, etc.) to `.toThrow()` rather than calling it bare. This ensures the test verifies the correct error type, not just that *something* throws. For async code, prefer `await expect(promise).rejects.toThrow(TypeError)` over manually checking `err.name`.

```javascript
// Preferred — verifies the error type
expect(() => null.foo).toThrow(TypeError);
expect(() => new Array(-1)).toThrow(RangeError);
await expect(Promise.reject(new TypeError("boom"))).rejects.toThrow(TypeError);

// Acceptable for cases where the error type is implementation-defined
expect(() => riskyOp()).toThrow();

// Avoid — doesn't verify the error type
expect(() => null.foo).toThrow();  // passes even if the wrong error is thrown

// Avoid — bypasses the matcher and only inspects a stringly-typed property
promise.catch((err) => {
  expect(err.name).toBe("TypeError");
});
```

## Testing Strategy Design Decision

JavaScript end-to-end tests are the **primary** testing mechanism. Every new feature or bug fix must include tests that validate the behavior through the full pipeline (lexer → parser → compiler → VM execution) using the most public surface available.

- **Specification by example** -- Each test file is a runnable specification of expected behavior.
- **End-to-end validation** -- Tests exercise the full pipeline, catching integration issues that unit tests would miss.
- **Readable specifications** -- JavaScript test files are readable by anyone familiar with Jest/Vitest conventions.
- **Source of truth** -- If a behavior isn't covered by a JavaScript test, it isn't guaranteed.

CLI behavior tests — the bun scripts under `scripts/test-cli-*.ts`, run by the workflows' `cli` job — form the second layer, verifying that the command-line tools produce correct output structure, **reject malformed source with the right error**, and that options like `--coverage`, `--output=json`, `--source-map`, and `--timeout` work end-to-end. These tests run in CI on every pull request.

Pascal unit tests (`*.Test.pas`) exist as a tertiary layer for behavior that cannot be reached through script code or other documented user-facing entry points. Even there, prefer stateless, repeatable input/output checks over tests that are tightly coupled to incidental implementation structure.

## How the GocciaTestRunner Works

The `GocciaTestRunner` program:

1. Scans the provided path for `.js`, `.jsx`, `.ts`, `.tsx`, `.mjs`, and `.mts` files. A directory named `node_modules` is never descended into: a committed `node_modules` tree is a module-resolution fixture the suites beside it import (see [Module Resolution](module-resolution.md)), not a suite of its own.
2. For each file, on a thread of its own (at most `--jobs` at once), creates a fresh `TGocciaEngine` (`Goccia.Embed`) with the capability set its options and `goccia.json` resolve to, applies source type from CLI/config or `.mjs`/`.mts` inference, prepares the host (`TGocciaEngine.PrepareHost`: console, timers, web APIs and the `goccia:*` modules, and the `FFI` global when that set grants `ffi` through `--allow-ffi` or the `permissions` block of the file's `goccia.json`), injects `--global`/`--globals`, and installs the testing API (`InstallTestingAPI` in `Goccia.HostAPI`).
3. Loads and runs the source (`TGocciaEngine.Run`). The testing prelude calls each test function through the host, so `--max-stack` counts from the test function's frame. After the suites, the runner waits for the work the tests left pending, such as a timer an `afterAll` set, and a rejection that work leaves unhandled fails the file.
4. Executes the script — `describe`/`test` blocks register themselves during execution. Nested `describe` blocks are supported; suite names are composed with ` > ` separators (e.g., `"Outer > Inner"`). Skip state is inherited by nested describes.
5. `RunTests` executes all registered tests, reconciles snapshots through the
   installed host, and collects results.
6. Frees the file's engine (`TGocciaEngine.Destroy`), and its heap with it, on the thread that ran the file. Every file starts on a fresh heap; there is no collection between files.
7. Aggregates pass/fail/skip counts across all files.
8. Prints a summary with total statistics.

## CLI Behavior Tests (CI Integration)

The CLI behaviour tests are standalone bun scripts under `scripts/test-cli-*.ts` that the PR and main workflows run (via `bun run`) in the `cli` job on every pull request. They assert on command output, exit codes, and error envelopes rather than internal state, catching regressions in the user-facing interface. `test-cli-parser.ts` and `test-cli-lexer.ts` specifically own the parser/lexer **rejection** paths (malformed source must fail with the correct `SyntaxError` and envelope) that the JavaScript suite cannot express.

**What the CLI tests cover:**

| Area | What is checked |
|------|----------------|
| GocciaTestRunner JSON output | `--output=<file>` writes valid JSON with a `totalFiles` field to a file; `--output=json` emits the same envelope to stdout and suppresses the human-readable summary; `--output=compact-json` emits the same stdout envelope without `build`, `memory`, `stdout`, or `stderr` |
| GocciaTestRunner coverage | `--coverage` prints summary; `--coverage-format=lcov` and `--coverage-format=json` write valid output files; branch coverage includes `BRDA`/`BRF` entries |
| GocciaTestRunner snapshots | External and inline creation, comparison and update; exact Vitest formatting; local/CI obsolete behavior; update aliases; stdin restrictions |
| GocciaRunner JSON output | `--output=json` envelope includes aggregate `ok`, `output`, `stdout`, `stderr`, `build`, `timing`, `memory`, `workers`, and per-input `files[]` entries with `fileName` and `result`; `--output=compact-json` emits the same envelope without `build`, `memory`, `stdout`, or `stderr` (the normalized `output` array and structured `error` are preserved) |
| GocciaBenchmarkRunner JSON output | `--format=json` writes the same JSON envelope as `GocciaRunner` and the test runner; `--format=compact-json` emits the same envelope without `build`, `memory`, `stdout`, or `stderr` at the top level or per-file |
| GocciaRunner error display | Syntax errors show source context, caret, and suggestions |
| GocciaRunner coverage | `--coverage` summary, lcov/json file output, Istanbul's statement, line, branch (`if`, `cond-expr`, `binary-expr`, `switch`, `default-arg`) and function definitions, JSX positions |
| GocciaRunner source maps | `--source-map` writes valid source map JSON; rejects stdin without explicit path |
| Numeric separator rejection | Trailing, leading, consecutive separators and invalid positions produce errors |
| Timeout handling | `--timeout` produces `TimeoutError` in JSON output |
| Permissions and limits | `test-cli-permissions.ts`: the `--allow-*`/`--deny-*` grammar, deny-by-default, config `permissions` blocks, what each binary honors, `--max-*` units, and the removed flags and keys exiting `2` with their replacement |
| Global injection | `--global`, `--globals` file/module injection, collision detection |
| Stdin smoke tests | Piped input executes correctly |
| GocciaBenchmarkRunner output | `--format=json` produces valid JSON with benchmark structure |
| Differential suites | `test-cli-differential.ts` runs each `scripts/differential/` suite under GocciaScript and the external runtimes its classification names, requiring them to agree on failed-test **name** sets and counts; vitest gates the testing-API suites (matchers, lifecycle, mocks) and bun gates the language ones, with bun advisory on matcher and lifecycle suites, skipped outright on mock suites, and `*.goccia.test.js` suites run under GocciaScript alone and must pass outright; a per-file timeout counts as a divergence. See [Differential Testing](differential-testing.md) |

This table is non-exhaustive — the `scripts/test-cli-*.ts` scripts that the `cli` job runs are the source of truth. The intent is to check all CLI options, all parser/lexer error paths, and all output-format correctness. To add a new check, add a case to the matching `scripts/test-cli-*.ts` (parser/lexer rejection → `test-cli-parser.ts` / `test-cli-lexer.ts`; CLI flags/output → `test-cli.ts` / `test-cli-apps.ts`); the `cli` job already invokes these scripts, so no workflow change is needed.

## Pascal Unit Tests (Tertiary)

Pascal unit tests are the tertiary testing layer for low-level internals that are difficult to validate through JavaScript alone (e.g., data-format parsers, numeric conversion, coverage bookkeeping, and the snapshot store). They live beside the unit they test as `*.Test.pas` under `source/units`, `source/shared` and `source/app`. Some examples:

| Test File | Tests |
|-----------|-------|
| `Goccia.Lexer.Test.pas` | Hashbang handling, Unicode line terminators in comments, identifier and keyword tokenization |
| `Goccia.TOML.Test.pas` | `TGocciaTOMLParser.ParseDocument` scalar and array kinds, CRLF normalization in multiline strings, Unicode escapes and keys |
| `Goccia.NumberConversion.Test.pas` | `ToUint32`, `ToInt32`, `ToUint16`, `ToUint8Clamp` and float32 rounding |
| `Goccia.Coverage.Test.pas` | `TGocciaCoverageTracker.MergeFrom` summing statement, branch and function hits by location, the line count as the most among the statements starting on a line, Istanbul's ordering of sites, and report path keys |
| `Goccia.Builtins.Testing.Snapshots.Test.pas` | Snapshot creation, comparison, update and pruning in each update mode |

These compile as standalone executables and run directly:

```bash
./build.pas tests
./build/Goccia.Lexer.Test
./build/Goccia.TOML.Test
./build/Goccia.Coverage.Test
```

### Pascal Test Framework

Pascal tests use `TestingPascalLibrary.pas` which provides a `TTestSuite` base class and a generic `Expect<T>` fluent assertion API:

```pascal
type
  TMyTests = class(TTestSuite)
    procedure TestSomething;
    procedure SetupTests; override;
  end;

procedure TMyTests.SetupTests;
begin
  Test('should do something', TestSomething);
end;

procedure TMyTests.TestSomething;
begin
  Expect<string>(Name).ToBe('hello');
  Expect<Double>(Total).ToBe(42);
  Expect<Integer>(Length(Params)).ToBe(2);
  Expect<Boolean>(Found).ToBe(True);
end;
```

#### Generic Assertions

The primary assertion API uses a standalone generic `Expect<T>` function that returns a `TExpect<T>` record:

| Usage | Description |
|-------|-------------|
| `Expect<string>(actual).ToBe(expected)` | String equality |
| `Expect<Double>(actual).ToBe(expected)` | Double equality (exact) |
| `Expect<Integer>(actual).ToBe(expected)` | Integer equality |
| `Expect<Boolean>(actual).ToBe(True)` | Assert value is `True` |
| `Expect<Boolean>(actual).ToBe(False)` | Assert value is `False` |

On failure, `ToBe` produces descriptive messages like `Expected "hello" to be "world"` or `Expected 3.14 to be 2.71`. Strings of every kind (`string`, `AnsiString`, `WideString`, `ShortString`) appear in full, and `TestingPascalLibrary.Test.pas` checks these messages. The RTL's `Exception` stores its message as an `AnsiString` in the system code page, so on Windows a character outside that code page shows as `?`.

`TTestSuite` also provides a `Fail(Message)` method to force a test failure with a custom message.

#### FPC 3.2.2 AArch64 Compiler Bug

`Expect<T>` is intentionally a **standalone generic function** (not a method on `TTestSuite`) and `TExpect<T>` is a **record** (not a class). This design works around a known FPC 3.2.2 AArch64 compiler crash: when a class with a generic method is inherited across compilation units, the compiler raises an internal `EAccessViolation`. The standalone function + record pattern avoids this entirely while preserving the fluent `Expect<T>(...).ToBe(...)` syntax.

### NaN Checks in Pascal Tests

`Expect<Double>(actual).ToBe(expected)` compares exactly, so it cannot assert NaN. Test NaN with `Math.IsNaN`:

```pascal
Expect<Boolean>(Math.IsNaN(Value)).ToBe(True);
```

**When to write Pascal unit tests vs JavaScript tests:**

- **JavaScript tests** (preferred) — For any behavior observable from script code. This is almost everything.
- **Pascal unit tests** — Only for internal implementation details not reachable from script code (e.g., the coverage tracker's merge of per-thread hit counts).

## CI Integration

GitHub Actions CI (`.github/workflows/ci.yml`) runs on push to `main` and tags, with a post-build job fan-out plus release packaging. Its first job, `supersede`, cancels any in-flight `main` run whose commit is an ancestor of the pushed commit, or its own run when a newer `main` commit is already being tested; tag and dispatched runs are never cancelled. See [Build System](build-system.md#ciyml--push-to-main--tags) for the rules.

```text
supersede → build → test             → artifacts (main), release (tags)
                   → toml-compliance  →
                   → json5-compliance →
                   → awfy             →
                   → jetstream        →
                   → web-tooling      →
                   → benchmark        →
                   → cli              →
                   → test262 → test262-merge
docs, test-structure (no build)
```

**`build`** — Installs FPC 3.3.1 (the pinned build `FPC_VERSION`, `3.3.1-b9`) through `frostney/fpc-toolchain`, which ships every target for its runner's host: only the Darwin targets need a macOS runner, and the Linux and Windows targets cross-compile on Linux. It builds all binaries for its target and uploads them as intermediate artifacts. **`test-structure`** runs `scripts/check-test-structure.ts` and the report-publishing helper tests without a build.

**`test`** (needs build, all platforms) — Downloads pre-built binaries, runs all JavaScript tests and Pascal unit tests. Outputs JSON files via `--output=<file>` for CI timing comparison. Before the Pascal unit tests, Linux and macOS add the names only the hosts file lists (`.github/scripts/add-hosts-file-fixtures.sh`), so the resolver test in `HTTPClient.Test.pas` runs; `GOCCIA_HOSTS_FILE_FIXTURE` makes it fail, rather than skip, when its name is missing.

**`toml-compliance`** (all platforms) — Downloads the prebuilt `GocciaTOMLComplianceRunner`, prepares the exact pinned `toml-test` checkout, and relies on the runner exit status. CI performs only lightweight validation of the report envelope before uploading it.

**`json5-compliance`** (all platforms) — Downloads `GocciaTestRunner`, verifies that the committed generated parser suite names the pinned JSON5 revision, and runs it together with the local stringify suite. CI relies on TestRunner's exit status and performs only lightweight validation of its JSON report before upload.

**`test262`** (needs build, ubuntu-latest x64 only, **non-blocking**) — Runs the official conformance suite in four shards, from the shared pin in `scripts/test262-suite-sha.txt`; `test262-merge` merges the shards, uploads the JSON report, and saves a `main` baseline cache for PR deltas. The run step is `continue-on-error: true` so known steady-state conformance failures do not block unrelated work; the downstream PR comment still gates regressions against the cached main baseline. **See [test262.md](test262.md) for the harness contract** and [Build System](build-system.md#ciyml--push-to-main--tags) for the workflow wiring.

**`awfy`** (needs build, ubuntu-latest x64 only) — Runs the pinned AWFY JavaScript report set from `perf/awfy/manifest.json` under Goccia bytecode, QuickJS, `node --jitless`, JavaScriptCore with its JIT off (`jsc --useJIT=false`, from Ubuntu's `libjavascriptcoregtk-bin`), and the latest Node Current release resolved at workflow time with six interleaved warm samples per engine. Uploads the normalized `awfy-report` artifact; on `main`, publishes the compressed report plus daily pointer to the `awfy/` Vercel Blob namespace when `BLOB_READ_WRITE_TOKEN` is configured.

**`jetstream`** / **`web-tooling`** (ubuntu-latest x64 only) — A `*-plan` job reads the workload matrix from the lane's manifest, `*-workload` runs each workload against the built binaries, and the final job merges the reports. See [JetStream 3](benchmarks.md#jetstream-3-reference-lane) and [Web Tooling](benchmarks.md#web-tooling-goccia-lane).

**`benchmark`** (needs build, all platforms) — Downloads pre-built binaries, runs all benchmarks.

**`cli`** (needs build, all platforms) — Downloads pre-built binaries, builds the FFI fixture library, and runs CLI behavior smoke tests via Bun: `test-cli-assertions.ts` (the shared matchers), `test-cli.ts` (options across all apps), `test-cli-lexer.ts` (numeric-separator rejection), `test-cli-parser.ts` (error display), `test-cli-config.ts` (config-file loading and per-file inheritance), `test-cli-permissions.ts` (capability flags, config `permissions` blocks, limits, and removed options), and `test-cli-apps.ts` (app-specific features, including `GocciaScriptLoaderBare` stdin/file checks, CLI-local `print`, and runtime-global absence), `test-cli-embedded-resources.ts`, `test-cli-intl-taint.ts` (Intl robustness under lazy materialization), `test-cli-ffi-callback-slots.ts` (FFI callback slot exhaustion and reuse, in a process of its own), and `test-cli-differential.ts` (differential suites against Vitest and bun). On Linux and macOS, `test-cli-permissions.ts` fetches a name only the hosts file lists and `scripts/fetch-e2e.sh` an IPv6-only one, both added as for `test`. The x86-64 Linux leg also runs the pinned es-toolkit compatibility probes and uploads their classified JSON report. Windows runs additionally assert that `GocciaRunner` does not link against OpenSSL DLLs.

**`artifacts`** (needs test + toml-compliance + json5-compliance + awfy + jetstream + web-tooling + benchmark + cli, `main` only) — Uploads release binaries after all checks pass. `test262` is **not** a gating dependency — failing tests there cannot block a release.

**`nightly`** (`.github/workflows/nightly.yml`) — Runs on a schedule, every three hours. It takes the newest `CI` run that succeeded for a push to `main` and publishes that run's builds as the `nightly` prerelease when its commit is ahead of the one the last nightly was built from, or has diverged from it after a rewritten `main`; when nothing new has passed, or the run's one-day build artifacts have expired, it publishes nothing. "Run workflow" on `main` publishes the newest passing build at once. A missing target artifact fails the job and leaves the existing release in place. It needs the whole run to succeed, so `test-structure` and `test262-merge` gate it as well: failing test262 tests do not fail `test262-merge`, but a missing or timed-out shard does. See [Build System](build-system.md#ciyml--push-to-main--tags).

**`gc-stress`** (`.github/workflows/gc-stress.yml`) — Runs every Sunday and on demand. `scripts/engine/gc-stress-ci.py` runs GocciaScript's tests, the GC regression scripts and the GC-sensitive part of test262 with a 16 KB nursery and `--gc-verify`, so the collector runs every few kilobytes and checks the heap after each collection, and runs the same tests plainly with the same `--prod` binaries. Only tests that pass plainly but fail under stress fail the job. A failing scheduled or `main` run opens or updates one tracking issue; a run on any other branch only prints what it would file. See [Tooling — GC stress](contributing/tooling.md#gc-stress).

**`live-fetch`** (`.github/workflows/live-fetch.yml`) — Runs daily and on demand on macOS. It downloads the nightly macOS arm64 build and runs `scripts/check-live-fetch.js`, which fetches the first 40 pages of the project's sitemap one after another, each with a 15-second deadline; any GET that does not complete fails the job. It is the only check of fetch over SecureTransport against a real HTTPS server, because a macOS runner cannot be made to trust a local fixture's certificate without a person approving it. It is not a pull-request gate, because it depends on the network and on the live site.

**`release`** (same gates, tags only) — Packages and publishes release archives.

The `test`, `awfy`, `jetstream`, `web-tooling`, `benchmark`, `cli`, `toml-compliance`, `json5-compliance`, and `test262` lanes run in parallel after `build`. Every lane reuses the already-built binaries from the matrix build artifacts instead of installing FPC again, so platform-specific issues are caught on the same Linux, macOS, and Windows targets as the main test lanes (test262 runs on Linux only because its outcomes are platform-independent for this engine; AWFY, JetStream and Web Tooling run on Linux x64 only for now, and measuring them on the other shipped platforms, which [ADR 0176](adr/0176-the-interpreter-beats-quickjs-before-gcbundle-a-jit-and-a-compiled-executable.md)'s first target is judged on, is tracked in [#2102](https://github.com/frostney/GocciaScript/issues/2102)).

### PR Workflow (`.github/workflows/pr.yml`)

Runs on every pull request, including stacked PRs whose base is not `main`, on **ubuntu-latest x64 only**:

```text
build → test, boot-timing, benchmark*      → timing comment (benchmark* also → benchmark comment)
      → awfy-report*                        → AWFY Results comment
      → jetstream-workload* → jetstream-report → JetStream comment
      → web-tooling-workload → web-tooling-report → Web Tooling comment
      → test262 → test262-merge             → test262 comment
      → cli
docs, test-structure, website (no build)  * also needs build-main, a build of the PR's base commit
```

`docs` runs the Markdown lint and the `scripts/check-doc-*.ts` / `check-conformance-claims.ts` checks. `website` runs the website's own checks ([Build System](build-system.md#pryml--pull-requests)).

The PR workflow also runs the pinned es-toolkit compatibility probes in the Linux `cli` job and retains the classified JSON report. It posts a **Suite Timing** comment with expandable test-runner and benchmark summaries. Each summary shows timing, top-level GocciaScript GC metrics, and selected FreePascal heap allocation metrics. GC memory rows aggregate the per-file engines' heaps: collections and allocated bytes are summed over the files, in a parallel run live bytes add up across files and the peak is the largest `--jobs` files' peaks together, while in a sequential run the peak is the largest file's and the live bytes are the last file's (`AggregateMemoryStats` in `Goccia.CLI.FileEngine`). Freeing a file's engine and its heap at the end of the file is not counted as a collection. The comment hides negative FreePascal heap free-space deltas because they are valid allocator diagnostics but are noisy in a PR summary. See [benchmarks.md](benchmarks.md#pr-benchmark-comparison) for details on the benchmark comparison format.

The **AWFY Results** comment posts median timings and geomean ratios for the pinned AWFY report set under Goccia bytecode, QuickJS, `node --jitless`, JavaScriptCore with its JIT off, and the latest Node Current release. The underlying `awfy-report` artifact keeps the six raw interleaved samples per engine, each with its warm-up times, measured window and place in the engine order, min/max/CV, environment metadata, and first-class failure outcomes.

The **test262 conformance comment** posts a non-blocking summary using the [`actions/github-script@v7`](https://github.com/actions/github-script) pattern, marker `<!-- test262-results -->`. The markdown is generated by `bun scripts/run_test262_suite.ts --comment <results.json> <baseline.json|->`. It contains:

- A **per-category breakdown table** (run / passed / Δ pass / failed / timeouts / Δ timeout / wrapper-infra / pass-rate / Δ rate) for each top-level category the run covered ([test262.md](test262.md#test-corpus)), plus a totals row and one row per mode (`non-strict runs`, `strict runs`). Every count is a count of runs: an unflagged test file runs once in each mode ([test262.md](test262.md#strict-mode)).
- A **wrapper-infra-failure top-line warning** when non-zero (gates the run as untrustworthy).
- An **Areas closest to 100%** table listing the three test262 directories (keyed by the first two path components, e.g. `built-ins/Math`, `language/expressions`) with the highest pass rate, filtered to areas with **at least 25 attempted runs** and **below 100%**.
- When a `main` baseline is cached, **Δ vs main** columns show absolute count delta and percentage-point delta. Deltas below ±0.05pp render as `±0pp` to keep the table readable.
- A collapsible **Per-test deltas** section listing newly-passing, newly-failing, newly-timed-out, and no-longer-timed-out runs, each named `<test id> (<mode>)`. Runs compare by test ID and mode, so a strict-only PASS → FAIL fails the gate like any other.
- A **grading-contract note** when the cached baseline's `run.gradingContract` differs from the run's. PASS → FAIL transitions are then listed but do not fail the gate, because the runner's grading moved them, not the engine; see [test262.md](test262.md#updating-the-contract).

The website compatibility dashboard at `/compatibility` reads main-branch test262 reports from Vercel Blob at request time with CDN caching. Main CI publishes future dashboard points directly, and `cd website && bun run backfill-test262` can seed historical days from retained artifacts or fresh historical reruns. Website builds do not download artifact ZIPs or bake test262 data into the deployment. This keeps the public pass-rate timeline and group coverage tables tied to generated CI data rather than hand-maintained percentages.

Weekly crons open (or update) a single PR every Monday with the latest upstream SHA so pins don't go stale:

| Suite | Workflow | Schedule | Pin location |
|-------|----------|----------|--------------|
| test262 | `test262-bump.yml` | 06:00 UTC | `scripts/test262-suite-sha.txt` |
| toml-test | `toml-test-bump.yml` | 06:30 UTC | `tests/compliance/toml-test.pin` |
| json5 | `json5-test-bump.yml` | 06:45 UTC | `tests/compliance/json5.pin` and `tests/built-ins/JSON5/upstream-parse.js` |
| yaml-test-suite | `yaml-test-bump.yml` | 07:00 UTC | `scripts/run_yaml_test_suite.py` |

Each workflow reuses a fixed branch (`chore/<suite>-bump`), so an unmerged PR is updated in place rather than replaced. See [test262.md](test262.md) for details on the test262 harness contract.

### Running test262 locally

```bash
./build.pas test262runner
./build/GocciaTest262Runner \
  --suite-dir=/path/to/test262 \
  --filter="built-ins/Array/*" \
  --output=results.json
```

The native runner accepts an existing checkout via `--suite-dir`; CI checks out
the SHA pinned in `scripts/test262-suite-sha.txt`. Run
`./build/GocciaTest262Runner --help` for execution options including
`--categories`, `--max-tests`, `--jobs`,
`--timeout` (a duration such as `20s`), and `--max-memory` (a size such as `512MiB`). The TypeScript utility handles only
cross-shard merge, profile aggregation, and comment rendering.

## Coverage

The GocciaTestRunner and GocciaRunner support JavaScript source-level coverage reporting via the `--coverage` flag. Coverage tracks which statements, lines, branches, and functions of JavaScript source code are executed at runtime, over the entry file and every module it imports.

The definitions are **Istanbul's**, not those of any particular runtime: what counts as a statement, a line, a branch or a function, and how each is located and numbered, follow [istanbul-lib-instrument](https://github.com/istanbuljs/istanbuljs/tree/main/packages/istanbul-lib-instrument) (its code visitor), [istanbul-lib-coverage](https://github.com/istanbuljs/istanbuljs/tree/main/packages/istanbul-lib-coverage) (`FileCoverage`, `getLineCoverage`) and the `lcovonly` reporter of [istanbul-reports](https://github.com/istanbuljs/istanbuljs/tree/main/packages/istanbul-reports). Run on a CommonJS file, `nyc` and GocciaScript produce the same `lcov.info` records and the same `statementMap`, `fnMap`, `branchMap`, `s`, `f` and `b`, apart from the `SF:` path (see [Report Path Keys](#report-path-keys)). Two differences remain: the instrumenter's comment hints (`/* istanbul ignore next */`, `ignore if`, `ignore else`, `ignore file`) are not read, so hinted code is counted like any other ([#2186](https://github.com/frostney/GocciaScript/issues/2186)), and eval code is not covered.

### Usage

```bash
# Console summary (printed after execution)
./build/GocciaTestRunner tests --coverage

# lcov output (for Codecov, Coveralls, or genhtml)
./build/GocciaTestRunner tests --coverage --coverage-format=lcov --coverage-output=coverage.lcov

# JSON output (Istanbul's coverage map)
./build/GocciaTestRunner tests --coverage --coverage-format=json --coverage-output=coverage.json

# GocciaRunner also supports coverage
./build/GocciaRunner example.js --coverage
```

### What is Tracked

Locations are Babel's: line 1-based, column 0-based (in UTF-16 code units), end column exclusive. A counter runs each time execution reaches its site, so a loop statement's own counter runs once per entry and the statements in its body once per iteration. Istanbul instruments the whole file, so a function that is never called still lists its statements, with 0.

**Statements (`s`, `statementMap`):** one counter for each of

- an expression statement and `break`, `continue`, `debugger`, `return`, `throw`, `try`, `if`, `for`, `for...in`, `for...of` (including `for await`), `while`, `do...while`, `switch`, `with` and labeled statements (each label is a statement of its own);
- the initializer of each declarator that has one, located at the initializer: `let`, `const`, `var`, destructuring and `using` declarations, the declarators of a `for(;;)` head, and exported declarations;
- the value of each class property, located at the value and counted each time the field initializer runs (an instance field once per construction, a static field once);
- an arrow function's concise body, which Istanbul turns into `return <expression>`, located at the expression and counted on each call.

Function and class declarations, blocks, export wrappers, directives (`"use strict"`) and empty statements are not statements.

**Lines (lcov `DA:`):** `FileCoverage.getLineCoverage`. Each statement is taken at its start line, and a line's count is the **most** among the statements that start on it. Only those lines are listed: a line no statement starts on (the closing brace of a block, a blank line, the second line of a multi-line statement) has no record. `LF` is the number of listed lines, `LH` the number with a count above 0. So `const f = () => 1;` called five times from a `for...of` on the next line is `DA:1,5` (the declarator's initializer ran once and the arrow's body five times; the line takes the greater count) and `DA:2,5`.

**Functions (`f`, `fnMap`, lcov `FN:`/`FNDA:`):** every function declaration, function expression, arrow function, class method (the constructor, getters and setters included) and object method (getters and setters included); private methods are not listed, as in Istanbul, and neither are static blocks. `decl` is the identifier's location for a function declaration or expression that has one, otherwise the first character of the node; `loc` is the body; the count is the number of calls (a generator's, when its body first runs). Only a function declaration or expression with an identifier of its own has a name: any other function, an arrow bound to a `const` included, is named `(anonymous_<index>)` by its index in the file's function list, in source order.

**Branches (`b`, `branchMap`, lcov `BRDA:`):** each branch has a type, a `loc` and one location and counter per path.

| Type | Paths |
|------|-------|
| `if` | 2: the consequent (located at the whole `if` statement) and the alternate, which Istanbul counts without an `else` too (the empty block it adds has no location; its counter runs when the test is false) |
| `cond-expr` | 2: the consequent and the alternate of `? :` |
| `binary-expr` | one per leaf of a maximal chain of `&&`, `\|\|` and `??`: nested logical expressions flatten whatever their operator, and parentheses make no difference, so `(a && b) \|\| c` has the leaves `a`, `b` and `c`. A leaf counts how many times it was evaluated |
| `switch` | one per case clause, `default` included: the counter is the clause's first statement, so a fall-through into it counts |
| `default-arg` | 1 per default parameter or destructuring default, counted when the default value is evaluated |

**Parallel runs:** `--jobs=N` does not change any of these numbers. Each worker collects into its own tracker and the counts are added together at the end (statements, branches and functions are matched by location, since each worker meets them in its own order), so every count for a given run is identical whatever `--jobs` is set to.

### Output Formats

| Format | Flag | Description |
|--------|------|-------------|
| Console | `--coverage` | Summary table with statement, line, branch, and function totals printed to stdout after execution; with one file, its source with the count of each line a statement starts on (`!!` marks a listed line that never ran) |
| lcov | `--coverage-format=lcov --coverage-output=<file>` | The `lcovonly` tracefile, in its order: `TN`, `SF`, the `FN` lines, `FNF`, `FNH`, the `FNDA` lines, the `DA` lines, `LF`, `LH`, the `BRDA` lines, `BRF`, `BRH`, `end_of_record`. `BRDA:<line>,<branch>,<path>,<count>` carries the line the branch starts on and the branch's 0-based index in the file, and prints a count of 0 rather than a dash |
| JSON | `--coverage-format=json --coverage-output=<file>` | Istanbul's coverage map: per file, `path`, `statementMap`, `fnMap` (`name`, `decl`, `loc`, `line`), `branchMap` (`type`, `loc`, `locations`, `line`), `s`, `f` and `b`, keyed `"0"`, `"1"`, ... in the order Istanbul numbers its sites (source order, an enclosing site before the ones inside it). The alternate of an `if` without `else` has the empty location `{"start":{},"end":{}}`, as in Istanbul |

### Report Path Keys

Every file gets exactly **one** record per run, under one canonical path,
however it was reached. A file can enter a run by two routes at once — named on
the command line, and imported by another file — and each route knows it by a
different spelling: the command line supplies whatever the user typed, import
resolution produces an absolute path. Keying by the incoming spelling used to
split such a file into two records whose hits were never added together.

The canonical form, applied uniformly to the console summary, the lcov `SF:`
records, and the JSON keys and `"path"` fields:

- **repo-relative** when the file is under a repository root — the nearest
  ancestor directory holding a `.git` entry (a directory in a normal clone, a
  file in a linked worktree or submodule);
- **absolute** when the file is outside any repository;
- **`/` as the separator, on every platform**, including Windows.

Repo-relative is what the consumers want: Codecov matches report paths against
repository paths, so a build machine's absolute paths need a `fixes` mapping
while repo-relative paths match directly, and genhtml resolves relative paths
against its working directory. Forward slashes matter because imports resolve
through FPC's `ExpandFileName` — on Windows the raw paths carry backslashes,
which genhtml and Codecov commonly mishandle in an `SF:` record.

Canonicalization is textual (`ExpandFileName` plus the repo-root trim), so it
does not resolve symlinks: two spellings that differ by a symlinked ancestor —
macOS's `/var` → `/private/var`, say — still produce separate records. Reaching
the same file by two genuinely different real paths is not something normal
entry-plus-import runs do.

Paths with no on-disk backing are identities, not files — `<stdin>`, and
multifile section names such as `<stdin>[part1]` — and are left verbatim. Report
keys are never used to read source: the tracker remembers the native on-disk
path behind each key, so reports still render source when the process runs from
outside the repository.

### JSX Source Map Integration

When coverage is used with `.jsx`/`.tsx` files, the JSX transformer produces a source map that maps transformed (post-JSX) coordinates back to the original source. The report writer can translate the location of every statement, branch path and function through such a map (`TGocciaSourceMap.Translate`), but no host registers it with the coverage tracker, so lcov and JSON reports give the positions the parser saw in the transformed source (see [Architecture](#architecture)). The CLI suite checks that a `.jsx` file's branches and functions are reported on their original lines.

### Architecture

Coverage is recorded by the interpreter, which counts the sites the compiler marks. With `TGocciaEngine.Coverage` set, `Goccia.Compiler` registers each Istanbul site of a covered source with the thread's `TGocciaCoverageTracker` (`Goccia.Coverage.pas`) and emits a counter instruction at it: `opCovStmt` at a statement (its `C` operand is the statement's index in the file's list), `opCovBranch` at the start of a branch path (`A` the path, `C` the branch) and `opCovFn` at the start of a function's body (`C` the function). Branches are registered with their type, location and the location of every path before their paths are emitted. The instructions do nothing in normal dispatch; with coverage on, `Goccia.VM`'s `Execute` runs every instruction through the coverage hook (`CovStep`), which counts these three. A coverage run takes neither `FastLoop` nor compiled code, and compiles every function eagerly (a lazily compiled function would not list its statements until it was called); without `--coverage` nothing is registered or recorded. No separate build is needed. Engine preludes (`<goccia:...>`) and eval code are not covered.

Locations come from the AST's source spans, converted to Babel's. The parser gives an operator expression (a binary or conditional expression, a call, a member access) the span of its operator or last token, so the compiler takes an expression's location as the extent of the node and its operands, parentheses included, as Babel's node covers them.

Data is collected by `TGocciaCoverageTracker`, a per-thread tracker holding, per file, the statement list (location and count), the branches (type, location, path locations and counts) and the functions, each with an index by location so that compiling a site again (the code of a `finally` block, which runs through two paths) finds the same counter. Each file's thread initializes its own instance and, when the file ends, merges it into the run's tracker via `TGocciaCoverageTracker.MergeFrom`, which adds the counts site by site, matched by location, because the indexes differ between threads. The lines, the totals and Istanbul's numbering of the maps are derived when a report is written (`LineCoverage`, `StatementOrder`, `BranchOrder`, `FunctionOrder`). Output formatting is in `Goccia.Coverage.Report.pas`, which can translate positions through a source map registered with `TGocciaCoverageTracker.RegisterSourceMap`. The hosts register no map: positions come from the source the parser saw, which for `.jsx`/`.tsx` is the JSX transformer's output.
