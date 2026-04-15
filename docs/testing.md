# Testing

*For contributors writing, running, or debugging tests.*

## Executive Summary

- **Three testing layers** — JavaScript end-to-end tests (primary), CLI behavior tests (secondary), Pascal unit tests (tertiary)
- **Built-in test framework** — `describe`/`test`/`expect` with async support, mock functions, lifecycle hooks, and Vitest-compatible matchers
- **One method per file** — Each test file focuses on a single method; edge cases are co-located with happy-path tests
- **Run with**: `./build.pas testrunner && ./build/TestRunner tests --asi`

GocciaScript uses three testing layers in priority order:

1. **JavaScript end-to-end tests (primary)** -- `.js` tests in `tests/` that exercise the full pipeline through the same public surface that users call. CI runs the full suite in both **interpreted** and **bytecode** mode. Every new feature or bug fix should include tests at this layer.
2. **CLI behavior tests (CI integration)** -- Shell-level checks in `.github/workflows/pr.yml` that verify `ScriptLoader`, `TestRunner`, and `BenchmarkRunner` CLI behavior: JSON output structure, coverage CLI (console summary, lcov, JSON, branch coverage), error display (source context, caret, suggestions), numeric separator rejection, source map generation, stdin smoke tests, timeout handling, global injection, and benchmark output format.
3. **Pascal unit tests (tertiary)** -- Native `*.Test.pas` coverage for low-level runtime and value system internals that are not reachable through a stable public API.

When choosing where to add coverage, prefer the most public entry point available:

- JavaScript feature behavior: add or extend tests under `tests/`
- CLI behavior (`ScriptLoader`, `TestRunner`, `BenchmarkRunner`): add command-level smoke tests in `pr.yml` that assert on visible output
- Pascal unit tests: add native `*.Test.pas` coverage only when the behavior is genuinely internal or not reachable through a documented user-facing entry point

Avoid tests that lock onto private helper functions or transient implementation structure when the same behavior can be validated through a documented user-facing command or script entry point. Where native Pascal tests are still appropriate, keep them as stateless and repeatable as possible so they behave like pure input/output checks rather than process-sensitive probes.

Keep suite titles, test names, and failure messages aligned with the layer under test. End-to-end JavaScript and CLI tests should use JavaScript/runtime terminology and describe behavior at that layer, while lower-level Pascal tests may use implementation terms when that is the behavior being exercised.

## Test Organization

```text
tests/
├── built-ins/              # Built-in object tests
│   ├── Array/              # Array constructor and prototype methods
│   │   ├── array-creation.js
│   │   ├── array-modification.js
│   │   ├── from.js         # Array.from
│   │   ├── of.js           # Array.of
│   │   └── prototype/
│   │       ├── map.js, filter.js, reduce.js, forEach.js
│   │       ├── find.js, findIndex.js, indexOf.js, lastIndexOf.js
│   │       ├── sort.js, splice.js, shift-unshift.js, fill.js, at.js
│   │       ├── includes.js, concat.js, reverse.js
│   │       └── ...
│   ├── ArrayBuffer/        # ArrayBuffer constructor, static/prototype methods
│   │   ├── constructor.js, isView.js, toString-tag.js
│   │   └── prototype/
│   │       └── slice.js
│   ├── constructors/       # Built-in constructor validation
│   │   └── require-new.js  # Verifies constructors require `new`
│   ├── Error/
│   ├── JSON/
│   ├── Map/
│   ├── Math/
│   ├── Number/             # Number methods and constants
│   │   ├── parseInt.js, parseFloat.js, isNaN.js, isFinite.js, isInteger.js
│   │   └── constants.js    # MAX_SAFE_INTEGER, EPSILON, isSafeInteger, etc.
│   ├── Object/             # Object static and prototype methods
│   │   ├── keys.js, values.js, entries.js, assign.js, create.js, is.js
│   │   ├── defineProperty.js, defineProperties.js, getOwnPropertyDescriptor.js
│   │   ├── freeze.js       # Object.freeze, Object.isFrozen
│   │   ├── getPrototypeOf.js, setPrototypeOf.js
│   │   ├── fromEntries.js, groupBy.js
│   │   ├── prototype/      # Object.prototype instance methods
│   │   │   ├── toString.js, hasOwnProperty.js, isPrototypeOf.js
│   │   │   ├── propertyIsEnumerable.js, toLocaleString.js, valueOf.js
│   │   └── ...
│   ├── Promise/             # Promise constructor, static methods, microtask ordering
│   │   ├── constructor.js, resolve.js, reject.js
│   │   ├── all.js, all-settled.js, race.js, any.js
│   │   ├── microtask-ordering.js, thenable-adoption.js, error-cases.js
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
    ├── decorators/         # TC39 Stage 3 decorators and decorator metadata
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
    │   └── unsupported-features.js  # Verifies parser skip for for/while/do-while/var/with
    └── unary-operators.js
```

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

### Cross-Platform Newline Rules for Data Format Parsers

When a parser implements a format with spec-defined newline semantics, test the format semantics directly instead of inheriting the host OS newline convention.

- Do not treat `LineEnding` / `sLineBreak` as the expected runtime result for parsed data just because the test is running on Windows.
- Prefer explicit `\r\n` or `#13#10` fixtures when adding regression tests for multiline parsing, folding, or block-scalar behavior.
- Assert the format-defined canonical result. Example: TOML multiline strings normalize recognized newlines to LF (`\n`) even when the source text uses CRLF.
- For parser inputs that come from files, use the shared `Goccia.TextFiles` helpers (`ReadUTF8FileText`, `ReadUTF8FileLines`, `StringListToLFText`) instead of `TStringList.LoadFromFile` or `string(UTF8String(...))`. Keep the text as `UTF8String` until parser entry, canonicalize parser-facing source text to LF through the shared helpers, and add at least one regression that hits the real file-loading path with non-ASCII data.
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

### Build the TestRunner

```bash
./build.pas testrunner
```

### Run All Tests

```bash
# Interpreted mode (default) — include --asi for ASI test coverage
./build/TestRunner tests --asi

# Bytecode mode
./build/TestRunner tests --mode=bytecode --asi
```

Both modes must pass. The `--asi` flag enables automatic semicolon insertion tests under `tests/language/asi/`. CI runs the full suite with `--asi` in both interpreted and bytecode mode as separate matrix jobs.

### Run a Specific Test File

```bash
./build/TestRunner tests/language/expressions/addition/basic-addition.js
```

### Run a Test Category

```bash
./build/TestRunner tests/built-ins/Math/
```

### TestRunner Options

| Flag | Description |
|------|-------------|
| `--no-progress` | Suppress per-file progress output |
| `--no-results` | Suppress test results summary |
| `--exit-on-first-failure` | Stop on first test failure |
| `--silent` | Suppress all console output from test scripts |
| `--jobs=N` / `-j N` | Number of parallel worker threads (default: CPU count) |

```bash
# CI-friendly: no progress, stop on first failure
./build/TestRunner tests --no-progress --exit-on-first-failure

# Silent mode: only show results, suppress script console output
./build/TestRunner tests --silent

# Run tests with 4 parallel workers; --jobs=1 forces sequential execution
./build/TestRunner tests --jobs=4
```

**Official YAML Suite**

For YAML parse-validity checks against the official `yaml-test-suite`, run:

```bash
python3 scripts/run_yaml_test_suite.py
python3 scripts/run_yaml_test_suite.py --output=tmp/yaml-suite-results.json
python3 scripts/run_yaml_test_suite.py --suite-dir=/path/to/yaml-test-suite
```

This check is intentionally parse-only: it compares whether each suite case should parse or fail, not whether Goccia's runtime values exactly match the suite's JSON fixtures. That distinction matters because Goccia intentionally canonicalizes complex YAML mapping keys into strings.

**Official TOML Suite**

For TOML 1.1.0 checks against the official `toml-test` corpus, run:

```bash
python3 scripts/run_toml_test_suite.py
python3 scripts/run_toml_test_suite.py --output=tmp/toml-suite-results.json
python3 scripts/run_toml_test_suite.py --suite-dir=/path/to/toml-test
python3 scripts/run_toml_test_suite.py --harness=./build/GocciaTOMLCheck --output=tmp/toml-suite-results.json
```

Unlike the YAML script, this harness compares both parse/fail behavior and the official tagged JSON fixtures for valid cases. It uses a Pascal decoder built around `TGocciaTOMLParser.ParseDocument(...)` so TOML scalar kinds like `integer`, `float`, `datetime`, `datetime-local`, `date-local`, and `time-local` remain visible during compliance checks even though the normal runtime surface still maps date/time values to strings.

The TOML runner exits non-zero when any case fails or times out, so it is safe to use directly in CI. When `--harness` is omitted it compiles `scripts/GocciaTOMLCheck.dpr` automatically; CI uses a prebuilt harness from the matrix build artifacts instead.

The harness and any file-backed parser regression must read source text as UTF-8 bytes first and only then hand it to the parser. In this codebase, `{$mode delphi}` + `{H+}` means plain `string` is an `AnsiString` alias with the default/system code page, while `UTF8String` is the explicit `AnsiString(CP_UTF8)` variant. That means a plain `string` temporary is not a “Unicode string that happens to contain UTF-8”; it is a separate ansistring type that may retag or transcode the bytes. On Windows this commonly shows up as mojibake such as `José -> JosÃ©` or Unicode TOML keys no longer matching. Keep raw file-backed TOML text as `UTF8String` until `TGocciaTOMLParser.Parse(...)` / `ParseDocument(...)` receives it.

**Official JSON5 Suite**

For JSON5 parser compatibility checks against the official `json5/json5` parser test corpus, run:

```bash
python3 scripts/run_json5_test_suite.py
python3 scripts/run_json5_test_suite.py --output=tmp/json5-suite-results.json
python3 scripts/run_json5_test_suite.py --suite-dir=/path/to/json5
python3 scripts/run_json5_test_suite.py --harness=./build/GocciaJSON5Check --output=tmp/json5-suite-results.json
```

This runner extracts parser cases from the upstream `test/parse.js` and `test/errors.js` files, evaluates them with the reference implementation under Node.js, then compares Goccia's Pascal harness output against the canonical tagged values for valid cases. Invalid cases must fail to parse. The same command also runs `tests/built-ins/JSON5/stringify.js`, which mirrors the upstream stringify surface inside Goccia's JavaScript test harness. The runner exits non-zero when either the upstream parser corpus or the JSON5 stringify suite fails.

### Run Pascal Unit Tests

```bash
./build.pas tests
for t in build/Goccia.*.Test; do "$t"; done
```

## Writing Tests

See [Test Framework API](testing-api.md) for the complete test authoring reference including assertions, mocks, lifecycle hooks, async patterns, and cross-runtime compatibility.

## Test Principles

1. **JavaScript tests are the source of truth** — The JavaScript test suite is the primary mechanism for verifying correctness and ECMAScript compatibility. If a behavior isn't covered by a JavaScript test, it isn't guaranteed.

2. **Isolation** — Each test file is independent. No shared state between files. The TestRunner executes each file in a fresh engine instance.

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

JavaScript end-to-end tests are the **primary** testing mechanism. Every new feature or bug fix must include tests that validate the behavior through the full pipeline (lexer → parser → interpreter/VM execution) using the most public surface available. CI runs all tests in both interpreted and bytecode mode.

- **Specification by example** -- Each test file is a runnable specification of expected behavior.
- **End-to-end validation** -- Tests exercise the full pipeline, catching integration issues that unit tests would miss.
- **Readable specifications** -- JavaScript test files are readable by anyone familiar with Jest/Vitest conventions.
- **Source of truth** -- If a behavior isn't covered by a JavaScript test, it isn't guaranteed.

CLI behavior tests in `pr.yml` form the second layer, verifying that the command-line tools produce correct output structure, handle error cases gracefully, and that flags like `--coverage`, `--output=json`, `--source-map`, and `--timeout` work end-to-end. These tests run in CI on every pull request.

Pascal unit tests (`*.Test.pas`) exist as a tertiary layer for behavior that cannot be reached through script code or other documented user-facing entry points. Even there, prefer stateless, repeatable input/output checks over tests that are tightly coupled to incidental implementation structure.

## How the TestRunner Works

The `TestRunner` program:

1. Scans the provided path for `.js`, `.jsx`, `.ts`, `.tsx`, and `.mjs` files.
2. For each file, creates a fresh `TGocciaEngine` with `[ggTestAssertions, ggFFI]`.
3. Loads the source and appends a `runTests()` call.
4. Executes the script — `describe`/`test` blocks register themselves during execution. Nested `describe` blocks are supported; suite names are composed with ` > ` separators (e.g., `"Outer > Inner"`). Skip state is inherited by nested describes.
5. `runTests()` executes all registered tests and collects results.
6. When running multiple files, `GC.Collect` runs after each file to reclaim memory between script executions.
7. Aggregates pass/fail/skip counts across all files.
8. Prints a summary with total statistics.

## CLI Behavior Tests (CI Integration)

The PR workflow (`.github/workflows/pr.yml`) includes shell-level smoke tests that verify CLI tool behavior on every pull request. These checks assert on command output rather than internal state, catching regressions in the user-facing interface.

**What the CLI tests cover:**

| Area | What is checked |
|------|----------------|
| TestRunner JSON output | `--output` produces valid JSON with `mode`, `totalFiles` fields |
| TestRunner coverage | `--coverage` prints summary; `--coverage-format=lcov` and `--coverage-format=json` write valid output files; branch coverage includes `BRDA`/`BRF` entries |
| ScriptLoader JSON output | `--output=json` envelope includes `ok`, `value`, `output` fields |
| ScriptLoader error display | Syntax errors show source context, caret, and suggestions |
| ScriptLoader coverage | `--coverage` summary, lcov/json file output, bytecode mode coverage |
| ScriptLoader source maps | `--source-map` writes valid source map JSON; rejects stdin without explicit path |
| Numeric separator rejection | Trailing, leading, consecutive separators and invalid positions produce errors |
| Timeout handling | `--timeout` produces `TimeoutError` in JSON output |
| Global injection | `--global`, `--globals` file/module injection, collision detection |
| Stdin smoke tests | Piped input executes correctly in both interpreted and bytecode modes |
| BenchmarkRunner output | `--format=json` produces valid JSON with benchmark structure |

This table is non-exhaustive — the [pr.yml workflow](../.github/workflows/pr.yml) is the source of truth. The intent is to check all CLI flags, all parser error display paths, and all output format correctness. To add a new CLI behavior check, add a step to the appropriate job in `pr.yml`.

## Pascal Unit Tests (Tertiary)

Pascal unit tests are the tertiary testing layer for low-level internals that are difficult to validate through JavaScript alone (e.g., value representation, function/scope internals, and test-framework state behavior).

| Test File | Tests |
|-----------|-------|
| `Goccia.Values.Primitives.Test.pas` | Primitive value creation, type conversion (`ToStringLiteral`, `ToBooleanLiteral`, `ToNumberLiteral`, `TypeName`, `TypeOf`) |
| `Goccia.Values.FunctionValue.Test.pas` | Function/method creation, closure capture, parameter handling, scope resolution, AST evaluation |
| `Goccia.Values.ObjectValue.Test.pas` | Object property operations (`AssignProperty`, `GetProperty`, `DeleteProperty`), prototype chain resolution |
| `Goccia.Builtins.TestAssertions.Test.pas` | All `TGocciaExpectationValue` matchers (`toBe`, `toEqual`, `toStrictEqual`, `toContainEqual`, `toMatchObject`, `toMatch`, `toBeNull`, `toBeNaN`, `toBeUndefined`, `toBeDefined`, `toBeTruthy`, `toBeFalsy`, `toBeGreaterThan`, `toBeLessThan`, `toContain`, `toHaveLength`, `toHaveProperty`, `toBeCloseTo`, `not`); skip and conditional APIs (`describe.skip`, `describe.skipIf`, `describe.runIf`, `test.skipIf`, `test.runIf`) |

These compile as standalone executables and run directly:

```bash
./build.pas tests
./build/Goccia.Values.Primitives.Test
./build/Goccia.Values.FunctionValue.Test
./build/Goccia.Values.ObjectValue.Test
./build/Goccia.Builtins.TestAssertions.Test
```

### Pascal Test Framework

Pascal tests use `TestRunner.pas` which provides a `TTestSuite` base class and a generic `Expect<T>` fluent assertion API:

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
  Expect<string>(Value.ToStringLiteral.Value).ToBe('hello');
  Expect<Double>(Value.ToNumberLiteral.Value).ToBe(42);
  Expect<Integer>(Length(Params)).ToBe(2);
  Expect<Boolean>(Value.ToBooleanLiteral.Value).ToBe(True);
  Expect<Boolean>(Value.ToNumberLiteral.IsNaN).ToBe(False);
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

On failure, `ToBe` produces descriptive messages like `Expected "hello" to be "world"` or `Expected 3.14 to be 2.71`.

`TTestSuite` also provides a `Fail(Message)` method to force a test failure with a custom message.

#### FPC 3.2.2 AArch64 Compiler Bug

`Expect<T>` is intentionally a **standalone generic function** (not a method on `TTestSuite`) and `TExpect<T>` is a **record** (not a class). This design works around a known FPC 3.2.2 AArch64 compiler crash: when a class with a generic method is inherited across compilation units, the compiler raises an internal `EAccessViolation`. The standalone function + record pattern avoids this entirely while preserving the fluent `Expect<T>(...).ToBe(...)` syntax.

### NaN Checks in Pascal Tests

When testing for NaN, use the `IsNaN` property on `TGocciaNumberLiteralValue`:

```pascal
// Correct — uses the property accessor
Expect<Boolean>(Value.ToNumberLiteral.IsNaN).ToBe(True);

// Also correct — IsNaN delegates to Math.IsNaN(FValue) internally
Math.IsNaN(Value.ToNumberLiteral.Value)
```

`TGocciaNumberLiteralValue` stores a single `Double` in `FValue` using standard IEEE 754 bit patterns for NaN, Infinity, and -0. The `IsNaN`, `IsInfinity`, and `IsNegativeZero` property accessors delegate to `Math.IsNaN`, `Math.IsInfinite`, and an endian-neutral sign-bit check respectively. Prefer the property accessors for readability.

#### Testing the Test Framework Itself

`Goccia.Builtins.TestAssertions.Test.pas` validates the JS-level assertion matchers from Pascal. Since the matchers call `AssertionPassed`/`AssertionFailed` (which update `TGocciaTestAssertions` internal state) rather than raising Pascal exceptions, the native tests use two helpers to verify both the return value and the recorded pass/fail outcome:

- `ExpectPass(result)` — Asserts the matcher returned `UndefinedValue` **and** `CurrentTestHasFailures` is `False`
- `ExpectFail(result)` — Asserts the matcher returned `UndefinedValue` **and** `CurrentTestHasFailures` is `True`

Each test starts with a `BeforeEach` override that calls `FAssertions.ResetCurrentTestState` to clear the failure flag. The `CurrentTestHasFailures` property and `ResetCurrentTestState` method are public on `TGocciaTestAssertions` for this purpose.

**When to write Pascal unit tests vs JavaScript tests:**

- **JavaScript tests** (preferred) — For any behavior observable from script code. This is almost everything.
- **Pascal unit tests** — Only for internal implementation details not reachable from script code (e.g., the test framework's own pass/fail recording logic).

## CI Integration

GitHub Actions CI (`.github/workflows/ci.yml`) runs on push to `main` and tags, with a five-job pipeline plus release packaging:

```text
build → test             → artifacts
      → toml-compliance  →
      → json5-compliance →
      → benchmark        →
      → examples         →
```

**`build`** — Installs FPC once per platform, compiles all binaries, uploads them as intermediate artifacts.

**`test`** (needs build, all platforms) — Downloads pre-built binaries, runs all JavaScript tests and Pascal unit tests. Outputs JSON files via `--output=<file>` for CI timing comparison.

**`toml-compliance`** (all platforms) — Downloads the prebuilt `GocciaTOMLCheck` harness from the matrix build artifacts, resolves `python3` or `python`, runs `scripts/run_toml_test_suite.py --harness=... --output=toml-test-results-<target>.json`, checks that the JSON summary reports zero failures, and uploads the per-platform TOML conformance report as a workflow artifact.

**`json5-compliance`** (all platforms) — Downloads the prebuilt `GocciaJSON5Check` harness and `TestRunner` binary from the matrix build artifacts, resolves `python3` or `python`, runs `scripts/run_json5_test_suite.py --harness=... --test-runner=... --output=json5-test-results-<target>.json`, checks that both the parser and stringify summaries report zero failures, and uploads the per-platform JSON5 conformance report as a workflow artifact.

**`benchmark`** (needs build, all platforms) — Downloads pre-built binaries, runs all benchmarks.

**`examples`** (needs build, all platforms) — Runs the example scripts and ScriptLoader CLI smoke tests.

**`artifacts`** (needs test + toml-compliance + json5-compliance + benchmark + examples, `main` only) — Uploads release binaries after all checks pass.

**`release`** (needs test + toml-compliance + json5-compliance + benchmark + examples, tags only) — Packages and publishes release archives after the same gates pass.

The `test`, `benchmark`, `examples`, and `toml-compliance` jobs run in parallel after `build`. The TOML conformance lane reuses the already-built harness binary from the matrix build artifacts instead of installing FPC again, so platform-specific TOML issues are caught on the same Linux, macOS, and Windows targets as the main test lanes.

### PR Workflow (`.github/workflows/pr.yml`)

Runs on pull requests targeting `main`, on **ubuntu-latest x64 only**:

```text
build → test
      → benchmark → PR comment
      → examples
```

The benchmark comparison comment includes a **Suite Timing** section showing test execution and benchmark duration for both modes. See [benchmarks.md](benchmarks.md#pr-benchmark-comparison) for details on the comparison format.

## Coverage

The TestRunner and ScriptLoader support JavaScript source-level coverage reporting via the `--coverage` flag. Coverage tracks which lines and branches of JavaScript source code are executed at runtime.

### Usage

```bash
# Console summary (printed after test results)
./build/TestRunner tests --coverage

# lcov output (for Codecov, Coveralls, or genhtml)
./build/TestRunner tests --coverage --coverage-format=lcov --coverage-output=coverage.lcov

# JSON output (Istanbul-compatible)
./build/TestRunner tests --coverage --coverage-format=json --coverage-output=coverage.json

# ScriptLoader also supports coverage
./build/ScriptLoader example.js --coverage
```

### What is Tracked

**Line coverage:** Which source lines were executed and how many times. Instrumented in both the tree-walk interpreter (`EvaluateStatement`/`EvaluateExpression`) and the bytecode VM (main dispatch loop with line-change deduplication).

**Branch coverage:** Which branch arms were taken at:

- `if`/`else` statements (branch 0 = consequent, branch 1 = alternate)
- Ternary expressions (`? :`)
- Short-circuit operators (`&&`, `||`, `??`)
- `switch` statement case clauses

### Output Formats

| Format | Flag | Description |
|--------|------|-------------|
| Console | `--coverage` | Summary table printed to stdout after test results |
| lcov | `--coverage-format=lcov --coverage-output=<file>` | Standard lcov tracefile with `DA:` and `BRDA:` entries |
| JSON | `--coverage-format=json --coverage-output=<file>` | Istanbul-compatible JSON for tooling integration |

### Architecture

Coverage uses a runtime boolean check (`CoverageEnabled` on `TGocciaEvaluationContext` for the interpreter, `FCoverageEnabled` on `TGocciaVM` for bytecode). When `--coverage` is not passed, the boolean is `False` and branch prediction makes the check effectively free — no separate build is needed.

Data is collected by the `TGocciaCoverageTracker` singleton (`Goccia.Coverage.pas`), which follows the same Initialize/Shutdown pattern as `TGarbageCollector` and `TGocciaCallStack`. Output formatting is in `Goccia.Coverage.Report.pas`.
