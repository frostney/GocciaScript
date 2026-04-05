# Testing

*For contributors writing, running, or debugging tests.*

JavaScript end-to-end tests are the **primary** way of testing GocciaScript and ensuring ECMAScript compatibility. Every new feature or bug fix should include tests that exercise the full pipeline (lexer → parser → evaluator) through the same public surface that users call. Pascal unit tests exist as a secondary layer for low-level runtime and value system validation.

When choosing where to add coverage, prefer the most public entry point available:

- JavaScript feature behavior: add or extend tests under `tests/`
- CLI behavior (`ScriptLoader`, `TestRunner`, `BenchmarkRunner`): add command-level or workflow-level smoke tests that assert on the visible output
- Pascal unit tests: prefer visible public behavior and deterministic input/output, and add native `*.Test.pas` coverage only when the behavior is genuinely internal or shared in a way that is not reachable through a stable public API

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

## Running Tests

### Build the TestRunner

```bash
./build.pas testrunner
```

### Run All Tests

```bash
./build/TestRunner tests
```

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

```bash
# CI-friendly: no progress, stop on first failure
./build/TestRunner tests --no-progress --exit-on-first-failure

# Silent mode: only show results, suppress script console output
./build/TestRunner tests --silent
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

The harness and any file-backed parser regression must read source text as UTF-8 bytes first and only then hand it to the parser. On Windows, explicit casts like `string(UTF8StringBytes)` and default `TStringList.LoadFromFile` calls can silently reinterpret UTF-8 through the local ANSI code page, which shows up in CI as mojibake such as `José -> JosÃ©` or Unicode TOML keys no longer matching.

### Run Pascal Unit Tests

```bash
./build.pas tests
for t in build/Goccia.*.Test; do "$t"; done
```

## Writing Tests

### Test File Structure

Each test file is a standalone JavaScript file using the built-in test framework:

```javascript
describe("Feature Name", () => {
  test("should do something specific", () => {
    const result = someOperation();
    expect(result).toBe(expectedValue);
  });

  test("should handle edge case", () => {
    expect(() => {
      riskyOperation();
    }).toThrow(RangeError);
  });

  describe("sub-feature", () => {
    test("nested test", () => {
      expect(1 + 1).toBe(2);
    });
  });
});
```

Nested `describe` blocks compose their suite names with ` > ` separators. In the example above, the nested test's suite name would be `"Feature Name > sub-feature"`.

### Cross-Platform Newline Rules for Data Format Parsers

When a parser implements a format with spec-defined newline semantics, test the format semantics directly instead of inheriting the host OS newline convention.

- Do not treat `LineEnding` / `sLineBreak` as the expected runtime result for parsed data just because the test is running on Windows.
- Prefer explicit `\r\n` or `#13#10` fixtures when adding regression tests for multiline parsing, folding, or block-scalar behavior.
- Assert the format-defined canonical result. Example: TOML multiline strings normalize recognized newlines to LF (`\n`) even when the source text uses CRLF.
- For parser inputs that come from files, read UTF-8 bytes with the shared helper instead of `TStringList.LoadFromFile` or `string(UTF8String(...))`, then add at least one regression that hits the real file-loading path with non-ASCII data.
- Keep one public-surface regression in `tests/` and, when the parser exposes a reusable native utility like `TGocciaTOMLParser`, add a focused Pascal regression alongside it as well.

### Test Metadata (Optional)

Test files can include JSDoc-style metadata:

```javascript
/*---
description: Tests for basic addition operations
features: [addition, arithmetic]
---*/
```

### Available Assertions

```javascript
// Equality
expect(value).toBe(expected);           // Strict equality (===)
expect(value).toEqual(expected);        // Deep equality
expect(value).toStrictEqual(expected);  // Deep equality alias for Vitest compatibility

// Type checks
expect(value).toBeNull();
expect(value).toBeNaN();
expect(value).toBeUndefined();
expect(value).toBeDefined();
expect(value).toBeTruthy();
expect(value).toBeFalsy();

// Comparison
expect(value).toBeGreaterThan(n);
expect(value).toBeLessThan(n);
expect(value).toBeCloseTo(n, digits);

// Collections
expect(array).toContain(item);       // Array element, Set element, or string substring
expect(array).toContainEqual(item);  // Deep-equal array element
expect(string).toMatch("part");      // String substring match
expect(string).toMatch(/pattern/);   // Regular expression match
expect(value).toMatchObject(obj);    // Partial recursive object match
expect(value).toHaveLength(n);
expect(value).toHaveProperty(name);

// Type
expect(value).toBeInstanceOf(ClassName);

// Errors
expect(() => throwingFn()).toThrow();
expect(() => throwingFn()).toThrow(TypeError);  // Check error constructor
expect(() => throwingFn()).toThrow(RangeError);

// Negation
expect(value).not.toBe(wrong);
expect(value).not.toContain(item);
```

When `.toMatch()` receives a `RegExp`, the matcher uses regex semantics but does not mutate or depend on the regex object's current `lastIndex`.

### Lifecycle Hooks

`beforeAll` and `afterAll` run once per suite. `beforeEach` and `afterEach` run around every test in the suite and are inherited by nested suites.

```javascript
describe("with setup", () => {
  let instance;

  beforeAll(() => {
    instance = createSharedFixture();
  });

  beforeEach(() => {
    instance = new MyClass();
  });

  afterEach(() => {
    // cleanup
  });

  afterAll(() => {
    instance = null;
  });

  test("uses instance", () => {
    expect(instance).toBeTruthy();
  });
});
```

Hooks can also be `async`, allowing `await` in the hook body:

```javascript
describe("async setup", () => {
  beforeEach(async () => {
    const data = await Promise.resolve("ready");
  });

  afterEach(async () => {
    await Promise.resolve();
  });
});
```

### Focus, Placeholders, and Parameterized Tests

```javascript
test.only("run just this test", () => {
  expect(2 + 2).toBe(4);
});

describe.only("run just this suite", () => {
  test("focused suite test", () => {
    expect(true).toBe(true);
  });
});

test.todo("add edge-case coverage");

test.each([
  [1, 2, 3],
  [2, 3, 5],
])("adds %i + %i = %i", (a, b, expected) => {
  expect(a + b).toBe(expected);
});

describe.each([
  ["one", 1],
  ["two", 2],
])("row %s", (label, value) => {
  test("uses each row as suite arguments", () => {
    expect(value > 0).toBe(true);
  });
});
```

When any `.only` test or suite is registered, all non-focused tests are treated as skipped for that run. `test.todo(...)` placeholders are also reported as skipped.

### Async Tests (Promises)

Test callbacks can be `async` functions, allowing `await` directly in the test body and inside `expect()` calls:

```javascript
test("async test with await", async () => {
  const result = await Promise.resolve(42);
  expect(result).toBe(42);
});

test("await inside expect", async () => {
  expect(await Promise.resolve(42)).toBe(42);
});

test("await async function result in expect", async () => {
  const fetchData = async () => [1, 2, 3];
  expect(await fetchData()).toEqual([1, 2, 3]);
});
```

Tests can also return a Promise from a non-async callback. The test framework automatically drains the microtask queue after each test callback and checks the returned Promise's state. If the Promise is rejected, the test fails with the rejection reason.

```javascript
test("async value check", () => {
  return Promise.resolve(42).then((v) => {
    expect(v).toBe(42);
  });
});

test("async error handling", () => {
  return Promise.reject("err")
    .catch((e) => "recovered")
    .then((v) => {
      expect(v).toBe("recovered");
    });
});
```

Both patterns work because GocciaScript's `await` is a synchronous microtask drain — the entire async function body executes within a single `.Call()`, so assertions run before the Promise settles. Place assertions inside `.then()` or `.catch()` handlers when using the Promise-return pattern.

**Important:** If a test returns a Promise that is still pending after the microtask queue drains, the test **fails** with "Promise still pending after microtask drain". Since GocciaScript has no event loop, a pending Promise after drain will never settle — this catches tests with missing assertions or broken async chains. This mirrors how Jest/Vitest fail tests with a timeout when the returned Promise never resolves.

**Testing intentionally-pending Promises:** When testing behavior around forever-pending Promises (e.g., verifying that `reject()` after `resolve(pendingPromise)` is ignored), never return the pending Promise. Instead, use a separate settled Promise chain to verify state after microtasks drain:

```javascript
test("reject after resolve with pending promise is ignored", () => {
  const pending = new Promise(() => {});
  let rejectHandlerCalled = false;
  const p = new Promise((resolve, reject) => {
    resolve(pending);
    reject("should be ignored");
  });
  p.catch(() => { rejectHandlerCalled = true; });
  // Return a separate settled chain — assertions run after microtask drain
  return Promise.resolve().then(() => {
    expect(rejectHandlerCalled).toBe(false);
  });
});
```

### Promise Matchers: `.resolves` and `.rejects`

The `expect()` object supports `.resolves` and `.rejects` properties for Vitest/Jest-compatible Promise assertions:

```javascript
// .resolves — unwrap a fulfilled Promise
test("resolves example", async () => {
  await expect(Promise.resolve(42)).resolves.toBe(42);
  await expect(Promise.resolve([1, 2])).resolves.toEqual([1, 2]);
  await expect(Promise.resolve(null)).resolves.toBeNull();
});

// .rejects — unwrap a rejected Promise
test("rejects example", async () => {
  await expect(Promise.reject("oops")).rejects.toBe("oops");
  await expect(Promise.reject({ code: 404 })).rejects.toEqual({ code: 404 });
});

// .rejects.toThrow() — check rejection error type
test("rejects.toThrow example", async () => {
  const fn = async () => { throw new TypeError("bad"); };
  await expect(fn()).rejects.toThrow(TypeError);
});
```

Both properties drain the microtask queue, then return a new expectation with the unwrapped value. All standard matchers (`.toBe()`, `.toEqual()`, `.toThrow()`, `.not`, etc.) chain after `.resolves`/`.rejects`. Both require an actual Promise — call async functions explicitly: `expect(fn())` not `expect(fn)`.

### Skipping Tests and Suites

Individual tests can be skipped unconditionally with `test.skip`:

```javascript
test.skip("not yet implemented", () => {
  // This test will be counted but not executed
});
```

Entire suites can be skipped unconditionally with `describe.skip`:

```javascript
describe.skip("feature under development", () => {
  test("will not run", () => { ... });
  test("also will not run", () => { ... });
});
```

### Conditional Skipping and Running

Both `describe` and `test` support `skipIf(condition)` and `runIf(condition)` for conditional execution. Each returns a function that accepts the usual `(name, fn)` arguments:

```javascript
const hasFeature = typeof Temporal !== "undefined";

// Skip if condition is truthy
describe.skipIf(true)("skipped suite", () => { ... });
test.skipIf(true)("skipped test", () => { ... });

// Run only if condition is truthy (inverse of skipIf)
describe.runIf(hasFeature)("Temporal tests", () => { ... });
test.runIf(hasFeature)("uses Temporal.Now", () => { ... });
```

These follow the [Bun test runner API](https://bun.com/reference/bun/test/Describe/skipIf). When skipped, tests are counted in the total but not executed and reported as skipped.

## Cross-Runtime Compatibility (Vitest)

Tests are designed to pass in both GocciaScript's TestRunner and standard JavaScript via [Vitest](https://vitest.dev/). This ensures tests serve as both GocciaScript validation and ECMAScript conformance checks.

### Running with Vitest

```bash
npx vitest run                    # Run all tests
npx vitest run tests/built-ins/   # Run a category
npx vitest                        # Watch mode
```

The `vitest.config.js` at the project root configures Vitest to discover test files in `tests/`.

### Writing Cross-Compatible Tests

When writing tests that should pass in both environments, follow these patterns:

**Iterators** — GocciaScript returns arrays from `Map.keys()`, `Map.values()`, `Map.entries()`, and `Set.values()`, while standard JS returns iterator objects. Wrap calls with spread to normalize:

```javascript
// Works in both GocciaScript and standard JS
expect([...map.keys()]).toEqual(["a", "b", "c"]);
expect([...set.values()]).toEqual([1, 2, 3]);
```

**GocciaScript-specific behaviors** — Some tests exercise GocciaScript extensions or intentional divergences from the spec (e.g., `Math.clamp`, emoji identifiers, arrow function `this` binding in object methods). These will fail in Vitest since standard JS doesn't support them. This is expected.

### Known Vitest Divergences

| Category | GocciaScript | Standard JS |
|----------|-------------|-------------|
| `Math.clamp` | Supported (TC39 proposal) | Not available |
| Emoji identifiers | Supported | Not supported by V8/Rollup |
| Arrow methods `this` | Binds to owning object | Inherits from enclosing scope |
| Global `parseInt`, `isNaN`, etc. | Not available (use `Number.*`) | Available as global functions |

## Test Principles

1. **JavaScript tests are the source of truth** — The JavaScript test suite is the primary mechanism for verifying correctness and ECMAScript compatibility. If a behavior isn't covered by a JavaScript test, it isn't guaranteed.

2. **Isolation** — Each test file is independent. No shared state between files. The TestRunner executes each file in a fresh engine instance.

3. **Grouped by feature** — Tests are organized by the feature they validate, mirroring the structure of the language specification. Prototype methods live in `prototype/` subfolders; static methods live in the built-in's root folder.

4. **One method per file** — Each test file focuses on a single method, operation, or language feature. `fill.js` tests `TypedArray.prototype.fill`; `map.js` tests `Array.prototype.map`. Never bundle multiple methods into a single file.

5. **Edge cases are co-located** — Edge cases (NaN, Infinity, negative indices, empty collections, clamping, boundary conditions, type coercion) belong **in the same file** as the happy-path tests for that method. Do not create separate `edge-cases.js` files — they become disconnected from the feature they validate and make it unclear which method they test.

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

## How the TestRunner Works

The `TestRunner` program:

1. Scans the provided path for `.js`, `.jsx`, `.ts`, `.tsx`, and `.mjs` files.
2. For each file, creates a fresh `TGocciaEngine` with `DefaultGlobals + [ggTestAssertions]`.
3. Loads the source and appends a `runTests()` call.
4. Executes the script — `describe`/`test` blocks register themselves during execution. Nested `describe` blocks are supported; suite names are composed with ` > ` separators (e.g., `"Outer > Inner"`). Skip state is inherited by nested describes.
5. `runTests()` executes all registered tests and collects results.
6. When running multiple files, `GC.Collect` runs after each file to reclaim memory between script executions.
7. Aggregates pass/fail/skip counts across all files.
8. Prints a summary with total statistics.

## Pascal Unit Tests (Secondary)

Pascal unit tests are a secondary testing layer for low-level value system internals. They validate things that are difficult to test through JavaScript alone (e.g., memory layout, internal type conversion edge cases).

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

When testing for NaN, use the `IsNaN` property on `TGocciaNumberLiteralValue` rather than `Math.IsNaN()`:

```pascal
// Correct — uses the internal special value flag
Expect<Boolean>(Value.ToNumberLiteral.IsNaN).ToBe(True);

// WRONG — NaN values store 0.0 internally, so Math.IsNaN returns False
IsNaN(Value.ToNumberLiteral.Value)  // always False!
```

This is because GocciaScript represents NaN via a `FSpecialValue = nsvNaN` flag with `FValue` set to 0 (avoiding floating-point NaN propagation issues). The `IsNaN` property checks the flag; `Math.IsNaN` checks the stored double.

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
      → benchmark        →
      → examples         →
```

**`build`** — Installs FPC once per platform, compiles all binaries, uploads them as intermediate artifacts.

**`test`** (needs build, all platforms) — Downloads pre-built binaries, runs all JavaScript tests and Pascal unit tests. Outputs JSON files via `--output=<file>` for CI timing comparison.

**`toml-compliance`** (all platforms) — Downloads the prebuilt `GocciaTOMLCheck` harness from the matrix build artifacts, resolves `python3` or `python`, runs `scripts/run_toml_test_suite.py --harness=... --output=toml-test-results-<target>.json`, checks that the JSON summary reports zero failures, and uploads the per-platform TOML conformance report as a workflow artifact.

**`benchmark`** (needs build, all platforms) — Downloads pre-built binaries, runs all benchmarks.

**`examples`** (needs build, all platforms) — Runs the example scripts and ScriptLoader CLI smoke tests.

**`artifacts`** (needs test + toml-compliance + benchmark + examples, `main` only) — Uploads release binaries after all checks pass.

**`release`** (needs test + toml-compliance + benchmark + examples, tags only) — Packages and publishes release archives after the same gates pass.

The `test`, `benchmark`, `examples`, and `toml-compliance` jobs run in parallel after `build`. The TOML conformance lane reuses the already-built harness binary from the matrix build artifacts instead of installing FPC again, so platform-specific TOML issues are caught on the same Linux, macOS, and Windows targets as the main test lanes.

### PR Workflow (`.github/workflows/pr.yml`)

Runs on pull requests targeting `main`, on **ubuntu-latest x64 only**:

```text
build → test
      → benchmark → PR comment
      → examples
```

The benchmark comparison comment includes a **Suite Timing** section showing test execution and benchmark duration for both modes. See [benchmarks.md](benchmarks.md#pr-benchmark-comparison) for details on the comparison format.
