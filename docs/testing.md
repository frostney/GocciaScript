# Testing

JavaScript end-to-end tests are the **primary** way of testing GocciaScript and ensuring ECMAScript compatibility. Every new feature or bug fix should include JavaScript tests that exercise the full pipeline (lexer → parser → evaluator). Pascal unit tests exist as a secondary layer for low-level value system validation.

## Test Organization

```
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
│   ├── Error/
│   ├── JSON/
│   ├── Map/
│   ├── Math/
│   ├── Number/             # Number methods and constants
│   │   ├── parseInt.js, parseFloat.js, isNaN.js, isFinite.js, isInteger.js
│   │   └── constants.js    # MAX_SAFE_INTEGER, EPSILON, isSafeInteger, etc.
│   ├── Object/             # Object static methods
│   │   ├── keys.js, values.js, entries.js, assign.js, create.js, is.js
│   │   ├── defineProperty.js, defineProperties.js, getOwnPropertyDescriptor.js
│   │   ├── freeze.js       # Object.freeze, Object.isFrozen
│   │   ├── getPrototypeOf.js
│   │   ├── fromEntries.js
│   │   └── ...
│   ├── Set/
│   ├── String/
│   │   └── prototype/
│   └── Symbol/
│
└── language/               # Core language feature tests
    ├── classes/            # Class declarations, inheritance, private fields/methods/getters/setters
    ├── declarations/       # let, const
    ├── expressions/        # Arithmetic, comparison, logical, destructuring, etc.
    │   ├── addition/       # Addition with ToPrimitive
    │   ├── bitwise/        # Bitwise OR, AND, XOR, NOT, shifts
    │   ├── modulo/         # Floating-point modulo
    │   ├── conditional/    # Ternary precedence
    │   ├── optional-chaining/  # Optional chaining (?.) edge cases
    │   └── ...
    ├── functions/          # Arrow functions, closures, higher-order, recursion
    │   └── function-length-name.js  # Function.length and Function.name
    ├── identifiers/        # Unicode support, reserved words
    ├── modules/            # Import/export
    ├── objects/            # Object literals, methods, computed properties
    ├── statements/         # if/else, switch/case/break, try/catch/finally
    │   └── try-catch/      # Try-catch-finally edge cases
    └── unary-operators.js
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

### Run Pascal Unit Tests

```bash
./build.pas tests
./build/Goccia.Values.Primitives.Test
./build/Goccia.Values.FunctionValue.Test
./build/Goccia.Values.ObjectValue.Test
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
    }).toThrow();
  });
});
```

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

// Type checks
expect(value).toBeNull();
expect(value).toBeNaN();
expect(value).toBeUndefined();
expect(value).toBeTruthy();
expect(value).toBeFalsy();

// Comparison
expect(value).toBeGreaterThan(n);
expect(value).toBeLessThan(n);
expect(value).toBeCloseTo(n, digits);

// Collections
expect(value).toContain(item);
expect(value).toHaveLength(n);
expect(value).toHaveProperty(name);

// Type
expect(value).toBeInstanceOf(ClassName);

// Errors
expect(() => throwingFn()).toThrow();

// Negation
expect(value).not.toBe(wrong);
expect(value).not.toContain(item);
```

### Lifecycle Hooks

```javascript
describe("with setup", () => {
  let instance;

  beforeEach(() => {
    instance = new MyClass();
  });

  afterEach(() => {
    // cleanup
  });

  test("uses instance", () => {
    expect(instance).toBeTruthy();
  });
});
```

### Skipping Tests

```javascript
test.skip("not yet implemented", () => {
  // This test will be counted but not executed
});
```

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

**GocciaScript-specific behaviors** — Some tests exercise GocciaScript extensions or intentional divergences from the spec (e.g., `Math.clamp`, `in` operator on strings/primitives, emoji identifiers, arrow function `this` binding in object methods). These will fail in Vitest since standard JS doesn't support them. This is expected.

### Known Vitest Divergences

| Category | GocciaScript | Standard JS |
|----------|-------------|-------------|
| `Math.clamp` | Supported (TC39 proposal) | Not available |
| `"prop" in null` | Returns `false` | Throws `TypeError` |
| `0 in "string"` | Returns `true`/`false` | Throws `TypeError` |
| Emoji identifiers | Supported | Not supported by V8/Rollup |
| `Symbol` in template literals | Converts to `"Symbol(desc)"` | Throws `TypeError` |
| Arrow methods `this` | Binds to owning object | Inherits from enclosing scope |
| Global `parseInt`, `isNaN`, etc. | Not available (use `Number.*`) | Available as global functions |

## Test Principles

1. **JavaScript tests are the source of truth** — The JavaScript test suite is the primary mechanism for verifying correctness and ECMAScript compatibility. If a behavior isn't covered by a JavaScript test, it isn't guaranteed.

2. **Isolation** — Each test file is independent. No shared state between files. The TestRunner executes each file in a fresh engine instance.

3. **Grouped by feature** — Tests are organized by the feature they validate, mirroring the structure of the language specification.

4. **One concern per file** — Each test file focuses on a single method, operation, or language feature. `array-creation.js` tests array creation; `map.js` tests `Array.prototype.map`.

5. **Edge cases matter** — Tests should cover `NaN`, `undefined`, `null`, empty arrays, negative numbers, boundary conditions, and type coercion scenarios.

6. **Readable as specification** — Test names should describe the expected behavior. A passing test suite serves as living documentation of what GocciaScript supports.

## How the TestRunner Works

The `TestRunner` program:

1. Scans the provided path for `.js` files.
2. For each file, creates a fresh `TGocciaEngine` with `DefaultGlobals + [ggTestAssertions]`.
3. Loads the source and appends a `runTests()` call.
4. Executes the script — `describe`/`test` blocks register themselves during execution.
5. `runTests()` executes all registered tests and collects results.
6. Aggregates pass/fail/skip counts across all files.
7. Prints a summary with total statistics.

## Pascal Unit Tests (Secondary)

Pascal unit tests are a secondary testing layer for low-level value system internals. They validate things that are difficult to test through JavaScript alone (e.g., memory layout, internal type conversion edge cases).

| Test File | Tests |
|-----------|-------|
| `Goccia.Values.Primitives.Test.pas` | Primitive value creation, type conversion (`ToStringLiteral`, `ToBooleanLiteral`, `ToNumberLiteral`, `TypeName`, `TypeOf`) |
| `Goccia.Values.FunctionValue.Test.pas` | Function/method creation, closure capture, parameter handling, scope resolution, AST evaluation |
| `Goccia.Values.ObjectValue.Test.pas` | Object property operations (`AssignProperty`, `GetProperty`, `DeleteProperty`), prototype chain resolution |

These compile as standalone executables and run directly:

```bash
./build.pas tests
./build/Goccia.Values.Primitives.Test
./build/Goccia.Values.FunctionValue.Test
./build/Goccia.Values.ObjectValue.Test
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

**When to write Pascal unit tests vs JavaScript tests:**
- **JavaScript tests** (preferred) — For any behavior observable from script code. This is almost everything.
- **Pascal unit tests** — Only for internal implementation details not reachable from script code.

## Performance Benchmarks

GocciaScript includes a benchmark runner for measuring execution performance. Benchmarks live in the `benchmarks/` directory and use a `suite`/`bench` API.

### Running Benchmarks

```bash
# Build the BenchmarkRunner
./build.pas benchmarkrunner

# Run all benchmarks
./build/BenchmarkRunner benchmarks

# Run a specific benchmark
./build/BenchmarkRunner benchmarks/fibonacci.js
```

### Writing Benchmarks

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

### How the BenchmarkRunner Works

The `BenchmarkRunner` program:

1. Scans the provided path for `.js` files.
2. For each file, creates a `TGocciaEngine` with `DefaultGlobals + [ggBenchmark]`.
3. Loads the source and appends a `runBenchmarks()` call.
4. Executes the script via `ExecuteWithTiming` — which measures lex, parse, and execute phases separately.
5. `suite()` calls execute immediately, registering `bench()` entries.
6. `runBenchmarks()` runs each registered benchmark:
   - **Warmup:** 3 iterations to stabilize.
   - **Calibrate:** Doubles batch size until it runs for at least 1 second.
   - **Measure:** Runs the calibrated number of iterations and records total time.
7. Reports ops/sec, mean ms/op, iteration count, and engine timing breakdown per file.

### Benchmark Script API

| Function | Description |
|----------|-------------|
| `suite(name, fn)` | Group benchmarks (like `describe` in tests). Executes `fn` immediately. |
| `bench(name, fn)` | Register a benchmark. `fn` is called many times during measurement. |
| `runBenchmarks()` | Execute all registered benchmarks and return results. Injected automatically by BenchmarkRunner. |

### Available Benchmarks

| File | Covers |
|------|--------|
| `benchmarks/fibonacci.js` | Recursive vs iterative computation |
| `benchmarks/arrays.js` | Array.from, map, filter, reduce, forEach, find, sort, flat, flatMap |
| `benchmarks/objects.js` | Object creation, property access, Object.keys/entries, spread |
| `benchmarks/strings.js` | Concatenation, template literals, split/join, indexOf, trim, replace, pad |
| `benchmarks/classes.js` | Instantiation, method dispatch, single/multi-level inheritance, private fields, getters/setters |
| `benchmarks/closures.js` | Closure capture, higher-order functions, call/apply/bind, recursion |
| `benchmarks/collections.js` | Set add/has/delete/forEach, Map set/get/has/delete/forEach/keys/values |
| `benchmarks/json.js` | JSON.parse, JSON.stringify, roundtrip with nested and mixed data |
| `benchmarks/destructuring.js` | Array/object/parameter destructuring, rest, defaults, nesting |

### Sample Output

```
Running benchmark: benchmarks/fibonacci.js
  Lex: 0ms | Parse: 0ms | Execute: 9984ms

  fibonacci
    recursive fib(15)                        239 ops/sec      4.19 ms/op    (240 iterations)
    recursive fib(20)                         15 ops/sec     64.75 ms/op    (20 iterations)
    iterative fib(20) via reduce          10,341 ops/sec      0.10 ms/op    (18200 iterations)

Benchmark Summary
  Total benchmarks: 3
  Total duration: 10.0s
```

## CI Integration

GitHub Actions CI (`.github/workflows/ci.yml`) runs on push to `main` and on pull requests, with a four-job pipeline:

```
build → test       → artifacts
      → benchmark  →
```

**`build`** — Installs FPC once per platform, compiles all binaries, uploads them as intermediate artifacts.

**`test`** (needs build, all platforms) — Downloads pre-built binaries, runs all JavaScript tests and Pascal unit tests.

**`benchmark`** (needs build, all platforms) — Downloads pre-built binaries, runs all benchmarks.

**`artifacts`** (needs test + benchmark, push only) — Uploads release binaries after both test and benchmark pass.

The `test` and `benchmark` jobs run in parallel since they both depend only on `build`. FPC is installed once per platform, not repeated. Both run across Linux, macOS, and Windows (x64 + ARM where applicable), so platform-specific regressions are caught in both tests and benchmarks.
