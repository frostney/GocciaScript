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
    │   ├── modulo/         # Floating-point modulo
    │   ├── conditional/    # Ternary precedence
    │   └── ...
    ├── functions/          # Arrow functions, closures, higher-order, recursion
    │   └── function-length-name.js  # Function.length and Function.name
    ├── identifiers/        # Unicode support, reserved words
    ├── modules/            # Import/export
    ├── objects/            # Object literals, methods, computed properties
    ├── statements/         # if/else, switch/case/break, try/catch/finally
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
| `Goccia.Values.Primitives.Test.pas` | Primitive value creation, type conversion, equality |
| `Goccia.Values.FunctionValue.Test.pas` | Function creation, closure capture, parameter handling |
| `Goccia.Values.ObjectValue.Test.pas` | Object property operations, prototype chain, descriptors |

These compile as standalone executables and run directly:

```bash
./build.pas tests
./build/Goccia.Values.Primitives.Test
```

**When to write Pascal unit tests vs JavaScript tests:**
- **JavaScript tests** (preferred) — For any behavior observable from script code. This is almost everything.
- **Pascal unit tests** — Only for internal implementation details not reachable from script code.

## CI Integration

The GitHub Actions workflow (`.github/workflows/build.yml`) runs on every push to `main`:

1. Builds Pascal unit tests and runs them.
2. Builds the TestRunner.
3. Runs all JavaScript tests via `./build/TestRunner tests`.
4. Builds ScriptLoader and REPL to verify compilation.

Tests run across Linux, macOS, and Windows (both x64 and ARM where applicable).
