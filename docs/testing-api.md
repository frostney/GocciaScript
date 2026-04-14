# Test Framework API

*Assertions, mocks, lifecycle hooks, async patterns, and cross-runtime compatibility.*

## Executive Summary

- **Test file structure** --- Each test file is a standalone JavaScript file using `describe`/`test` blocks with nested suites composing names via ` > ` separators
- **Assertion API** --- `expect(value)` chains with `.toBe()`, `.toEqual()`, `.toThrow()`, `.toContain()`, `.toMatch()`, `.not`, and other matchers for equality, type, comparison, and collection checks
- **Mock functions** --- `mock()` creates tracked mock functions and `spyOn()` wraps existing methods; both record calls, arguments, return values, and support configurable behavior
- **Lifecycle hooks** --- `beforeAll`/`afterAll` run once per suite, `beforeEach`/`afterEach` run around every test and are inherited by nested suites, and `onTestFinished` registers per-test cleanup
- **Async patterns** --- Tests can be `async` functions or return Promises; `.resolves`/`.rejects` matchers unwrap Promises for Vitest/Jest-compatible assertions
- **Vitest compatibility** --- Tests are designed to pass in both GocciaScript's TestRunner and Vitest, with known divergences around `mock()`/`spyOn()` globals, `Math.clamp`, emoji identifiers, and arrow method `this` binding

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

### Mock Functions

`mock()` creates a mock function that tracks calls, arguments, return values, and `this` values. `spyOn()` wraps an existing object method with a spy that tracks calls while passing through to the original implementation by default.

```javascript
// Create a mock function
const fn = mock();                     // Returns undefined by default
const fnWithImpl = mock((x) => x * 2); // With an implementation

// Call tracking
fn(1, 2);
fn("a", "b");
fn.mock.calls;      // [[1, 2], ["a", "b"]]
fn.mock.results;    // [{ type: "return", value: undefined }, ...]
fn.mock.contexts;   // [this values for each call]
fn.mock.instances;  // [] (only populated for new calls; see note below)
fn.mock.lastCall;   // ["a", "b"]

// Configure behavior
fn.mockReturnValue(42);              // All calls return 42
fn.mockReturnValueOnce(1);           // Next call returns 1
fn.mockImplementation((x) => x + 1); // Set implementation
fn.mockImplementationOnce(() => 99);  // One-shot implementation

// Chaining
fn.mockReturnValueOnce(1).mockReturnValueOnce(2).mockReturnValueOnce(3);

// Reset
fn.mockClear();    // Clear tracking, keep implementation
fn.mockReset();    // Clear everything

// Naming
fn.mockName("myFn");
fn.getMockName();  // "myFn"
```

**Priority order** when a mock is called:

1. One-shot implementation (`mockImplementationOnce`) --- FIFO
2. One-shot return value (`mockReturnValueOnce`) --- FIFO
3. Permanent implementation (`mockImplementation`)
4. Permanent return value (`mockReturnValue`)
5. Return `undefined`

#### `spyOn(object, methodName)`

Creates a spy on an existing object method:

```javascript
const obj = { greet: (name) => "hello " + name };
const spy = spyOn(obj, "greet");

obj.greet("world");  // "hello world" --- passes through by default
spy.mock.calls;      // [["world"]]

// Override implementation
spy.mockImplementation(() => "mocked");
obj.greet("test");   // "mocked"

// Restore original
spy.mockRestore();
obj.greet("test");   // "hello test"
```

#### Mock Matchers

```javascript
// Call tracking
expect(fn).toHaveBeenCalled();
expect(fn).toHaveBeenCalledTimes(3);
expect(fn).toHaveBeenCalledWith(1, 2);        // Any call matched
expect(fn).toHaveBeenLastCalledWith("last");   // Last call matched
expect(fn).toHaveBeenNthCalledWith(2, "second"); // Nth call (1-based)

// Return tracking
expect(fn).toHaveReturned();
expect(fn).toHaveReturnedTimes(2);
expect(fn).toHaveReturnedWith(42);             // Any return matched
expect(fn).toHaveLastReturnedWith(42);         // Last return matched
expect(fn).toHaveNthReturnedWith(1, "first");  // Nth return (1-based)

// Negation
expect(fn).not.toHaveBeenCalled();
expect(fn).not.toHaveBeenCalledWith(5, 6);
```

All mock matchers use deep equality for argument and return value comparison.

**GocciaScript vs Vitest/Jest:** `mock()` and `spyOn()` are standalone globals in GocciaScript (equivalent to `vi.fn()` / `vi.spyOn()` in Vitest or `jest.fn()` / `jest.spyOn()` in Jest). Tests using these APIs are GocciaScript-specific and will not run in Vitest without adaptation. GocciaScript follows the Vitest/Jest convention where `mock.instances` only stores objects created via `new`, and `mock.contexts` stores the `this` value for every call.

### Lifecycle Hooks

`beforeAll` and `afterAll` run once per suite. `beforeEach` and `afterEach` run around every test in the suite and are inherited by nested suites. `onTestFinished` registers a per-test cleanup callback from inside the test body --- it runs after all `afterEach` hooks.

| Hook | Scope | Runs |
|------|-------|------|
| `beforeAll(fn)` | Suite | Once before all tests in the suite |
| `beforeEach(fn)` | Suite (inherited) | Before each test |
| `afterEach(fn)` | Suite (inherited) | After each test |
| `afterAll(fn)` | Suite | Once after all tests in the suite |
| `onTestFinished(fn)` | Current test | After afterEach, only for the current test |

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

`onTestFinished` is useful for inline cleanup that is specific to a single test:

```javascript
test("temporary resource", () => {
  const resource = acquireResource();
  onTestFinished(() => {
    resource.release();
  });
  expect(resource.isActive()).toBe(true);
});
```

Multiple `onTestFinished` callbacks run in registration order. Callbacks are scoped to the current test --- they do not leak to subsequent tests.

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

Both patterns work because GocciaScript's `await` is a synchronous microtask drain --- the entire async function body executes within a single `.Call()`, so assertions run before the Promise settles. Place assertions inside `.then()` or `.catch()` handlers when using the Promise-return pattern.

**Important:** If a test returns a Promise that is still pending after the microtask queue drains, the test **fails** with "Promise still pending after microtask drain". Since GocciaScript has no event loop, a pending Promise after drain will never settle --- this catches tests with missing assertions or broken async chains. This mirrors how Jest/Vitest fail tests with a timeout when the returned Promise never resolves.

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
  // Return a separate settled chain --- assertions run after microtask drain
  return Promise.resolve().then(() => {
    expect(rejectHandlerCalled).toBe(false);
  });
});
```

### Promise Matchers: `.resolves` and `.rejects`

The `expect()` object supports `.resolves` and `.rejects` properties for Vitest/Jest-compatible Promise assertions:

```javascript
// .resolves --- unwrap a fulfilled Promise
test("resolves example", async () => {
  await expect(Promise.resolve(42)).resolves.toBe(42);
  await expect(Promise.resolve([1, 2])).resolves.toEqual([1, 2]);
  await expect(Promise.resolve(null)).resolves.toBeNull();
});

// .rejects --- unwrap a rejected Promise
test("rejects example", async () => {
  await expect(Promise.reject("oops")).rejects.toBe("oops");
  await expect(Promise.reject({ code: 404 })).rejects.toEqual({ code: 404 });
});

// .rejects.toThrow() --- check rejection error type
test("rejects.toThrow example", async () => {
  const fn = async () => { throw new TypeError("bad"); };
  await expect(fn()).rejects.toThrow(TypeError);
});
```

Both properties drain the microtask queue, then return a new expectation with the unwrapped value. All standard matchers (`.toBe()`, `.toEqual()`, `.toThrow()`, `.not`, etc.) chain after `.resolves`/`.rejects`. Both require an actual Promise --- call async functions explicitly: `expect(fn())` not `expect(fn)`.

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

**Iterators** --- GocciaScript returns arrays from `Map.keys()`, `Map.values()`, `Map.entries()`, and `Set.values()`, while standard JS returns iterator objects. Wrap calls with spread to normalize:

```javascript
// Works in both GocciaScript and standard JS
expect([...map.keys()]).toEqual(["a", "b", "c"]);
expect([...set.values()]).toEqual([1, 2, 3]);
```

**GocciaScript-specific behaviors** --- Some tests exercise GocciaScript extensions or intentional divergences from the spec (e.g., `Math.clamp`, emoji identifiers, arrow function `this` binding in object methods). These will fail in Vitest since standard JS doesn't support them. This is expected.

### Known Vitest Divergences

| Category | GocciaScript | Standard JS |
|----------|-------------|-------------|
| `Math.clamp` | Supported (TC39 proposal) | Not available |
| Emoji identifiers | Supported | Not supported by V8/Rollup |
| Arrow methods `this` | Binds to owning object | Inherits from enclosing scope |
| Global `parseInt`, `isNaN`, etc. | Not available (use `Number.*`) | Available as global functions |
| `mock()` / `spyOn()` | Standalone globals | `vi.fn()` / `vi.spyOn()` (Vitest) or `jest.fn()` / `jest.spyOn()` (Jest) |

## Related documents

- [Testing](testing.md) --- test organization, directory layout, running tests, and test principles
- [Built-ins](built-ins.md) --- test assertion built-in reference
