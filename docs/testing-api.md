# Test Framework API

*Assertions, mocks, lifecycle hooks, async patterns, and cross-runtime compatibility.*

## Executive Summary

- **Test file structure** --- Each test file is a standalone JavaScript file using `describe`/`test` blocks with nested suites composing names via ` > ` separators
- **Assertion API** --- `expect(value)` chains with `.toBe()`, `.toEqual()`, `.toThrow()`, `.toContain()`, `.toMatch()`, `.not`, and other matchers for equality, type, comparison, and collection checks
- **Mock functions** --- `mock()` creates tracked mock functions and `spyOn()` wraps existing methods; both record calls, arguments, return values, and support configurable behavior
- **Lifecycle hooks** --- `beforeAll`/`afterAll` run once per suite, `beforeEach`/`afterEach` run around every test and are inherited by nested suites, and `onTestFinished` registers per-test cleanup
- **Async patterns** --- Tests can be `async` functions or return Promises; `.resolves`/`.rejects` matchers unwrap Promises for Vitest/Jest-compatible assertions
- **Vitest is the compatibility target** --- The testing API aims at being an exact Vitest drop-in, so Vitest decides what a matcher, hook, or accounting rule is *supposed* to do; bun is a fast proxy whose expect differs from Vitest's often enough that it cannot decide (see [Differential Testing](differential-testing.md)). Known deliberate divergences remain around `mock()`/`spyOn()` globals, `Math.clamp`, emoji identifiers, and arrow-function `this` binding on object properties

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

### Importing the API

`GocciaTestRunner` injects the whole testing API as globals, matching Vitest's `globals: true`, so a suite needs no imports. The same API is also importable from `goccia:test`, which is the canonical spelling in documentation:

```javascript
import { describe, expect, mock, spyOn, test } from "goccia:test";
```

The module exports everything the globals do — `describe`, `test`, `it`, `expect`, `beforeAll`, `beforeEach`, `afterEach`, `afterAll`, `onTestFinished`, `mock`, `spyOn`, and `runTests` — and the imported `test` and `describe` carry their modifiers (`.each`, `.skip`, `.only`, `.todo`). Both spellings drive the same registry, so a mock created through the import is assertable through the global `expect` and the other way round.

#### Availability per binary

The module namespace and the globals install independently. Every host that applies the loader runtime profile registers `goccia:test`; only the runner adds the globals on top.

| Binary | `goccia:test` | Testing globals |
|---|---|---|
| `GocciaTestRunner` | Yes | Yes |
| `GocciaScriptLoader` | Yes | No |
| `GocciaREPL` | Yes | No |
| `GocciaSandboxRunner` | Yes | No |
| `GocciaBenchmarkRunner` | Yes | No |
| `GocciaScriptLoaderBare` | No — attaches no runtime | No |

An embedder gets the same split: `ApplyLoaderRuntimeProfile` registers the module only, and `TGocciaTestingLibraryRuntimeExtension.CreateModuleOnly` is the direct spelling for hosts that assemble their own profile. Passing `AInjectGlobals = True` to that extension's ordinary constructor is what makes the globals appear, and the runner is the only host that does it.

Outside the runner the assertions object is built lazily, on the first import that resolves. A script that never imports `goccia:test` pays nothing for its availability.

#### Running an imported suite outside the runner

`describe` and `test` only *register* — they never execute on their own. Under `GocciaTestRunner` the runner drives execution after the file is evaluated. A loader script has no such driver, so registrations would simply sit in the root suite and nothing would run. `runTests` is the entry point that closes that gap:

```javascript
import { expect, runTests, test } from "goccia:test";

test("adds", () => {
  expect(1 + 1).toBe(2);
});

const results = runTests({ showTestResults: false });
if (results.failed > 0) {
  throw new Error(`${results.failed} test(s) failed`);
}
```

`runTests` executes everything registered so far, prints its report unless `showTestResults` is `false`, and returns a result object with `passed`, `failed`, `skipped`, `totalTests`, `totalRunTests`, `assertions`, `duration`, `suiteErrors`, `failedTests`, and `summary`. It accepts `{ exitOnFirstFailure, showTestResults }`.

`runTests` reports; it does not decide. A failing test does not by itself change the loader's exit status, because the loader has no notion of a test outcome — the script owns that decision, and throwing on `results.failed > 0` as above is what turns a failure into a non-zero exit. A second `runTests` call resets the statistics and re-runs the whole registry, not just the tests registered since the previous call — registration accumulates for the life of the script.

### Available Assertions

```javascript
// Equality
expect(value).toBe(expected);           // Strict equality (===)
expect(value).toEqual(expected);        // Deep equality, ignoring undefined-valued keys and array sparseness
expect(value).toStrictEqual(expected);  // Deep equality, type and undefined sensitive

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
expect(value).toHaveProperty("a.b[0].c", value);  // Path, optional expected value
expect(value).toHaveProperty(["a", "b", 0, "c"], value);

// Type
expect(value).toBeInstanceOf(ClassName);

// Errors
expect(() => throwingFn()).toThrow();                       // Anything thrown
expect(() => throwingFn()).toThrow("partial message");      // Message substring
expect(() => throwingFn()).toThrow(/message pattern/);      // Message pattern
expect(() => throwingFn()).toThrow(TypeError);              // Error constructor
expect(() => throwingFn()).toThrow(new Error("exact"));     // Equal messages
expect(() => throwingFn()).toThrow(expect.any(TypeError));  // Asymmetric matcher
expect(() => nonThrowingFn()).not.toThrow();

// Negation
expect(value).not.toBe(wrong);
expect(value).not.toContain(item);
```

When `.toMatch()` receives a `RegExp`, the matcher uses regex semantics but does not mutate or depend on the regex object's current `lastIndex`.

#### Deep equality

`.toEqual()` and `.toStrictEqual()` share one recursive comparison and differ only in what they forgive. Both use `Object.is` at the leaves, so `NaN` equals `NaN` while `0` and `-0` stay distinct, and both compare `Set` and `Map` contents without regard to insertion order, matching members and entries by deep equality rather than by reference. Neither ever equates different kinds of container: a `Map` is not a plain object, and a `Set` is not an array.

`.toEqual()` ignores three things that `.toStrictEqual()` enforces:

| Ignored by `.toEqual()` | Example |
|---|---|
| Object keys whose value is `undefined` | `{ a: 1, b: undefined }` equals `{ a: 1 }` |
| Array sparseness | `[1, , 3]` equals `[1, undefined, 3]` |
| The object's type | `new Point(1)` equals `{ x: 1 }` |

The forgiveness stops at array length. Arrays of different lengths are never equal under either matcher, so `[1]` does not equal `[1, undefined]` in either direction, and `[]` does not equal `[undefined]`. An `undefined` *item* is only ignorable where a hole and an explicit `undefined` collapse to the same thing at the same index; a trailing one still lengthens the array.

Type equality under `.toStrictEqual()` is a constructor comparison, not a "class instance versus literal" distinction: two values match only when `a.constructor === b.constructor`. A class instance therefore matches only an instance of the same class, a null-prototype object does not match `{}` (it has no `constructor` at all), and `Object.create(proto)` takes its constructor from `proto` — `Object.create(Array.prototype)` compares as an `Array`, not as a plain object.

**Errors** are compared by a rule of their own, and both matchers apply it — they differ only in the constructor check `.toStrictEqual()` adds on top. An error's `name` and `message` are inherited or non-enumerable, so comparing visible properties alone would make every error equal to every other one and to `{}`. Both matchers therefore compare `name` and `message` alongside the error's own enumerable properties, and an error is never equal to a plain object:

```javascript
expect(new Error("a")).toEqual(new Error("a"));
expect(new Error("a")).not.toEqual(new Error("b"));
expect(new TypeError("m")).not.toEqual(new Error("m"));
expect(new Error("a")).not.toEqual({ name: "Error", message: "a" });
expect(new Error("m", { cause: "c" })).not.toEqual(new Error("m", { cause: "d" }));
```

What counts as an error is the internal slot an error constructor installs, not the prototype chain: `Object.create(Error.prototype)` is a plain object and equals `{}`. Under `.toEqual()` an error is then identified by its `name` rather than by its class, so `class MyError extends Error {}` that leaves `name` alone inherits `"Error"` and equals a plain `Error` with the same message; assigning `this.name` makes it distinct. `.toStrictEqual()` makes no exception for errors: its constructor check applies to them like to any other object, so that same subclass is *not* strictly equal to `Error`.

`name`, `message`, and `cause` are compared by value and are never stringified, so a `name` whose `toString` throws does not turn an assertion into a raised error. `cause` is compared asymmetrically — it participates only when the *expected* error has a defined one. `new Error("m", { cause: "c" })` therefore equals `new Error("m")`, while swapping the two sides does not hold, and a `cause` that is present but `undefined` reads as absent to both matchers. A `cause` chain that loops back on itself terminates rather than recursing. `stack` never participates; `AggregateError`'s `errors` does, and is compared like any other value whenever both sides are `AggregateError`s. `.toMatchObject()` keeps subset semantics for the containers around errors but not for the errors themselves: `{ e: new Error("x") }` matches `{ e: {} }`, because an empty expected object constrains nothing, but it does not match `{ e: new Error("y") }`.

One deliberate divergence, in `DOMException`. A DOMException has no error slot, and in a standard runtime its `name`, `message`, and `code` are prototype getters rather than own properties — so Vitest sees two objects with nothing own-enumerable to compare and calls any two DOMExceptions with the same class equal, whatever their `name`s (bun agrees with Vitest here). This engine materializes those three as ordinary own enumerable properties, so it keeps them apart. That difference is intentional and stands: two DOMExceptions with different `name`s differ in every way a test cares about, and Vitest's all-equal result is arguably a bug in it rather than a rule worth copying. No upstream issue has been filed.

#### Property paths

`.toHaveProperty()` accepts a path rather than a flat key. A string path splits on dots and understands bracket indices, so `"items[0].type"` and `"a.b"` both walk into nested values; an array path (`["a", "b", 0, "c"]`) takes each segment verbatim, which is also how you reach a key that itself contains a dot. Each segment resolves the way a normal property read would, so inherited members and the members of a boxed primitive (`"name.length"`) are found. A second argument is compared with `.toEqual()` semantics. Walking into `null` or `undefined` reports a normal assertion failure rather than raising.

`.toMatchObject()` matches a subset of keys at every level and recurses per index through arrays, so `{ list: [{ a: 1, b: 2 }] }` matches `{ list: [{ a: 1 }] }`. Arrays must still match in length. An expected plain object may describe an array (`{ l: { 0: 1 } }` matches `{ l: [1] }`), but an expected array only matches an array.

`.toThrow()` accepts every Vitest argument form, and each form works under `.not` and after `.rejects`. A string matches when the thrown message *contains* it, except for the empty string: `toThrow("")` asserts that the message *is* empty rather than matching everything. A `RegExp` matches the message; a constructor matches by `instanceof`, so a subclass satisfies both its own class and its parent. An `Error` instance is not a message comparison — it is the deep-equality rule above, so the thrown error must also agree on `name`, on own enumerable properties, on `cause` where the expected error has one, and on `errors` when both are `AggregateError`s. A thrown value that is not an error consequently never matches an `Error` instance, even with the same `message`. An asymmetric matcher is delegated to the matcher itself and tested against the thrown value rather than its message, so `expect(fn).toThrow(expect.any(TypeError))` is the asymmetric spelling of `toThrow(TypeError)`. For the string and `RegExp` forms the message of a non-error is the thrown value itself when it is a string, or its `message` property when it has one. Like `.toMatch()`, the `RegExp` form does not depend on or mutate `lastIndex`.

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
fn.mockResolvedValue(42);            // All calls return fresh fulfilled promises
fn.mockResolvedValueOnce(1);         // Next call returns a fulfilled promise
fn.mockRejectedValue(error);         // All calls return fresh rejected promises
fn.mockRejectedValueOnce(error);     // Next call returns a rejected promise
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

1. One-shot queue (`mockImplementationOnce`, `mockReturnValueOnce`, `mockResolvedValueOnce`, or `mockRejectedValueOnce`) --- shared FIFO
2. Permanent implementation (`mockImplementation`, `mockResolvedValue`, or `mockRejectedValue`)
3. Permanent return value (`mockReturnValue`)
4. Return `undefined`

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
expect(fn).toHaveBeenCalledOnce();
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

Tests can also return a Promise from a non-async callback. The test framework automatically drains the microtask queue, pumps any pending fetch completions, and checks the returned Promise's state. If the Promise is rejected, the test fails with the rejection reason.

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

Both patterns work because GocciaScript's `await` is a synchronous drain --- the entire async function body executes within a single `.Call()`, and fetch-backed Promises are settled by pumping fetch completions while waiting. Place assertions inside `.then()` or `.catch()` handlers when using the Promise-return pattern.

**Important:** If a test returns a Promise that is still pending after the microtask queue drains and all pending fetch completions have been pumped, the test **fails** with "Promise still pending after microtask drain". Since GocciaScript has no general event loop, a non-fetch pending Promise after drain will never settle --- this catches tests with missing assertions or broken async chains. This mirrors how Jest/Vitest fail tests with a timeout when the returned Promise never resolves.

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

The target for these is [Vitest's `skipIf`/`runIf`](https://vitest.dev/api/#test-skipif). When skipped, tests are counted in the total but not executed and reported as skipped. The exact accounting of the conditional and `todo` forms has not been through the differential lane yet — the lifecycle battery covers hook and describe accounting, not these — so treat the details here as intent rather than as verified parity.

## Cross-Runtime Compatibility (Vitest)

[Vitest](https://vitest.dev/) is the compatibility target, not merely a second place the tests happen to run: the testing API aims at being an exact drop-in, so where this engine and Vitest disagree about a matcher, a hook, or how a result is counted, Vitest is right by definition and the difference is a defect unless it is written down here as deliberate. Tests that pass in both therefore serve as GocciaScript validation and as ECMAScript conformance checks at once.

That reference covers what the matchers *mean*, not only the API shape: equality, `Set`/`Map`, error, and `toThrow` behavior is probed against a pinned Vitest release and reconciled. Accepting unmodified Vitest suites is a stated project direction rather than a current guarantee — the divergences listed below are the remaining gap. See [Vision](../VISION.md) for where this is heading.

Bun runs the same tests far faster and is used that way as a proxy, but it is advisory only. A three-way audit of 223 probes found bun and Vitest disagreeing on 30 of 178 matcher probes, in both directions, so bun agreeing is evidence and bun disagreeing is a question — neither is a verdict. [Differential Testing](differential-testing.md) describes how the two roles are enforced per battery.

### Running with Vitest

```bash
npx vitest run                    # Run all tests
npx vitest run tests/built-ins/   # Run a category
npx vitest                        # Watch mode
```

Running `tests/` under Vitest needs a local Vitest install and a config that points at those files; the repository pins its own Vitest only for the differential battery lane, under `scripts/differential/`.

### The `vitest` compatibility shim

A suite written against Vitest imports from a bare `vitest` specifier, which would otherwise resolve to nothing. `GocciaTestRunner` ships a small shim inside the binary and resolves that specifier to it by default, so such a suite runs unchanged. Pass `--no-vitest-compat` to leave the specifier unresolvable.

The shim is a runner default only. It does not follow `goccia:test` to the other binaries: in `GocciaScriptLoader`, `GocciaREPL`, `GocciaSandboxRunner`, and `GocciaBenchmarkRunner` a bare `vitest` import fails to resolve even though `goccia:test` imports fine. Resolving a bare specifier that the host cannot honor is a Vitest-shaped promise, and only the runner is Vitest-shaped. There is no CLI flag or configuration key that turns it on elsewhere; an embedder that wants it installs `TGocciaVitestCompatRuntimeExtension` alongside the testing extension.

The shim re-exports `goccia:test` and adds the `vi` namespace. `vi` exists only in the shim — the engine itself never grows one:

| Member | Behavior |
|---|---|
| `vi.fn` | The engine's `mock` |
| `vi.spyOn` | The engine's `spyOn` |
| `vi.mock` | Supported, **factory form only** (see below) |
| `vi.unmock` | Supported; hoisted with `vi.mock`, last directive in source order wins |
| everything else | Throws, naming the member and why it cannot be honored |

Nothing is a silent no-op. Every member that throws names its own reason rather than a blanket one.

#### `vi.mock` — what is supported

`vi.mock(specifier, factory)` replaces a module for the duration of one test file. The call is **hoisted**: it applies to imports written above it, exactly as in Vitest.

```javascript
import { vi } from "vitest";
import { getRate } from "./rates.js";

vi.mock("./rates.js", () => ({ getRate: () => 42 }));

test("uses the mocked rate", () => {
  expect(getRate()).toBe(42);
});
```

Hoisting works by a pre-pass over the entry test file: before any import is linked, each `vi.mock` specifier is resolved to its absolute address and a generated module is injected into the loader's virtual-module registry, which resolves ahead of files on disk. The factory is inlined into that generated module verbatim, so it runs in its own module scope, lazily, the first time something imports the mocked module.

Three Vitest behaviors follow from that structure rather than from emulation:

- **A factory normally may not reference variables from the test file** — it is in a different module scope, so such a reference is a `ReferenceError`, as in Vitest. The exact behavior depends on source type, and there is one case where this engine is *looser* than Vitest:

  | Source type | Mocked module imported | Factory referencing a test-file `const` |
  |---|---|---|
  | Module (`.mjs`, `--source-type=module`, or `source-type: module`) | statically or dynamically | `ReferenceError` — matches Vitest |
  | Script (plain `.js`, the default) | statically | `ReferenceError` (TDZ: `Cannot access 'x' before initialization`) |
  | Script (plain `.js`, the default) | dynamically, via `await import()` | **Resolves the variable** — looser than Vitest |

  Under script source type the entry file's top-level `const`/`let` are backed by the global scope, so a factory that runs *after* the test body has initialized them — which only happens when the mocked module is reached by a dynamic `import()` — can see them. Do not rely on this: it is an artifact of script-mode scoping, Vitest rejects the same code, and running the suite under module source type turns it back into an error.
- **Factories are lazy.** A factory for a module nothing imports never runs.
- **Isolation is per file.** The runner builds a fresh engine, module loader, and virtual-module registry per test file, so a mock cannot reach another file.
- **A nested `vi.mock` still hoists.** A call inside an `if`, a block, a function body, or a `test()` callback is hoisted and applied, and a warning is written to stderr naming the file and specifier. This matches Vitest, which hoists nested calls, warns, and documents that it will become an error in a future version. Move such a call to the top level.

Matching is by **resolved address**, so a consumer that spells the same file differently still gets the mock, and a mock applies to code under test and to the test file's own import as one shared instance.

#### `vi.mock` — what is not supported

The factory must be a **synchronous arrow function whose body is directly an object literal with plain named properties**. Anything else throws:

| Form | Status |
|---|---|
| `vi.mock(spec, () => ({ a: 1, fn: vi.fn() }))` | Supported |
| `vi.mock(spec)` with no factory (automock) | Throws |
| Spread-based partial mock: `() => ({ ...actual, fn: vi.fn() })` | Throws |
| `async` factory, or one with a block body (`() => { return {...}; }`) | Throws |
| Computed keys, getters, or setters in the returned object | Throws |
| `vi.doMock` / `vi.doUnmock` / `vi.resetModules` | Throws |
| `vi.importActual` / `vi.importMock` / `importOriginal` inside a factory | Throws |
| `vi.mocked` | Throws (type-only helper in Vitest) |
| `vi.hoisted` | Throws |

**These throw on first import of the mocked module, not at the `vi.mock` call.** The `vi.mock` call itself is a no-op by then — the work happened during hoisting — so the error surfaces where the generated module is first evaluated. A mocked module nothing imports never reports its error at all, which matches the laziness Vitest also has.

Export names are read statically from the factory's returned object literal, because the engine resolves named imports at link time and a generated module must declare its exports up front. That is what rules out spread-based partial mocks — a real gap, and the most likely reason an unmodified Vitest suite will not run as-is.

Automock throws because it would have to execute the real module's top-level code and then deep-wrap its exports (functions to spies, classes to mock constructors, arrays to empty arrays); none of that machinery exists yet. `vi.doMock`, `vi.doUnmock`, and `vi.resetModules` throw because they need the module cache mutated after load, which the loader has no eviction path for. `vi.importActual` and `vi.importMock` throw because the registry holds one module per resolved address, so once an address is shadowed the real module is no longer reachable. `vi.hoisted` throws because the factory is relocated into its own module scope, which is precisely what `vi.hoisted` exists to work around.

Further divergences from Vitest worth knowing:

- A missing export is reported **eagerly, at link time**, as `Module "./m.js" has no export named "x"`. Vitest reports it lazily, at property access.
- An **aliased or namespaced callee silently does nothing**: `import { vi as v } from "vitest"; v.mock(...)` is not hoisted and never applies. This is Vitest parity — Vitest's hoist is a syntactic transform that matches only the literal `vi.mock` / `vitest.mock` spellings — but it is silent in both, so prefer the literal spelling.
- A **non-string specifier is skipped**, since the address cannot be resolved before evaluation.
- Vitest silently yields `undefined` for a `var` referenced from a factory; here it is a `ReferenceError`.

#### Why the other members throw

The fake-timer family throws because there is no fake clock — timers run on the real event loop. `vi.stubGlobal` and `vi.stubEnv` throw because globals are not snapshotted, so a stub could not be unwound safely. `vi.clearAllMocks`, `vi.resetAllMocks`, and `vi.restoreAllMocks` throw because no registry of created mocks exists; call `mockClear`, `mockReset`, or `mockRestore` on the mock itself. `vi.resetModules` throws because the loader has no cache-eviction path.

### Writing Cross-Compatible Tests

When writing tests that should pass in both environments, follow these patterns:

**Iterators** --- GocciaScript returns arrays from `Map.keys()`, `Map.values()`, `Map.entries()`, and `Set.values()`, while standard JS returns iterator objects. Wrap calls with spread to normalize:

```javascript
// Works in both GocciaScript and standard JS
expect([...map.keys()]).toEqual(["a", "b", "c"]);
expect([...set.values()]).toEqual([1, 2, 3]);
```

This applies to the iterator-returning methods only. Comparing the collections themselves needs no spreading, since `Map` and `Set` equality is order-insensitive in both environments:

```javascript
expect(map).toEqual(new Map([["a", 1]]));
expect(set).toEqual(new Set([2, 1]));
```

**GocciaScript-specific behaviors** --- Some tests exercise GocciaScript extensions or intentional divergences from the spec (e.g., `Math.clamp`, emoji identifiers, arrow function `this` binding in object methods). These will fail in Vitest since standard JS doesn't support them. This is expected.

### Known Vitest Divergences

| Category | GocciaScript | Standard JS |
|----------|-------------|-------------|
| `Math.clamp` | Supported (TC39 proposal) | Not available |
| Emoji identifiers | Supported | Not supported by V8/Rollup |
| Arrow methods `this` | Binds to owning object | Inherits from enclosing scope |
| Global `parseInt`, `isNaN`, etc. | Available as shims; prefer `Number.*` | Available as global functions |
| `mock()` / `spyOn()` | Standalone globals | `vi.fn()` / `vi.spyOn()` (Vitest) or `jest.fn()` / `jest.spyOn()` (Jest) |
| `vi.mock` factories | Must directly return an object literal; no automock, no spread-based partial mock | Any factory shape; automock and `importOriginal` partial mocks supported |
| Missing export on a mock | Reported eagerly at link time | Reported lazily, at property access |

## Related documents

- [Testing](testing.md) --- test organization, directory layout, running tests, and test principles
- [Built-ins](built-ins.md) --- test assertion built-in reference
