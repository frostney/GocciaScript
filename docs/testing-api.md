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

The module namespace and the globals install independently. A host that applies the loader runtime profile in its default configuration — `ApplyLoaderRuntimeProfile(ARuntime)`, i.e. `ATestingModule = True` — registers `goccia:test`; only the runner adds the globals on top. `ApplyLoaderRuntimeProfile(ARuntime, False)` applies the rest of the profile with the `goccia:test` registration suppressed, which is what `GocciaTestRunner` does so its own `TGocciaTestingLibraryRuntimeExtension` owns the single registration.

| Binary | `goccia:test` | Testing globals |
|---|---|---|
| `GocciaTestRunner` | Yes | Yes |
| `GocciaScriptLoader` | Yes | No |
| `GocciaREPL` | Yes | No |
| `GocciaSandboxRunner` | Yes | No |
| `GocciaBenchmarkRunner` | Yes | No |
| `GocciaScriptLoaderBare` | No — attaches no runtime | No |

The [timer surface](#fake-timers) — `goccia:timers` and the `setTimeout` family — is narrower still: `GocciaTestRunner` only. The timers are deterministic and carry no ambient authority, but a scheduling surface is one a sandboxed script does not otherwise get, and the acceptance target for them is the runner. An embedder that wants them installs `TGocciaTimersRuntimeExtension`.

An embedder gets the same split: `ApplyLoaderRuntimeProfile` with its default `ATestingModule = True` registers the module only, and `TGocciaTestingLibraryRuntimeExtension.CreateModuleOnly` is the direct spelling for hosts that assemble their own profile. Passing `AInjectGlobals = True` to that extension's ordinary constructor is what makes the globals appear, and the runner is the only host that does it.

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

`.toHaveProperty()` accepts a path rather than a flat key. The whole path is first tried as a *literal own key*, and only split into segments if no such key exists — so `expect({ "a.b": 5 }).toHaveProperty("a.b")` passes. That check runs ahead of the walk rather than as a fallback, so on `{ "a.b": 5, a: { b: 9 } }` the path reads `5`, never `9`. It considers own properties only (an inherited `"a.b"` is not found this way) but ignores enumerability, and it applies to the whole path only — a dotted key nested deeper still needs an array path (`["a", "b.c"]`).

Otherwise a string path splits on dots and understands bracket indices, so `"items[0].type"` and `"a.b"` both walk into nested values; an array path (`["a", "b", 0, "c"]`) takes each segment verbatim, which is also how you reach a key that itself contains a dot. Empty segments are dropped, so `"a."`, `".a"` and `"a..b"` mean `"a"`, `"a"` and `"a.b"`; an empty-string key is therefore reachable only as a literal whole path (`toHaveProperty("", 1)`) or through an array path. A number is accepted as a path and reaches an index or numeric key through the literal check. Each segment resolves the way a normal property read would, so inherited members and the members of a boxed primitive (`"name.length"`) are found. A second argument is compared with `.toEqual()` semantics. Walking into `null` or `undefined` reports a normal assertion failure rather than raising.

Two smaller divergences fall out of that, both in goccia's favour, and both where Vitest crashes rather than answering. A path of nothing but separators (`"."`), and a number that names no own key (`toHaveProperty(7)` on a two-element array), each make Vitest throw a `TypeError` out of its path parser; goccia reports the same verdict as an ordinary assertion failure instead.

`.toMatchObject()` matches a subset of keys at every level and recurses per index through arrays, so `{ list: [{ a: 1, b: 2 }] }` matches `{ list: [{ a: 1 }] }`. Arrays must still match in length. An expected plain object may describe an array (`{ l: { 0: 1 } }` matches `{ l: [1] }`), but an expected array only matches an array.

A second deliberate divergence lives here, alongside the `DOMException` one: a **directly cyclic array**. Vitest's top-level array walk has no cycle guard, so `expect(a).toMatchObject(b)` with `a[0] === a` and `b[0] === b` dies with `RangeError: Maximum call stack size exceeded` — in both polarities, so there is no verdict to copy. Only that shape is affected; the same cyclic array nested one level inside an object matches cleanly in Vitest, as do cyclic objects, `Set`s and `Map`s. Goccia terminates and reports a match, which is exactly what Vitest itself answers whenever it completes. Reproducing a stack overflow to stay bug-compatible would be strictly worse for users, so this stands.

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

A Promise a timer will settle also works, and settles instantly: the [virtual timer queue](#fake-timers) is run to the next due timer wherever the engine would otherwise be waiting, so `await new Promise((r) => setTimeout(r, 5000))` returns without 5000 milliseconds passing. Under `vi.useFakeTimers()` it does not — the suite owns the clock there, and the test advances it explicitly.

**Important:** If a test returns a Promise that is still pending after the microtask queue drains, all pending fetch completions have been pumped, and every real-mode timer has run, the test **fails** with "Promise still pending after microtask drain". Since GocciaScript has no event loop beyond those sources, such a Promise will never settle --- this catches tests with missing assertions or broken async chains. This mirrors how Jest/Vitest fail tests with a timeout when the returned Promise never resolves.

When a returned Promise rejects, the failure line reports the reason as `Returned Promise rejected: <reason>`. An `Error` is named and described --- `Error: boom`, or the class name for a subclass that does not set its own `name`, such as `MyError: boom` --- because its `name` lives on the prototype and its `message` is non-enumerable, so serializing the object alone would render it as `{}`. Any other reason is serialized as a value.

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

The target for these is [Vitest's `skipIf`/`runIf`](https://vitest.dev/api/#test-skipif). When skipped, tests are counted in the total but not executed and reported as skipped. The exact accounting of the conditional and `todo` forms has not been through the differential lane yet — the lifecycle differential suite covers hook and describe accounting, not these — so treat the details here as intent rather than as verified parity.

## Cross-Runtime Compatibility (Vitest)

[Vitest](https://vitest.dev/) is the compatibility target, not merely a second place the tests happen to run: the testing API aims at being an exact drop-in, so where this engine and Vitest disagree about a matcher, a hook, or how a result is counted, Vitest is right by definition and the difference is a defect unless it is written down here as deliberate. Tests that pass in both therefore serve as GocciaScript validation and as ECMAScript conformance checks at once.

That reference covers what the matchers *mean*, not only the API shape: equality, `Set`/`Map`, error, and `toThrow` behavior is probed against a pinned Vitest release and reconciled. Accepting unmodified Vitest suites is a stated project direction rather than a current guarantee — the divergences listed below are the remaining gap. See [Vision](../VISION.md) for where this is heading.

Bun runs the same tests far faster and is used that way as a proxy, but it is advisory only. A three-way audit of 223 probes found bun and Vitest disagreeing on 30 of 178 matcher probes, in both directions, so bun agreeing is evidence and bun disagreeing is a question — neither is a verdict. [Differential Testing](differential-testing.md) describes how the two roles are enforced per differential suite.

### Running with Vitest

```bash
npx vitest run                    # Run all tests
npx vitest run tests/built-ins/   # Run a category
npx vitest                        # Watch mode
```

Running `tests/` under Vitest needs a local Vitest install and a config that points at those files; the repository pins its own Vitest only for the differential suite lane, under `scripts/differential/`.

### The `vitest` compatibility shim

A suite written against Vitest imports from a bare `vitest` specifier, which would otherwise resolve to nothing. `GocciaTestRunner` ships a small shim inside the binary and resolves that specifier to it by default, so such a suite runs unchanged. Pass `--no-vitest-compat` to leave the specifier unresolvable.

The shim is a runner default only. It does not follow `goccia:test` to the other binaries: in `GocciaScriptLoader`, `GocciaREPL`, `GocciaSandboxRunner`, and `GocciaBenchmarkRunner` a bare `vitest` import fails to resolve even though `goccia:test` imports fine. Resolving a bare specifier that the host cannot honor is a Vitest-shaped promise, and only the runner is Vitest-shaped. There is no CLI flag or configuration key that turns it on elsewhere; an embedder that wants it installs `TGocciaVitestCompatRuntimeExtension` alongside the testing extension. The shim statically imports `goccia:timers`, so `TGocciaTimersRuntimeExtension` is a hard prerequisite: without it every `vitest` import fails to resolve, not only the `vi` timer members. Install the timers extension whenever the compat shim is installed (the runner always does).

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
- **A nested `vi.mock` still hoists, wherever it is written.** The hoister walks the whole file, so a call inside an `if`, a block, a function body, a `test()` callback, a call *callee*, a conditional, an array element, an object property, a class method, or a class static block is hoisted and applied — and a warning is written to stderr naming the file and specifier. This matches Vitest, which walks the whole AST, hoists nested calls, warns, and documents that it will become an error in a future version. Move such a call to the top level.

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
| A key that is not usable as an export name, including reserved words: `() => ({ class: 1 })` | Throws (`default` is the exception — it becomes the default export) |
| `vi.doMock` / `vi.doUnmock` / `vi.resetModules` | Throws |
| `vi.importActual` / `vi.importMock` / `importOriginal` inside a factory | Throws |
| `vi.hoisted` | Throws |

**These throw on first import of the mocked module, not at the `vi.mock` call.** The `vi.mock` call itself is a no-op by then — the work happened during hoisting — so the error surfaces where the generated module is first evaluated. A mocked module nothing imports never reports its error at all, which matches the laziness Vitest also has.

Export names are read statically from the factory's returned object literal, because the engine resolves named imports at link time and a generated module must declare its exports up front. That is what rules out spread-based partial mocks — a real gap, and the most likely reason an unmodified Vitest suite will not run as-is.

Automock throws because it would have to execute the real module's top-level code and then deep-wrap its exports (functions to spies, classes to mock constructors, arrays to empty arrays); none of that machinery exists yet. `vi.doMock`, `vi.doUnmock`, and `vi.resetModules` throw because they need the module cache mutated after load, which the loader has no eviction path for. `vi.importActual` and `vi.importMock` throw because the registry holds one module per resolved address, so once an address is shadowed the real module is no longer reachable. `vi.hoisted` throws because the factory is relocated into its own module scope, which is precisely what `vi.hoisted` exists to work around.

Further divergences from Vitest worth knowing:

- A missing export is reported **eagerly, at link time**, as `Module "./m.js" has no export named "x"`. Vitest reports it lazily, at property access.
- An **aliased or namespaced callee silently does nothing**: `import { vi as v } from "vitest"; v.mock(...)` is not hoisted and never applies. This is Vitest parity — Vitest's hoist is a syntactic transform that matches only the literal `vi.mock` / `vitest.mock` spellings — but it is silent in both, so prefer the literal spelling.
- A **non-string specifier is skipped**, since the address cannot be resolved before evaluation.
- Vitest silently yields `undefined` for a `var` referenced from a factory; here it is a `ReferenceError`.

#### The rest of the `vi` namespace

| Member | Status |
|---|---|
| `vi.fn`, `vi.spyOn` | Supported — the engine's `mock` and `spyOn`, registered so the bulk members below can drain them |
| `vi.mocked` | Supported — the identity function it is in Vitest |
| `vi.stubGlobal`, `vi.unstubAllGlobals` | Supported |
| `vi.stubEnv`, `vi.unstubAllEnvs` | Supported, over an injected `process.env` |
| `vi.clearAllMocks`, `vi.resetAllMocks`, `vi.restoreAllMocks` | Supported |
| The fake-timer family | Supported — see [Fake timers](#fake-timers) |
| `vi.waitFor`, `vi.waitUntil` | Throws — async polling, which needs a suspension point |
| `vi.hoisted` | Throws |
| `vi.doMock`, `vi.doUnmock`, `vi.resetModules` | Throws |
| `vi.importActual`, `vi.importMock` | Throws |
| `vi.setConfig`, `vi.resetConfig` | Throws |

`vi.stubGlobal` records the value a name held before its **first** stub, so restubbing the same name repeatedly still unwinds to the original, and `vi.unstubAllGlobals` deletes a name that did not exist rather than leaving it behind as an undefined global. `vi.stubEnv` and `vi.unstubAllEnvs` behave the same way over `process.env`, and match Vitest's remaining details: the value is coerced with `String()`, and an `undefined` value deletes the variable instead of setting it.

#### Fake timers

`GocciaTestRunner` provides `setTimeout`, `clearTimeout`, `setInterval` and `clearInterval` over a **virtual timer queue**, and the whole Vitest fake-timer family on top of it:

| Member | Notes |
|---|---|
| `vi.useFakeTimers([config])` | Only `config.now` is read; see below |
| `vi.useRealTimers()`, `vi.isFakeTimers()` | |
| `vi.setSystemTime(dateOrMs)`, `vi.getMockedSystemTime()`, `vi.getRealSystemTime()` | |
| `vi.advanceTimersByTime(ms)` / `vi.advanceTimersByTimeAsync(ms)` | |
| `vi.advanceTimersToNextTimer()` / `vi.advanceTimersToNextTimerAsync()` | |
| `vi.runAllTimers()` / `vi.runAllTimersAsync()` | |
| `vi.runOnlyPendingTimers()` / `vi.runOnlyPendingTimersAsync()` | |
| `vi.getTimerCount()`, `vi.clearAllTimers()` | |
| `vi.advanceTimersToNextFrame`, `vi.runAllTicks` | Throw — no `requestAnimationFrame`, and no `process.nextTick` queue (promise jobs run on the engine microtask queue, which the `…Async` members already drain) |
| `vi.setTimerTickMode` | `"manual"` is accepted and does nothing — it names the only behaviour there is. Every other mode throws: they advance the clock against real elapsed time, which no GocciaScript clock measures |

Every implemented member returns `vi`, so calls chain; the three that throw are listed in the last row above. The semantics were probed against the pinned Vitest 4.1.10 — whose fake timers wrap `@sinonjs/fake-timers` — rather than read off its documentation, and are locked in by a [vitest-gated differential suite](differential-testing.md). The details worth knowing:

- **`advanceTimersByTime` runs no microtasks between timers.** A promise callback a timer queued waits until the advance returns. The `…Async` variants drain the microtask queue before the first timer and again after each one, which is the ordering a suite awaiting between ticks depends on.
- **Timers due at the same instant fire in registration order.** Ties break on creation time, then on id.
- **A zero-delay timer scheduled from inside a running timer is due one virtual millisecond later**, not at the current instant. That is what keeps a `setTimeout(f, 0)` chain from looping forever inside one advance.
- **An interval reschedules from its previous due time**, so it does not drift, and one advance fires every tick it crossed.
- **`runAllTimers` gives up after 10000 timers** with `Aborting after running 10000 timers, assuming an infinite loop!`.
- **`runOnlyPendingTimers` ticks to the latest due time among the timers pending when it was called** — so a timer one of them schedules inside that window still fires, and one scheduled beyond it stays pending.
- **A throwing timer callback stops the run only for some members.** Under `advanceTimersByTime` and `runOnlyPendingTimers` the first exception is recorded, the remaining timers still run, the clock reaches the instant it was asked for, and the error is rethrown when the advance ends. Under `runAllTimers` and `advanceTimersToNextTimer` it stops there and everything behind it stays pending. The three genuinely differ in Vitest, and each was probed on its own.
- **`setSystemTime` moves the wall clock without letting time pass.** Every pending timer keeps its remaining delay, forwards and backwards. It takes a number, a `Date`, or **anything else `new Date(...)` accepts** — a date string included.
- **A fractional delay is truncated; a fractional advance is banked.** `setTimeout(fn, 1.5)` is due at 1ms, but `advanceTimersByTime(1.5)` followed by `advanceTimersByTime(0.5)` moves the clock by a full 2ms and fires a timer due there. Both halves are Vitest's, and the pairing is unintuitive enough to be worth stating.
- **`Date`, `new Date()`, `Temporal.Now` and `performance.now()` all follow the mocked clock**, because it is installed on the [host environment](host-environment.md) rather than patched onto a global. `performance.now()` reports elapsed *virtual* time from the moment fake timers were installed, and a `setSystemTime` jump does not move it.
- **`vi.setSystemTime` works without `vi.useFakeTimers`**, freezing `Date` only and leaving timers and monotonic time alone.
- **Fake-timer state is not reset between tests.** Vitest leaves the clock installed for the rest of the file, and so does GocciaScript; each test file gets a fresh engine, so nothing leaks across files.
- **`AbortSignal.timeout()` is not faked.** It runs on the infrastructure monotonic clock, so advancing the virtual clock will not fire it. That is parity, not a gap: Vitest does not fake it either, because it is not one of the globals its clock replaces. A suite that needs an abort under fake timers should drive an `AbortController` from a timer callback instead.

Four shapes deliberately diverge from Vitest:

- **A timer id is a number**, as it is on the web. Vitest runs in Node, where the fake clock hands back a `Timeout` object with `ref`/`unref`/`refresh`; GocciaScript has no Node timer object to imitate and no event loop for those methods to mean anything to. `clearTimeout` takes either, which is what suites actually depend on.
- **`vi.useFakeTimers(config)` honours only `now`.** `toFake` has nothing to select from — there is one timer queue and it is always the faked one — and `shouldAdvanceTime` / `advanceTimeDelta` describe real elapsed time, which no GocciaScript clock measures. Both are ignored rather than rejected, so a suite that passes them still runs.
- **A non-finite system time is refused.** `vi.setSystemTime(NaN)`, an out-of-range date, or a string `Date` cannot parse, all throw a `TypeError`. Vitest admits them and leaves `Date.now()` reporting `NaN`; here the mocked clock reaches JavaScript as an integer nanosecond count on the [host environment](host-environment.md), so there is nothing for a `NaN` to be, and every consumer of the virtual clock quietly stops working once one is admitted.
- **An advance that can never finish is aborted.** A `setInterval` with a period of `0` re-arms at the instant it just ran, so the clock can never move past it; Vitest hangs forever on that shape and GocciaScript throws instead. Short of that the behaviour matches: every tick lands on the same instant, and the advance still finishes where it was asked to.

##### Without fake timers

The queue is still virtual when `vi.useFakeTimers()` was never called: no wall time passes, and the clock jumps to the next timer's due time whenever the engine would otherwise have nothing left to do. In practice that means an `await` on a promise a timer will settle:

```javascript
test("a timer settles the awaited promise", async () => {
  const value = await new Promise((resolve) => setTimeout(() => resolve(42), 5000));
  expect(value).toBe(42); // instantly — only the virtual clock moved
});
```

A delay is therefore an ordering key, not a duration. A timer-driven suite runs at full speed and reproducibly, and an uncleared `setInterval` cannot hang the run: the drains skip intervals entirely, and every one of them is bounded.

Four more rules make real mode predictable:

- **Timers a test body schedules run at the end of that test**, not at some later idle point. A `setTimeout` written inside a `test()` fires once the body has returned, before the next test starts.
- **Whatever is left over is dropped when the test ends.** An uncleared interval, or a chain longer than the drain reached, cannot fire inside the next test. Fake-timer state is untouched by this — that queue belongs to the suite.
- **A timer callback that throws fails the test that scheduled it**, and nothing else: it is reported as `uncaught exception in a timer callback`. It is not delivered to whatever frame happened to be awaiting, so a `try`/`catch` around an unrelated `await` will not see it and that `await` still resolves normally. This is Node's shape — an uncaught top-level error — rather than an exception at the wait.
- **Real outstanding work outranks virtual time.** While a `fetch` or an `Atomics.waitAsync` is still in flight, no timer runs. Without that rule `Promise.race([fetch(url), timeoutAfter(ms)])` resolved to the timeout every time, whatever `ms` was, because the virtual clock costs nothing to advance.

`performance.now()` is worth one note of its own: while timers are faked it measures elapsed virtual time from the install, so it is not on the same timeline as `performance.timeOrigin` (which keeps reporting the real process origin). Leaving fake timers puts it back on the real timeline.

##### `goccia:timers`

The engine surface underneath is importable on its own, for a suite that does not use the Vitest shim:

```javascript
import { useFakeTimers, advanceTimersByTime, useRealTimers } from "goccia:timers";
```

It exports the same operations plus the four timer globals, but speaks in epoch milliseconds rather than `Date` objects and returns `undefined` rather than chaining. Wrapping that in Vitest's shapes is exactly what the `vi` members do.

#### `process.env`

GocciaScript has no `process`. `vi.stubEnv` writes to whatever one the host injected, so a suite that needs it supplies it — the same `--global` and `--globals` options the loader has, now on `GocciaTestRunner` too:

```bash
./build/GocciaTestRunner suite.test.ts --global 'process={"env":{}}'
./build/GocciaTestRunner suite.test.ts --globals=env.json
```

`--globals` takes JSON, JSON5, TOML or YAML, so a file of preset variables works as well as an empty object:

```json
{ "process": { "env": { "API_BASE": "http://localhost" } } }
```

Globals are injected before the runtime extensions attach, so a module that reads `process.env` at import time sees it wherever `vitest` sits in the import order. A test file can also just define `globalThis.process = { env: {} }` itself.

Nothing is inherited from the machine's real environment. A suite reading a variable it never stubbed sees the same thing everywhere, and the engine reads no ambient process state — wall time, time zone and randomness all arrive through an [injected host environment](host-environment.md) too.

With no `process` at all, `vi.stubEnv` throws and names the two options rather than silently doing nothing.

#### Why the other members throw

`vi.waitFor` and `vi.waitUntil` throw even though fake timers now exist, because a fake clock is not what they need. They are async polling APIs: each retries its callback on an interval until it passes or a timeout elapses, which needs execution to suspend and resume between attempts. GocciaScript's runner has no such primitive — `await` is a synchronous drain, as [Async Tests](#async-tests-promises) describes, and the virtual timer queue only moves when a test moves it — so a poll loop would spin without anything ever being able to change the condition. Both members report that reason by name.

`vi.resetModules` throws because the loader has no cache-eviction path.

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

**Async context** --- A suite whose library keeps per-request state in `AsyncLocalStorage` runs unchanged: `node:async_hooks` is present in `GocciaTestRunner`, and the engine propagates the context across `await` and every promise-reaction continuation. See [Async Context](built-ins-async-context.md) for the surface and the two things that are out of scope (host-scheduled callbacks and the `async_hooks` observer API).

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
| `process` | Not provided; inject one with `--global` / `--globals` when a suite needs it | The real process environment and the rest of the Node `process` API |
| `import.meta.env` | Not available; `vi.stubEnv` writes to `process.env` | Vite populates it, and `vi.stubEnv` writes there |
| Timer ids | Numbers, as on the web | Node `Timeout` objects with `ref`/`unref`/`refresh` |
| Timers without `vi.useFakeTimers()` | Virtual: the clock jumps to the next due timer when the engine would otherwise wait, so a delay is an ordering key rather than a duration | Real elapsed time on the event loop |
| 12-hour `Intl` time separator | U+202F (narrow no-break space) before AM/PM | U+0020 in Node 24 (ICU 77.1) and bun 1.3 |

One of those rows is worth expanding, because it cost a debugging session before it was written down:

**The 12-hour time separator** is a data difference, not a formatting one. GocciaScript's `Intl` data is generated from CLDR 45, whose `en` time patterns put U+202F (narrow no-break space) before the day period; the ICU that Node 24 and bun 1.3 ship emits U+0020 for the same pattern. It applies to every 12-hour format — `timeStyle: "short"`, an explicit `hour`/`minute` skeleton, and `toLocaleTimeString` alike. A test written as `/^\d{2}:\d{2} (AM|PM)$/` therefore fails against `06:04 AM` for a reason nothing in the output shows. Match with `\s` or a character class covering both code points rather than a literal space. This is a deliberate divergence and stands: the pinned CLDR release is the source of truth for the data.

## Related documents

- [Testing](testing.md) --- test organization, directory layout, running tests, and test principles
- [Built-ins](built-ins.md) --- test assertion built-in reference
- [Async Context](built-ins-async-context.md) --- `node:async_hooks` for suites whose code under test carries per-request state
