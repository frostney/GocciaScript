// test262 assert API -- GocciaScript-compatible reimplementation
// Bridges test262 assertions to Goccia TestAssertions (expect()).  Both
// expect().toBe() and Object.is() use SameValue semantics natively, so
// NaN === NaN and +0/-0 edges are handled by the engine.
//
// Goccia TestAssertions integration:
//   - assert.throws  --> expect(fn).toThrow(ErrorConstructor)
//   - assert         --> expect(val).toBe(true)
//   - describe/test  --> wrapping provided by the runner

class Test262Error {
  constructor(message) {
    this.message = message || "";
    this.name = "Test262Error";
  }

  toString() {
    return "Test262Error: " + this.message;
  }
}

const $DONOTEVALUATE = () => {
  throw new Test262Error("Test262: This statement should not be evaluated.");
};

const $ERROR = (message) => {
  throw new Test262Error(message);
};

// --- Core assert: bridges to expect().toBe(true) ---
const assert = (mustBeTrue, message) => {
  expect(mustBeTrue).toBe(true);
};

// --- assert.sameValue: SameValue via expect().toBe() (native IsSameValue) ---
assert.sameValue = (actual, expected, message) => {
  expect(actual).toBe(expected);
};

// --- assert.notSameValue ---
assert.notSameValue = (actual, unexpected, message) => {
  expect(actual).not.toBe(unexpected);
};

// --- assert.throws: bridges to expect(fn).toThrow(ErrorConstructor) ---
assert.throws = (expectedErrorConstructor, func, message) => {
  expect(func).toThrow(expectedErrorConstructor);
};

// --- assert.compareArray: uses expect() for length and per-element SameValue ---
assert.compareArray = (actual, expected, message) => {
  expect(Array.isArray(actual)).toBe(true);
  expect(Array.isArray(expected)).toBe(true);
  expect(actual.length).toBe(expected.length);
  const entries = actual.entries();
  for (const [i, val] of entries) {
    expect(val).toBe(expected[i]);
  }
};
