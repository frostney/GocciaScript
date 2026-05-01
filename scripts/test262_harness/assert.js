// test262 assert API -- GocciaScript-compatible reimplementation.
// Assertions throw Test262Error directly because generated wrappers execute
// the conformance body before registering the runner test.

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

const __gocciaTest262String = (value) => {
  try {
    return String(value);
  } catch (error) {
    return "<unprintable>";
  }
};

// --- Core assert ---
const assert = (mustBeTrue, message) => {
  if (mustBeTrue !== true) {
    throw new Test262Error(message || "Expected true but got " + __gocciaTest262String(mustBeTrue));
  }
};

// --- assert.sameValue: SameValue via Object.is() ---
assert.sameValue = (actual, expected, message) => {
  if (!Object.is(actual, expected)) {
    throw new Test262Error(message || "Expected SameValue(" + __gocciaTest262String(actual) + ", " + __gocciaTest262String(expected) + ")");
  }
};

// --- assert.notSameValue ---
assert.notSameValue = (actual, unexpected, message) => {
  if (Object.is(actual, unexpected)) {
    throw new Test262Error(message || "Expected values to differ");
  }
};

// --- assert.throws ---
assert.throws = (expectedErrorConstructor, func, message) => {
  try {
    func();
  } catch (error) {
    if (error instanceof expectedErrorConstructor) {
      return;
    }
    throw new Test262Error(message || "Expected " + expectedErrorConstructor.name + " but got " + error);
  }
  throw new Test262Error(message || "Expected " + expectedErrorConstructor.name + " to be thrown");
};

// --- assert.compareArray ---
assert.compareArray = (actual, expected, message) => {
  if (!Array.isArray(actual) || !Array.isArray(expected)) {
    throw new Test262Error(message || "Expected both values to be arrays");
  }
  if (actual.length !== expected.length) {
    throw new Test262Error(message || "Expected array length " + expected.length + " but got " + actual.length);
  }
  const entries = actual.entries();
  for (const [i, val] of entries) {
    if (!Object.is(val, expected[i])) {
      throw new Test262Error(message || "Expected array element " + i + " to be SameValue");
    }
  }
};
