// test262 deepEqual.js -- GocciaScript-compatible reimplementation
// Provides deep structural equality comparison.

const deepEqual = (a, b) => {
  if (a === b) {
    return true;
  }

  // NaN check
  if (a !== a && b !== b) {
    return true;
  }

  if (a === null || b === null) {
    return false;
  }
  if (a === undefined || b === undefined) {
    return false;
  }
  if (typeof a !== typeof b) {
    return false;
  }

  if (typeof a !== "object") {
    return false;
  }

  if (Array.isArray(a)) {
    if (!Array.isArray(b)) {
      return false;
    }
    if (a.length !== b.length) {
      return false;
    }
    const entries = a.entries();
    for (const [i, val] of entries) {
      if (!deepEqual(val, b[i])) {
        return false;
      }
    }
    return true;
  }

  const keysA = Object.keys(a);
  const keysB = Object.keys(b);
  if (keysA.length !== keysB.length) {
    return false;
  }

  for (const key of keysA) {
    if (!Object.prototype.hasOwnProperty.call(b, key)) {
      return false;
    }
    if (!deepEqual(a[key], b[key])) {
      return false;
    }
  }
  return true;
};

assert.deepEqual = (actual, expected, message) => {
  if (!deepEqual(actual, expected)) {
    throw new Test262Error(
      (message ? message + " -- " : "") +
      "Expected deep equality"
    );
  }
};
