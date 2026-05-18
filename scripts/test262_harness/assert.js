// test262 sta.js + assert.js + compareArray.js — minimum-divergence adaptation.
//
// This file is loaded as the test262 `assert.js` include and also fills the
// roles of stock `sta.js` (defines Test262Error, $DONOTEVALUATE) and
// `compareArray.js` (assert.compareArray) so the orchestrator only has to
// load one file to get the full assert API.
//
// Aligned to tc39/test262 stock as closely as Goccia permits.  Two
// adaptations:
//
//   1. assert.compareArray uses for-of over .entries() instead of stock's
//      `for (var i = 0; i < a.length; i++) { ... }`. This is a historical
//      adaptation from before the runner enabled traditional for-loop
//      compatibility and should be re-evaluated against stock.
//
//   2. assert.throws uses `error instanceof expectedErrorConstructor`
//      instead of stock's `thrown.constructor !== expectedErrorConstructor`
//      because Goccia's native Error class hierarchy is missing
//      `prototype.constructor` (#519) — caught Errors have
//      e.constructor === undefined.  instanceof walks the prototype chain
//      which IS set up correctly.
//
// Everything else mirrors stock exactly.

function Test262Error(message) {
  this.message = message || "";
}

Test262Error.prototype.toString = function () {
  return "Test262Error: " + this.message;
};

Test262Error.thrower = function (message) {
  throw new Test262Error(message);
};

function $DONOTEVALUATE() {
  throw "Test262: This statement should not be evaluated.";
}

function assert(mustBeTrue, message) {
  if (mustBeTrue === true) {
    return;
  }
  if (message === undefined) {
    message = "Expected true but got " + assert._toString(mustBeTrue);
  }
  throw new Test262Error(message);
}

assert._isSameValue = function (a, b) {
  if (a === b) {
    return a !== 0 || 1 / a === 1 / b;
  }
  return a !== a && b !== b;
};

assert.sameValue = function (actual, expected, message) {
  try {
    if (assert._isSameValue(actual, expected)) {
      return;
    }
  } catch (error) {
    throw new Test262Error(
      (message || "") + " (_isSameValue operation threw) " + error,
    );
  }
  if (message === undefined) {
    message = "";
  } else {
    message += " ";
  }
  message +=
    "Expected SameValue(\u00ab" + assert._toString(actual) +
    "\u00bb, \u00ab" + assert._toString(expected) + "\u00bb) to be true";
  throw new Test262Error(message);
};

assert.notSameValue = function (actual, unexpected, message) {
  if (!assert._isSameValue(actual, unexpected)) {
    return;
  }
  if (message === undefined) {
    message = "";
  } else {
    message += " ";
  }
  message +=
    "Expected SameValue(\u00ab" + assert._toString(actual) +
    "\u00bb, \u00ab" + assert._toString(unexpected) + "\u00bb) to be false";
  throw new Test262Error(message);
};

assert.throws = function (expectedErrorConstructor, func, message) {
  if (typeof func !== "function") {
    throw new Test262Error(
      "assert.throws requires two arguments: the error constructor " +
        "and a function to run",
    );
  }
  if (message === undefined) {
    message = "";
  } else {
    message += " ";
  }
  try {
    func();
  } catch (thrown) {
    // Adaptation: instanceof instead of `thrown.constructor !== ctor`
    // because Error.prototype.constructor is missing in Goccia (#519).
    if (thrown instanceof expectedErrorConstructor) {
      return;
    }
    if (typeof thrown !== "object" || thrown === null) {
      message += "Thrown value was not an object!";
    } else {
      // Try to surface a reasonable name even with #519 in effect.
      var actualName = "(unknown)";
      if (thrown.name) actualName = thrown.name;
      message +=
        "Expected a " + expectedErrorConstructor.name +
        " but got a " + actualName;
    }
    throw new Test262Error(message);
  }
  message +=
    "Expected a " + expectedErrorConstructor.name +
    " to be thrown but no exception was thrown at all";
  throw new Test262Error(message);
};

assert._toString = function (value) {
  try {
    if (value === 0 && 1 / value === -Infinity) {
      return "-0";
    }
    return String(value);
  } catch (err) {
    if (typeof value === "object" || typeof value === "function") {
      return "[" + (typeof value) + "]";
    }
    return "<unprintable>";
  }
};

assert.compareArray = function (actual, expected, message) {
  if (!Array.isArray(actual) || !Array.isArray(expected)) {
    if (message === undefined) message = "";
    throw new Test262Error(
      message + " Expected both values to be arrays",
    );
  }
  if (actual.length !== expected.length) {
    throw new Test262Error(
      (message || "") +
        " Expected array length " + expected.length +
        " but got " + actual.length,
    );
  }
  // Adaptation: for-of over .entries() instead of stock's
  // `for (var i = 0; i < actual.length; i++) { ... }`. This predates
  // traditional for-loop compatibility in the test262 runner.
  for (const [i, val] of actual.entries()) {
    if (!assert._isSameValue(val, expected[i])) {
      throw new Test262Error(
        (message || "") +
          " Expected array element " + i +
          " to be SameValue(" + assert._toString(val) +
          ", " + assert._toString(expected[i]) + ")",
      );
    }
  }
};
