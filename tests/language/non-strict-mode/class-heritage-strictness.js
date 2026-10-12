/*---
description: A class heritage expression is strict code, and the code compiled after it keeps its own mode
features: [compat-non-strict-mode]
---*/

// ES2026 §11.2.2: all parts of a ClassDeclaration or ClassExpression,
// its ClassHeritage included, are strict mode code.
test("a function in a class heritage expression is strict", () => {
  class Derived extends (function () { return this === undefined ? Object : Array; })() {}
  expect(Object.getPrototypeOf(Derived)).toBe(Object);
});

test("sloppy code after a class heritage in a strict function stays sloppy", () => {
  function strictFunction() {
    "use strict";
    class Derived extends Object {}
    return Derived;
  }
  strictFunction();
  function sloppyThis() {
    return this;
  }
  expect(sloppyThis()).toBe(globalThis);
});

test("sloppy code after a class heritage in sloppy code stays sloppy", () => {
  class Derived extends Object {}
  function sloppyThis() {
    return this;
  }
  expect(Object.getPrototypeOf(Derived)).toBe(Object);
  expect(sloppyThis()).toBe(globalThis);
});
