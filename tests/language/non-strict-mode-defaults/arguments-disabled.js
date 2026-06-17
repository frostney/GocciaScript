/*---
description: The implicit arguments object requires --compat-arguments-object
features: [compat-function, unsafe-function-constructor]
---*/

test("ordinary functions do not create arguments by default", () => {
  function count() {
    return typeof arguments;
  }

  expect(count(1, 2)).toBe("undefined");
});

test("arguments remains restricted as a strict binding name", () => {
  expect(() => {
    Function("\"use strict\"; function echo(arguments) { return arguments; }");
  }).toThrow(SyntaxError);
});

test("delete non-configurable properties throws by default", () => {
  const obj = {};
  Object.defineProperty(obj, "fixed", {
    value: 1,
    configurable: false
  });

  expect(() => {
    delete obj.fixed;
  }).toThrow(TypeError);
});

test("ordinary function calls keep strict this by default", () => {
  function f() {
    return this;
  }

  expect(f()).toBeUndefined();
  expect(f.call(null)).toBeNull();
});
