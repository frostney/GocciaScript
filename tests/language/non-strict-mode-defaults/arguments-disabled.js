/*---
description: The implicit arguments object requires --compat-arguments-object
features: [compat-function]
---*/

test("ordinary functions do not create arguments by default", () => {
  function count() {
    return typeof arguments;
  }

  expect(count(1, 2)).toBe("undefined");
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
