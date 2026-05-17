/*---
description: Use Strict directives override non-strict compatibility semantics
features: [compat-non-strict-mode]
---*/

"use strict";

test("top-level use strict keeps regular function this strict", () => {
  function f() {
    return this;
  }

  expect(f()).toBeUndefined();
  expect(f.call(undefined)).toBeUndefined();
  expect(f.call(null)).toBeNull();
});

test("top-level use strict keeps failed property assignments strict", () => {
  const obj = {};
  Object.defineProperty(obj, "fixed", {
    value: 1,
    writable: false,
    configurable: true,
  });

  expect(() => {
    obj.fixed = 2;
  }).toThrow(TypeError);
  expect(obj.fixed).toBe(1);
});
