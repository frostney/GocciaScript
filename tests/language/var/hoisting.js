/*---
description: var declarations are hoisted - accessible before declaration as undefined
features: [compat-var]
---*/

test("var is accessible before declaration (hoisted as undefined)", () => {
  const fn = () => {
    const before = x;
    var x = 10;
    return before;
  };
  expect(fn()).toBeUndefined();
});

test("hoisted var gets assigned at declaration site", () => {
  const fn = () => {
    const before = y;
    var y = 42;
    return [before, y];
  };
  const result = fn();
  expect(result[0]).toBeUndefined();
  expect(result[1]).toBe(42);
});

// Module-scope hoisting: var is hoisted to file scope
const beforeHoistedVar = hoistedVar;
var hoistedVar = "assigned";

test("var hoisted at module top level", () => {
  expect(beforeHoistedVar).toBeUndefined();
  expect(hoistedVar).toBe("assigned");
});
