/*---
description: Anonymous function expressions
features: [compat-function]
---*/

test("anonymous function expression assigned to variable", () => {
  const multiply = function(a, b) {
    return a * b;
  };
  expect(multiply(3, 4)).toBe(12);
});

test("function expression as callback", () => {
  const arr = [1, 2, 3];
  const doubled = arr.map(function(x) {
    return x * 2;
  });
  expect(doubled).toEqual([2, 4, 6]);
});

test("immediately invoked function expression", () => {
  const result = (function() {
    return 42;
  })();
  expect(result).toBe(42);
});
