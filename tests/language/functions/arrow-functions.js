/*---
description: Arrow functions work correctly
features: [arrow-functions]
---*/

test("basic arrow function", () => {
  const square = (x) => x * x;
  expect(square(4)).toBe(16);
});

test("arrow function with no parameters", () => {
  const getValue = () => 42;
  expect(getValue()).toBe(42);
});

test("arrow function with multiple parameters", () => {
  const add = (a, b) => a + b;
  expect(add(3, 7)).toBe(10);
});

runTests();
