/*---
description: Basic function declaration
features: [compat-function]
---*/

test("function declaration with return value", () => {
  function add(a, b) {
    return a + b;
  }
  expect(add(1, 2)).toBe(3);
});

test("function declaration without return", () => {
  let called = false;
  function doSomething() {
    called = true;
  }
  doSomething();
  expect(called).toBe(true);
});

test("function declaration with multiple statements", () => {
  function compute(x) {
    const doubled = x * 2;
    const incremented = doubled + 1;
    return incremented;
  }
  expect(compute(5)).toBe(11);
});
