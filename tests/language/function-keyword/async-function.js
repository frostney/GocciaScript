/*---
description: Async function declarations and expressions
features: [compat-function]
---*/

test("async function declaration returns a promise", async () => {
  async function fetchValue() {
    return 42;
  }
  const result = await fetchValue();
  expect(result).toBe(42);
});

test("async function expression", async () => {
  const getValue = async function() {
    return "hello";
  };
  const result = await getValue();
  expect(result).toBe("hello");
});

test("async function with await", async () => {
  async function delayed() {
    const value = await Promise.resolve(10);
    return value * 2;
  }
  expect(await delayed()).toBe(20);
});
