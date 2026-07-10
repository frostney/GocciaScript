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

test("named async function expression source text starts at async keyword", () => {
  const source = (async function getValue(value) {
    return value;
  }).toString();

  expect(source.startsWith("async function getValue(value)")).toBe(true);
  expect(source.includes("return value")).toBe(true);
});

test("contextual keyword can name an async function expression", async () => {
  const f = async function from(value) {
    return value;
  };

  expect(await f(42)).toBe(42);
  expect(f.name).toBe("from");
});

test("async function with await", async () => {
  async function delayed() {
    const value = await Promise.resolve(10);
    return value * 2;
  }
  expect(await delayed()).toBe(20);
});

test("async function is not constructable", () => {
  const make = async function() {
    return 1;
  };

  expect(() => new make()).toThrow(TypeError);
});
