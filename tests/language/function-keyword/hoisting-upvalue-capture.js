/*---
description: Hoisted function declarations correctly capture later-declared lexical bindings
features: [compat-function]
---*/

test("hoisted function captures array-destructured variable", () => {
  function inner() { return a; }
  const [a, b] = [42, 99];
  expect(inner()).toBe(42);
});

test("hoisted function captures object-destructured variable", () => {
  function inner() { return x + y; }
  const { x, y } = { x: 10, y: 20 };
  expect(inner()).toBe(30);
});

test("hoisted function captures nested destructured variable", () => {
  function inner() { return z; }
  const { a: { b: z } } = { a: { b: 7 } };
  expect(inner()).toBe(7);
});

test("hoisted function captures rest-destructured variable", () => {
  function inner() { return rest; }
  const [first, ...rest] = [1, 2, 3];
  expect(inner()).toEqual([2, 3]);
});

test("hoisted function captures destructured with default", () => {
  function inner() { return val; }
  const { val = 55 } = {};
  expect(inner()).toBe(55);
});
