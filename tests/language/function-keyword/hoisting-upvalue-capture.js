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

test("hoisted function captures class declaration", () => {
  function inner() { return new MyClass().value; }
  class MyClass {
    constructor() {
      this.value = 42;
    }
  }
  expect(inner()).toBe(42);
});

test("hoisted function captures enum declaration", () => {
  function inner() { return Color.Red; }
  enum Color { Red = 0, Green = 1, Blue = 2 }
  expect(inner()).toBe(0);
});

test("hoisted function sees destructuring TDZ before initialization", () => {
  {
    function inner() { return value; }
    expect(() => inner()).toThrow(ReferenceError);
    const { value } = { value: 42 };
    expect(inner()).toBe(42);
  }
});

test("hoisted function sees class TDZ before initialization", () => {
  {
    function inner() { return new MyClass().value; }
    expect(() => inner()).toThrow(ReferenceError);
    class MyClass {
      constructor() {
        this.value = 42;
      }
    }
    expect(inner()).toBe(42);
  }
});

test("hoisted function sees enum TDZ before initialization", () => {
  {
    function inner() { return Color.Red; }
    expect(() => inner()).toThrow(ReferenceError);
    enum Color { Red = 0, Green = 1, Blue = 2 }
    expect(inner()).toBe(0);
  }
});

test("switch case hoists function declarations into the case scope", () => {
  let value;

  switch (1) {
    case 1:
      class CaseClass {
        constructor() {
          this.value = 42;
        }
      }
      value = read();
      function read() {
        return new CaseClass().value;
      }
      break;
  }

  expect(value).toBe(42);
});
