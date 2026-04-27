/*---
description: Generator function declarations and expressions
features: [compat-function, generators]
---*/

test("function* declaration yields values", () => {
  function* numbers() {
    yield 1;
    yield 2;
    return 3;
  }

  const iter = numbers();
  expect(iter.next()).toEqual({ value: 1, done: false });
  expect(iter.next()).toEqual({ value: 2, done: false });
  expect(iter.next()).toEqual({ value: 3, done: true });
});

test("function* expression yields values", () => {
  const numbers = function* () {
    yield 4;
  };

  expect(numbers().next()).toEqual({ value: 4, done: false });
});

test("named function* expression exposes its name internally", () => {
  const make = function* named() {
    yield named.name;
  };

  expect(make().next()).toEqual({ value: "named", done: false });
});

