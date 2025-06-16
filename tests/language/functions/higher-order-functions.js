/*---
description: Higher-order functions work correctly
features: [higher-order-functions]
---*/

test("function returning function", () => {
  const createMultiplier = (factor) => {
    return (value) => value * factor;
  };

  const double = createMultiplier(2);
  const triple = createMultiplier(3);

  expect(double(5)).toBe(10);
  expect(triple(4)).toBe(12);
});

test("function accepting function as parameter", () => {
  const applyOperation = (a, b, operation) => operation(a, b);

  const add = (x, y) => x + y;
  const multiply = (x, y) => x * y;

  expect(applyOperation(3, 4, add)).toBe(7);
  expect(applyOperation(3, 4, multiply)).toBe(12);
});

test("function composition", () => {
  const compose = (f, g) => (x) => f(g(x));

  const addOne = (x) => x + 1;
  const double = (x) => x * 2;
  const addOneThenDouble = compose(double, addOne);

  expect(addOneThenDouble(3)).toBe(8); // (3 + 1) * 2 = 8
});
