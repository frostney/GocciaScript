test("basic function composition", () => {
  const add = (a, b) => a + b;
  const double = (x) => x * 2;

  const addThenDouble = (a, b) => double(add(a, b));
  expect(addThenDouble(1, 2)).toBe(6);
});

test("nested arrow functions and complex scenarios", () => {
  const createMultiplier = (factor) => {
    return (base) => {
      return (times) => {
        return base * factor * times;
      };
    };
  };

  const doubleMultiplier = createMultiplier(2);
  const tripleMultiplier = createMultiplier(3);

  expect(doubleMultiplier(5)(3)).toBe(30); // 5 * 2 * 3
  expect(tripleMultiplier(4)(2)).toBe(24); // 4 * 3 * 2

  // Test function composition
  const compose = (f, g) => (x) => f(g(x));
  const addOne = (x) => x + 1;
  const double = (x) => x * 2;

  const addThenDouble = compose(double, addOne);
  const doubleThenAdd = compose(addOne, double);

  expect(addThenDouble(5)).toBe(12); // (5 + 1) * 2
  expect(doubleThenAdd(5)).toBe(11); // (5 * 2) + 1
});
