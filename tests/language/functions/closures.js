/*---
description: Function closures and scope work correctly
features: [closures, function-scope]
---*/

test("local variable scope", () => {
  const testScope = () => {
    let localVar = "local";
    return localVar;
  };
  expect(testScope()).toBe("local");
});

test("function can access outer scope", () => {
  let outerVar = "outer";
  const inner = () => outerVar;
  expect(inner()).toBe("outer");
});

test("basic closure", () => {
  const createCounter = () => {
    let count = 0;
    return () => ++count;
  };

  const counter = createCounter();
  expect(counter()).toBe(1);
  expect(counter()).toBe(2);
  expect(counter()).toBe(3);
});

test("closure with parameters", () => {
  const createAdder = (x) => (y) => x + y;

  const addFive = createAdder(5);
  expect(addFive(3)).toBe(8);
  expect(addFive(10)).toBe(15);
});

test("multiple closures share same scope", () => {
  const createCounterPair = () => {
    let count = 0;
    return {
      increment: () => ++count,
      decrement: () => --count,
    };
  };

  const counters = createCounterPair();
  expect(counters.increment()).toBe(1);
  expect(counters.increment()).toBe(2);
  expect(counters.decrement()).toBe(1);
});
