/*---
description: Function closures and scope work correctly
features: [closures, function-scope]
---*/

test("local variable scope", () => {
  function testScope() {
    let localVar = "local";
    return localVar;
  }
  expect(testScope()).toBe("local");
});

test("function can access outer scope", () => {
  let outerVar = "outer";
  function inner() {
    return outerVar;
  }
  expect(inner()).toBe("outer");
});

test("basic closure", () => {
  function createCounter() {
    let count = 0;
    return function () {
      count++;
      return count;
    };
  }

  const counter = createCounter();
  expect(counter()).toBe(1);
  expect(counter()).toBe(2);
  expect(counter()).toBe(3);
});

test("closure with parameters", () => {
  function createAdder(x) {
    return function (y) {
      return x + y;
    };
  }

  const addFive = createAdder(5);
  expect(addFive(3)).toBe(8);
  expect(addFive(10)).toBe(15);
});

test("multiple closures share same scope", () => {
  function createCounterPair() {
    let count = 0;
    return {
      increment: function () {
        return ++count;
      },
      decrement: function () {
        return --count;
      },
    };
  }

  const counters = createCounterPair();
  expect(counters.increment()).toBe(1);
  expect(counters.increment()).toBe(2);
  expect(counters.decrement()).toBe(1);
});

runTests();
