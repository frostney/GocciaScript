/*---
description: Nested function declarations
features: [compat-function]
---*/

test("function declared inside another function", () => {
  function outer(x) {
    function inner(y) {
      return x + y;
    }
    return inner(10);
  }
  expect(outer(5)).toBe(15);
});

test("inner function captures outer scope", () => {
  function makeAdder(x) {
    function adder(y) {
      return x + y;
    }
    return adder;
  }
  const add5 = makeAdder(5);
  expect(add5(3)).toBe(8);
});
