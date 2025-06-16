/*---
description: Function expressions work correctly including anonymous and named forms
features: [function-expression]
---*/

test("anonymous function expression", () => {
  const multiply = function (a, b) {
    return a * b;
  };
  expect(multiply(4, 5)).toBe(20);
  expect(typeof multiply).toBe("function");
});

test("named function expression", () => {
  const factorial = function fact(n) {
    if (n <= 1) return 1;
    return n * fact(n - 1);
  };
  expect(factorial(5)).toBe(120);
});

test("immediately invoked function expression (IIFE)", () => {
  const result = (function (x) {
    return x * 2;
  })(5);
  expect(result).toBe(10);
});

runTests();
