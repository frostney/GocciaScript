/*---
description: Named function expressions
features: [compat-function]
---*/

test("named function expression assigned to variable", () => {
  const greet = function sayHello(name) {
    return "Hello, " + name;
  };
  expect(greet("world")).toBe("Hello, world");
});

test("named function expression as argument", () => {
  const result = [1, 2, 3].reduce(function sum(acc, val) {
    return acc + val;
  }, 0);
  expect(result).toBe(6);
});

test("named function expression self-reference (recursion)", () => {
  const factorial = function fact(n) {
    if (n <= 1) return 1;
    return n * fact(n - 1);
  };
  expect(factorial(5)).toBe(120);
});

test("named function expression name not visible outside", () => {
  const f = function myFunc() {
    return 42;
  };
  expect(f()).toBe(42);
  expect(typeof myFunc).toBe("undefined");
});

test("named function expression name is read-only inside", () => {
  const f = function myFunc() {
    myFunc = 99;
  };
  expect(() => f()).toThrow();
});
