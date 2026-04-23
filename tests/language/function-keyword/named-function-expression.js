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
