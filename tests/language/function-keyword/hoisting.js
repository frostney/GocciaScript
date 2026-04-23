/*---
description: Function declarations are hoisted (name and value)
features: [compat-function]
---*/

test("function can be called before its declaration", () => {
  const result = greet("world");
  function greet(name) {
    return "Hello, " + name;
  }
  expect(result).toBe("Hello, world");
});

test("multiple hoisted functions", () => {
  const sum = add(first(), second());
  function add(a, b) { return a + b; }
  function first() { return 10; }
  function second() { return 20; }
  expect(sum).toBe(30);
});

test("function declaration overrides earlier var binding", () => {
  var x = 1;
  function x() { return 2; }
  expect(typeof x).toBe("function");
});
