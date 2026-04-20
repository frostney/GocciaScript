/*---
description: var supports destructuring patterns
features: [compat-var]
---*/

test("var with object destructuring", () => {
  var { a, b } = { a: 1, b: 2 };
  expect(a).toBe(1);
  expect(b).toBe(2);
});

test("var with array destructuring", () => {
  var [x, y] = [10, 20];
  expect(x).toBe(10);
  expect(y).toBe(20);
});
