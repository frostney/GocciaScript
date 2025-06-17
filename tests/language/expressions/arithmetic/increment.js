/*---
description: Increment and decrement operators work correctly
features: [increment-operators, decrement-operators]
---*/

test("pre-increment", () => {
  let a = 5;
  expect(++a).toBe(6);
  expect(a).toBe(6);
});

test("post-increment", () => {
  let a = 5;
  expect(a++).toBe(5);
  expect(a).toBe(6);
});
