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

test("pre-decrement", () => {
  let a = 5;
  expect(--a).toBe(4);
  expect(a).toBe(4);
});

test("post-decrement", () => {
  let a = 5;
  expect(a--).toBe(5);
  expect(a).toBe(4);
});

runTests();
