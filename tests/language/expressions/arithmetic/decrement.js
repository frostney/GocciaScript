/*---
description: Decrement operator works correctly
features: [decrement-operator]
---*/

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
