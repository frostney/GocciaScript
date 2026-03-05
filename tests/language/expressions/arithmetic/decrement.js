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

test("decrement preserves fractional part", () => {
  let x = 2.5;
  x--;
  expect(x).toBe(1.5);

  let y = 0.5;
  --y;
  expect(y).toBe(-0.5);
});
