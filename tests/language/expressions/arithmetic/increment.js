/*---
description: Increment operator works correctly
features: [increment-operators]
---*/

test("pre-increment", () => {
  let a = 5;
  expect(a).toBe(5);
  expect(++a).toBe(6);
  expect(a).toBe(6);
});

test("post-increment", () => {
  let a = 5;
  expect(a).toBe(5);
  expect(a++).toBe(5);
  expect(a).toBe(6);
});

test("increment preserves fractional part", () => {
  let x = 1.5;
  x++;
  expect(x).toBe(2.5);

  let y = -0.5;
  ++y;
  expect(y).toBe(0.5);
});
