/*---
description: Increment and decrement operators work correctly
features: [increment-operators, decrement-operators]
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

test("decrement preserves fractional part", () => {
  let x = 2.5;
  x--;
  expect(x).toBe(1.5);

  let y = 0.5;
  --y;
  expect(y).toBe(-0.5);
});
