/*---
description: Proven numeric locals add an Int16 immediate
features: [addition-operator]
---*/

test("adds an Int16 immediate to an integer local", () => {
  let n = 4;
  expect(n + 3).toBe(7);
  expect(3 + n).toBe(7);
  expect(n + -3).toBe(1);
  expect(-3 + n).toBe(1);
});

test("adds an Int16 immediate to a float local", () => {
  let n = 4.5;
  expect(n + 3).toBe(7.5);
  expect(3 + n).toBe(7.5);
  expect(n + -3).toBe(1.5);
  expect(-3 + n).toBe(1.5);
});
