/*---
description: Math.max
features: [Math.max]
---*/

test("Math.max", () => {
  expect(Math.max(1, 2, 3)).toBe(3);
  expect(Math.max(-1, -2, -3)).toBe(-1);
  expect(Math.max(1, 2, 3)).toBe(3);
  expect(Math.max(1, 2, 3, 4, 5)).toBe(5);
  expect(Math.max(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)).toBe(10);

  expect(Math.max()).toBe(-Infinity);

  expect(Math.max(NaN, 1, 2)).toBeNaN();
  expect(Math.max(1, 2, NaN)).toBeNaN();
  expect(Math.max(NaN, NaN)).toBeNaN();
  expect(Math.max(Infinity, 1, 2)).toBe(Infinity);
  expect(Math.max(-Infinity, 1, 2)).toBe(2);
  expect(Math.max(Infinity, -Infinity)).toBe(Infinity);
});
