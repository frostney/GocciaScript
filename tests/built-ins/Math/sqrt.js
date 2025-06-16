/*---
description: Math.sqrt
features: [Math.sqrt]
---*/

test("Math.sqrt", () => {
  expect(Math.sqrt(4)).toBe(2);
  expect(Math.sqrt(9)).toBe(3);
  expect(Math.sqrt(16)).toBe(4);
  expect(Math.sqrt(0)).toBe(0);
  expect(Math.sqrt(2)).toBeCloseTo(1.414, 3);
  expect(Math.sqrt(Infinity)).toBe(Infinity);
  expect(Math.sqrt(-1)).toBeNaN();
  expect(Math.sqrt(NaN)).toBeNaN(); // NaN should return NaN
});
