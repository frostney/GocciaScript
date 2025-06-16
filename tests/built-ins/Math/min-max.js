/*---
description: Math.min and Math.max methods work correctly
features: [Math.min, Math.max]
---*/

test("Math.min and Math.max", () => {
  expect(Math.min(1, 2, 3)).toBe(1);
  expect(Math.min(-1, -2, -3)).toBe(-3);
  expect(Math.max(1, 2, 3)).toBe(3);
  expect(Math.max(-1, -2, -3)).toBe(-1);

  expect(Math.min()).toBe(Infinity);
  expect(Math.max()).toBe(-Infinity);

  expect(Math.min(NaN, 1, 2)).toBeNaN();
  expect(Math.max(NaN, 1, 2)).toBeNaN();
  expect(Math.min(1, 2, NaN)).toBeNaN();
  expect(Math.max(1, 2, NaN)).toBeNaN();
  expect(Math.min(NaN, NaN)).toBeNaN();
  expect(Math.max(NaN, NaN)).toBeNaN();
  expect(Math.min(Infinity, 1, 2)).toBe(1);
  expect(Math.max(Infinity, 1, 2)).toBe(2);
  expect(Math.min(-Infinity, 1, 2)).toBe(-Infinity);
  expect(Math.max(-Infinity, 1, 2)).toBe(2);
  expect(Math.min(Infinity, -Infinity)).toBe(-Infinity);
  expect(Math.max(Infinity, -Infinity)).toBeInfinity();
  expect(Math.min(-Infinity, Infinity)).toBe(-Infinity);
});
