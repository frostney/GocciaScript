/*---
description: Math.trunc
features: [Math.trunc]
---*/

test("Math.trunc", () => {
  expect(Math.trunc(1.3)).toBe(1);
  expect(Math.trunc(-1.3)).toBe(-1);
  expect(Math.trunc(0.3)).toBeCloseTo(0);
  expect(Math.trunc(2.8)).toBe(2);
  expect(Math.trunc(-2.8)).toBe(-2);
  expect(Math.trunc(42.4452232115)).toBe(42);
  expect(Math.trunc(NaN)).toBeNaN();
  expect(Math.trunc(Infinity)).toBe(Infinity);
  expect(Math.trunc(-Infinity)).toBe(-Infinity);
  expect(Math.trunc(NaN)).toBeNaN();
});
