/*---
description: Math.sign
features: [Math.sign]
---*/

test("Math.sign", () => {
  expect(Math.sign(1)).toBe(1);
  expect(Math.sign(-1)).toBe(-1);
  expect(Math.sign(0)).toBeCloseTo(0);
  expect(Object.is(Math.sign(-0), -0)).toBe(true);
  expect(Math.sign(NaN)).toBeNaN();
  expect(Math.sign(Infinity)).toBe(1);
  expect(Math.sign(-Infinity)).toBe(-1);
});
