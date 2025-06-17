/*---
description: Math.sign
features: [Math.sign]
---*/

test("Math.sign", () => {
  expect(Math.sign(1)).toBe(1);
  expect(Math.sign(-1)).toBe(-1);
  expect(Math.sign(0)).toBe(0);
  expect(Math.sign(NaN)).toBe(NaN);
  expect(Math.sign(Infinity)).toBe(1);
  expect(Math.sign(-Infinity)).toBe(-1);
});
