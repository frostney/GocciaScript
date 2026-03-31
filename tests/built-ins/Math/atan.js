/*---
description: Math.atan
features: [Math.atan]
---*/

test("Math.atan", () => {
  expect(Math.atan(0)).toBe(0);
  expect(Number.isNaN(Math.atan(NaN))).toBe(true);
});
