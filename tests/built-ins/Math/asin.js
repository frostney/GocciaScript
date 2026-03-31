/*---
description: Math.asin
features: [Math.asin]
---*/

test("Math.asin", () => {
  expect(Math.asin(0)).toBe(0);
  expect(Number.isNaN(Math.asin(2))).toBe(true);
  expect(Number.isNaN(Math.asin(NaN))).toBe(true);
});
