/*---
description: Math.acos
features: [Math.acos]
---*/

test("Math.acos", () => {
  expect(Math.acos(1)).toBe(0);
  expect(Number.isNaN(Math.acos(2))).toBe(true);
  expect(Number.isNaN(Math.acos(NaN))).toBe(true);
});
