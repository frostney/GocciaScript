/*---
description: Math.atan2
features: [Math.atan2]
---*/

test("Math.atan2", () => {
  expect(Math.atan2(0, 1)).toBe(0);
  expect(Math.atan2(1, 0)).toBe(Math.PI / 2);
  expect(Number.isNaN(Math.atan2(NaN, 1))).toBe(true);
});
