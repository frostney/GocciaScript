/*---
description: Math trigonometric functions work correctly
features: [Math.sin, Math.cos, Math.tan]
---*/

test("Math trigonometric functions", () => {
  expect(Math.sin(0)).toBeCloseTo(0, 10);
  expect(Math.sin(Math.PI / 2)).toBeCloseTo(1, 10);
  expect(Math.cos(0)).toBeCloseTo(1, 10);
  expect(Math.cos(Math.PI)).toBeCloseTo(-1, 10);
  expect(Math.tan(0)).toBeCloseTo(0, 10);
  expect(Math.tan(Math.PI / 4)).toBeCloseTo(1, 10);
});
