/*---
description: Math.pow and Math.sqrt methods work correctly
features: [Math.pow, Math.sqrt]
---*/

test("Math.pow and Math.sqrt", () => {
  expect(Math.pow(2, 3)).toBe(8);
  expect(Math.pow(5, 2)).toBe(25);
  expect(Math.pow(10, 0)).toBe(1);
  expect(Math.pow(4, 0.5)).toBe(2);

  expect(Math.sqrt(16)).toBe(4);
  expect(Math.sqrt(25)).toBe(5);
  expect(Math.sqrt(0)).toBe(0);
  expect(Math.sqrt(2)).toBeCloseTo(1.414, 3);
});
