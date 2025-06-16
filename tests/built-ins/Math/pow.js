/*---
description: Math.pow and Math.sqrt methods work correctly
features: [Math.pow, Math.sqrt]
---*/

test("Math.pow", () => {
  expect(Math.pow(2, 3)).toBe(8);
  expect(Math.pow(5, 2)).toBe(25);
  expect(Math.pow(10, 0)).toBe(1);
  expect(Math.pow(4, 0.5)).toBe(2);
});
