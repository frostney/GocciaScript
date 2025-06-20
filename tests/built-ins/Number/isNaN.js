/*---
description: Number.isNaN function works correctly
features: [Number.isNaN]
---*/

test("Number.isNaN", () => {
  expect(Number.isNaN(NaN)).toBeTruthy();
  expect(Number.isNaN("abc")).toBeFalsy(); // Different from global isNaN
  expect(Number.isNaN(123)).toBeFalsy();
  expect(Number.isNaN("123")).toBeFalsy();
});
