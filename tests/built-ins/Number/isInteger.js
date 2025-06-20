/*---
description: Number.isInteger function works correctly
features: [Number.isInteger]
---*/

test("Number.isInteger", () => {
  expect(Number.isInteger(42)).toBeTruthy();
  expect(Number.isInteger(42.0)).toBeTruthy();
  expect(Number.isInteger(42.5)).toBeFalsy();
  expect(Number.isInteger("42")).toBeFalsy();
  expect(Number.isInteger(NaN)).toBeFalsy();
});
