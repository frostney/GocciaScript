/*---
description: Number.isFinite function works correctly
features: [Number.isFinite]
---*/

test("Number.isFinite", () => {
  expect(Number.isFinite(123)).toBeTruthy();
  expect(Number.isFinite("123")).toBeFalsy(); // Different from global isFinite
  expect(Number.isFinite(Infinity)).toBeFalsy();
  expect(Number.isFinite(NaN)).toBeFalsy();
});
