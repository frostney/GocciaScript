/*---
description: Number.isInteger function works correctly
features: [Number.isInteger]
---*/

test("Number.isInteger returns true for integers", () => {
  expect(Number.isInteger(42)).toBeTruthy();
  expect(Number.isInteger(42.0)).toBeTruthy();
  expect(Number.isInteger(0)).toBeTruthy();
  expect(Number.isInteger(-100)).toBeTruthy();
});

test("Number.isInteger returns false for non-integers", () => {
  expect(Number.isInteger(42.5)).toBeFalsy();
  expect(Number.isInteger(NaN)).toBeFalsy();
  expect(Number.isInteger(Infinity)).toBeFalsy();
  expect(Number.isInteger(-Infinity)).toBeFalsy();
});

test("Number.isInteger returns false for non-number types", () => {
  expect(Number.isInteger("42")).toBeFalsy();
  expect(Number.isInteger(true)).toBeFalsy();
  expect(Number.isInteger(null)).toBeFalsy();
  expect(Number.isInteger(undefined)).toBeFalsy();
  expect(Number.isInteger({})).toBeFalsy();
  expect(Number.isInteger([])).toBeFalsy();
});

test("Number.isInteger returns false with no arguments", () => {
  expect(Number.isInteger()).toBeFalsy();
});
