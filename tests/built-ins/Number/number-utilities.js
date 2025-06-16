/*---
description: Number utility functions work correctly
features: [Number.isNaN, Number.isFinite, Number.isInteger, Number.parseInt, Number.parseFloat]
---*/

test("Number.isNaN", () => {
  expect(Number.isNaN(NaN)).toBeTruthy();
  expect(Number.isNaN("abc")).toBeFalsy(); // Different from global isNaN
  expect(Number.isNaN(123)).toBeFalsy();
  expect(Number.isNaN("123")).toBeFalsy();
});

test("Number.isFinite", () => {
  expect(Number.isFinite(123)).toBeTruthy();
  expect(Number.isFinite("123")).toBeFalsy(); // Different from global isFinite
  expect(Number.isFinite(Infinity)).toBeFalsy();
  expect(Number.isFinite(NaN)).toBeFalsy();
});

test("Number.isInteger", () => {
  expect(Number.isInteger(42)).toBeTruthy();
  expect(Number.isInteger(42.0)).toBeTruthy();
  expect(Number.isInteger(42.5)).toBeFalsy();
  expect(Number.isInteger("42")).toBeFalsy();
  expect(Number.isInteger(NaN)).toBeFalsy();
});

test("Number.parseInt and Number.parseFloat", () => {
  expect(Number.parseInt("123")).toBe(123);
  expect(Number.parseInt("123.45")).toBe(123);
  expect(Number.parseInt("123abc")).toBe(123);

  expect(Number.parseFloat("123.45")).toBe(123.45);
  expect(Number.parseFloat("123")).toBe(123);
  expect(Number.parseFloat("123.45abc")).toBe(123.45);
});
