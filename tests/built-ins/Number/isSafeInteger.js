/*---
description: Number.isSafeInteger correctly identifies safe integers
features: [Number.isSafeInteger]
---*/

describe("Number.isSafeInteger", () => {
  test("returns true for safe integers", () => {
    expect(Number.isSafeInteger(0)).toBe(true);
    expect(Number.isSafeInteger(1)).toBe(true);
    expect(Number.isSafeInteger(-1)).toBe(true);
    expect(Number.isSafeInteger(42)).toBe(true);
    expect(Number.isSafeInteger(-100)).toBe(true);
  });

  test("returns true for MAX_SAFE_INTEGER", () => {
    expect(Number.isSafeInteger(Number.MAX_SAFE_INTEGER)).toBe(true);
  });

  test("returns true for MIN_SAFE_INTEGER", () => {
    expect(Number.isSafeInteger(Number.MIN_SAFE_INTEGER)).toBe(true);
  });

  test("returns false for values beyond safe range", () => {
    expect(Number.isSafeInteger(Number.MAX_SAFE_INTEGER + 1)).toBe(false);
    expect(Number.isSafeInteger(Number.MIN_SAFE_INTEGER - 1)).toBe(false);
  });

  test("returns false for non-integer numbers", () => {
    expect(Number.isSafeInteger(1.5)).toBe(false);
    expect(Number.isSafeInteger(0.1)).toBe(false);
    expect(Number.isSafeInteger(-3.14)).toBe(false);
  });

  test("returns false for special number values", () => {
    expect(Number.isSafeInteger(NaN)).toBe(false);
    expect(Number.isSafeInteger(Infinity)).toBe(false);
    expect(Number.isSafeInteger(-Infinity)).toBe(false);
  });

  test("returns false for non-number types", () => {
    expect(Number.isSafeInteger("42")).toBe(false);
    expect(Number.isSafeInteger(true)).toBe(false);
    expect(Number.isSafeInteger(null)).toBe(false);
    expect(Number.isSafeInteger(undefined)).toBe(false);
  });

  test("integer-valued floats are safe", () => {
    expect(Number.isSafeInteger(3.0)).toBe(true);
    expect(Number.isSafeInteger(-5.0)).toBe(true);
  });
});
