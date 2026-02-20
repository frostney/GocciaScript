/*---
description: Number constants (MAX_SAFE_INTEGER, MIN_SAFE_INTEGER, EPSILON, etc.)
features: [Number]
---*/

describe("Number constants", () => {
  test("MAX_SAFE_INTEGER", () => {
    expect(Number.MAX_SAFE_INTEGER).toBe(9007199254740991);
  });

  test("MIN_SAFE_INTEGER", () => {
    expect(Number.MIN_SAFE_INTEGER).toBe(-9007199254740991);
  });

  test("EPSILON", () => {
    expect(Number.EPSILON).toBeCloseTo(0, 3);
  });

  test("POSITIVE_INFINITY", () => {
    expect(Number.POSITIVE_INFINITY).toBe(Infinity);
  });

  test("NEGATIVE_INFINITY", () => {
    expect(Number.NEGATIVE_INFINITY).toBe(-Infinity);
  });

  test("NaN", () => {
    expect(Number.isNaN(Number.NaN)).toBe(true);
  });

  test("MAX_VALUE", () => {
    expect(Number.MAX_VALUE > 0).toBe(true);
    expect(Number.isFinite(Number.MAX_VALUE)).toBe(true);
  });

  test("MIN_VALUE", () => {
    expect(Number.MIN_VALUE > 0).toBe(true);
    expect(Number.MIN_VALUE < 1).toBe(true);
  });
});

describe("Number.isSafeInteger", () => {
  test("safe integers return true", () => {
    expect(Number.isSafeInteger(0)).toBe(true);
    expect(Number.isSafeInteger(42)).toBe(true);
    expect(Number.isSafeInteger(-42)).toBe(true);
    expect(Number.isSafeInteger(Number.MAX_SAFE_INTEGER)).toBe(true);
    expect(Number.isSafeInteger(Number.MIN_SAFE_INTEGER)).toBe(true);
  });

  test("non-integers return false", () => {
    expect(Number.isSafeInteger(3.14)).toBe(false);
    expect(Number.isSafeInteger(NaN)).toBe(false);
    expect(Number.isSafeInteger(Infinity)).toBe(false);
  });

  test("non-number arguments return false", () => {
    expect(Number.isSafeInteger("42")).toBe(false);
    expect(Number.isSafeInteger(true)).toBe(false);
  });
});
