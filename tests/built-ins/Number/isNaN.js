/*---
description: Number.isNaN function works correctly
features: [Number.isNaN]
---*/

describe("Number.isNaN", () => {
  test("returns true for NaN", () => {
    expect(Number.isNaN(NaN)).toBe(true);
  });

  test("returns false for numbers", () => {
    expect(Number.isNaN(42)).toBe(false);
    expect(Number.isNaN(0)).toBe(false);
    expect(Number.isNaN(123)).toBe(false);
    expect(Number.isNaN(Infinity)).toBe(false);
  });

  test("does not coerce non-number types (returns false)", () => {
    expect(Number.isNaN("abc")).toBe(false);
    expect(Number.isNaN("123")).toBe(false);
    expect(Number.isNaN(undefined)).toBe(false);
    expect(Number.isNaN(true)).toBe(false);
    expect(Number.isNaN(null)).toBe(false);
  });
});
