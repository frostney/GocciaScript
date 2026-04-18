/*---
description: Global isNaN function
features: [isNaN]
---*/

describe("isNaN", () => {
  test("returns true for NaN", () => {
    expect(isNaN(NaN)).toBe(true);
  });

  test("returns false for numbers", () => {
    expect(isNaN(0)).toBe(false);
    expect(isNaN(42)).toBe(false);
    expect(isNaN(-1)).toBe(false);
    expect(isNaN(Infinity)).toBe(false);
    expect(isNaN(-Infinity)).toBe(false);
  });

  test("coerces argument to number", () => {
    expect(isNaN("hello")).toBe(true);
    expect(isNaN("NaN")).toBe(true);
    expect(isNaN(undefined)).toBe(true);
    expect(isNaN("42")).toBe(false);
    expect(isNaN("")).toBe(false);
    expect(isNaN(null)).toBe(false);
    expect(isNaN(true)).toBe(false);
    expect(isNaN(false)).toBe(false);
  });

  test("differs from Number.isNaN (no coercion)", () => {
    expect(isNaN("hello")).toBe(true);
    expect(Number.isNaN("hello")).toBe(false);
    expect(isNaN(undefined)).toBe(true);
    expect(Number.isNaN(undefined)).toBe(false);
  });
});
