/*---
description: Global isFinite function
features: [isFinite]
---*/

describe("isFinite", () => {
  test("returns true for finite numbers", () => {
    expect(isFinite(0)).toBe(true);
    expect(isFinite(42)).toBe(true);
    expect(isFinite(-1)).toBe(true);
    expect(isFinite(3.14)).toBe(true);
  });

  test("returns false for non-finite values", () => {
    expect(isFinite(Infinity)).toBe(false);
    expect(isFinite(-Infinity)).toBe(false);
    expect(isFinite(NaN)).toBe(false);
  });

  test("coerces argument to number", () => {
    expect(isFinite("42")).toBe(true);
    expect(isFinite("")).toBe(true);
    expect(isFinite(null)).toBe(true);
    expect(isFinite(true)).toBe(true);
    expect(isFinite("hello")).toBe(false);
    expect(isFinite(undefined)).toBe(false);
  });

  test("differs from Number.isFinite (no coercion)", () => {
    expect(isFinite("42")).toBe(true);
    expect(Number.isFinite("42")).toBe(false);
    expect(isFinite(null)).toBe(true);
    expect(Number.isFinite(null)).toBe(false);
  });
});
