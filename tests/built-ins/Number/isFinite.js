/*---
description: Number.isFinite function works correctly
features: [Number.isFinite]
---*/

describe("Number.isFinite", () => {
  test("returns true for finite numbers", () => {
    expect(Number.isFinite(42)).toBe(true);
    expect(Number.isFinite(123)).toBe(true);
    expect(Number.isFinite(0)).toBe(true);
    expect(Number.isFinite(-100)).toBe(true);
    expect(Number.isFinite(3.14)).toBe(true);
  });

  test("returns false for Infinity", () => {
    expect(Number.isFinite(Infinity)).toBe(false);
    expect(Number.isFinite(-Infinity)).toBe(false);
  });

  test("returns false for NaN", () => {
    expect(Number.isFinite(NaN)).toBe(false);
  });

  test("does not coerce non-number types (returns false)", () => {
    expect(Number.isFinite("42")).toBe(false);
    expect(Number.isFinite("123")).toBe(false);
    expect(Number.isFinite(true)).toBe(false);
    expect(Number.isFinite(null)).toBe(false);
    expect(Number.isFinite(undefined)).toBe(false);
  });

  test("returns false for objects and arrays", () => {
    expect(Number.isFinite({})).toBe(false);
    expect(Number.isFinite([])).toBe(false);
    expect(Number.isFinite({ valueOf: () => 42 })).toBe(false);
  });

  test("returns false with no arguments", () => {
    expect(Number.isFinite()).toBe(false);
  });
});
