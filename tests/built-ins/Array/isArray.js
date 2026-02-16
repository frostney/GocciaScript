/*---
description: Array.isArray correctly identifies arrays
features: [Array.isArray]
---*/

describe("Array.isArray", () => {
  test("returns true for array literals", () => {
    expect(Array.isArray([])).toBe(true);
    expect(Array.isArray([1, 2, 3])).toBe(true);
  });

  test("returns true for Array constructor", () => {
    expect(Array.isArray(Array.of(1, 2))).toBe(true);
    expect(Array.isArray(Array.from([1]))).toBe(true);
  });

  test("returns false for non-arrays", () => {
    expect(Array.isArray({})).toBe(false);
    expect(Array.isArray("hello")).toBe(false);
    expect(Array.isArray(123)).toBe(false);
    expect(Array.isArray(true)).toBe(false);
    expect(Array.isArray(null)).toBe(false);
    expect(Array.isArray(undefined)).toBe(false);
  });

  test("returns false for array-like objects", () => {
    const arrayLike = { 0: "a", 1: "b", length: 2 };
    expect(Array.isArray(arrayLike)).toBe(false);
  });

  test("returns true for nested arrays", () => {
    expect(Array.isArray([[1], [2]])).toBe(true);
  });

  test("returns true for empty array", () => {
    expect(Array.isArray([])).toBe(true);
  });
});
