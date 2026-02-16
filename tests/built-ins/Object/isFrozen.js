/*---
description: Object.isFrozen correctly identifies frozen objects
features: [Object.isFrozen]
---*/

describe("Object.isFrozen", () => {
  test("returns false for regular objects", () => {
    const obj = { a: 1, b: 2 };
    expect(Object.isFrozen(obj)).toBe(false);
  });

  test("returns true after Object.freeze", () => {
    const obj = { x: 10 };
    Object.freeze(obj);
    expect(Object.isFrozen(obj)).toBe(true);
  });

  test("returns true for empty frozen object", () => {
    const obj = {};
    Object.freeze(obj);
    expect(Object.isFrozen(obj)).toBe(true);
  });

  test("primitives are considered frozen", () => {
    expect(Object.isFrozen(42)).toBe(true);
    expect(Object.isFrozen("hello")).toBe(true);
    expect(Object.isFrozen(true)).toBe(true);
    expect(Object.isFrozen(false)).toBe(true);
  });

  test("empty object is not frozen by default", () => {
    expect(Object.isFrozen({})).toBe(false);
  });

  test("object with only non-writable non-configurable properties", () => {
    const obj = {};
    Object.defineProperty(obj, "x", {
      value: 1,
      writable: false,
      configurable: false,
      enumerable: true,
    });
    // Not frozen unless also non-extensible, but our freeze makes it so
    // This tests the granularity of isFrozen
    const frozen = Object.freeze(obj);
    expect(Object.isFrozen(frozen)).toBe(true);
  });
});
