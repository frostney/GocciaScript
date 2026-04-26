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

  test("non-extensible object with non-writable non-configurable properties is structurally frozen", () => {
    const obj = {};
    Object.defineProperty(obj, "a", {
      value: 1,
      writable: false,
      configurable: false,
      enumerable: true,
    });
    Object.defineProperty(obj, "b", {
      value: 2,
      writable: false,
      configurable: false,
      enumerable: true,
    });
    Object.preventExtensions(obj);
    expect(Object.isFrozen(obj)).toBe(true);
  });

  test("non-extensible object with writable property is not frozen", () => {
    const obj = {};
    Object.defineProperty(obj, "a", {
      value: 1,
      writable: true,
      configurable: false,
      enumerable: true,
    });
    Object.preventExtensions(obj);
    expect(Object.isFrozen(obj)).toBe(false);
  });

  test("non-extensible object with configurable property is not frozen", () => {
    const obj = {};
    Object.defineProperty(obj, "a", {
      value: 1,
      writable: false,
      configurable: true,
      enumerable: true,
    });
    Object.preventExtensions(obj);
    expect(Object.isFrozen(obj)).toBe(false);
  });

  test("empty non-extensible object is frozen", () => {
    const obj = {};
    Object.preventExtensions(obj);
    expect(Object.isFrozen(obj)).toBe(true);
  });

  test("non-extensible object with accessor properties is frozen when non-configurable", () => {
    const obj = {};
    Object.defineProperty(obj, "x", {
      get() { return 42; },
      configurable: false,
      enumerable: true,
    });
    Object.preventExtensions(obj);
    expect(Object.isFrozen(obj)).toBe(true);
  });

  test("extensible object is never frozen", () => {
    const obj = {};
    Object.defineProperty(obj, "a", {
      value: 1,
      writable: false,
      configurable: false,
    });
    expect(Object.isFrozen(obj)).toBe(false);
  });
});
