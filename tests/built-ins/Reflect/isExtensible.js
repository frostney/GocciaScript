/*---
description: Reflect.isExtensible
features: [Reflect]
---*/

describe("Reflect.isExtensible", () => {
  test("returns true for extensible objects", () => {
    const obj = {};
    expect(Reflect.isExtensible(obj)).toBe(true);
  });

  test("returns false after preventExtensions", () => {
    const obj = {};
    Object.preventExtensions(obj);
    expect(Reflect.isExtensible(obj)).toBe(false);
  });

  test("returns false for frozen objects", () => {
    const obj = { x: 1 };
    Object.freeze(obj);
    expect(Reflect.isExtensible(obj)).toBe(false);
  });

  test("returns false for sealed objects", () => {
    const obj = { x: 1 };
    Object.seal(obj);
    expect(Reflect.isExtensible(obj)).toBe(false);
  });

  test("throws TypeError if target is not an object", () => {
    expect(() => Reflect.isExtensible(42)).toThrow(TypeError);
    expect(() => Reflect.isExtensible("str")).toThrow(TypeError);
    expect(() => Reflect.isExtensible(null)).toThrow(TypeError);
  });
});
