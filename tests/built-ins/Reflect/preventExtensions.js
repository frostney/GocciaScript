/*---
description: Reflect.preventExtensions
features: [Reflect]
---*/

describe("Reflect.preventExtensions", () => {
  test("returns true and prevents new properties", () => {
    const obj = { x: 1 };
    const result = Reflect.preventExtensions(obj);
    expect(result).toBe(true);
    expect(Reflect.isExtensible(obj)).toBe(false);
  });

  test("existing properties are still accessible", () => {
    const obj = { x: 42 };
    Reflect.preventExtensions(obj);
    expect(obj.x).toBe(42);
  });

  test("throws TypeError if target is not an object", () => {
    expect(() => Reflect.preventExtensions(42)).toThrow(TypeError);
    expect(() => Reflect.preventExtensions("str")).toThrow(TypeError);
    expect(() => Reflect.preventExtensions(null)).toThrow(TypeError);
  });
});
