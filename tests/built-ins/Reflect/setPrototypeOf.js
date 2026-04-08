/*---
description: Reflect.setPrototypeOf
features: [Reflect]
---*/

describe("Reflect.setPrototypeOf", () => {
  test("sets prototype and returns true", () => {
    const proto = { greet: () => "hello" };
    const obj = {};
    const result = Reflect.setPrototypeOf(obj, proto);
    expect(result).toBe(true);
    expect(obj.greet()).toBe("hello");
    expect(Reflect.getPrototypeOf(obj)).toBe(proto);
  });

  test("sets prototype to null", () => {
    const obj = { x: 1 };
    const result = Reflect.setPrototypeOf(obj, null);
    expect(result).toBe(true);
    expect(Reflect.getPrototypeOf(obj)).toBe(null);
  });

  test("returns false for non-extensible objects", () => {
    const obj = {};
    Object.preventExtensions(obj);
    const result = Reflect.setPrototypeOf(obj, { y: 2 });
    expect(result).toBe(false);
  });

  test("throws TypeError if target is not an object", () => {
    expect(() => Reflect.setPrototypeOf(42, {})).toThrow(TypeError);
    expect(() => Reflect.setPrototypeOf("str", {})).toThrow(TypeError);
  });

  test("throws TypeError if proto is not an object or null", () => {
    const obj = {};
    expect(() => Reflect.setPrototypeOf(obj, 42)).toThrow(TypeError);
    expect(() => Reflect.setPrototypeOf(obj, "str")).toThrow(TypeError);
    expect(() => Reflect.setPrototypeOf(obj, undefined)).toThrow(TypeError);
  });
});
