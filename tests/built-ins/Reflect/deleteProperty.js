/*---
description: Reflect.deleteProperty
features: [Reflect]
---*/

describe("Reflect.deleteProperty", () => {
  test("deletes an existing property and returns true", () => {
    const obj = { x: 1, y: 2 };
    const result = Reflect.deleteProperty(obj, "x");
    expect(result).toBe(true);
    expect(obj.x).toBe(undefined);
    expect(obj.y).toBe(2);
  });

  test("returns true for non-existent property", () => {
    const obj = { x: 1 };
    const result = Reflect.deleteProperty(obj, "z");
    expect(result).toBe(true);
  });

  test("returns false for non-configurable property", () => {
    const obj = {};
    Object.defineProperty(obj, "x", {
      value: 1,
      configurable: false,
    });
    const result = Reflect.deleteProperty(obj, "x");
    expect(result).toBe(false);
    expect(obj.x).toBe(1);
  });

  test("throws TypeError if target is not an object", () => {
    expect(() => Reflect.deleteProperty(42, "x")).toThrow(TypeError);
    expect(() => Reflect.deleteProperty("str", "x")).toThrow(TypeError);
  });
});
