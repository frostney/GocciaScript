/*---
description: Reflect.set
features: [Reflect]
---*/

describe("Reflect.set", () => {
  test("sets a property and returns true", () => {
    const obj = {};
    const result = Reflect.set(obj, "x", 42);
    expect(result).toBe(true);
    expect(obj.x).toBe(42);
  });

  test("updates an existing property", () => {
    const obj = { x: 1 };
    Reflect.set(obj, "x", 99);
    expect(obj.x).toBe(99);
  });

  test("works with named properties on arrays", () => {
    const arr = [1, 2, 3];
    Reflect.set(arr, "custom", "value");
    expect(arr.custom).toBe("value");
  });

  test("works with symbol keys", () => {
    const sym = Symbol("key");
    const obj = {};
    Reflect.set(obj, sym, "symbol-value");
    expect(obj[sym]).toBe("symbol-value");
  });

  test("throws TypeError if target is not an object", () => {
    expect(() => Reflect.set(42, "x", 1)).toThrow(TypeError);
    expect(() => Reflect.set("str", "x", 1)).toThrow(TypeError);
    expect(() => Reflect.set(null, "x", 1)).toThrow(TypeError);
  });
});
