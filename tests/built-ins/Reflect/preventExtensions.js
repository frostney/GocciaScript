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

  test("returns false when proxy preventExtensions trap returns false", () => {
    const proxy = new Proxy({}, {
      preventExtensions() {
        return false;
      },
    });

    expect(Reflect.preventExtensions(proxy)).toBe(false);
  });

  test("returns true when proxy preventExtensions trap prevents target extensions", () => {
    const target = {};
    const proxy = new Proxy(target, {
      preventExtensions(t) {
        Object.preventExtensions(t);
        return true;
      },
    });

    expect(Reflect.preventExtensions(proxy)).toBe(true);
    expect(Reflect.isExtensible(target)).toBe(false);
  });

  test("returns false for resizable ArrayBuffer typed array views", () => {
    const buffer = new ArrayBuffer(4, { maxByteLength: 8 });

    expect(Reflect.preventExtensions(new Uint8Array(buffer, 0, 4))).toBe(false);
    expect(Reflect.preventExtensions(new Uint8Array(buffer))).toBe(false);
  });

  test("returns false for growable SharedArrayBuffer length-tracking views", () => {
    const buffer = new SharedArrayBuffer(4, { maxByteLength: 8 });

    expect(Reflect.preventExtensions(new Uint8Array(buffer, 0, 4))).toBe(true);
    expect(Reflect.preventExtensions(new Uint8Array(buffer))).toBe(false);
  });

  test("throws TypeError if target is not an object", () => {
    expect(() => Reflect.preventExtensions(42)).toThrow(TypeError);
    expect(() => Reflect.preventExtensions("str")).toThrow(TypeError);
    expect(() => Reflect.preventExtensions(null)).toThrow(TypeError);
  });
});
