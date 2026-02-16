/*---
description: Property descriptor semantics
features: [Object.defineProperty, Object.getOwnPropertyDescriptor]
---*/

describe("property descriptors", () => {
  test("default descriptor for data property", () => {
    const obj = { x: 42 };
    const desc = Object.getOwnPropertyDescriptor(obj, "x");
    expect(desc.value).toBe(42);
    expect(desc.writable).toBe(true);
    expect(desc.enumerable).toBe(true);
    expect(desc.configurable).toBe(true);
  });

  test("defineProperty with non-writable", () => {
    const obj = {};
    Object.defineProperty(obj, "readonly", {
      value: 10,
      writable: false,
      enumerable: true,
      configurable: true,
    });
    expect(obj.readonly).toBe(10);

    // In strict mode, assignment to non-writable should throw
    expect(() => {
      obj.readonly = 99;
    }).toThrow();
    expect(obj.readonly).toBe(10);
  });

  test("defineProperty with non-enumerable", () => {
    const obj = { visible: 1 };
    Object.defineProperty(obj, "hidden", {
      value: 2,
      enumerable: false,
      writable: true,
      configurable: true,
    });

    expect(obj.hidden).toBe(2);
    expect(Object.keys(obj)).toEqual(["visible"]);
    expect(Object.getOwnPropertyNames(obj)).toContain("hidden");
  });

  test("defineProperty with non-configurable", () => {
    const obj = {};
    Object.defineProperty(obj, "locked", {
      value: 42,
      writable: true,
      enumerable: true,
      configurable: false,
    });

    const desc = Object.getOwnPropertyDescriptor(obj, "locked");
    expect(desc.configurable).toBe(false);

    // Cannot reconfigure a non-configurable property
    expect(() => {
      Object.defineProperty(obj, "locked", {
        enumerable: false,
      });
    }).toThrow();
  });

  test("accessor property with getter and setter", () => {
    const obj = {};
    let stored = 0;
    Object.defineProperty(obj, "value", {
      get: () => stored * 2,
      set: (v) => {
        stored = v;
      },
      enumerable: true,
      configurable: true,
    });

    obj.value = 5;
    expect(obj.value).toBe(10);
    expect(stored).toBe(5);
  });

  test("accessor property with only getter is read-only", () => {
    const obj = {};
    Object.defineProperty(obj, "readOnly", {
      get: () => 42,
      enumerable: true,
      configurable: true,
    });

    expect(obj.readOnly).toBe(42);
  });

  test("getOwnPropertyDescriptor returns undefined for missing properties", () => {
    const obj = { a: 1 };
    expect(Object.getOwnPropertyDescriptor(obj, "missing")).toBe(undefined);
  });

  test("defineProperties sets multiple properties", () => {
    const obj = {};
    Object.defineProperties(obj, {
      x: { value: 1, enumerable: true, writable: true, configurable: true },
      y: { value: 2, enumerable: true, writable: true, configurable: true },
      z: { value: 3, enumerable: false, writable: true, configurable: true },
    });

    expect(obj.x).toBe(1);
    expect(obj.y).toBe(2);
    expect(obj.z).toBe(3);
    expect(Object.keys(obj)).toEqual(["x", "y"]);
  });

  test("overwriting a data property with defineProperty", () => {
    const obj = { x: 1 };
    Object.defineProperty(obj, "x", {
      value: 42,
      writable: true,
      enumerable: true,
      configurable: true,
    });
    expect(obj.x).toBe(42);
  });
});
