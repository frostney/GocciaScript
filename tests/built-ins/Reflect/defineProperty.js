/*---
description: Reflect.defineProperty
features: [Reflect]
---*/

describe("Reflect.defineProperty", () => {
  test("defines a data property and returns true", () => {
    const obj = {};
    const result = Reflect.defineProperty(obj, "x", {
      value: 42,
      writable: true,
      enumerable: true,
      configurable: true,
    });
    expect(result).toBe(true);
    expect(obj.x).toBe(42);
  });

  test("defines a non-writable property", () => {
    const obj = {};
    Reflect.defineProperty(obj, "x", {
      value: 10,
      writable: false,
      configurable: true,
    });
    const desc = Object.getOwnPropertyDescriptor(obj, "x");
    expect(desc.value).toBe(10);
    expect(desc.writable).toBe(false);
  });

  test("defines an accessor property", () => {
    const obj = {};
    let stored = 0;
    Reflect.defineProperty(obj, "val", {
      get: () => stored,
      set: (v) => { stored = v; },
      enumerable: true,
      configurable: true,
    });
    obj.val = 99;
    expect(obj.val).toBe(99);
    expect(stored).toBe(99);
  });

  test("returns false when defining on non-configurable property fails", () => {
    const obj = {};
    Object.defineProperty(obj, "x", {
      value: 1,
      writable: false,
      configurable: false,
    });
    const result = Reflect.defineProperty(obj, "x", {
      value: 2,
      configurable: true,
    });
    expect(result).toBe(false);
    expect(obj.x).toBe(1);
  });

  test("throws TypeError if target is not an object", () => {
    expect(() => Reflect.defineProperty(42, "x", { value: 1 })).toThrow(TypeError);
    expect(() => Reflect.defineProperty("str", "x", { value: 1 })).toThrow(TypeError);
  });

  test("throws TypeError for mixed data and accessor descriptors", () => {
    const obj = {};

    // value + get is invalid
    expect(() => {
      Reflect.defineProperty(obj, "mixed1", {
        value: 1,
        get: () => 2,
      });
    }).toThrow(TypeError);

    // value + set is invalid
    expect(() => {
      Reflect.defineProperty(obj, "mixed2", {
        value: 1,
        set: (v) => {},
      });
    }).toThrow(TypeError);

    // writable + get is invalid
    expect(() => {
      Reflect.defineProperty(obj, "mixed3", {
        writable: true,
        get: () => 2,
      });
    }).toThrow(TypeError);

    // writable + set is invalid
    expect(() => {
      Reflect.defineProperty(obj, "mixed4", {
        writable: false,
        set: (v) => {},
      });
    }).toThrow(TypeError);
  });

  test("throws TypeError for non-callable getter and setter", () => {
    const obj = {};

    expect(() => {
      Reflect.defineProperty(obj, "badGet", { get: 42 });
    }).toThrow(TypeError);

    expect(() => {
      Reflect.defineProperty(obj, "badGet2", { get: "not a function" });
    }).toThrow(TypeError);

    expect(() => {
      Reflect.defineProperty(obj, "badSet", { set: 42 });
    }).toThrow(TypeError);

    expect(() => {
      Reflect.defineProperty(obj, "badSet2", { set: "not a function" });
    }).toThrow(TypeError);

    // undefined getter/setter is allowed
    const result = Reflect.defineProperty(obj, "getOnly", {
      get: () => 99,
      set: undefined,
      configurable: true,
    });
    expect(result).toBe(true);
    expect(obj.getOnly).toBe(99);
  });
});
