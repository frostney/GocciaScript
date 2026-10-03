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

  test("throws TypeError if descriptor is not an object", () => {
    const obj = {};
    expect(() => Reflect.defineProperty(obj, "x", 42)).toThrow(TypeError);
    expect(() => Reflect.defineProperty(obj, "y", null)).toThrow(TypeError);
    expect(() => Reflect.defineProperty(obj, "z", "string")).toThrow(TypeError);
    expect(() => Reflect.defineProperty(obj, "w", true)).toThrow(TypeError);
    expect(() => Reflect.defineProperty(obj, "v", undefined)).toThrow(TypeError);
  });

  test("throws for invalid descriptor before target descriptor lookup", () => {
    let trapCalled = false;
    const proxy = new Proxy({}, {
      getOwnPropertyDescriptor() {
        trapCalled = true;
        throw new Error("trap should not be called");
      },
    });

    expect(() => Reflect.defineProperty(proxy, "x", undefined)).toThrow(TypeError);
    expect(trapCalled).toBe(false);
  });

  test("propagates abrupt completion from property key before validating descriptor", () => {
    const obj = {};
    let propertyKeyWasConverted = false;
    const propertyKey = {
      toString() {
        propertyKeyWasConverted = true;
        throw new Error("property key failure");
      },
    };

    expect(() => Reflect.defineProperty(obj, propertyKey)).toThrow(Error);
    expect(propertyKeyWasConverted).toBe(true);
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

  test("redefines the name of a non-extensible class and reports a new property as not defined", () => {
    class K {}
    Object.preventExtensions(K);

    expect(Reflect.defineProperty(K, "name", { value: "Renamed" })).toBe(true);
    expect(Object.getOwnPropertyDescriptor(K, "name")).toEqual({
      value: "Renamed",
      writable: false,
      enumerable: false,
      configurable: true,
    });
    expect(Reflect.defineProperty(K, "added", { value: 1 })).toBe(false);
    expect(Object.hasOwn(K, "added")).toBe(false);
  });

  test("on a frozen class accepts only a definition that changes nothing", () => {
    class K {
      constructor(a) {}
    }
    Object.freeze(K);

    expect(Reflect.defineProperty(K, "name", { value: "Other" })).toBe(false);
    expect(Reflect.defineProperty(K, "length", { writable: true })).toBe(false);
    expect(Reflect.defineProperty(K, "name", { value: "K", configurable: false })).toBe(true);
    expect(Reflect.defineProperty(K, "length", { value: 1 })).toBe(true);
    expect(K.name).toBe("K");
    expect(K.length).toBe(1);
  });

  test("changes only the given attributes of a class's length and name", () => {
    class K {
      constructor(a, b) {}
    }

    expect(Reflect.defineProperty(K, "length", { enumerable: true })).toBe(true);
    expect(Object.getOwnPropertyDescriptor(K, "length")).toEqual({
      value: 2,
      writable: false,
      enumerable: true,
      configurable: true,
    });
    expect(Reflect.defineProperty(K, "name", {})).toBe(true);
    expect(Object.getOwnPropertyDescriptor(K, "name")).toEqual({
      value: "K",
      writable: false,
      enumerable: false,
      configurable: true,
    });
  });

  test("does not add back a deleted class length or name on a non-extensible class", () => {
    class K {}
    delete K.length;
    delete K.name;
    Object.preventExtensions(K);

    expect(Reflect.defineProperty(K, "length", { value: 1 })).toBe(false);
    expect(Reflect.defineProperty(K, "name", { value: "Back" })).toBe(false);
    expect(Object.hasOwn(K, "length")).toBe(false);
    expect(Object.hasOwn(K, "name")).toBe(false);
    expect(Object.getOwnPropertyNames(K)).toEqual(["prototype"]);
  });

  test("defines a deleted class name again on an extensible class", () => {
    class K {}
    delete K.name;

    expect(Reflect.defineProperty(K, "name", { value: "Again" })).toBe(true);
    expect(Object.hasOwn(K, "name")).toBe(true);
    expect(K.name).toBe("Again");
  });
});
