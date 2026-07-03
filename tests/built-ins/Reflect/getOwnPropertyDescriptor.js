/*---
description: Reflect.getOwnPropertyDescriptor
features: [Reflect]
---*/

describe("Reflect.getOwnPropertyDescriptor", () => {
  test("returns descriptor for data property", () => {
    const obj = {};
    Object.defineProperty(obj, "x", {
      value: 42,
      writable: true,
      enumerable: false,
      configurable: true,
    });
    const desc = Reflect.getOwnPropertyDescriptor(obj, "x");
    expect(desc.value).toBe(42);
    expect(desc.writable).toBe(true);
    expect(desc.enumerable).toBe(false);
    expect(desc.configurable).toBe(true);
  });

  test("returns data descriptor keys in FromPropertyDescriptor order", () => {
    const obj = {};
    Object.defineProperty(obj, "x", {
      value: 42,
      writable: true,
      enumerable: true,
      configurable: true,
    });

    expect(Object.keys(Reflect.getOwnPropertyDescriptor(obj, "x"))).toEqual([
      "value",
      "writable",
      "enumerable",
      "configurable",
    ]);
  });

  test("returns descriptor for accessor property", () => {
    const obj = {};
    const getter = () => 10;
    Object.defineProperty(obj, "y", {
      get: getter,
      enumerable: true,
      configurable: true,
    });
    const desc = Reflect.getOwnPropertyDescriptor(obj, "y");
    expect(desc.get).toBe(getter);
    expect(desc.enumerable).toBe(true);
    expect(desc.configurable).toBe(true);
  });

  test("returns accessor descriptor keys in FromPropertyDescriptor order", () => {
    const obj = {};
    const getter = () => 10;
    const setter = () => {};
    Object.defineProperty(obj, "y", {
      get: getter,
      set: setter,
      enumerable: true,
      configurable: true,
    });

    expect(Object.keys(Reflect.getOwnPropertyDescriptor(obj, "y"))).toEqual([
      "get",
      "set",
      "enumerable",
      "configurable",
    ]);
  });

  test("returns symbol data descriptor keys in FromPropertyDescriptor order", () => {
    const sym = Symbol("x");
    const obj = {};
    Object.defineProperty(obj, sym, {
      value: 1,
      writable: true,
      enumerable: true,
      configurable: true,
    });

    expect(Object.keys(Reflect.getOwnPropertyDescriptor(obj, sym))).toEqual([
      "value",
      "writable",
      "enumerable",
      "configurable",
    ]);
  });

  test("returns undefined for non-existent property", () => {
    const obj = { x: 1 };
    expect(Reflect.getOwnPropertyDescriptor(obj, "z")).toBe(undefined);
  });

  test("does not traverse prototype chain", () => {
    const proto = { inherited: true };
    const obj = Object.create(proto);
    expect(Reflect.getOwnPropertyDescriptor(obj, "inherited")).toBe(undefined);
  });

  test("throws TypeError if target is not an object", () => {
    expect(() => Reflect.getOwnPropertyDescriptor(42, "x")).toThrow(TypeError);
    expect(() => Reflect.getOwnPropertyDescriptor("str", "x")).toThrow(TypeError);
  });
});
