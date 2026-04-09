/*---
description: Reflect.get
features: [Reflect]
---*/

describe("Reflect.get", () => {
  test("gets a property value", () => {
    const obj = { x: 42 };
    expect(Reflect.get(obj, "x")).toBe(42);
  });

  test("returns undefined for missing property", () => {
    const obj = { x: 1 };
    expect(Reflect.get(obj, "y")).toBe(undefined);
  });

  test("traverses prototype chain", () => {
    const proto = { greeting: "hello" };
    const obj = Object.create(proto);
    expect(Reflect.get(obj, "greeting")).toBe("hello");
  });

  test("works with arrays", () => {
    const arr = [10, 20, 30];
    expect(Reflect.get(arr, "length")).toBe(3);
  });

  test("works with symbol keys", () => {
    const sym = Symbol("key");
    const obj = {};
    obj[sym] = "symbol-value";
    expect(Reflect.get(obj, sym)).toBe("symbol-value");
  });

  test("throws TypeError if target is not an object", () => {
    expect(() => Reflect.get(42, "x")).toThrow(TypeError);
    expect(() => Reflect.get("str", "x")).toThrow(TypeError);
    expect(() => Reflect.get(null, "x")).toThrow(TypeError);
    expect(() => Reflect.get(undefined, "x")).toThrow(TypeError);
  });
});
