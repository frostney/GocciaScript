/*---
description: Reflect.has
features: [Reflect]
---*/

describe("Reflect.has", () => {
  test("returns true for own property", () => {
    const obj = { x: 1 };
    expect(Reflect.has(obj, "x")).toBe(true);
  });

  test("returns false for missing property", () => {
    const obj = { x: 1 };
    expect(Reflect.has(obj, "y")).toBe(false);
  });

  test("checks prototype chain", () => {
    const proto = { inherited: true };
    const obj = Object.create(proto);
    expect(Reflect.has(obj, "inherited")).toBe(true);
  });

  test("works with arrays and length", () => {
    const arr = [1, 2, 3];
    expect(Reflect.has(arr, "length")).toBe(true);
  });

  test("works with symbol keys", () => {
    const sym = Symbol("myKey");
    const obj = { [sym]: "value" };
    expect(Reflect.has(obj, sym)).toBe(true);
    expect(Reflect.has(obj, Symbol("other"))).toBe(false);
  });

  test("throws TypeError if target is not an object", () => {
    expect(() => Reflect.has(42, "x")).toThrow(TypeError);
    expect(() => Reflect.has("str", "x")).toThrow(TypeError);
    expect(() => Reflect.has(null, "x")).toThrow(TypeError);
  });
});
