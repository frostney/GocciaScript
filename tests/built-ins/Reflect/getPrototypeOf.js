/*---
description: Reflect.getPrototypeOf
features: [Reflect]
---*/

describe("Reflect.getPrototypeOf", () => {
  test("returns the prototype of an object created with Object.create", () => {
    const proto = { greet: () => "hello" };
    const obj = Object.create(proto);
    expect(Reflect.getPrototypeOf(obj)).toBe(proto);
  });

  test("returns null for objects with null prototype", () => {
    const obj = Object.create(null);
    expect(Reflect.getPrototypeOf(obj)).toBe(null);
  });

  test("returns class prototype for instances", () => {
    class Animal {
      speak() {
        return "...";
      }
    }
    const a = new Animal();
    const proto = Reflect.getPrototypeOf(a);
    expect(proto !== null).toBe(true);
  });

  test("throws TypeError if target is not an object", () => {
    expect(() => Reflect.getPrototypeOf(42)).toThrow(TypeError);
    expect(() => Reflect.getPrototypeOf("str")).toThrow(TypeError);
    expect(() => Reflect.getPrototypeOf(true)).toThrow(TypeError);
    expect(() => Reflect.getPrototypeOf(null)).toThrow(TypeError);
    expect(() => Reflect.getPrototypeOf(undefined)).toThrow(TypeError);
  });
});
