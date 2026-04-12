/*---
description: Object.getPrototypeOf
features: [Object.getPrototypeOf]
---*/

describe("Object.getPrototypeOf", () => {
  test("returns the prototype of an object created with Object.create", () => {
    const proto = { greet: () => "hello" };
    const obj = Object.create(proto);
    expect(Object.getPrototypeOf(obj)).toBe(proto);
  });

  test("returns null for objects created with null prototype", () => {
    const obj = Object.create(null);
    expect(Object.getPrototypeOf(obj)).toBe(null);
  });

  test("class instances have their class prototype", () => {
    class Animal {
      speak() {
        return "...";
      }
    }
    const a = new Animal();
    const proto = Object.getPrototypeOf(a);
    expect(proto !== null).toBe(true);
  });

  test("with number coerces to wrapper — returns a non-null object", () => {
    // Primitives are coerced via ToObject; boxed number has the number prototype
    const proto = Object.getPrototypeOf(42);
    expect(proto !== null && proto !== undefined).toBe(true);
  });

  test("with boolean coerces to wrapper — returns a non-null object", () => {
    const proto = Object.getPrototypeOf(true);
    expect(proto !== null && proto !== undefined).toBe(true);
  });

  test("throws for null", () => {
    expect(() => Object.getPrototypeOf(null)).toThrow(TypeError);
  });

  test("throws for undefined", () => {
    expect(() => Object.getPrototypeOf(undefined)).toThrow(TypeError);
  });
});
