/*---
description: Reflect.apply
features: [Reflect]
---*/

describe("Reflect.apply", () => {
  test("calls a function with the given this and arguments", () => {
    const fn = (a, b) => a + b;
    expect(Reflect.apply(fn, undefined, [1, 2])).toBe(3);
  });

  test("passes thisArgument correctly", () => {
    class Obj {
      constructor(x) {
        this.x = x;
      }
      getX() {
        return this.x;
      }
    }
    const obj = new Obj(42);
    expect(Reflect.apply(obj.getX, obj, [])).toBe(42);
  });

  test("works with empty arguments list", () => {
    const fn = () => "hello";
    expect(Reflect.apply(fn, undefined, [])).toBe("hello");
  });

  test("throws TypeError if target is not callable", () => {
    expect(() => Reflect.apply({}, undefined, [])).toThrow(TypeError);
    expect(() => Reflect.apply(42, undefined, [])).toThrow(TypeError);
    expect(() => Reflect.apply("string", undefined, [])).toThrow(TypeError);
  });

  test("works with array-like objects", () => {
    const fn = (a, b, c) => a + b + c;
    const arrayLike = { 0: 10, 1: 20, 2: 30, length: 3 };
    expect(Reflect.apply(fn, undefined, arrayLike)).toBe(60);
  });

  test("works with array-like object with zero length", () => {
    const fn = () => "no-args";
    expect(Reflect.apply(fn, undefined, { length: 0 })).toBe("no-args");
  });

  test("works with array-like object with missing indices", () => {
    const fn = (a, b) => [a, b];
    const arrayLike = { 0: "x", length: 2 };
    const result = Reflect.apply(fn, undefined, arrayLike);
    expect(result[0]).toBe("x");
    expect(result[1]).toBe(undefined);
  });

  test("works with array-like object with string length", () => {
    const fn = (a) => a;
    const arrayLike = { 0: "hello", length: "1" };
    expect(Reflect.apply(fn, undefined, arrayLike)).toBe("hello");
  });

  test("throws TypeError if argumentsList is not an object", () => {
    const fn = () => {};
    expect(() => Reflect.apply(fn, undefined, "not-array")).toThrow(TypeError);
    expect(() => Reflect.apply(fn, undefined, 42)).toThrow(TypeError);
    expect(() => Reflect.apply(fn, undefined, true)).toThrow(TypeError);
  });
});
