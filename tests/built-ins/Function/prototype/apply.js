/*---
description: Function.prototype.apply
features: [Function]
---*/

describe("Function.prototype.apply", () => {
  test("calls a function with the given this and arguments array", () => {
    const fn = (a, b) => a + b;
    expect(fn.apply(undefined, [1, 2])).toBe(3);
  });

  test("passes thisArg correctly", () => {
    class Obj {
      constructor(x) {
        this.x = x;
      }
      getX() {
        return this.x;
      }
    }
    const obj = new Obj(42);
    expect(obj.getX.apply(obj, [])).toBe(42);
  });

  test("works with empty arguments array", () => {
    const fn = () => "hello";
    expect(fn.apply(undefined, [])).toBe("hello");
  });

  test("works with no arguments at all", () => {
    const fn = () => "default";
    expect(fn.apply(undefined)).toBe("default");
  });

  test("treats undefined argArray as no arguments", () => {
    const fn = () => "ok";
    expect(fn.apply(undefined, undefined)).toBe("ok");
  });

  test("treats null argArray as no arguments", () => {
    const fn = () => "ok";
    expect(fn.apply(undefined, null)).toBe("ok");
  });

  test("works with array-like objects", () => {
    const fn = (a, b, c) => a + b + c;
    const arrayLike = { 0: 10, 1: 20, 2: 30, length: 3 };
    expect(fn.apply(undefined, arrayLike)).toBe(60);
  });

  test("works with array-like object with zero length", () => {
    const fn = () => "no-args";
    expect(fn.apply(undefined, { length: 0 })).toBe("no-args");
  });

  test("works with array-like object with missing indices", () => {
    const fn = (a, b) => [a, b];
    const arrayLike = { 0: "x", length: 2 };
    const result = fn.apply(undefined, arrayLike);
    expect(result[0]).toBe("x");
    expect(result[1]).toBe(undefined);
  });

  test("works with array-like object with string length", () => {
    const fn = (a) => a;
    const arrayLike = { 0: "hello", length: "1" };
    expect(fn.apply(undefined, arrayLike)).toBe("hello");
  });

  test("treats absent length as zero", () => {
    const fn = (...args) => args.length;
    expect(fn.apply(undefined, {})).toBe(0);
    expect(fn.apply(undefined, { 0: "ignored" })).toBe(0);
  });

  test("treats non-numeric string length as zero", () => {
    const fn = (...args) => args.length;
    expect(fn.apply(undefined, { length: "foo" })).toBe(0);
    expect(fn.apply(undefined, { 0: "ignored", length: "bar" })).toBe(0);
  });

  test("throws RangeError for excessively large array-like length", () => {
    const fn = () => {};
    expect(() => fn.apply(undefined, { length: 2000000000 })).toThrow(RangeError);
  });

  test("throws TypeError if argArray is not an object", () => {
    const fn = () => {};
    expect(() => fn.apply(undefined, "not-array")).toThrow(TypeError);
    expect(() => fn.apply(undefined, 42)).toThrow(TypeError);
    expect(() => fn.apply(undefined, true)).toThrow(TypeError);
  });

  test("throws TypeError when called on non-function", () => {
    const obj = { apply: Function.prototype.apply };
    expect(() => obj.apply(undefined, [])).toThrow(TypeError);
  });

  test("has correct name and length", () => {
    expect(Function.prototype.apply.name).toBe("apply");
    expect(Function.prototype.apply.length).toBe(2);
  });

  test("works on class constructors as type conversion", () => {
    expect(Number.apply(null, [42])).toBe(42);
    expect(String.apply(null, [42])).toBe("42");
    expect(Boolean.apply(null, [1])).toBe(true);
  });
});
