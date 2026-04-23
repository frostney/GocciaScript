/*---
description: Function.prototype.call
features: [Function]
---*/

describe("Function.prototype.call", () => {
  test("calls a function with given this and arguments", () => {
    const fn = (a, b) => a + b;
    expect(fn.call(undefined, 1, 2)).toBe(3);
  });

  test("passes thisArg correctly", () => {
    class Obj {
      constructor(x) { this.x = x; }
      getX() { return this.x; }
    }
    const obj = new Obj(42);
    expect(obj.getX.call(obj)).toBe(42);
  });

  test("works with no arguments", () => {
    const fn = () => "hello";
    expect(fn.call(undefined)).toBe("hello");
  });

  test("accessible via Function.prototype.call", () => {
    expect(typeof Function.prototype.call).toBe("function");
  });

  test("has correct name and length", () => {
    expect(Function.prototype.call.name).toBe("call");
    expect(Function.prototype.call.length).toBe(1);
  });

  test("works on class constructors as type conversion", () => {
    expect(Number.call(null, 42)).toBe(42);
    expect(Number.call(null, "3.14")).toBe(3.14);
    expect(Number.call(null)).toBe(0);
    expect(String.call(null, 42)).toBe("42");
    expect(String.call(null, true)).toBe("true");
    expect(String.call(null)).toBe("");
    expect(Boolean.call(null, 1)).toBe(true);
    expect(Boolean.call(null, 0)).toBe(false);
  });
});
