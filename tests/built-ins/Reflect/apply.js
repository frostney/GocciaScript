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

  test("throws TypeError if argumentsList is not an array", () => {
    const fn = () => {};
    expect(() => Reflect.apply(fn, undefined, "not-array")).toThrow(TypeError);
  });
});
