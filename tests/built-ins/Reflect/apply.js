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

  test("array holes in argumentsList are passed as undefined", () => {
    const fn = (a, b, c, d) => [a, b, c, d];
    const result = Reflect.apply(fn, undefined, ["arg1", 2, , null]);
    expect(result).toEqual(["arg1", 2, undefined, null]);
  });

  test("holes at every position of argumentsList become undefined", () => {
    const collect = (...args) => args.map((a) => String(a)).join("|");

    expect(Reflect.apply(collect, undefined, [1, , 3])).toBe("1|undefined|3");
    expect(Reflect.apply(collect, undefined, [, 2, 3])).toBe("undefined|2|3");
    expect(Reflect.apply(collect, undefined, [1, 2, ,])).toBe("1|2|undefined");
    expect(Reflect.apply(collect, undefined, [, , ,])).toBe(
      "undefined|undefined|undefined",
    );
    expect(Reflect.apply(collect, undefined, [,])).toBe("undefined");
    expect(
      Reflect.apply((a, b, c) => b === undefined, undefined, [1, , 3]),
    ).toBe(true);
  });

  // CreateListFromArrayLike step 6b reads each index with Get, so a hole is
  // resolved through the prototype chain rather than replaced with undefined.
  test("holes in argumentsList resolve through the array prototype chain", () => {
    const collect = (...args) => args.map((a) => String(a)).join("|");

    Object.defineProperty(Array.prototype, 1, {
      get() {
        return "inherited";
      },
      configurable: true,
    });

    try {
      expect(Reflect.apply(collect, undefined, [1, , 3])).toBe("1|inherited|3");
      expect(Reflect.apply(collect, undefined, [1, , 3, 4])).toBe(
        "1|inherited|3|4",
      );

      class Box {
        constructor(...args) {
          this.tag = args.map((a) => String(a)).join("|");
        }
      }

      expect(Reflect.construct(Box, [1, , 3]).tag).toBe("1|inherited|3");
    } finally {
      delete Array.prototype[1];
    }
  });

  test("uses the array's length, not its dense element count", () => {
    const collect = (...args) => args.map((a) => String(a)).join("|");
    const grown = [1, 2];
    grown.length = 5;

    expect(Reflect.apply(collect, undefined, grown)).toBe(
      "1|2|undefined|undefined|undefined",
    );

    const huge = [1, 2];
    huge.length = 2000000;
    expect(() => Reflect.apply(collect, undefined, huge)).toThrow(RangeError);
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

  test("throws TypeError when argumentsList is missing", () => {
    const fn = () => {};
    expect(() => Reflect.apply(fn, undefined)).toThrow(TypeError);
  });

  test("passes every argument for any argument count", () => {
    const collect = (...args) => args.length + ":" + args.join("");
    const first = (a, b) => [a, b].join("|");
    const many = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17];

    expect(Reflect.apply(collect, undefined, many.slice(0, 1))).toBe("1:1");
    expect(Reflect.apply(collect, undefined, many.slice(0, 7))).toBe("7:1234567");
    expect(Reflect.apply(collect, undefined, many.slice(0, 8))).toBe("8:12345678");
    expect(Reflect.apply(collect, undefined, many.slice(0, 9))).toBe("9:123456789");
    expect(Reflect.apply(collect, undefined, many)).toBe("17:1234567891011121314151617");
    expect(Reflect.apply(first, undefined, many)).toBe("1|2");
  });

  test("passes this and many arguments to a method", () => {
    const holder = {
      tag: "T",
      join(...parts) {
        return this.tag + parts.join("");
      },
    };

    expect(Reflect.apply(holder.join, { tag: "U" }, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10])).toBe("U12345678910");
  });
});
