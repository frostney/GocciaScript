/*---
description: Function.prototype.bind
features: [Function]
---*/

describe("Function.prototype.bind", () => {
  test("returns a bound function", () => {
    const fn = (a, b) => a + b;
    const bound = fn.bind(null, 1);
    expect(typeof bound).toBe("function");
    expect(bound(2)).toBe(3);
  });

  test("binds this value", () => {
    class Obj {
      constructor(x) { this.x = x; }
      getX() { return this.x; }
    }
    const obj = new Obj(42);
    const getX = obj.getX.bind(obj);
    expect(getX()).toBe(42);
  });

  test("accessible via Function.prototype.bind", () => {
    expect(typeof Function.prototype.bind).toBe("function");
  });

  test("has correct name and length", () => {
    expect(Function.prototype.bind.name).toBe("bind");
    expect(Function.prototype.bind.length).toBe(1);
  });

  test("works on class constructors", () => {
    const bnc = Number.bind(null);
    expect(bnc(42)).toBe(42);
    expect(bnc("3.14")).toBe(3.14);

    const bsc = String.bind(null);
    expect(bsc(42)).toBe("42");

    const bbc = Boolean.bind(null);
    expect(bbc(1)).toBe(true);
    expect(bbc(0)).toBe(false);
  });
});
