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

  test("bound length ignores non-number and inherited target length", () => {
    const foo = () => {};

    Object.defineProperty(foo, "length", { value: undefined });
    expect(foo.bind(null, 1).length).toBe(0);

    Object.defineProperty(foo, "length", { value: "1" });
    expect(foo.bind(null, 1).length).toBe(0);

    const bar = () => {};
    Object.setPrototypeOf(bar, { length: 42 });
    expect(delete bar.length).toBe(true);
    expect(Function.prototype.bind.call(bar, null, 1).length).toBe(0);
  });

  test("bound length is snapshotted at bind time", () => {
    const fn = (a, b, c) => {};
    const bound = fn.bind(null);

    Object.defineProperty(fn, "length", { value: 0, configurable: true });

    expect(bound.length).toBe(3);
  });

  test("bound length snapshots non-number and infinite target lengths", () => {
    const nonNumber = () => {};
    Object.defineProperty(nonNumber, "length", {
      value: undefined,
      configurable: true,
    });
    const nonNumberBound = nonNumber.bind(null);
    Object.defineProperty(nonNumber, "length", { value: 3, configurable: true });
    expect(nonNumberBound.length).toBe(0);

    const infinite = () => {};
    Object.defineProperty(infinite, "length", {
      value: Infinity,
      configurable: true,
    });
    expect(infinite.bind(null, 1).length).toBe(Infinity);
  });

  test("bound name is snapshotted at bind time", () => {
    const fn = () => {};
    Object.defineProperty(fn, "name", { value: "before", configurable: true });
    const bound = fn.bind(null);

    Object.defineProperty(fn, "name", { value: "after", configurable: true });

    expect(bound.name).toBe("bound before");
  });
});
