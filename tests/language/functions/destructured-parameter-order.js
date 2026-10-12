/*---
description: Destructured parameters are bound left to right with the other parameters, and every parameter binding stays in its temporal dead zone until it is initialized
features: [destructuring, default-parameters, rest-parameters, temporal-dead-zone, arrow-function, classes, generators, async-functions, async-generators]
---*/

describe("destructured parameter binding order", () => {
  test("a pattern default reading its own binding throws ReferenceError", () => {
    expect(() => (({ a = a }) => a)({})).toThrow(ReferenceError);
    expect(() => (([a = a]) => a)([])).toThrow(ReferenceError);
    expect(() => (({ x: { a = a } }) => a)({ x: {} })).toThrow(ReferenceError);
    expect(() => (({ x: [a = a] }) => a)({ x: [] })).toThrow(ReferenceError);
    expect(() => (({ [a]: a }) => a)({})).toThrow(ReferenceError);
  });

  test("a pattern default reading a later parameter throws ReferenceError", () => {
    expect(() => (({ a = b }, b) => a)({}, 1)).toThrow(ReferenceError);
    expect(() => (({ a = c }, { c }) => a)({}, { c: 3 })).toThrow(ReferenceError);
    expect(() => (([a = r, ...r]) => a)([])).toThrow(ReferenceError);
    expect(() => (({ a = b, b }) => a)({ b: 2 })).toThrow(ReferenceError);
    expect(() => ((...[a = b, b]) => a)(undefined, 1)).toThrow(ReferenceError);
    expect(() => (({ [b]: a }, b) => a)({ k: 1 }, "k")).toThrow(ReferenceError);
  });

  test("a default after a pattern reads the destructured value", () => {
    expect((({ a }, b = a) => b)({ a: 1 })).toBe(1);
    expect((({ a } = { a: 2 }, b = a) => b)()).toBe(2);
    expect((({ a, ...r }, b = r) => b)({ a: 1, c: 2 })).toEqual({ c: 2 });
    expect((({ a }, [b = a]) => b)({ a: 11 }, [])).toBe(11);
    expect(((...[a, b = a]) => [a, b])(5)).toEqual([5, 5]);
    expect((({ a = 1, b = a + 1 }, [c = b + 1] = [], d = c + 1) => [a, b, c, d])({})).toEqual([1, 2, 3, 4]);
  });

  test("an earlier parameter is visible to a later pattern", () => {
    expect(((b, { a = b }) => a)(3, {})).toBe(3);
    expect(((b, { [b]: a }) => a)("k", { k: 1 })).toBe(1);
    expect(((b = 4, [a = b]) => a)(undefined, [])).toBe(4);
  });

  test("initializers run in source order", () => {
    const log = [];
    ((x = log.push("x"), { y = log.push("y") }, z = log.push("z")) => 0)(undefined, {}, undefined);
    expect(log).toEqual(["x", "y", "z"]);

    const keys = [];
    (({ [keys.push("key")]: a = keys.push("default") }, b = keys.push("b")) => 0)({});
    expect(keys).toEqual(["key", "default", "b"]);
  });

  test("a failing pattern stops before later initializers run", () => {
    const log = [];
    expect(() => (({ a }, b = log.push("b")) => 0)(null)).toThrow(TypeError);
    expect(log).toEqual([]);
  });

  test("closures in defaults capture parameters", () => {
    expect((({ a }, f = () => a) => f())({ a: 3 })).toBe(3);
    expect((({ a = () => b }, b) => a())({}, 6)).toBe(6);
    expect((({ a = () => c }, { c }) => a())({}, { c: 8 })).toBe(8);
    expect(() => (({ a = (() => c)() }, { c }) => a)({}, { c: 8 })).toThrow(ReferenceError);
    expect(((x, { a = () => x }) => a())(1, {})).toBe(1);
    expect((({ a }, f = () => a) => {
      a = 9;
      return f();
    })({ a: 1 })).toBe(9);
    expect((({ a = 1 }, b = a + 1) => () => [a, b])({})()).toEqual([1, 2]);
  });

  test("object methods, generators and accessors", () => {
    const object = {
      m({ a = a }) {
        return a;
      },
      ok({ a }, b = a) {
        return b;
      },
      *g({ a = b }, b) {
        yield a;
      },
      *gOk({ a }, b = a) {
        yield b;
      },
      set s({ a = a }) {},
      set sOk([a, b = a]) {
        this.value = b;
      },
    };
    expect(() => object.m({})).toThrow(ReferenceError);
    expect(object.ok({ a: 7 })).toBe(7);
    expect(() => object.g({}, 1)).toThrow(ReferenceError);
    expect(object.gOk({ a: 4 }).next().value).toBe(4);
    expect(() => {
      object.s = {};
    }).toThrow(ReferenceError);
    object.sOk = [3];
    expect(object.value).toBe(3);
  });

  test("class constructors, methods and accessors", () => {
    class Point {
      constructor({ x = y }, y) {
        this.x = x;
      }
    }
    expect(() => new Point({}, 1)).toThrow(ReferenceError);

    class Pair {
      constructor({ a }, b = a) {
        this.b = b;
      }
      m({ a = a }) {
        return a;
      }
      static s([a = b], b) {
        return a;
      }
      static sOk([a], b = a) {
        return b;
      }
      set v({ a = a }) {}
      rest(...[a, b = a]) {
        return [a, b];
      }
      restPlain(...[a, b]) {
        return [a, b];
      }
    }
    expect(new Pair({ a: 2 }).b).toBe(2);
    expect(() => new Pair({}).m({})).toThrow(ReferenceError);
    expect(() => Pair.s([], 1)).toThrow(ReferenceError);
    expect(Pair.sOk([5])).toBe(5);
    expect(() => {
      new Pair({}).v = {};
    }).toThrow(ReferenceError);
    expect(new Pair({}).rest(1)).toEqual([1, 1]);
    expect(new Pair({}).restPlain(1, 2)).toEqual([1, 2]);

    class Base {
      constructor(value) {
        this.value = value;
      }
    }
    class Derived extends Base {
      constructor({ a }, b = a) {
        super(b);
      }
    }
    class DerivedTdz extends Base {
      constructor({ a = b }, b) {
        super(a);
      }
    }
    expect(new Derived({ a: 2 }).value).toBe(2);
    expect(() => new DerivedTdz({}, 1)).toThrow(ReferenceError);
  });

  test("async arrows and methods reject", async () => {
    await expect((async ({ a = a }) => a)({})).rejects.toThrow(ReferenceError);
    await expect((async ({ a = b }, b) => a)({}, 1)).rejects.toThrow(ReferenceError);
    expect(await (async ({ a }, b = a) => b)({ a: 1 })).toBe(1);

    const object = {
      async m({ a = a }) {
        return a;
      },
    };
    await expect(object.m({})).rejects.toThrow(ReferenceError);

    class C {
      async m([a = b], b) {
        return a;
      }
      async ok([a], b = a) {
        return b;
      }
    }
    await expect(new C().m([], 1)).rejects.toThrow(ReferenceError);
    expect(await new C().ok([6])).toBe(6);
  });

  test("async generator methods throw when called", async () => {
    const object = {
      async *g({ a = a }) {
        yield a;
      },
      async *ok({ a }, b = a) {
        yield b;
      },
    };
    expect(() => object.g({})).toThrow(ReferenceError);
    expect(await object.ok({ a: 5 }).next()).toEqual({ value: 5, done: false });
  });
});
