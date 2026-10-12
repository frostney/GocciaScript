/*---
description: Function declarations, function expressions, generator functions and async functions bind destructured parameters left to right, with every parameter binding in its temporal dead zone until it is initialized
features: [compat-function, destructuring, default-parameters, temporal-dead-zone, generators, async-functions, async-generators]
---*/

describe("destructured parameters of function-keyword functions", () => {
  test("declarations and expressions", () => {
    function self({ a = a }) {
      return a;
    }
    function later({ a = b }, b) {
      return a;
    }
    function after({ a }, b = a) {
      return b;
    }
    expect(() => self({})).toThrow(ReferenceError);
    expect(() => later({}, 1)).toThrow(ReferenceError);
    expect(after({ a: 9 })).toBe(9);
    expect(() => (function ([a = a]) {
      return a;
    })([])).toThrow(ReferenceError);
    expect((function ({ a }, [b = a]) {
      return b;
    })({ a: 2 }, [])).toBe(2);
  });

  test("a constructor called with new", () => {
    function F({ a = b }, b) {
      this.a = a;
    }
    function G({ a }, b = a) {
      this.b = b;
    }
    expect(() => new F({}, 1)).toThrow(ReferenceError);
    expect(new G({ a: 3 }).b).toBe(3);
  });

  test("generator functions throw when called", () => {
    function* g({ a = a }) {
      yield a;
    }
    function* ok({ a }, b = a) {
      yield b;
    }
    expect(() => g({})).toThrow(ReferenceError);
    expect(ok({ a: 4 }).next().value).toBe(4);
  });

  test("async functions reject", async () => {
    async function f({ a = a }) {
      return a;
    }
    async function ok({ a }, b = a) {
      return b;
    }
    await expect(f({})).rejects.toThrow(ReferenceError);
    expect(await ok({ a: 1 })).toBe(1);
  });

  test("async generator functions throw when called", async () => {
    async function* g({ a = b }, b) {
      yield a;
    }
    async function* ok({ a }, b = a) {
      yield b;
    }
    expect(() => g({}, 1)).toThrow(ReferenceError);
    expect(await ok({ a: 5 }).next()).toEqual({ value: 5, done: false });
  });
});
