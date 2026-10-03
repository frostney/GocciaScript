/*---
description: The in operator finds a symbol key anywhere on the prototype chain
features: [Symbol, prototype-chain]
---*/

// ES2026 §13.10.1 step 6 returns HasProperty(rval, key), and §10.1.7.1
// OrdinaryHasProperty asks the prototype when the object has no own property,
// for a symbol key as for a string key.
describe("in with a symbol key", () => {
  const key = Symbol("key");

  test("a well-known symbol inherited from a built-in prototype is found", () => {
    expect(Symbol.iterator in []).toBe(true);
    expect(Symbol.iterator in new Map()).toBe(true);
    expect(Symbol.iterator in "abc".split("")).toBe(true);
    expect(Symbol.toPrimitive in new Date()).toBe(true);
    expect(Symbol.asyncIterator in ({ async *gen() {} }).gen()).toBe(true);
  });

  test("a symbol on an ordinary prototype is found", () => {
    expect(key in Object.create({ [key]: 1 })).toBe(true);
    expect(key in Object.create(Object.create({ [key]: 1 }))).toBe(true);
  });

  test("a symbol method of a base class is found on an instance and a static one on the subclass", () => {
    class Base { static [key]() {} [key]() {} }
    class Derived extends Base {}
    expect(key in new Derived()).toBe(true);
    expect(key in Derived).toBe(true);
  });

  test("a symbol present nowhere on the chain is not found", () => {
    expect(key in {}).toBe(false);
    expect(key in Object.create(null)).toBe(false);
    expect(Symbol.iterator in {}).toBe(false);
  });

  test("a Proxy in the chain is asked through its has trap", () => {
    const seen = [];
    const child = Object.create(new Proxy({}, {
      has(target, k) {
        seen.push(k);
        return true;
      },
    }));
    expect(key in child).toBe(true);
    expect(seen).toEqual([key]);
  });

  test("a prototype cycle through a Proxy throws RangeError", () => {
    const object = {};
    Object.setPrototypeOf(object, new Proxy(Object.create(object), {}));
    expect(() => key in object).toThrow(RangeError);
  });
});
