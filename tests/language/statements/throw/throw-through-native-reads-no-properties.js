/*---
description: A throw that leaves the VM through native code reads no property of the thrown value (ES2026 §14.14, §14.15.3)
features: [Error, Proxy]
---*/

// ThrowStatement evaluation and the completion record that carries the value
// perform no [[Get]] on it. Error.prototype.toString (§20.5.3.4) is the
// operation that reads `name` and then `message`, once each, when asked to.

const countingError = (kind) => {
  const log = [];
  const error = new Error("x");
  Object.defineProperty(error, "message", {
    get() { log.push("message"); return "m"; },
  });
  Object.defineProperty(error, "name", {
    get() { log.push("name"); return "N"; },
  });
  return { error, log };
};

const throughNative = {
  forEach: (e) => [1].forEach(() => { throw e; }),
  map: (e) => [1].map(() => { throw e; }),
  sort: (e) => [2, 1].sort(() => { throw e; }),
  replace: (e) => "a".replace("a", () => { throw e; }),
  reduce: (e) => [1, 2].reduce(() => { throw e; }),
  JSONparse: (e) => JSON.parse("[1]", () => { throw e; }),
};

describe("a throw through native code", () => {
  for (const [label, run] of Object.entries(throughNative)) {
    test(`${label} reads neither name nor message and keeps the value`, () => {
      const { error, log } = countingError();
      let caught;
      try { run(error); } catch (x) { caught = x; }
      expect(caught).toBe(error);
      expect(log).toEqual([]);
    });
  }

  test("an Error.prototype.toString on the caught value then reads name and message once each, in order", () => {
    const { error, log } = countingError();
    try { throughNative.forEach(error); } catch (x) {}
    expect(String(error)).toBe("N: m");
    expect(log).toEqual(["name", "message"]);
  });

  test("getters on the prototype chain are not read either", () => {
    const log = [];
    class Custom extends Error {
      get name() { log.push("name"); return "Custom"; }
      get message() { log.push("message"); return "m"; }
    }
    const error = new Custom();
    let caught;
    try { throughNative.forEach(error); } catch (x) { caught = x; }
    expect(caught).toBe(error);
    expect(log).toEqual([]);
  });

  test("a Proxy thrown value runs no trap", () => {
    const log = [];
    const proxy = new Proxy(new Error("x"), {
      get(target, key, receiver) { log.push(`get:${String(key)}`); return Reflect.get(target, key, receiver); },
      getOwnPropertyDescriptor(target, key) { log.push(`gopd:${String(key)}`); return Reflect.getOwnPropertyDescriptor(target, key); },
      getPrototypeOf(target) { log.push("getPrototypeOf"); return Reflect.getPrototypeOf(target); },
    });
    let caught;
    try { throughNative.forEach(proxy); } catch (x) { caught = x; }
    expect(caught).toBe(proxy);
    expect(log).toEqual([]);
  });

  test("an object whose prototype is a Proxy runs no trap", () => {
    const log = [];
    const proto = new Proxy({}, {
      get(target, key) { log.push(`get:${String(key)}`); return undefined; },
      getOwnPropertyDescriptor(target, key) { log.push(`gopd:${String(key)}`); return undefined; },
    });
    const thrown = Object.create(proto);
    let caught;
    try { throughNative.forEach(thrown); } catch (x) { caught = x; }
    expect(caught).toBe(thrown);
    expect(log).toEqual([]);
  });

  test("a rejected promise from a native callback keeps the value without reads", async () => {
    const { error, log } = countingError();
    let caught;
    try { await Promise.resolve([1]).then((a) => a.forEach(() => { throw error; })); } catch (x) { caught = x; }
    expect(caught).toBe(error);
    expect(log).toEqual([]);
  });
});
