/*---
description: for...in enumerates a prototype chain of any length
features: [for-in, prototype-chain]
---*/

// ES2026 §14.7.5.10.2.1 %ForInIteratorPrototype%.next() moves to the
// prototype until it is null; nothing bounds the length of the chain.
const chain = (length, ownKeys) => {
  let object = { root: 1 };
  for (const i of Array.from({ length }, (_, n) => n)) {
    object = ownKeys
      ? Object.create(object, { ["k" + i]: { value: i, enumerable: true } })
      : Object.create(object);
  }
  return object;
};

const enumerate = (object) => {
  const keys = [];
  for (const key in object) {
    keys.push(key);
  }
  return keys;
};

describe("for...in over a deep prototype chain", () => {
  test("every key of a 300-link chain with a key per link is enumerated", () => {
    const keys = enumerate(chain(300, true));
    expect(keys.length).toBe(301);
    expect(keys[0]).toBe("k299");
    expect(keys[300]).toBe("root");
  });

  test("the key at the far end of a 5,000-link chain is enumerated", () => {
    expect(enumerate(chain(5000, false))).toEqual(["root"]);
  });

  // A super() call into a native constructor in bytecode mode sets the new
  // object's prototype after running the constructor's own code, without a
  // cycle check, so a user hook can close the stored chain into a loop
  // (interpreted mode and Node.js read the prototype first). Whatever the
  // chain looks like, for...in must end: with the keys, or with RangeError.
  test("for...in ends on an object whose stored chain loops back", () => {
    let captured = null;
    const originalSet = Map.prototype.set;
    Map.prototype.set = ({ set(key, value) {
      captured = captured || this;
      return originalSet.call(this, key, value);
    } }).set;
    class Derived extends Map {
      constructor(entries) {
        super(entries);
      }
    }
    const newTarget = new Proxy((class {}).bind(null), {
      get: (target, key, receiver) => key === "prototype"
        ? (captured ? Object.create(captured) : Derived.prototype)
        : Reflect.get(target, key, receiver),
    });
    let object;
    try {
      object = Reflect.construct(Derived, [[[1, 2]]], newTarget);
    } finally {
      Map.prototype.set = originalSet;
    }
    let outcome;
    try {
      outcome = enumerate(object).join(",");
    } catch (error) {
      outcome = error.constructor.name;
    }
    expect(outcome === "" || outcome === "RangeError").toBe(true);
  });
});
