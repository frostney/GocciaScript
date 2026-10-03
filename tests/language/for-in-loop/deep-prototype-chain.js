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
});
