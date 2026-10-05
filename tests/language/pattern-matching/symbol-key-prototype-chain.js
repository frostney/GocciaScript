/*---
description: An object pattern finds a symbol key anywhere on the prototype chain
features: [pattern-matching, Symbol, prototype-chain]
---*/

describe("object patterns with a symbol key", () => {
  const key = Symbol("key");

  test("a symbol inherited from a built-in prototype matches", () => {
    expect([] is { [Symbol.iterator]: _ }).toBe(true);
    expect(new Map() is { [Symbol.iterator]: _ }).toBe(true);
  });

  test("a symbol on an ordinary prototype matches", () => {
    expect(Object.create({ [key]: 1 }) is { [key]: 1 }).toBe(true);
    expect(Object.create(null) is { [key]: _ }).toBe(false);
  });
});
