/*---
description: TC39 Pattern Matching statement integrations
features: [pattern-matching, for-of, async-await]
---*/

describe("pattern matching integrations", () => {
  test("for-of filters iterations and exposes subject plus pattern bindings", () => {
    const seen = [];

    for (const item is { x: const x } of [{ x: 1 }, { y: 2 }, { x: 3 }]) {
      seen.push(item.x);
      seen.push(x);
    }

    expect(seen).toEqual([1, 1, 3, 3]);
    expect(() => item).toThrow(ReferenceError);
    expect(() => x).toThrow(ReferenceError);
  });

  test("for-await-of filters awaited iterations", () => {
    const fn = async () => {
      const seen = [];
      const values = [Promise.resolve({ x: 1 }), Promise.resolve({ y: 2 }), Promise.resolve({ x: 3 })];

      for await (const item is { x: const x } of values) {
        seen.push(item.x);
        seen.push(x);
      }

      return seen;
    };

    return fn().then((seen) => {
      expect(seen).toEqual([1, 1, 3, 3]);
    });
  });

  test("pattern catches handle matches and rethrow mismatches after finally", () => {
    const seen = [];

    try {
      throw { code: 5 };
    } catch (error is { code: const code }) {
      seen.push(error.code);
      seen.push(code);
    }

    try {
      try {
        throw { other: 1 };
      } catch (error is { code: const code }) {
        seen.push(code);
      } finally {
        seen.push("finally");
      }
    } catch (outer) {
      seen.push("outer");
    }

    expect(seen).toEqual([5, 5, "finally", "outer"]);
  });
});
