/*---
description: Symbol.customMatcher participates in value patterns
features: [pattern-matching, Symbol.customMatcher]
---*/

describe("Symbol.customMatcher", () => {
  test("value patterns call custom matcher methods", () => {
    const hints = [];
    const Even = {
      [Symbol.customMatcher](subject, hint) {
        hints.push(hint.matchType);
        return subject % 2 === 0;
      }
    };

    expect(4 is Even).toBe(true);
    expect(5 is Even).toBe(false);
    expect(hints).toEqual(["boolean", "boolean"]);
  });

  test("extractor patterns call custom matcher methods", () => {
    const hints = [];
    const Pair = {
      [Symbol.customMatcher](subject, hint) {
        hints.push(hint.matchType);
        if (subject.kind !== "pair") {
          return false;
        }
        return [subject.left, subject.right];
      }
    };

    let result = 0;
    if ({ kind: "pair", left: 2, right: 3 } is Pair(const left, const right)) {
      result = left + right;
    }

    expect(result).toBe(5);
    expect({ kind: "other" } is Pair(_, _)).toBe(false);
    expect(hints).toEqual(["extractor", "extractor"]);
    expect(() => left).toThrow(ReferenceError);
  });
});
