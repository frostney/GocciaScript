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
});
