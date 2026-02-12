/*---
description: Symbol.for
features: [Symbol.for]
---*/

test("Symbol.for", () => {
  const symbol1 = Symbol.for("foo");
  const symbol2 = Symbol.for("foo");

  expect(symbol1).toBe(symbol2);
  expect(`${symbol1}`).toBe("Symbol(foo)");
  expect(`${symbol2}`).toBe("Symbol(foo)");
});

test("Symbol.for with different keys", () => {
  const s1 = Symbol.for("a");
  const s2 = Symbol.for("b");
  const s3 = Symbol.for("a");

  expect(s1).toBe(s3);
  expect(s1).not.toBe(s2);
});

test("Symbol.for key coercion", () => {
  // All arguments are coerced to string via ToStringLiteral
  const sNull = Symbol.for("null");
  const sUndefined = Symbol.for("undefined");
  const sZero = Symbol.for("0");
  const sFalse = Symbol.for("false");

  // These should match their string equivalents
  expect(Symbol.for(null)).toBe(sNull);
  expect(Symbol.for(undefined)).toBe(sUndefined);
  expect(Symbol.for(0)).toBe(sZero);
  expect(Symbol.for(false)).toBe(sFalse);
});
