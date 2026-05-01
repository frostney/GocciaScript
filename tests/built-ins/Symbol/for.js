/*---
description: Symbol.for
features: [Symbol.for]
---*/

test("Symbol.for", () => {
  const symbol1 = Symbol.for("foo");
  const symbol2 = Symbol.for("foo");

  expect(symbol1).toBe(symbol2);
  expect(String(symbol1)).toBe("Symbol(foo)");
  expect(String(symbol2)).toBe("Symbol(foo)");
});

test("Symbol.for with different keys", () => {
  const s1 = Symbol.for("a");
  const s2 = Symbol.for("b");
  const s3 = Symbol.for("a");

  expect(s1).toBe(s3);
  expect(s1).not.toBe(s2);
});

test("Symbol.for key coercion", () => {
  // All arguments are coerced with ToString.
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

test("Symbol.for key coercion uses object toString", () => {
  const calls = [];
  const key = {
    valueOf() {
      calls.push("valueOf");
      return {};
    },
    toString() {
      calls.push("toString");
      return "object-key";
    },
  };

  const symbol = Symbol.for(key);
  expect(symbol).toBe(Symbol.for("object-key"));
  expect(calls).toEqual(["toString"]);
});

test("Symbol.for rejects symbol keys", () => {
  expect(() => Symbol.for(Symbol("key"))).toThrow(TypeError);
});
