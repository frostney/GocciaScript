/*---
description: Symbol.keyFor
features: [Symbol.keyFor]
---*/

test("Symbol.keyFor", () => {
  const symbol = Symbol.for("foo");
  expect(Symbol.keyFor(symbol)).toBe("foo");
});

test("Symbol.keyFor with non-global symbol", () => {
  const localSymbol = Symbol("local");
  expect(Symbol.keyFor(localSymbol)).toBe(undefined);
});

test("Symbol.keyFor with non-symbol values", () => {
  expect(Symbol.keyFor(null)).toBe(null);
  expect(Symbol.keyFor(undefined)).toBe(undefined);
  expect(Symbol.keyFor(0)).toBe(null);
  expect(Symbol.keyFor(false)).toBe(null);
  expect(Symbol.keyFor("string")).toBe(null);
});
