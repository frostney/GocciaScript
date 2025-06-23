/*---
description: Symbol.keyFor
features: [Symbol.keyFor]
---*/

test.skip("Symbol.keyFor", () => {
  const symbol = Symbol.for("foo");
  expect(Symbol.keyFor(symbol)).toBe("foo");
});

test.skip("Symbol.keyFor with edge cases", () => {
  expect(Symbol.keyFor(null)).toBe(null);
  expect(Symbol.keyFor(undefined)).toBe(undefined);
  expect(Symbol.keyFor(0)).toBe(null);
  expect(Symbol.keyFor(false)).toBe(null);
  expect(Symbol.keyFor(1)).toBe(null);
  expect(Symbol.keyFor(true)).toBe(null);
  expect(Symbol.keyFor(NaN)).toBe(null);
  expect(Symbol.keyFor(Infinity)).toBe(null);
});
