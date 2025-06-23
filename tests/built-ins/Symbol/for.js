/*---
description: Symbol.for
features: [Symbol.for]
---*/

test.skip("Symbol.for", () => {
  const symbol1 = Symbol.for("foo");
  const symbol2 = Symbol.for("foo");

  expect(symbol1).toBe(symbol2);
  expect(symbol1.toString()).toBe("Symbol(foo)");
  expect(symbol2.toString()).toBe("Symbol(foo)");
});

test.skip("Symbol.for with edge cases", () => {
  expect(Symbol.for(null)).toBe(Symbol.for(undefined));
  expect(Symbol.for(0)).toBe(Symbol.for(false));
  expect(Symbol.for(1)).toBe(Symbol.for(true));
  expect(Symbol.for(NaN)).toBe(Symbol.for(Infinity));
  expect(Symbol.for(Infinity)).toBe(Symbol.for(NaN));
  expect(Symbol.for(undefined)).toBe(Symbol.for(null));
});
