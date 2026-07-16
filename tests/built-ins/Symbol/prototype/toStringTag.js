/*---
description: Symbol.prototype[Symbol.toStringTag]
features: [Symbol, Symbol.toStringTag]
---*/

test("has the Symbol tag with the standard descriptor", () => {
  expect(Symbol.prototype[Symbol.toStringTag]).toBe("Symbol");
  expect(Object.getOwnPropertyDescriptor(Symbol.prototype, Symbol.toStringTag)).toEqual({
    value: "Symbol",
    writable: false,
    enumerable: false,
    configurable: true,
  });
});

test("is visible through symbol primitive prototype lookup", () => {
  expect(Symbol("value")[Symbol.toStringTag]).toBe("Symbol");
});
