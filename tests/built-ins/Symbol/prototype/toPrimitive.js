/*---
description: Symbol.prototype[Symbol.toPrimitive]
features: [Symbol, Symbol.toPrimitive]
---*/

test("returns the underlying symbol regardless of the hint", () => {
  const symbol = Symbol("foo");
  const method = Symbol.prototype[Symbol.toPrimitive];
  expect(method.call(symbol, "string")).toBe(symbol);
  expect(method.call(symbol, "number")).toBe(symbol);
  expect(method.call(symbol, "default")).toBe(symbol);
  expect(method.call(Object(symbol))).toBe(symbol);
});

test("rejects incompatible receivers", () => {
  const method = Symbol.prototype[Symbol.toPrimitive];
  for (const receiver of [42, "hello", {}, null, undefined]) {
    expect(() => method.call(receiver, "string")).toThrow(TypeError);
  }
});
