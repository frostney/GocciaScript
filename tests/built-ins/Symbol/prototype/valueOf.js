/*---
description: Symbol.prototype.valueOf
features: [Symbol]
---*/

test("returns the underlying symbol from primitive and boxed receivers", () => {
  const symbol = Symbol("foo");
  expect(Symbol.prototype.valueOf.call(symbol)).toBe(symbol);
  expect(Symbol.prototype.valueOf.call(Object(symbol))).toBe(symbol);
});

test("rejects incompatible receivers", () => {
  for (const receiver of [42, "hello", {}, null, undefined]) {
    expect(() => Symbol.prototype.valueOf.call(receiver)).toThrow(TypeError);
  }
});

test("is not constructable", () => {
  expect(() => new Symbol.prototype.valueOf()).toThrow(TypeError);
});
