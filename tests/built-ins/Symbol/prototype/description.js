/*---
description: Symbol.prototype.description
features: [Symbol]
---*/

test("returns the description from primitive and boxed symbols", () => {
  expect(Symbol("foo").description).toBe("foo");
  expect(Symbol().description).toBeUndefined();
  expect(Object(Symbol("boxed")).description).toBe("boxed");
});

test("is inherited rather than an own primitive property", () => {
  const symbol = Symbol("description");
  expect(Object.hasOwn(symbol, "description")).toBe(false);
});

test("rejects incompatible receivers", () => {
  const getter = Object.getOwnPropertyDescriptor(Symbol.prototype, "description").get;
  expect(() => getter.call({})).toThrow(TypeError);
  expect(() => getter.call(null)).toThrow(TypeError);
});
