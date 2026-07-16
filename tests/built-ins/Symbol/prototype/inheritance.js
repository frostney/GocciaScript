/*---
description: Symbol prototype inheritance and constructor link
features: [Symbol]
---*/

test("inherits from Object.prototype", () => {
  expect(Object.getPrototypeOf(Symbol.prototype)).toBe(Object.prototype);
});

test("links back to Symbol", () => {
  expect(Symbol.prototype.constructor).toBe(Symbol);
});

test("is the prototype used for symbol primitives and wrappers", () => {
  expect(Object.getPrototypeOf(Symbol("x"))).toBe(Symbol.prototype);
  expect(Object.getPrototypeOf(Object(Symbol("x")))).toBe(Symbol.prototype);
});
