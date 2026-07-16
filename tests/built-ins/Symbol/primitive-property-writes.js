/*---
description: Symbol primitive property writes
features: [Symbol]
---*/

test("rejects string-keyed and symbol-keyed writes", () => {
  const symbol = Symbol("target");
  const key = Symbol("key");

  expect(() => {
    symbol.extra = "value";
  }).toThrow(TypeError);
  expect(() => {
    symbol[key] = "value";
  }).toThrow(TypeError);
  expect(() => {
    symbol[Symbol.toStringTag] = "tag";
  }).toThrow(TypeError);
});
