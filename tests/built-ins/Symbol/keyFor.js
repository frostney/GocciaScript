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

test("Symbol.keyFor throws TypeError for non-symbol values", () => {
  expect(() => { Symbol.keyFor(null); }).toThrow(TypeError);
  expect(() => { Symbol.keyFor(undefined); }).toThrow(TypeError);
  expect(() => { Symbol.keyFor(0); }).toThrow(TypeError);
  expect(() => { Symbol.keyFor(false); }).toThrow(TypeError);
  expect(() => { Symbol.keyFor("string"); }).toThrow(TypeError);
  expect(() => { Symbol.keyFor({}); }).toThrow(TypeError);
});

test("Symbol.keyFor throws TypeError with no arguments", () => {
  expect(() => { Symbol.keyFor(); }).toThrow(TypeError);
});
