/*---
description: Object.getOwnPropertySymbols returns own symbol-keyed properties
features: [Object.getOwnPropertySymbols]
---*/

test("returns empty array for object without symbol properties", () => {
  const obj = { a: 1, b: 2 };
  expect(Object.getOwnPropertySymbols(obj)).toEqual([]);
});

test("returns symbol keys", () => {
  const s1 = Symbol("first");
  const s2 = Symbol("second");
  const obj = { [s1]: 1, [s2]: 2, regular: 3 };
  const symbols = Object.getOwnPropertySymbols(obj);
  expect(symbols.length).toBe(2);
  expect(symbols).toContain(s1);
  expect(symbols).toContain(s2);
});

test("does not include string-keyed properties", () => {
  const sym = Symbol("key");
  const obj = { [sym]: "symbol", stringKey: "string" };
  const symbols = Object.getOwnPropertySymbols(obj);
  expect(symbols.length).toBe(1);
  expect(symbols[0]).toBe(sym);
});

test("returns empty array for empty object", () => {
  expect(Object.getOwnPropertySymbols({})).toEqual([]);
});

test("works with Symbol.iterator", () => {
  const obj = { [Symbol.iterator]: "iter" };
  const symbols = Object.getOwnPropertySymbols(obj);
  expect(symbols.length).toBe(1);
  expect(symbols[0]).toBe(Symbol.iterator);
});
