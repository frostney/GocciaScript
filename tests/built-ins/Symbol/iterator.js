/*---
description: Symbol.iterator is a well-known symbol
features: [Symbol.iterator]
---*/

test("Symbol.iterator exists", () => {
  expect(Symbol.iterator).not.toBeUndefined();
});

test("Symbol.iterator is a symbol", () => {
  expect(typeof Symbol.iterator).toBe("symbol");
});

test("Symbol.iterator is always the same reference", () => {
  const iter1 = Symbol.iterator;
  const iter2 = Symbol.iterator;
  expect(iter1).toBe(iter2);
});

test("Symbol.iterator can be used as a property key", () => {
  const obj = { [Symbol.iterator]: "iterator-value" };
  expect(obj[Symbol.iterator]).toBe("iterator-value");
});
