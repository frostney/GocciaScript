/*---
description: Typeof operator returns correct type names
features: [typeof-operator]
---*/

test("typeof operator", () => {
  expect(typeof 42).toBe("number");
  expect(typeof "hello").toBe("string");
  expect(typeof true).toBe("boolean");
  expect(typeof undefined).toBe("undefined");
  expect(typeof null).toBe("object"); // JavaScript quirk
  expect(typeof {}).toBe("object");
  expect(typeof []).toBe("object");
  expect(typeof (() => {})).toBe("function");
});

test("typeof symbol", () => {
  const sym = Symbol("test");
  expect(typeof sym).toBe("symbol");
  expect(typeof Symbol()).toBe("symbol");
  expect(typeof Symbol.iterator).toBe("symbol");
});

test("typeof NaN is number", () => {
  expect(typeof NaN).toBe("number");
});

test("typeof Infinity is number", () => {
  expect(typeof Infinity).toBe("number");
});
