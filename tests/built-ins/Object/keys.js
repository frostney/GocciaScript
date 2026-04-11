/*---
description: Object.keys
features: [Object.keys]
---*/

test("Object.keys", () => {
  const obj = { a: 1, b: 2, c: 3 };
  expect(Object.keys(obj)).toEqual(["a", "b", "c"]);
});

test("Object.keys with number coerces to empty array", () => {
  expect(Object.keys(42)).toEqual([]);
  expect(Object.keys(0)).toEqual([]);
  expect(Object.keys(-1)).toEqual([]);
});

test("Object.keys with boolean coerces to empty array", () => {
  expect(Object.keys(true)).toEqual([]);
  expect(Object.keys(false)).toEqual([]);
});

test("Object.keys with string returns character indices", () => {
  expect(Object.keys("str")).toEqual(["0", "1", "2"]);
  expect(Object.keys("")).toEqual([]);
  expect(Object.keys("a")).toEqual(["0"]);
});

test("Object.keys throws for null", () => {
  expect(() => Object.keys(null)).toThrow(TypeError);
});

test("Object.keys throws for undefined", () => {
  expect(() => Object.keys(undefined)).toThrow(TypeError);
});
