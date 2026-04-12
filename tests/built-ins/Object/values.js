/*---
description: Object.values
features: [Object.values]
---*/

test("Object.values", () => {
  const obj = { a: 1, b: 2, c: 3 };
  expect(Object.values(obj)).toEqual([1, 2, 3]);
});

test("Object.values with number coerces to empty array", () => {
  expect(Object.values(42)).toEqual([]);
  expect(Object.values(0)).toEqual([]);
});

test("Object.values with boolean coerces to empty array", () => {
  expect(Object.values(true)).toEqual([]);
  expect(Object.values(false)).toEqual([]);
});

test("Object.values with string returns character values", () => {
  expect(Object.values("str")).toEqual(["s", "t", "r"]);
  expect(Object.values("")).toEqual([]);
});

test("Object.values throws for null", () => {
  expect(() => Object.values(null)).toThrow(TypeError);
});

test("Object.values throws for undefined", () => {
  expect(() => Object.values(undefined)).toThrow(TypeError);
});
