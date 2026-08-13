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

test("Object.keys omits symbol-keyed properties", () => {
  const symbol = Symbol("hidden");
  const object = { visible: true, [symbol]: "secret" };

  expect(Object.keys(object)).toEqual(["visible"]);
  expect(Object.getOwnPropertySymbols(object)).toEqual([symbol]);
});

test("Object.keys sorts array-index keys inserted in descending order", () => {
  const object = {};
  for (const index of [3, 2, 1, 0]) object[index] = index;

  expect(Object.keys(object)).toEqual(["0", "1", "2", "3"]);
});

test("Object.keys sorts array-index keys inserted in random order", () => {
  const object = {};
  for (const index of [42, 7, 19, 0, 100, 3, 58, 27, 12]) object[index] = index;

  expect(Object.keys(object)).toEqual([
    "0",
    "3",
    "7",
    "12",
    "19",
    "27",
    "42",
    "58",
    "100",
  ]);
});

test("Object.keys sorts a hundred array-index keys inserted in permuted order", () => {
  const count = 100;
  // (index * 37) % count visits every index exactly once because 37 and 100
  // are coprime, giving a deterministic unordered insertion sequence.
  const permuted = Array.from({ length: count }, (_, index) => (index * 37) % count);
  const object = {};
  for (const index of permuted) object[index] = index;

  expect(Object.keys(object)).toEqual(
    Array.from({ length: count }, (_, index) => String(index)),
  );
});

test("Object.keys puts array-index keys first and keeps string keys in insertion order", () => {
  const object = { zebra: 1 };
  object[5] = 5;
  object.apple = 2;
  object[2] = 2;
  object["01"] = "leading zero is not an index";
  object[4294967295] = "2^32-1 is not an index";
  object[10] = 10;
  object.mango = 3;

  expect(Object.keys(object)).toEqual([
    "2",
    "5",
    "10",
    "zebra",
    "apple",
    "01",
    "4294967295",
    "mango",
  ]);
});

test("Object.keys sorts many array-index keys inserted in descending order", () => {
  const count = 5000;
  const descending = Array.from({ length: count }, (_, index) => count - 1 - index);
  const object = {};
  for (const index of descending) object[index] = index;

  const keys = Object.keys(object);
  expect(keys.length).toBe(count);
  expect(keys.every((key, index) => key === String(index))).toBe(true);
});

test("Object.keys throws for null", () => {
  expect(() => Object.keys(null)).toThrow(TypeError);
});

test("Object.keys throws for undefined", () => {
  expect(() => Object.keys(undefined)).toThrow(TypeError);
});

test("Object.keys property descriptor on Object", () => {
  const desc = Object.getOwnPropertyDescriptor(Object, "keys");
  expect(desc.writable).toBe(true);
  expect(desc.enumerable).toBe(false);
  expect(desc.configurable).toBe(true);
});
