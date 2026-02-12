/*---
description: Map.prototype.keys returns keys as an array
features: [Map.prototype.keys]
---*/

test("keys returns all keys in insertion order", () => {
  const map = new Map([["c", 3], ["a", 1], ["b", 2]]);
  expect([...map.keys()]).toEqual(["c", "a", "b"]);
});

test("keys on empty Map", () => {
  const map = new Map();
  expect([...map.keys()]).toEqual([]);
});

test("keys with non-string keys", () => {
  const map = new Map([[1, "one"], [true, "yes"], [null, "nothing"]]);
  const keys = [...map.keys()];
  expect(keys).toEqual([1, true, null]);
});
