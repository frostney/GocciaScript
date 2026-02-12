/*---
description: Map.prototype.entries returns [key, value] pairs as an array
features: [Map.prototype.entries]
---*/

test("entries returns key-value pairs in insertion order", () => {
  const map = new Map([["a", 1], ["b", 2], ["c", 3]]);
  expect(map.entries()).toEqual([["a", 1], ["b", 2], ["c", 3]]);
});

test("entries on empty Map", () => {
  const map = new Map();
  expect(map.entries()).toEqual([]);
});

test("entries with non-string keys", () => {
  const map = new Map([[1, "one"], [true, "yes"]]);
  expect(map.entries()).toEqual([[1, "one"], [true, "yes"]]);
});
