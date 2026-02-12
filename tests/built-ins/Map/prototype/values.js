/*---
description: Map.prototype.values returns values as an array
features: [Map.prototype.values]
---*/

test("values returns all values in insertion order", () => {
  const map = new Map([["a", 1], ["b", 2], ["c", 3]]);
  expect(map.values()).toEqual([1, 2, 3]);
});

test("values on empty Map", () => {
  const map = new Map();
  expect(map.values()).toEqual([]);
});

test("values with duplicate values across keys", () => {
  const map = new Map([["a", 1], ["b", 1], ["c", 1]]);
  expect(map.values()).toEqual([1, 1, 1]);
});
