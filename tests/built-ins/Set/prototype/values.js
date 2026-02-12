/*---
description: Set.prototype.values returns values as an array
features: [Set.prototype.values]
---*/

test("values returns all values in insertion order", () => {
  const set = new Set([3, 1, 4, 1, 5]);
  const vals = set.values();
  expect(vals).toEqual([3, 1, 4, 5]);
});

test("values on empty Set", () => {
  const set = new Set();
  expect(set.values()).toEqual([]);
});

test("values with mixed types", () => {
  const set = new Set([1, "hello", true, null]);
  const vals = set.values();
  expect(vals).toEqual([1, "hello", true, null]);
});
