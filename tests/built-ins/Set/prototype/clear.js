/*---
description: Set.prototype.clear removes all values from the Set
features: [Set.prototype.clear]
---*/

test("clear removes all values", () => {
  const set = new Set([1, 2, 3, 4, 5]);
  expect(set.size).toBe(5);
  set.clear();
  expect(set.size).toBe(0);
  expect(set.has(1)).toBe(false);
  expect(set.has(5)).toBe(false);
});

test("clear on empty Set", () => {
  const set = new Set();
  set.clear();
  expect(set.size).toBe(0);
});

test("add after clear", () => {
  const set = new Set([1, 2, 3]);
  set.clear();
  set.add(4);
  expect(set.size).toBe(1);
  expect(set.has(4)).toBe(true);
  expect(set.has(1)).toBe(false);
});
