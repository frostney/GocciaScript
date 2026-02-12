/*---
description: Set.prototype.delete removes a value from the Set
features: [Set.prototype.delete]
---*/

test("delete returns true when value existed", () => {
  const set = new Set([1, 2, 3]);
  expect(set.delete(2)).toBe(true);
  expect(set.size).toBe(2);
  expect(set.has(2)).toBe(false);
});

test("delete returns false when value did not exist", () => {
  const set = new Set([1, 2, 3]);
  expect(set.delete(4)).toBe(false);
  expect(set.size).toBe(3);
});

test("delete with NaN", () => {
  const set = new Set([NaN, 1, 2]);
  expect(set.delete(NaN)).toBe(true);
  expect(set.size).toBe(2);
  expect(set.has(NaN)).toBe(false);
});

test("delete does not affect other values", () => {
  const set = new Set([1, 2, 3, 4, 5]);
  set.delete(3);
  expect(set.has(1)).toBe(true);
  expect(set.has(2)).toBe(true);
  expect(set.has(3)).toBe(false);
  expect(set.has(4)).toBe(true);
  expect(set.has(5)).toBe(true);
});

test("delete on empty Set", () => {
  const set = new Set();
  expect(set.delete(1)).toBe(false);
});
