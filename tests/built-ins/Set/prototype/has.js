/*---
description: Set.prototype.has checks if a value exists in the Set
features: [Set.prototype.has]
---*/

test("has returns true for existing values", () => {
  const set = new Set([1, 2, 3]);
  expect(set.has(1)).toBe(true);
  expect(set.has(2)).toBe(true);
  expect(set.has(3)).toBe(true);
});

test("has returns false for non-existing values", () => {
  const set = new Set([1, 2, 3]);
  expect(set.has(4)).toBe(false);
  expect(set.has(0)).toBe(false);
  expect(set.has("1")).toBe(false);
});

test("has with NaN", () => {
  const set = new Set([NaN]);
  expect(set.has(NaN)).toBe(true);
});

test("has with null and undefined", () => {
  const set = new Set([null]);
  expect(set.has(null)).toBe(true);
  expect(set.has(undefined)).toBe(false);
});

test("has with object identity", () => {
  const obj = { key: "value" };
  const set = new Set([obj]);
  expect(set.has(obj)).toBe(true);
  expect(set.has({ key: "value" })).toBe(false);
});

test("has on empty Set", () => {
  const set = new Set();
  expect(set.has(1)).toBe(false);
  expect(set.has(undefined)).toBe(false);
});
