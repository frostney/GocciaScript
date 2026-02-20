/*---
description: Set constructor creates Set objects correctly
features: [Set]
---*/

test("empty Set constructor", () => {
  const set = new Set();
  expect(set.size).toBe(0);
});

test("Set constructor with array", () => {
  const set = new Set([1, 2, 3]);
  expect(set.size).toBe(3);
  expect(set.has(1)).toBe(true);
  expect(set.has(2)).toBe(true);
  expect(set.has(3)).toBe(true);
});

test("Set constructor removes duplicates", () => {
  const set = new Set([1, 2, 2, 3, 3, 3]);
  expect(set.size).toBe(3);
  expect(set.has(1)).toBe(true);
  expect(set.has(2)).toBe(true);
  expect(set.has(3)).toBe(true);
});

test("Set constructor with string values", () => {
  const set = new Set(["a", "b", "c", "a"]);
  expect(set.size).toBe(3);
  expect(set.has("a")).toBe(true);
  expect(set.has("b")).toBe(true);
  expect(set.has("c")).toBe(true);
});

test("Set constructor with mixed types", () => {
  const set = new Set([1, "1", true, null, undefined]);
  expect(set.size).toBe(5);
  expect(set.has(1)).toBe(true);
  expect(set.has("1")).toBe(true);
  expect(set.has(true)).toBe(true);
  expect(set.has(null)).toBe(true);
  expect(set.has(undefined)).toBe(true);
});

test("Set constructor with NaN deduplication", () => {
  const set = new Set([NaN, NaN, NaN]);
  expect(set.size).toBe(1);
  expect(set.has(NaN)).toBe(true);
});

test("Set constructor with another Set", () => {
  const original = new Set([1, 2, 3]);
  const copy = new Set(original);
  expect(copy.size).toBe(3);
  expect(copy.has(1)).toBe(true);
  expect(copy.has(2)).toBe(true);
  expect(copy.has(3)).toBe(true);
  expect(copy).not.toBe(original);
});

test("Set treats -0 and +0 as same value", () => {
  const set = new Set([-0, 0]);
  expect(set.size).toBe(1);
  expect(set.has(0)).toBe(true);
  expect(set.has(-0)).toBe(true);
});

test("Set.prototype is an object", () => {
  expect(typeof Set.prototype).toBe("object");
});

test("new Set instanceof Set", () => {
  const s = new Set();
  expect(s instanceof Set).toBe(true);
});

test("Set.prototype.constructor is Set", () => {
  expect(Set.prototype.constructor).toBe(Set);
});

test("instance constructor is Set", () => {
  const s = new Set();
  expect(s.constructor).toBe(Set);
});
