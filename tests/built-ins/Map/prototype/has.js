/*---
description: Map.prototype.has checks if a key exists in the Map
features: [Map.prototype.has]
---*/

test("has returns true for existing key", () => {
  const map = new Map([["a", 1], ["b", 2]]);
  expect(map.has("a")).toBe(true);
  expect(map.has("b")).toBe(true);
});

test("has returns false for non-existing key", () => {
  const map = new Map([["a", 1]]);
  expect(map.has("b")).toBe(false);
  expect(map.has(1)).toBe(false);
});

test("has with NaN key", () => {
  const map = new Map([[NaN, "value"]]);
  expect(map.has(NaN)).toBe(true);
});

test("has distinguishes types", () => {
  const map = new Map([[1, "number"]]);
  expect(map.has(1)).toBe(true);
  expect(map.has("1")).toBe(false);
});

test("has on empty Map", () => {
  const map = new Map();
  expect(map.has("anything")).toBe(false);
});

test("has after delete", () => {
  const map = new Map([["a", 1]]);
  expect(map.has("a")).toBe(true);
  map.delete("a");
  expect(map.has("a")).toBe(false);
});
