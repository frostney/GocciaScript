/*---
description: Map.prototype.set adds or updates a key-value pair
features: [Map.prototype.set]
---*/

test("set returns the Map for chaining", () => {
  const map = new Map();
  const result = map.set("a", 1);
  expect(result).toBe(map);
});

test("set chaining", () => {
  const map = new Map();
  map.set("a", 1).set("b", 2).set("c", 3);
  expect(map.size).toBe(3);
  expect(map.get("a")).toBe(1);
  expect(map.get("b")).toBe(2);
  expect(map.get("c")).toBe(3);
});

test("set overwrites existing key", () => {
  const map = new Map([["a", 1]]);
  map.set("a", 99);
  expect(map.size).toBe(1);
  expect(map.get("a")).toBe(99);
});

test("set preserves insertion order", () => {
  const map = new Map();
  map.set("c", 3);
  map.set("a", 1);
  map.set("b", 2);
  const keys = [...map.keys()];
  expect(keys).toEqual(["c", "a", "b"]);
});

test("set with NaN key", () => {
  const map = new Map();
  map.set(NaN, "first");
  map.set(NaN, "second");
  expect(map.size).toBe(1);
  expect(map.get(NaN)).toBe("second");
});
