/*---
description: Map.prototype.clear removes all entries from the Map
features: [Map.prototype.clear]
---*/

test("clear removes all entries", () => {
  const map = new Map([["a", 1], ["b", 2], ["c", 3]]);
  expect(map.size).toBe(3);
  map.clear();
  expect(map.size).toBe(0);
  expect(map.has("a")).toBe(false);
  expect(map.has("b")).toBe(false);
  expect(map.has("c")).toBe(false);
});

test("clear on empty Map", () => {
  const map = new Map();
  map.clear();
  expect(map.size).toBe(0);
});

test("set after clear", () => {
  const map = new Map([["a", 1]]);
  map.clear();
  map.set("b", 2);
  expect(map.size).toBe(1);
  expect(map.has("a")).toBe(false);
  expect(map.has("b")).toBe(true);
});
