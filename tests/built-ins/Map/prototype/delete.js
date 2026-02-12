/*---
description: Map.prototype.delete removes a key-value pair from the Map
features: [Map.prototype.delete]
---*/

test("delete returns true when key existed", () => {
  const map = new Map([["a", 1], ["b", 2]]);
  expect(map.delete("a")).toBe(true);
  expect(map.size).toBe(1);
  expect(map.has("a")).toBe(false);
  expect(map.get("a")).toBeUndefined();
});

test("delete returns false when key did not exist", () => {
  const map = new Map([["a", 1]]);
  expect(map.delete("b")).toBe(false);
  expect(map.size).toBe(1);
});

test("delete with NaN key", () => {
  const map = new Map([[NaN, "value"]]);
  expect(map.delete(NaN)).toBe(true);
  expect(map.size).toBe(0);
});

test("delete does not affect other entries", () => {
  const map = new Map([["a", 1], ["b", 2], ["c", 3]]);
  map.delete("b");
  expect(map.has("a")).toBe(true);
  expect(map.has("b")).toBe(false);
  expect(map.has("c")).toBe(true);
});

test("delete on empty Map", () => {
  const map = new Map();
  expect(map.delete("a")).toBe(false);
});
