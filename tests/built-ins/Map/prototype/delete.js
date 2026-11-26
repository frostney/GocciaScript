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

test("throws TypeError when called on non-Map", () => {
  const del = Map.prototype.delete;
  expect(() => del.call(Map.prototype, "key")).toThrow(TypeError);
  expect(() => del.call({}, "key")).toThrow(TypeError);
  expect(() => del.call(new Set(), "key")).toThrow(TypeError);
});

test("many deletes keep the order of the entries left, with and without an iterator open", () => {
  const range = (from, to) => Array.from({ length: to - from }, (_, k) => from + k);
  const map = new Map();
  for (const i of range(0, 300)) map.set(i, i * 2);
  for (const i of range(0, 300)) if (i % 4 !== 0) map.delete(i);
  const left = [...map.keys()];
  expect(left.length).toBe(75);
  expect(left[0]).toBe(0);
  expect(left[74]).toBe(296);
  expect(map.get(296)).toBe(592);

  const open = new Map();
  for (const i of range(0, 300)) open.set(i, i);
  const it = open.keys();
  expect(it.next().value).toBe(0);
  for (const i of range(1, 290)) open.delete(i);
  expect([...it]).toEqual(range(290, 300));
  for (const i of range(0, 1000)) { open.set("k" + i, i); open.delete("k" + i); }
  expect(open.size).toBe(11);
  expect([...open.keys()]).toEqual([0, ...range(290, 300)]);
});
