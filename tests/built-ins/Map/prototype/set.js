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

test("set normalizes a -0 key to +0", () => {
  const map = new Map();
  map.set(-0, "neg");
  expect(map.has(0)).toBe(true);
  expect(map.get(0)).toBe("neg");
  expect(map.size).toBe(1);
  const key = [...map.keys()][0];
  expect(Object.is(key, 0)).toBe(true);
  expect(Object.is(key, -0)).toBe(false);
});

test("set treats +0 and -0 as the same key", () => {
  const map = new Map();
  map.set(0, "a");
  map.set(-0, "b");
  expect(map.size).toBe(1);
  expect(map.get(0)).toBe("b");
});

test("set keys by string content, not string identity", () => {
  const map = new Map();
  map.set("a" + "b", 1);
  map.set("ab", 2);
  expect(map.size).toBe(1);
  expect(map.get("ab")).toBe(2);
});

test("set treats number 1 and string '1' as distinct keys", () => {
  const map = new Map();
  map.set(1, "num");
  map.set("1", "str");
  expect(map.size).toBe(2);
  expect(map.get(1)).toBe("num");
  expect(map.get("1")).toBe("str");
});

test("set uses reference identity for object keys", () => {
  const map = new Map();
  const a = {};
  map.set(a, 1);
  map.set(a, 2);
  map.set({}, 3);
  expect(map.size).toBe(2);
  expect(map.get(a)).toBe(2);
});

test("throws TypeError when called on non-Map", () => {
  const set = Map.prototype.set;
  expect(() => set.call(Map.prototype, "k", "v")).toThrow(TypeError);
  expect(() => set.call({}, "k", "v")).toThrow(TypeError);
  expect(() => set.call(new Set(), "k", "v")).toThrow(TypeError);
});
