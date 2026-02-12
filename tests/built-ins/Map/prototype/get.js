/*---
description: Map.prototype.get returns the value for a given key
features: [Map.prototype.get]
---*/

test("get returns value for existing key", () => {
  const map = new Map([["key", "value"]]);
  expect(map.get("key")).toBe("value");
});

test("get returns undefined for non-existing key", () => {
  const map = new Map([["key", "value"]]);
  expect(map.get("missing")).toBeUndefined();
});

test("get with numeric keys", () => {
  const map = new Map([[1, "one"], [2, "two"]]);
  expect(map.get(1)).toBe("one");
  expect(map.get(2)).toBe("two");
  expect(map.get("1")).toBeUndefined();
});

test("get with NaN key", () => {
  const map = new Map([[NaN, "nan-value"]]);
  expect(map.get(NaN)).toBe("nan-value");
});

test("get with null and undefined keys", () => {
  const map = new Map([[null, "null-val"], [undefined, "undef-val"]]);
  expect(map.get(null)).toBe("null-val");
  expect(map.get(undefined)).toBe("undef-val");
});

test("get with object key uses reference equality", () => {
  const key = { id: 1 };
  const map = new Map([[key, "found"]]);
  expect(map.get(key)).toBe("found");
  expect(map.get({ id: 1 })).toBeUndefined();
});

test("get on empty Map", () => {
  const map = new Map();
  expect(map.get("anything")).toBeUndefined();
});
