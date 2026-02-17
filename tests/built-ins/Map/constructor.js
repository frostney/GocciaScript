/*---
description: Map constructor creates Map objects correctly
features: [Map]
---*/

test("empty Map constructor", () => {
  const map = new Map();
  expect(map.size).toBe(0);
});

test("Map constructor with entries array", () => {
  const map = new Map([
    ["a", 1],
    ["b", 2],
    ["c", 3],
  ]);
  expect(map.size).toBe(3);
  expect(map.get("a")).toBe(1);
  expect(map.get("b")).toBe(2);
  expect(map.get("c")).toBe(3);
});

test("Map constructor with duplicate keys keeps last value", () => {
  const map = new Map([
    ["a", 1],
    ["b", 2],
    ["a", 3],
  ]);
  expect(map.size).toBe(2);
  expect(map.get("a")).toBe(3);
  expect(map.get("b")).toBe(2);
});

test("Map constructor with non-string keys", () => {
  const objKey = { id: 1 };
  const map = new Map([
    [1, "number"],
    [true, "boolean"],
    [null, "null"],
    [objKey, "object"],
  ]);
  expect(map.size).toBe(4);
  expect(map.get(1)).toBe("number");
  expect(map.get(true)).toBe("boolean");
  expect(map.get(null)).toBe("null");
  expect(map.get(objKey)).toBe("object");
});

test("Map constructor with NaN key deduplication", () => {
  const map = new Map([
    [NaN, "first"],
    [NaN, "second"],
  ]);
  expect(map.size).toBe(1);
  expect(map.get(NaN)).toBe("second");
});

test("Map.prototype is an object", () => {
  expect(typeof Map.prototype).toBe("object");
});

test("new Map instanceof Map", () => {
  const m = new Map();
  expect(m instanceof Map).toBe(true);
});

test("Map.prototype.constructor is Map", () => {
  expect(Map.prototype.constructor).toBe(Map);
});

test("instance constructor is Map", () => {
  const m = new Map();
  expect(m.constructor).toBe(Map);
});
