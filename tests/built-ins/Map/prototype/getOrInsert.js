/*---
description: Map.prototype.getOrInsert returns existing value or inserts default
features: [Map.prototype.getOrInsert]
---*/

const isGocciaScript = typeof GocciaScript !== "undefined";

describe.runIf(isGocciaScript)("Map.prototype.getOrInsert", () => {
  test("returns existing value without overwriting", () => {
    const map = new Map([["key", "original"]]);
    const result = map.getOrInsert("key", "replacement");
    expect(result).toBe("original");
    expect(map.get("key")).toBe("original");
  });

  test("inserts and returns default value when key is absent", () => {
    const map = new Map();
    const result = map.getOrInsert("key", "default");
    expect(result).toBe("default");
    expect(map.get("key")).toBe("default");
    expect(map.size).toBe(1);
  });

  test("works with numeric keys", () => {
    const map = new Map([[1, "one"]]);
    expect(map.getOrInsert(1, "replacement")).toBe("one");
    expect(map.getOrInsert(2, "two")).toBe("two");
    expect(map.size).toBe(2);
  });

  test("works with NaN key", () => {
    const map = new Map();
    map.getOrInsert(NaN, "nan-value");
    expect(map.get(NaN)).toBe("nan-value");
    expect(map.getOrInsert(NaN, "other")).toBe("nan-value");
  });

  test("works with null and undefined keys", () => {
    const map = new Map();
    expect(map.getOrInsert(null, "null-val")).toBe("null-val");
    expect(map.getOrInsert(undefined, "undef-val")).toBe("undef-val");
    expect(map.size).toBe(2);
    expect(map.getOrInsert(null, "other")).toBe("null-val");
  });

  test("works with object keys using reference equality", () => {
    const key = { id: 1 };
    const map = new Map();
    map.getOrInsert(key, "found");
    expect(map.get(key)).toBe("found");
    expect(map.getOrInsert({ id: 1 }, "other")).toBe("other");
    expect(map.size).toBe(2);
  });

  test("default value can be any type", () => {
    const map = new Map();
    const arr = [];
    expect(map.getOrInsert("arr", arr)).toBe(arr);
    expect(map.getOrInsert("num", 42)).toBe(42);
    expect(map.getOrInsert("bool", false)).toBe(false);
    expect(map.getOrInsert("null", null)).toBe(null);
  });

  test("handles default values for grouping pattern", () => {
    const map = new Map();
    map.getOrInsert("a", []).push(1);
    map.getOrInsert("a", []).push(2);
    map.getOrInsert("b", []).push(3);
    expect(map.get("a").length).toBe(2);
    expect(map.get("b").length).toBe(1);
  });

  test("handles counter pattern", () => {
    const map = new Map();
    const keys = ["a", "b", "a", "c", "b", "a"];
    keys.forEach((key) => {
      map.set(key, map.getOrInsert(key, 0) + 1);
    });
    expect(map.get("a")).toBe(3);
    expect(map.get("b")).toBe(2);
    expect(map.get("c")).toBe(1);
  });
});
