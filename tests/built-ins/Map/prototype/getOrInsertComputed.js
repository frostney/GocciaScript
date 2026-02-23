/*---
description: Map.prototype.getOrInsertComputed returns existing value or computes and inserts
features: [Map.prototype.getOrInsertComputed]
---*/

const isGocciaScript = typeof GocciaScript !== "undefined";

describe.runIf(isGocciaScript)("Map.prototype.getOrInsertComputed", () => {
  test("returns existing value without calling callback", () => {
    const map = new Map([["key", "original"]]);
    let called = false;
    const result = map.getOrInsertComputed("key", () => {
      called = true;
      return "computed";
    });
    expect(result).toBe("original");
    expect(called).toBe(false);
  });

  test("calls callback and inserts when key is absent", () => {
    const map = new Map();
    const result = map.getOrInsertComputed("key", () => "computed");
    expect(result).toBe("computed");
    expect(map.get("key")).toBe("computed");
    expect(map.size).toBe(1);
  });

  test("callback receives the key as argument", () => {
    const map = new Map();
    let receivedKey;
    map.getOrInsertComputed("myKey", (key) => {
      receivedKey = key;
      return "value";
    });
    expect(receivedKey).toBe("myKey");
  });

  test("throws TypeError for non-callable callback", () => {
    const map = new Map();
    expect(() => map.getOrInsertComputed("key", "not a function")).toThrow(
      TypeError,
    );
    expect(() => map.getOrInsertComputed("key", 42)).toThrow(TypeError);
    expect(() => map.getOrInsertComputed("key", null)).toThrow(TypeError);
    expect(() => map.getOrInsertComputed("key", undefined)).toThrow(TypeError);
  });

  test("works with NaN key", () => {
    const map = new Map();
    map.getOrInsertComputed(NaN, () => "nan-value");
    expect(map.get(NaN)).toBe("nan-value");
    expect(map.getOrInsertComputed(NaN, () => "other")).toBe("nan-value");
  });

  test("works with object keys", () => {
    const key = { id: 1 };
    const map = new Map();
    map.getOrInsertComputed(key, () => "found");
    expect(map.get(key)).toBe("found");
  });

  test("lazy computation avoids expensive operations", () => {
    const map = new Map([["cached", 42]]);
    let computeCount = 0;
    const compute = (key) => {
      computeCount = computeCount + 1;
      return key.length;
    };
    map.getOrInsertComputed("cached", compute);
    map.getOrInsertComputed("cached", compute);
    map.getOrInsertComputed("new", compute);
    expect(computeCount).toBe(1);
  });

  test("handles grouping pattern with computed", () => {
    const map = new Map();
    const data = [
      ["a", 1],
      ["b", 2],
      ["a", 3],
    ];
    data.forEach(([key, val]) => {
      map.getOrInsertComputed(key, () => []).push(val);
    });
    expect(map.get("a").length).toBe(2);
    expect(map.get("b").length).toBe(1);
  });
});
