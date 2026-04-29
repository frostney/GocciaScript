/*---
description: WeakMap.prototype.getOrInsert returns existing values or inserts defaults
features: [WeakMap.prototype.getOrInsert, Symbol]
---*/

const hasGoccia = typeof Goccia !== "undefined";

describe.runIf(hasGoccia)("WeakMap.prototype.getOrInsert", () => {
  test("returns existing value without overwriting", () => {
    const key = {};
    const map = new WeakMap([[key, "original"]]);
    expect(map.getOrInsert(key, "replacement")).toBe("original");
    expect(map.get(key)).toBe("original");
  });

  test("inserts and returns default value for absent key", () => {
    const key = {};
    const map = new WeakMap();
    const value = {};
    expect(map.getOrInsert(key, value)).toBe(value);
    expect(map.get(key)).toBe(value);
  });

  test("works with non-registered symbols", () => {
    const key = Symbol("local");
    const map = new WeakMap();
    expect(map.getOrInsert(key, 10)).toBe(10);
    expect(map.getOrInsert(key, 20)).toBe(10);
  });

  test("rejects primitive and registered symbol keys", () => {
    const map = new WeakMap();
    expect(() => map.getOrInsert(1, "bad")).toThrow(TypeError);
    expect(() => map.getOrInsert(null, "bad")).toThrow(TypeError);
    expect(() => map.getOrInsert(Symbol.for("registered"), "bad")).toThrow(TypeError);
  });

  test("throws TypeError when receiver is not a WeakMap", () => {
    const getOrInsert = WeakMap.prototype.getOrInsert;
    expect(() => getOrInsert.call({}, {}, 1)).toThrow(TypeError);
    expect(() => getOrInsert.call(WeakMap.prototype, {}, 1)).toThrow(TypeError);
  });
});
