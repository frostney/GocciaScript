/*---
description: WeakMap.prototype.getOrInsertComputed computes only missing values
features: [WeakMap.prototype.getOrInsertComputed]
---*/

const hasGoccia = typeof Goccia !== "undefined";

describe.runIf(hasGoccia)("WeakMap.prototype.getOrInsertComputed", () => {
  test("returns existing value without calling callback", () => {
    const key = {};
    const map = new WeakMap([[key, "original"]]);
    let calls = 0;
    const result = map.getOrInsertComputed(key, () => {
      calls = calls + 1;
      return "replacement";
    });
    expect(result).toBe("original");
    expect(calls).toBe(0);
  });

  test("computes, inserts, and returns missing value", () => {
    const key = {};
    const map = new WeakMap();
    let seen = undefined;
    const value = map.getOrInsertComputed(key, (k) => {
      seen = k;
      return { created: true };
    });
    expect(seen).toBe(key);
    expect(value.created).toBe(true);
    expect(map.get(key)).toBe(value);
  });

  test("requires a callable callback", () => {
    const map = new WeakMap();
    expect(() => map.getOrInsertComputed({}, 1)).toThrow(TypeError);
  });

  test("rejects primitive and registered symbol keys before callback", () => {
    const map = new WeakMap();
    let calls = 0;
    const callback = () => {
      calls = calls + 1;
      return "bad";
    };
    expect(() => map.getOrInsertComputed(1, callback)).toThrow(TypeError);
    expect(() => map.getOrInsertComputed(Symbol.for("registered"), callback)).toThrow(TypeError);
    expect(calls).toBe(0);
  });

  test("throws TypeError when receiver is not a WeakMap", () => {
    const getOrInsertComputed = WeakMap.prototype.getOrInsertComputed;
    expect(() => getOrInsertComputed.call({}, {}, () => 1)).toThrow(TypeError);
    expect(() => getOrInsertComputed.call(WeakMap.prototype, {}, () => 1)).toThrow(TypeError);
  });
});
