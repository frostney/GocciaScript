/*---
description: WeakMap integrates with Goccia.gc weak tracing
features: [WeakMap, Goccia, Symbol]
---*/

const hasGoccia = typeof Goccia !== "undefined";

describe.runIf(hasGoccia)("WeakMap GC behavior", () => {
  test("keeps value reachable while key is strongly reachable", () => {
    const key = {};
    const map = new WeakMap();
    (() => {
      map.set(key, { marker: 123 });
    })();
    Goccia.gc();
    expect(map.get(key).marker).toBe(123);
  });

  test("keeps live keyed values after GC", () => {
    const liveKey = {};
    const map = new WeakMap();
    map.set(liveKey, { marker: "live" });
    (() => {
      Array.from({ length: 200 }, (_, i) => i).forEach((i) => {
        map.set({ key: i }, { value: i });
      });
    })();
    Goccia.gc();
    expect(map.get(liveKey).marker).toBe("live");
  });

  test("constructor roots current set method across iterable user code", () => {
    const original = WeakMap.prototype.set;
    const key = {};
    let calls = 0;

    Object.defineProperty(WeakMap.prototype, "set", {
      configurable: true,
      get() {
        return {
          set(k, v) {
            calls = calls + 1;
            return original.call(this, k, v);
          },
        }.set;
      },
    });

    try {
      const iterable = {
        [Symbol.iterator]() {
          Goccia.gc();
          return [[key, "value"]][Symbol.iterator]();
        },
      };
      const map = new WeakMap(iterable);
      expect(calls).toBe(1);
      expect(map.get(key)).toBe("value");
    } finally {
      Object.defineProperty(WeakMap.prototype, "set", {
        value: original,
        writable: true,
        configurable: true,
      });
    }
  });

  test("constructor roots iterable while reading current set method", () => {
    const original = WeakMap.prototype.set;
    const key = {};

    Object.defineProperty(WeakMap.prototype, "set", {
      configurable: true,
      get() {
        Goccia.gc();
        return original;
      },
    });

    try {
      const map = new WeakMap((() => [[key, "value"]])());
      expect(map.get(key)).toBe("value");
    } finally {
      Object.defineProperty(WeakMap.prototype, "set", {
        value: original,
        writable: true,
        configurable: true,
      });
    }
  });
});
