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

  test("walks a deep key-as-value chain after GC without quadratic blowup", () => {
    const map = new WeakMap();
    const head = {};
    const length = 60000;
    let key = head;
    Array.from({ length }).forEach(() => {
      const next = {};
      map.set(key, next);
      key = next;
    });
    Goccia.gc();

    let walked = 0;
    let node = head;
    Array.from({ length: length + 1 }).forEach(() => {
      if (node !== undefined) {
        walked = walked + 1;
        node = map.get(node);
      }
    });
    expect(walked).toBe(length + 1);
    expect(node).toBe(undefined);
  });

  test("collects an unreachable deep chain so keys are not retained", () => {
    Goccia.gc();
    const baseline = Goccia.gc.bytesAllocated;

    (() => {
      const map = new WeakMap();
      let key = {};
      Array.from({ length: 60000 }).forEach(() => {
        const next = {};
        map.set(key, next);
        key = next;
      });
      expect(Goccia.gc.bytesAllocated).toBeGreaterThan(baseline);
    })();

    Goccia.gc();
    expect(Goccia.gc.bytesAllocated).toBeLessThan(baseline + 1024 * 1024);
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
