/*---
description: WeakSet integrates with Goccia.gc weak sweeping
features: [WeakSet, Goccia, Symbol]
---*/

const hasGoccia = typeof Goccia !== "undefined";

describe.runIf(hasGoccia)("WeakSet GC behavior", () => {
  test("keeps value present while it is strongly reachable", () => {
    const value = {};
    const set = new WeakSet([value]);
    Goccia.gc();
    expect(set.has(value)).toBe(true);
  });

  test("keeps live values after GC", () => {
    const liveValue = {};
    const set = new WeakSet();
    set.add(liveValue);
    (() => {
      Array.from({ length: 200 }, (_, i) => i).forEach((i) => {
        set.add({ value: i });
      });
    })();
    Goccia.gc();
    expect(set.has(liveValue)).toBe(true);
  });

  test("constructor roots current add method across iterable user code", () => {
    const original = WeakSet.prototype.add;
    const value = {};
    let calls = 0;

    Object.defineProperty(WeakSet.prototype, "add", {
      configurable: true,
      get() {
        return {
          add(v) {
            calls = calls + 1;
            return original.call(this, v);
          },
        }.add;
      },
    });

    try {
      const iterable = {
        [Symbol.iterator]() {
          Goccia.gc();
          return [value][Symbol.iterator]();
        },
      };
      const set = new WeakSet(iterable);
      expect(calls).toBe(1);
      expect(set.has(value)).toBe(true);
    } finally {
      Object.defineProperty(WeakSet.prototype, "add", {
        value: original,
        writable: true,
        configurable: true,
      });
    }
  });

  test("constructor roots iterable while reading current add method", () => {
    const original = WeakSet.prototype.add;
    const value = {};

    Object.defineProperty(WeakSet.prototype, "add", {
      configurable: true,
      get() {
        Goccia.gc();
        return original;
      },
    });

    try {
      const set = new WeakSet((() => [value])());
      expect(set.has(value)).toBe(true);
    } finally {
      Object.defineProperty(WeakSet.prototype, "add", {
        value: original,
        writable: true,
        configurable: true,
      });
    }
  });
});
