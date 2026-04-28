/*---
description: WeakMap integrates with Goccia.gc weak tracing
features: [WeakMap, Goccia]
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

  test("sweeps unreachable entries without dropping live keyed values", () => {
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
    expect(map.has({})).toBe(false);
  });
});
