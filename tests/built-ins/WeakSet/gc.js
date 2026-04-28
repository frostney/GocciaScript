/*---
description: WeakSet integrates with Goccia.gc weak sweeping
features: [WeakSet, Goccia]
---*/

const hasGoccia = typeof Goccia !== "undefined";

describe.runIf(hasGoccia)("WeakSet GC behavior", () => {
  test("keeps value present while it is strongly reachable", () => {
    const value = {};
    const set = new WeakSet([value]);
    Goccia.gc();
    expect(set.has(value)).toBe(true);
  });

  test("sweeps unreachable entries without dropping live values", () => {
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
    expect(set.has({})).toBe(false);
  });
});
