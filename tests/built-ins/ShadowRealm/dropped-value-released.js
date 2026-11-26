/*---
description: >
  Creating a ShadowRealm does not change which values a collection keeps: a
  value a called function dropped is reclaimable once the call returns, as it
  is in a program with one realm.
features: [ShadowRealm, Goccia.gc]
---*/

const SIZE = 32 * 1024 * 1024;

const collect = () => {
  Goccia.gc();
  return Goccia.gc.bytesAllocated;
};

describe("a value a call dropped, once a ShadowRealm exists", () => {
  test("is reclaimed by a collection in the caller", () => {
    new ShadowRealm();
    const drop = () => {
      let buffer = new ArrayBuffer(SIZE);
      buffer = null;
    };
    const run = () => {
      const baseline = collect();
      drop();
      Goccia.gc();
      return Goccia.gc.bytesAllocated - baseline;
    };
    expect(run()).toBeLessThan(SIZE / 2);
  });

  test("is reclaimed after a destructuring that took it", () => {
    new ShadowRealm();
    const run = () => {
      const baseline = collect();
      let [first] = [new ArrayBuffer(SIZE)];
      first = null;
      Goccia.gc();
      return Goccia.gc.bytesAllocated - baseline;
    };
    expect(run()).toBeLessThan(SIZE / 2);
  });

  test("still runs a function of another realm in that realm", () => {
    const realm = new ShadowRealm();
    const keys = realm.evaluate("(o) => Object.keys(globalThis).includes('marker')");
    globalThis.marker = 1;
    try {
      expect(keys()).toBe(false);
    } finally {
      delete globalThis.marker;
    }
  });
});
