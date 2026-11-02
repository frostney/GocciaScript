/*---
description: >
  A script's top-level var binding lives in the global object (ES2026
  §9.1.1.4 Global Environment Records): once the script assigns the binding
  again, the initializer is unreachable and a collection reclaims it, also when
  a closure compiled before the declaration names the binding.
features: [var, closures, Goccia.gc]
---*/

const SIZE = 32 * 1024 * 1024;

const measure = (drop) => {
  Goccia.gc();
  const before = Goccia.gc.bytesAllocated;
  drop();
  Goccia.gc();
  return before - Goccia.gc.bytesAllocated;
};

var plain = new ArrayBuffer(SIZE);
const plainFreed = measure(() => {
  plain = null;
});

const readEarly = () => early;
var early = new ArrayBuffer(SIZE);
const earlyClosureFreed = measure(() => {
  early = null;
});

describe("a reassigned top-level var binding", () => {
  test("releases a plain initializer", () => {
    expect(plainFreed).toBeGreaterThanOrEqual(SIZE);
    expect(plain).toBe(null);
  });

  test("releases the initializer a closure compiled before the declaration names", () => {
    expect(earlyClosureFreed).toBeGreaterThanOrEqual(SIZE);
    expect(readEarly()).toBe(null);
  });
});
