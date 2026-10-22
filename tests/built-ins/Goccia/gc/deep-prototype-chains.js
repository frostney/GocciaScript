/*---
description: Goccia.gc() marks prototype and class chains of any depth without exhausting the native stack, and keeps every link
features: [Goccia.gc]
---*/

// The collector marks from an explicit mark stack rather than one native
// frame per object (ADR 0147), so a collection taken while a deep chain is
// alive neither overflows the native stack nor frees any link of it. Each
// chain is collected while only its far end is held, then walked to check
// every link survived. The depths are ones the recursive marker this replaced
// crashed at in a production build.
const range = (n) => Array.from({ length: n });
// A GC-stress build collects every 8 instructions, so building a chain there
// costs its length times the live heap. A stress run uses chains of 1,000
// links, deep enough to spill the mark stack; the full depths run in every
// other build.
const depth = (n) => (Goccia.build.gcStress ? 1_000 : n);

describe("Goccia.gc() over prototype and class chains", () => {
  test("a prototype chain", () => {
    const base = { base: true };
    let leaf = base;
    for (const _ of range(depth(100_000))) leaf = Object.create(leaf);
    Goccia.gc();
    let cur = leaf;
    for (const _ of range(depth(100_000))) cur = Object.getPrototypeOf(cur);
    expect(cur).toBe(base);
  });

  test("a class chain", () => {
    class Base {
      base = "base";
    }
    let Leaf = Base;
    for (const _ of range(depth(30000))) {
      const Parent = Leaf;
      Leaf = class extends Parent {};
    }
    Goccia.gc();
    const instance = new Leaf();
    expect(instance.base).toBe("base");
    expect(instance instanceof Base).toBe(true);
  });
});
