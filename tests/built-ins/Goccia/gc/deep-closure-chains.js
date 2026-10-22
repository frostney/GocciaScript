/*---
description: Goccia.gc() marks closures over the scopes of the closures before them of any depth without exhausting the native stack, and keeps every link
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

describe("Goccia.gc() over closures over the scopes of the closures before them", () => {
  test("closures over the scopes of the closures before them", () => {
    let f = () => "end";
    for (const _ of range(depth(300_000))) {
      const prev = f;
      f = () => prev;
    }
    Goccia.gc();
    let cur = f;
    for (const _ of range(depth(300_000))) cur = cur();
    expect(cur()).toBe("end");
  });
});
