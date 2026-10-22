/*---
description: Goccia.gc() marks promise reaction chains of any depth without exhausting the native stack, and keeps every link
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
const depth = (n) => ((typeof Goccia !== "undefined" && Goccia.build.gcStress) ? 1_000 : n);

describe("Goccia.gc() over promise reaction chains", () => {
  test("a chain of promise reactions waiting on one pending promise", async () => {
    let resolve;
    const first = new Promise((r) => {
      resolve = r;
    });
    let last = first;
    for (const _ of range(depth(100_000))) last = last.then((v) => v + 1);
    Goccia.gc();
    resolve(0);
    expect(await last).toBe(depth(100_000));
  });
});
