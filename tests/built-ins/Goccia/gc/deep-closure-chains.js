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

describe("Goccia.gc() over closures over the scopes of the closures before them", () => {
  test("closures over the scopes of the closures before them", () => {
    let f = () => "end";
    for (const _ of range(300_000)) {
      const prev = f;
      f = () => prev;
    }
    Goccia.gc();
    let cur = f;
    for (const _ of range(300_000)) cur = cur();
    expect(cur()).toBe("end");
  });
});
