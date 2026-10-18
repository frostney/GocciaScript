/*---
description: Goccia.gc() marks Sets that each hold the next of any depth without exhausting the native stack, and keeps every link
features: [Goccia.gc]
---*/

// The collector marks from an explicit mark stack rather than one native
// frame per object (ADR 0147), so a collection taken while a deep chain is
// alive neither overflows the native stack nor frees any link of it. Each
// chain is collected while only its far end is held, then walked to check
// every link survived. The depths are ones the recursive marker this replaced
// crashed at in a production build.
const range = (n) => Array.from({ length: n });

describe("Goccia.gc() over Sets that each hold the next", () => {
  test("a chain of Sets", () => {
    let head = new Set(["end"]);
    for (const _ of range(300_000)) head = new Set([head]);
    Goccia.gc();
    let cur = head;
    for (const _ of range(300_000)) cur = cur.values().next().value;
    expect(cur.has("end")).toBe(true);
  });
});
