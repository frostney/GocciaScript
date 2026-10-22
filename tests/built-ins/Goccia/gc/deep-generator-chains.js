/*---
description: Goccia.gc() marks suspended generators that each hold the one before of any depth without exhausting the native stack, and keeps every link
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

describe("Goccia.gc() over suspended generators that each hold the one before", () => {
  test("suspended generators that each hold the one before", () => {
    const make = (prev) => ({
      *run() {
        const held = prev;
        yield 0;
        return held;
      },
    }).run();
    let head = null;
    for (const _ of range(depth(100_000))) {
      const next = make(head);
      next.next();
      head = next;
    }
    Goccia.gc();
    let finished = 0;
    let cur = head;
    for (const _ of range(depth(100_000))) {
      const step = cur.next();
      if (step.done) finished++;
      cur = step.value;
    }
    expect(finished).toBe(depth(100_000));
    expect(cur).toBe(null);
  });
});
