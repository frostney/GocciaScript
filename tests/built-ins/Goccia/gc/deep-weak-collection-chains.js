/*---
description: Goccia.gc() marks chains reached through WeakMap entries of any depth without exhausting the native stack, and keeps every link
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

describe("Goccia.gc() over chains reached through WeakMap entries", () => {
  test("a deep chain reachable only through a live WeakMap key", () => {
    const key = {};
    const map = new WeakMap();
    let head = { end: true };
    for (const _ of range(depth(100_000))) head = { child: head };
    map.set(key, head);
    head = null;
    Goccia.gc();
    let cur = map.get(key);
    for (const _ of range(depth(100_000))) cur = cur.child;
    expect(cur.end).toBe(true);
  });

  test("a chain of WeakMap entries whose values hold the next key", () => {
    // A collection takes one whole-heap pass per link of this chain (#1860),
    // so a stress run, which collects every 8 instructions, uses 100 keys.
    const count = Goccia.build.gcStress ? 100 : 2000;
    const map = new WeakMap();
    const keys = range(count).map(() => ({}));
    keys.forEach((k, i) => {
      if (i + 1 < keys.length) map.set(k, { next: keys[i + 1] });
    });
    const first = keys[0];
    keys.length = 0;
    Goccia.gc();
    let links = 0;
    let cur = first;
    for (const _ of range(count - 1)) {
      cur = map.get(cur).next;
      links++;
    }
    expect(links).toBe(count - 1);
    expect(map.has(cur)).toBe(false);
  });
});
