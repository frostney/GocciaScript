/*---
description: Goccia.gc() marks nested objects and arrays of any depth without exhausting the native stack, and keeps every link
features: [Goccia.gc]
---*/

// The collector marks from an explicit mark stack rather than one native
// frame per object (ADR 0147), so a collection taken while a deep chain is
// alive neither overflows the native stack nor frees any link of it. Each
// chain is collected while only its far end is held, then walked to check
// every link survived. The depths are ones the recursive marker this replaced
// crashed at in a production build.
const range = (n) => Array.from({ length: n });

describe("Goccia.gc() over nested objects and arrays", () => {
  test("a chain of nested objects", () => {
    let head = { end: true };
    for (const _ of range(100_000)) head = { child: head };
    Goccia.gc();
    let links = 0;
    let cur = head;
    for (const _ of range(100_000)) {
      cur = cur.child;
      links++;
    }
    expect(links).toBe(100_000);
    expect(cur.end).toBe(true);
  });

  test("a chain of nested arrays", () => {
    let head = ["end"];
    for (const _ of range(300_000)) head = [head];
    Goccia.gc();
    let cur = head;
    for (const _ of range(300_000)) cur = cur[0];
    expect(cur).toEqual(["end"]);
  });
});
