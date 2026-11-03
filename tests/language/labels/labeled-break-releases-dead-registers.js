/*---
description: >
  A `break label` out of a labeled loop lands past the clear the loop makes
  for its own exit, so the labeled loop clears the registers its last
  iteration used above the live ones once more after its breaks: a collection
  after the program drops a container the loop filled reclaims it
  (ES2026 §14.13 labelled statements).
features: [labelled-statements, for-of, break, Goccia.gc]
---*/

const ITEM_SIZE = 4 * 1024 * 1024;
const ITEMS = 10;

describe("labeled loop left by break label", () => {
  test("releases what its last iteration filled", () => {
    const run = () => {
      Goccia.gc();
      const baseline = Goccia.gc.bytesAllocated;
      let held = [];
      outer: for (const _ of Array.from({ length: ITEMS * 2 })) {
        for (const __ of [1]) {
          const buffer = new ArrayBuffer(ITEM_SIZE);
          held.push(buffer);
          if (held.length === ITEMS) {
            break outer;
          }
        }
      }
      const filled = held.length;
      held = null;
      Goccia.gc();
      return [filled, Goccia.gc.bytesAllocated - baseline];
    };
    const [filled, grown] = run();
    expect(filled).toBe(ITEMS);
    // One buffer is the last iteration's `buffer` register: it must be gone too.
    expect(grown).toBeLessThan(ITEM_SIZE / 2);
  });

  test("keeps the bindings that are live after the labeled loop", () => {
    const run = () => {
      const kept = [1, 2, 3];
      let total = 0;
      outer: for (const a of kept) {
        for (const b of kept) {
          if (b === 3) {
            continue outer;
          }
          if (a === 3) {
            break outer;
          }
          total += a * b;
        }
      }
      return [kept.length, total];
    };
    expect(run()).toEqual([3, 1 + 2 + 2 + 4]);
  });
});
