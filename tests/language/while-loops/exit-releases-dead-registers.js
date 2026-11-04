/*---
description: >
  A while loop that has finished clears the registers its last iteration used
  above the live ones, so a collection after the program drops a container the
  loop filled reclaims it (ES2026 §14.7.3 the while statement).
features: [while-loops, Goccia.gc]
---*/

const ITEM_SIZE = 4 * 1024 * 1024;
const ITEMS = 10;

describe("while loop that has finished", () => {
  test("releases what it filled", () => {
    const run = () => {
      Goccia.gc();
      const baseline = Goccia.gc.bytesAllocated;
      let held = [];
      let count = 0;
      while (count < ITEMS) {
        const buffer = new ArrayBuffer(ITEM_SIZE);
        held.push(buffer);
        count++;
      }
      held = null;
      Goccia.gc();
      return [count, Goccia.gc.bytesAllocated - baseline];
    };
    const [count, grown] = run();
    expect(count).toBe(ITEMS);
    expect(grown).toBeLessThan(ITEM_SIZE * ITEMS / 2);
  });
});
