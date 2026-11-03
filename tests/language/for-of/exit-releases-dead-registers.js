/*---
description: >
  A loop that has finished leaves the registers its last iteration used above
  the live ones holding whatever that iteration left in them, such as the
  receiver of a method call. They are dead once control is past the loop
  (ES2026 §14.7 iteration statements), so the loop clears them: a collection
  after the program drops its last reference to a container the loop filled
  reclaims it, also in a frame that stays alive across the collection.
features: [for-of, async-await, break, Goccia.gc]
---*/

const ITEM_SIZE = 4 * 1024 * 1024;
const ITEMS = 10;
const FILLED = ITEM_SIZE * ITEMS;

const collect = () => {
  Goccia.gc();
  return Goccia.gc.bytesAllocated;
};

describe("for-of loop that has finished", () => {
  test("releases what it filled in a synchronous function", () => {
    const run = () => {
      const baseline = collect();
      let held = [];
      for (const _ of Array.from({ length: ITEMS })) {
        const buffer = new ArrayBuffer(ITEM_SIZE);
        held.push(buffer);
      }
      const filled = held.length;
      held = null;
      Goccia.gc();
      return [filled, Goccia.gc.bytesAllocated - baseline];
    };
    const [filled, grown] = run();
    expect(filled).toBe(ITEMS);
    expect(grown).toBeLessThan(FILLED / 2);
  });

  test("releases what it filled in an async function that awaits in the loop", async () => {
    const alloc = async () => new ArrayBuffer(ITEM_SIZE);
    const run = async () => {
      const baseline = collect();
      let held = [];
      for (const _ of Array.from({ length: ITEMS })) {
        const buffer = await alloc();
        held.push(buffer);
      }
      const filled = held.length;
      held = null;
      Goccia.gc();
      return [filled, Goccia.gc.bytesAllocated - baseline];
    };
    const [filled, grown] = await run();
    expect(filled).toBe(ITEMS);
    expect(grown).toBeLessThan(FILLED / 2);
  });

  test("releases what it filled when the loop is left by break", () => {
    const run = () => {
      const baseline = collect();
      let held = [];
      for (const _ of Array.from({ length: ITEMS * 2 })) {
        const buffer = new ArrayBuffer(ITEM_SIZE);
        held.push(buffer);
        if (held.length === ITEMS) {
          break;
        }
      }
      const filled = held.length;
      held = null;
      Goccia.gc();
      return [filled, Goccia.gc.bytesAllocated - baseline];
    };
    const [filled, grown] = run();
    expect(filled).toBe(ITEMS);
    expect(grown).toBeLessThan(FILLED / 2);
  });

  test("keeps the bindings that are live after the loop", () => {
    const run = () => {
      const kept = { value: 7 };
      const list = [];
      let sum = 0;
      for (const item of [1, 2, 3]) {
        list.push(item * 2);
        sum += item;
      }
      for (const item of list) {
        for (const inner of [item, item + 1]) {
          sum += inner;
        }
      }
      return [kept.value, list, sum];
    };
    expect(run()).toEqual([7, [2, 4, 6], 6 + 2 + 3 + 4 + 5 + 6 + 7]);
  });
});

// A script's top level is a frame that stays alive for the whole run, so the
// loop's registers matter there too. The declarations avoid a top-level
// initializer, whose register is a separate retention path.
const topLevelBaseline = collect();
let topLevelHeld;
topLevelHeld = [];
for (const _ of Array.from({ length: ITEMS })) {
  const buffer = new ArrayBuffer(ITEM_SIZE);
  topLevelHeld.push(buffer);
}
topLevelHeld = null;
Goccia.gc();
const topLevelGrown = Goccia.gc.bytesAllocated - topLevelBaseline;

describe("for-of loop that has finished at a script's top level", () => {
  test("releases what it filled", () => {
    expect(topLevelGrown).toBeLessThan(FILLED / 2);
  });
});
