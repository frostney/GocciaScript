/*---
description: >
  A catch clause inside a generator clears the registers the try block used
  above the live ones (ES2026 §14.15.3 try statement), so a container the
  block filled is reclaimed once the program drops it, while the generator
  keeps every binding that is live across the statement.
features: [generators, try-catch, Goccia.gc]
---*/

const ITEM_SIZE = 4 * 1024 * 1024;
const ITEMS = 10;

const collect = () => {
  Goccia.gc();
  return Goccia.gc.bytesAllocated;
};

const source = {
  *run() {
    const baseline = collect();
    const kept = ["live", "across"];
    let held = [];
    let count = 0;
    try {
      for (const _ of Array.from({ length: ITEMS * 2 })) {
        const buffer = yield count;
        held.push(buffer);
        count++;
      }
    } catch (e) {
      count = -count;
    }
    held = null;
    Goccia.gc();
    yield [count, Goccia.gc.bytesAllocated - baseline];
    yield kept.join(" ");
  },
};

describe("catch clause in a generator", () => {
  test("releases a container the try block filled and keeps live bindings", () => {
    const generator = source.run();
    generator.next();
    for (const _ of Array.from({ length: ITEMS })) {
      generator.next(new ArrayBuffer(ITEM_SIZE));
    }
    const [count, grown] = generator.throw(new Error("stop")).value;
    expect(count).toBe(-ITEMS);
    expect(grown).toBeLessThan(ITEM_SIZE * ITEMS / 2);
    expect(generator.next().value).toBe("live across");
  });
});
