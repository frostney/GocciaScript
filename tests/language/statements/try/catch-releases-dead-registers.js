/*---
description: >
  A catch clause starts from an exception that unwound the try block, so what
  the block's last statements left in the registers above the live ones, such
  as the receiver of a method call, is dead (ES2026 §14.15.3 try statement).
  The clause clears them: a collection after the program drops its last
  reference to a container the block filled reclaims it, also in a frame that
  stays alive across the collection, and every binding live across the
  statement keeps its value.
features: [try-catch, async-await, Goccia.gc]
---*/

const ITEM_SIZE = 4 * 1024 * 1024;
const ITEMS = 10;
const FILLED = ITEM_SIZE * ITEMS;

const collect = () => {
  Goccia.gc();
  return Goccia.gc.bytesAllocated;
};

describe("catch clause after a try block that filled a container", () => {
  test("releases it when the exception comes from an awaited rejection", async () => {
    let calls = 0;
    const alloc = async () => {
      calls++;
      if (calls > ITEMS) {
        throw new Error("refused");
      }
      return new ArrayBuffer(ITEM_SIZE);
    };
    const run = async () => {
      const baseline = collect();
      let held = [];
      let count = 0;
      try {
        for (const _ of Array.from({ length: ITEMS * 2 })) {
          const buffer = await alloc();
          held.push(buffer);
          count++;
        }
      } catch (e) {
        expect(e.message).toBe("refused");
      }
      held = null;
      Goccia.gc();
      return [count, Goccia.gc.bytesAllocated - baseline];
    };
    const [count, grown] = await run();
    expect(count).toBe(ITEMS);
    expect(grown).toBeLessThan(FILLED / 2);
  });

  test("releases it when the exception is thrown by a statement in the block", () => {
    const run = () => {
      const baseline = collect();
      let held = [];
      let count = 0;
      try {
        for (const _ of Array.from({ length: ITEMS * 2 })) {
          const buffer = new ArrayBuffer(ITEM_SIZE);
          held.push(buffer);
          count++;
          if (count === ITEMS) throw 1;
        }
      } catch (e) {
        expect(e).toBe(1);
      }
      held = null;
      Goccia.gc();
      return Goccia.gc.bytesAllocated - baseline;
    };
    expect(run()).toBeLessThan(FILLED / 2);
  });
});

describe("catch clause keeps the bindings that are live across the statement", () => {
  test("a binding declared before the try, an outer temporary and a captured local", () => {
    const run = () => {
      const kept = { value: 1 };
      let total = 0;
      const read = () => total;
      const results = [kept, [total]].map((item) => {
        try {
          total += 1;
          const inner = [item, total];
          inner.push(total * 2);
          throw inner;
        } catch (thrown) {
          return thrown.length + read() + kept.value;
        }
      });
      return results;
    };
    expect(run()).toEqual([3 + 1 + 1, 3 + 2 + 1]);
  });

  test("an async function's bindings survive a catch after an awaited rejection", async () => {
    const fail = async () => {
      throw new Error("no");
    };
    const run = async () => {
      const before = [1, 2, 3];
      let after = 0;
      try {
        const seen = before.map((n) => n * 2);
        await fail();
        after = seen.length;
      } catch (error) {
        after = before.length + error.message.length;
      }
      return [before.length, after];
    };
    expect(await run()).toEqual([3, 5]);
  });

  test("a try with a finally block still runs its finally and rethrows", () => {
    const order = [];
    const run = () => {
      try {
        try {
          const items = [1, 2, 3];
          items.push(4);
          throw items.length;
        } catch (thrown) {
          order.push(`catch ${thrown}`);
          throw thrown + 1;
        } finally {
          order.push("finally");
        }
      } catch (rethrown) {
        order.push(`outer ${rethrown}`);
      }
    };
    run();
    expect(order).toEqual(["catch 4", "finally", "outer 5"]);
  });
});

// A script's top level is a frame that stays alive for the whole run, so the
// same registers matter there: this runs at the top level of a script.
const topLevelBaseline = collect();
let topLevelHeld;
let topLevelCaught;
topLevelHeld = [];
try {
  for (const _ of Array.from({ length: ITEMS * 2 })) {
    topLevelHeld.push(new ArrayBuffer(ITEM_SIZE));
    if (topLevelHeld.length === ITEMS) {
      throw topLevelHeld.length;
    }
  }
} catch (thrown) {
  topLevelCaught = thrown;
}
topLevelHeld = null;
const topLevelGrown = collect() - topLevelBaseline;

describe("catch clause at a script's top level", () => {
  test("releases a container the try block filled", () => {
    expect(topLevelCaught).toBe(ITEMS);
    expect(topLevelGrown).toBeLessThan(FILLED / 2);
  });
});
