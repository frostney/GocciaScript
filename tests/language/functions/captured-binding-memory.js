/*---
description: >
  The storage of a binding a closure captures is managed memory: a collection
  frees it once no closure can reach it, and the closures, generators and
  async functions that still hold it keep reading and writing it across
  collections.
features: [arrow-functions, closures, generators, async-functions, Goccia.gc]
---*/

const hasGoccia = typeof Goccia !== "undefined";

const COUNT = 2000;
const indices = [...Array(COUNT).keys()];

describe.runIf(hasGoccia)("captured binding memory", () => {
  test("is freed once no closure can reach it", () => {
    const f = (a) => (() => a + 1)();
    const run = () => indices.reduce((sum, i) => sum + f(i), 0);
    run();
    Goccia.gc();
    const before = Goccia.gc.bytesAllocated;
    for (const _ of [1, 2, 3, 4, 5]) {
      expect(run()).toBe((COUNT * (COUNT + 1)) / 2);
    }
    Goccia.gc();
    // Each run opens and drops COUNT cells; none of them may stay behind.
    expect(Goccia.gc.bytesAllocated - before).toBeLessThan(COUNT * 8);
  });
});

describe("a captured binding across collections", () => {
  const collect = () => {
    if (hasGoccia) {
      Goccia.gc();
    }
  };

  test("stays shared by every closure that captured it after its frame returned", () => {
    const makeCounter = () => {
      let count = 0;
      return {
        increment: () => {
          count = count + 1;
          return count;
        },
        read: () => count,
      };
    };
    const counter = makeCounter();
    collect();
    counter.increment();
    collect();
    counter.increment();
    collect();
    expect(counter.read()).toBe(2);
  });

  test("survives when only a nested closure still reaches it", () => {
    const outer = (value) => {
      const middle = () => () => value * 2;
      return middle();
    };
    const inner = outer(21);
    collect();
    expect(inner()).toBe(42);
  });

  test("survives in a suspended generator", () => {
    const source = {
      *make(start) {
        let current = start;
        const bump = () => {
          current = current + 1;
        };
        for (const _ of [1, 2, 3]) {
          yield current;
          bump();
        }
      },
    };
    const gen = source.make(10);
    expect(gen.next().value).toBe(10);
    collect();
    expect(gen.next().value).toBe(11);
    collect();
    expect(gen.next().value).toBe(12);
  });

  test("survives across an await", async () => {
    let total = 0;
    const add = async (amount) => {
      const before = () => total;
      await Promise.resolve();
      collect();
      total = before() + amount;
      return total;
    };
    await add(1);
    await add(2);
    expect(total).toBe(3);
  });

  test("survives when a closure is stored only on an object", () => {
    const holder = {};
    const install = (secret) => {
      holder.reveal = () => secret;
    };
    install("kept");
    collect();
    expect(holder.reveal()).toBe("kept");
  });
});
