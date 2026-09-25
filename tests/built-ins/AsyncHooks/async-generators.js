/*---
description: async generator resumptions observe the same async context as Node
features: [AsyncLocalStorage, async-generators]
---*/

import { AsyncLocalStorage } from "node:async_hooks";

// Async generators drive their bodies through per-mode request queues that the
// async-context seams do not touch directly. Every expectation here was probed
// against Node v24.0.1 and holds in both executors: the body observes the
// context of whichever call resumed it, and the surrounding frames are
// unaffected.
const als = new AsyncLocalStorage();

describe("AsyncLocalStorage across async generators", () => {
  test("the store reaches the body and survives yields and awaits", async () => {
    const seen = [];
    const source = {
      async *values() {
        seen.push(als.getStore());
        yield 1;
        seen.push(als.getStore());
        await Promise.resolve();
        seen.push(als.getStore());
        yield 2;
        seen.push(als.getStore());
      },
    };

    await als.run("GEN", async () => {
      for await (const value of source.values()) {
        seen.push(als.getStore() + ":" + value);
      }
    });
    expect(seen).toEqual(["GEN", "GEN:1", "GEN", "GEN", "GEN:2", "GEN"]);
    expect(als.getStore()).toBeUndefined();
  });

  test("a generator resumed outside a run sees no store", async () => {
    const seen = [];
    const source = {
      async *values() {
        seen.push(als.getStore());
        yield 1;
        seen.push(als.getStore());
      },
    };

    let iterator;
    als.run("CREATED", () => {
      iterator = source.values();
    });
    await iterator.next();
    await iterator.next();
    expect(seen).toEqual([undefined, undefined]);
  });

  test("a generator resumed inside a run sees that run's store", async () => {
    const seen = [];
    const source = {
      async *values() {
        seen.push(als.getStore());
        yield 1;
        seen.push(als.getStore());
      },
    };

    const iterator = source.values();
    await iterator.next();
    await als.run("RESUMER", async () => {
      await iterator.next();
    });
    expect(seen).toEqual([undefined, "RESUMER"]);
  });

  test("a queued second request does not displace the running body's store", async () => {
    const seen = [];
    let release;
    const gate = new Promise((resolve) => {
      release = resolve;
    });
    const source = {
      async *values() {
        seen.push(als.getStore());
        await gate;
        seen.push(als.getStore());
        yield 1;
        seen.push(als.getStore());
        yield 2;
      },
    };

    const iterator = source.values();
    const first = als.run("FIRST", () => iterator.next());
    const second = als.run("SECOND", () => iterator.next());
    release(1);
    await first;
    await second;
    expect(seen).toEqual(["FIRST", "FIRST", "FIRST"]);
    expect(als.getStore()).toBeUndefined();
  });

  test("nested runs around a for-await keep their own stores", async () => {
    const seen = [];
    const source = {
      async *values() {
        seen.push(als.getStore());
        yield 1;
      },
    };

    await als.run("OUTER", async () => {
      await als.run("INNER", async () => {
        for await (const value of source.values()) {
          seen.push(als.getStore() + ":" + value);
        }
      });
      seen.push(als.getStore());
    });
    expect(seen).toEqual(["INNER", "INNER:1", "OUTER"]);
  });
});
