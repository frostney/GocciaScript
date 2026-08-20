/*---
description: AsyncLocalStorage.prototype.run binds a store for its callback and every continuation created inside it
features: [AsyncLocalStorage]
---*/

import { AsyncLocalStorage } from "node:async_hooks";

describe("AsyncLocalStorage.prototype.run", () => {
  test("binds the store for the synchronous callback", () => {
    const als = new AsyncLocalStorage();
    als.run("ctx", () => {
      expect(als.getStore()).toBe("ctx");
    });
    expect(als.getStore()).toBeUndefined();
  });

  test("returns the callback result and forwards extra arguments", () => {
    const als = new AsyncLocalStorage();
    expect(als.run("ctx", () => 42)).toBe(42);
    expect(als.run("ctx", (first, second) => [first, second, als.getStore()], 1, 2))
      .toEqual([1, 2, "ctx"]);
  });

  test("the store survives an await", async () => {
    const als = new AsyncLocalStorage();
    await als.run("ctx-1", async () => {
      expect(als.getStore()).toBe("ctx-1");
      await Promise.resolve();
      expect(als.getStore()).toBe("ctx-1");
    });
    expect(als.getStore()).toBeUndefined();
  });

  test("interleaved chains keep separate stores", async () => {
    const als = new AsyncLocalStorage();
    const seen = [];
    await Promise.all([
      als.run("a", async () => {
        await Promise.resolve();
        await Promise.resolve();
        seen.push(als.getStore());
      }),
      als.run("b", async () => {
        await Promise.resolve();
        seen.push(als.getStore());
      }),
    ]);
    expect(seen.sort()).toEqual(["a", "b"]);
  });

  test("many concurrent chains each keep their own store", async () => {
    const als = new AsyncLocalStorage();
    const observed = [];
    const chain = (tag) =>
      als.run(tag, async () => {
        for (const step of [0, 1, 2]) {
          await Promise.resolve();
          observed.push([tag, als.getStore()]);
        }
      });
    await Promise.all([chain(1), chain(2), chain(3)]);
    expect(observed.every(([tag, store]) => tag === store)).toBe(true);
  });

  test("nested runs restore the enclosing store", () => {
    const als = new AsyncLocalStorage();
    als.run("outer", () => {
      als.run("inner", () => {
        expect(als.getStore()).toBe("inner");
      });
      expect(als.getStore()).toBe("outer");
    });
  });

  test("three instances stay independent", async () => {
    const first = new AsyncLocalStorage();
    const second = new AsyncLocalStorage();
    const third = new AsyncLocalStorage();
    await first.run("X", async () => {
      await second.run("Y", async () => {
        await third.run("Z", async () => {
          await Promise.resolve();
          expect([first.getStore(), second.getStore(), third.getStore()])
            .toEqual(["X", "Y", "Z"]);
        });
        expect(third.getStore()).toBeUndefined();
        expect([first.getStore(), second.getStore()]).toEqual(["X", "Y"]);
      });
    });
    expect([first.getStore(), second.getStore(), third.getStore()])
      .toEqual([undefined, undefined, undefined]);
  });

  test("a callback that throws after an await still restores the store", async () => {
    const als = new AsyncLocalStorage();
    let message = null;
    try {
      await als.run("rejecting", async () => {
        await Promise.resolve();
        expect(als.getStore()).toBe("rejecting");
        throw new Error("boom");
      });
    } catch (error) {
      message = error.message;
    }
    expect(message).toBe("boom");
    expect(als.getStore()).toBeUndefined();
  });

  test("a rejection awaited inside the callback keeps the store", async () => {
    const als = new AsyncLocalStorage();
    await als.run("kept", async () => {
      try {
        await Promise.reject(new Error("inner"));
      } catch {
        // The store must survive the rejection path, not only the happy one.
      }
      expect(als.getStore()).toBe("kept");
    });
    expect(als.getStore()).toBeUndefined();
  });
});
