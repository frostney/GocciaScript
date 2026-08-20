/*---
description: async context survives a collection taken while a scoped context is installed
features: [AsyncLocalStorage, AsyncResource]
---*/

import { AsyncLocalStorage, AsyncResource } from "node:async_hooks";

// A snapshot displaced by run/exit/runInAsyncScope is reachable only from the
// engine's saved-context stack. If that stack were not a collection root, a
// collection taken from inside the callback would free the displaced snapshot
// and the restore would read freed memory — a crash, not a wrong value. Every
// test here forces two collections so a surviving snapshot has to be genuinely
// rooted rather than merely not-yet-swept.
const collect = () => {
  Goccia.gc();
  Goccia.gc();
};

describe("AsyncLocalStorage under garbage collection", () => {
  test("a nested run restores the outer store after a collection", () => {
    const als = new AsyncLocalStorage();
    als.run("OUTER", () => {
      als.run("INNER", () => {
        collect();
        expect(als.getStore()).toBe("INNER");
      });
      expect(als.getStore()).toBe("OUTER");
    });
    expect(als.getStore()).toBeUndefined();
  });

  test("exit restores the surrounding store after a collection", () => {
    const als = new AsyncLocalStorage();
    als.run("OUTER", () => {
      als.exit(() => {
        collect();
        expect(als.getStore()).toBeUndefined();
      });
      expect(als.getStore()).toBe("OUTER");
    });
  });

  test("runInAsyncScope restores the surrounding store after a collection", () => {
    const als = new AsyncLocalStorage();
    let resource;
    als.run("CAPTURED", () => {
      resource = new AsyncResource("probe");
    });
    als.run("OUTER", () => {
      resource.runInAsyncScope(() => {
        collect();
        expect(als.getStore()).toBe("CAPTURED");
      });
      expect(als.getStore()).toBe("OUTER");
    });
  });

  test("a bound function restores the surrounding store after a collection", () => {
    const als = new AsyncLocalStorage();
    let bound;
    als.run("BOUND", () => {
      bound = AsyncLocalStorage.bind(() => {
        collect();
        return als.getStore();
      });
    });
    als.run("OUTER", () => {
      expect(bound()).toBe("BOUND");
      expect(als.getStore()).toBe("OUTER");
    });
  });

  test("deeply nested runs each restore their own store after collections", () => {
    const als = new AsyncLocalStorage();
    const seen = [];
    const descend = (depth) => {
      if (depth === 0) {
        collect();
        return;
      }
      als.run(depth, () => {
        descend(depth - 1);
        seen.push(als.getStore());
      });
    };
    descend(6);
    expect(seen).toEqual([1, 2, 3, 4, 5, 6]);
  });

  test("a store survives collections taken across a microtask drain", async () => {
    const als = new AsyncLocalStorage();
    const seen = [];
    await als.run("DRAINED", () =>
      Promise.resolve()
        .then(() => {
          collect();
          seen.push(als.getStore());
        })
        .then(() => {
          collect();
          seen.push(als.getStore());
        }));
    expect(seen).toEqual(["DRAINED", "DRAINED"]);
    expect(als.getStore()).toBeUndefined();
  });

  test("interleaved chains survive collections between their resumptions", async () => {
    // The chains are `.then` chains rather than async callbacks on purpose.
    // Collecting from inside a bytecode async function that has already
    // resumed from a suspension faults the VM, independently of this module —
    // `(async () => { await Promise.resolve(); Goccia.gc(); })()` alone
    // reproduces it on 0.13.0. A `.then` handler runs as an ordinary microtask
    // job, which is the seam under test here anyway.
    const als = new AsyncLocalStorage();
    const observed = [];
    const chain = (tag) =>
      als.run(tag, () =>
        Promise.resolve()
          .then(() => {
            collect();
            observed.push(tag === als.getStore());
          })
          .then(() => {
            collect();
            observed.push(tag === als.getStore());
          }));
    await Promise.all([chain("a"), chain("b"), chain("c")]);
    expect(observed).toEqual([true, true, true, true, true, true]);
  });

  test("a continuation captured before a collection still carries its store", async () => {
    const als = new AsyncLocalStorage();
    let settle;
    const pending = new Promise((resolve) => {
      settle = resolve;
    });
    let seen = "unset";
    als.run("CAPTURED", () => {
      pending.then(() => {
        seen = als.getStore();
      });
    });
    collect();
    settle(1);
    await pending;
    await Promise.resolve();
    expect(seen).toBe("CAPTURED");
  });
});
