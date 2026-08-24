// Async-context propagation through node:async_hooks.
//
// Scope is deliberately the propagation core — the part bun, Node and
// GocciaScript agree on. Everything about `disable()` and the constructor
// options (`defaultValue`, `name`), plus `emitDestroy()`'s return value, is
// covered by tests/built-ins/AsyncHooks instead: bun 1.3.14 diverges from Node
// on all of them — it still models `disable()` as an instance-wide flag, so a
// continuation captured before the disable loses its store — and gating on bun
// there would report bun's gaps as GocciaScript divergences. See the
// CLASSIFICATION entry in scripts/test-cli-differential.ts.

import { AsyncLocalStorage, AsyncResource } from "node:async_hooks";

describe("async context propagation", () => {
  test("the store survives an await", async () => {
    const als = new AsyncLocalStorage();
    await als.run("ctx-1", async () => {
      expect(als.getStore()).toBe("ctx-1");
      await Promise.resolve();
      expect(als.getStore()).toBe("ctx-1");
    });
    expect(als.getStore()).toBe(undefined);
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
          observed.push(tag === als.getStore());
        }
      });
    await Promise.all([chain(1), chain(2), chain(3)]);
    expect(observed).toEqual([true, true, true, true, true, true, true, true, true]);
  });

  test("nested runs restore the enclosing store", () => {
    const als = new AsyncLocalStorage();
    const seen = [];
    als.run("outer", () => {
      als.run("inner", () => seen.push(als.getStore()));
      seen.push(als.getStore());
    });
    expect(seen).toEqual(["inner", "outer"]);
  });

  test("three instances stay independent across awaits", async () => {
    const first = new AsyncLocalStorage();
    const second = new AsyncLocalStorage();
    const third = new AsyncLocalStorage();
    const seen = [];
    await first.run("X", async () => {
      await second.run("Y", async () => {
        await third.run("Z", async () => {
          await Promise.resolve();
          seen.push([first.getStore(), second.getStore(), third.getStore()]);
        });
        seen.push([first.getStore(), second.getStore(), third.getStore()]);
      });
    });
    seen.push([first.getStore(), second.getStore(), third.getStore()]);
    expect(seen).toEqual([
      ["X", "Y", "Z"],
      ["X", "Y", undefined],
      [undefined, undefined, undefined],
    ]);
  });

  test("exit clears the store for its callback only", () => {
    const als = new AsyncLocalStorage();
    const seen = [];
    als.run("with-store", () => {
      als.exit(() => seen.push(als.getStore()));
      seen.push(als.getStore());
    });
    expect(seen).toEqual([undefined, "with-store"]);
  });

  test("enterWith binds for the rest of the execution, including after an await", async () => {
    const als = new AsyncLocalStorage();
    const seen = [];
    await als.run("start", async () => {
      await Promise.resolve();
      als.enterWith("entered");
      seen.push(als.getStore());
      await Promise.resolve();
      seen.push(als.getStore());
    });
    seen.push(als.getStore());
    expect(seen).toEqual(["entered", "entered", undefined]);
  });

  test("a then registered inside run sees the store when settled outside", async () => {
    const als = new AsyncLocalStorage();
    let settle;
    const pending = new Promise((resolve) => {
      settle = resolve;
    });
    const chained = als.run("registered", () =>
      pending.then(() => als.getStore()));
    settle(1);
    expect(await chained).toBe("registered");
    expect(als.getStore()).toBe(undefined);
  });

  test("catch and finally continuations carry the store", async () => {
    const als = new AsyncLocalStorage();
    const seen = [];
    await als.run("handled", () =>
      Promise.reject(new Error("rejected"))
        .catch(() => seen.push(als.getStore()))
        .finally(() => seen.push(als.getStore())));
    expect(seen).toEqual(["handled", "handled"]);
  });

  test("a foreign continuation does not leak into the frame that drained it", async () => {
    const als = new AsyncLocalStorage();
    const seen = [];
    let settle;
    const pending = new Promise((resolve) => {
      settle = resolve;
    });
    als.run("foreign", () => {
      pending.then(() => seen.push(als.getStore()));
    });

    await als.run("own", async () => {
      settle(1);
      await pending;
      seen.push(als.getStore());
    });
    seen.push(als.getStore());
    expect(seen).toEqual(["foreign", "own", undefined]);
  });

  test("a callback that throws after an await still restores the store", async () => {
    const als = new AsyncLocalStorage();
    const seen = [];
    let message = null;
    try {
      await als.run("rejecting", async () => {
        await Promise.resolve();
        seen.push(als.getStore());
        throw new Error("boom");
      });
    } catch (error) {
      message = error.message;
    }
    seen.push(message);
    seen.push(als.getStore());
    expect(seen).toEqual(["rejecting", "boom", undefined]);
  });

  test("a rejection awaited inside the callback keeps the store", async () => {
    const als = new AsyncLocalStorage();
    const seen = [];
    await als.run("kept", async () => {
      try {
        await Promise.reject(new Error("inner"));
      } catch {
        seen.push("caught");
      }
      seen.push(als.getStore());
    });
    seen.push(als.getStore());
    expect(seen).toEqual(["caught", "kept", undefined]);
  });

  test("a queued microtask sees the store of the scope that queued it", async () => {
    const als = new AsyncLocalStorage();
    let seen = "unset";
    als.run("queued", () => {
      queueMicrotask(() => {
        seen = als.getStore();
      });
    });
    await Promise.resolve();
    expect(seen).toBe("queued");
  });

  test("run returns the callback result and forwards extra arguments", () => {
    const als = new AsyncLocalStorage();
    expect(als.run("ctx", () => 42)).toBe(42);
    expect(als.run("ctx", (first, second) => [first, second, als.getStore()], 1, 2))
      .toEqual([1, 2, "ctx"]);
  });

  test("AsyncResource replays the context it captured", async () => {
    const als = new AsyncLocalStorage();
    let resource;
    let bound;
    let staticBound;
    als.run("captured", () => {
      resource = new AsyncResource("probe");
      bound = resource.bind(() => als.getStore());
      staticBound = AsyncResource.bind(() => als.getStore());
    });
    expect(resource.runInAsyncScope(() => als.getStore())).toBe("captured");
    expect(bound()).toBe("captured");
    expect(staticBound()).toBe("captured");
    expect(await resource.runInAsyncScope(async () => {
      await Promise.resolve();
      return als.getStore();
    })).toBe("captured");
    expect(als.getStore()).toBe(undefined);
  });

  test("AsyncLocalStorage.bind and .snapshot pin to the calling context", () => {
    const als = new AsyncLocalStorage();
    let bound;
    let snapshot;
    als.run("pinned", () => {
      bound = AsyncLocalStorage.bind(() => als.getStore());
      snapshot = AsyncLocalStorage.snapshot();
    });
    expect(bound()).toBe("pinned");
    expect(snapshot(() => als.getStore())).toBe("pinned");
  });

  test("a static named bind does not shadow Function.prototype.bind", () => {
    const read = ((value) => ["target", value]).bind(null, 1);
    expect(read()).toEqual(["target", 1]);
  });

  // Receiver forwarding for an undefined thisArg is NOT asserted here: bun
  // 1.3.14 substitutes undefined where Node forwards the call-site receiver
  // (fixed in bun 1.4.0), so gating on it makes the verdict depend on the
  // oracle's version. Covered against Node in tests/built-ins/AsyncHooks.

  test("bind rejects a non-callable at bind time", () => {
    expect(() => AsyncResource.bind(42)).toThrow(TypeError);
    expect(() => new AsyncResource("probe").bind(42)).toThrow(TypeError);
    expect(() => AsyncLocalStorage.bind(42)).toThrow(TypeError);
  });

  // disable() semantics are NOT asserted here at all: bun 1.3.14 models
  // disable as an instance-wide flag where Node edits the current context
  // frame (bun 1.4.0 matches Node), so any disable-shaped assertion gates on
  // the oracle's version. Covered against Node in tests/built-ins/AsyncHooks.

  test("an async generator body observes the resuming call's store", async () => {
    const als = new AsyncLocalStorage();
    const seen = [];
    const source = {
      async *values() {
        seen.push(als.getStore());
        yield 1;
        seen.push(als.getStore());
        await Promise.resolve();
        seen.push(als.getStore());
        yield 2;
      },
    };

    await als.run("GEN", async () => {
      for await (const value of source.values()) {
        seen.push(als.getStore() + ":" + value);
      }
    });

    const iterator = source.values();
    await iterator.next();
    await als.run("RESUMER", async () => {
      await iterator.next();
    });

    expect(seen).toEqual([
      "GEN", "GEN:1", "GEN", "GEN", "GEN:2",
      undefined, "RESUMER", "RESUMER",
    ]);
  });
});
