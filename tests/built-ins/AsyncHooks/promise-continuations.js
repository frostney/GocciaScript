/*---
description: async context travels with promise reaction continuations, not with the code that settles the promise
features: [AsyncLocalStorage]
---*/

import { AsyncLocalStorage } from "node:async_hooks";

describe("AsyncLocalStorage promise continuations", () => {
  test("a then registered inside run sees the store when settled outside", async () => {
    const als = new AsyncLocalStorage();
    let settle;
    const pending = new Promise((resolve) => {
      settle = resolve;
    });
    const chained = als.run("registered", () =>
      pending.then(() => als.getStore()).then((value) => {
        expect(value).toBe("registered");
        return als.getStore();
      }));
    settle(1);
    expect(await chained).toBe("registered");
    expect(als.getStore()).toBeUndefined();
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

  test("a continuation does not leak its store into the frame that drained it", async () => {
    const als = new AsyncLocalStorage();
    let settle;
    const pending = new Promise((resolve) => {
      settle = resolve;
    });
    als.run("foreign", () => {
      pending.then(() => {
        expect(als.getStore()).toBe("foreign");
      });
    });

    await als.run("own", async () => {
      settle(1);
      await pending;
      expect(als.getStore()).toBe("own");
    });
    expect(als.getStore()).toBeUndefined();
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
});
