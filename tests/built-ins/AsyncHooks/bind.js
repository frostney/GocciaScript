/*---
description: AsyncLocalStorage.bind and AsyncLocalStorage.snapshot pin a callback to the context of the call
features: [AsyncLocalStorage]
---*/

import { AsyncLocalStorage } from "node:async_hooks";

describe("AsyncLocalStorage statics", () => {
  test("bind pins a callback to the context it was bound in", () => {
    const als = new AsyncLocalStorage();
    let bound;
    als.run("bound-in", () => {
      bound = AsyncLocalStorage.bind(() => als.getStore());
    });
    expect(bound()).toBe("bound-in");
    expect(als.getStore()).toBeUndefined();
  });

  test("bind forwards arguments", () => {
    const als = new AsyncLocalStorage();
    let bound;
    als.run("ctx", () => {
      bound = AsyncLocalStorage.bind((first, second) => [first, second, als.getStore()]);
    });
    expect(bound(1, 2)).toEqual([1, 2, "ctx"]);
  });

  test("snapshot returns a runner for the captured context", () => {
    const als = new AsyncLocalStorage();
    let snapshot;
    als.run("snapshotted", () => {
      snapshot = AsyncLocalStorage.snapshot();
    });
    expect(snapshot(() => als.getStore())).toBe("snapshotted");
    expect(snapshot((first) => [first, als.getStore()], 1)).toEqual([1, "snapshotted"]);
  });

  test("does not shadow Function.prototype.bind for ordinary functions", () => {
    const target = { tag: "target" };
    const read = ((value) => [target.tag, value]).bind(null, 1);
    expect(read()).toEqual(["target", 1]);
  });
});
