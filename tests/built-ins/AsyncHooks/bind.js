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

  test("bind rejects a non-callable at bind time", () => {
    expect(() => AsyncLocalStorage.bind(42)).toThrow(TypeError);
  });

  test("a snapshot runner rejects a non-callable at call time", () => {
    // snapshot() has no callback to validate, so the check lands on its runner.
    const snapshot = AsyncLocalStorage.snapshot();
    expect(() => snapshot(42)).toThrow(TypeError);
  });

  test("bind and snapshot report Node's name and length", () => {
    const target = (first, second) => [first, second];
    expect(AsyncLocalStorage.bind(target).name).toBe("bound");
    expect(AsyncLocalStorage.bind(target).length).toBe(2);
    // The runner takes (callback, ...args), so its length is one regardless.
    expect(AsyncLocalStorage.snapshot().name).toBe("bound");
    expect(AsyncLocalStorage.snapshot().length).toBe(1);
  });

  test("bind forwards the call-site receiver", () => {
    const read = ({ read() { return this.tag; } }).read;
    expect(({ tag: "holder", method: AsyncLocalStorage.bind(read) }).method())
      .toBe("holder");
  });

  test("a snapshot runner does not forward its own receiver", () => {
    // Node implements snapshot() as AsyncResource.bind((cb, ...args) =>
    // cb(...args)), and that plain call passes no receiver, so a runner
    // installed as an object method must not hand its holder to the callback.
    // Probed against Node v24.0.1.
    const read = ({
      read() {
        return this === undefined ? "undefined" : "leaked";
      },
    }).read;
    const runner = AsyncLocalStorage.snapshot();
    expect(({ tag: "holder", run: runner }).run(read)).toBe("undefined");
    expect(runner(read)).toBe("undefined");
  });

  test("does not shadow Function.prototype.bind for ordinary functions", () => {
    const target = { tag: "target" };
    const read = ((value) => [target.tag, value]).bind(null, 1);
    expect(read()).toEqual(["target", 1]);
  });
});
