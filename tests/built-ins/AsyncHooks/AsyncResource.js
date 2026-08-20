/*---
description: AsyncResource captures the async context at construction and replays it on demand
features: [AsyncResource, AsyncLocalStorage]
---*/

import { AsyncLocalStorage, AsyncResource } from "node:async_hooks";

const captureIn = (als, store) => {
  let captured;
  als.run(store, () => {
    captured = new AsyncResource("probe");
  });
  return captured;
};

describe("AsyncResource", () => {
  test("runInAsyncScope replays the captured context", () => {
    const als = new AsyncLocalStorage();
    const resource = captureIn(als, "captured");
    expect(resource.runInAsyncScope(() => als.getStore())).toBe("captured");
    expect(als.getStore()).toBeUndefined();
  });

  test("runInAsyncScope forwards a receiver and extra arguments", () => {
    const resource = new AsyncResource("probe");
    const receiver = { tag: "receiver" };
    const observed = resource.runInAsyncScope(
      ({ tag }, first) => [tag, first],
      undefined,
      receiver,
      1,
    );
    expect(observed).toEqual(["receiver", 1]);
  });

  test("the captured context survives an await inside the scope", async () => {
    const als = new AsyncLocalStorage();
    const resource = captureIn(als, "captured");
    const observed = await resource.runInAsyncScope(async () => {
      await Promise.resolve();
      return als.getStore();
    });
    expect(observed).toBe("captured");
  });

  test("bind returns a function pinned to the captured context", () => {
    const als = new AsyncLocalStorage();
    const resource = captureIn(als, "captured");
    const bound = resource.bind(() => als.getStore());
    expect(bound()).toBe("captured");
  });

  test("the static bind pins to the context of the bind call", () => {
    const als = new AsyncLocalStorage();
    let bound;
    als.run("static", () => {
      bound = AsyncResource.bind(() => als.getStore());
    });
    expect(bound()).toBe("static");
  });

  test("asyncId and triggerAsyncId are numbers and emitDestroy chains", () => {
    const resource = new AsyncResource("probe");
    expect(typeof resource.asyncId()).toBe("number");
    expect(typeof resource.triggerAsyncId()).toBe("number");
    expect(resource.emitDestroy()).toBe(resource);
  });

  test("distinct resources report distinct async ids", () => {
    const first = new AsyncResource("probe");
    const second = new AsyncResource("probe");
    expect(first.asyncId()).not.toBe(second.asyncId());
  });

  test("prototype methods reject a foreign receiver", () => {
    const foreign = { asyncId: AsyncResource.prototype.asyncId };
    expect(() => foreign.asyncId()).toThrow(TypeError);
  });
});
