/*---
description: node:async_hooks exposes AsyncLocalStorage and AsyncResource
features: [AsyncLocalStorage, AsyncResource]
---*/

import asyncHooks, { AsyncLocalStorage, AsyncResource } from "node:async_hooks";
import * as namespace from "node:async_hooks";

describe("node:async_hooks module", () => {
  test("named exports are constructors", () => {
    expect(typeof AsyncLocalStorage).toBe("function");
    expect(typeof AsyncResource).toBe("function");
  });

  test("the namespace carries the two constructors and a default export", () => {
    expect(Object.keys(namespace).sort())
      .toEqual(["AsyncLocalStorage", "AsyncResource", "default"]);
    expect(namespace.default).toBe(asyncHooks);
  });

  test("default export carries the same constructors", () => {
    expect(asyncHooks.AsyncLocalStorage).toBe(AsyncLocalStorage);
    expect(asyncHooks.AsyncResource).toBe(AsyncResource);
  });

  test("instances are instanceof their constructor", () => {
    expect(new AsyncLocalStorage() instanceof AsyncLocalStorage).toBe(true);
    expect(new AsyncResource("probe") instanceof AsyncResource).toBe(true);
  });

  test("an options object is accepted and may be omitted", () => {
    expect(typeof new AsyncLocalStorage()).toBe("object");
    expect(typeof new AsyncLocalStorage({})).toBe("object");
  });

  test("prototype methods reject a foreign receiver", () => {
    const foreign = { getStore: AsyncLocalStorage.prototype.getStore };
    expect(() => foreign.getStore()).toThrow(TypeError);
  });
});
