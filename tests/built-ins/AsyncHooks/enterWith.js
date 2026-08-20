/*---
description: AsyncLocalStorage.prototype.enterWith binds a store for the rest of the current execution
features: [AsyncLocalStorage]
---*/

import { AsyncLocalStorage } from "node:async_hooks";

describe("AsyncLocalStorage.prototype.enterWith", () => {
  test("binds the store immediately and across a later await", async () => {
    const als = new AsyncLocalStorage();
    await als.run("start", async () => {
      await Promise.resolve();
      als.enterWith("entered");
      expect(als.getStore()).toBe("entered");
      await Promise.resolve();
      expect(als.getStore()).toBe("entered");
    });
    expect(als.getStore()).toBeUndefined();
  });

  test("returns undefined", () => {
    const als = new AsyncLocalStorage();
    als.run("outer", () => {
      expect(als.enterWith("inner")).toBeUndefined();
      expect(als.getStore()).toBe("inner");
    });
  });

  test("re-enables a disabled instance", () => {
    const als = new AsyncLocalStorage({ defaultValue: "DEF" });
    als.disable();
    expect(als.getStore()).toBe("DEF");
    als.run("scope", () => {
      als.enterWith("entered");
      expect(als.getStore()).toBe("entered");
    });
  });
});
