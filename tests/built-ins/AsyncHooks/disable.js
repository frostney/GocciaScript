/*---
description: AsyncLocalStorage.prototype.disable stops the instance reporting stores until it is used again
features: [AsyncLocalStorage]
---*/

import { AsyncLocalStorage } from "node:async_hooks";

describe("AsyncLocalStorage.prototype.disable", () => {
  test("getStore reports undefined after disable", () => {
    const als = new AsyncLocalStorage();
    als.disable();
    expect(als.getStore()).toBeUndefined();
  });

  test("getStore reports the default value after disable", () => {
    const als = new AsyncLocalStorage({ defaultValue: "DEF" });
    als.disable();
    expect(als.getStore()).toBe("DEF");
  });

  test("run re-enables the instance", () => {
    const als = new AsyncLocalStorage({ defaultValue: "DEF" });
    als.disable();
    als.run("again", () => {
      expect(als.getStore()).toBe("again");
    });
    expect(als.getStore()).toBe("DEF");
  });

  test("disabling inside a run drops the store for the rest of it", () => {
    const als = new AsyncLocalStorage();
    als.run("bound", () => {
      expect(als.getStore()).toBe("bound");
      als.disable();
      expect(als.getStore()).toBeUndefined();
    });
  });

  test("returns undefined", () => {
    expect(new AsyncLocalStorage().disable()).toBeUndefined();
  });
});
