/*---
description: AsyncLocalStorage.prototype.getStore reports the bound store, or the instance default value
features: [AsyncLocalStorage]
---*/

import { AsyncLocalStorage } from "node:async_hooks";

describe("AsyncLocalStorage.prototype.getStore", () => {
  test("is undefined outside any run", () => {
    expect(new AsyncLocalStorage().getStore()).toBeUndefined();
  });

  test("reports the configured default value when nothing is bound", () => {
    const als = new AsyncLocalStorage({ defaultValue: "DEF" });
    expect(als.getStore()).toBe("DEF");
    als.run("bound", () => {
      expect(als.getStore()).toBe("bound");
    });
    expect(als.getStore()).toBe("DEF");
  });

  test("an explicitly bound undefined store wins over the default value", () => {
    const als = new AsyncLocalStorage({ defaultValue: "DEF" });
    als.run(undefined, () => {
      expect(als.getStore()).toBeUndefined();
    });
  });

  test("name reports the configured name, or the empty string", () => {
    expect(new AsyncLocalStorage({ name: "requests" }).name).toBe("requests");
    expect(new AsyncLocalStorage().name).toBe("");
  });
});
