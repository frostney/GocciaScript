/*---
description: AsyncLocalStorage.prototype.exit runs a callback with no store bound
features: [AsyncLocalStorage]
---*/

import { AsyncLocalStorage } from "node:async_hooks";

describe("AsyncLocalStorage.prototype.exit", () => {
  test("clears the store for the callback and restores it afterwards", () => {
    const als = new AsyncLocalStorage();
    als.run("with-store", () => {
      als.exit(() => {
        expect(als.getStore()).toBeUndefined();
      });
      expect(als.getStore()).toBe("with-store");
    });
  });

  test("clears the store even when a default value is configured", () => {
    const als = new AsyncLocalStorage({ defaultValue: "DEF" });
    als.exit(() => {
      expect(als.getStore()).toBeUndefined();
    });
    expect(als.getStore()).toBe("DEF");
  });

  test("returns the callback result and forwards extra arguments", () => {
    const als = new AsyncLocalStorage();
    expect(als.exit(() => 7)).toBe(7);
    expect(als.exit((first, second) => [first, second], 1, 2)).toEqual([1, 2]);
  });

  test("leaves other instances untouched", () => {
    const first = new AsyncLocalStorage();
    const second = new AsyncLocalStorage();
    first.run("A", () => {
      second.run("B", () => {
        first.exit(() => {
          expect(first.getStore()).toBeUndefined();
          expect(second.getStore()).toBe("B");
        });
      });
    });
  });
});
