/*---
description: AsyncLocalStorage.prototype.disable deletes the binding from the current frame only
features: [AsyncLocalStorage]
---*/

import { AsyncLocalStorage } from "node:async_hooks";

// disable() is a frame edit, not an instance-wide switch. Everything below was
// probed against Node v24.0.1, whose AsyncContextFrame model this follows: a
// continuation captured before the disable keeps the store it captured,
// because the disable never reached that frame.
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

  test("run binds again after a disable", () => {
    const als = new AsyncLocalStorage({ defaultValue: "DEF" });
    als.disable();
    als.run("again", () => {
      expect(als.getStore()).toBe("again");
    });
    expect(als.getStore()).toBe("DEF");
  });

  test("enterWith binds again after a disable", () => {
    const als = new AsyncLocalStorage({ defaultValue: "DEF" });
    als.disable();
    als.enterWith("entered");
    expect(als.getStore()).toBe("entered");
  });

  test("disabling inside a run drops the store for the rest of it", () => {
    const als = new AsyncLocalStorage();
    als.run("bound", () => {
      expect(als.getStore()).toBe("bound");
      als.disable();
      expect(als.getStore()).toBeUndefined();
    });
  });

  test("disabling inside an inner run leaves the outer store intact", () => {
    const als = new AsyncLocalStorage();
    als.run("outer", () => {
      als.run("inner", () => {
        als.disable();
        expect(als.getStore()).toBeUndefined();
      });
      expect(als.getStore()).toBe("outer");
    });
  });

  test("the disabled frame stays disabled across its own awaits", async () => {
    const als = new AsyncLocalStorage();
    await als.run("bound", async () => {
      als.disable();
      expect(als.getStore()).toBeUndefined();
      await Promise.resolve();
      expect(als.getStore()).toBeUndefined();
    });
  });

  test("a continuation captured before the disable keeps its store", async () => {
    const als = new AsyncLocalStorage();
    let settle;
    const pending = new Promise((resolve) => {
      settle = resolve;
    });
    let seen = "unset";
    als.run("captured", () => {
      pending.then(() => {
        seen = als.getStore();
      });
    });
    als.disable();
    settle(1);
    await pending;
    await Promise.resolve();
    expect(seen).toBe("captured");
  });

  test("a disable followed by a re-binding run still leaves the capture intact", async () => {
    const als = new AsyncLocalStorage();
    let settle;
    const pending = new Promise((resolve) => {
      settle = resolve;
    });
    let seen = "unset";
    als.run("captured", () => {
      pending.then(() => {
        seen = als.getStore();
      });
    });
    als.disable();
    als.run("rebound", () => {});
    settle(1);
    await pending;
    await Promise.resolve();
    expect(seen).toBe("captured");
  });

  test("exit on a disabled instance reports undefined, not the default value", () => {
    const als = new AsyncLocalStorage({ defaultValue: "DEF" });
    als.disable();
    als.exit(() => {
      expect(als.getStore()).toBeUndefined();
    });
    expect(als.getStore()).toBe("DEF");
  });

  test("returns undefined", () => {
    expect(new AsyncLocalStorage().disable()).toBeUndefined();
  });
});
