// Regression: importing a runtime module (node:async_hooks) from a directory
// that enables --unsafe-shadowrealm used to segfault during a multi-file run on
// one worker thread. EnableShadowRealm pins ShadowRealm.prototype for the
// engine's lifetime but never released the pin at teardown, so the next
// garbage collection after this engine was torn down walked the freed prototype
// into its released realm graph (a module import binding still pointing at the
// released node:async_hooks module) and faulted in MarkRoots. This file plus a
// sibling under the same unsafe-shadowrealm config reproduces the two-file,
// one-worker warm-up that triggered it; both must run clean in both modes.

import { AsyncLocalStorage } from "node:async_hooks";

describe("ShadowRealm + node:async_hooks warm-up", () => {
  test("imports the runtime module without faulting", () => {
    expect(typeof AsyncLocalStorage).toBe("function");
    const als = new AsyncLocalStorage();
    expect(als.getStore()).toBeUndefined();
    expect(als.run("bound", () => als.getStore())).toBe("bound");
  });

  test("ShadowRealm is still available alongside the runtime module", () => {
    expect(typeof ShadowRealm).toBe("function");
    const realm = new ShadowRealm();
    expect(realm.evaluate("1 + 1")).toBe(2);
  });
});
