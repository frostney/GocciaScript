/*---
description: FinalizationRegistry integrates with Goccia.gc and the finalization cleanup queue
features: [FinalizationRegistry, Goccia, queueMicrotask]
---*/

const hasGoccia = typeof Goccia !== "undefined";

describe.runIf(hasGoccia)("FinalizationRegistry GC behavior", () => {
  test("cleanup is not synchronous in Goccia.gc and runs after normal microtasks", () => {
    const log = [];
    let done;
    const registry = new FinalizationRegistry((held) => {
      log.push("finalizer:" + held);
      done(log.slice());
    });
    globalThis.__finalizationRegistryKeep = registry;

    const result = new Promise((resolve) => {
      done = resolve;
    });

    queueMicrotask(() => {
      let target = {};
      registry.register(target, "held");
      target = null;
    });
    queueMicrotask(() => {
      Goccia.gc();
    });
    queueMicrotask(() => {
      Goccia.gc();
      log.push("after-gc");
      queueMicrotask(() => log.push("microtask"));
      expect(log).toEqual(["after-gc"]);
    });

    return result.then((finalLog) => {
      globalThis.__finalizationRegistryKeep = undefined;
      expect(registry.unregister({})).toBe(false);
      expect(finalLog).toEqual(["after-gc", "microtask", "finalizer:held"]);
    });
  });

  test("held value is delivered to cleanup callback", () => {
    const held = { marker: "held" };
    let done;
    const registry = new FinalizationRegistry((value) => {
      done(value);
    });
    globalThis.__finalizationRegistryKeep = registry;

    const result = new Promise((resolve) => {
      done = resolve;
    });

    queueMicrotask(() => {
      let target = {};
      registry.register(target, held);
      target = null;
    });
    queueMicrotask(() => {
      Goccia.gc();
    });
    queueMicrotask(() => {
      Goccia.gc();
    });

    return result.then((value) => {
      globalThis.__finalizationRegistryKeep = undefined;
      expect(registry.unregister({})).toBe(false);
      expect(value).toBe(held);
      expect(value.marker).toBe("held");
    });
  });

  test("unregister prevents queued cleanup for the removed cell", () => {
    const log = [];
    const token = {};
    const registry = new FinalizationRegistry((held) => {
      log.push(held);
    });

    const result = new Promise((resolve) => {
      queueMicrotask(() => {
        let target = {};
        registry.register(target, "held", token);
        target = null;
      });
      queueMicrotask(() => {
        expect(registry.unregister(token)).toBe(true);
        Goccia.gc();
      });
      queueMicrotask(() => {
        Goccia.gc();
      });
      queueMicrotask(() => {
        resolve(log.slice());
      });
    });

    return result.then((finalLog) => {
      expect(finalLog).toEqual([]);
    });
  });

  test("unregister does not remove cells without a matching token", () => {
    const log = [];
    const token = {};
    let done;
    const registry = new FinalizationRegistry((held) => {
      log.push(held);
      done(log.slice());
    });
    globalThis.__finalizationRegistryKeep = registry;

    const result = new Promise((resolve) => {
      done = resolve;
    });

    queueMicrotask(() => {
      let target = {};
      registry.register(target, "held");
      target = null;
    });
    queueMicrotask(() => {
      expect(registry.unregister(token)).toBe(false);
      Goccia.gc();
    });
    queueMicrotask(() => {
      Goccia.gc();
    });

    return result.then((finalLog) => {
      globalThis.__finalizationRegistryKeep = undefined;
      expect(registry.unregister({})).toBe(false);
      expect(finalLog).toEqual(["held"]);
    });
  });

  test("cleanup jobs enqueue microtasks before the next cleanup job runs", () => {
    const log = [];
    let cleanupCount = 0;
    let done;
    const result = new Promise((resolve) => {
      done = resolve;
    });
    const registry = new FinalizationRegistry((held) => {
      log.push("finalizer:" + held);
      queueMicrotask(() => {
        log.push("microtask:" + held);
        cleanupCount = cleanupCount + 1;
        if (cleanupCount === 2) {
          done(log.slice());
        }
      });
    });
    globalThis.__finalizationRegistryKeep = registry;

    queueMicrotask(() => {
      let first = {};
      let second = {};
      registry.register(first, "first");
      registry.register(second, "second");
      first = null;
      second = null;
    });
    queueMicrotask(() => {
      Goccia.gc();
    });
    queueMicrotask(() => {
      Goccia.gc();
    });

    return result.then((finalLog) => {
      globalThis.__finalizationRegistryKeep = undefined;
      expect(registry.unregister({})).toBe(false);
      expect(finalLog).toEqual([
        "finalizer:second",
        "microtask:second",
        "finalizer:first",
        "microtask:first"
      ]);
    });
  });
});
