/*---
description: Cleanup jobs for many registrations collected at once drain in linear time, in order, and unregister still finds the ones not yet cleaned up
features: [FinalizationRegistry, Goccia, queueMicrotask]
---*/

// A GC-stress build collects every 8 instructions, and each collection walks
// every registration, so a stress run registers fewer.
const count = typeof Goccia !== "undefined" && Goccia.build.gcStress ? 2000 : 50000;

describe.runIf(typeof Goccia !== "undefined")("FinalizationRegistry cleanup drain", () => {
  test(`${count} registrations collected by one collection all reach the callback`, () => {
    const seen = [];
    let done;
    const result = new Promise((resolve) => {
      done = resolve;
    });
    const registry = new FinalizationRegistry((held) => {
      seen.push(held);
      if (seen.length === count) {
        done(seen);
      }
    });
    globalThis.__finalizationRegistryKeep = registry;

    queueMicrotask(() => {
      for (const i of Array.from({ length: count }, (_, k) => k)) {
        registry.register({}, i);
      }
    });
    queueMicrotask(() => {
      Goccia.gc();
    });
    queueMicrotask(() => {
      Goccia.gc();
    });

    return result.then((finalSeen) => {
      globalThis.__finalizationRegistryKeep = undefined;
      expect(finalSeen.length).toBe(count);
      // One collection empties the cells from the last registered to the
      // first, and their callbacks run in that order.
      expect(finalSeen.every((held, i) => held === count - 1 - i)).toBe(true);
    });
  });

  test("unregister in a cleanup callback removes the cells still waiting for theirs", () => {
    const log = [];
    const token = {};
    const registry = new FinalizationRegistry((held) => {
      log.push(held);
      if (held === 9) {
        // 9 has been cleaned up; 8, 6, 4, 2 and 0 are still waiting.
        log.push(registry.unregister(token));
        log.push(registry.unregister(token));
      }
    });
    globalThis.__finalizationRegistryKeep = registry;

    const result = new Promise((resolve) => {
      queueMicrotask(() => {
        for (const i of [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]) {
          if (i % 2 === 0) {
            registry.register({}, i, token);
          } else {
            registry.register({}, i);
          }
        }
      });
      queueMicrotask(() => {
        Goccia.gc();
      });
      queueMicrotask(() => {
        Goccia.gc();
      });
      queueMicrotask(() => {
        setTimeout(() => resolve(log.slice()), 0);
      });
    });

    return result.then((finalLog) => {
      globalThis.__finalizationRegistryKeep = undefined;
      expect(finalLog).toEqual([9, true, false, 7, 5, 3, 1]);
    });
  });
});
