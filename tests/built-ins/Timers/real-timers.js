/*---
description: real-mode timers run where the engine would otherwise have nothing left to do
features: [Timers]
---*/

import { AsyncLocalStorage } from "node:async_hooks";
import {
  advanceTimersByTime,
  isFakeTimers,
  useFakeTimers,
  useRealTimers,
} from "goccia:timers";

// Without fake timers the queue is still virtual: no wall time passes, the
// clock simply jumps to the next due timer whenever the engine is about to run
// out of work. The two points where that happens are an `await` on a promise a
// timer will settle, and the end of the run.

describe("real-mode timers", () => {
  test("awaiting a promise a timer settles resolves through the queue", async () => {
    expect(isFakeTimers()).toBe(false);

    const value = await new Promise((resolve) => {
      setTimeout(() => resolve("settled"), 50);
    });

    expect(value).toBe("settled");
  });

  test("a chain of timers resolves in order", async () => {
    const log = [];
    await new Promise((resolve) => {
      setTimeout(() => {
        log.push("first");
        setTimeout(() => {
          log.push("second");
          resolve();
        }, 0);
      }, 0);
    });

    expect(log).toEqual(["first", "second"]);
  });

  test("a rejection from a timer callback reaches the awaiting frame", async () => {
    await expect(
      new Promise((resolve, reject) => {
        setTimeout(() => reject(new Error("late failure")), 5);
      }),
    ).rejects.toThrow("late failure");
  });

  test("an interval can drive a promise and then be cleared", async () => {
    let ticks = 0;
    const id = setInterval(() => {
      ticks += 1;
    }, 10);

    await new Promise((resolve) => setTimeout(resolve, 35));
    clearInterval(id);

    expect(ticks).toBe(3);
  });
});

describe("async context through a timer callback", () => {
  test("a callback observes the store that was in effect where it was scheduled", async () => {
    const storage = new AsyncLocalStorage();
    const seen = [];

    const scheduled = storage.run("OUTER", () => {
      return new Promise((resolve) => {
        setTimeout(() => {
          seen.push(storage.getStore());
          resolve();
        }, 5);
      });
    });

    // The `run` has already returned by the time the timer fires, so a
    // callback that read the ambient context would see nothing. The snapshot
    // captured at registration is what carries the store into it.
    expect(storage.getStore()).toBeUndefined();
    await scheduled;

    expect(seen).toEqual(["OUTER"]);
    expect(storage.getStore()).toBeUndefined();
  });

  test("two timers scheduled under different stores stay separate", async () => {
    const storage = new AsyncLocalStorage();
    const seen = [];

    const first = storage.run("A", () =>
      new Promise((resolve) => {
        setTimeout(() => {
          seen.push(storage.getStore());
          resolve();
        }, 10);
      }),
    );
    const second = storage.run("B", () =>
      new Promise((resolve) => {
        setTimeout(() => {
          seen.push(storage.getStore());
          resolve();
        }, 5);
      }),
    );

    await second;
    await first;

    expect(seen).toEqual(["B", "A"]);
  });

  test("fake timers carry the same snapshot", () => {
    useFakeTimers();
    try {
      const storage = new AsyncLocalStorage();
      const seen = [];

      storage.run("FAKE", () => {
        setTimeout(() => seen.push(storage.getStore()), 5);
      });
      expect(seen).toEqual([]);

      // Advancing from outside the `run` still reaches the callback with the
      // store, because the context travels with the timer rather than with the
      // frame that advanced the clock.
      advanceTimersByTime(5);
      expect(seen).toEqual(["FAKE"]);
      expect(storage.getStore()).toBeUndefined();
    } finally {
      useRealTimers();
    }
  });
});
