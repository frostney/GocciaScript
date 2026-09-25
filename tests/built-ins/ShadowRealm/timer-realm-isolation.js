/*---
description: a child realm's synchronous wait does not run the parent realm's timers
features: [ShadowRealm, Timers]
---*/

// The virtual timer queue is one per thread, and a ShadowRealm child engine
// runs on the same thread as its parent. The drains therefore have to ask
// whether the realm currently executing is the one whose timers the queue
// carries: without that, a wait reached from inside the child ran the PARENT's
// callbacks while the child's realm was installed as current, so parent code
// executed against child intrinsics — an isolation break that nothing reports.
//
// This lives here rather than beside the other timer tests because it needs
// `unsafe-shadowrealm`, and a directory carrying that flag cannot also import a
// runtime module: doing so faults the engine during the parallel runner's
// warm-up. That is a separate, pre-existing defect — `node:async_hooks` and
// `goccia:csv` reproduce it identically — so this file uses the timer globals
// only. See docs/adr/0113-deterministic-virtual-timer-queue.md.

// `Array.fromAsync` over an async iterator awaits on the caller's own stack
// rather than suspending, which is what makes the drain reachable at all: an
// ordinary `async` function suspends and resumes through a promise reaction, so
// its `await` never gets there. This shape does.
const PENDING_FROM_ASYNC =
  "(() => {" +
  "  const iterable = {" +
  "    [Symbol.asyncIterator]: () => ({ next: () => new Promise(() => {}) })" +
  "  };" +
  "  try { Array.fromAsync(iterable); } catch (error) { /* never settles */ }" +
  "  return 'child-ran';" +
  "})()";

const pendingFromAsyncHere = () => {
  const iterable = {
    [Symbol.asyncIterator]: () => ({ next: () => new Promise(() => {}) }),
  };
  try {
    Array.fromAsync(iterable);
  } catch (error) {
    // never settles
  }
  return "parent-ran";
};

describe("a child realm does not run the parent's timers", () => {
  test("the drain is reachable at all from this shape", () => {
    let parentTimerRan = false;
    setTimeout(() => {
      parentTimerRan = true;
    }, 0);

    // Run in the parent, where the queue's owner IS the current realm: the
    // timer runs. Without this half the isolation test below would pass for the
    // wrong reason — because nothing reached the drain either way.
    expect(pendingFromAsyncHere()).toBe("parent-ran");
    expect(parentTimerRan).toBe(true);
  });

  test("the same wait inside a ShadowRealm leaves the parent's queue alone", () => {
    let parentTimerRan = false;
    const id = setTimeout(() => {
      parentTimerRan = true;
    }, 0);

    const realm = new ShadowRealm();
    const result = realm.evaluate(PENDING_FROM_ASYNC);

    expect(result).toBe("child-ran");
    expect(parentTimerRan).toBe(false);

    clearTimeout(id);
  });
});
