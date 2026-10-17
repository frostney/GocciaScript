/*---
description: a module whose first top-level await settles at once still resumes once an Atomics.waitAsync timeout expires
features: [top-level-await, Atomics]
---*/

// The first await settles at once, so the rest of the body runs as a job in
// the run's final idle wait. A waiter with a timeout settles itself; the idle
// wait used to stop before it expired and leave the body suspended for good.
// The test is registered before the awaits and runs after the body, so it
// reads the value only a resumed body writes.
let outcome = "the body never resumed";

describe("top-level await on an Atomics.waitAsync timeout", () => {
  test("the waiter times out and the body runs on", () => {
    expect(outcome).toBe("timed-out");
  });
});

const view = new Int32Array(new SharedArrayBuffer(4));
await Promise.resolve();
outcome = await Atomics.waitAsync(view, 0, 0, 10).value;
