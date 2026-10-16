/*---
description: a module that schedules a timer from a job the final idle wait runs still resumes once the timer fires
features: [top-level-await, Atomics, Timers]
---*/

// The Atomics.waitAsync timeout settles inside the run's final idle wait,
// after the timer extension's pass. The job that resumes the body then
// schedules a timer, and nothing used to run it: the body stayed suspended.
// The test is registered before the awaits and runs after the body.
let reached = "the body never resumed";

describe("top-level await on a timer scheduled late in the idle wait", () => {
  test("the timer runs and the body finishes", () => {
    expect(reached).toBe("after the timer");
  });
});

const view = new Int32Array(new SharedArrayBuffer(4));
await Promise.resolve();
await Atomics.waitAsync(view, 0, 0, 10).value;
await new Promise((resolve) => setTimeout(resolve, 10));
reached = "after the timer";
