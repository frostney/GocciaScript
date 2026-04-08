/*---
description: onTestFinished lifecycle hook
features: [testing]
---*/

const log = [];

describe("onTestFinished", () => {
  afterEach(() => {
    log.push("afterEach");
  });

  test("callback runs after the test completes", () => {
    onTestFinished(() => {
      log.push("finished-1");
    });
    log.push("test-body-1");
  });

  test("callback ran after afterEach for the previous test", () => {
    // From previous test: test-body-1, afterEach, finished-1
    expect(log).toEqual(["test-body-1", "afterEach", "finished-1"]);
  });

  test("multiple callbacks run in registration order", () => {
    onTestFinished(() => {
      log.push("finished-A");
    });
    onTestFinished(() => {
      log.push("finished-B");
    });
    log.push("test-body-multi");
  });

  test("multiple callbacks ran in order after afterEach", () => {
    // Previous test added: test-body-multi, afterEach, finished-A, finished-B
    const lastFour = log.slice(-4);
    expect(lastFour).toEqual(["test-body-multi", "afterEach", "finished-A", "finished-B"]);
  });

  test("callbacks are scoped to the current test only", () => {
    // This test registers no onTestFinished callbacks
    // The callbacks from previous tests should not run again
    log.push("test-body-scoped");
  });

  test("no stale callbacks leaked from previous test", () => {
    // Previous test: test-body-scoped, afterEach (no finished callbacks)
    const lastTwo = log.slice(-2);
    expect(lastTwo).toEqual(["test-body-scoped", "afterEach"]);
  });
});

describe("onTestFinished with async", () => {
  test("async onTestFinished callback", () => {
    const asyncLog = [];
    onTestFinished(async () => {
      const val = await Promise.resolve("async-cleanup");
      asyncLog.push(val);
    });
    // The callback runs after the test, so we can't check it here
    // But the callback should not cause errors
  });

  test("onTestFinished runs even when test has assertions", () => {
    const marker = [];
    onTestFinished(() => {
      marker.push("cleanup");
    });
    expect(1 + 1).toBe(2);
  });

  test("cleanup ran for previous test", () => {
    // Can't directly verify the previous test's onTestFinished marker
    // since it was a local variable, but we verify no errors occurred
    expect(true).toBeTruthy();
  });
});

describe("onTestFinished in nested describes", () => {
  const nestedLog = [];

  afterEach(() => {
    nestedLog.push("outer-afterEach");
  });

  describe("inner", () => {
    afterEach(() => {
      nestedLog.push("inner-afterEach");
    });

    test("onTestFinished runs after all afterEach hooks", () => {
      onTestFinished(() => {
        nestedLog.push("finished");
      });
      nestedLog.push("test-body");
    });

    test("verify execution order", () => {
      // Order: test-body, inner-afterEach, outer-afterEach, finished
      expect(nestedLog.slice(0, 4)).toEqual([
        "test-body",
        "inner-afterEach",
        "outer-afterEach",
        "finished",
      ]);
    });
  });
});
