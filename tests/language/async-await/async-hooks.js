/*---
description: Async beforeEach and afterEach hooks
features: [async-await]
---*/

const log = [];

describe("async hooks", () => {
  beforeEach(async () => {
    const val = await Promise.resolve("setup");
    log.push(val);
  });

  afterEach(async () => {
    const val = await Promise.resolve("teardown");
    log.push(val);
  });

  test("async beforeEach runs before test", () => {
    expect(log).toContain("setup");
  });

  test("async beforeEach runs before each test", () => {
    const setupCount = log.filter((x) => x === "setup").length;
    expect(setupCount).toBe(2);
  });

  test("async afterEach runs after previous tests", () => {
    const teardownCount = log.filter((x) => x === "teardown").length;
    expect(teardownCount).toBe(2);
  });

  test("async hooks with multiple awaits", () => {
    const totalEntries = log.length;
    expect(totalEntries).toBeGreaterThan(0);
  });
});
