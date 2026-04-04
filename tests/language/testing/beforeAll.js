/*---
description: beforeAll runs once before the tests in its suite
features: [Test Assertions]
---*/

describe("beforeAll", () => {
  let callCount = 0;
  const events = [];

  beforeAll(() => {
    callCount += 1;
    events.push("beforeAll");
  });

  test("runs before the first test in the suite", () => {
    events.push("test-1");
    expect(callCount).toBe(1);
    expect(events).toEqual(["beforeAll", "test-1"]);
  });

  test("does not rerun for later tests", () => {
    events.push("test-2");
    expect(callCount).toBe(1);
    expect(events).toEqual(["beforeAll", "test-1", "test-2"]);
  });
});
