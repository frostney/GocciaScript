/*---
description: Temporal.Instant constructor
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.Instant constructor", () => {
  test("epochNanoseconds getter", () => {
    const instant = Temporal.Instant.fromEpochMilliseconds(5000);
    expect(instant.epochNanoseconds).toBe(5000000000);
  });
});
