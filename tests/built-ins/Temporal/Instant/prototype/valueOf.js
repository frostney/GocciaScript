/*---
description: Temporal.Instant.prototype.valueOf
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.Instant.prototype.valueOf", () => {
  test("valueOf throws", () => {
    const instant = Temporal.Instant.fromEpochMilliseconds(0);
    expect(() => instant.valueOf()).toThrow(TypeError);
  });
});
