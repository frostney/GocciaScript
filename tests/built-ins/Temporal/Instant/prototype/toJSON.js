/*---
description: Temporal.Instant.prototype.toJSON
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.Instant.prototype.toJSON", () => {
  test("toJSON()", () => {
    const instant = Temporal.Instant.fromEpochMilliseconds(0);
    expect(instant.toJSON()).toBe(instant.toString());
  });
});
