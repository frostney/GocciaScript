/*---
description: Temporal.Instant.prototype.toString
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.Instant.prototype.toString", () => {
  test("toString() for epoch", () => {
    const instant = Temporal.Instant.fromEpochMilliseconds(0);
    expect(instant.toString()).toBe("1970-01-01T00:00:00Z");
  });

  test("toString() for specific time", () => {
    // 2024-01-15T12:30:45.000Z = epoch ms
    const ms = 1705321845000;
    const instant = Temporal.Instant.fromEpochMilliseconds(ms);
    expect(instant.toString()).toBe("2024-01-15T12:30:45Z");
  });
});
