/*---
description: Temporal.Instant.prototype.subtract
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.Instant.prototype.subtract", () => {
  test("subtract() with time duration", () => {
    const instant = Temporal.Instant.fromEpochMilliseconds(3600000);
    const result = instant.subtract(new Temporal.Duration(0, 0, 0, 0, 1));
    expect(result.epochMilliseconds).toBe(0);
  });

  test("subtract() throws on weeks", () => {
    const instant = Temporal.Instant.fromEpochMilliseconds(0);
    expect(() => {
      instant.subtract(new Temporal.Duration(0, 0, 1));
    }).toThrow(RangeError);
  });
});
