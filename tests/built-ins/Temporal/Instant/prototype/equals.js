/*---
description: Temporal.Instant.prototype.equals
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.Instant.prototype.equals", () => {
  test("equals()", () => {
    const i1 = Temporal.Instant.fromEpochMilliseconds(1000);
    const i2 = Temporal.Instant.fromEpochMilliseconds(1000);
    const i3 = Temporal.Instant.fromEpochMilliseconds(2000);
    expect(i1.equals(i2)).toBe(true);
    expect(i1.equals(i3)).toBe(false);
  });
});
