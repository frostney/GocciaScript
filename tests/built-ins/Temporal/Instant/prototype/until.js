/*---
description: Temporal.Instant.prototype.until
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.Instant.prototype.until", () => {
  test("until()", () => {
    const i1 = Temporal.Instant.fromEpochMilliseconds(0);
    const i2 = Temporal.Instant.fromEpochMilliseconds(3600000);
    const dur = i1.until(i2);
    expect(dur.hours).toBe(1);
    expect(dur.minutes).toBe(0);
  });
});
