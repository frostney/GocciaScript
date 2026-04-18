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

  test("until at extreme range does not overflow", () => {
    // Span of ~200 million days — would overflow Int64 if collapsed to total ns
    const i1 = Temporal.Instant.fromEpochMilliseconds(-8640000000000000);
    const i2 = Temporal.Instant.fromEpochMilliseconds(8640000000000000);
    const dur = i1.until(i2);
    // Total = 17280000000000000 ms = 4800000000 hours
    expect(dur.hours).toBe(4800000000);
    expect(dur.minutes).toBe(0);
    expect(dur.seconds).toBe(0);
    expect(dur.milliseconds).toBe(0);
  });
});
