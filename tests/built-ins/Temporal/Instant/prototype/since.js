/*---
description: Temporal.Instant.prototype.since
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.Instant.prototype.since", () => {
  test("since() default returns hours", () => {
    const i1 = Temporal.Instant.fromEpochMilliseconds(3600000);
    const i2 = Temporal.Instant.fromEpochMilliseconds(0);
    const dur = i1.since(i2);
    expect(dur.hours).toBe(1);
  });

  test("since at extreme range with largestUnit microseconds does not overflow", () => {
    const i1 = Temporal.Instant.fromEpochMilliseconds(8640000000000000);
    const i2 = Temporal.Instant.fromEpochMilliseconds(-8640000000000000);
    const dur = i1.since(i2, { largestUnit: "microseconds" });
    expect(dur.microseconds).toBe(17280000000000000000);
    expect(dur.nanoseconds).toBe(0);
  });

  test("since at extreme range with largestUnit nanoseconds does not overflow", () => {
    const i1 = Temporal.Instant.fromEpochMilliseconds(8640000000000000);
    const i2 = Temporal.Instant.fromEpochMilliseconds(-8640000000000000);
    const dur = i1.since(i2, { largestUnit: "nanoseconds" });
    expect(dur.nanoseconds).toBe(17280000000000000000000);
    expect(dur.toString()).toBe("PT17280000000000S");
  });

  test("since negative at extreme range with largestUnit nanoseconds does not overflow", () => {
    const i1 = Temporal.Instant.fromEpochMilliseconds(-8640000000000000);
    const i2 = Temporal.Instant.fromEpochMilliseconds(8640000000000000);
    const dur = i1.since(i2, { largestUnit: "nanoseconds" });
    expect(dur.nanoseconds).toBe(-17280000000000000000000);
    expect(dur.toString()).toBe("-PT17280000000000S");
  });

  test("since() with largestUnit minutes", () => {
    const i1 = Temporal.Instant.fromEpochMilliseconds(90061000);
    const i2 = Temporal.Instant.fromEpochMilliseconds(0);
    expect(i1.since(i2, { largestUnit: "minutes" }).toString()).toBe("PT1501M1S");
  });

  test("since() with largestUnit seconds", () => {
    const i1 = Temporal.Instant.fromEpochMilliseconds(90061000);
    const i2 = Temporal.Instant.fromEpochMilliseconds(0);
    expect(i1.since(i2, { largestUnit: "seconds" }).toString()).toBe("PT90061S");
  });

  test("since() with smallestUnit seconds and roundingMode halfExpand", () => {
    const i1 = Temporal.Instant.fromEpochMilliseconds(5500);
    const i2 = Temporal.Instant.fromEpochMilliseconds(0);
    expect(i1.since(i2, { smallestUnit: "seconds", roundingMode: "halfExpand" }).toString()).toBe("PT6S");
  });

  test("since() with largestUnit nanoseconds formats balanced seconds", () => {
    const i1 = Temporal.Instant.fromEpochMilliseconds(5000);
    const i2 = Temporal.Instant.fromEpochMilliseconds(0);
    const dur = i1.since(i2, { largestUnit: "nanoseconds" });
    expect(dur.nanoseconds).toBe(5000000000);
    expect(dur.toString()).toBe("PT5S");
  });
});
