/*---
description: Temporal.Instant.prototype.until
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.Instant.prototype.until", () => {
  test("until() default returns hours", () => {
    const i1 = Temporal.Instant.fromEpochMilliseconds(0);
    const i2 = Temporal.Instant.fromEpochMilliseconds(3600000);
    const dur = i1.until(i2);
    expect(dur.hours).toBe(1);
    expect(dur.minutes).toBe(0);
  });

  test("until at extreme range does not overflow", () => {
    const i1 = Temporal.Instant.fromEpochMilliseconds(-8640000000000000);
    const i2 = Temporal.Instant.fromEpochMilliseconds(8640000000000000);
    const dur = i1.until(i2);
    expect(dur.hours).toBe(4800000000);
    expect(dur.minutes).toBe(0);
    expect(dur.seconds).toBe(0);
    expect(dur.milliseconds).toBe(0);
  });

  test("until at extreme range with largestUnit microseconds does not overflow", () => {
    const i1 = Temporal.Instant.fromEpochMilliseconds(-8640000000000000);
    const i2 = Temporal.Instant.fromEpochMilliseconds(8640000000000000);
    const dur = i1.until(i2, { largestUnit: "microseconds" });
    expect(dur.toString()).toBe("PT17280000000000S");
    expect(dur.nanoseconds).toBe(0);
  });

  test("until at extreme range with largestUnit nanoseconds does not overflow", () => {
    const i1 = Temporal.Instant.fromEpochMilliseconds(-8640000000000000);
    const i2 = Temporal.Instant.fromEpochMilliseconds(8640000000000000);
    const dur = i1.until(i2, { largestUnit: "nanoseconds" });
    expect(dur.toString()).toBe("PT17280000000000S");
  });

  test("until negative at extreme range with largestUnit nanoseconds does not overflow", () => {
    const i1 = Temporal.Instant.fromEpochMilliseconds(8640000000000000);
    const i2 = Temporal.Instant.fromEpochMilliseconds(-8640000000000000);
    const dur = i1.until(i2, { largestUnit: "nanoseconds" });
    expect(dur.toString()).toBe("-PT17280000000000S");
  });

  test("until() with largestUnit minutes", () => {
    const i1 = Temporal.Instant.fromEpochMilliseconds(0);
    const i2 = Temporal.Instant.fromEpochMilliseconds(90061000);
    expect(i1.until(i2, { largestUnit: "minutes" }).toString()).toBe("PT1501M1S");
  });

  test("until() with largestUnit seconds", () => {
    const i1 = Temporal.Instant.fromEpochMilliseconds(0);
    const i2 = Temporal.Instant.fromEpochMilliseconds(90061000);
    expect(i1.until(i2, { largestUnit: "seconds" }).toString()).toBe("PT90061S");
  });

  test("until() with largestUnit milliseconds", () => {
    const i1 = Temporal.Instant.fromEpochMilliseconds(0);
    const i2 = Temporal.Instant.fromEpochMilliseconds(5000);
    const dur = i1.until(i2, { largestUnit: "milliseconds" });
    expect(dur.milliseconds).toBe(5000);
    expect(dur.seconds).toBe(0);
  });

  test("until() with largestUnit nanoseconds formats balanced seconds", () => {
    const i1 = Temporal.Instant.fromEpochMilliseconds(0);
    const i2 = Temporal.Instant.fromEpochMilliseconds(5000);
    const dur = i1.until(i2, { largestUnit: "nanoseconds" });
    expect(dur.nanoseconds).toBe(5000000000);
    expect(dur.toString()).toBe("PT5S");
  });

  test("until() negative with largestUnit minutes", () => {
    const i1 = Temporal.Instant.fromEpochMilliseconds(90000);
    const i2 = Temporal.Instant.fromEpochMilliseconds(0);
    expect(i1.until(i2, { largestUnit: "minutes" }).toString()).toBe("-PT1M30S");
  });

  test("until() with smallestUnit seconds truncates milliseconds", () => {
    const i1 = Temporal.Instant.fromEpochMilliseconds(0);
    const i2 = Temporal.Instant.fromEpochMilliseconds(5500);
    expect(i1.until(i2, { smallestUnit: "seconds" }).toString()).toBe("PT5S");
  });

  test("until() with smallestUnit seconds and roundingMode halfExpand", () => {
    const i1 = Temporal.Instant.fromEpochMilliseconds(0);
    const i2 = Temporal.Instant.fromEpochMilliseconds(5500);
    expect(i1.until(i2, { smallestUnit: "seconds", roundingMode: "halfExpand" }).toString()).toBe("PT6S");
  });

  test("until() with smallestUnit minutes truncates seconds", () => {
    const i1 = Temporal.Instant.fromEpochMilliseconds(0);
    const i2 = Temporal.Instant.fromEpochMilliseconds(90500);
    expect(i1.until(i2, { largestUnit: "minutes", smallestUnit: "minutes" }).toString()).toBe("PT1M");
  });

  test("until() with roundingIncrement 15 and smallestUnit minutes", () => {
    const i1 = Temporal.Instant.fromEpochMilliseconds(0);
    const i2 = Temporal.Instant.fromEpochMilliseconds(50 * 60000);
    expect(i1.until(i2, { largestUnit: "hours", smallestUnit: "minutes", roundingIncrement: 15 }).toString()).toBe("PT45M");
  });
});
