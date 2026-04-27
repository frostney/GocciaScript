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

  test("until() negative with largestUnit minutes", () => {
    const i1 = Temporal.Instant.fromEpochMilliseconds(90000);
    const i2 = Temporal.Instant.fromEpochMilliseconds(0);
    expect(i1.until(i2, { largestUnit: "minutes" }).toString()).toBe("-PT1M30S");
  });
});
