/*---
description: Temporal.ZonedDateTime.prototype.until
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.ZonedDateTime.prototype.until", () => {
  test("until() default returns hours", () => {
    const z1 = Temporal.Instant.fromEpochMilliseconds(0).toZonedDateTimeISO("UTC");
    const z2 = Temporal.Instant.fromEpochMilliseconds(7200000).toZonedDateTimeISO("UTC");
    const dur = z1.until(z2);
    expect(dur.hours).toBe(2);
    expect(dur.minutes).toBe(0);
  });

  test("until() with largestUnit days", () => {
    const z1 = Temporal.Instant.fromEpochMilliseconds(0).toZonedDateTimeISO("UTC");
    const z2 = Temporal.Instant.fromEpochMilliseconds(90000000).toZonedDateTimeISO("UTC");
    const dur = z1.until(z2, { largestUnit: "days" });
    expect(dur.days).toBe(1);
    expect(dur.hours).toBe(1);
  });

  test("until() with largestUnit minutes", () => {
    const z1 = Temporal.Instant.fromEpochMilliseconds(0).toZonedDateTimeISO("UTC");
    const z2 = Temporal.Instant.fromEpochMilliseconds(5400000).toZonedDateTimeISO("UTC");
    const dur = z1.until(z2, { largestUnit: "minutes" });
    expect(dur.minutes).toBe(90);
    expect(dur.hours).toBe(0);
  });

  test("until() with largestUnit seconds", () => {
    const z1 = Temporal.Instant.fromEpochMilliseconds(0).toZonedDateTimeISO("UTC");
    const z2 = Temporal.Instant.fromEpochMilliseconds(90000).toZonedDateTimeISO("UTC");
    const dur = z1.until(z2, { largestUnit: "seconds" });
    expect(dur.seconds).toBe(90);
    expect(dur.minutes).toBe(0);
  });

  test("until() with largestUnit microseconds does not overflow", () => {
    const z1 = Temporal.Instant.fromEpochMilliseconds(-8640000000000000).toZonedDateTimeISO("UTC");
    const z2 = Temporal.Instant.fromEpochMilliseconds(8640000000000000).toZonedDateTimeISO("UTC");
    const dur = z1.until(z2, { largestUnit: "microseconds" });
    expect(dur.toString()).toBe("PT17280000000000S");
    expect(dur.nanoseconds).toBe(0);
  });

  test("until() with largestUnit nanoseconds formats balanced seconds", () => {
    const z1 = Temporal.Instant.fromEpochMilliseconds(0).toZonedDateTimeISO("UTC");
    const z2 = Temporal.Instant.fromEpochMilliseconds(5000).toZonedDateTimeISO("UTC");
    const dur = z1.until(z2, { largestUnit: "nanoseconds" });
    expect(dur.nanoseconds).toBe(5000000000);
    expect(dur.toString()).toBe("PT5S");
  });

  test("until() with smallestUnit minutes truncates seconds", () => {
    const z1 = Temporal.Instant.fromEpochMilliseconds(0).toZonedDateTimeISO("UTC");
    const z2 = Temporal.Instant.fromEpochMilliseconds(5400000 + 30000).toZonedDateTimeISO("UTC");
    const dur = z1.until(z2, { largestUnit: "hours", smallestUnit: "minutes" });
    expect(dur.hours).toBe(1);
    expect(dur.minutes).toBe(30);
    expect(dur.seconds).toBe(0);
  });

  test("until() with smallestUnit minutes and roundingMode ceil", () => {
    const z1 = Temporal.Instant.fromEpochMilliseconds(0).toZonedDateTimeISO("UTC");
    const z2 = Temporal.Instant.fromEpochMilliseconds(5400000 + 30000).toZonedDateTimeISO("UTC");
    const dur = z1.until(z2, { largestUnit: "hours", smallestUnit: "minutes", roundingMode: "ceil" });
    expect(dur.hours).toBe(1);
    expect(dur.minutes).toBe(31);
  });
});
