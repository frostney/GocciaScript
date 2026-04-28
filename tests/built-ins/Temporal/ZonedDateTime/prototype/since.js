/*---
description: Temporal.ZonedDateTime.prototype.since
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.ZonedDateTime.prototype.since", () => {
  test("since() default returns hours", () => {
    const z1 = Temporal.Instant.fromEpochMilliseconds(7200000).toZonedDateTimeISO("UTC");
    const z2 = Temporal.Instant.fromEpochMilliseconds(0).toZonedDateTimeISO("UTC");
    const dur = z1.since(z2);
    expect(dur.hours).toBe(2);
    expect(dur.minutes).toBe(0);
  });

  test("since() with largestUnit minutes", () => {
    const z1 = Temporal.Instant.fromEpochMilliseconds(5400000).toZonedDateTimeISO("UTC");
    const z2 = Temporal.Instant.fromEpochMilliseconds(0).toZonedDateTimeISO("UTC");
    const dur = z1.since(z2, { largestUnit: "minutes" });
    expect(dur.minutes).toBe(90);
    expect(dur.hours).toBe(0);
  });

  test("since() with largestUnit microseconds does not overflow", () => {
    const z1 = Temporal.Instant.fromEpochMilliseconds(8640000000000000).toZonedDateTimeISO("UTC");
    const z2 = Temporal.Instant.fromEpochMilliseconds(-8640000000000000).toZonedDateTimeISO("UTC");
    const dur = z1.since(z2, { largestUnit: "microseconds" });
    expect(dur.toString()).toBe("PT17280000000000S");
    expect(dur.nanoseconds).toBe(0);
  });

  test("since() with largestUnit nanoseconds formats balanced seconds", () => {
    const z1 = Temporal.Instant.fromEpochMilliseconds(5000).toZonedDateTimeISO("UTC");
    const z2 = Temporal.Instant.fromEpochMilliseconds(0).toZonedDateTimeISO("UTC");
    const dur = z1.since(z2, { largestUnit: "nanoseconds" });
    expect(dur.nanoseconds).toBe(5000000000);
    expect(dur.toString()).toBe("PT5S");
  });
});
