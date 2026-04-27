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
});
