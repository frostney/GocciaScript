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

  test("until() rounds days using spring-forward day length", () => {
    const start = Temporal.ZonedDateTime.from("2024-03-09T00:00:00-05:00[America/New_York]");
    const beforeHalf = Temporal.ZonedDateTime.from("2024-03-10T12:00:00-04:00[America/New_York]");
    const afterHalf = Temporal.ZonedDateTime.from("2024-03-10T12:30:00-04:00[America/New_York]");

    const unrounded = start.until(beforeHalf, { largestUnit: "days" });
    expect(unrounded.days).toBe(1);
    expect(unrounded.hours).toBe(11);

    const roundedBefore = start.until(beforeHalf, {
      largestUnit: "days",
      smallestUnit: "days",
      roundingMode: "halfExpand",
    });
    const roundedAfter = start.until(afterHalf, {
      largestUnit: "days",
      smallestUnit: "days",
      roundingMode: "halfExpand",
    });

    expect(roundedBefore.days).toBe(1);
    expect(roundedAfter.days).toBe(2);

    const roundedHours = start.until(afterHalf, {
      largestUnit: "days",
      smallestUnit: "hours",
      roundingMode: "halfExpand",
    });
    expect(roundedHours.days).toBe(1);
    expect(roundedHours.hours).toBe(12);
  });

  test("until() rounds days using fall-back day length", () => {
    const start = Temporal.ZonedDateTime.from("2024-11-02T00:00:00-04:00[America/New_York]");
    const afterHalf = Temporal.ZonedDateTime.from("2024-11-03T11:30:00-05:00[America/New_York]");

    const unrounded = start.until(afterHalf, { largestUnit: "days" });
    expect(unrounded.days).toBe(1);
    expect(unrounded.hours).toBe(12);
    expect(unrounded.minutes).toBe(30);

    const rounded = start.until(afterHalf, {
      largestUnit: "days",
      smallestUnit: "days",
      roundingMode: "halfExpand",
    });
    expect(rounded.days).toBe(2);

    const roundedHours = start.until(afterHalf, {
      largestUnit: "days",
      smallestUnit: "hours",
      roundingMode: "halfExpand",
    });
    expect(roundedHours.days).toBe(1);
    expect(roundedHours.hours).toBe(13);
  });
});
