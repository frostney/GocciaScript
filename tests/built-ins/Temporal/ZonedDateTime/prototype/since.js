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

  test("since() handles epoch limits with calendar units", () => {
    const max = new Temporal.ZonedDateTime(86400_0000_0000_000_000_000n, "UTC");
    const maxLimit = max.withTimeZone("+02:00");
    const maxInstance = new Temporal.PlainDateTime(1970, 1, 1, 1, 1, 1, 1, 1, 1).toZonedDateTime("+02:00");
    expect(maxLimit.since(maxInstance, { largestUnit: "years" }).toString())
      .toBe("P273790Y8M12DT58M58.998998999S");

    const min = new Temporal.ZonedDateTime(-86400_0000_0000_000_000_000n, "UTC");
    const minLimit = min.withTimeZone("-08:12");
    const minInstance = new Temporal.PlainDateTime(1970, 1, 1, 1, 1, 1, 1, 1, 1).toZonedDateTime("-08:12");
    expect(minLimit.since(minInstance, { largestUnit: "years" }).toString())
      .toBe("-P273790Y8M12DT9H13M1.001001001S");
  });

  test("since() rounds days using spring-forward day length", () => {
    const start = Temporal.ZonedDateTime.from("2024-03-09T00:00:00-05:00[America/New_York]");
    const beforeHalf = Temporal.ZonedDateTime.from("2024-03-10T12:00:00-04:00[America/New_York]");
    const afterHalf = Temporal.ZonedDateTime.from("2024-03-10T12:30:00-04:00[America/New_York]");

    const unrounded = beforeHalf.since(start, { largestUnit: "days" });
    expect(unrounded.days).toBe(1);
    expect(unrounded.hours).toBe(12);

    const roundedBefore = beforeHalf.since(start, {
      largestUnit: "days",
      smallestUnit: "days",
      roundingMode: "halfExpand",
    });
    const roundedAfter = afterHalf.since(start, {
      largestUnit: "days",
      smallestUnit: "days",
      roundingMode: "halfExpand",
    });

    expect(roundedBefore.days).toBe(2);
    expect(roundedAfter.days).toBe(2);

    const roundedHours = afterHalf.since(start, {
      largestUnit: "days",
      smallestUnit: "hours",
      roundingMode: "halfExpand",
    });
    expect(roundedHours.days).toBe(1);
    expect(roundedHours.hours).toBe(13);
  });

  test("since() rounds days using fall-back day length", () => {
    const start = Temporal.ZonedDateTime.from("2024-11-02T00:00:00-04:00[America/New_York]");
    const afterHalf = Temporal.ZonedDateTime.from("2024-11-03T11:30:00-05:00[America/New_York]");

    const unrounded = afterHalf.since(start, { largestUnit: "days" });
    expect(unrounded.days).toBe(1);
    expect(unrounded.hours).toBe(11);
    expect(unrounded.minutes).toBe(30);

    const rounded = afterHalf.since(start, {
      largestUnit: "days",
      smallestUnit: "days",
      roundingMode: "halfExpand",
    });
    expect(rounded.days).toBe(1);

    const roundedHours = afterHalf.since(start, {
      largestUnit: "days",
      smallestUnit: "hours",
      roundingMode: "halfExpand",
    });
    expect(roundedHours.days).toBe(1);
    expect(roundedHours.hours).toBe(12);
  });
});
