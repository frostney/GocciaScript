/*---
description: Temporal.Duration.prototype.round
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.Duration.prototype.round", () => {
  test("round time units", () => {
    const d = new Temporal.Duration(0, 0, 0, 0, 25, 30);
    const rounded = d.round({ largestUnit: "day" });
    expect(rounded.days).toBe(1);
    expect(rounded.hours).toBe(1);
    expect(rounded.minutes).toBe(30);
  });

  test("round to hours", () => {
    const d = new Temporal.Duration(0, 0, 0, 0, 1, 45);
    const rounded = d.round({ smallestUnit: "hour", roundingMode: "halfExpand" });
    expect(rounded.hours).toBe(2);
  });

  test("round BigInt-backed exact-time duration", () => {
    const i1 = Temporal.Instant.fromEpochMilliseconds(-8640000000000000);
    const i2 = Temporal.Instant.fromEpochMilliseconds(8640000000000000);
    const d = i1.until(i2, { largestUnit: "microseconds" });
    const rounded = d.round({ smallestUnit: "seconds" });
    expect(rounded.toString()).toBe("PT17280000000000S");
  });

  test("round floor", () => {
    const d = new Temporal.Duration(0, 0, 0, 0, 1, 59);
    const rounded = d.round({ smallestUnit: "hour", roundingMode: "floor" });
    expect(rounded.hours).toBe(1);
  });

  test("round with smallestUnit week", () => {
    const d = Temporal.Duration.from({ days: 10 });
    const rounded = d.round({ smallestUnit: "week", roundingMode: "trunc" });
    expect(rounded.weeks).toBe(1);
    expect(rounded.days).toBe(0);
  });

  test("round years+months to months with relativeTo", () => {
    const d = Temporal.Duration.from({ years: 1, months: 6 });
    const rounded = d.round({ smallestUnit: "months", relativeTo: "2024-01-01" });
    // Default largestUnit is "year" (largest existing unit)
    expect(rounded.years).toBe(1);
    expect(rounded.months).toBe(6);
    expect(rounded.days).toBe(0);
  });

  test("round years+months+days to months with relativeTo", () => {
    const d = Temporal.Duration.from({ years: 1, months: 6, days: 20 });
    // 2024-01-01 + 1Y = 2025-01-01, + 6M = 2025-07-01, + 20D = 2025-07-21
    // 18 complete months from 2024-01-01 to 2025-07-01; remaining 20/31 ≈ 0.645 → rounds to 19 months
    const rounded = d.round({ smallestUnit: "months", relativeTo: "2024-01-01" });
    expect(rounded.years).toBe(1);
    expect(rounded.months).toBe(7);
    expect(rounded.days).toBe(0);
  });

  test("round to years with relativeTo", () => {
    const d = Temporal.Duration.from({ years: 1, months: 6 });
    // 2024-01-01 + 1Y6M = 2025-07-01
    // 1 complete year (2024-01-01 to 2025-01-01 = 366 days, 2024 is leap)
    // remaining: 2025-01-01 to 2025-07-01 = 181 days
    // year period: 2025-01-01 to 2026-01-01 = 365 days
    // fraction: 181/365 ≈ 0.496 < 0.5 → rounds to 1
    const rounded = d.round({ smallestUnit: "years", relativeTo: "2024-01-01" });
    expect(rounded.years).toBe(1);
    expect(rounded.months).toBe(0);
  });

  test("round to years rounds up at half boundary", () => {
    const d = Temporal.Duration.from({ years: 1, months: 7 });
    // 2023-07-01 + 1Y7M = 2025-02-01
    // 1 complete year (2023-07-01 to 2024-07-01 = 366 days)
    // remaining: 2024-07-01 to 2025-02-01 = 215 days
    // year period: 2024-07-01 to 2025-07-01 = 365 days
    // fraction: 215/365 ≈ 0.589 >= 0.5 → rounds to 2
    const rounded = d.round({ smallestUnit: "years", relativeTo: "2023-07-01" });
    expect(rounded.years).toBe(2);
    expect(rounded.months).toBe(0);
  });

  test("round months+hours to days with relativeTo", () => {
    const d = Temporal.Duration.from({ months: 1, hours: 36 });
    // 2024-01-15 + 1M = 2024-02-15 (31 days), then 36h = 1.5 days
    // total = 32.5 days → rounds to 33 days (halfExpand)
    // rebalance: 1 month (31 days) + 2 days
    const rounded = d.round({ smallestUnit: "days", relativeTo: "2024-01-15" });
    expect(rounded.months).toBe(1);
    expect(rounded.days).toBe(2);
    expect(rounded.hours).toBe(0);
  });

  test("round with relativeTo string datetime", () => {
    const d = Temporal.Duration.from({ years: 2, months: 3 });
    const rounded = d.round({ smallestUnit: "months", relativeTo: "2020-06-15T10:30:00" });
    expect(rounded.years).toBe(2);
    expect(rounded.months).toBe(3);
  });

  test("round with relativeTo PlainDate object", () => {
    const d = Temporal.Duration.from({ years: 1, months: 6 });
    const rd = Temporal.PlainDate.from("2024-01-01");
    const rounded = d.round({ smallestUnit: "months", relativeTo: rd });
    expect(rounded.years).toBe(1);
    expect(rounded.months).toBe(6);
  });

  test("round years+months without relativeTo throws", () => {
    const d = Temporal.Duration.from({ years: 1, months: 6 });
    expect(() => d.round({ smallestUnit: "months" })).toThrow();
  });

  test("round with largestUnit smaller than smallestUnit throws", () => {
    const d = Temporal.Duration.from({ days: 10 });
    expect(() => d.round({ smallestUnit: "month", largestUnit: "day", relativeTo: "2024-01-01" })).toThrow();
  });

  test("round applies years and months as separate calendar steps", () => {
    // 2020-02-29 + 1Y = 2021-02-28 (day clamps), + 1M = 2021-03-28 = 393 days.
    // Collapsing to +13M gives 2021-03-29 = 394 days (wrong).
    const d = Temporal.Duration.from({ years: 1, months: 1 });
    const inDays = d.round({ smallestUnit: "days", largestUnit: "days", relativeTo: "2020-02-29" });
    expect(inDays.days).toBe(393);
  });

  test("round months with roundingIncrement uses real calendar bucket span", () => {
    // P3M from 2021-01-31: end = 2021-04-30 (clamped). Bucket [2M, 4M]:
    // 2M = 2021-03-31, 4M = 2021-05-31 → span = 61 days.
    // Position = 2021-04-30 - 2021-03-31 = 30 days. 30/61 < 0.5 → round to 2M.
    const d = Temporal.Duration.from({ months: 3 });
    const rounded = d.round({
      smallestUnit: "months", roundingIncrement: 2, relativeTo: "2021-01-31"
    });
    expect(rounded.months).toBe(2);
  });

  test("round negative months with roundingIncrement uses correct bucket", () => {
    // -P3M from 2021-07-31: end = 2021-04-30. Bucket [-4M, -2M]:
    // -4M = 2021-03-31, -2M = 2021-05-31 → span = 61 days.
    // Position from -4M boundary = 30 days. 30/61 < 0.5 → round to -4M.
    const d = Temporal.Duration.from({ months: -3 });
    const rounded = d.round({
      smallestUnit: "months", roundingIncrement: 2, relativeTo: "2021-07-31"
    });
    expect(rounded.months).toBe(-4);
  });

  test("round large time-only duration to months folds carry days", () => {
    // PT2000H = ~83.33 days from 2021-01-01:
    // Jan 31 + Feb 28 = 59 days → 2 months, remainder 24.33/31 ≈ 0.785 → rounds to 3 months
    const d = Temporal.Duration.from({ hours: 2000 });
    const rounded = d.round({ smallestUnit: "months", relativeTo: "2021-01-01" });
    expect(rounded.months).toBe(3);
  });

  test("round with largestUnit month flattens years", () => {
    const d = Temporal.Duration.from({ years: 1, months: 6 });
    const rounded = d.round({ smallestUnit: "months", largestUnit: "months", relativeTo: "2024-01-01" });
    expect(rounded.years).toBe(0);
    expect(rounded.months).toBe(18);
  });

  test("round rejects relativeTo as ZonedDateTime", () => {
    const d = Temporal.Duration.from({ years: 1, months: 6 });
    const zdt = Temporal.ZonedDateTime.from("2024-01-01T00:00:00+00:00[UTC]");
    expect(() => d.round({ smallestUnit: "months", relativeTo: zdt })).toThrow(RangeError);
  });

  test("round rejects relativeTo as ZonedDateTime to days", () => {
    const d = Temporal.Duration.from({ years: 1, months: 1 });
    const zdt = Temporal.ZonedDateTime.from("2020-02-29T10:30:00+00:00[UTC]");
    expect(() => d.round({ smallestUnit: "days", largestUnit: "days", relativeTo: zdt })).toThrow(RangeError);
  });

  test("round with relativeTo as property bag", () => {
    const d = Temporal.Duration.from({ years: 1, months: 6 });
    const rounded = d.round({ smallestUnit: "months", relativeTo: { year: 2024, month: 1, day: 1 } });
    expect(rounded.years).toBe(1);
    expect(rounded.months).toBe(6);
  });

  test("round with relativeTo as property bag to days", () => {
    const d = Temporal.Duration.from({ years: 1, months: 1 });
    const rounded = d.round({ smallestUnit: "days", largestUnit: "days", relativeTo: { year: 2020, month: 2, day: 29 } });
    expect(rounded.days).toBe(393);
  });

  test("round with relativeTo property bag missing fields throws", () => {
    const d = Temporal.Duration.from({ years: 1, months: 6 });
    expect(() => d.round({ smallestUnit: "months", relativeTo: { year: 2024, month: 1 } })).toThrow();
    expect(() => d.round({ smallestUnit: "months", relativeTo: {} })).toThrow();
  });

  test("round with relativeTo property bag with invalid date throws", () => {
    const d = Temporal.Duration.from({ years: 1, months: 6 });
    expect(() => d.round({ smallestUnit: "months", relativeTo: { year: 2024, month: 13, day: 1 } })).toThrow();
    expect(() => d.round({ smallestUnit: "months", relativeTo: { year: 2024, month: 2, day: 30 } })).toThrow();
  });
});
