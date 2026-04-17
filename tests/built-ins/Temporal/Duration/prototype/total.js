/*---
description: Temporal.Duration.prototype.total
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.Duration.prototype.total", () => {
  test("total() with time-only durations", () => {
    const d = new Temporal.Duration(0, 0, 0, 0, 1, 30);
    expect(d.total("minutes")).toBe(90);
    expect(d.total("hours")).toBe(1.5);
    expect(d.total("seconds")).toBe(5400);
  });

  test("total() throws RangeError without relativeTo for year/month durations", () => {
    const withYears = new Temporal.Duration(1);
    expect(() => withYears.total("days")).toThrow(RangeError);

    const withMonths = new Temporal.Duration(0, 3);
    expect(() => withMonths.total("days")).toThrow(RangeError);

    const withBoth = new Temporal.Duration(1, 2, 0, 5);
    expect(() => withBoth.total("hours")).toThrow(RangeError);
  });

  test("total() with relativeTo converts years and months to days", () => {
    const d = Temporal.Duration.from({ years: 1, months: 6 });
    expect(d.total({ unit: "days", relativeTo: "2024-01-01" })).toBe(547);
  });

  test("total() with relativeTo converts to months", () => {
    const d = Temporal.Duration.from({ years: 1, months: 6 });
    expect(d.total({ unit: "months", relativeTo: "2024-01-01" })).toBe(18);
  });

  test("total() with relativeTo converts to years", () => {
    const d = Temporal.Duration.from({ years: 2 });
    expect(d.total({ unit: "years", relativeTo: "2024-01-01" })).toBe(2);
  });

  test("total() accounts for actual month lengths", () => {
    // Feb 2024 is a leap year (29 days)
    const oneMonth = Temporal.Duration.from({ months: 1 });
    expect(oneMonth.total({ unit: "days", relativeTo: "2024-02-01" })).toBe(29);
    // Feb 2023 is not a leap year (28 days)
    expect(oneMonth.total({ unit: "days", relativeTo: "2023-02-01" })).toBe(28);
    // January always has 31 days
    expect(oneMonth.total({ unit: "days", relativeTo: "2024-01-01" })).toBe(31);
  });

  test("total() accounts for actual year lengths", () => {
    const oneYear = Temporal.Duration.from({ years: 1 });
    expect(oneYear.total({ unit: "days", relativeTo: "2024-01-01" })).toBe(366);
    expect(oneYear.total({ unit: "days", relativeTo: "2023-01-01" })).toBe(365);
  });

  test("total() with relativeTo and sub-day units", () => {
    const d = Temporal.Duration.from({ years: 1, months: 6 });
    const totalHours = d.total({ unit: "hours", relativeTo: "2024-01-01" });
    expect(totalHours).toBe(547 * 24);

    const totalMinutes = d.total({ unit: "minutes", relativeTo: "2024-01-01" });
    expect(totalMinutes).toBe(547 * 24 * 60);
  });

  test("total() with mixed calendar and day components", () => {
    const d = Temporal.Duration.from({ months: 1, days: 15 });
    // Jan has 31 days, so 1 month + 15 days = 46 days from Jan 1
    expect(d.total({ unit: "days", relativeTo: "2024-01-01" })).toBe(46);
  });

  test("total() months with fractional result", () => {
    const d = Temporal.Duration.from({ months: 1, days: 15 });
    const months = d.total({ unit: "months", relativeTo: "2024-01-01" });
    // 1 month from Jan 1 = Feb 1; remaining 15 days; Feb has 29 days in 2024
    expect(months).toBeCloseTo(1 + 15 / 29, 10);
  });

  test("total() years with fractional result", () => {
    const d = Temporal.Duration.from({ years: 1, months: 6 });
    const years = d.total({ unit: "years", relativeTo: "2024-01-01" });
    // 1 year from Jan 1 2024 = Jan 1 2025; remaining 6 months = 181 days;
    // year span from Jan 1 2025 to Jan 1 2026 = 365 days
    expect(years).toBeCloseTo(1 + 181 / 365, 10);
  });

  test("total() with relativeTo as PlainDate object", () => {
    const d = Temporal.Duration.from({ months: 6 });
    const relTo = Temporal.PlainDate.from("2024-01-01");
    const days = d.total({ unit: "days", relativeTo: relTo });
    // Jan(31) + Feb(29) + Mar(31) + Apr(30) + May(31) + Jun(30) = 182
    expect(days).toBe(182);
  });

  test("total() months/years requires relativeTo", () => {
    const d = Temporal.Duration.from({ days: 30 });
    expect(() => d.total("months")).toThrow(RangeError);
    expect(() => d.total("years")).toThrow(RangeError);
  });

  test("total() months with time-only overflow into days", () => {
    // PT8760H = 365 days worth of hours; from 2023-01-01 that lands on 2024-01-01 = 12 months
    const d = new Temporal.Duration(0, 0, 0, 0, 8760);
    expect(d.total({ unit: "months", relativeTo: "2023-01-01" })).toBe(12);
  });

  test("total() years with time-only overflow into days", () => {
    // PT8760H = 365 days; from 2023-01-01 that is exactly 1 year
    const d = new Temporal.Duration(0, 0, 0, 0, 8760);
    expect(d.total({ unit: "years", relativeTo: "2023-01-01" })).toBe(1);
  });

  test("total() accepts singular unit names", () => {
    const d = new Temporal.Duration(0, 0, 0, 0, 1, 30);
    expect(d.total("minute")).toBe(90);
    expect(d.total("hour")).toBe(1.5);
    expect(d.total("second")).toBe(5400);
    expect(d.total("nanosecond")).toBe(5400000000000);
    expect(d.total("microsecond")).toBe(5400000000);
    expect(d.total("millisecond")).toBe(5400000);
  });

  test("total() with day-clamping edge case", () => {
    // Adding 1 month from Jan 31 → Feb 29 (2024 leap year, clamped)
    const d = Temporal.Duration.from({ months: 1 });
    expect(d.total({ unit: "days", relativeTo: "2024-01-31" })).toBe(29);
  });

  test("total() leap-day clamping with separate year and month steps", () => {
    // P1Y1M from 2020-02-29: +1Y → 2021-02-28 (clamped), +1M → 2021-03-28
    const d = Temporal.Duration.from({ years: 1, months: 1 });
    const days = d.total({ unit: "days", relativeTo: "2020-02-29" });
    // 2020-02-29 to 2021-03-28 = 393 days
    expect(days).toBe(393);
  });
});
