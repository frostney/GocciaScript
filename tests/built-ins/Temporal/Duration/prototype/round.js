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

  test("round with largestUnit month flattens years", () => {
    const d = Temporal.Duration.from({ years: 1, months: 6 });
    const rounded = d.round({ smallestUnit: "months", largestUnit: "months", relativeTo: "2024-01-01" });
    expect(rounded.years).toBe(0);
    expect(rounded.months).toBe(18);
  });
});
