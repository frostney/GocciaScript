/*---
description: Temporal.PlainYearMonth.prototype.until
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainYearMonth.prototype.until", () => {
  test("until() default returns years and months", () => {
    const a = new Temporal.PlainYearMonth(2024, 3);
    const b = new Temporal.PlainYearMonth(2025, 6);
    const dur = a.until(b);
    expect(dur.years).toBe(1);
    expect(dur.months).toBe(3);
  });

  test("until() with largestUnit year", () => {
    const a = new Temporal.PlainYearMonth(2024, 3);
    const b = new Temporal.PlainYearMonth(2025, 6);
    expect(a.until(b, { largestUnit: "years" }).toString()).toBe("P1Y3M");
  });

  test("until() with largestUnit month returns months only", () => {
    const a = new Temporal.PlainYearMonth(2024, 3);
    const b = new Temporal.PlainYearMonth(2025, 6);
    expect(a.until(b, { largestUnit: "months" }).toString()).toBe("P15M");
  });

  test("until() negative result with largestUnit months", () => {
    const a = new Temporal.PlainYearMonth(2025, 6);
    const b = new Temporal.PlainYearMonth(2024, 3);
    expect(a.until(b, { largestUnit: "months" }).toString()).toBe("-P15M");
  });

  test("until() same month returns zero", () => {
    const a = new Temporal.PlainYearMonth(2024, 6);
    const dur = a.until(a, { largestUnit: "months" });
    expect(dur.years).toBe(0);
    expect(dur.months).toBe(0);
  });

  test("until() with smallestUnit year truncates months", () => {
    const a = new Temporal.PlainYearMonth(2020, 1);
    const b = new Temporal.PlainYearMonth(2023, 8);
    expect(a.until(b, { largestUnit: "years", smallestUnit: "year" }).toString()).toBe("P3Y");
  });

  test("until() with smallestUnit year and roundingMode halfExpand rounds up", () => {
    const a = new Temporal.PlainYearMonth(2020, 1);
    const b = new Temporal.PlainYearMonth(2023, 8);
    expect(a.until(b, { largestUnit: "years", smallestUnit: "year", roundingMode: "halfExpand" }).toString()).toBe("P4Y");
  });

  test("until() with roundingIncrement 3 and smallestUnit month", () => {
    const a = new Temporal.PlainYearMonth(2020, 1);
    const b = new Temporal.PlainYearMonth(2020, 8);
    expect(a.until(b, { largestUnit: "months", smallestUnit: "month", roundingIncrement: 3 }).toString()).toBe("P6M");
  });
});
