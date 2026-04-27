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
});
