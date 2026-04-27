/*---
description: Temporal.PlainYearMonth.prototype.since
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainYearMonth.prototype.since", () => {
  test("since() default returns years and months", () => {
    const a = new Temporal.PlainYearMonth(2025, 6);
    const b = new Temporal.PlainYearMonth(2024, 3);
    const dur = a.since(b);
    expect(dur.years).toBe(1);
    expect(dur.months).toBe(3);
  });

  test("since() with largestUnit months returns months only", () => {
    const a = new Temporal.PlainYearMonth(2025, 6);
    const b = new Temporal.PlainYearMonth(2024, 3);
    expect(a.since(b, { largestUnit: "months" }).toString()).toBe("P15M");
  });
});
