/*---
description: Temporal.PlainYearMonth.prototype.until
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainYearMonth.prototype.until", () => {
  test("until", () => {
    const a = new Temporal.PlainYearMonth(2024, 3);
    const b = new Temporal.PlainYearMonth(2025, 6);
    const dur = a.until(b);
    expect(dur.years).toBe(1);
    expect(dur.months).toBe(3);
  });
});
