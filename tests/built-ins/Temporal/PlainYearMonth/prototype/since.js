/*---
description: Temporal.PlainYearMonth.prototype.since
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainYearMonth.prototype.since", () => {
  test("since", () => {
    const a = new Temporal.PlainYearMonth(2025, 6);
    const b = new Temporal.PlainYearMonth(2024, 3);
    const dur = a.since(b);
    expect(dur.years).toBe(1);
    expect(dur.months).toBe(3);
  });
});
