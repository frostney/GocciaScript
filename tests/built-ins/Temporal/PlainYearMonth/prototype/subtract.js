/*---
description: Temporal.PlainYearMonth.prototype.subtract
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainYearMonth.prototype.subtract", () => {
  test("subtract", () => {
    const ym = new Temporal.PlainYearMonth(2024, 3);
    const sub = ym.subtract({ months: 4 });
    expect(sub.year).toBe(2023);
    expect(sub.month).toBe(11);
  });
});
