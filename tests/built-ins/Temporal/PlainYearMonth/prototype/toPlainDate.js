/*---
description: Temporal.PlainYearMonth.prototype.toPlainDate
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainYearMonth.prototype.toPlainDate", () => {
  test("toPlainDate", () => {
    const ym = new Temporal.PlainYearMonth(2024, 3);
    const d = ym.toPlainDate({ day: 15 });
    expect(d.year).toBe(2024);
    expect(d.month).toBe(3);
    expect(d.day).toBe(15);
  });
});
