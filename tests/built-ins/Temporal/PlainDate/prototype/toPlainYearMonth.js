/*---
description: Temporal.PlainDate.prototype.toPlainYearMonth
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainDate.prototype.toPlainYearMonth", () => {
  test("toPlainYearMonth()", () => {
    const d = new Temporal.PlainDate(2024, 3, 15);
    const ym = d.toPlainYearMonth();
    expect(ym.year).toBe(2024);
    expect(ym.month).toBe(3);
  });
});
