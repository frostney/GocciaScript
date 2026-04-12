/*---
description: Temporal.PlainDateTime.prototype.toPlainYearMonth
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainDateTime.prototype.toPlainYearMonth", () => {
  test("toPlainYearMonth()", () => {
    const dt = new Temporal.PlainDateTime(2024, 3, 15, 13, 45, 30);
    const ym = dt.toPlainYearMonth();
    expect(ym.year).toBe(2024);
    expect(ym.month).toBe(3);
  });
});
