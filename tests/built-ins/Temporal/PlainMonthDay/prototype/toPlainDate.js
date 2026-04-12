/*---
description: Temporal.PlainMonthDay.prototype.toPlainDate
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainMonthDay.prototype.toPlainDate", () => {
  test("toPlainDate", () => {
    const md = new Temporal.PlainMonthDay(12, 25);
    const d = md.toPlainDate({ year: 2024 });
    expect(d.year).toBe(2024);
    expect(d.month).toBe(12);
    expect(d.day).toBe(25);
  });
});
