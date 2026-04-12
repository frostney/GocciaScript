/*---
description: Temporal.PlainDate.prototype.toPlainMonthDay
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainDate.prototype.toPlainMonthDay", () => {
  test("toPlainMonthDay()", () => {
    const d = new Temporal.PlainDate(2024, 3, 15);
    const md = d.toPlainMonthDay();
    expect(md.monthCode).toBe("M03");
    expect(md.day).toBe(15);
  });
});
