/*---
description: Temporal.PlainDateTime.prototype.toPlainMonthDay
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainDateTime.prototype.toPlainMonthDay", () => {
  test("toPlainMonthDay()", () => {
    const dt = new Temporal.PlainDateTime(2024, 3, 15, 13, 45, 30);
    const md = dt.toPlainMonthDay();
    expect(md.monthCode).toBe("M03");
    expect(md.day).toBe(15);
  });
});
