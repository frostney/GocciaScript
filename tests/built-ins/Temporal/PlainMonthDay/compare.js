/*---
description: Temporal.PlainMonthDay.compare
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainMonthDay.compare", () => {
  test("compare equal month-days", () => {
    const a = new Temporal.PlainMonthDay(6, 15);
    const b = new Temporal.PlainMonthDay(6, 15);
    expect(Temporal.PlainMonthDay.compare(a, b)).toBe(0);
  });

  test("compare different months", () => {
    const a = new Temporal.PlainMonthDay(3, 15);
    const b = new Temporal.PlainMonthDay(6, 15);
    expect(Temporal.PlainMonthDay.compare(a, b)).toBe(-1);
    expect(Temporal.PlainMonthDay.compare(b, a)).toBe(1);
  });

  test("compare different days same month", () => {
    const a = new Temporal.PlainMonthDay(6, 10);
    const b = new Temporal.PlainMonthDay(6, 20);
    expect(Temporal.PlainMonthDay.compare(a, b)).toBe(-1);
  });
});
