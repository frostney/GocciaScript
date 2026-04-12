/*---
description: Temporal.PlainMonthDay.prototype.equals
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainMonthDay.prototype.equals", () => {
  test("equals", () => {
    const a = new Temporal.PlainMonthDay(12, 25);
    const b = new Temporal.PlainMonthDay(12, 25);
    const c = new Temporal.PlainMonthDay(12, 26);
    expect(a.equals(b)).toBe(true);
    expect(a.equals(c)).toBe(false);
  });

  test("equals accepts object with numeric month", () => {
    const md = new Temporal.PlainMonthDay(7, 16);
    expect(md.equals({ month: 7, day: 16 })).toBe(true);
  });

  test("equals throws on conflicting month and monthCode", () => {
    const md = new Temporal.PlainMonthDay(7, 16);
    expect(() => md.equals({ monthCode: "M07", month: 8, day: 16 })).toThrow();
  });
});
