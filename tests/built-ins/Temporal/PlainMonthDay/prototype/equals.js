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
});
