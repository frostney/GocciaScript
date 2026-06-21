/*---
description: Temporal.PlainMonthDay.prototype.toString
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainMonthDay.prototype.toString", () => {
  test("toString", () => {
    expect(new Temporal.PlainMonthDay(12, 25).toString()).toBe("12-25");
    expect(new Temporal.PlainMonthDay(1, 1).toString()).toBe("01-01");
  });

  test("toString includes reference year for non-ISO calendar annotation", () => {
    expect(new Temporal.PlainMonthDay(3, 15, "gregory").toString()).toBe("1972-03-15[u-ca=gregory]");
  });
});
