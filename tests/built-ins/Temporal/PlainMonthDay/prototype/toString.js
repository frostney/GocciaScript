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
});
