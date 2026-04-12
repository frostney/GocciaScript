/*---
description: Temporal.PlainMonthDay.prototype.toJSON
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainMonthDay.prototype.toJSON", () => {
  test("toJSON", () => {
    expect(new Temporal.PlainMonthDay(12, 25).toJSON()).toBe("12-25");
  });
});
