/*---
description: Temporal.PlainMonthDay.prototype.valueOf
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainMonthDay.prototype.valueOf", () => {
  test("valueOf throws", () => {
    const md = new Temporal.PlainMonthDay(12, 25);
    expect(() => md.valueOf()).toThrow(TypeError);
  });
});
