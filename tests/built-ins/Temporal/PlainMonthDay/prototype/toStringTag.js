/*---
description: Temporal.PlainMonthDay.prototype[Symbol.toStringTag]
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainMonthDay.prototype[Symbol.toStringTag]", () => {
  test("Symbol.toStringTag", () => {
    const md = new Temporal.PlainMonthDay(1, 1);
    expect(Object.prototype.toString.call(md)).toBe("[object Temporal.PlainMonthDay]");
  });
});
