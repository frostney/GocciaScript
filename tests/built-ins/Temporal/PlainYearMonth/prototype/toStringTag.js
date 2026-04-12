/*---
description: Temporal.PlainYearMonth.prototype[Symbol.toStringTag]
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainYearMonth.prototype[Symbol.toStringTag]", () => {
  test("Symbol.toStringTag", () => {
    const ym = new Temporal.PlainYearMonth(2024, 1);
    expect(Object.prototype.toString.call(ym)).toBe("[object Temporal.PlainYearMonth]");
  });
});
