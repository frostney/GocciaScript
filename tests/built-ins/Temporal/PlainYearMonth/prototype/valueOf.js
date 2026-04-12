/*---
description: Temporal.PlainYearMonth.prototype.valueOf
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainYearMonth.prototype.valueOf", () => {
  test("valueOf throws", () => {
    const ym = new Temporal.PlainYearMonth(2024, 3);
    expect(() => ym.valueOf()).toThrow(TypeError);
  });
});
