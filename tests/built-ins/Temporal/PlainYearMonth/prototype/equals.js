/*---
description: Temporal.PlainYearMonth.prototype.equals
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainYearMonth.prototype.equals", () => {
  test("equals", () => {
    const a = new Temporal.PlainYearMonth(2024, 3);
    const b = new Temporal.PlainYearMonth(2024, 3);
    const c = new Temporal.PlainYearMonth(2024, 4);
    expect(a.equals(b)).toBe(true);
    expect(a.equals(c)).toBe(false);
  });
});
