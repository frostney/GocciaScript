/*---
description: Temporal.PlainYearMonth.compare
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainYearMonth.compare", () => {
  test("compare", () => {
    const a = new Temporal.PlainYearMonth(2024, 3);
    const b = new Temporal.PlainYearMonth(2024, 6);
    expect(Temporal.PlainYearMonth.compare(a, b)).toBe(-1);
    expect(Temporal.PlainYearMonth.compare(b, a)).toBe(1);
    expect(Temporal.PlainYearMonth.compare(a, a)).toBe(0);
  });
});
