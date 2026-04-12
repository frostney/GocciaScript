/*---
description: Temporal.PlainYearMonth.prototype.with
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainYearMonth.prototype.with", () => {
  test("with", () => {
    const ym = new Temporal.PlainYearMonth(2024, 3);
    const updated = ym.with({ month: 6 });
    expect(updated.year).toBe(2024);
    expect(updated.month).toBe(6);
  });
});
