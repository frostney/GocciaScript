/*---
description: Temporal.PlainYearMonth.prototype.add
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainYearMonth.prototype.add", () => {
  test("add", () => {
    const ym = new Temporal.PlainYearMonth(2024, 3);
    const added = ym.add({ months: 10 });
    expect(added.year).toBe(2025);
    expect(added.month).toBe(1);
  });

  test("add years", () => {
    const ym = new Temporal.PlainYearMonth(2024, 6);
    const added = ym.add({ years: 2 });
    expect(added.year).toBe(2026);
    expect(added.month).toBe(6);
  });
});
