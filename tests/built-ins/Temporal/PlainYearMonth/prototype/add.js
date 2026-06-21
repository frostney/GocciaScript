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

  test("add walks Coptic month 13 into the next year", () => {
    const ym = Temporal.PlainYearMonth.from({ year: 1742, monthCode: "M13", calendar: "coptic" });
    const result = ym.add({ months: 1 });
    expect(result.year).toBe(1743);
    expect(result.month).toBe(1);
    expect(result.monthCode).toBe("M01");
  });

  test("add rejects units lower than months", () => {
    const ym = new Temporal.PlainYearMonth(2024, 6);
    expect(() => ym.add({ days: 1 })).toThrow(RangeError);
    expect(() => ym.add({ weeks: 1 })).toThrow(RangeError);
  });
});
