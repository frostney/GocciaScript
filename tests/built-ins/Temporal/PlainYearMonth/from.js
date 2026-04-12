/*---
description: Temporal.PlainYearMonth.from
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainYearMonth.from", () => {
  test("from string", () => {
    const ym = Temporal.PlainYearMonth.from("2024-03");
    expect(ym.year).toBe(2024);
    expect(ym.month).toBe(3);
  });

  test("from PlainYearMonth", () => {
    const original = new Temporal.PlainYearMonth(2024, 6);
    const copy = Temporal.PlainYearMonth.from(original);
    expect(copy.year).toBe(2024);
    expect(copy.month).toBe(6);
  });

  test("from object", () => {
    const ym = Temporal.PlainYearMonth.from({ year: 2024, month: 7 });
    expect(ym.year).toBe(2024);
    expect(ym.month).toBe(7);
  });
});
