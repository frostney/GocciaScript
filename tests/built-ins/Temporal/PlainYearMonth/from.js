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

  test("from object with month", () => {
    const ym = Temporal.PlainYearMonth.from({ year: 2024, month: 7 });
    expect(ym.year).toBe(2024);
    expect(ym.month).toBe(7);
  });

  test("from object with monthCode", () => {
    const ym = Temporal.PlainYearMonth.from({ year: 2024, monthCode: "M05" });
    expect(ym.year).toBe(2024);
    expect(ym.monthCode).toBe("M05");
    expect(ym.month).toBe(5);
  });

  test("from object with consistent month and monthCode", () => {
    const ym = Temporal.PlainYearMonth.from({ year: 2024, monthCode: "M03", month: 3 });
    expect(ym.month).toBe(3);
  });

  test("from throws on conflicting month and monthCode", () => {
    expect(() => Temporal.PlainYearMonth.from({ year: 2024, monthCode: "M05", month: 6 })).toThrow();
  });

  test("from throws on missing month and monthCode", () => {
    expect(() => Temporal.PlainYearMonth.from({ year: 2024 })).toThrow();
  });

  test("from rejects non-canonical monthCode", () => {
    expect(() => Temporal.PlainYearMonth.from({ year: 2024, monthCode: "M1" })).toThrow();
    expect(() => Temporal.PlainYearMonth.from({ year: 2024, monthCode: "M001" })).toThrow();
  });

  test("from rejects out-of-range monthCode", () => {
    expect(() => Temporal.PlainYearMonth.from({ year: 2024, monthCode: "M13" })).toThrow();
    expect(() => Temporal.PlainYearMonth.from({ year: 2024, monthCode: "M00" })).toThrow();
  });
});
