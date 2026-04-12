/*---
description: Temporal.PlainYearMonth constructor
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainYearMonth constructor", () => {
  test("constructor", () => {
    const ym = new Temporal.PlainYearMonth(2024, 3);
    expect(ym.year).toBe(2024);
    expect(ym.month).toBe(3);
  });

  test("calendarId is iso8601", () => {
    const ym = new Temporal.PlainYearMonth(2024, 1);
    expect(ym.calendarId).toBe("iso8601");
  });

  test("monthCode", () => {
    expect(new Temporal.PlainYearMonth(2024, 1).monthCode).toBe("M01");
    expect(new Temporal.PlainYearMonth(2024, 12).monthCode).toBe("M12");
  });

  test("daysInMonth", () => {
    expect(new Temporal.PlainYearMonth(2024, 2).daysInMonth).toBe(29);
    expect(new Temporal.PlainYearMonth(2023, 2).daysInMonth).toBe(28);
    expect(new Temporal.PlainYearMonth(2024, 1).daysInMonth).toBe(31);
  });

  test("daysInYear", () => {
    expect(new Temporal.PlainYearMonth(2024, 1).daysInYear).toBe(366);
    expect(new Temporal.PlainYearMonth(2023, 1).daysInYear).toBe(365);
  });

  test("monthsInYear", () => {
    expect(new Temporal.PlainYearMonth(2024, 1).monthsInYear).toBe(12);
  });

  test("inLeapYear", () => {
    expect(new Temporal.PlainYearMonth(2024, 1).inLeapYear).toBe(true);
    expect(new Temporal.PlainYearMonth(2023, 1).inLeapYear).toBe(false);
  });

  test("invalid month throws", () => {
    expect(() => new Temporal.PlainYearMonth(2024, 0)).toThrow(RangeError);
    expect(() => new Temporal.PlainYearMonth(2024, 13)).toThrow(RangeError);
  });
});
