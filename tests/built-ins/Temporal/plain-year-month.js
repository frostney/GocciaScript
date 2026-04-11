/*---
description: Temporal.PlainYearMonth construction, properties, and methods
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainYearMonth", () => {

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

  test("compare", () => {
    const a = new Temporal.PlainYearMonth(2024, 3);
    const b = new Temporal.PlainYearMonth(2024, 6);
    expect(Temporal.PlainYearMonth.compare(a, b)).toBe(-1);
    expect(Temporal.PlainYearMonth.compare(b, a)).toBe(1);
    expect(Temporal.PlainYearMonth.compare(a, a)).toBe(0);
  });

  test("with", () => {
    const ym = new Temporal.PlainYearMonth(2024, 3);
    const updated = ym.with({ month: 6 });
    expect(updated.year).toBe(2024);
    expect(updated.month).toBe(6);
  });

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

  test("subtract", () => {
    const ym = new Temporal.PlainYearMonth(2024, 3);
    const sub = ym.subtract({ months: 4 });
    expect(sub.year).toBe(2023);
    expect(sub.month).toBe(11);
  });

  test("until", () => {
    const a = new Temporal.PlainYearMonth(2024, 3);
    const b = new Temporal.PlainYearMonth(2025, 6);
    const dur = a.until(b);
    expect(dur.years).toBe(1);
    expect(dur.months).toBe(3);
  });

  test("since", () => {
    const a = new Temporal.PlainYearMonth(2025, 6);
    const b = new Temporal.PlainYearMonth(2024, 3);
    const dur = a.since(b);
    expect(dur.years).toBe(1);
    expect(dur.months).toBe(3);
  });

  test("equals", () => {
    const a = new Temporal.PlainYearMonth(2024, 3);
    const b = new Temporal.PlainYearMonth(2024, 3);
    const c = new Temporal.PlainYearMonth(2024, 4);
    expect(a.equals(b)).toBe(true);
    expect(a.equals(c)).toBe(false);
  });

  test("toString", () => {
    expect(new Temporal.PlainYearMonth(2024, 3).toString()).toBe("2024-03");
    expect(new Temporal.PlainYearMonth(2024, 12).toString()).toBe("2024-12");
  });

  test("toJSON", () => {
    expect(new Temporal.PlainYearMonth(2024, 3).toJSON()).toBe("2024-03");
  });

  test("valueOf throws", () => {
    const ym = new Temporal.PlainYearMonth(2024, 3);
    expect(() => ym.valueOf()).toThrow(TypeError);
  });

  test("toPlainDate", () => {
    const ym = new Temporal.PlainYearMonth(2024, 3);
    const d = ym.toPlainDate({ day: 15 });
    expect(d.year).toBe(2024);
    expect(d.month).toBe(3);
    expect(d.day).toBe(15);
  });

  test("invalid month throws", () => {
    expect(() => new Temporal.PlainYearMonth(2024, 0)).toThrow(RangeError);
    expect(() => new Temporal.PlainYearMonth(2024, 13)).toThrow(RangeError);
  });

  test("Symbol.toStringTag", () => {
    const ym = new Temporal.PlainYearMonth(2024, 1);
    expect(Object.prototype.toString.call(ym)).toBe("[object Temporal.PlainYearMonth]");
  });
});
