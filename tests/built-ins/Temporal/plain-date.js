/*---
description: Temporal.PlainDate construction, properties, and methods
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainDate", () => {

  test("constructor", () => {
    const d = new Temporal.PlainDate(2024, 3, 15);
    expect(d.year).toBe(2024);
    expect(d.month).toBe(3);
    expect(d.day).toBe(15);
  });

  test("calendarId is iso8601", () => {
    const d = new Temporal.PlainDate(2024, 1, 1);
    expect(d.calendarId).toBe("iso8601");
  });

  test("monthCode", () => {
    expect(new Temporal.PlainDate(2024, 1, 1).monthCode).toBe("M01");
    expect(new Temporal.PlainDate(2024, 12, 1).monthCode).toBe("M12");
  });

  test("dayOfWeek (ISO: 1=Monday..7=Sunday)", () => {
    // 2024-01-01 is Monday
    expect(new Temporal.PlainDate(2024, 1, 1).dayOfWeek).toBe(1);
    // 2024-01-07 is Sunday
    expect(new Temporal.PlainDate(2024, 1, 7).dayOfWeek).toBe(7);
  });

  test("dayOfYear", () => {
    expect(new Temporal.PlainDate(2024, 1, 1).dayOfYear).toBe(1);
    expect(new Temporal.PlainDate(2024, 12, 31).dayOfYear).toBe(366);
    expect(new Temporal.PlainDate(2023, 12, 31).dayOfYear).toBe(365);
  });

  test("daysInMonth", () => {
    expect(new Temporal.PlainDate(2024, 2, 1).daysInMonth).toBe(29);
    expect(new Temporal.PlainDate(2023, 2, 1).daysInMonth).toBe(28);
    expect(new Temporal.PlainDate(2024, 1, 1).daysInMonth).toBe(31);
  });

  test("daysInYear", () => {
    expect(new Temporal.PlainDate(2024, 1, 1).daysInYear).toBe(366);
    expect(new Temporal.PlainDate(2023, 1, 1).daysInYear).toBe(365);
  });

  test("monthsInYear", () => {
    expect(new Temporal.PlainDate(2024, 1, 1).monthsInYear).toBe(12);
  });

  test("daysInWeek", () => {
    expect(new Temporal.PlainDate(2024, 1, 1).daysInWeek).toBe(7);
  });

  test("inLeapYear", () => {
    expect(new Temporal.PlainDate(2024, 1, 1).inLeapYear).toBe(true);
    expect(new Temporal.PlainDate(2023, 1, 1).inLeapYear).toBe(false);
    expect(new Temporal.PlainDate(2000, 1, 1).inLeapYear).toBe(true);
    expect(new Temporal.PlainDate(1900, 1, 1).inLeapYear).toBe(false);
  });

  test("toString()", () => {
    expect(new Temporal.PlainDate(2024, 3, 15).toString()).toBe("2024-03-15");
    expect(new Temporal.PlainDate(2024, 1, 1).toString()).toBe("2024-01-01");
  });

  test("toJSON() returns same as toString()", () => {
    const d = new Temporal.PlainDate(2024, 6, 15);
    expect(d.toJSON()).toBe(d.toString());
  });

  test("equals()", () => {
    const d1 = new Temporal.PlainDate(2024, 3, 15);
    const d2 = new Temporal.PlainDate(2024, 3, 15);
    const d3 = new Temporal.PlainDate(2024, 3, 16);
    expect(d1.equals(d2)).toBe(true);
    expect(d1.equals(d3)).toBe(false);
  });

  test("with()", () => {
    const d = new Temporal.PlainDate(2024, 3, 15);
    const updated = d.with({ month: 6 });
    expect(updated.year).toBe(2024);
    expect(updated.month).toBe(6);
    expect(updated.day).toBe(15);
  });

  test("add() with duration", () => {
    const d = new Temporal.PlainDate(2024, 1, 31);
    const result = d.add(new Temporal.Duration(0, 1));
    expect(result.year).toBe(2024);
    expect(result.month).toBe(2);
    expect(result.day).toBe(29);
  });

  test("add() with days", () => {
    const d = new Temporal.PlainDate(2024, 1, 1);
    const result = d.add(new Temporal.Duration(0, 0, 0, 10));
    expect(result.toString()).toBe("2024-01-11");
  });

  test("subtract()", () => {
    const d = new Temporal.PlainDate(2024, 3, 15);
    const result = d.subtract(new Temporal.Duration(0, 0, 0, 15));
    expect(result.toString()).toBe("2024-02-29");
  });

  test("until()", () => {
    const d1 = new Temporal.PlainDate(2024, 1, 1);
    const d2 = new Temporal.PlainDate(2024, 1, 11);
    const dur = d1.until(d2);
    expect(dur.days).toBe(10);
  });

  test("since()", () => {
    const d1 = new Temporal.PlainDate(2024, 1, 11);
    const d2 = new Temporal.PlainDate(2024, 1, 1);
    const dur = d1.since(d2);
    expect(dur.days).toBe(10);
  });

  test("from() with string", () => {
    const d = Temporal.PlainDate.from("2024-03-15");
    expect(d.year).toBe(2024);
    expect(d.month).toBe(3);
    expect(d.day).toBe(15);
  });

  test("from() with object", () => {
    const d = Temporal.PlainDate.from({ year: 2024, month: 6, day: 15 });
    expect(d.year).toBe(2024);
    expect(d.month).toBe(6);
    expect(d.day).toBe(15);
  });

  test("compare()", () => {
    const d1 = new Temporal.PlainDate(2024, 1, 1);
    const d2 = new Temporal.PlainDate(2024, 6, 1);
    expect(Temporal.PlainDate.compare(d1, d2)).toBe(-1);
    expect(Temporal.PlainDate.compare(d2, d1)).toBe(1);
    expect(Temporal.PlainDate.compare(d1, d1)).toBe(0);
  });

  test("toPlainDateTime()", () => {
    const d = new Temporal.PlainDate(2024, 3, 15);
    const dt = d.toPlainDateTime();
    expect(dt.year).toBe(2024);
    expect(dt.month).toBe(3);
    expect(dt.day).toBe(15);
    expect(dt.hour).toBe(0);
    expect(dt.minute).toBe(0);
  });

});
