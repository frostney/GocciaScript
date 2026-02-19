/*---
description: Temporal.PlainDateTime construction, properties, and methods
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainDateTime", () => {

  test("constructor", () => {
    const dt = new Temporal.PlainDateTime(2024, 3, 15, 10, 30, 45);
    expect(dt.year).toBe(2024);
    expect(dt.month).toBe(3);
    expect(dt.day).toBe(15);
    expect(dt.hour).toBe(10);
    expect(dt.minute).toBe(30);
    expect(dt.second).toBe(45);
  });

  test("constructor with defaults", () => {
    const dt = new Temporal.PlainDateTime(2024, 1, 1);
    expect(dt.hour).toBe(0);
    expect(dt.minute).toBe(0);
    expect(dt.second).toBe(0);
    expect(dt.millisecond).toBe(0);
  });

  test("calendarId", () => {
    const dt = new Temporal.PlainDateTime(2024, 1, 1);
    expect(dt.calendarId).toBe("iso8601");
  });

  test("date getters", () => {
    const dt = new Temporal.PlainDateTime(2024, 3, 15, 10, 30);
    expect(dt.dayOfWeek).toBe(5);
    expect(dt.daysInMonth).toBe(31);
    expect(dt.daysInYear).toBe(366);
    expect(dt.inLeapYear).toBe(true);
  });

  test("toString()", () => {
    expect(new Temporal.PlainDateTime(2024, 3, 15, 10, 30, 0).toString()).toBe("2024-03-15T10:30:00");
    expect(new Temporal.PlainDateTime(2024, 1, 1, 0, 0, 0).toString()).toBe("2024-01-01T00:00:00");
  });

  test("toJSON()", () => {
    const dt = new Temporal.PlainDateTime(2024, 3, 15, 10, 30);
    expect(dt.toJSON()).toBe(dt.toString());
  });

  test("equals()", () => {
    const dt1 = new Temporal.PlainDateTime(2024, 3, 15, 10, 30);
    const dt2 = new Temporal.PlainDateTime(2024, 3, 15, 10, 30);
    const dt3 = new Temporal.PlainDateTime(2024, 3, 15, 10, 31);
    expect(dt1.equals(dt2)).toBe(true);
    expect(dt1.equals(dt3)).toBe(false);
  });

  test("with()", () => {
    const dt = new Temporal.PlainDateTime(2024, 3, 15, 10, 30);
    const updated = dt.with({ hour: 14, month: 6 });
    expect(updated.year).toBe(2024);
    expect(updated.month).toBe(6);
    expect(updated.day).toBe(15);
    expect(updated.hour).toBe(14);
    expect(updated.minute).toBe(30);
  });

  test("withPlainTime()", () => {
    const dt = new Temporal.PlainDateTime(2024, 3, 15, 10, 30);
    const result = dt.withPlainTime();
    expect(result.hour).toBe(0);
    expect(result.minute).toBe(0);
    expect(result.day).toBe(15);
  });

  test("add()", () => {
    const dt = new Temporal.PlainDateTime(2024, 1, 31, 23, 0);
    const result = dt.add(new Temporal.Duration(0, 1, 0, 0, 2));
    expect(result.month).toBe(3);
    expect(result.day).toBe(1);
    expect(result.hour).toBe(1);
  });

  test("subtract()", () => {
    const dt = new Temporal.PlainDateTime(2024, 3, 1, 0, 0);
    const result = dt.subtract(new Temporal.Duration(0, 0, 0, 1));
    expect(result.month).toBe(2);
    expect(result.day).toBe(29);
  });

  test("until()", () => {
    const dt1 = new Temporal.PlainDateTime(2024, 1, 1, 0, 0);
    const dt2 = new Temporal.PlainDateTime(2024, 1, 2, 12, 0);
    const dur = dt1.until(dt2);
    expect(dur.days).toBe(1);
    expect(dur.hours).toBe(12);
  });

  test("since()", () => {
    const dt1 = new Temporal.PlainDateTime(2024, 1, 2, 12, 0);
    const dt2 = new Temporal.PlainDateTime(2024, 1, 1, 0, 0);
    const dur = dt1.since(dt2);
    expect(dur.days).toBe(1);
    expect(dur.hours).toBe(12);
  });

  test("toPlainDate()", () => {
    const dt = new Temporal.PlainDateTime(2024, 3, 15, 10, 30);
    const d = dt.toPlainDate();
    expect(d.year).toBe(2024);
    expect(d.month).toBe(3);
    expect(d.day).toBe(15);
  });

  test("toPlainTime()", () => {
    const dt = new Temporal.PlainDateTime(2024, 3, 15, 10, 30, 45);
    const t = dt.toPlainTime();
    expect(t.hour).toBe(10);
    expect(t.minute).toBe(30);
    expect(t.second).toBe(45);
  });

  test("from() with string", () => {
    const dt = Temporal.PlainDateTime.from("2024-03-15T10:30:00");
    expect(dt.year).toBe(2024);
    expect(dt.month).toBe(3);
    expect(dt.day).toBe(15);
    expect(dt.hour).toBe(10);
    expect(dt.minute).toBe(30);
  });

  test("from() with date-only string", () => {
    const dt = Temporal.PlainDateTime.from("2024-03-15");
    expect(dt.year).toBe(2024);
    expect(dt.month).toBe(3);
    expect(dt.day).toBe(15);
    expect(dt.hour).toBe(0);
  });

  test("compare()", () => {
    const dt1 = new Temporal.PlainDateTime(2024, 1, 1, 0, 0);
    const dt2 = new Temporal.PlainDateTime(2024, 6, 1, 0, 0);
    expect(Temporal.PlainDateTime.compare(dt1, dt2)).toBe(-1);
    expect(Temporal.PlainDateTime.compare(dt2, dt1)).toBe(1);
    expect(Temporal.PlainDateTime.compare(dt1, dt1)).toBe(0);
  });

});
