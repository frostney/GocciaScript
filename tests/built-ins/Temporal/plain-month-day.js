/*---
description: Temporal.PlainMonthDay construction, properties, and methods
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainMonthDay", () => {

  test("constructor", () => {
    const md = new Temporal.PlainMonthDay(12, 25);
    expect(md.monthCode).toBe("M12");
    expect(md.day).toBe(25);
  });

  test("calendarId is iso8601", () => {
    const md = new Temporal.PlainMonthDay(1, 1);
    expect(md.calendarId).toBe("iso8601");
  });

  test("from string", () => {
    const md = Temporal.PlainMonthDay.from("12-25");
    expect(md.monthCode).toBe("M12");
    expect(md.day).toBe(25);
  });

  test("from PlainMonthDay", () => {
    const original = new Temporal.PlainMonthDay(6, 15);
    const copy = Temporal.PlainMonthDay.from(original);
    expect(copy.monthCode).toBe("M06");
    expect(copy.day).toBe(15);
  });

  test("from object with monthCode", () => {
    const md = Temporal.PlainMonthDay.from({ monthCode: "M07", day: 4 });
    expect(md.monthCode).toBe("M07");
    expect(md.day).toBe(4);
  });

  test("from object with month", () => {
    const md = Temporal.PlainMonthDay.from({ month: 7, day: 4 });
    expect(md.monthCode).toBe("M07");
    expect(md.day).toBe(4);
  });

  test("equals", () => {
    const a = new Temporal.PlainMonthDay(12, 25);
    const b = new Temporal.PlainMonthDay(12, 25);
    const c = new Temporal.PlainMonthDay(12, 26);
    expect(a.equals(b)).toBe(true);
    expect(a.equals(c)).toBe(false);
  });

  test("toString", () => {
    expect(new Temporal.PlainMonthDay(12, 25).toString()).toBe("12-25");
    expect(new Temporal.PlainMonthDay(1, 1).toString()).toBe("01-01");
  });

  test("toJSON", () => {
    expect(new Temporal.PlainMonthDay(12, 25).toJSON()).toBe("12-25");
  });

  test("valueOf throws", () => {
    const md = new Temporal.PlainMonthDay(12, 25);
    expect(() => md.valueOf()).toThrow(TypeError);
  });

  test("toPlainDate", () => {
    const md = new Temporal.PlainMonthDay(12, 25);
    const d = md.toPlainDate({ year: 2024 });
    expect(d.year).toBe(2024);
    expect(d.month).toBe(12);
    expect(d.day).toBe(25);
  });

  test("Feb 29 is valid", () => {
    const md = new Temporal.PlainMonthDay(2, 29);
    expect(md.day).toBe(29);
    expect(md.monthCode).toBe("M02");
  });

  test("invalid day throws", () => {
    expect(() => new Temporal.PlainMonthDay(2, 30)).toThrow(RangeError);
    expect(() => new Temporal.PlainMonthDay(0, 1)).toThrow(RangeError);
    expect(() => new Temporal.PlainMonthDay(13, 1)).toThrow(RangeError);
  });

  test("Symbol.toStringTag", () => {
    const md = new Temporal.PlainMonthDay(1, 1);
    expect(Object.prototype.toString.call(md)).toBe("[object Temporal.PlainMonthDay]");
  });
});
