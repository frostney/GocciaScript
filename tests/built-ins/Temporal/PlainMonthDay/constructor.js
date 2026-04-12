/*---
description: Temporal.PlainMonthDay constructor
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainMonthDay constructor", () => {
  test("constructor", () => {
    const md = new Temporal.PlainMonthDay(12, 25);
    expect(md.monthCode).toBe("M12");
    expect(md.day).toBe(25);
  });

  test("calendarId is iso8601", () => {
    const md = new Temporal.PlainMonthDay(1, 1);
    expect(md.calendarId).toBe("iso8601");
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
});
