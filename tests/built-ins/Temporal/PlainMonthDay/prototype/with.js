/*---
description: Temporal.PlainMonthDay.prototype.with
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";
const hasCalendarICU = isTemporal && (() => {
  try {
    return Temporal.PlainMonthDay.from({ calendar: "hebrew", monthCode: "M11", day: 4 }).calendarId === "hebrew";
  } catch (_) {
    return false;
  }
})();

describe.runIf(isTemporal)("Temporal.PlainMonthDay.prototype.with", () => {
  test("with accepts numeric month", () => {
    const md = new Temporal.PlainMonthDay(3, 15);
    const updated = md.with({ month: 8 });
    expect(updated.monthCode).toBe("M08");
  });

  test("with throws on conflicting month and monthCode", () => {
    const md = new Temporal.PlainMonthDay(3, 15);
    expect(() => md.with({ monthCode: "M05", month: 6 })).toThrow();
  });

  test("with accepts consistent month and monthCode", () => {
    const md = new Temporal.PlainMonthDay(3, 15);
    const updated = md.with({ monthCode: "M05", month: 5 });
    expect(updated.monthCode).toBe("M05");
  });

  test("with resolves non-ISO monthCode fields", () => {
    if (!hasCalendarICU) {
      expect(() => Temporal.PlainMonthDay.from({ calendar: "hebrew", monthCode: "M11", day: 4 })).toThrow(RangeError);
      return;
    }
    const md = Temporal.PlainMonthDay.from({ calendar: "hebrew", monthCode: "M11", day: 4 });
    const updatedMonth = md.with({ monthCode: "M10" });
    expect(updatedMonth.monthCode).toBe("M10");
    expect(updatedMonth.day).toBe(4);

    const updatedDay = md.with({ day: 24 });
    expect(updatedDay.monthCode).toBe("M11");
    expect(updatedDay.day).toBe(24);
  });

  test("with rejects bare month for non-ISO month-day", () => {
    const md = Temporal.PlainMonthDay.from({ year: 2021, month: 1, day: 15, calendar: "gregory" });
    expect(() => md.with({ month: 12 })).toThrow(TypeError);
  });
});
