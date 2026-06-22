/*---
description: Temporal.PlainDate.prototype calendar getters (dayOfWeek, dayOfYear, etc.)
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";
const hasLunisolarCalendarICU = isTemporal && (() => {
  try {
    const d = Temporal.PlainDate.from({
      calendar: "chinese",
      year: 2025,
      monthCode: "M06L",
      day: 1,
    });
    return d.toString({ calendarName: "always" }) === "2025-07-25[u-ca=chinese]";
  } catch (_) {
    return false;
  }
})();

describe.runIf(isTemporal)("Temporal.PlainDate.prototype calendar getters", () => {
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

  test("lunisolar getters expose related year and leap month", () => {
    if (!hasLunisolarCalendarICU) {
      return;
    }

    for (const calendar of ["chinese", "dangi"]) {
      const d = Temporal.PlainDate.from({
        calendar,
        year: 2025,
        monthCode: "M06L",
        day: 1,
      });
      expect(d.toString({ calendarName: "always" })).toBe(`2025-07-25[u-ca=${calendar}]`);
      expect(d.year).toBe(2025);
      expect(d.month).toBe(7);
      expect(d.monthCode).toBe("M06L");
      expect(d.day).toBe(1);
      expect(d.daysInYear).toBe(384);
      expect(d.monthsInYear).toBe(13);
      expect(d.inLeapYear).toBe(true);
    }
  });
});
