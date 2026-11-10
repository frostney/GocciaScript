/*---
description: Chinese and Dangi dates beyond year 10000 either way follow a mean lunisolar model that round-trips through ISO
features: [Temporal]
---*/

const hasLunisolarCalendarICU = typeof Temporal !== "undefined" && (() => {
  try {
    const d = Temporal.PlainDate.from({ calendar: "chinese", year: 2025, monthCode: "M06L", day: 1 });
    return d.toString({ calendarName: "always" }) === "2025-07-25[u-ca=chinese]";
  } catch (_) {
    return false;
  }
})();

describe.runIf(hasLunisolarCalendarICU)("Temporal.PlainDate.from with far Chinese and Dangi years", () => {
  test("a new year far in the past or future exists and starts in January or February of its ISO year", () => {
    for (const calendar of ["chinese", "dangi"]) {
      for (const year of [-250000, -10001, 10001, 250000]) {
        const date = Temporal.PlainDate.from({ calendar, year, month: 1, day: 1 });
        expect(date.year).toBe(year);
        expect(date.monthCode).toBe("M01");
        expect(date.day).toBe(1);
        const iso = date.withCalendar("iso8601");
        expect(iso.year).toBe(year);
        expect(iso.month <= 2).toBe(true);
      }
    }
  });

  test("each day across the model boundaries reads back as the same date", () => {
    for (const calendar of ["chinese", "dangi"]) {
      for (const start of ["+010000-10-01", "-010001-10-01"]) {
        let previous = null;
        for (const i of Array.from({ length: 300 }, (_, k) => k)) {
          const iso = Temporal.PlainDate.from(start).add({ days: i });
          const date = iso.withCalendar(calendar);
          const back = Temporal.PlainDate.from({ calendar, year: date.year, monthCode: date.monthCode, day: date.day });
          expect(back.equals(date)).toBe(true);
          const key = date.year * 10000 + date.month * 100 + date.day;
          if (previous !== null) expect(key > previous).toBe(true);
          previous = key;
        }
      }
    }
  });

  test("the edges of the representable range convert", () => {
    for (const calendar of ["chinese", "dangi"]) {
      const min = Temporal.PlainDate.from("-271821-04-19").withCalendar(calendar);
      const max = Temporal.PlainDate.from("+275760-09-13").withCalendar(calendar);
      expect(min.year).toBe(-271821);
      expect(max.year).toBe(275760);
      expect(max.daysInMonth >= 29).toBe(true);
    }
  });
});
