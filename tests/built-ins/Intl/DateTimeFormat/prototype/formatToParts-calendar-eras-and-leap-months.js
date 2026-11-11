/*---
description: formatToParts names the era before an Islamic epoch and shows the Chinese and Dangi leap months Temporal's calendar data gives
features: [Intl.DateTimeFormat, Temporal]
---*/

const utcDate = (year, monthIndex, day) => {
  const date = new Date(0);
  date.setUTCFullYear(year, monthIndex, day);
  return date;
};

const partsOf = (calendar, options, date) => {
  const parts = {};
  for (const part of new Intl.DateTimeFormat("en", { calendar, timeZone: "UTC", ...options }).formatToParts(date)) {
    parts[part.type] = part.value;
  }
  return parts;
};

const supportsCalendar = (calendar) => {
  try {
    return new Intl.DateTimeFormat("en", { calendar }).resolvedOptions().calendar === calendar;
  } catch (_) {
    return false;
  }
};

const hasLunisolarCalendarICU = typeof Temporal !== "undefined" && supportsCalendar("chinese") && (() => {
  try {
    const d = Temporal.PlainDate.from({ calendar: "chinese", year: 2025, monthCode: "M06L", day: 1 });
    return d.toString({ calendarName: "always" }) === "2025-07-25[u-ca=chinese]";
  } catch (_) {
    return false;
  }
})();

describe.runIf(supportsCalendar("islamic-civil"))("Intl.DateTimeFormat eras before an Islamic epoch", () => {
  test("a year before the Hijra has its own era and a positive year", () => {
    const options = { era: "short", year: "numeric" };
    const before = partsOf("islamic-civil", options, utcDate(600, 5, 15));
    const after = partsOf("islamic-civil", options, utcDate(2025, 5, 15));
    expect(after.year).toBe("1446");
    expect(typeof before.era).toBe("string");
    expect(before.era).not.toBe(after.era);
    expect(Number(before.year) > 0).toBe(true);
  });
});

describe.runIf(hasLunisolarCalendarICU)("Intl.DateTimeFormat Chinese and Dangi leap months", () => {
  test("1987-08-23 is day 29 of the leap sixth month, as Temporal reads it", () => {
    const options = { year: "numeric", month: "numeric", day: "numeric" };
    for (const calendar of ["chinese", "dangi"]) {
      const leap = partsOf(calendar, options, utcDate(1987, 7, 23));
      const plain = partsOf(calendar, options, utcDate(1987, 6, 25));
      const temporal = Temporal.PlainDate.from("1987-08-23").withCalendar(calendar);
      expect(temporal.monthCode).toBe("M06L");
      expect(Number(leap.day)).toBe(temporal.day);
      expect(leap.relatedYear).toBe("1987");
      expect(Number(plain.month)).toBe(6);
      expect(leap.month).not.toBe(plain.month);
      expect(leap.month.includes(plain.month)).toBe(true);
    }
  });

  test("a leap month ICU agrees on keeps its own text", () => {
    const options = { year: "numeric", month: "numeric", day: "numeric" };
    const leap = partsOf("chinese", options, utcDate(2025, 6, 25));
    const plain = partsOf("chinese", options, utcDate(2025, 5, 25));
    expect(Number(leap.day)).toBe(1);
    expect(leap.relatedYear).toBe("2025");
    expect(leap.month).not.toBe(plain.month);
    expect(leap.month.includes(plain.month)).toBe(true);
  });
});
