/*---
description: Intl.DateTimeFormat.prototype.formatToParts
features: [Intl]
---*/

const isIntl = typeof Intl !== "undefined";
const isTemporal = typeof Temporal !== "undefined";

const partsString = (parts) => {
  return parts.map((part) => part.value).join("");
};

const expectCurrentDateParts = (readActual, formatAt) => {
  const before = Date.now();
  const actual = partsString(readActual());
  const after = Date.now();
  const beforeFormatted = partsString(formatAt(before));
  const afterFormatted = partsString(formatAt(after));

  expect(actual === beforeFormatted || actual === afterFormatted).toBe(true);
};

describe.runIf(isIntl)("Intl.DateTimeFormat.prototype.formatToParts", () => {
  test("formatToParts uses the current date for omitted and undefined values", () => {
    const dtf = new Intl.DateTimeFormat("en-US", { timeZone: "UTC" });
    expectCurrentDateParts(() => dtf.formatToParts(), (value) => dtf.formatToParts(value));
    expectCurrentDateParts(() => dtf.formatToParts(undefined), (value) => dtf.formatToParts(value));
  });

  test("formatToParts rejects invalid numeric dates", () => {
    const dtf = new Intl.DateTimeFormat("en-US");
    expect(() => dtf.formatToParts(NaN)).toThrow(RangeError);
  });

  test("formatToParts normalizes platform-dependent era data", () => {
    const makeDate = (year) => {
      const date = new Date(0);
      date.setFullYear(year, 5, 15);
      date.setHours(0, 0, 0, 0);
      return date;
    };
    const eraOf = (formatter, year) => {
      return formatter.formatToParts(makeDate(year))
        .find((part) => part.type === "era").value;
    };
    const islamic = new Intl.DateTimeFormat("en", {
      calendar: "islamic-civil",
      era: "long",
      year: "numeric",
    });
    const coptic = new Intl.DateTimeFormat("en", {
      calendar: "coptic",
      era: "long",
      year: "numeric",
    });

    expect(eraOf(islamic, 600) === eraOf(islamic, 2025)).toBe(false);
    expect(eraOf(coptic, 250)).toBe(eraOf(coptic, 2025));
  });

  test("formatToParts normalizes Chinese leap month parts", () => {
    if (!isTemporal) {
      return;
    }

    const leapMonthDate = Temporal.PlainDate.from({
      calendar: "chinese",
      year: 1987,
      month: 7,
      day: 29,
    });
    const nextMonthDate = Temporal.PlainDate.from({
      calendar: "chinese",
      year: 1987,
      month: 8,
      day: 30,
    });
    const formatter = new Intl.DateTimeFormat("en", {
      calendar: "chinese",
      timeZone: "UTC",
      year: "numeric",
      month: "numeric",
      day: "numeric",
    });
    const monthPart = (date) => formatter.formatToParts(
      date.withCalendar("iso8601").toZonedDateTime("UTC").epochMilliseconds
    ).find((part) => part.type === "month");

    expect(leapMonthDate.monthCode).toBe("M06L");
    expect(monthPart(leapMonthDate).value).toBe("6bis");
    expect(nextMonthDate.monthCode).toBe("M07");
    expect(monthPart(nextMonthDate).value).toBe("7");
  });
});
