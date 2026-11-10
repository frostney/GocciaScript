/*---
description: Intl.DateTimeFormat formats dates before 1582 in the proleptic Gregorian calendar
features: [Intl]
---*/

const isIntl = typeof Intl !== "undefined";

describe.runIf(isIntl)("Intl.DateTimeFormat proleptic Gregorian calendar", () => {
  const options = { timeZone: "UTC", year: "numeric", month: "numeric", day: "numeric" };

  test("a date in the year 1000 keeps its Gregorian day", () => {
    const dtf = new Intl.DateTimeFormat("en-US", options);
    expect(dtf.format(Date.UTC(1000, 0, 1))).toBe("1/1/1000");
  });

  test("the day before the 1582 Gregorian reform is not a Julian date", () => {
    const dtf = new Intl.DateTimeFormat("en-US", options);
    expect(dtf.format(Date.UTC(1582, 9, 14))).toBe("10/14/1582");
  });
});
