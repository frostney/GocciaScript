/*---
description: Intl.DateTimeFormat formats ranges before 1582 in the proleptic Gregorian calendar
features: [Intl]
---*/

const isIntl = typeof Intl !== "undefined";

describe.runIf(isIntl)("Intl.DateTimeFormat formatRange proleptic Gregorian calendar", () => {
  const options = { timeZone: "UTC", year: "numeric", month: "numeric", day: "numeric" };

  test("a range in the year 1000 keeps its Gregorian days", () => {
    const dtf = new Intl.DateTimeFormat("en-US", options);
    const text = dtf.formatRange(Date.UTC(1000, 0, 1), Date.UTC(1000, 0, 2));
    expect(text.includes("1/1/1000")).toBe(true);
    expect(text.includes("1/2/1000")).toBe(true);
  });

  test("formatRangeToParts reports the Gregorian year", () => {
    const dtf = new Intl.DateTimeFormat("en-US", options);
    const parts = dtf.formatRangeToParts(Date.UTC(1000, 0, 1), Date.UTC(1000, 0, 2));
    expect(parts.filter((p) => p.type === "year").map((p) => p.value)).toEqual(["1000", "1000"]);
  });
});
