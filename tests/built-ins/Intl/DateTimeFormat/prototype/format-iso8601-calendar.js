/*---
description: Intl.DateTimeFormat names months and eras in the ISO 8601 calendar, also with an ICU whose iso8601 data has no names (ICU 78)
features: [Intl]
---*/

const isIntl = typeof Intl !== "undefined";

describe.runIf(isIntl)("Intl.DateTimeFormat ISO 8601 calendar", () => {
  const start = Date.UTC(2024, 11, 26, 11);
  const end = Date.UTC(2025, 1, 3, 11);
  const iso = (options) => new Intl.DateTimeFormat("en-u-ca-iso8601", { ...options, timeZone: "UTC" });

  test("a lone month is named", () => {
    expect(iso({ month: "long" }).format(start)).toBe("December");
    expect(iso({ month: "short" }).formatToParts(start)).toEqual([{ type: "month", value: "Dec" }]);
  });

  test("an era is named", () => {
    const parts = iso({ era: "short", year: "numeric" }).formatToParts(start);
    expect(parts.find((part) => part.type === "era").value).toBe("AD");
  });

  test("a range names both months", () => {
    const range = iso({ month: "long", year: "numeric" }).formatRange(start, end);
    expect(range.includes("December")).toBe(true);
    expect(range.includes("February")).toBe(true);
  });

  test("a range of one date formats as that date", () => {
    const dtf = iso({ month: "long", day: "numeric" });
    expect(dtf.formatRange(start, start)).toBe(dtf.format(start));
  });

  test("Date.prototype.toLocaleString and Temporal.PlainMonthDay agree", () => {
    const options = { month: "long" };
    const date = new Date(Date.UTC(2024, 11, 26, 11)).toLocaleString("en-u-ca-iso8601", { ...options, timeZone: "UTC" });
    expect(date).toBe("December");
    if (typeof Temporal !== "undefined") {
      expect(new Temporal.PlainMonthDay(12, 26).toLocaleString("en-u-ca-iso8601", options)).toBe(date);
    }
  });
});
