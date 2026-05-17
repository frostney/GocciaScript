/*---
description: Intl.DateTimeFormat.prototype.formatRange
features: [Intl]
---*/

const isIntl = typeof Intl !== "undefined";
const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isIntl)("Intl.DateTimeFormat.prototype.formatRange", () => {
  test("formatRange is exposed", () => {
    const dtf = new Intl.DateTimeFormat("en-US");
    expect(typeof dtf.formatRange).toBe("function");
  });

  test("formats a collapsed date range using requested fields", () => {
    const dtf = new Intl.DateTimeFormat("en-US", {
      year: "numeric",
      month: "short",
      day: "numeric",
      timeZone: "UTC"
    });
    expect(dtf.format(Date.UTC(2026, 0, 1))).toBe("Jan 1, 2026");
    expect(dtf.formatRange(Date.UTC(2026, 0, 1), Date.UTC(2026, 0, 15))).toBe("Jan 1 – 15, 2026");
  });

  test("throws RangeError for invalid endpoints", () => {
    const dtf = new Intl.DateTimeFormat("en-US");
    expect(() => dtf.formatRange(NaN, 0)).toThrow(RangeError);
    expect(() => dtf.formatRange(0, NaN)).toThrow(RangeError);
  });
});

describe.runIf(isIntl && isTemporal)("Intl.DateTimeFormat.prototype.formatRange Temporal inputs", () => {
  test("uses Temporal.PlainDateTime defaults without applying the formatter time zone", () => {
    const dtf = new Intl.DateTimeFormat("en-US", { timeZone: "Pacific/Apia" });
    const start = new Temporal.PlainDateTime(2021, 8, 4, 0, 30, 45);
    const end = new Temporal.PlainDateTime(2021, 8, 4, 23, 30, 45);
    expect(dtf.formatRange(start, end)).toBe("8/4/2021, 12:30:45 AM – 11:30:45 PM");
  });

  test("rejects Temporal.ZonedDateTime directly", () => {
    const dtf = new Intl.DateTimeFormat("en-US");
    const start = new Temporal.ZonedDateTime(0n, "UTC");
    const end = new Temporal.ZonedDateTime(1000000000n, "UTC");
    expect(() => dtf.formatRange(start, end)).toThrow(TypeError);
  });
});
