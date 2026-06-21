/*---
description: Intl.DateTimeFormat.prototype.formatRange
features: [Intl]
---*/

const isIntl = typeof Intl !== "undefined";
const isTemporal = typeof Temporal !== "undefined";
const hasFullICU = isIntl && new Intl.NumberFormat("en-US").format(NaN) === "NaN";

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
    if (!hasFullICU) {
      expect(typeof dtf.formatRange(Date.UTC(2026, 0, 1), Date.UTC(2026, 0, 15))).toBe("string");
      return;
    }
    expect(dtf.format(Date.UTC(2026, 0, 1))).toBe("Jan 1, 2026");
    expect(dtf.formatRange(Date.UTC(2026, 0, 1), Date.UTC(2026, 0, 15))).toBe("Jan 1 – 15, 2026");
  });

  test("honors explicit hourCycle options in range skeletons", () => {
    const start = Date.UTC(2026, 0, 1, 0, 30);
    const end = Date.UTC(2026, 0, 1, 1, 30);
    const options = { hour: "numeric", minute: "2-digit", timeZone: "UTC" };

    if (!hasFullICU) {
      expect(typeof new Intl.DateTimeFormat("en-US", { ...options, hourCycle: "h11" }).formatRange(start, end))
        .toBe("string");
      return;
    }

    expect(new Intl.DateTimeFormat("en-US", { ...options, hourCycle: "h11" }).formatRange(start, end))
      .toBe("0:30 – 1:30 AM");
    expect(new Intl.DateTimeFormat("en-US", { ...options, hourCycle: "h12" }).formatRange(start, end))
      .toBe("12:30 – 1:30 AM");
    expect(new Intl.DateTimeFormat("en-US", { ...options, hourCycle: "h23" }).formatRange(start, end))
      .toBe("00:30 – 01:30");
    expect(new Intl.DateTimeFormat("en-US", { ...options, hourCycle: "h24" }).formatRange(start, end))
      .toBe("24:30 – 01:30");
    expect(new Intl.DateTimeFormat("en-US-u-hc-h24", { ...options, hourCycle: "h11" }).formatRange(start, end))
      .toBe("0:30 – 1:30 AM");
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
    if (!hasFullICU) {
      expect(typeof dtf.formatRange(start, end)).toBe("string");
      return;
    }
    expect(dtf.formatRange(start, end)).toBe("8/4/2021, 12:30:45 AM – 11:30:45 PM");
  });

  test("localizes large Temporal.PlainDate ranges through ICU", () => {
    const dtf = new Intl.DateTimeFormat("fr-FR", {
      weekday: "long",
      year: "numeric",
      month: "long",
      day: "numeric"
    });
    const start = new Temporal.PlainDate(10000, 1, 1);
    const end = new Temporal.PlainDate(10000, 1, 2);
    if (!hasFullICU) {
      expect(typeof dtf.formatRange(start, end)).toBe("string");
      return;
    }
    expect(dtf.formatRange(start, end)).toBe("samedi 1 – dimanche 2 janvier 10000");
  });

  test("formats Temporal.PlainDate extremes without Date TimeClip", () => {
    const dtf = new Intl.DateTimeFormat("en", {
      weekday: "long",
      year: "numeric",
      month: "numeric",
      day: "numeric",
      calendar: "iso8601"
    });
    const start = new Temporal.PlainDate(-271821, 4, 19);
    const end = new Temporal.PlainDate(275760, 9, 13);
    const result = dtf.formatRange(start, end);
    const weekdays = ["Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"];

    if (!hasFullICU) {
      expect(typeof result).toBe("string");
      return;
    }

    expect(result).toContain("-271821");
    expect(result).toContain("19");
    expect(result).toContain("275760");
    expect(result).toContain("13");
    expect(weekdays.some((weekday) => result.includes(weekday))).toBe(true);
  });

  test("does not collapse Temporal.PlainDate ranges separated by Gregorian cycles", () => {
    const dtf = new Intl.DateTimeFormat("en-US", {
      year: "numeric",
      month: "short",
      day: "numeric"
    });
    const start = new Temporal.PlainDate(10000, 1, 1);
    const end = new Temporal.PlainDate(10400, 1, 1);

    if (!hasFullICU) {
      expect(typeof dtf.formatRange(start, end)).toBe("string");
      return;
    }

    expect(dtf.formatRange(start, end)).toBe("Jan 1, 10000 – Jan 1, 10400");
  });

  test("formats Temporal.PlainDate ranges with mixed eras", () => {
    const dtf = new Intl.DateTimeFormat("en", {
      calendar: "iso8601",
      era: "short",
      year: "numeric",
      month: "long",
      day: "numeric"
    });
    const start = new Temporal.PlainDate(-271821, 4, 19);
    const end = new Temporal.PlainDate(275760, 9, 13);
    const result = dtf.formatRange(start, end);

    if (!hasFullICU) {
      expect(typeof result).toBe("string");
      return;
    }

    expect(result).toContain("BC");
    expect(result).toContain("AD");
    expect(result).toContain("-271821");
    expect(result).toContain("275760");
    expect(result).toContain("19");
    expect(result).toContain("13");
  });

  test("rejects Temporal.ZonedDateTime directly", () => {
    const dtf = new Intl.DateTimeFormat("en-US");
    const start = new Temporal.ZonedDateTime(0n, "UTC");
    const end = new Temporal.ZonedDateTime(1000000000n, "UTC");
    expect(() => dtf.formatRange(start, end)).toThrow(TypeError);
  });
});
