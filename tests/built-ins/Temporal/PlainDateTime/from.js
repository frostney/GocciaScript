/*---
description: Temporal.PlainDateTime.from
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";
const hasCalendarICU = isTemporal && (() => {
  try {
    return Temporal.PlainDateTime.from({
      year: 1420,
      monthCode: "M09",
      day: 24,
      hour: 10,
      minute: 30,
      calendar: "islamic-civil"
    }).toString() === "2000-01-01T10:30:00[u-ca=islamic-civil]";
  } catch (_) {
    return false;
  }
})();

describe.runIf(isTemporal)("Temporal.PlainDateTime.from", () => {
  test("from() with string", () => {
    const dt = Temporal.PlainDateTime.from("2024-03-15T10:30:00");
    expect(dt.year).toBe(2024);
    expect(dt.month).toBe(3);
    expect(dt.day).toBe(15);
    expect(dt.hour).toBe(10);
    expect(dt.minute).toBe(30);
  });

  test("from() with date-only string", () => {
    const dt = Temporal.PlainDateTime.from("2024-03-15");
    expect(dt.year).toBe(2024);
    expect(dt.month).toBe(3);
    expect(dt.day).toBe(15);
    expect(dt.hour).toBe(0);
  });

  test("from() accepts +000000 as year zero", () => {
    const dt = Temporal.PlainDateTime.from("+000000-03-31T00:45");
    expect(dt.year).toBe(0);
    expect(dt.month).toBe(3);
    expect(dt.day).toBe(31);
    expect(dt.hour).toBe(0);
    expect(dt.minute).toBe(45);
  });

  test("from() converts supported non-ISO calendar date fields", () => {
    if (!hasCalendarICU) {
      expect(() => Temporal.PlainDateTime.from({
        year: 1420,
        monthCode: "M09",
        day: 24,
        hour: 10,
        minute: 30,
        calendar: "islamic-civil"
      })).toThrow(RangeError);
      return;
    }
    const dt = Temporal.PlainDateTime.from({
      year: 1420,
      monthCode: "M09",
      day: 24,
      hour: 10,
      minute: 30,
      calendar: "islamic-civil"
    });
    expect(dt.year).toBe(1420);
    expect(dt.month).toBe(9);
    expect(dt.day).toBe(24);
    expect(dt.hour).toBe(10);
    expect(dt.minute).toBe(30);
    expect(dt.toString()).toBe("2000-01-01T10:30:00[u-ca=islamic-civil]");
  });
});

describe.runIf(isTemporal)("Temporal.PlainDateTime.from non-finite fields", () => {
  test("hour Infinity throws RangeError", () => {
    expect(() => Temporal.PlainDateTime.from({ year: 2026, month: 6, day: 11, hour: Infinity })).toThrow(RangeError);
  });
});
