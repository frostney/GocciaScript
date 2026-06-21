/*---
description: Temporal.PlainDate.from
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";
const hasCalendarICU = isTemporal && (() => {
  try {
    return Temporal.PlainDate.from({
      year: 1420,
      month: 9,
      day: 24,
      calendar: "islamic-civil"
    }).toString() === "2000-01-01[u-ca=islamic-civil]";
  } catch (_) {
    return false;
  }
})();

describe.runIf(isTemporal)("Temporal.PlainDate.from", () => {
  test("from() with string", () => {
    const d = Temporal.PlainDate.from("2024-03-15");
    expect(d.year).toBe(2024);
    expect(d.month).toBe(3);
    expect(d.day).toBe(15);
  });

  test("from() with object", () => {
    const d = Temporal.PlainDate.from({ year: 2024, month: 6, day: 15 });
    expect(d.year).toBe(2024);
    expect(d.month).toBe(6);
    expect(d.day).toBe(15);
  });

  test("constrain clamps day", () => {
    const d = Temporal.PlainDate.from({ year: 2024, month: 2, day: 30 }, { overflow: "constrain" });
    expect(d.day).toBe(29);
  });

  test("reject throws on invalid day", () => {
    expect(() => {
      Temporal.PlainDate.from({ year: 2024, month: 2, day: 30 }, { overflow: "reject" });
    }).toThrow(RangeError);
  });

  test("from with overflow constrain clamps month", () => {
    const d = Temporal.PlainDate.from({ year: 2024, month: 13, day: 15 }, { overflow: "constrain" });
    expect(d.month).toBe(12);
    expect(d.day).toBe(15);
  });

  test("from with overflow constrain rejects day below 1", () => {
    expect(() => {
      Temporal.PlainDate.from({ year: 2024, month: 3, day: 0 }, { overflow: "constrain" });
    }).toThrow(RangeError);
  });

  test("from() accepts +000000 as year zero", () => {
    const d = Temporal.PlainDate.from("+000000-03-31");
    expect(d.year).toBe(0);
    expect(d.month).toBe(3);
    expect(d.day).toBe(31);
  });

  test("from() converts supported non-ISO calendar fields", () => {
    if (!hasCalendarICU) {
      expect(() => Temporal.PlainDate.from({
        year: 1420,
        month: 9,
        day: 24,
        calendar: "islamic-civil"
      })).toThrow(RangeError);
      return;
    }
    const d = Temporal.PlainDate.from({
      year: 1420,
      month: 9,
      day: 24,
      calendar: "islamic-civil"
    });
    expect(d.year).toBe(1420);
    expect(d.month).toBe(9);
    expect(d.monthCode).toBe("M09");
    expect(d.day).toBe(24);
    expect(d.calendarId).toBe("islamic-civil");
    expect(d.toString()).toBe("2000-01-01[u-ca=islamic-civil]");
  });
});

describe.runIf(isTemporal)("Temporal.PlainDate.from non-finite fields", () => {
  test("year Infinity throws RangeError", () => {
    expect(() => Temporal.PlainDate.from({ year: Infinity, month: 1, day: 1 })).toThrow(RangeError);
  });

  test("year NaN throws RangeError", () => {
    expect(() => Temporal.PlainDate.from({ year: NaN, month: 1, day: 1 })).toThrow(RangeError);
  });
});
