/*---
description: Temporal.PlainDateTime.prototype.with
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";
const hasCalendarICU = isTemporal && (() => {
  try {
    return Temporal.PlainDate.from({ year: 5784, monthCode: "M05L", day: 1, calendar: "hebrew" }).calendarId === "hebrew";
  } catch (_) {
    return false;
  }
})();

describe.runIf(isTemporal)("Temporal.PlainDateTime.prototype.with", () => {
  test("with()", () => {
    const dt = new Temporal.PlainDateTime(2024, 3, 15, 10, 30);
    const updated = dt.with({ hour: 14, month: 6 });
    expect(updated.year).toBe(2024);
    expect(updated.month).toBe(6);
    expect(updated.day).toBe(15);
    expect(updated.hour).toBe(14);
    expect(updated.minute).toBe(30);
  });

  test("with resolves non-ISO calendar fields and preserves time", () => {
    const dt = new Temporal.PlainDateTime(2024, 8, 8, 10, 30, 5, 6, 7, 8, "hebrew");
    if (!hasCalendarICU) {
      expect(() => dt.with({ year: 5783, hour: 14 })).toThrow(RangeError);
      return;
    }
    const updated = dt.with({ year: 5783, hour: 14 });
    expect(updated.year).toBe(5783);
    expect(updated.month).toBe(11);
    expect(updated.monthCode).toBe("M11");
    expect(updated.day).toBe(4);
    expect(updated.hour).toBe(14);
    expect(updated.minute).toBe(30);
    expect(updated.second).toBe(5);
    expect(updated.millisecond).toBe(6);
    expect(updated.microsecond).toBe(7);
    expect(updated.nanosecond).toBe(8);
  });
});
