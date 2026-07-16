/*---
description: Temporal.PlainDate.prototype.add
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

describe.runIf(isTemporal)("Temporal.PlainDate.prototype.add", () => {
  test("add() with duration", () => {
    const d = new Temporal.PlainDate(2024, 1, 31);
    const result = d.add(new Temporal.Duration(0, 1));
    expect(result.year).toBe(2024);
    expect(result.month).toBe(2);
    expect(result.day).toBe(29);
    expect(() => d.add(new Temporal.Duration(0, 1), { overflow: "reject" })).toThrow(RangeError);
  });

  test("add() with days", () => {
    const d = new Temporal.PlainDate(2024, 1, 1);
    const result = d.add(new Temporal.Duration(0, 0, 0, 10));
    expect(result.toString()).toBe("2024-01-11");
  });

  test("add rejects huge calendar month offsets without walking every month", () => {
    const d = new Temporal.PlainDate(2024, 1, 1);
    expect(() => d.add({ months: 500000000 })).toThrow(RangeError);
  });

  test("add constrains Hebrew leap month when target year has no leap month", () => {
    if (!hasCalendarICU) {
      expect(() => Temporal.PlainDate.from({ year: 5784, monthCode: "M05L", day: 1, calendar: "hebrew" })).toThrow(RangeError);
      return;
    }
    const d = Temporal.PlainDate.from({ year: 5784, monthCode: "M05L", day: 1, calendar: "hebrew" });
    const result = d.add({ years: 1 });
    expect(result.year).toBe(5785);
    expect(result.month).toBe(6);
    expect(result.monthCode).toBe("M06");
    expect(result.day).toBe(1);
    expect(() => d.add({ years: 1 }, { overflow: "reject" })).toThrow(RangeError);
  });

  test("add walks Coptic month 13 into the next year", () => {
    const d = Temporal.PlainDate.from({ year: 1742, monthCode: "M13", day: 1, calendar: "coptic" });
    const result = d.add({ months: 1 });
    expect(result.year).toBe(1743);
    expect(result.month).toBe(1);
    expect(result.monthCode).toBe("M01");
    expect(result.day).toBe(1);
  });

  test("rejects invalid duration strings", () => {
    expect(() => new Temporal.PlainDate(2024, 1, 1).add("not-a-duration")).toThrow(RangeError);
  });
});
