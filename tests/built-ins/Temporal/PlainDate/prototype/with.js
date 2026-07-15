/*---
description: Temporal.PlainDate.prototype.with
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

describe.runIf(isTemporal)("Temporal.PlainDate.prototype.with", () => {
  test("with()", () => {
    const d = new Temporal.PlainDate(2024, 3, 15);
    const updated = d.with({ month: 6 });
    expect(updated.year).toBe(2024);
    expect(updated.month).toBe(6);
    expect(updated.day).toBe(15);
  });

  test("with preserves non-ISO monthCode when year changes", () => {
    const d = new Temporal.PlainDate(2024, 8, 8, "hebrew");
    if (!hasCalendarICU) {
      expect(() => d.with({ year: 5783 })).toThrow(RangeError);
      return;
    }
    const updated = d.with({ year: 5783 });
    expect(updated.year).toBe(5783);
    expect(updated.month).toBe(11);
    expect(updated.monthCode).toBe("M11");
    expect(updated.day).toBe(4);
  });

  test("with constrains Hebrew leap month to common-year Adar", () => {
    if (!hasCalendarICU) {
      expect(() => Temporal.PlainDate.from({ year: 5784, monthCode: "M05L", day: 1, calendar: "hebrew" })).toThrow(RangeError);
      return;
    }
    const d = Temporal.PlainDate.from({ year: 5784, monthCode: "M05L", day: 1, calendar: "hebrew" });
    const updated = d.with({ year: 5783 });
    expect(updated.year).toBe(5783);
    expect(updated.month).toBe(6);
    expect(updated.monthCode).toBe("M06");
    expect(updated.day).toBe(1);
    expect(() => d.with({ year: 5783 }, { overflow: "reject" })).toThrow(RangeError);
  });

  test("with reads options before validating calendar fields", () => {
    let overflowRead = false;
    const options = {
      get overflow() {
        overflowRead = true;
        return "constrain";
      }
    };
    const date = new Temporal.PlainDate(2025, 7, 31);
    expect(() => date.with({ monthCode: "M08L" }, options)).toThrow(RangeError);
    expect(overflowRead).toBe(true);
  });
});
