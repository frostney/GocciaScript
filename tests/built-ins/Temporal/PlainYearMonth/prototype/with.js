/*---
description: Temporal.PlainYearMonth.prototype.with
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainYearMonth.prototype.with", () => {
  test("with", () => {
    const ym = new Temporal.PlainYearMonth(2024, 3);
    const updated = ym.with({ month: 6 });
    expect(updated.year).toBe(2024);
    expect(updated.month).toBe(6);
  });

  test("with resolves non-ISO calendar month and reference day", () => {
    const ym = Temporal.PlainYearMonth.from({ year: 1716, monthCode: "M04", calendar: "coptic" });
    const updated = ym.with({ month: 1 });
    expect(updated.year).toBe(1716);
    expect(updated.month).toBe(1);
    expect(updated.monthCode).toBe("M01");
    expect(updated.toPlainDate({ day: 1 }).withCalendar("iso8601").day).toBe(
      Number(updated.toString({ calendarName: "always" }).slice(1).split("-")[2].slice(0, 2))
    );
  });
});
