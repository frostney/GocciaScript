/*---
description: Temporal.ZonedDateTime.prototype.with
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.ZonedDateTime.prototype.with", () => {
  test("with modifies fields", () => {
    const epochNs = 1710510330000n * 1000000n;
    const zdt = new Temporal.ZonedDateTime(epochNs, "UTC");
    const updated = zdt.with({ hour: 10 });
    expect(updated.hour).toBe(10);
    expect(updated.year).toBe(2024);
    expect(updated.month).toBe(3);
    expect(updated.day).toBe(15);
  });

  test("with resolves non-ISO calendar fields", () => {
    const zdt = Temporal.ZonedDateTime.from({
      era: "be",
      eraYear: 2543,
      monthCode: "M01",
      day: 1,
      hour: 12,
      minute: 34,
      calendar: "buddhist",
      timeZone: "UTC"
    });
    const updated = zdt.with({ year: 2220 });
    expect(updated.year).toBe(2220);
    expect(updated.month).toBe(1);
    expect(updated.monthCode).toBe("M01");
    expect(updated.day).toBe(1);
    expect(updated.hour).toBe(12);
    expect(updated.minute).toBe(34);
    expect(updated.calendarId).toBe("buddhist");
  });
});
