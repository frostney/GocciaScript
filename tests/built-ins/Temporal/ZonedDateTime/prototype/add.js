/*---
description: Temporal.ZonedDateTime.prototype.add
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.ZonedDateTime.prototype.add", () => {
  test("add duration", () => {
    const epochNs = 1710510330000n * 1000000n;
    const zdt = new Temporal.ZonedDateTime(epochNs, "UTC");
    const added = zdt.add({ hours: 2, minutes: 30 });
    expect(added.hour).toBe(16);
    expect(added.minute).toBe(15);
  });

  test("add walks Coptic month 13 into the next year", () => {
    const zdt = Temporal.ZonedDateTime.from({
      year: 1742,
      monthCode: "M13",
      day: 1,
      hour: 12,
      minute: 34,
      timeZone: "UTC",
      calendar: "coptic",
    });
    const result = zdt.add({ months: 1 });
    expect(result.year).toBe(1743);
    expect(result.month).toBe(1);
    expect(result.monthCode).toBe("M01");
    expect(result.day).toBe(1);
    expect(result.hour).toBe(12);
    expect(result.minute).toBe(34);
  });
});
