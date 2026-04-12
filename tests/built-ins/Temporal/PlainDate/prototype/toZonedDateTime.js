/*---
description: Temporal.PlainDate.prototype.toZonedDateTime
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainDate.prototype.toZonedDateTime", () => {
  test("toZonedDateTime()", () => {
    const d = new Temporal.PlainDate(2024, 3, 15);
    const zdt = d.toZonedDateTime("UTC");
    expect(zdt.year).toBe(2024);
    expect(zdt.month).toBe(3);
    expect(zdt.day).toBe(15);
    expect(zdt.hour).toBe(0);
    expect(zdt.timeZoneId).toBe("UTC");
  });
});
