/*---
description: Temporal.PlainDateTime.prototype.toZonedDateTime
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainDateTime.prototype.toZonedDateTime", () => {
  test("toZonedDateTime()", () => {
    const dt = new Temporal.PlainDateTime(2024, 3, 15, 13, 45, 30);
    const zdt = dt.toZonedDateTime("UTC");
    expect(zdt.year).toBe(2024);
    expect(zdt.month).toBe(3);
    expect(zdt.day).toBe(15);
    expect(zdt.hour).toBe(13);
    expect(zdt.timeZoneId).toBe("UTC");
  });
});
