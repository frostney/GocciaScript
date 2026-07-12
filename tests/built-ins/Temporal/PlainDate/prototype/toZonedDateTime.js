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

  test("toZonedDateTime ignores a plainTime calendar annotation", () => {
    const date = new Temporal.PlainDate(2024, 3, 15);
    const result = date.toZonedDateTime({
      plainTime: "12:34:56.987654321[!u-ca=unknown]",
      timeZone: "UTC"
    });
    expect(result.toPlainTime().toString()).toBe("12:34:56.987654321");
  });
});
