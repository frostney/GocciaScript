/*---
description: Temporal.ZonedDateTime.prototype.toPlainDateTime
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.ZonedDateTime.prototype.toPlainDateTime", () => {
  test("toPlainDateTime", () => {
    const epochNs = 1710510330000n * 1000000n;
    const zdt = new Temporal.ZonedDateTime(epochNs, "UTC");
    const dt = zdt.toPlainDateTime();
    expect(dt.year).toBe(2024);
    expect(dt.month).toBe(3);
    expect(dt.day).toBe(15);
    expect(dt.hour).toBe(13);
    expect(dt.minute).toBe(45);
    expect(dt.second).toBe(30);
  });
});
