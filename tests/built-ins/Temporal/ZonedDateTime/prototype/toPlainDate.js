/*---
description: Temporal.ZonedDateTime.prototype.toPlainDate
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.ZonedDateTime.prototype.toPlainDate", () => {
  test("toPlainDate", () => {
    const epochNs = 1710510330000 * 1000000;
    const zdt = new Temporal.ZonedDateTime(epochNs, "UTC");
    const d = zdt.toPlainDate();
    expect(d.year).toBe(2024);
    expect(d.month).toBe(3);
    expect(d.day).toBe(15);
  });
});
