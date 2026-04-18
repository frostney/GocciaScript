/*---
description: Temporal.ZonedDateTime.prototype.startOfDay
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.ZonedDateTime.prototype.startOfDay", () => {
  test("startOfDay", () => {
    const epochNs = 1710510330000n * 1000000n;
    const zdt = new Temporal.ZonedDateTime(epochNs, "UTC");
    const sod = zdt.startOfDay();
    expect(sod.hour).toBe(0);
    expect(sod.minute).toBe(0);
    expect(sod.second).toBe(0);
    expect(sod.year).toBe(2024);
    expect(sod.month).toBe(3);
    expect(sod.day).toBe(15);
  });
});
