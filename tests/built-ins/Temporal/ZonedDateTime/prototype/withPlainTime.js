/*---
description: Temporal.ZonedDateTime.prototype.withPlainTime
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.ZonedDateTime.prototype.withPlainTime", () => {
  test("withPlainTime", () => {
    const epochNs = 1710510330000n * 1000000n;
    const zdt = new Temporal.ZonedDateTime(epochNs, "UTC");
    const updated = zdt.withPlainTime(new Temporal.PlainTime(0, 0, 0));
    expect(updated.hour).toBe(0);
    expect(updated.minute).toBe(0);
    expect(updated.second).toBe(0);
    expect(updated.year).toBe(2024);
    expect(updated.month).toBe(3);
    expect(updated.day).toBe(15);
  });
});
