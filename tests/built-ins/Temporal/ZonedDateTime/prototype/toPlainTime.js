/*---
description: Temporal.ZonedDateTime.prototype.toPlainTime
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.ZonedDateTime.prototype.toPlainTime", () => {
  test("toPlainTime", () => {
    const epochNs = 1710510330000 * 1000000;
    const zdt = new Temporal.ZonedDateTime(epochNs, "UTC");
    const t = zdt.toPlainTime();
    expect(t.hour).toBe(13);
    expect(t.minute).toBe(45);
    expect(t.second).toBe(30);
  });
});
