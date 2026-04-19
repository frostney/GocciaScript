/*---
description: Temporal.ZonedDateTime.prototype.subtract
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.ZonedDateTime.prototype.subtract", () => {
  test("subtract duration", () => {
    const epochNs = 1710510330000n * 1000000n;
    const zdt = new Temporal.ZonedDateTime(epochNs, "UTC");
    const sub = zdt.subtract({ hours: 1 });
    expect(sub.hour).toBe(12);
    expect(sub.minute).toBe(45);
  });
});
