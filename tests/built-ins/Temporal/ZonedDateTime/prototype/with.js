/*---
description: Temporal.ZonedDateTime.prototype.with
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.ZonedDateTime.prototype.with", () => {
  test("with modifies fields", () => {
    const epochNs = 1710510330000n * 1000000n;
    const zdt = new Temporal.ZonedDateTime(epochNs, "UTC");
    const updated = zdt.with({ hour: 10 });
    expect(updated.hour).toBe(10);
    expect(updated.year).toBe(2024);
    expect(updated.month).toBe(3);
    expect(updated.day).toBe(15);
  });
});
