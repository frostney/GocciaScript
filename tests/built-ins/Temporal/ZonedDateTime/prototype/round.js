/*---
description: Temporal.ZonedDateTime.prototype.round
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.ZonedDateTime.prototype.round", () => {
  test("roundingMode trunc", () => {
    const epochNs = 1710510330000 * 1000000;
    const zdt = new Temporal.ZonedDateTime(epochNs, "UTC");
    const rounded = zdt.round({ smallestUnit: "second", roundingMode: "trunc" });
    expect(rounded.second).toBe(30);
    expect(rounded.millisecond).toBe(0);
  });
});
