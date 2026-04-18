/*---
description: Temporal.ZonedDateTime.prototype.toInstant
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.ZonedDateTime.prototype.toInstant", () => {
  test("toInstant", () => {
    const epochNs = 1710510330000n * 1000000n;
    const zdt = new Temporal.ZonedDateTime(epochNs, "UTC");
    const inst = zdt.toInstant();
    expect(inst.epochMilliseconds).toBe(1710510330000);
  });
});
