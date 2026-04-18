/*---
description: Temporal.ZonedDateTime.prototype.add
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.ZonedDateTime.prototype.add", () => {
  test("add duration", () => {
    const epochNs = 1710510330000n * 1000000n;
    const zdt = new Temporal.ZonedDateTime(epochNs, "UTC");
    const added = zdt.add({ hours: 2, minutes: 30 });
    expect(added.hour).toBe(16);
    expect(added.minute).toBe(15);
  });
});
