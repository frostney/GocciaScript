/*---
description: Temporal.ZonedDateTime.prototype.toString
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.ZonedDateTime.prototype.toString", () => {
  test("toString includes timezone annotation", () => {
    const epochNs = 1710510330000n * 1000000n;
    const zdt = new Temporal.ZonedDateTime(epochNs, "UTC");
    const s = zdt.toString();
    expect(s).toContain("[UTC]");
    expect(s).toContain("2024-03-15");
    expect(s).toContain("13:45:30");
  });
});
