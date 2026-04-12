/*---
description: Temporal.ZonedDateTime.prototype.equals
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.ZonedDateTime.prototype.equals", () => {
  test("equals", () => {
    const a = new Temporal.ZonedDateTime(1000000000, "UTC");
    const b = new Temporal.ZonedDateTime(1000000000, "UTC");
    const c = new Temporal.ZonedDateTime(2000000000, "UTC");
    expect(a.equals(b)).toBe(true);
    expect(a.equals(c)).toBe(false);
  });
});
