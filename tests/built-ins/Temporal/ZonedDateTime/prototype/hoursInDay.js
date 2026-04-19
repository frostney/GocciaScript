/*---
description: Temporal.ZonedDateTime.prototype.hoursInDay
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.ZonedDateTime.prototype.hoursInDay", () => {
  test("hoursInDay", () => {
    const zdt = new Temporal.ZonedDateTime(0n, "UTC");
    expect(zdt.hoursInDay).toBe(24);
  });
});
