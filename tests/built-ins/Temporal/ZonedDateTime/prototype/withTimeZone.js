/*---
description: Temporal.ZonedDateTime.prototype.withTimeZone
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.ZonedDateTime.prototype.withTimeZone", () => {
  test("withTimeZone", () => {
    const zdt = new Temporal.ZonedDateTime(0n, "UTC");
    const rezoned = zdt.withTimeZone("UTC");
    expect(rezoned.timeZoneId).toBe("UTC");
    expect(rezoned.epochMilliseconds).toBe(0);
  });
});
