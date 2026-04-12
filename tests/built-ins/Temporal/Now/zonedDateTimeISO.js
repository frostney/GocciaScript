/*---
description: Temporal.Now.zonedDateTimeISO
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.Now.zonedDateTimeISO", () => {
  test("zonedDateTimeISO() returns a ZonedDateTime", () => {
    const zdt = Temporal.Now.zonedDateTimeISO();
    expect(zdt.epochMilliseconds > 0).toBe(true);
    expect(typeof zdt.timeZoneId).toBe("string");
    expect(zdt.timeZoneId.length > 0).toBe(true);
    expect(zdt.year > 2020).toBe(true);
  });
});
