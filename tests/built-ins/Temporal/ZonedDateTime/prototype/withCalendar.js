/*---
description: Temporal.ZonedDateTime.prototype.withCalendar
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.ZonedDateTime.prototype.withCalendar", () => {
  test("withCalendar preserves instant and time zone", () => {
    const zdt = new Temporal.ZonedDateTime(0n, "UTC");
    const result = zdt.withCalendar("gregory");
    expect(result.calendarId).toBe("gregory");
    expect(result.timeZoneId).toBe("UTC");
    expect(result.epochNanoseconds).toBe(0n);
  });

  test("withCalendar canonicalizes calendar aliases", () => {
    const zdt = new Temporal.ZonedDateTime(0n, "UTC");
    expect(zdt.withCalendar("islamicc").calendarId).toBe("islamic-civil");
  });
});
