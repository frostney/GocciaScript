/*---
description: Temporal.ZonedDateTime.from
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.ZonedDateTime.from", () => {
  test("from ISO string with timezone", () => {
    const zdt = Temporal.ZonedDateTime.from("2024-03-15T13:45:30+00:00[UTC]");
    expect(zdt.year).toBe(2024);
    expect(zdt.month).toBe(3);
    expect(zdt.day).toBe(15);
    expect(zdt.hour).toBe(13);
    expect(zdt.timeZoneId).toBe("UTC");
  });
});
