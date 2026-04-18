/*---
description: Temporal.ZonedDateTime.prototype calendar getters
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.ZonedDateTime.prototype calendar getters", () => {
  test("calendar getters", () => {
    const epochNs = 1710510330000n * 1000000n;
    const zdt = new Temporal.ZonedDateTime(epochNs, "UTC");
    expect(zdt.dayOfWeek).toBe(5); // Friday
    expect(zdt.dayOfYear).toBe(75);
    expect(zdt.daysInWeek).toBe(7);
    expect(zdt.daysInMonth).toBe(31);
    expect(zdt.daysInYear).toBe(366);
    expect(zdt.monthsInYear).toBe(12);
    expect(zdt.inLeapYear).toBe(true);
    expect(zdt.monthCode).toBe("M03");
  });
});
