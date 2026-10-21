/*---
description: Temporal.ZonedDateTime.prototype.startOfDay
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.ZonedDateTime.prototype.startOfDay", () => {
  test("startOfDay", () => {
    const epochNs = 1710510330000n * 1000000n;
    const zdt = new Temporal.ZonedDateTime(epochNs, "UTC");
    const sod = zdt.startOfDay();
    expect(sod.hour).toBe(0);
    expect(sod.minute).toBe(0);
    expect(sod.second).toBe(0);
    expect(sod.year).toBe(2024);
    expect(sod.month).toBe(3);
    expect(sod.day).toBe(15);
  });

  test("startOfDay skips a midnight daylight-saving gap after 2037", () => {
    const zdt = Temporal.ZonedDateTime.from("2040-09-02T12:00[America/Santiago]");
    expect(zdt.startOfDay().toString()).toBe("2040-09-02T01:00:00-03:00[America/Santiago]");
  });

  test("startOfDay is midnight on a southern daylight-saving day after 2037", () => {
    const zdt = Temporal.ZonedDateTime.from("2040-10-07T12:00[Australia/Sydney]");
    expect(zdt.startOfDay().toString()).toBe("2040-10-07T00:00:00+10:00[Australia/Sydney]");
  });
});
