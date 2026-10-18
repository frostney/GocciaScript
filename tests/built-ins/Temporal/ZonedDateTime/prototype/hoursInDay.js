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

  test("daylight-saving days after 2037 are 23 and 25 hours long", () => {
    const hours = (text) => Temporal.ZonedDateTime.from(text).hoursInDay;
    expect(hours("2040-03-11T12:00[America/Los_Angeles]")).toBe(23);
    expect(hours("2040-11-04T12:00[America/Los_Angeles]")).toBe(25);
    expect(hours("2040-10-07T12:00[Australia/Sydney]")).toBe(23);
    expect(hours("2040-04-01T12:00[Australia/Sydney]")).toBe(25);
    expect(hours("2040-10-28T12:00[Europe/Dublin]")).toBe(25);
  });
});
