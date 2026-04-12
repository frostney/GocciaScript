/*---
description: Temporal.ZonedDateTime constructor
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.ZonedDateTime constructor", () => {
  test("constructor from epoch nanoseconds", () => {
    // 2024-03-15T13:45:30Z = epoch ms 1710510330000
    const epochNs = 1710510330000 * 1000000;
    const zdt = new Temporal.ZonedDateTime(epochNs, "UTC");
    expect(zdt.timeZoneId).toBe("UTC");
    expect(zdt.year).toBe(2024);
    expect(zdt.month).toBe(3);
    expect(zdt.day).toBe(15);
    expect(zdt.hour).toBe(13);
    expect(zdt.minute).toBe(45);
    expect(zdt.second).toBe(30);
  });

  test("calendarId is iso8601", () => {
    const zdt = new Temporal.ZonedDateTime(0, "UTC");
    expect(zdt.calendarId).toBe("iso8601");
  });

  test("epochMilliseconds", () => {
    const zdt = new Temporal.ZonedDateTime(1710510330000000000, "UTC");
    expect(zdt.epochMilliseconds).toBe(1710510330000);
  });

  test("epochNanoseconds", () => {
    const zdt = new Temporal.ZonedDateTime(1710510330000000000, "UTC");
    expect(zdt.epochNanoseconds).toBe(1710510330000000000);
  });

  test("offset for UTC", () => {
    const zdt = new Temporal.ZonedDateTime(0, "UTC");
    expect(zdt.offset).toBe("+00:00");
  });

  test("offsetNanoseconds for UTC", () => {
    const zdt = new Temporal.ZonedDateTime(0, "UTC");
    expect(zdt.offsetNanoseconds).toBe(0);
  });
});
