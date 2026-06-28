/*---
description: Temporal.ZonedDateTime constructor
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.ZonedDateTime constructor", () => {
  test("constructor from epoch nanoseconds", () => {
    // 2024-03-15T13:45:30Z = epoch ms 1710510330000
    const epochNs = 1710510330000n * 1000000n;
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
    const zdt = new Temporal.ZonedDateTime(0n, "UTC");
    expect(zdt.calendarId).toBe("iso8601");
  });

  test("canonical named time zones are accepted and preserved", () => {
    const zones = [
      "Africa/Cairo",
      "America/Los_Angeles",
      "Asia/Tokyo",
      "Europe/London",
      "Pacific/Auckland",
      "UTC",
    ];
    for (const id of zones) {
      expect(new Temporal.ZonedDateTime(0n, id).timeZoneId).toBe(id);
    }
    expect(() => new Temporal.ZonedDateTime(0n, "Not/AZone")).toThrow(RangeError);
  });

  test("constructor stores non-ISO calendarId", () => {
    const zdt = new Temporal.ZonedDateTime(0n, "Europe/Madrid", "gregory");
    expect(zdt.calendarId).toBe("gregory");
  });

  test("constructor canonicalizes calendar aliases", () => {
    expect(new Temporal.ZonedDateTime(0n, "UTC", "islamicc").calendarId).toBe("islamic-civil");
    expect(new Temporal.ZonedDateTime(0n, "UTC", "ethiopic-amete-alem").calendarId).toBe("ethioaa");
  });

  test("epochMilliseconds", () => {
    const zdt = new Temporal.ZonedDateTime(1710510330000000000n, "UTC");
    expect(zdt.epochMilliseconds).toBe(1710510330000);
  });

  test("epochNanoseconds returns BigInt", () => {
    const zdt = new Temporal.ZonedDateTime(1710510330000000000n, "UTC");
    expect(zdt.epochNanoseconds).toBe(1710510330000000000n);
    expect(typeof zdt.epochNanoseconds).toBe("bigint");
  });

  test("offset for UTC", () => {
    const zdt = new Temporal.ZonedDateTime(0n, "UTC");
    expect(zdt.offset).toBe("+00:00");
  });

  test("offsetNanoseconds for UTC", () => {
    const zdt = new Temporal.ZonedDateTime(0n, "UTC");
    expect(zdt.offsetNanoseconds).toBe(0);
  });

  test("requires BigInt for epochNanoseconds", () => {
    expect(() => new Temporal.ZonedDateTime(0, "UTC")).toThrow(TypeError);
  });
});
