/*---
description: Temporal.Now methods
features: [Temporal]
---*/

describe("Temporal.Now", () => {

  test("instant() returns an Instant", () => {
    const instant = Temporal.Now.instant();
    expect(instant.epochMilliseconds > 0).toBe(true);
    const str = instant.toString();
    expect(str.length > 0).toBe(true);
  });

  test("plainDateISO() returns a PlainDate", () => {
    const date = Temporal.Now.plainDateISO();
    expect(date.year > 2020).toBe(true);
    expect(date.month >= 1).toBe(true);
    expect(date.month <= 12).toBe(true);
    expect(date.day >= 1).toBe(true);
    expect(date.day <= 31).toBe(true);
  });

  test("plainTimeISO() returns a PlainTime", () => {
    const time = Temporal.Now.plainTimeISO();
    expect(time.hour >= 0).toBe(true);
    expect(time.hour <= 23).toBe(true);
    expect(time.minute >= 0).toBe(true);
    expect(time.minute <= 59).toBe(true);
  });

  test("plainDateTimeISO() returns a PlainDateTime", () => {
    const dt = Temporal.Now.plainDateTimeISO();
    expect(dt.year > 2020).toBe(true);
    expect(dt.hour >= 0).toBe(true);
    expect(dt.hour <= 23).toBe(true);
    const str = dt.toString();
    expect(str.length > 0).toBe(true);
  });

});
