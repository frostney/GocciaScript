/*---
description: Temporal.PlainDateTime constructor
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainDateTime constructor", () => {
  test("constructor", () => {
    const dt = new Temporal.PlainDateTime(2024, 3, 15, 10, 30, 45);
    expect(dt.year).toBe(2024);
    expect(dt.month).toBe(3);
    expect(dt.day).toBe(15);
    expect(dt.hour).toBe(10);
    expect(dt.minute).toBe(30);
    expect(dt.second).toBe(45);
  });

  test("constructor with defaults", () => {
    const dt = new Temporal.PlainDateTime(2024, 1, 1);
    expect(dt.hour).toBe(0);
    expect(dt.minute).toBe(0);
    expect(dt.second).toBe(0);
    expect(dt.millisecond).toBe(0);
  });

  test("calendarId", () => {
    const dt = new Temporal.PlainDateTime(2024, 1, 1);
    expect(dt.calendarId).toBe("iso8601");
  });

  test("date getters", () => {
    const dt = new Temporal.PlainDateTime(2024, 3, 15, 10, 30);
    expect(dt.dayOfWeek).toBe(5);
    expect(dt.daysInMonth).toBe(31);
    expect(dt.daysInYear).toBe(366);
    expect(dt.inLeapYear).toBe(true);
  });
});
