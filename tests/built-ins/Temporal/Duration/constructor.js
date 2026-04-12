/*---
description: Temporal.Duration constructor
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.Duration constructor", () => {
  test("constructor with all zeros", () => {
    const d = new Temporal.Duration();
    expect(d.years).toBe(0);
    expect(d.months).toBe(0);
    expect(d.weeks).toBe(0);
    expect(d.days).toBe(0);
    expect(d.hours).toBe(0);
    expect(d.minutes).toBe(0);
    expect(d.seconds).toBe(0);
    expect(d.milliseconds).toBe(0);
    expect(d.microseconds).toBe(0);
    expect(d.nanoseconds).toBe(0);
  });

  test("constructor with components", () => {
    const d = new Temporal.Duration(1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
    expect(d.years).toBe(1);
    expect(d.months).toBe(2);
    expect(d.weeks).toBe(3);
    expect(d.days).toBe(4);
    expect(d.hours).toBe(5);
    expect(d.minutes).toBe(6);
    expect(d.seconds).toBe(7);
    expect(d.milliseconds).toBe(8);
    expect(d.microseconds).toBe(9);
    expect(d.nanoseconds).toBe(10);
  });
});
