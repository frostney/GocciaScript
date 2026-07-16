/*---
description: Temporal.Duration.prototype.add
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.Duration.prototype.add", () => {
  test("add()", () => {
    const d1 = new Temporal.Duration(0, 0, 0, 5);
    const d2 = new Temporal.Duration(0, 0, 0, 10);
    const sum = d1.add(d2);
    expect(sum.days).toBe(15);
  });

  test("calendar units in either duration throw RangeError", () => {
    expect(() => new Temporal.Duration(1).add({ days: 1 })).toThrow(RangeError);
    expect(() => new Temporal.Duration(0, 0, 0, 1).add({ months: 1 })).toThrow(RangeError);
  });

  test("result is rebalanced to the larger of both largest units", () => {
    const sum = Temporal.Duration.from({ seconds: 90 }).add({ milliseconds: 5500 });
    expect(sum.seconds).toBe(95);
    expect(sum.milliseconds).toBe(500);
  });

  test("rebalanced component rounding past 2^53 seconds throws RangeError", () => {
    const one = Temporal.Duration.from({ nanoseconds: 9.007199254740991e24 });
    const two = Temporal.Duration.from({ microseconds: 1_000_000 });
    expect(() => one.add(two)).toThrow(RangeError);
  });

  test("rejects invalid duration strings", () => {
    expect(() => new Temporal.Duration(0, 0, 0, 1).add("not-a-duration")).toThrow(RangeError);
  });
});
