/*---
description: Temporal.PlainTime.prototype.add
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainTime.prototype.add", () => {
  test("add()", () => {
    const t = new Temporal.PlainTime(10, 30, 0);
    const result = t.add(new Temporal.Duration(0, 0, 0, 0, 2, 45));
    expect(result.hour).toBe(13);
    expect(result.minute).toBe(15);
  });

  test("add() wraps around midnight", () => {
    const t = new Temporal.PlainTime(23, 0, 0);
    const result = t.add(new Temporal.Duration(0, 0, 0, 0, 2));
    expect(result.hour).toBe(1);
  });

  test("hours beyond Int32 wrap correctly into the clock", () => {
    const time = Temporal.PlainTime.from({ hour: 0 }).add({
      hours: 3000000001,
    });
    expect(time.hour).toBe(1);
  });

  test("rejects invalid duration strings", () => {
    expect(() => new Temporal.PlainTime(12).add("not-a-duration")).toThrow(RangeError);
  });
});
