/*---
description: Temporal.PlainMonthDay.from throws RangeError for out-of-range values
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainMonthDay.from invalid values", () => {
  test("throws RangeError for month 0", () => {
    expect(() => {
      Temporal.PlainMonthDay.from({ month: 0, day: 1 });
    }).toThrow(RangeError);
  });

  test("constrains month 13 by default", () => {
    const result = Temporal.PlainMonthDay.from({ month: 13, day: 1 });
    expect(result.monthCode).toBe("M12");
    expect(result.day).toBe(1);
  });
});
