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

  test("throws RangeError for month 13", () => {
    expect(() => {
      Temporal.PlainMonthDay.from({ month: 13, day: 1 });
    }).toThrow(RangeError);
  });
});
