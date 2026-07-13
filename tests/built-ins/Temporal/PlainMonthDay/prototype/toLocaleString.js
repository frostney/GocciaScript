/*---
description: Temporal.PlainMonthDay.prototype.toLocaleString
features: [Temporal, Intl]
---*/

const hasTemporalIntl = typeof Temporal !== "undefined" && typeof Intl !== "undefined";

describe.runIf(hasTemporalIntl)("Temporal.PlainMonthDay.prototype.toLocaleString", () => {
  test("rejects an ISO calendar that does not match the locale calendar", () => {
    const monthDay = new Temporal.PlainMonthDay(1, 1);
    expect(() => monthDay.toLocaleString("en-US-u-ca-gregory")).toThrow(
      RangeError
    );
  });
});
