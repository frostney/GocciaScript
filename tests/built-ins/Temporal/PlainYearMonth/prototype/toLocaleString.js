/*---
description: Temporal.PlainYearMonth.prototype.toLocaleString
features: [Temporal, Intl]
---*/

const hasTemporalIntl = typeof Temporal !== "undefined" && typeof Intl !== "undefined";

describe.runIf(hasTemporalIntl)("Temporal.PlainYearMonth.prototype.toLocaleString", () => {
  test("rejects an ISO calendar that does not match the locale calendar", () => {
    const yearMonth = new Temporal.PlainYearMonth(2024, 1);
    expect(() => yearMonth.toLocaleString("en-US-u-ca-gregory")).toThrow(
      RangeError
    );
  });
});
