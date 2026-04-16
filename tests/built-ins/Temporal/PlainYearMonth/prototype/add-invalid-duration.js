/*---
description: Temporal.PlainYearMonth.prototype.add/subtract throw RangeError for invalid duration strings
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainYearMonth.prototype.add/subtract invalid duration", () => {
  test("add throws RangeError for non-duration string", () => {
    const ym = new Temporal.PlainYearMonth(2024, 6);
    expect(() => {
      ym.add("not-a-duration");
    }).toThrow(RangeError);
  });

  test("subtract throws RangeError for non-duration string", () => {
    const ym = new Temporal.PlainYearMonth(2024, 6);
    expect(() => {
      ym.subtract("not-a-duration");
    }).toThrow(RangeError);
  });
});
