/*---
description: Temporal.PlainTime.prototype.add/subtract throw RangeError for invalid duration strings
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainTime.prototype.add/subtract invalid duration", () => {
  test("add throws RangeError for non-duration string", () => {
    const time = new Temporal.PlainTime(12, 0, 0);
    expect(() => {
      time.add("not-a-duration");
    }).toThrow(RangeError);
  });

  test("subtract throws RangeError for non-duration string", () => {
    const time = new Temporal.PlainTime(12, 0, 0);
    expect(() => {
      time.subtract("not-a-duration");
    }).toThrow(RangeError);
  });
});
