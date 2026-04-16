/*---
description: Temporal.PlainDate.prototype.add/subtract throw RangeError for invalid duration strings
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainDate.prototype.add/subtract invalid duration", () => {
  test("add throws RangeError for non-duration string", () => {
    const date = new Temporal.PlainDate(2024, 1, 1);
    expect(() => {
      date.add("not-a-duration");
    }).toThrow(RangeError);
  });

  test("subtract throws RangeError for non-duration string", () => {
    const date = new Temporal.PlainDate(2024, 1, 1);
    expect(() => {
      date.subtract("not-a-duration");
    }).toThrow(RangeError);
  });
});
