/*---
description: Temporal.Duration.prototype.add/subtract throw RangeError for invalid duration strings
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.Duration.prototype.add/subtract invalid duration", () => {
  test("add throws RangeError for non-duration string", () => {
    const dur = new Temporal.Duration(0, 0, 0, 1);
    expect(() => {
      dur.add("not-a-duration");
    }).toThrow(RangeError);
  });

  test("subtract throws RangeError for non-duration string", () => {
    const dur = new Temporal.Duration(0, 0, 0, 1);
    expect(() => {
      dur.subtract("not-a-duration");
    }).toThrow(RangeError);
  });
});
