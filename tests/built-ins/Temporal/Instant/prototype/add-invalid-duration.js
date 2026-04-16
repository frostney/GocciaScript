/*---
description: Temporal.Instant.prototype.add/subtract throw RangeError for invalid duration strings
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.Instant.prototype.add/subtract invalid duration", () => {
  test("add throws RangeError for non-duration string", () => {
    const inst = Temporal.Instant.fromEpochMilliseconds(0);
    expect(() => {
      inst.add("not-a-duration");
    }).toThrow(RangeError);
  });

  test("subtract throws RangeError for non-duration string", () => {
    const inst = Temporal.Instant.fromEpochMilliseconds(0);
    expect(() => {
      inst.subtract("not-a-duration");
    }).toThrow(RangeError);
  });
});
