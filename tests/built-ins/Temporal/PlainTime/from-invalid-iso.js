/*---
description: Temporal.PlainTime.from throws RangeError for invalid ISO strings
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainTime.from invalid ISO", () => {
  test("throws RangeError for non-time string", () => {
    expect(() => {
      Temporal.PlainTime.from("not-a-time");
    }).toThrow(RangeError);
  });
});
