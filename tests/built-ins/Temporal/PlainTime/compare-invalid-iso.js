/*---
description: Temporal.PlainTime.compare throws RangeError for invalid ISO strings
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainTime.compare invalid ISO", () => {
  test("throws RangeError for non-time string", () => {
    expect(() => {
      Temporal.PlainTime.compare("not-a-time", new Temporal.PlainTime());
    }).toThrow(RangeError);
  });
});
