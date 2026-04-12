/*---
description: Temporal.PlainDateTime.prototype.round
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainDateTime.prototype.round", () => {
  test("roundingMode floor", () => {
    const dt = new Temporal.PlainDateTime(2024, 3, 15, 13, 45, 30, 500);
    const rounded = dt.round({ smallestUnit: "second", roundingMode: "floor" });
    expect(rounded.second).toBe(30);
    expect(rounded.millisecond).toBe(0);
  });

  test("roundingMode ceil", () => {
    const dt = new Temporal.PlainDateTime(2024, 3, 15, 13, 45, 30, 1);
    const rounded = dt.round({ smallestUnit: "second", roundingMode: "ceil" });
    expect(rounded.second).toBe(31);
  });
});
