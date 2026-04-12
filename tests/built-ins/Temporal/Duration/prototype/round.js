/*---
description: Temporal.Duration.prototype.round
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.Duration.prototype.round", () => {
  test("round time units", () => {
    const d = new Temporal.Duration(0, 0, 0, 0, 25, 30);
    const rounded = d.round({ largestUnit: "day" });
    expect(rounded.days).toBe(1);
    expect(rounded.hours).toBe(1);
    expect(rounded.minutes).toBe(30);
  });

  test("round to hours", () => {
    const d = new Temporal.Duration(0, 0, 0, 0, 1, 45);
    const rounded = d.round({ smallestUnit: "hour", roundingMode: "halfExpand" });
    expect(rounded.hours).toBe(2);
  });

  test("round floor", () => {
    const d = new Temporal.Duration(0, 0, 0, 0, 1, 59);
    const rounded = d.round({ smallestUnit: "hour", roundingMode: "floor" });
    expect(rounded.hours).toBe(1);
  });
});
