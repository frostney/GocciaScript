/*---
description: Temporal.Instant.prototype.round
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.Instant.prototype.round", () => {
  test("round()", () => {
    // 1500ms = 1.5 seconds, rounds to 2 seconds
    const instant = Temporal.Instant.fromEpochMilliseconds(1500);
    const rounded = instant.round("second");
    expect(rounded.epochMilliseconds).toBe(2000);
  });

  test("roundingMode trunc", () => {
    const inst = Temporal.Instant.fromEpochMilliseconds(1710510330999);
    const rounded = inst.round({ smallestUnit: "second", roundingMode: "trunc" });
    expect(rounded.epochMilliseconds).toBe(1710510330000);
  });
});
