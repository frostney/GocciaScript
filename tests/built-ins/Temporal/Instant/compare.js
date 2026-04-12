/*---
description: Temporal.Instant.compare
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.Instant.compare", () => {
  test("compare()", () => {
    const i1 = Temporal.Instant.fromEpochMilliseconds(1000);
    const i2 = Temporal.Instant.fromEpochMilliseconds(2000);
    expect(Temporal.Instant.compare(i1, i2)).toBe(-1);
    expect(Temporal.Instant.compare(i2, i1)).toBe(1);
    expect(Temporal.Instant.compare(i1, i1)).toBe(0);
  });
});
