/*---
description: Temporal.Duration.compare
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.Duration.compare", () => {
  test("compare()", () => {
    const d1 = new Temporal.Duration(0, 0, 0, 0, 1);
    const d2 = new Temporal.Duration(0, 0, 0, 0, 2);
    expect(Temporal.Duration.compare(d1, d2)).toBe(-1);
    expect(Temporal.Duration.compare(d2, d1)).toBe(1);
    expect(Temporal.Duration.compare(d1, d1)).toBe(0);
  });

  test("compare() handles BigInt-backed exact-time durations", () => {
    const i1 = Temporal.Instant.fromEpochMilliseconds(-8640000000000000);
    const i2 = Temporal.Instant.fromEpochMilliseconds(8640000000000000);
    const large = i1.until(i2, { largestUnit: "microseconds" });
    const larger = large.add({ seconds: 1 });
    expect(Temporal.Duration.compare(large, larger)).toBe(-1);
    expect(Temporal.Duration.compare(larger, large)).toBe(1);
    expect(Temporal.Duration.compare(large, large)).toBe(0);
  });
});
