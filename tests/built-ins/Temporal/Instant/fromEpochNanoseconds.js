/*---
description: Temporal.Instant.fromEpochNanoseconds
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.Instant.fromEpochNanoseconds", () => {
  test("fromEpochNanoseconds()", () => {
    const instant = Temporal.Instant.fromEpochNanoseconds(1000000000n);
    expect(instant.epochMilliseconds).toBe(1000);
    expect(instant.epochNanoseconds).toBe(1000000000n);
  });

  test("requires BigInt argument", () => {
    expect(() => Temporal.Instant.fromEpochNanoseconds(1000000000)).toThrow(TypeError);
  });

  test("accepts epoch nanoseconds range boundaries", () => {
    const max = Temporal.Instant.fromEpochNanoseconds(8640000000000000000000n);
    const min = Temporal.Instant.fromEpochNanoseconds(-8640000000000000000000n);
    expect(max.epochNanoseconds).toBe(8640000000000000000000n);
    expect(min.epochNanoseconds).toBe(-8640000000000000000000n);
  });

  test("throws RangeError for out-of-range epoch nanoseconds", () => {
    expect(() => Temporal.Instant.fromEpochNanoseconds(8640000000000000000001n)).toThrow(RangeError);
    expect(() => Temporal.Instant.fromEpochNanoseconds(-8640000000000000000001n)).toThrow(RangeError);
  });
});
