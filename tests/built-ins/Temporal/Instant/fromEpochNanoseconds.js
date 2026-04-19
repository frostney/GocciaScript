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
});
