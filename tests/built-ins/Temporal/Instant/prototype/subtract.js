/*---
description: Temporal.Instant.prototype.subtract
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.Instant.prototype.subtract", () => {
  test("subtract() with time duration", () => {
    const instant = Temporal.Instant.fromEpochMilliseconds(3600000);
    const result = instant.subtract(new Temporal.Duration(0, 0, 0, 0, 1));
    expect(result.epochMilliseconds).toBe(0);
  });

  test("subtract() normalizes sub-millisecond borrow", () => {
    const instant = Temporal.Instant.fromEpochNanoseconds(1n);
    const result = instant.subtract(new Temporal.Duration(0, 0, 0, 0, 0, 0, 0, 0, 1));
    expect(result.epochNanoseconds).toBe(-999n);
  });

  test("subtract() does not overflow 32-bit sub-millisecond accumulation", () => {
    const instant = Temporal.Instant.fromEpochMilliseconds(0);
    const result = instant.subtract(new Temporal.Duration(0, 0, 0, 0, 0, 0, 0, 0, 2147484));
    expect(result.epochNanoseconds).toBe(-2147484000n);
  });

  test("subtract() accepts BigInt-backed microsecond durations", () => {
    const min = Temporal.Instant.fromEpochMilliseconds(-8640000000000000);
    const max = Temporal.Instant.fromEpochMilliseconds(8640000000000000);
    const duration = min.until(max, { largestUnit: "microseconds" });
    expect(max.subtract(duration).epochNanoseconds).toBe(min.epochNanoseconds);
  });

  test("subtract() throws on weeks", () => {
    const instant = Temporal.Instant.fromEpochMilliseconds(0);
    expect(() => {
      instant.subtract(new Temporal.Duration(0, 0, 1));
    }).toThrow(RangeError);
  });
});
