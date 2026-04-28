/*---
description: Temporal.Instant.prototype.add
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.Instant.prototype.add", () => {
  test("add() with time duration", () => {
    const instant = Temporal.Instant.fromEpochMilliseconds(0);
    const result = instant.add(new Temporal.Duration(0, 0, 0, 0, 1));
    expect(result.epochMilliseconds).toBe(3600000);
  });

  test("add() normalizes sub-millisecond carry", () => {
    const instant = Temporal.Instant.fromEpochNanoseconds(999999n);
    const result = instant.add(new Temporal.Duration(0, 0, 0, 0, 0, 0, 0, 0, 1));
    expect(result.epochNanoseconds).toBe(1000999n);
  });

  test("add() does not overflow 32-bit sub-millisecond accumulation", () => {
    const instant = Temporal.Instant.fromEpochMilliseconds(0);
    const result = instant.add(new Temporal.Duration(0, 0, 0, 0, 0, 0, 0, 0, 2147484));
    expect(result.epochNanoseconds).toBe(2147484000n);
  });

  test("add() accepts BigInt-backed microsecond durations", () => {
    const min = Temporal.Instant.fromEpochMilliseconds(-8640000000000000);
    const max = Temporal.Instant.fromEpochMilliseconds(8640000000000000);
    const duration = min.until(max, { largestUnit: "microseconds" });
    expect(min.add(duration).epochNanoseconds).toBe(max.epochNanoseconds);
  });

  test("add() throws on years", () => {
    const instant = Temporal.Instant.fromEpochMilliseconds(0);
    expect(() => {
      instant.add(new Temporal.Duration(1));
    }).toThrow(RangeError);
  });

  test("add() throws on months", () => {
    const instant = Temporal.Instant.fromEpochMilliseconds(0);
    expect(() => {
      instant.add(new Temporal.Duration(0, 1));
    }).toThrow(RangeError);
  });
});
