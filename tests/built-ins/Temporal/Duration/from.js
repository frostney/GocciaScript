/*---
description: Temporal.Duration.from
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.Duration.from", () => {
  test("from() with string", () => {
    const d = Temporal.Duration.from("P1Y2M3DT4H5M6S");
    expect(d.years).toBe(1);
    expect(d.months).toBe(2);
    expect(d.days).toBe(3);
    expect(d.hours).toBe(4);
    expect(d.minutes).toBe(5);
    expect(d.seconds).toBe(6);
  });

  test("from() with object", () => {
    const d = Temporal.Duration.from({ hours: 5, minutes: 30 });
    expect(d.hours).toBe(5);
    expect(d.minutes).toBe(30);
    expect(d.days).toBe(0);
  });

  test("from() preserves BigInt-backed duration components", () => {
    const i1 = Temporal.Instant.fromEpochMilliseconds(-8640000000000000);
    const i2 = Temporal.Instant.fromEpochMilliseconds(8640000000000000);
    const d = i1.until(i2, { largestUnit: "microseconds" });
    expect(Temporal.Duration.from(d).toString()).toBe("PT17280000000000S");
  });
});

describe.runIf(typeof Temporal !== "undefined")("Temporal.Duration.from components beyond Int64", () => {
  test("nanoseconds near the 2^53-second cap are preserved exactly", () => {
    const d = Temporal.Duration.from({ nanoseconds: 9.007199254740991e24 });
    expect(d.nanoseconds).toBe(9.007199254740991e24);
  });

  test("nanoseconds past the 2^53-second cap throw RangeError", () => {
    expect(() => Temporal.Duration.from({ nanoseconds: 1e30 })).toThrow(RangeError);
  });

  test("non-integral fields throw RangeError", () => {
    expect(() => Temporal.Duration.from({ seconds: 1.5 })).toThrow(RangeError);
  });
});
