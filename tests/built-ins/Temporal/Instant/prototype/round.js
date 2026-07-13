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

  test("round at large epoch does not overflow", () => {
    // 9223372036855 > Int64_MAX / 1e6 — overflows Int64 when * 1e6
    const inst = Temporal.Instant.fromEpochMilliseconds(9223372036855);
    const rounded = inst.round("second");
    expect(rounded.epochMilliseconds).toBe(9223372037000);
  });

  test("round at extreme epoch boundary", () => {
    // Near max allowed: 1e8 days = 8.64e15 ms — would overflow Int64 if * 1e6
    const inst = Temporal.Instant.fromEpochMilliseconds(8640000000000000);
    const rounded = inst.round("hour");
    expect(rounded.epochMilliseconds).toBe(8640000000000000);
  });

  test("round rejects day as smallestUnit", () => {
    const inst = Temporal.Instant.fromEpochMilliseconds(0);
    expect(() => {
      inst.round({ smallestUnit: "day", roundingIncrement: 200000 });
    }).toThrow(RangeError);
  });

  test("negative instants use as-if-positive rounding directions", () => {
    const hour = new Temporal.Instant(-1000000000000000000n);
    expect(hour.round({ smallestUnit: "hour", roundingMode: "expand" }).epochNanoseconds)
      .toBe(-999997200000000000n);

    const second = new Temporal.Instant(-65261246399500000000n);
    expect(second.round({ smallestUnit: "second", roundingMode: "trunc" }).epochNanoseconds)
      .toBe(-65261246400000000000n);
  });
});
