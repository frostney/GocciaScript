/*---
description: Temporal.Duration magnitude validation (TC39 §7.5.22)
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.Duration magnitude validation", () => {
  // Calendar units: absolute value must be < 2^32 (4294967296)

  test("years at 2^32 throws RangeError", () => {
    expect(() => new Temporal.Duration(4294967296)).toThrow(RangeError);
  });

  test("years at -(2^32) throws RangeError", () => {
    expect(() => new Temporal.Duration(-4294967296)).toThrow(RangeError);
  });

  test("months at 2^32 throws RangeError", () => {
    expect(() => new Temporal.Duration(0, 4294967296)).toThrow(RangeError);
  });

  test("weeks at 2^32 throws RangeError", () => {
    expect(() => new Temporal.Duration(0, 0, 4294967296)).toThrow(RangeError);
  });

  test("years just below 2^32 is valid", () => {
    const d = new Temporal.Duration(4294967295);
    expect(d.years).toBe(4294967295);
  });

  test("months just below 2^32 is valid", () => {
    const d = new Temporal.Duration(0, 4294967295);
    expect(d.months).toBe(4294967295);
  });

  test("weeks just below 2^32 is valid", () => {
    const d = new Temporal.Duration(0, 0, 4294967295);
    expect(d.weeks).toBe(4294967295);
  });

  test("negative years just above -(2^32) is valid", () => {
    const d = new Temporal.Duration(-4294967295);
    expect(d.years).toBe(-4294967295);
  });

  // Normalized time: |days*86400 + hours*3600 + minutes*60 + seconds + ms*1e-3 + us*1e-6 + ns*1e-9| < 2^53

  test("days producing normalized seconds >= 2^53 throws RangeError", () => {
    // 2^53 / 86400 ≈ 1.0425e11, so 104251000000 days exceeds the limit
    expect(() => new Temporal.Duration(0, 0, 0, 104251000000)).toThrow(RangeError);
  });

  test("hours producing normalized seconds >= 2^53 throws RangeError", () => {
    // 2^53 / 3600 ≈ 2.502e12
    expect(() => new Temporal.Duration(0, 0, 0, 0, 2502000000000)).toThrow(RangeError);
  });

  test("seconds at 2^53 throws RangeError", () => {
    expect(() => new Temporal.Duration(0, 0, 0, 0, 0, 0, 9007199254740992)).toThrow(RangeError);
  });

  test("moderate day count is valid", () => {
    // 1000 days = 86,400,000 normalized seconds, well under 2^53
    const d = new Temporal.Duration(0, 0, 0, 1000);
    expect(d.days).toBe(1000);
  });

  // Validation via Temporal.Duration.from (object form -> DurationFromObject)

  test("from() with object with years >= 2^32 throws RangeError", () => {
    expect(() => Temporal.Duration.from({ years: 4294967296 })).toThrow(RangeError);
  });

  test("from() with object with valid years succeeds", () => {
    const d = Temporal.Duration.from({ years: 100 });
    expect(d.years).toBe(100);
  });
});
