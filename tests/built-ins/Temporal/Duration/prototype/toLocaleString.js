/*---
description: Temporal.Duration.prototype.toLocaleString
features: [Temporal, Intl.DurationFormat]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.Duration.prototype.toLocaleString", () => {
  test("locales uses CanonicalizeLocaleList error ordering", () => {
    const duration = new Temporal.Duration(0, 0, 0, 0, 1);

    expect(() => duration.toLocaleString([1])).toThrow(TypeError);

    const sparseLocales = [];
    sparseLocales[1] = "not_valid";
    expect(() => duration.toLocaleString(sparseLocales)).toThrow(RangeError);
  });

  test("options are coerced with ToObject", () => {
    const duration = new Temporal.Duration(0, 0, 0, 0, 1);

    expect(() => duration.toLocaleString("en-US", null)).toThrow(TypeError);

    Boolean.prototype.style = "narrow";
    try {
      expect(duration.toLocaleString("en-US", true)).toBe(
        duration.toLocaleString("en-US", new Boolean(true))
      );
    } finally {
      delete Boolean.prototype.style;
    }
  });
});
