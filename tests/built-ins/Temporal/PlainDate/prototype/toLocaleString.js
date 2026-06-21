/*---
description: Temporal.PlainDate.prototype.toLocaleString
features: [Temporal, Intl.DateTimeFormat]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainDate.prototype.toLocaleString", () => {
  test("locales uses CanonicalizeLocaleList error ordering", () => {
    const date = new Temporal.PlainDate(2024, 3, 15);

    expect(() => date.toLocaleString([1])).toThrow(TypeError);

    const sparseLocales = [];
    sparseLocales[1] = "not_valid";
    expect(() => date.toLocaleString(sparseLocales)).toThrow(RangeError);
  });

  test("options are coerced with ToObject", () => {
    const date = new Temporal.PlainDate(2024, 3, 15);

    expect(() => date.toLocaleString("en-US", null)).toThrow(TypeError);

    Boolean.prototype.year = "2-digit";
    try {
      expect(date.toLocaleString("en-US", true)).toBe(
        date.toLocaleString("en-US", new Boolean(true))
      );
    } finally {
      delete Boolean.prototype.year;
    }
  });
});
