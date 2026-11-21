/*---
description: Temporal.PlainDate.prototype.toLocaleString
features: [Temporal, Intl.DateTimeFormat]
---*/

const isTemporal = typeof Temporal !== "undefined";
const hasFullICU = typeof Intl !== "undefined" &&
  new Intl.NumberFormat("en-US").format(NaN) === "NaN";

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

  test("names the month in the iso8601 calendar", () => {
    const result = new Temporal.PlainDate(2020, 1, 1).toLocaleString("en", {
      calendar: "iso8601",
      month: "long"
    });

    if (!hasFullICU) {
      expect(typeof result).toBe("string");
      return;
    }

    expect(result).toBe("January");
  });
});
