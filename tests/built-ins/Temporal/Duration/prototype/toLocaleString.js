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

  // toLocaleString formats with Intl.DurationFormat, whose constructor
  // (ECMA-402 13.1.1) takes options through ResolveOptions without
  // coerce-options: GetOptionsObject (9.2.11) accepts only undefined or an
  // object, and throws a TypeError for anything else.
  test("options must be an object or undefined", () => {
    const duration = new Temporal.Duration(0, 0, 0, 0, 1);

    expect(() => duration.toLocaleString("en-US", null)).toThrow(TypeError);
    expect(() => duration.toLocaleString("en-US", true)).toThrow(TypeError);
    expect(duration.toLocaleString("en-US", new Boolean(true))).toBe(duration.toLocaleString("en-US"));
  });
});
