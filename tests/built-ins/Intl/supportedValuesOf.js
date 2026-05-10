/*---
description: Intl.supportedValuesOf
features: [Intl]
---*/

const isIntl = typeof Intl !== "undefined";

describe.runIf(isIntl)("Intl.supportedValuesOf", () => {
  test("returns an array for 'calendar'", () => {
    const result = Intl.supportedValuesOf("calendar");
    expect(Array.isArray(result)).toBe(true);
    expect(result.length > 0).toBe(true);
  });

  test("returns an array for 'collation'", () => {
    const result = Intl.supportedValuesOf("collation");
    expect(Array.isArray(result)).toBe(true);
  });

  test("returns an array for 'currency'", () => {
    const result = Intl.supportedValuesOf("currency");
    expect(Array.isArray(result)).toBe(true);
    expect(result.length > 0).toBe(true);
  });

  test("returns an array for 'numberingSystem'", () => {
    const result = Intl.supportedValuesOf("numberingSystem");
    expect(Array.isArray(result)).toBe(true);
    expect(result.length > 0).toBe(true);
  });

  test("returns an array for 'timeZone'", () => {
    const result = Intl.supportedValuesOf("timeZone");
    expect(Array.isArray(result)).toBe(true);
    expect(result.length > 0).toBe(true);
  });

  test("returns an array for 'unit'", () => {
    const result = Intl.supportedValuesOf("unit");
    expect(Array.isArray(result)).toBe(true);
    expect(result.length > 0).toBe(true);
  });

  test("throws RangeError for invalid key", () => {
    expect(() => Intl.supportedValuesOf("invalidKey")).toThrow(RangeError);
  });
});
