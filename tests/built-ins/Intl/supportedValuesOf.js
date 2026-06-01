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

  test("collation values exclude Collator-disallowed entries", () => {
    const result = Intl.supportedValuesOf("collation");
    expect(result.includes("big5han")).toBe(false);
    expect(result.includes("direct")).toBe(false);
    expect(result.includes("ducet")).toBe(false);
    expect(result.includes("gb2312")).toBe(false);
    expect(result.includes("reformed")).toBe(false);
    expect(result.includes("search")).toBe(false);
    expect(result.includes("standard")).toBe(false);
  });

  test("collation values are accepted by Intl.Collator", () => {
    const locales = ["en", "ar", "de", "es", "hi", "ko", "ln", "si", "sv", "zh"];

    for (const collation of Intl.supportedValuesOf("collation")) {
      let supported = false;
      for (const locale of locales) {
        const options = new Intl.Collator(locale, { collation }).resolvedOptions();
        if (options.collation === collation) {
          supported = true;
          break;
        }
      }
      expect(supported).toBe(true);
    }
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
