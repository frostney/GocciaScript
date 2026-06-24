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

  test("collation values are canonical data values", () => {
    const result = Intl.supportedValuesOf("collation");

    for (const value of result) {
      expect(typeof value).toBe("string");
      expect(value.length > 0).toBe(true);
    }
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

  test("timeZone values include accepted primary identifiers", () => {
    const result = Intl.supportedValuesOf("timeZone");
    const primaryIdentifiers = [
      "America/Detroit",
      "America/Indiana/Indianapolis",
      "Europe/Istanbul",
      "Pacific/Chatham",
    ];

    for (const timeZone of primaryIdentifiers) {
      expect(result.includes(timeZone)).toBe(true);
      expect(new Intl.DateTimeFormat("en", { timeZone }).resolvedOptions().timeZone)
        .toBe(timeZone);
    }
  });

  test("timeZone values exclude known link aliases", () => {
    const result = Intl.supportedValuesOf("timeZone");

    expect(result.includes("Asia/Istanbul")).toBe(false);
    expect(result.includes("NZ-CHAT")).toBe(false);
    expect(result.includes("US/Michigan")).toBe(false);
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
