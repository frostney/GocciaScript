/*---
description: Intl.Locale constructor
features: [Intl]
---*/

const isIntl = typeof Intl !== "undefined";

describe.runIf(isIntl && typeof Intl.Locale !== "undefined")("Intl.Locale constructor", () => {
  test("creates an instance from a locale string", () => {
    const locale = new Intl.Locale("en-US");
    expect(locale).toBeInstanceOf(Intl.Locale);
  });

  test("language property returns the language subtag", () => {
    const locale = new Intl.Locale("en-US");
    expect(locale.language).toBe("en");
  });

  test("region property returns the region subtag", () => {
    const locale = new Intl.Locale("en-US");
    expect(locale.region).toBe("US");
  });

  test("toString returns the canonical locale string", () => {
    const locale = new Intl.Locale("en-US");
    expect(locale.toString()).toBe("en-US");
  });

  test("script property returns the script subtag when present", () => {
    const locale = new Intl.Locale("zh-Hant-TW");
    expect(locale.script).toBe("Hant");
  });

  test("throws TypeError when called without new", () => {
    expect(() => Intl.Locale("en-US")).toThrow(TypeError);
  });
});
