/*---
description: Intl.Locale constructor
features: [Intl]
---*/

const isIntl = typeof Intl !== "undefined";
const hasFullICU = isIntl && new Intl.NumberFormat("en-US").format(NaN) === "NaN";

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

  test("parses Unicode extension keywords into properties", () => {
    const locale = new Intl.Locale("en-US-u-nu-latn-ca-gregory-co-phonebk-hc-h24-kf-upper-kn");
    expect(locale.calendar).toBe("gregory");
    expect(locale.numberingSystem).toBe("latn");
    expect(locale.collation).toBe("phonebk");
    expect(locale.hourCycle).toBe("h24");
    expect(locale.caseFirst).toBe("upper");
    expect(locale.numeric).toBe(true);
    expect(locale.baseName).toBe("en-US");
  });

  test("constructor options override Unicode extension keywords", () => {
    const locale = new Intl.Locale("en-u-kf-upper-kn", {
      caseFirst: "lower",
      numeric: false,
    });
    expect(locale.caseFirst).toBe("lower");
    expect(locale.numeric).toBe(false);
    expect(locale.toString()).toBe("en-u-kf-lower-kn-false");
  });

  test("constructor language and region options update the canonical locale", () => {
    const locale = new Intl.Locale("en", { language: "ar", region: "EG" });
    expect(locale.toString()).toBe("ar-EG");
    expect(locale.baseName).toBe("ar-EG");
  });

  test("numeric Unicode extension canonicalizes true to a keyword", () => {
    const locale = new Intl.Locale("en-u-kn-true");
    expect(locale.numeric).toBe(true);
    expect(locale.toString()).toBe("en-u-kn");
  });

  test("preserves empty caseFirst Unicode extension keyword", () => {
    const locale = new Intl.Locale("de-u-kf");
    expect(locale.toString()).toBe("de-u-kf");
    if (hasFullICU) {
      expect(locale.maximize().toString()).toBe("de-Latn-DE-u-kf");
    } else {
      expect(locale.maximize().toString()).toBe("de-u-kf");
    }
  });

  test("baseName strips all extensions and private use subtags", () => {
    const locale = new Intl.Locale("en-US-a-foo-u-ca-gregory-x-bar");
    expect(locale.baseName).toBe("en-US");
    expect(locale.calendar).toBe("gregory");
  });

  test("script property returns the script subtag when present", () => {
    const locale = new Intl.Locale("zh-Hant-TW");
    expect(locale.script).toBe("Hant");
  });

  test("throws TypeError when called without new", () => {
    expect(() => Intl.Locale("en-US")).toThrow(TypeError);
  });
});
