/*---
description: Intl.getCanonicalLocales
features: [Intl]
---*/

const isIntl = typeof Intl !== "undefined";

describe.runIf(isIntl)("Intl.getCanonicalLocales", () => {
  test("canonicalises lowercase locale tag to BCP 47", () => {
    const result = Intl.getCanonicalLocales("en-us");
    expect(result[0]).toBe("en-US");
  });

  test("preserves already-canonical locale tag", () => {
    const result = Intl.getCanonicalLocales("de-DE");
    expect(result[0]).toBe("de-DE");
  });

  test("returns an array", () => {
    const result = Intl.getCanonicalLocales("en-US");
    expect(Array.isArray(result)).toBe(true);
  });

  test("accepts an array of locales", () => {
    const result = Intl.getCanonicalLocales(["en-us", "fr-fr"]);
    expect(result.length).toBe(2);
    expect(result[0]).toBe("en-US");
    expect(result[1]).toBe("fr-FR");
  });

  test("canonicalises grandfathered tags through CLDR aliases", () => {
    expect(Intl.getCanonicalLocales("zh-guoyu")[0]).toBe("zh");
  });

  test("throws RangeError for structurally invalid locale tag", () => {
    expect(() => Intl.getCanonicalLocales("!")).toThrow(RangeError);
  });
});
