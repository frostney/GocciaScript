/*---
description: Intl.PluralRules constructor
features: [Intl]
---*/

const isIntl = typeof Intl !== "undefined";

describe.runIf(isIntl)("Intl.PluralRules constructor", () => {
  test("creates an instance with a locale argument", () => {
    const pr = new Intl.PluralRules("en");
    expect(pr).toBeInstanceOf(Intl.PluralRules);
  });

  test("creates an instance with no arguments", () => {
    const pr = new Intl.PluralRules();
    expect(pr).toBeInstanceOf(Intl.PluralRules);
  });

  test("select property is a function", () => {
    const pr = new Intl.PluralRules("en");
    expect(typeof pr.select).toBe("function");
  });

  test("resolvedOptions returns an object with locale", () => {
    const pr = new Intl.PluralRules("en");
    const options = pr.resolvedOptions();
    expect(typeof options.locale).toBe("string");
  });
});
