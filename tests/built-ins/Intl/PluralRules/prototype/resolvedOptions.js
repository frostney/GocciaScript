/*---
description: Intl.PluralRules.prototype.resolvedOptions
features: [Intl]
---*/

describe("Intl.PluralRules.prototype.resolvedOptions", () => {
  test("default includes all spec-required fields", () => {
    const opts = new Intl.PluralRules("en-US").resolvedOptions();
    expect(opts.locale).toBe("en-US");
    expect(opts.type).toBe("cardinal");
    expect(opts.minimumIntegerDigits).toBe(1);
    expect(opts.minimumFractionDigits).toBe(0);
    expect(opts.maximumFractionDigits).toBe(3);
  });

  test("pluralCategories is an array containing other", () => {
    const opts = new Intl.PluralRules("en-US").resolvedOptions();
    expect(Array.isArray(opts.pluralCategories)).toBe(true);
    expect(opts.pluralCategories.includes("other")).toBe(true);
  });

  test("ordinal type is preserved", () => {
    const opts = new Intl.PluralRules("en-US", {
      type: "ordinal",
    }).resolvedOptions();
    expect(opts.type).toBe("ordinal");
  });

  test("significantDigits rounding omits fraction digit fields", () => {
    const opts = new Intl.PluralRules("en-US", {
      minimumSignificantDigits: 1,
      maximumSignificantDigits: 5,
    }).resolvedOptions();
    expect(opts.minimumSignificantDigits).toBe(1);
    expect(opts.maximumSignificantDigits).toBe(5);
    expect(opts.minimumFractionDigits).toBe(undefined);
    expect(opts.maximumFractionDigits).toBe(undefined);
  });
});
