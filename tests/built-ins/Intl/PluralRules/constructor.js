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

describe.runIf(isIntl)("Intl.PluralRules non-finite digit options", () => {
  test("minimumIntegerDigits Infinity throws RangeError", () => {
    expect(() => new Intl.PluralRules("en", { minimumIntegerDigits: Infinity })).toThrow(RangeError);
  });

  test("maximumSignificantDigits NaN throws RangeError", () => {
    expect(() => new Intl.PluralRules("en", { maximumSignificantDigits: NaN })).toThrow(RangeError);
  });
});

describe.runIf(isIntl)("Intl.PluralRules finite digit range validation", () => {
  test("minimumIntegerDigits above 21 throws RangeError", () => {
    expect(() => new Intl.PluralRules("en", { minimumIntegerDigits: 22 })).toThrow(RangeError);
  });

  test("maximumFractionDigits above 100 throws RangeError", () => {
    expect(() => new Intl.PluralRules("en", { maximumFractionDigits: 101 })).toThrow(RangeError);
  });
});

describe.runIf(isIntl)("Intl.PluralRules digit option edge validation", () => {
  test("negative minimumFractionDigits throws RangeError", () => {
    expect(() => new Intl.PluralRules("en", { minimumFractionDigits: -1 })).toThrow(RangeError);
  });

  test("minimumFractionDigits above maximumFractionDigits throws RangeError", () => {
    expect(() => new Intl.PluralRules("en", { minimumFractionDigits: 5, maximumFractionDigits: 2 })).toThrow(RangeError);
  });

  test("minimumSignificantDigits above maximumSignificantDigits throws RangeError", () => {
    expect(() => new Intl.PluralRules("en", { minimumSignificantDigits: 10, maximumSignificantDigits: 5 })).toThrow(RangeError);
  });

  test("minimumFractionDigits alone raises the maximum to match", () => {
    const rules = new Intl.PluralRules("en", { minimumFractionDigits: 7 });
    expect(typeof rules.select(1)).toBe("string");
  });
});
