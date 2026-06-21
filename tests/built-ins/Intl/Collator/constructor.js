/*---
description: Intl.Collator constructor
features: [Intl]
---*/

const isIntl = typeof Intl !== "undefined";
const hasFullICU = isIntl && new Intl.NumberFormat("en-US").format(NaN) === "NaN";

describe.runIf(isIntl)("Intl.Collator constructor", () => {
  test("creates an instance with no arguments", () => {
    const collator = new Intl.Collator();
    expect(collator).toBeInstanceOf(Intl.Collator);
  });

  test("creates an instance with a locale argument", () => {
    const collator = new Intl.Collator("en-US");
    expect(collator).toBeInstanceOf(Intl.Collator);
  });

  test("creates an instance with an array locale argument", () => {
    const collator = new Intl.Collator(["en-US"]);
    expect(collator).toBeInstanceOf(Intl.Collator);
  });

  test("rejects invalid locale arguments", () => {
    expect(() => new Intl.Collator(null)).toThrow(TypeError);
    expect(() => new Intl.Collator([NaN])).toThrow(TypeError);
    expect(() => new Intl.Collator(["i"])).toThrow(RangeError);
    expect(() => new Intl.Collator(["de_DE"])).toThrow(RangeError);
  });

  test("compare property is a function", () => {
    const collator = new Intl.Collator();
    expect(typeof collator.compare).toBe("function");
  });

  test("resolvedOptions returns an object with locale", () => {
    const collator = new Intl.Collator("en-US");
    const options = collator.resolvedOptions();
    expect(typeof options.locale).toBe("string");
  });

  test("resolves supported Unicode extension keys", () => {
    const numeric = new Intl.Collator("en-u-kn-true").resolvedOptions();
    expect(numeric.locale).toBe("en-u-kn");
    expect(numeric.numeric).toBe(true);

    const numericOverride = new Intl.Collator("en-u-kn-false", { numeric: true }).resolvedOptions();
    expect(numericOverride.locale).toBe("en");
    expect(numericOverride.numeric).toBe(true);

    const caseFirst = new Intl.Collator("en-u-kf-lower").resolvedOptions();
    expect(caseFirst.locale).toBe("en-u-kf-lower");
    expect(caseFirst.caseFirst).toBe("lower");

    const collation = new Intl.Collator("de-u-co-phonebk").resolvedOptions();
    if (hasFullICU) {
      expect(collation.locale).toBe("de-u-co-phonebk");
      expect(collation.collation).toBe("phonebk");
    } else {
      expect(collation.locale).toBe("de");
      expect(collation.collation).toBe("default");
    }
  });

  test("ignores Unicode extension-like private-use subtags", () => {
    const options = new Intl.Collator("de-x-u-co-phonebk").resolvedOptions();
    expect(options.collation).toBe("default");
  });

  test("rejects invalid caseFirst option values", () => {
    expect(() => new Intl.Collator("en", { caseFirst: "invalid" })).toThrow(RangeError);
  });

  test("rejects invalid string option values", () => {
    expect(() => new Intl.Collator("en", { usage: "invalid" })).toThrow(RangeError);
    expect(() => new Intl.Collator("en", { sensitivity: "invalid" })).toThrow(RangeError);
    expect(() => new Intl.Collator("en", { localeMatcher: "invalid" })).toThrow(RangeError);
  });

  test("object-coerces non-undefined options", () => {
    expect(() => new Intl.Collator("en", null)).toThrow(TypeError);
    expect(new Intl.Collator("en", true)).toBeInstanceOf(Intl.Collator);
  });
});
