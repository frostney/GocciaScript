/*---
description: Intl.Collator constructor
features: [Intl]
---*/

const isIntl = typeof Intl !== "undefined";

describe.runIf(isIntl)("Intl.Collator constructor", () => {
  test("creates an instance with no arguments", () => {
    const collator = new Intl.Collator();
    expect(collator).toBeInstanceOf(Intl.Collator);
  });

  test("creates an instance with a locale argument", () => {
    const collator = new Intl.Collator("en-US");
    expect(collator).toBeInstanceOf(Intl.Collator);
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
    expect(collation.locale).toBe("de-u-co-phonebk");
    expect(collation.collation).toBe("phonebk");
  });
});
