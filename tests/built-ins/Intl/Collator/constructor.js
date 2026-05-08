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
});
