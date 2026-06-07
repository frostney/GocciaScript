/*---
description: Intl constructor supportedLocalesOf
features: [Intl]
---*/

const isIntl = typeof Intl !== "undefined";

describe.runIf(isIntl)("Intl constructor supportedLocalesOf", () => {
  const constructors = [
    Intl.Collator,
    Intl.DateTimeFormat,
    Intl.DisplayNames,
    Intl.DurationFormat,
    Intl.ListFormat,
    Intl.NumberFormat,
    Intl.PluralRules,
    Intl.RelativeTimeFormat,
    Intl.Segmenter,
  ].filter((constructor) => typeof constructor !== "undefined");

  test("ignores Unicode extensions when matching available locales", () => {
    for (const constructor of constructors) {
      const result = constructor.supportedLocalesOf(["de-u-co-phonebk"]);
      expect(result.length).toBe(1);
      expect(result[0]).toBe("de-u-co-phonebk");
    }
  });
});
