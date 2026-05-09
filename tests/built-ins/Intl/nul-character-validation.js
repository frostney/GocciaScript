/*---
description: Intl constructors reject strings containing NUL characters
features: [Intl]
---*/

const isIntl = typeof Intl !== "undefined";

describe.runIf(isIntl)("NUL character validation in Intl constructors", () => {
  test("Intl.Collator rejects sensitivity with NUL", () => {
    expect(() => new Intl.Collator("en", { sensitivity: "base\0cookie" })).toThrow(RangeError);
  });

  test("Intl.Collator rejects usage with NUL", () => {
    expect(() => new Intl.Collator("en", { usage: "sort\0evil" })).toThrow(RangeError);
  });

  test("Intl.Collator rejects caseFirst with NUL", () => {
    expect(() => new Intl.Collator("en", { caseFirst: "upper\0evil" })).toThrow(RangeError);
  });

  test("Intl.Collator rejects localeMatcher with NUL", () => {
    expect(() => new Intl.Collator("en", { localeMatcher: "lookup\0evil" })).toThrow(RangeError);
  });

  test("Intl.NumberFormat rejects style with NUL", () => {
    expect(() => new Intl.NumberFormat("en", { style: "decimal\0cookie" })).toThrow(RangeError);
  });

  test("Intl.NumberFormat rejects localeMatcher with NUL", () => {
    expect(() => new Intl.NumberFormat("en", { localeMatcher: "lookup\0evil" })).toThrow(RangeError);
  });

  test("Intl.NumberFormat rejects currency with NUL", () => {
    expect(() => new Intl.NumberFormat("en", { style: "currency", currency: "USD\0evil" })).toThrow(RangeError);
  });

  test("Intl.NumberFormat rejects currencyDisplay with NUL", () => {
    expect(() => new Intl.NumberFormat("en", { style: "currency", currency: "USD", currencyDisplay: "symbol\0evil" })).toThrow(RangeError);
  });

  test("Intl.DateTimeFormat rejects localeMatcher with NUL", () => {
    expect(() => new Intl.DateTimeFormat("en", { localeMatcher: "lookup\0evil" })).toThrow(RangeError);
  });

  test("Intl.DateTimeFormat rejects hourCycle with NUL", () => {
    expect(() => new Intl.DateTimeFormat("en", { hourCycle: "h12\0evil" })).toThrow(RangeError);
  });

  test("Intl.DateTimeFormat rejects weekday with NUL", () => {
    expect(() => new Intl.DateTimeFormat("en", { weekday: "long\0evil" })).toThrow(RangeError);
  });

  test("Intl.DateTimeFormat rejects timeZone with NUL", () => {
    expect(() => new Intl.DateTimeFormat("en", { timeZone: "UTC\0evil" })).toThrow(RangeError);
  });

  test("Intl.DateTimeFormat rejects formatMatcher with NUL", () => {
    expect(() => new Intl.DateTimeFormat("en", { formatMatcher: "basic\0evil" })).toThrow(RangeError);
  });

  test("Intl.PluralRules rejects type with NUL", () => {
    expect(() => new Intl.PluralRules("en", { type: "cardinal\0evil" })).toThrow(RangeError);
  });

  test("Intl.PluralRules rejects localeMatcher with NUL", () => {
    expect(() => new Intl.PluralRules("en", { localeMatcher: "lookup\0evil" })).toThrow(RangeError);
  });

  test("Intl.RelativeTimeFormat rejects style with NUL", () => {
    expect(() => new Intl.RelativeTimeFormat("en", { style: "long\0evil" })).toThrow(RangeError);
  });

  test("Intl.RelativeTimeFormat rejects numeric with NUL", () => {
    expect(() => new Intl.RelativeTimeFormat("en", { numeric: "always\0evil" })).toThrow(RangeError);
  });

  test("Intl.RelativeTimeFormat rejects localeMatcher with NUL", () => {
    expect(() => new Intl.RelativeTimeFormat("en", { localeMatcher: "lookup\0evil" })).toThrow(RangeError);
  });

  test("Intl.RelativeTimeFormat.format rejects unit with NUL", () => {
    const rtf = new Intl.RelativeTimeFormat("en");
    expect(() => rtf.format(1, "day\0evil")).toThrow(RangeError);
  });

  test("Intl.ListFormat rejects type with NUL", () => {
    expect(() => new Intl.ListFormat("en", { type: "conjunction\0evil" })).toThrow(RangeError);
  });

  test("Intl.ListFormat rejects style with NUL", () => {
    expect(() => new Intl.ListFormat("en", { style: "long\0evil" })).toThrow(RangeError);
  });

  test("Intl.DisplayNames rejects type with NUL", () => {
    expect(() => new Intl.DisplayNames("en", { type: "language\0evil" })).toThrow(RangeError);
  });

  test("Intl.DisplayNames rejects style with NUL", () => {
    expect(() => new Intl.DisplayNames("en", { type: "language", style: "long\0evil" })).toThrow(RangeError);
  });

  test("Intl.DisplayNames rejects fallback with NUL", () => {
    expect(() => new Intl.DisplayNames("en", { type: "language", fallback: "code\0evil" })).toThrow(RangeError);
  });

  test("Intl.Segmenter rejects granularity with NUL", () => {
    expect(() => new Intl.Segmenter("en", { granularity: "word\0evil" })).toThrow(RangeError);
  });

  test("Intl.DurationFormat rejects style with NUL", () => {
    expect(() => new Intl.DurationFormat("en", { style: "long\0evil" })).toThrow(RangeError);
  });

  test("Intl.Locale rejects tag with NUL", () => {
    expect(() => new Intl.Locale("de\0evil")).toThrow(RangeError);
  });

  test("Intl.Locale rejects language option with NUL", () => {
    expect(() => new Intl.Locale("de", { language: "it\0biscotto" })).toThrow(RangeError);
  });

  test("Intl.Locale rejects script option with NUL", () => {
    expect(() => new Intl.Locale("de", { script: "Latn\0evil" })).toThrow(RangeError);
  });

  test("Intl.Locale rejects region option with NUL", () => {
    expect(() => new Intl.Locale("de", { region: "DE\0evil" })).toThrow(RangeError);
  });

  test("Intl.Locale rejects calendar option with NUL", () => {
    expect(() => new Intl.Locale("de", { calendar: "gregory\0evil" })).toThrow(RangeError);
  });

  test("Intl.Locale rejects collation option with NUL", () => {
    expect(() => new Intl.Locale("de", { collation: "phonebk\0evil" })).toThrow(RangeError);
  });

  test("Intl.Locale rejects hourCycle option with NUL", () => {
    expect(() => new Intl.Locale("de", { hourCycle: "h12\0evil" })).toThrow(RangeError);
  });

  test("Intl.Locale rejects caseFirst option with NUL", () => {
    expect(() => new Intl.Locale("de", { caseFirst: "upper\0evil" })).toThrow(RangeError);
  });

  test("Intl.Locale rejects numberingSystem option with NUL", () => {
    expect(() => new Intl.Locale("de", { numberingSystem: "latn\0evil" })).toThrow(RangeError);
  });

  test("supportedLocalesOf rejects localeMatcher with NUL", () => {
    expect(() => Intl.Collator.supportedLocalesOf(["en"], { localeMatcher: "lookup\0evil" })).toThrow(RangeError);
  });

  test("getCanonicalLocales rejects tag with NUL", () => {
    expect(() => Intl.getCanonicalLocales("de\0Latn")).toThrow(RangeError);
  });
});
