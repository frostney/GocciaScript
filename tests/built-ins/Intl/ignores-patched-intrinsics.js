/*---
description: Intl built-ins (the intl prelude) look up nothing a script can replace, beyond what ECMA-402 reads
features: [Intl, Temporal]
---*/

import { ARRAY_ITERATION, expectClean, installTripwire } from "../helpers/intrinsic-tripwire.js";

const wire = installTripwire();
afterAll(() => wire.uninstall());

const clean = (r, allowed) => expectClean(r, allowed);
const make = (path, ...args) => clean(wire.construct(path, ...args));
const covers = (proto, receiver, cases) => wire.covers(proto, receiver, cases);

test("Intl.getCanonicalLocales and Intl.supportedValuesOf", () => {
  expect(clean(wire.invoke("Intl.getCanonicalLocales", Intl, ["EN-us", "de"]))).toEqual(["en-US", "de"]);
  expect(clean(wire.invoke("Intl.supportedValuesOf", Intl, "calendar")).length > 0).toBe(true);
});

test("Intl.Locale", () => {
  const loc = make("Intl.Locale", "en-Latn-US-u-ca-gregory-co-phonebk-hc-h12-kf-upper-kn-nu-latn-fw-mon", { region: "GB" });
  covers("Intl.Locale.prototype", loc, [
    ["maximize", [], undefined],
    ["minimize", [], undefined],
    ["toString", [], "en-Latn-GB-u-ca-gregory-co-phonebk-fw-mon-hc-h12-kf-upper-kn-nu-latn"],
    ["baseName", [], "en-Latn-GB"],
    ["calendar", [], "gregory"],
    ["caseFirst", [], "upper"],
    ["collation", [], "phonebk"],
    ["firstDayOfWeek", [], "mon"],
    ["hourCycle", [], "h12"],
    ["numeric", [], true],
    ["numberingSystem", [], "latn"],
    ["language", [], "en"],
    ["script", [], "Latn"],
    ["region", [], "GB"],
    ["variants", [], undefined],
    ["getCalendars", [], ["gregory"]],
    ["getCollations", [], ["phonebk"]],
    ["getHourCycles", [], ["h12"]],
    ["getNumberingSystems", [], ["latn"]],
    ["getTimeZones", [], undefined],
    ["getTextInfo", [], { direction: "ltr" }],
    ["getWeekInfo", [], undefined],
  ]);
  expect(clean(wire.invoke("Intl.Locale.prototype.toString", make("Intl.Locale", "en-US")))).toBe("en-US");
});

test("Intl.Collator", () => {
  const c = make("Intl.Collator", "de", { sensitivity: "base" });
  const compare = clean(wire.invoke("Intl.Collator.prototype.compare", c));
  expect(clean(wire.watch(() => compare("a", "A")))).toBe(0);
  covers("Intl.Collator.prototype", c, [["resolvedOptions", [], undefined]]);
  expect(clean(wire.invoke("Intl.Collator.supportedLocalesOf", Intl.Collator, ["de"]))).toEqual(["de"]);
});

test("Intl.NumberFormat", () => {
  const nf = make("Intl.NumberFormat", "en-US", { style: "currency", currency: "USD", currencySign: "accounting" });
  const format = clean(wire.invoke("Intl.NumberFormat.prototype.format", nf));
  expect(clean(wire.watch(() => format(-1234.5)))).toBe("($1,234.50)");
  expect(clean(wire.watch(() => format("12345678901234567890.5")))).toBe("$12,345,678,901,234,567,890.50");
  expect(clean(wire.watch(() => format(12n)))).toBe("$12.00");
  covers("Intl.NumberFormat.prototype", nf, [
    ["formatToParts", [1.5], undefined],
    ["formatRange", [1, 2], undefined],
    ["formatRangeToParts", [1, 2], undefined],
    ["resolvedOptions", [], undefined],
  ]);
  const sig = make("Intl.NumberFormat", "en", { maximumSignificantDigits: 2, notation: "compact" });
  expect(clean(wire.invoke("Intl.NumberFormat.prototype.formatToParts", sig, 0x1f)).length > 0).toBe(true);
  expect(clean(wire.invoke("Intl.NumberFormat.supportedLocalesOf", Intl.NumberFormat, "en"))).toEqual(["en"]);
  expect(clean(wire.invoke("Number.prototype.toLocaleString", 1234.5, "de"))).toBe("1.234,5");
  expect(clean(wire.invoke("BigInt.prototype.toLocaleString", 1234n, "en"))).toBe("1,234");
});

test("Intl.PluralRules", () => {
  const pr = make("Intl.PluralRules", "en", { maximumFractionDigits: 0 });
  covers("Intl.PluralRules.prototype", pr, [
    ["select", [1.2], "one"],
    ["selectRange", [1, 2], "other"],
    ["resolvedOptions", [], undefined],
  ]);
  const sig = make("Intl.PluralRules", "en", { maximumSignificantDigits: 1 });
  expect(clean(wire.invoke("Intl.PluralRules.prototype.select", sig, 1.4))).toBe("one");
  expect(clean(wire.invoke("Intl.PluralRules.supportedLocalesOf", Intl.PluralRules, "en"))).toEqual(["en"]);
});

test("Intl.ListFormat", () => {
  const lf = make("Intl.ListFormat", "en", { type: "disjunction" });
  // StringListFromIterable reads the list's iterator (14.5.4)
  covers("Intl.ListFormat.prototype", lf, [
    ["format", [["a", "b", "c"]], "a, b, or c", ARRAY_ITERATION],
    ["formatToParts", [["a", "b"]], undefined, ARRAY_ITERATION],
    ["resolvedOptions", [], undefined],
  ]);
  expect(clean(wire.invoke("Intl.ListFormat.supportedLocalesOf", Intl.ListFormat, "en"))).toEqual(["en"]);
});

test("Intl.DateTimeFormat and the Date locale methods", () => {
  const dtf = make("Intl.DateTimeFormat", "en-US", { timeZone: "+05:30", dateStyle: "medium", timeStyle: "short" });
  const format = clean(wire.invoke("Intl.DateTimeFormat.prototype.format", dtf));
  expect(typeof clean(wire.watch(() => format(0)))).toBe("string");
  covers("Intl.DateTimeFormat.prototype", dtf, [
    ["formatToParts", [0], undefined],
    ["formatRange", [0, 86400000], undefined],
    ["formatRangeToParts", [0, 86400000], undefined],
    ["resolvedOptions", [], undefined],
  ]);
  const chinese = make("Intl.DateTimeFormat", "zh-u-ca-chinese", { month: "numeric", timeZone: "UTC" });
  const formatChinese = clean(wire.invoke("Intl.DateTimeFormat.prototype.format", chinese));
  expect(typeof clean(wire.watch(() => formatChinese(0)))).toBe("string");
  expect(clean(wire.invoke("Intl.DateTimeFormat.supportedLocalesOf", Intl.DateTimeFormat, "en"))).toEqual(["en"]);
  const d = new Date(0);
  for (const m of ["toLocaleString", "toLocaleDateString", "toLocaleTimeString"]) {
    expect(typeof clean(wire.invoke(`Date.prototype.${m}`, d, "en", { timeZone: "UTC" }))).toBe("string");
  }
  expect(clean(wire.invoke("Date.prototype.toLocaleString", new Date(NaN)))).toBe("Invalid Date");
});

test("Intl.RelativeTimeFormat", () => {
  const rtf = make("Intl.RelativeTimeFormat", "en", { numeric: "auto" });
  covers("Intl.RelativeTimeFormat.prototype", rtf, [
    ["format", [-1, "day"], "yesterday"],
    ["formatToParts", [3, "hours"], undefined],
    ["resolvedOptions", [], undefined],
  ]);
  expect(clean(wire.invoke("Intl.RelativeTimeFormat.supportedLocalesOf", Intl.RelativeTimeFormat, "en"))).toEqual(["en"]);
});

test("Intl.DisplayNames", () => {
  const dn = make("Intl.DisplayNames", "en", { type: "calendar" });
  covers("Intl.DisplayNames.prototype", dn, [
    ["of", ["gregory"], undefined],
    ["resolvedOptions", [], undefined],
  ]);
  const lang = make("Intl.DisplayNames", "en", { type: "language" });
  expect(typeof clean(wire.invoke("Intl.DisplayNames.prototype.of", lang, "en-Latn-US"))).toBe("string");
  const cur = make("Intl.DisplayNames", "en", { type: "currency" });
  expect(typeof clean(wire.invoke("Intl.DisplayNames.prototype.of", cur, "EUR"))).toBe("string");
  expect(clean(wire.invoke("Intl.DisplayNames.supportedLocalesOf", Intl.DisplayNames, "en"))).toEqual(["en"]);
});

test("Intl.Segmenter, %SegmentsPrototype% and %SegmentIteratorPrototype%", () => {
  const seg = make("Intl.Segmenter", "en", { granularity: "word" });
  covers("Intl.Segmenter.prototype", seg, [["resolvedOptions", [], undefined]]);
  const segments = clean(wire.invoke("Intl.Segmenter.prototype.segment", seg, "Hi there"));
  covers("%SegmentsPrototype%", segments, [
    ["containing", [4], { segment: "there", index: 3, input: "Hi there", isWordLike: true }],
    ["[Symbol.iterator]", [], undefined],
  ]);
  const it = clean(wire.invoke("%SegmentsPrototype%[Symbol.iterator]", segments));
  covers("%SegmentIteratorPrototype%", it, [["next", [], { value: { segment: "Hi", index: 0, input: "Hi there", isWordLike: true }, done: false }]]);
  expect(clean(wire.invoke("Intl.Segmenter.supportedLocalesOf", Intl.Segmenter, "en"))).toEqual(["en"]);
});

test("Intl.DurationFormat and Temporal.Duration.prototype.toLocaleString", () => {
  const df = make("Intl.DurationFormat", "en", { style: "digital", fractionalDigits: 3 });
  covers("Intl.DurationFormat.prototype", df, [
    ["format", [{ hours: 1, minutes: 2, seconds: 3, milliseconds: 45 }], "1:02:03.045"],
    ["formatToParts", [{ hours: 1 }], undefined],
    ["resolvedOptions", [], undefined],
  ]);
  expect(clean(wire.invoke("Intl.DurationFormat.supportedLocalesOf", Intl.DurationFormat, "en"))).toEqual(["en"]);
  const dur = Temporal.Duration.from({ hours: 1, minutes: 30 });
  expect(typeof clean(wire.invoke("Temporal.Duration.prototype.toLocaleString", dur, "en"))).toBe("string");
});

test("the Temporal locale methods", () => {
  const values = [
    ["PlainDate", Temporal.PlainDate.from("2020-01-15")],
    ["PlainTime", Temporal.PlainTime.from("10:20")],
    ["PlainDateTime", Temporal.PlainDateTime.from("2020-01-15T10:20")],
    // a year-month or month-day formats only in the formatter's own calendar
    ["PlainYearMonth", Temporal.PlainYearMonth.from({ year: 2020, month: 1, calendar: "gregory" })],
    ["PlainMonthDay", Temporal.PlainMonthDay.from({ monthCode: "M01", day: 15, calendar: "gregory" })],
    ["Instant", Temporal.Instant.from("2020-01-15T10:20Z")],
    ["ZonedDateTime", Temporal.ZonedDateTime.from("2020-01-15T10:20[UTC]")],
  ];
  for (const [cls, v] of values) {
    expect(typeof clean(wire.invoke(`Temporal.${cls}.prototype.toLocaleString`, v, "en"))).toBe("string");
  }
});

test("the String locale methods", () => {
  expect(clean(wire.invoke("String.prototype.localeCompare", "a", "b", "en"))).toBe(-1);
  expect(clean(wire.invoke("String.prototype.toLocaleUpperCase", "i", "tr"))).toBe("İ");
  expect(clean(wire.invoke("String.prototype.toLocaleLowerCase", "I", "tr"))).toBe("ı");
});

test("every Intl method, getter and constructor has a case", () => {
  const owners = ["Intl", "%SegmentsPrototype%", "%SegmentIteratorPrototype%"];
  for (const C of ["Collator", "DateTimeFormat", "DisplayNames", "DurationFormat", "ListFormat", "Locale", "NumberFormat",
    "PluralRules", "RelativeTimeFormat", "Segmenter"]) {
    owners.push(`Intl.${C}`, `Intl.${C}.prototype`);
  }
  expect(owners.flatMap((o) => wire.uncovered(o))).toEqual([]);
});
