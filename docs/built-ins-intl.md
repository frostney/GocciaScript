# Intl Built-ins

*ECMA-402 Internationalization API reference*

Implements the [ECMA-402 Internationalization API](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl). See the [ECMA-402 specification](https://tc39.es/ecma402/) for the full specification reference.

**GocciaScript differences:** Requires system ICU library for full functionality. Falls back to embedded CLDR data when ICU is unavailable.

## Executive Summary

- **ECMA-402 conformance** -- Implements the Internationalization API with locale-aware formatting for numbers, dates, lists, durations, and relative time
- **Data strategy** -- System ICU library as primary data source, embedded CLDR resource as fallback for single-binary portability
- **Locale negotiation** -- BCP 47 locale identifiers with Unicode extension keys, best-fit and lookup matching algorithms
- **Segmentation** -- `Intl.Segmenter` provides Unicode-aware grapheme, word, and sentence segmentation
- **Collation and display** -- `Intl.Collator` for locale-sensitive string comparison, `Intl.DisplayNames` for localized display of language, region, and script names

## Data Strategy

`Intl` constructors load locale data from ICU libraries or embedded CLDR data. Lookup order is:

1. System ICU library (preferred for full locale coverage and up-to-date data)
2. Embedded CLDR resource (fallback for environments without ICU)

### Platform ICU details

| Platform | Library | Notes |
|----------|---------|-------|
| macOS | `libicucore` | Ships with the OS; available on all supported macOS versions |
| Linux | `libicui18n` | Typically provided by the `libicu` or `icu` system package |
| Windows | `icu.dll` | Available on Windows 10 1903+ (shared with Temporal timezone support) |

The embedded CLDR data is generated from upstream CLDR JSON distributions and linked as a FreePascal `.res` resource. This keeps locale data available in single-binary builds even when system ICU cannot provide the requested data. Define `GOCCIA_INTL_NO_EMBEDDED_CLDR` at compile time to omit the embedded fallback.

## Namespace Structure

```javascript
Intl.Locale             // Unicode locale identifier
Intl.Collator           // Locale-sensitive string comparison
Intl.NumberFormat       // Locale-sensitive number formatting
Intl.DateTimeFormat     // Locale-sensitive date and time formatting
Intl.PluralRules        // Plural-sensitive formatting
Intl.RelativeTimeFormat // Locale-sensitive relative time formatting
Intl.ListFormat         // Locale-sensitive list formatting
Intl.DisplayNames       // Locale-sensitive display names
Intl.Segmenter          // Unicode segmentation
Intl.DurationFormat     // Locale-sensitive duration formatting
```

**Static methods:**

| Method | Description |
|--------|-------------|
| `Intl.getCanonicalLocales(locales)` | Return an array of canonical locale strings |
| `Intl.supportedValuesOf(key)` | Return an array of supported values for a given key (`"calendar"`, `"collation"`, `"currency"`, `"numberingSystem"`, `"timeZone"`, `"unit"`) |

## Intl.Locale

Represents a Unicode locale identifier with parsed subtags and extension keys.

| Constructor / Static | Description |
|---------------------|-------------|
| `new Intl.Locale(tag, options?)` | Create from a BCP 47 language tag string with optional override options |

| Getter | Description |
|--------|-------------|
| `baseName` | Base locale identifier without extension keys |
| `calendar` | Calendar type (Unicode `ca` extension) |
| `caseFirst` | Case-first sort order (`"upper"`, `"lower"`, `"false"`) |
| `collation` | Collation type (Unicode `co` extension) |
| `hourCycle` | Hour cycle (`"h11"`, `"h12"`, `"h23"`, `"h24"`) |
| `language` | Language subtag |
| `numberingSystem` | Numbering system (Unicode `nu` extension) |
| `numeric` | Whether numeric collation is enabled |
| `region` | Region subtag |
| `script` | Script subtag |

| Method | Description |
|--------|-------------|
| `maximize()` | Return a new Locale with likely subtags added |
| `minimize()` | Return a new Locale with likely subtags removed |
| `getCalendars()` | Return an array of preferred calendars |
| `getCollations()` | Return an array of preferred collations |
| `getHourCycles()` | Return an array of preferred hour cycles |
| `getNumberingSystems()` | Return an array of preferred numbering systems |
| `getTimeZones()` | Return an array of preferred time zones |
| `getTextInfo()` | Return an object with `direction` (`"ltr"` or `"rtl"`) |
| `getWeekInfo()` | Return an object with `firstDay` and `weekend` |
| `toString()` | Return the canonical locale string |

## Intl.Collator

Provides locale-sensitive string comparison.

| Constructor / Static | Description |
|---------------------|-------------|
| `new Intl.Collator(locales?, options?)` | Create a collator for the given locale(s) |
| `Intl.Collator.supportedLocalesOf(locales, options?)` | Return supported locales from the requested list |

| Method | Description |
|--------|-------------|
| `compare(x, y)` | Compare two strings; returns negative, zero, or positive |
| `resolvedOptions()` | Return an object reflecting the locale and collation options |

## Intl.NumberFormat

Provides locale-sensitive number formatting.

| Constructor / Static | Description |
|---------------------|-------------|
| `new Intl.NumberFormat(locales?, options?)` | Create a number formatter for the given locale(s) |
| `Intl.NumberFormat.supportedLocalesOf(locales, options?)` | Return supported locales from the requested list |

| Method | Description |
|--------|-------------|
| `format(value)` | Format a number as a locale-specific string |
| `formatToParts(value)` | Return an array of format parts with `type` and `value` |
| `formatRange(start, end)` | Format a range between two numbers |
| `formatRangeToParts(start, end)` | Return an array of format parts for a range |
| `resolvedOptions()` | Return an object reflecting the locale and formatting options |

## Intl.DateTimeFormat

Provides locale-sensitive date and time formatting.

| Constructor / Static | Description |
|---------------------|-------------|
| `new Intl.DateTimeFormat(locales?, options?)` | Create a date/time formatter for the given locale(s) |
| `Intl.DateTimeFormat.supportedLocalesOf(locales, options?)` | Return supported locales from the requested list |

| Method | Description |
|--------|-------------|
| `format(date)` | Format a date as a locale-specific string |
| `formatToParts(date)` | Return an array of format parts with `type` and `value` |
| `formatRange(startDate, endDate)` | Format a date range |
| `formatRangeToParts(startDate, endDate)` | Return an array of format parts for a date range |
| `resolvedOptions()` | Return an object reflecting the locale and formatting options |

## Intl.PluralRules

Provides plural-sensitive formatting and plural category selection.

| Constructor / Static | Description |
|---------------------|-------------|
| `new Intl.PluralRules(locales?, options?)` | Create plural rules for the given locale(s) |
| `Intl.PluralRules.supportedLocalesOf(locales, options?)` | Return supported locales from the requested list |

| Method | Description |
|--------|-------------|
| `select(value)` | Return the plural category (`"zero"`, `"one"`, `"two"`, `"few"`, `"many"`, `"other"`) |
| `selectRange(start, end)` | Return the plural category for a range |
| `resolvedOptions()` | Return an object reflecting the locale and plural options |

## Intl.RelativeTimeFormat

Provides locale-sensitive relative time formatting.

| Constructor / Static | Description |
|---------------------|-------------|
| `new Intl.RelativeTimeFormat(locales?, options?)` | Create a relative time formatter for the given locale(s) |
| `Intl.RelativeTimeFormat.supportedLocalesOf(locales, options?)` | Return supported locales from the requested list |

| Method | Description |
|--------|-------------|
| `format(value, unit)` | Format a relative time value (e.g., `"in 3 days"`, `"2 hours ago"`) |
| `formatToParts(value, unit)` | Return an array of format parts with `type` and `value` |
| `resolvedOptions()` | Return an object reflecting the locale and formatting options |

## Intl.ListFormat

Provides locale-sensitive list formatting.

| Constructor / Static | Description |
|---------------------|-------------|
| `new Intl.ListFormat(locales?, options?)` | Create a list formatter for the given locale(s) |
| `Intl.ListFormat.supportedLocalesOf(locales, options?)` | Return supported locales from the requested list |

| Method | Description |
|--------|-------------|
| `format(list)` | Format a list of strings (e.g., `"A, B, and C"`) |
| `formatToParts(list)` | Return an array of format parts with `type` and `value` |
| `resolvedOptions()` | Return an object reflecting the locale and formatting options |

## Intl.DisplayNames

Provides localized display names for languages, regions, scripts, currencies, calendars, and date-time fields.

| Constructor / Static | Description |
|---------------------|-------------|
| `new Intl.DisplayNames(locales, options)` | Create a display name resolver for the given locale(s) and type |
| `Intl.DisplayNames.supportedLocalesOf(locales, options?)` | Return supported locales from the requested list |

| Method | Description |
|--------|-------------|
| `of(code)` | Return the display name for the given code, or `undefined` |
| `resolvedOptions()` | Return an object reflecting the locale and display name options |

## Intl.Segmenter

Provides Unicode-aware text segmentation by grapheme, word, or sentence boundaries.

| Constructor / Static | Description |
|---------------------|-------------|
| `new Intl.Segmenter(locales?, options?)` | Create a segmenter for the given locale(s) and granularity |
| `Intl.Segmenter.supportedLocalesOf(locales, options?)` | Return supported locales from the requested list |

| Method | Description |
|--------|-------------|
| `segment(string)` | Return a `Segments` object for iterating over segments |
| `resolvedOptions()` | Return an object reflecting the locale and segmentation options |

**Segments object:**

| Method | Description |
|--------|-------------|
| `containing(index)` | Return the segment at the given index |
| `[Symbol.iterator]()` | Iterate over segments, each with `segment`, `index`, `input`, and `isWordLike` (word granularity only) |

## Intl.DurationFormat

Provides locale-sensitive duration formatting.

| Constructor / Static | Description |
|---------------------|-------------|
| `new Intl.DurationFormat(locales?, options?)` | Create a duration formatter for the given locale(s) |
| `Intl.DurationFormat.supportedLocalesOf(locales, options?)` | Return supported locales from the requested list |

| Method | Description |
|--------|-------------|
| `format(duration)` | Format a duration object as a locale-specific string |
| `formatToParts(duration)` | Return an array of format parts with `type` and `value` |
| `resolvedOptions()` | Return an object reflecting the locale and formatting options |

## Related documents

- [Built-in Objects](built-ins.md) -- Full built-ins reference (all other types)
- [Language Features](language.md) -- Language-level syntax and semantics
- [Language Tables](language-tables.md) -- ECMAScript feature coverage
