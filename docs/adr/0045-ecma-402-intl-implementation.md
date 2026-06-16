# ECMA-402 Intl implementation

**Date:** 2026-05-08
**Area:** `runtime`

ECMA-402 Intl namespace. Full implementation of the Internationalization API: Intl.Locale, Intl.Collator, Intl.NumberFormat, Intl.DateTimeFormat, Intl.PluralRules, Intl.RelativeTimeFormat, Intl.ListFormat, Intl.DisplayNames, Intl.Segmenter, Intl.DurationFormat, plus Intl.getCanonicalLocales and Intl.supportedValuesOf. Data strategy mirrors Temporal timezone: system ICU library (macOS libicucore, Linux libicui18n, Windows icu.dll shared with Temporal) as primary, embedded CLDR resource as fallback. Shared units in `source/shared/` (ICU.pas, BCP47.pas, IntlTypes.pas, IntlICU.pas, IntlCLDRData.pas, IntlLocaleResolver.pas) provide platform-independent Intl infrastructure. Existing locale stubs on String/Number/Date prototypes wired to Intl formatters. [built-ins-intl.md](../built-ins-intl.md). [build-system.md § Generated Intl Data](../build-system.md#generated-intl-data).
