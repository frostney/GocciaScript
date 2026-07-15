{$I Shared.inc}

{ Temporal embeds generated IANA TZif data by default so Windows and other
  zoneinfo-less platforms can resolve named time zones without external files. }
{$IFNDEF GOCCIA_TEMPORAL_NO_EMBEDDED_TZDATA}
{$DEFINE GOCCIA_TEMPORAL_EMBEDDED_TZDATA}
{$ENDIF}

{ Intl embeds a minimal CLDR subset so locale canonicalization and
  formatting work even when system ICU is unavailable. Under LAKON
  the embedded-resource loader (TResourceStream / MAKEINTRESOURCE)
  has no platform yet, so the define stays off and CLDRData's
  not-available branch compiles instead (every TryGet* answers
  False) — the same defer-with-gate shape as the rung-1 ICU guards.
  Embedding the 1.19 MB blob as a generated Pascal constant (the
  UnicodeData pattern) waits for an Intl consumer and a data-region
  bump on the Lakon side. }
{$IFNDEF GOCCIA_INTL_NO_EMBEDDED_CLDR}
{$IFNDEF LAKON}
{$DEFINE GOCCIA_INTL_EMBEDDED_CLDR}
{$ENDIF}
{$ENDIF}

{ RegExp embeds Unicode Character Database property tables so
  \p Unicode property escapes resolve when system ICU is unavailable. }
{$IFNDEF GOCCIA_REGEXP_NO_EMBEDDED_UCD}
{$DEFINE GOCCIA_REGEXP_EMBEDDED_UCD}
{$ENDIF}

{ Uncomment to enable bridge-crossing metrics on the GocciaScript runtime.
  When enabled, TGocciaRuntimeOperations tracks per-operation counts and
  prints a summary to stderr on destruction. }
{.$DEFINE BRIDGE_METRICS}

{ Uncomment to enable GC timing instrumentation. When enabled,
  TGarbageCollector measures mark and sweep phase durations per
  collection and prints a summary on destruction. }
{.$DEFINE GC_TIMING}
