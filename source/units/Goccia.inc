{$I Shared.inc}

{ Temporal embeds generated IANA TZif data by default so Windows and other
  zoneinfo-less platforms can resolve named time zones without external files. }
{$IFNDEF GOCCIA_TEMPORAL_NO_EMBEDDED_TZDATA}
{$DEFINE GOCCIA_TEMPORAL_EMBEDDED_TZDATA}
{$ENDIF}

{ Uncomment to enable bridge-crossing metrics on the GocciaScript runtime.
  When enabled, TGocciaRuntimeOperations tracks per-operation counts and
  prints a summary to stderr on destruction. }
{.$DEFINE BRIDGE_METRICS}

{ Uncomment to enable GC timing instrumentation. When enabled,
  TGarbageCollector measures mark and sweep phase durations per
  collection and prints a summary on destruction. }
{.$DEFINE GC_TIMING}
