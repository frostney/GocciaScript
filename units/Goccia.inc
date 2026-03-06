{$mode delphi} {H+}
{$IFDEF PRODUCTION}
  {$overflowchecks off}
  {$rangechecks off}
  {$S-}
  {$OBJECTCHECKS OFF}
  {$C-}
{$ELSE}
  {$overflowchecks on}
  {$rangechecks on}
  {$S+}
  {$OBJECTCHECKS ON}
  {$C+}
{$ENDIF}
{$modeswitch advancedrecords}
{$modeswitch multihelpers}

{ Uncomment to enable bridge-crossing metrics on the GocciaScript runtime.
  When enabled, TGocciaRuntimeOperations tracks per-operation counts and
  prints a summary to stderr on destruction. }
{.$DEFINE BRIDGE_METRICS}
