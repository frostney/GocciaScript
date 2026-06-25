{$mode delphi} {$H+}
{$M+}
{$IFDEF PRODUCTION}
  {$optimization autoInline}
  { Never enable FASTMATH. It reassociates and contracts floating-point
    operations and assumes no NaN/Inf, violating the strict IEEE-754 double
    semantics JavaScript requires. -O4 (used for production builds) would
    otherwise turn it on and miscompile float-sensitive paths — e.g. the
    edge-case Temporal ZonedDateTime.hoursInDay DST math (PR #797). This
    source-level guard keeps it off regardless of the optimization level. }
  {$OPTIMIZATION NOFASTMATH}
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
