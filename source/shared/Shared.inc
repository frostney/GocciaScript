{$IFDEF FPC}
  {$mode delphiunicode} {$H+}
{$ENDIF}

{ Normalize compiler-specific CPU symbols once. Shared code branches on these
  project symbols instead of repeating FPC/Delphi aliases at every ABI site. }
{$IF DEFINED(CPU64) OR DEFINED(CPU64BITS)}
  {$DEFINE GOCCIA_CPU_64}
{$ENDIF}
{$IF DEFINED(CPUX86_64) OR DEFINED(CPUX64)}
  {$DEFINE GOCCIA_CPU_X86_64}
{$ENDIF}
{$IF DEFINED(CPUAARCH64) OR DEFINED(CPUARM64)}
  {$DEFINE GOCCIA_CPU_AARCH64}
{$ENDIF}
{$IF DEFINED(CPUI386) OR DEFINED(CPUX86)}
  {$DEFINE GOCCIA_CPU_X86}
{$ENDIF}
{$IF DEFINED(GOCCIA_CPU_X86_64) OR DEFINED(GOCCIA_CPU_AARCH64) OR DEFINED(GOCCIA_CPU_X86)}
  {$DEFINE GOCCIA_CPU_FFI_SUPPORTED}
{$ENDIF}

{ FPC defaults to AT&T syntax on x86. Delphi and GocciaScript assembly use
  Intel syntax, so establish the mode once for every included unit. }
{$IF DEFINED(FPC) AND (DEFINED(GOCCIA_CPU_X86_64) OR DEFINED(GOCCIA_CPU_X86))}
  {$ASMMODE INTEL}
{$ENDIF}
{$M+}
{$IFDEF PRODUCTION}
  {$IFDEF FPC}
    {$optimization autoInline}
  {$ENDIF}
  { Never enable FASTMATH. It reassociates and contracts floating-point
    operations and assumes no NaN/Inf, violating the strict IEEE-754 double
    semantics JavaScript requires. -O4 (used for production builds) would
    otherwise turn it on and miscompile float-sensitive paths — e.g. the
    edge-case Temporal ZonedDateTime.hoursInDay DST math (PR #797). This
    source-level guard keeps it off regardless of the optimization level. }
  {$IFDEF FPC}
    {$OPTIMIZATION NOFASTMATH}
  {$ENDIF}
  {$overflowchecks off}
  {$rangechecks off}
  {$S-}
  {$IFDEF FPC}
    {$OBJECTCHECKS OFF}
  {$ENDIF}
  {$C-}
{$ELSE}
  {$overflowchecks on}
  {$rangechecks on}
  {$S+}
  {$IFDEF FPC}
    {$OBJECTCHECKS ON}
  {$ENDIF}
  {$C+}
{$ENDIF}
{$IFDEF FPC}
  {$modeswitch advancedrecords}
{$ENDIF}

{ Nested comments, on both compilers.

  This is a JavaScript engine written in Pascal, so comments quote JavaScript
  constantly, and JavaScript is full of braces. Without this, a comment
  containing an export clause or an empty block ends at the first closing
  brace inside it, and the compiler then reports an "illegal character" at
  whatever token follows — a column unrelated to the real cause.

  Nesting fixes the balanced case, which is the one that occurs in practice.
  A lone unmatched closing brace still ends the comment, so keep braces in
  comment prose balanced or spell them out in words.

  The spelling differs per compiler: FPC takes a modeswitch, Delphi a
  directive. Define GOCCIA_NO_NESTED_COMMENTS to opt out. }
{$IFNDEF GOCCIA_NO_NESTED_COMMENTS}
  {$IFDEF FPC}
    {$modeswitch nestedcomments}
  {$ELSE}
    {$NESTEDCOMMENTS ON}
  {$ENDIF}
{$ENDIF}
