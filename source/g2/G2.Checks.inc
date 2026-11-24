{ G2's code indexes variable-length records past their declared bounds (array[0..0] of TValue), relies on
  wrapping integer arithmetic and has its own native stack guard: the range, overflow, assertion and stack
  checks of a development build stay off for it, so it compiles and runs the same inside the apps' builds }
{$R-}{$Q-}{$C-}{$S-}
{ the JIT emits x86-64 code for the System V calling convention and reads threadvars %fs-relative: Linux only }
{$IF defined(CPUX86_64) and defined(LINUX)}{$DEFINE G2JIT}{$IFEND}
{ the C library's math functions: libm, or the Universal CRT on Windows (msvcrt lacks C99's cbrt, log2, expm1 and
  the like) }
{$MACRO ON}
{$IFDEF MSWINDOWS}{$DEFINE G2LIBM:='ucrtbase.dll'}{$ELSE}{$DEFINE G2LIBM:='m'}{$ENDIF}
