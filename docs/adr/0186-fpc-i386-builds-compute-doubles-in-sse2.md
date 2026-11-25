# 0186 - FPC i386 builds compute doubles in SSE2

**Date:** 2026-11-25
**Area:** `build`, `runtime`
**Related:** [ADR 0016](0016-ieee-754-number-representation.md)

## Context

FPC's i386 code generator did Single and Double arithmetic on the x87 unit.
x87 rounds a result to the precision-control width while keeping its own,
wider exponent range, and rounds again when it stores the Double. A result
in Double's subnormal range could therefore round twice: on the
`i386-win32` build, `1.2349373183628205e-308 / 3.3` and
`1.447496838033e-154 * 5.5919834578432674e-155` came out one bit off, and
`(1 + 2 ** -26) * 2 ** -600 * ((1 - 2 ** -26 + 2 ** -52) * 2 ** -475)`
gave 0 instead of `5e-324`. The 64-bit targets compute in SSE2 or on
AArch64's FPU and round once.

## Decision

`Shared.inc` sets `{$FPUTYPE SSE2}` for every FPC i386 unit. Single and
Double arithmetic moves to SSE2. `Extended` stays the 80-bit x87 type, and
functions still return floating-point results in `ST(0)`, so the calling
convention and the prebuilt RTL do not change. FPC's `SetExceptionMask` and
`SetRoundMode` set both units, and its Win32 exception handler translates
SSE2 traps.

## Consequences

Double arithmetic on FPC's `i386-win32` build rounds once and returns the
same bits as the other targets. The build needs a CPU with SSE2: a
Pentium 4, Athlon 64 or later. A 32-bit Windows 8 or later requires SSE2 to
install, so only Windows 7 or XP on an older CPU loses the build. FPC's
i386 `GetRoundMode` reads the x87 control word, not MXCSR, so the decimal
parser's inexact fast path stays off on 32-bit x86. Delphi's Win32 compiler
generates only x87 code, so its build still rounds a subnormal product or
quotient twice.
