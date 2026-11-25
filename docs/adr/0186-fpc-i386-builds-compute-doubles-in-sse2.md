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

SSE2 arithmetic follows MXCSR, which FPC's setters change only in part: they
leave its flush-to-zero and denormals-are-zero bits as the caller set them,
on i386 they read the rounding mode and exception mask back from the x87
control word, and they also store each value in the process-wide defaults
that threads started afterwards load. On FPC's x86 and x86-64 builds,
`EnterGocciaFloatingPointScope` therefore saves the x87 control word and
MXCSR and loads the engine's settings into the calling thread's registers
itself, flush-to-zero and denormals-are-zero cleared, and
`LeaveGocciaFloatingPointScope` loads the saved values back, MXCSR's status
flags included. On FPC's AArch64 builds, the scope saves FPCR, clears its FZ
bit and restores it.

## Consequences

Double arithmetic on FPC's `i386-win32` build rounds once and returns the
same bits as the other targets. Every FPC i386 build needs a CPU with SSE2:
a Pentium 4, Athlon 64 or later. A 32-bit Windows 8 or later requires SSE2
to install, so only Windows 7 or XP on an older CPU loses the `i386-win32`
build. In FPC's x86, x86-64 and AArch64 builds, a host that runs with
flush-to-zero set still gets gradual underflow inside the engine, and a
thread it starts during an engine call no longer inherits the engine's
settings. Delphi builds keep the mask, rounding and precision setters alone. FPC's
i386 `GetRoundMode` reads the x87 control word, not MXCSR, so the decimal
parser's inexact fast path stays off on 32-bit x86. Delphi's Win32 compiler
generates only x87 code, so its build still rounds a subnormal product or
quotient twice.
