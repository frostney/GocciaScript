unit Goccia.FloatingPoint;

{$I Goccia.inc}

interface

uses
  Math;

type
  {$IFDEF FPC}
  TGocciaExceptionMask = TFPUExceptionMask;
  TGocciaRoundingMode = TFPURoundingMode;
  {$ELSE}
  TGocciaExceptionMask = TArithmeticExceptionMask;
  TGocciaRoundingMode = TRoundingMode;
  {$ENDIF}

  TGocciaFloatingPointState = record
    ExceptionMask: TGocciaExceptionMask;
    RoundingMode: TGocciaRoundingMode;
    PrecisionMode: TFPUPrecisionMode;
    {$IFDEF GOCCIA_FLUSH_TO_ZERO_CONTROL}
    { The caller's whole SSE control and status register (MXCSR) on x86, or
      its floating-point control register (FPCR) on AArch64. }
    VectorControl: LongWord;
    {$ENDIF}
  end;

{$IF DEFINED(FPC) AND (DEFINED(GOCCIA_CPU_X86_64) OR DEFINED(GOCCIA_CPU_X86))}
const
  { MXCSR bits that replace a subnormal result or operand with zero. }
  MXCSR_FLUSH_TO_ZERO = LongWord($8000);
  MXCSR_DENORMALS_ARE_ZERO = LongWord($0040);
{$ELSEIF DEFINED(FPC) AND DEFINED(GOCCIA_CPU_AARCH64)}
const
  { FPCR.FZ replaces a subnormal result or operand with zero. }
  FPCR_FLUSH_TO_ZERO = LongWord(1) shl 24;

function GetFPCR: LongWord;
procedure SetFPCR(const AValue: LongWord);
{$ENDIF}

procedure EnterGocciaFloatingPointScope(
  out AState: TGocciaFloatingPointState);
procedure LeaveGocciaFloatingPointScope(
  const AState: TGocciaFloatingPointState);

implementation

{$IF DEFINED(FPC) AND DEFINED(GOCCIA_CPU_AARCH64)}
function GetFPCR: LongWord; assembler; nostackframe;
asm
  mrs x0, fpcr
end;

procedure SetFPCR(const AValue: LongWord); assembler; nostackframe;
asm
  // A 32-bit argument leaves x0's upper half unspecified; FPCR's is RES0.
  mov w0, w0
  msr fpcr, x0
end;
{$ENDIF}

{ ECMAScript arithmetic is IEEE 754 binary64 with gradual underflow, so the
  scope also clears the flush-to-zero modes a host may have set: the
  subnormal results a run computes would otherwise come out zero. FPC's
  setters leave those bits alone, and on i386 they read the rounding mode and
  exception mask from the x87 control word only, so the scope saves the
  vector unit's register itself and restores it whole. }
procedure EnterGocciaFloatingPointScope(
  out AState: TGocciaFloatingPointState);
begin
  AState.ExceptionMask := GetExceptionMask;
  AState.RoundingMode := GetRoundMode;
  AState.PrecisionMode := GetPrecisionMode;
  {$IF DEFINED(FPC) AND (DEFINED(GOCCIA_CPU_X86_64) OR DEFINED(GOCCIA_CPU_X86))}
  AState.VectorControl := GetMXCSR;
  {$ELSEIF DEFINED(FPC) AND DEFINED(GOCCIA_CPU_AARCH64)}
  AState.VectorControl := GetFPCR;
  {$ENDIF}

  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow,
    exUnderflow, exPrecision]);
  SetRoundMode(rmNearest);
  SetPrecisionMode(pmDouble);
  {$IF DEFINED(FPC) AND (DEFINED(GOCCIA_CPU_X86_64) OR DEFINED(GOCCIA_CPU_X86))}
  SetMXCSR(GetMXCSR and not (MXCSR_FLUSH_TO_ZERO or MXCSR_DENORMALS_ARE_ZERO));
  {$ELSEIF DEFINED(FPC) AND DEFINED(GOCCIA_CPU_AARCH64)}
  SetFPCR(GetFPCR and not FPCR_FLUSH_TO_ZERO);
  {$ENDIF}
end;

procedure LeaveGocciaFloatingPointScope(
  const AState: TGocciaFloatingPointState);
begin
  SetPrecisionMode(AState.PrecisionMode);
  SetRoundMode(AState.RoundingMode);
  SetExceptionMask(AState.ExceptionMask);
  { Last, so the caller gets back its own vector rounding mode, exception
    mask and flush-to-zero modes (and on x86 its MXCSR status flags), not
    the x87 settings copied over them. }
  {$IF DEFINED(FPC) AND (DEFINED(GOCCIA_CPU_X86_64) OR DEFINED(GOCCIA_CPU_X86))}
  SetMXCSR(AState.VectorControl);
  {$ELSEIF DEFINED(FPC) AND DEFINED(GOCCIA_CPU_AARCH64)}
  SetFPCR(AState.VectorControl);
  {$ENDIF}
end;

end.
