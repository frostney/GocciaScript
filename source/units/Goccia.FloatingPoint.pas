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
    {$IF DEFINED(FPC) AND (DEFINED(GOCCIA_CPU_X86_64) OR DEFINED(GOCCIA_CPU_X86))}
    { The caller's x87 control word and its whole SSE control and status
      register (MXCSR). }
    ControlWord: Word;
    VectorControl: LongWord;
    {$ELSE}
    ExceptionMask: TGocciaExceptionMask;
    RoundingMode: TGocciaRoundingMode;
    PrecisionMode: TFPUPrecisionMode;
    {$IFDEF GOCCIA_FLUSH_TO_ZERO_CONTROL}
    { The caller's floating-point control register (FPCR). }
    VectorControl: LongWord;
    {$ENDIF}
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

{ ECMAScript arithmetic is IEEE 754 binary64 with gradual underflow, so the
  scope also clears the flush-to-zero modes a host may have set: the
  subnormal results a run computes would otherwise come out zero. }

{$IF DEFINED(FPC) AND (DEFINED(GOCCIA_CPU_X86_64) OR DEFINED(GOCCIA_CPU_X86))}
const
  X87_EXCEPTION_MASKS = Word($003F);
  X87_PRECISION_CONTROL = Word($0300);
  X87_DOUBLE_PRECISION = Word($0200);
  X87_ROUNDING_CONTROL = Word($0C00);
  MXCSR_EXCEPTION_MASKS = LongWord($1F80);
  MXCSR_ROUNDING_CONTROL = LongWord($6000);

{ FPC's Set8087CW and SetMXCSR, and the mask, rounding and precision setters
  built on them, also store the value in Default8087CW and DefaultMXCSR,
  which every thread the process starts afterwards loads. The scope's
  settings belong to the calling thread alone, so it loads the registers
  itself. As in Set8087CW, fnclex first clears pending x87 exceptions, so
  unmasking one cannot raise it. }
procedure LoadControlWord(const AValue: Word);
var
  Value: Word;
begin
  Value := AValue;
  asm
    fnclex
    fldcw Value
  end;
end;

procedure LoadVectorControl(const AValue: LongWord);
var
  Value: LongWord;
begin
  Value := AValue;
  asm
    ldmxcsr Value
  end;
end;

{ Nearest rounding, double precision and every exception masked, on both
  units; flush-to-zero and denormals-are-zero off. The caller gets back its
  exact registers, MXCSR's status flags included. }
procedure EnterGocciaFloatingPointScope(
  out AState: TGocciaFloatingPointState);
begin
  AState.ControlWord := Get8087CW;
  AState.VectorControl := GetMXCSR;
  LoadControlWord((AState.ControlWord and
    not (X87_ROUNDING_CONTROL or X87_PRECISION_CONTROL)) or
    X87_EXCEPTION_MASKS or X87_DOUBLE_PRECISION);
  LoadVectorControl((AState.VectorControl and not (MXCSR_ROUNDING_CONTROL or
    MXCSR_FLUSH_TO_ZERO or MXCSR_DENORMALS_ARE_ZERO)) or
    MXCSR_EXCEPTION_MASKS);
end;

procedure LeaveGocciaFloatingPointScope(
  const AState: TGocciaFloatingPointState);
begin
  LoadControlWord(AState.ControlWord);
  LoadVectorControl(AState.VectorControl);
end;
{$ELSE}
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

procedure EnterGocciaFloatingPointScope(
  out AState: TGocciaFloatingPointState);
begin
  AState.ExceptionMask := GetExceptionMask;
  AState.RoundingMode := GetRoundMode;
  AState.PrecisionMode := GetPrecisionMode;
  {$IFDEF GOCCIA_FLUSH_TO_ZERO_CONTROL}
  AState.VectorControl := GetFPCR;
  {$ENDIF}

  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow,
    exUnderflow, exPrecision]);
  SetRoundMode(rmNearest);
  SetPrecisionMode(pmDouble);
  {$IFDEF GOCCIA_FLUSH_TO_ZERO_CONTROL}
  SetFPCR(GetFPCR and not FPCR_FLUSH_TO_ZERO);
  {$ENDIF}
end;

procedure LeaveGocciaFloatingPointScope(
  const AState: TGocciaFloatingPointState);
begin
  SetPrecisionMode(AState.PrecisionMode);
  SetRoundMode(AState.RoundingMode);
  SetExceptionMask(AState.ExceptionMask);
  { Last, so the caller also gets back its own flush-to-zero mode. }
  {$IFDEF GOCCIA_FLUSH_TO_ZERO_CONTROL}
  SetFPCR(AState.VectorControl);
  {$ENDIF}
end;
{$ENDIF}

end.
