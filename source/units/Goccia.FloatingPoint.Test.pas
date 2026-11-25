program Goccia.FloatingPoint.Test;

{$I Goccia.inc}

uses
  Math,

  NumberBits,
  TestingPascalLibrary,

  Goccia.FloatingPoint;

type
  TFloatingPointTests = class(TTestSuite)
  private
    procedure TestScopeConfiguresAndRestoresEnvironment;
    {$IFDEF GOCCIA_FLUSH_TO_ZERO_CONTROL}
    procedure TestScopeUnderflowsGraduallyUnderHostFlushToZero;
    {$ENDIF}
  public
    procedure SetupTests; override;
  end;

procedure TFloatingPointTests.SetupTests;
begin
  Test('scope configures and restores the floating-point environment',
    TestScopeConfiguresAndRestoresEnvironment);
  {$IFDEF GOCCIA_FLUSH_TO_ZERO_CONTROL}
  Test('scope underflows gradually under a host''s flush-to-zero mode',
    TestScopeUnderflowsGraduallyUnderHostFlushToZero);
  {$ENDIF}
end;

procedure TFloatingPointTests.TestScopeConfiguresAndRestoresEnvironment;
var
  OriginalMask: TGocciaExceptionMask;
  OriginalRounding: TGocciaRoundingMode;
  OriginalPrecision: TFPUPrecisionMode;
  FloatingPointState: TGocciaFloatingPointState;
begin
  OriginalMask := GetExceptionMask;
  OriginalRounding := GetRoundMode;
  OriginalPrecision := GetPrecisionMode;

  EnterGocciaFloatingPointScope(FloatingPointState);
  try
    Expect<Boolean>(GetExceptionMask = [exInvalidOp, exDenormalized,
      exZeroDivide, exOverflow, exUnderflow, exPrecision]).ToBe(True);
    Expect<Boolean>(GetRoundMode = rmNearest).ToBe(True);
    Expect<Boolean>(GetPrecisionMode = pmDouble).ToBe(True);
  finally
    LeaveGocciaFloatingPointScope(FloatingPointState);
  end;

  Expect<Boolean>(GetExceptionMask = OriginalMask).ToBe(True);
  Expect<Boolean>(GetRoundMode = OriginalRounding).ToBe(True);
  Expect<Boolean>(GetPrecisionMode = OriginalPrecision).ToBe(True);
end;

{$IFDEF GOCCIA_FLUSH_TO_ZERO_CONTROL}
var
  // Globals, so the compiler cannot fold the products below.
  NormalFactor, SmallFactor, SubnormalOperand, ScaleFactor: Double;

{ A host may run with flush-to-zero set, as audio and graphics code often
  does. Inside the scope a subnormal result and a subnormal operand keep
  their values, and leaving it gives the host its exact register back. }
procedure TFloatingPointTests.TestScopeUnderflowsGraduallyUnderHostFlushToZero;
var
  Original, HostControl, Restored: LongWord;
  FloatingPointState: TGocciaFloatingPointState;
  Product, Scaled: Double;
begin
  NormalFactor := LdExp(1.0, -600);
  SmallFactor := LdExp(1.0, -460);
  SubnormalOperand := LdExp(1.0, -1060);
  ScaleFactor := LdExp(1.0, 100);
  {$IF DEFINED(GOCCIA_CPU_AARCH64)}
  Original := GetFPCR;
  HostControl := Original or FPCR_FLUSH_TO_ZERO;
  SetFPCR(HostControl);
  {$ELSE}
  Original := GetMXCSR;
  // Flush-to-zero, denormals-are-zero and rounding toward +Infinity, set in
  // MXCSR alone: on i386 the x87 control word keeps the caller's mode.
  HostControl := (Original and not LongWord($6000)) or LongWord($2000) or
    MXCSR_FLUSH_TO_ZERO or MXCSR_DENORMALS_ARE_ZERO;
  SetMXCSR(HostControl);
  {$ENDIF}
  try
    EnterGocciaFloatingPointScope(FloatingPointState);
    try
      Product := NormalFactor * SmallFactor;
      Scaled := SubnormalOperand * ScaleFactor;
    finally
      LeaveGocciaFloatingPointScope(FloatingPointState);
    end;
    {$IF DEFINED(GOCCIA_CPU_AARCH64)}
    Restored := GetFPCR;
    {$ELSE}
    Restored := GetMXCSR;
    {$ENDIF}
  finally
    {$IF DEFINED(GOCCIA_CPU_AARCH64)}
    SetFPCR(Original);
    {$ELSE}
    SetMXCSR(Original);
    {$ENDIF}
  end;
  // Compared as bits: the host's denormals-are-zero mode, now undone, would
  // also have compared a subnormal equal to zero.
  Expect<UInt64>(DoubleToBits(Product)).ToBe(DoubleToBits(SubnormalOperand));
  Expect<UInt64>(DoubleToBits(Scaled)).ToBe(DoubleToBits(LdExp(1.0, -960)));
  Expect<UInt64>(Restored).ToBe(HostControl);
end;
{$ENDIF}

begin
  TestRunnerProgram.AddSuite(
    TFloatingPointTests.Create('Goccia.FloatingPoint'));
  TestRunnerProgram.Run;

  ExitCode := TestResultToExitCode;
end.
