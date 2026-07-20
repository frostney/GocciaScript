program Goccia.FloatingPoint.Test;

{$I Goccia.inc}

uses
  Math,

  TestingPascalLibrary,

  Goccia.FloatingPoint;

type
  TFloatingPointTests = class(TTestSuite)
  private
    procedure TestScopeConfiguresAndRestoresEnvironment;
  public
    procedure SetupTests; override;
  end;

procedure TFloatingPointTests.SetupTests;
begin
  Test('scope configures and restores the floating-point environment',
    TestScopeConfiguresAndRestoresEnvironment);
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

begin
  TestRunnerProgram.AddSuite(
    TFloatingPointTests.Create('Goccia.FloatingPoint'));
  TestRunnerProgram.Run;

  ExitCode := TestResultToExitCode;
end.
