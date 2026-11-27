program Goccia.EngineFault.Test;

{$I Goccia.inc}

uses
  SysUtils,

  TestingPascalLibrary,

  Goccia.EngineFault;

type
  TEngineFaultTests = class(TTestSuite)
  private
    procedure TestFaultFamily;
    procedure TestConvertibleClasses;
    procedure TestDescendantsInherit;
    procedure TestRuntimeErrorRaises;
  public
    procedure SetupTests; override;
  end;

  { an exception a host or the engine raises for an ordinary failure }
  EOrdinaryFailure = class(Exception);

procedure TEngineFaultTests.SetupTests;
begin
  Test('the fault family is what every handler must stop on', TestFaultFamily);
  Test('a refusal, a limit and a domain error stay ordinary failures', TestConvertibleClasses);
  Test('a descendant of a family member is a fault too', TestDescendantsInherit);
  Test('an engine check raises the mapped exception instead of halting', TestRuntimeErrorRaises);
end;

function Fault(const AException: Exception): Boolean;
begin
  try
    Result := IsEngineFault(AException);
  finally
    AException.Free;
  end;
end;

procedure TEngineFaultTests.TestFaultFamily;
begin
  Expect<Boolean>(Fault(EObjectCheck.Create('x'))).ToBe(True);
  Expect<Boolean>(Fault(EAccessViolation.Create('x'))).ToBe(True);
  Expect<Boolean>(Fault(EInvalidPointer.Create('x'))).ToBe(True);
  Expect<Boolean>(Fault(EDivByZero.Create('x'))).ToBe(True);
  Expect<Boolean>(Fault(EPrivilege.Create('x'))).ToBe(True);
  Expect<Boolean>(Fault(EExternalException.Create('x'))).ToBe(True);
  Expect<Boolean>(Fault(EAssertionFailed.Create('x'))).ToBe(True);
  Expect<Boolean>(Fault(EAbstractError.Create('x'))).ToBe(True);
  Expect<Boolean>(Fault(EStackOverflow.Create('x'))).ToBe(True);
end;

procedure TEngineFaultTests.TestConvertibleClasses;
begin
  Expect<Boolean>(Fault(EOutOfMemory.Create('x'))).ToBe(False);
  Expect<Boolean>(Fault(ERangeError.Create('x'))).ToBe(False);
  Expect<Boolean>(Fault(EIntOverflow.Create('x'))).ToBe(False);
  Expect<Boolean>(Fault(EConvertError.Create('x'))).ToBe(False);
  Expect<Boolean>(Fault(EInvalidOpException.Create('x'))).ToBe(False);
  Expect<Boolean>(Fault(EInvalidCast.Create('x'))).ToBe(False);
  Expect<Boolean>(Fault(EOrdinaryFailure.Create('x'))).ToBe(False);
  Expect<Boolean>(Fault(Exception.Create('x'))).ToBe(False);
end;

procedure TEngineFaultTests.TestDescendantsInherit;
begin
  { EBusError, a misaligned or unmapped access, descends from EAccessViolation }
  Expect<Boolean>(Fault(EBusError.Create('x'))).ToBe(True);
end;

procedure TEngineFaultTests.TestRuntimeErrorRaises;
var
  Raised: string;
begin
  Raised := '';
  try
    RaiseRuntimeError(204);
  except
    on E: Exception do
      Raised := E.ClassName;
  end;
  Expect<string>(Raised).ToBe('EInvalidPointer');
  Raised := '';
  try
    RaiseRuntimeError(202);
  except
    on E: Exception do
      Raised := E.ClassName;
  end;
  Expect<string>(Raised).ToBe('EStackOverflow');
end;

begin
  TestRunnerProgram.AddSuite(TEngineFaultTests.Create('Goccia.EngineFault'));
  TestRunnerProgram.Run;

  ExitCode := TestResultToExitCode;
end.
