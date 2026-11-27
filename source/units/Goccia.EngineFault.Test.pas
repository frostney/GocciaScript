program Goccia.EngineFault.Test;

{$I Goccia.inc}

uses
  SysUtils,

  TestingPascalLibrary,

  Goccia.EngineFault;

type
  TEngineFaultTests = class(TTestSuite)
  private
    procedure TestIntegrityFamily;
    procedure TestConvertibleClasses;
    procedure TestHostFamily;
    procedure TestDescendantsInherit;
  public
    procedure SetupTests; override;
  end;

  { an exception a host or the engine raises for an ordinary failure }
  EOrdinaryFailure = class(Exception);

procedure TEngineFaultTests.SetupTests;
begin
  Test('the integrity family is what a script boundary must re-raise', TestIntegrityFamily);
  Test('a refusal, a limit and a domain error stay convertible', TestConvertibleClasses);
  Test('a host also stops on a stack overflow that got past the engine', TestHostFamily);
  Test('a descendant of a family member is a fault too', TestDescendantsInherit);
end;

function IntegrityFault(const AException: Exception): Boolean;
begin
  try
    Result := IsEngineIntegrityFault(AException);
  finally
    AException.Free;
  end;
end;

function HostFault(const AException: Exception): Boolean;
begin
  try
    Result := IsEngineFault(AException);
  finally
    AException.Free;
  end;
end;

procedure TEngineFaultTests.TestIntegrityFamily;
begin
  Expect<Boolean>(IntegrityFault(EObjectCheck.Create('x'))).ToBe(True);
  Expect<Boolean>(IntegrityFault(EAccessViolation.Create('x'))).ToBe(True);
  Expect<Boolean>(IntegrityFault(EInvalidPointer.Create('x'))).ToBe(True);
  Expect<Boolean>(IntegrityFault(EDivByZero.Create('x'))).ToBe(True);
  Expect<Boolean>(IntegrityFault(EPrivilege.Create('x'))).ToBe(True);
  Expect<Boolean>(IntegrityFault(EExternalException.Create('x'))).ToBe(True);
  Expect<Boolean>(IntegrityFault(EAssertionFailed.Create('x'))).ToBe(True);
  Expect<Boolean>(IntegrityFault(EAbstractError.Create('x'))).ToBe(True);
end;

procedure TEngineFaultTests.TestConvertibleClasses;
begin
  Expect<Boolean>(IntegrityFault(EStackOverflow.Create('x'))).ToBe(False);
  Expect<Boolean>(IntegrityFault(EOutOfMemory.Create('x'))).ToBe(False);
  Expect<Boolean>(IntegrityFault(ERangeError.Create('x'))).ToBe(False);
  Expect<Boolean>(IntegrityFault(EIntOverflow.Create('x'))).ToBe(False);
  Expect<Boolean>(IntegrityFault(EConvertError.Create('x'))).ToBe(False);
  Expect<Boolean>(IntegrityFault(EInvalidOpException.Create('x'))).ToBe(False);
  Expect<Boolean>(IntegrityFault(EInvalidCast.Create('x'))).ToBe(False);
  Expect<Boolean>(IntegrityFault(EOrdinaryFailure.Create('x'))).ToBe(False);
  Expect<Boolean>(IntegrityFault(Exception.Create('x'))).ToBe(False);
end;

procedure TEngineFaultTests.TestHostFamily;
begin
  Expect<Boolean>(HostFault(EAccessViolation.Create('x'))).ToBe(True);
  Expect<Boolean>(HostFault(EAssertionFailed.Create('x'))).ToBe(True);
  Expect<Boolean>(HostFault(EStackOverflow.Create('x'))).ToBe(True);
  Expect<Boolean>(HostFault(EOutOfMemory.Create('x'))).ToBe(False);
  Expect<Boolean>(HostFault(EConvertError.Create('x'))).ToBe(False);
  Expect<Boolean>(HostFault(EOrdinaryFailure.Create('x'))).ToBe(False);
  Expect<Boolean>(HostFault(Exception.Create('x'))).ToBe(False);
end;

procedure TEngineFaultTests.TestDescendantsInherit;
begin
  { EBusError, a misaligned or unmapped access, descends from EAccessViolation }
  Expect<Boolean>(IntegrityFault(EBusError.Create('x'))).ToBe(True);
  Expect<Boolean>(HostFault(EBusError.Create('x'))).ToBe(True);
end;

begin
  TestRunnerProgram.AddSuite(TEngineFaultTests.Create('Goccia.EngineFault'));
  TestRunnerProgram.Run;

  ExitCode := TestResultToExitCode;
end.
