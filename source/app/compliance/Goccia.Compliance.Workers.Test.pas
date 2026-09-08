program Goccia.Compliance.Workers.Test;

{$I Goccia.inc}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Process,
  SysUtils,

  ProcessorDetection,
  TestingPascalLibrary,

  Goccia.Compliance;

type
  TComplianceWorkerTests = class(TTestSuite)
  private
    procedure TestPositiveWorkerCount;
    {$IFDEF DARWIN}
    procedure TestMacOSWorkerCount;
    {$ENDIF}
  public
    procedure SetupTests; override;
  end;

procedure TComplianceWorkerTests.SetupTests;
begin
  Test('Default worker counts are positive', TestPositiveWorkerCount);
  {$IFDEF DARWIN}
  Test('CLI and compliance defaults match macOS online CPUs', TestMacOSWorkerCount);
  {$ENDIF}
end;

procedure TComplianceWorkerTests.TestPositiveWorkerCount;
begin
  Expect<Boolean>(GetProcessorCount > 0).ToBe(True);
  Expect<Boolean>(DefaultComplianceJobs > 0).ToBe(True);
end;

{$IFDEF DARWIN}
procedure TComplianceWorkerTests.TestMacOSWorkerCount;
var
  Output: AnsiString;
  ProcessorCount: Integer;
begin
  Expect<Boolean>(RunCommand('/usr/bin/getconf', ['NPROCESSORS_ONLN'], Output)).ToBe(True);
  ProcessorCount := StrToInt(Trim(string(Output)));
  Expect<Boolean>(ProcessorCount > 0).ToBe(True);
  Expect<Integer>(GetProcessorCount).ToBe(ProcessorCount);
  Expect<Integer>(DefaultComplianceJobs).ToBe(ProcessorCount);
end;
{$ENDIF}

begin
  TestRunnerProgram.AddSuite(TComplianceWorkerTests.Create('Compliance worker defaults'));
  TestRunnerProgram.Run;
  ExitCode := TestResultToExitCode;
end.
