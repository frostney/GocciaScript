program Goccia.Compliance.Workers.Test;

{$I Goccia.inc}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  Process,
  SysUtils,

  ProcessorDetection,
  TestingPascalLibrary,

  Goccia.CLI.Application,
  Goccia.Compliance;

type
  TWorkerTestApplication = class(TGocciaCLIApplication)
  protected
    procedure Configure; override;
    function UsageLine: string; override;
    procedure ExecuteWithPaths(const APaths: TStringList); override;
  public
    function JobsForFiles(const AFileCount: Integer): Integer;
  end;

  TComplianceWorkerTests = class(TTestSuite)
  private
    procedure TestPositiveWorkerCount;
    {$IFDEF DARWIN}
    procedure TestMacOSWorkerCount;
    {$ENDIF}
  public
    procedure SetupTests; override;
  end;

procedure TWorkerTestApplication.Configure;
begin
end;

function TWorkerTestApplication.UsageLine: string;
begin
  Result := 'worker-count-test';
end;

procedure TWorkerTestApplication.ExecuteWithPaths(const APaths: TStringList);
begin
end;

function TWorkerTestApplication.JobsForFiles(const AFileCount: Integer): Integer;
begin
  Result := GetJobCount(AFileCount);
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
  Application: TWorkerTestApplication;
begin
  Expect<Boolean>(RunCommand('/usr/bin/getconf', ['NPROCESSORS_ONLN'], Output)).ToBe(True);
  ProcessorCount := StrToInt(Trim(string(Output)));
  Expect<Boolean>(ProcessorCount > 0).ToBe(True);
  Expect<Integer>(GetProcessorCount).ToBe(ProcessorCount);
  Expect<Integer>(DefaultComplianceJobs).ToBe(ProcessorCount);
  Application := TWorkerTestApplication.Create('worker-count-test');
  try
    Expect<Integer>(Application.JobsForFiles(ProcessorCount + 1)).ToBe(ProcessorCount);
    Expect<Integer>(Application.JobsForFiles(1)).ToBe(1);
  finally
    Application.Free;
  end;
end;
{$ENDIF}

begin
  TestRunnerProgram.AddSuite(TComplianceWorkerTests.Create('Compliance worker defaults'));
  TestRunnerProgram.Run;
  ExitCode := TestResultToExitCode;
end.
