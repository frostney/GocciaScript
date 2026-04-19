program MemoryDetection.Test;

{$I Shared.inc}

uses
  SysUtils,

  MemoryDetection,
  TestingPascalLibrary;

type
  TMemoryDetectionTests = class(TTestSuite)
  private
    procedure TestGetAvailableMemoryBytesIsPositive;
    procedure TestGetAvailableMemoryBytesReasonableMinimum;
    procedure TestGetAvailableMemoryBytesReasonableMaximum;
  public
    procedure SetupTests; override;
  end;

const
  ONE_HUNDRED_MB = Int64(100) * 1024 * 1024;
  ONE_HUNDRED_TB = Int64(100) * 1024 * 1024 * 1024 * 1024;

procedure TMemoryDetectionTests.SetupTests;
begin
  Test('GetAvailableMemoryBytes returns positive value', TestGetAvailableMemoryBytesIsPositive);
  Test('GetAvailableMemoryBytes is at least 100 MB', TestGetAvailableMemoryBytesReasonableMinimum);
  Test('GetAvailableMemoryBytes is below 100 TB', TestGetAvailableMemoryBytesReasonableMaximum);
end;

procedure TMemoryDetectionTests.TestGetAvailableMemoryBytesIsPositive;
begin
  Expect<Boolean>(GetAvailableMemoryBytes > 0).ToBe(True);
end;

procedure TMemoryDetectionTests.TestGetAvailableMemoryBytesReasonableMinimum;
begin
  Expect<Boolean>(GetAvailableMemoryBytes >= ONE_HUNDRED_MB).ToBe(True);
end;

procedure TMemoryDetectionTests.TestGetAvailableMemoryBytesReasonableMaximum;
begin
  Expect<Boolean>(GetAvailableMemoryBytes < ONE_HUNDRED_TB).ToBe(True);
end;

begin
  TestRunnerProgram.AddSuite(TMemoryDetectionTests.Create('MemoryDetection'));
  TestRunnerProgram.Run;
  ExitCode := TestResultToExitCode;
end.
