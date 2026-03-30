program Goccia.Timeout.Test;

{$I Goccia.inc}

uses
  SysUtils,

  GarbageCollector.Generic,
  TestRunner,

  Goccia.TestSetup,
  Goccia.Timeout;

type
  TTimeoutTests = class(TTestSuite)
  private
    procedure TestTimeoutDoesNotTriggerWhenDisabled;
    procedure TestTimeoutTriggersAfterDeadline;
  public
    procedure SetupTests; override;
  end;

procedure TTimeoutTests.SetupTests;
begin
  Test('Timeout does not trigger when disabled', TestTimeoutDoesNotTriggerWhenDisabled);
  Test('Timeout triggers after deadline', TestTimeoutTriggersAfterDeadline);
end;

procedure TTimeoutTests.TestTimeoutDoesNotTriggerWhenDisabled;
begin
  ClearExecutionTimeout;
  CheckExecutionTimeout;
  Expect<Boolean>(True).ToBe(True);
end;

procedure TTimeoutTests.TestTimeoutTriggersAfterDeadline;
var
  DidTimeout: Boolean;
  I: Integer;
begin
  DidTimeout := False;
  StartExecutionTimeout(1);
  Sleep(5);
  try
    for I := 1 to 2048 do
      CheckExecutionTimeout;
  except
    on TGocciaTimeoutError do
      DidTimeout := True;
  end;
  ClearExecutionTimeout;
  Expect<Boolean>(DidTimeout).ToBe(True);
end;

begin
  TGarbageCollector.Initialize;
  try
    TestRunnerProgram.AddSuite(TTimeoutTests.Create('Timeout'));
    TestRunnerProgram.Run;
    ExitCode := TestResultToExitCode;
  finally
    TGarbageCollector.Shutdown;
  end;
end.
