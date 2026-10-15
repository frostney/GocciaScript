program Goccia.Engine.IdleWait.Test;

{$I Goccia.inc}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes,
  SysUtils,

  TestingPascalLibrary,
  TimingUtils,

  Goccia.Engine,
  Goccia.Executor.Bytecode,
  Goccia.InstructionLimit,
  Goccia.SourcePipeline,
  Goccia.TestSetup,
  Goccia.Timeout;

const
  { The entry suspends on a waiter that settles itself only after 5 000 ms,
    so a run that ends well before that was ended by the host. }
  PROMPT_END_MS = 2000;
  RUN_DEADLINE_MS = 200;
  WAIT_ON_A_WAITER_WITH_A_TIMEOUT =
    'const view = new Int32Array(new SharedArrayBuffer(4));' + LineEnding +
    'await Atomics.waitAsync(view, 0, 0, 5000).value;';

type
  { Stands in for a host that stops the run while the engine is idle: its
    pass spends the instruction budget, which is what a host stop does. }
  TSpendBudgetExtension = class(TGocciaEngineExtension)
  public
    procedure WaitForIdle; override;
  end;

  TEngineIdleWaitTests = class(TTestSuite)
  private
    function RunEntry(const AStopFromExtension: Boolean;
      out AElapsedMilliseconds: Int64): string;
    procedure TestHostStopEndsAtomicsWait;
    procedure TestDeadlineEndsAtomicsWait;
  protected
    procedure BeforeEach; override;
    procedure AfterEach; override;
  public
    procedure SetupTests; override;
  end;

procedure TSpendBudgetExtension.WaitForIdle;
begin
  StartInstructionLimit(1);
  IncrementInstructionCounter;
end;

procedure TEngineIdleWaitTests.SetupTests;
begin
  Test('a host stop ends the idle wait on an Atomics.waitAsync timeout',
    TestHostStopEndsAtomicsWait);
  Test('the run deadline ends the idle wait on an Atomics.waitAsync timeout',
    TestDeadlineEndsAtomicsWait);
end;

procedure TEngineIdleWaitTests.BeforeEach;
begin
  inherited BeforeEach;
  ClearInstructionLimit;
  ClearExecutionTimeout;
end;

procedure TEngineIdleWaitTests.AfterEach;
begin
  ClearInstructionLimit;
  ClearExecutionTimeout;
  inherited AfterEach;
end;

{ Runs a module entry that suspends on an Atomics.waitAsync timeout and
  returns the class name of the exception that ended it, or '' when it
  finished. }
function TEngineIdleWaitTests.RunEntry(const AStopFromExtension: Boolean;
  out AElapsedMilliseconds: Int64): string;
var
  Engine: TGocciaEngine;
  Executor: TGocciaBytecodeExecutor;
  Source: TStringList;
  Started: Int64;
begin
  Result := '';
  Source := TStringList.Create;
  Executor := TGocciaBytecodeExecutor.Create;
  Engine := nil;
  try
    Source.Text := WAIT_ON_A_WAITER_WITH_A_TIMEOUT;
    Engine := TGocciaEngine.Create('<idle-wait>', Source, Executor);
    Engine.SourceType := stModule;
    if AStopFromExtension then
      Engine.AddExtension(TSpendBudgetExtension.Create);
    Started := GetMilliseconds;
    try
      Engine.Execute;
    except
      on E: Exception do
        Result := E.ClassName;
    end;
    AElapsedMilliseconds := GetMilliseconds - Started;
  finally
    { The extension's spent budget outlives the run on this thread. }
    ClearInstructionLimit;
    Engine.Free;
    Executor.Free;
    Source.Free;
  end;
end;

procedure TEngineIdleWaitTests.TestHostStopEndsAtomicsWait;
var
  Elapsed: Int64;
begin
  Expect<string>(RunEntry(True, Elapsed))
    .ToBe(TGocciaInstructionLimitError.ClassName);
  Expect<Boolean>(Elapsed < PROMPT_END_MS).ToBe(True);
end;

procedure TEngineIdleWaitTests.TestDeadlineEndsAtomicsWait;
var
  Elapsed: Int64;
begin
  StartExecutionTimeout(RUN_DEADLINE_MS);
  Expect<string>(RunEntry(False, Elapsed))
    .ToBe(TGocciaTimeoutError.ClassName);
  Expect<Boolean>(Elapsed < PROMPT_END_MS).ToBe(True);
end;

begin
  TestRunnerProgram.AddSuite(TEngineIdleWaitTests.Create(
    'Engine idle wait'));
  RunGocciaTests;
  ExitCode := TestResultToExitCode;
end.
