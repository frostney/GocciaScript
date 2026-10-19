program Goccia.Builtins.Atomics.Test;

{$I Goccia.inc}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes,
  SysUtils,

  TestingPascalLibrary,

  Goccia.Arguments.Collection,
  Goccia.Builtins.Atomics,
  Goccia.Engine,
  Goccia.TestSetup,
  Goccia.Values.FunctionBase,
  Goccia.Values.Primitives;

const
  { One agent waits with a 1 ms timeout. Nothing else can notify while it is
    suspended, so the suspension always ends by timing out; the seam then
    decides whether a notify lands before the wait re-enters the critical
    section. The script's value is "<wait result>:<notify count>". }
  NOTIFY_FROM_SEAM =
    'const view = new Int32Array(new SharedArrayBuffer(4));' + LineEnding +
    'let notified = -1;' + LineEnding +
    'globalThis.notifyNow = () => { notified = Atomics.notify(view, 0); };' +
    LineEnding +
    'const waited = Atomics.wait(view, 0, 0, 1);' + LineEnding +
    '`${waited}:${notified}`;';
  NOTIFY_AFTER_WAIT =
    'const view = new Int32Array(new SharedArrayBuffer(4));' + LineEnding +
    'const waited = Atomics.wait(view, 0, 0, 1);' + LineEnding +
    'const notified = Atomics.notify(view, 0);' + LineEnding +
    '`${waited}:${notified}`;';

var
  GEngine: TGocciaEngine;
  GSeamCalls: Integer;

{ Runs the script's notifyNow on the waiting agent, between the end of the
  suspension and the wait's critical section. }
procedure NotifyFromSeam;
var
  Arguments: TGocciaArgumentsCollection;
begin
  Inc(GSeamCalls);
  Arguments := TGocciaArgumentsCollection.Create;
  try
    (GEngine.GlobalScope.GetValue('notifyNow') as TGocciaFunctionBase)
      .Call(Arguments, TGocciaUndefinedLiteralValue.UndefinedValue);
  finally
    Arguments.Free;
  end;
end;

type
  TAtomicsWaitNotifyOrderTests = class(TTestSuite)
  private
    function Run(const ASource: string): string;
    procedure TestNotifyBeforeCriticalSectionIsOk;
    procedure TestNotifyAfterTimeoutCountsNoWaiter;
  protected
    procedure BeforeEach; override;
  public
    procedure SetupTests; override;
  end;

procedure TAtomicsWaitNotifyOrderTests.SetupTests;
begin
  Test('a notify after the suspension timed out but before the wait ' +
    'decides its result wakes the waiter: wait is "ok", notify counts 1',
    TestNotifyBeforeCriticalSectionIsOk);
  Test('a notify after the wait decided it timed out counts no waiter: ' +
    'wait is "timed-out", notify counts 0',
    TestNotifyAfterTimeoutCountsNoWaiter);
end;

procedure TAtomicsWaitNotifyOrderTests.BeforeEach;
begin
  inherited BeforeEach;
  SetAtomicsWaitSuspensionEndedHookForTesting(nil);
  GSeamCalls := 0;
end;

function TAtomicsWaitNotifyOrderTests.Run(const ASource: string): string;
var
  Source: TStringList;
begin
  Source := TStringList.Create;
  GEngine := nil;
  try
    Source.Text := ASource;
    GEngine := TGocciaEngine.Create('<atomics-wait-order>', Source);
    Result := (GEngine.Execute.Result as TGocciaStringLiteralValue).Value;
  finally
    GEngine.Free;
    GEngine := nil;
    Source.Free;
  end;
end;

procedure TAtomicsWaitNotifyOrderTests.TestNotifyBeforeCriticalSectionIsOk;
begin
  SetAtomicsWaitSuspensionEndedHookForTesting(@NotifyFromSeam);
  try
    Expect<string>(Run(NOTIFY_FROM_SEAM)).ToBe('ok:1');
  finally
    SetAtomicsWaitSuspensionEndedHookForTesting(nil);
  end;
  Expect<Integer>(GSeamCalls).ToBe(1);
end;

procedure TAtomicsWaitNotifyOrderTests.TestNotifyAfterTimeoutCountsNoWaiter;
begin
  Expect<string>(Run(NOTIFY_AFTER_WAIT)).ToBe('timed-out:0');
end;

begin
  TestRunnerProgram.AddSuite(TAtomicsWaitNotifyOrderTests.Create(
    'Atomics.wait and notify order'));
  RunGocciaTests;
  ExitCode := TestResultToExitCode;
end.
