program Goccia.Engine.AsyncDelegation.Test;

{$I Goccia.inc}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes,

  TestingPascalLibrary,

  Goccia.Engine,
  Goccia.TestSetup,
  Goccia.Values.Primitives;

type
  TAsyncDelegationTests = class(TTestSuite)
  private
    procedure TestDelegateSettlesOnLaterHostEntry;
    procedure TestDelegateResultMessage;
  public
    procedure SetupTests; override;
  end;

procedure TAsyncDelegationTests.SetupTests;
begin
  Test('An async delegate rejection identifies its non-object result',
    TestDelegateResultMessage);
  Test('An async delegate remains pending until a later host entry settles it',
    TestDelegateSettlesOnLaterHostEntry);
end;

procedure TAsyncDelegationTests.TestDelegateSettlesOnLaterHostEntry;
var
  Engine: TGocciaEngine;
  Source: TStringList;
  Stage: TGocciaGlobalEvalStage;
begin
  Source := TStringList.Create;
  Engine := TGocciaEngine.Create('<async-delegation-test>', Source);
  try
    Engine.EvaluateGlobalEval(
      'globalThis.status = "pending";' +
      'globalThis.settle = undefined;' +
      'const source = {' +
      '  [Symbol.asyncIterator]() { return this; },' +
      '  next() { return new Promise((resolve) => { globalThis.settle = resolve; }); }' +
      '};' +
      'const it = ({ async *g() { yield* source; } }).g();' +
      'it.next().then((r) => { globalThis.status = r.value; },' +
      '  () => { globalThis.status = "rejected"; });',
      '<async-delegation-start>', Stage);
    Engine.WaitForRuntimeIdle;
    Expect<string>(Engine.EvaluateGlobalEval('globalThis.status;',
      '<async-delegation-pending>', Stage).ToStringLiteral.Value).ToBe('pending');
    Engine.EvaluateGlobalEval('globalThis.settle({ value: "fulfilled", done: false });',
      '<async-delegation-settle>', Stage);
    Engine.WaitForRuntimeIdle;
    Expect<string>(Engine.EvaluateGlobalEval('globalThis.status;',
      '<async-delegation-result>', Stage).ToStringLiteral.Value).ToBe('fulfilled');
  finally
    Engine.Free;
    Source.Free;
  end;
end;

procedure TAsyncDelegationTests.TestDelegateResultMessage;
var
  Engine: TGocciaEngine;
  Source: TStringList;
  Stage: TGocciaGlobalEvalStage;
begin
  Source := TStringList.Create;
  Engine := TGocciaEngine.Create('<async-delegation-message>', Source);
  try
    Engine.EvaluateGlobalEval(
      'globalThis.message = "pending";' +
      'const source = {' +
      '  [Symbol.asyncIterator]() { return this; },' +
      '  next() { return Promise.resolve(3); }' +
      '};' +
      '({ async *g() { yield* source; } }).g().next().catch((error) => {' +
      '  globalThis.message = error.name + ": " + error.message;' +
      '});', '<async-delegation-message>', Stage);
    Engine.WaitForRuntimeIdle;
    Expect<string>(Engine.EvaluateGlobalEval('globalThis.message;',
      '<async-delegation-message-result>', Stage).ToStringLiteral.Value)
      .ToBe('TypeError: Iterator result 3 is not an object');
  finally
    Engine.Free;
    Source.Free;
  end;
end;

begin
  TestRunnerProgram.AddSuite(TAsyncDelegationTests.Create('Async delegation'));
  TestRunnerProgram.Run;
end.
