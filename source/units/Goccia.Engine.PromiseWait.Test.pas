program Goccia.Engine.PromiseWait.Test;

{$I Goccia.inc}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes,
  SysUtils,

  HTTPTypes,
  TestingPascalLibrary,
  TimingUtils,

  Goccia.Arguments.Collection,
  Goccia.Capabilities,
  Goccia.Engine,
  Goccia.FetchTransport,
  Goccia.GarbageCollector,
  Goccia.Runtime,
  Goccia.RuntimeExtensions.Fetch,
  Goccia.RuntimeExtensions.TestingLibrary,
  Goccia.Scope.BindingMap,
  Goccia.TestSetup,
  Goccia.Values.Error,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives,
  Goccia.Values.PromiseValue;

const
  API_HOST = 'api.example';
  { The fake transport answers from another thread after this long. }
  ANSWER_DELAY_MS = 50;
  { How long hostUntilCancelled's WaitForIdle blocks before it gives up. }
  HOST_GIVE_UP_MS = 2000;

type
  { Stands in for an embedder's host-backed async API. hostValue(x) returns a
    pending promise, HasPendingWork reports it, and WaitForIdle settles it
    with x (rejects it when x is a string), as an answer from the host would.
    hostUntilCancelled() stands for host work that runs until the script
    ends it with cancelHost(): its WaitForIdle blocks until then, giving up
    after HOST_GIVE_UP_MS, and settles the promise with "cancelled" or
    "gave up". }
  THostValueExtension = class(TGocciaRuntimeExtension)
  private
    FPending: TList;
    FValues: TList;
    FUntilCancelled: TList;
    FCancelled: Boolean;
    FWaitCount: Integer;
    FSettlesReadyWorkFirst: Boolean;
    procedure SettleValues;
    function HostValue(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function HostUntilCancelled(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function CancelHost(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
  public
    procedure Attach(const ARuntime: TGocciaRuntimeCore); override;
    procedure Detach; override;
    procedure WaitForIdle; override;
    function HasPendingWork: Boolean; override;
    { With SettlesReadyWorkFirst, the promise wait settles the hostValue
      promises on their own when there are any, without blocking on
      hostUntilCancelled: the bounded step an extension whose WaitForIdle
      blocks on one piece of work takes so another can be awaited first. }
    function RunPendingWorkForPromiseWait: Boolean; override;
    property WaitCount: Integer read FWaitCount;
    property SettlesReadyWorkFirst: Boolean read FSettlesReadyWorkFirst
      write FSettlesReadyWorkFirst;
  end;

  TDelayedAnswer = class(TThread)
  private
    FResponder: IGocciaFetchResponder;
  protected
    procedure Execute; override;
  public
    constructor Create(const AResponder: IGocciaFetchResponder);
  end;

  { Answers every request from another thread after ANSWER_DELAY_MS. }
  TLaterTransport = class(TInterfacedObject, IGocciaFetchTransport)
  private
    FAnswers: TList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Fetch(const ARequest: TGocciaFetchTransportRequest;
      const AResponder: IGocciaFetchResponder);
  end;

  TPromiseWaitTests = class(TTestSuite)
  private
    function RunModule(const ASource: string;
      out AWaitCount: Integer; const AWithFetch: Boolean = False;
      const ABoundedStep: Boolean = False): string;
    procedure TestHostPromiseInsideTest;
    procedure TestHostPromiseInResolves;
    procedure TestHostPromiseInHook;
    procedure TestRejectedHostPromiseFailsTheTest;
    procedure TestAwaitedFetchIsNotHeldByHostWork;
    procedure TestBoundedStepLetsReadyWorkBeAwaitedFirst;
  public
    procedure SetupTests; override;
  end;

{ THostValueExtension }

procedure THostValueExtension.Attach(const ARuntime: TGocciaRuntimeCore);
begin
  inherited Attach(ARuntime);
  FPending := TList.Create;
  FValues := TList.Create;
  FUntilCancelled := TList.Create;
  Runtime.Engine.GlobalScope.DefineLexicalBinding('hostValue',
    TGocciaNativeFunctionValue.Create(HostValue, 'hostValue', 1), dtConst);
  Runtime.Engine.GlobalScope.DefineLexicalBinding('hostUntilCancelled',
    TGocciaNativeFunctionValue.Create(HostUntilCancelled,
      'hostUntilCancelled', 0), dtConst);
  Runtime.Engine.GlobalScope.DefineLexicalBinding('cancelHost',
    TGocciaNativeFunctionValue.Create(CancelHost, 'cancelHost', 0), dtConst);
end;

procedure THostValueExtension.Detach;
begin
  FPending.Free;
  FValues.Free;
  FUntilCancelled.Free;
  inherited Detach;
end;

function THostValueExtension.HostValue(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Promise: TGocciaPromiseValue;
begin
  Promise := TGocciaPromiseValue.Create;
  TGarbageCollector.Instance.AddTempRoot(Promise);
  TGarbageCollector.Instance.AddTempRoot(AArgs.GetElement(0));
  FPending.Add(Promise);
  FValues.Add(AArgs.GetElement(0));
  Result := Promise;
end;

function THostValueExtension.HostUntilCancelled(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Promise: TGocciaPromiseValue;
begin
  Promise := TGocciaPromiseValue.Create;
  TGarbageCollector.Instance.AddTempRoot(Promise);
  FUntilCancelled.Add(Promise);
  FCancelled := False;
  Result := Promise;
end;

function THostValueExtension.CancelHost(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  FCancelled := True;
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

procedure THostValueExtension.SettleValues;
var
  Promise: TGocciaPromiseValue;
  Value: TGocciaValue;
begin
  while FPending.Count > 0 do
  begin
    Promise := TGocciaPromiseValue(FPending[0]);
    Value := TGocciaValue(FValues[0]);
    FPending.Delete(0);
    FValues.Delete(0);
    if Value is TGocciaStringLiteralValue then
      Promise.Reject(Value)
    else
      Promise.Resolve(Value);
    TGarbageCollector.Instance.RemoveTempRoot(Promise);
    TGarbageCollector.Instance.RemoveTempRoot(Value);
  end;
end;

procedure THostValueExtension.WaitForIdle;
var
  Promise: TGocciaPromiseValue;
  Started: Int64;
  Outcome: string;
begin
  Inc(FWaitCount);
  SettleValues;
  if FUntilCancelled.Count > 0 then
  begin
    { Nothing on this thread can cancel while this blocks: only the script
      can, after this returns. }
    Started := GetMilliseconds;
    while not FCancelled and (GetMilliseconds - Started < HOST_GIVE_UP_MS) do
      Sleep(5);
    if FCancelled then
      Outcome := 'cancelled'
    else
      Outcome := 'gave up';
    while FUntilCancelled.Count > 0 do
    begin
      Promise := TGocciaPromiseValue(FUntilCancelled[0]);
      FUntilCancelled.Delete(0);
      Promise.Resolve(TGocciaStringLiteralValue.Create(Outcome));
      TGarbageCollector.Instance.RemoveTempRoot(Promise);
    end;
  end;
end;

function THostValueExtension.HasPendingWork: Boolean;
begin
  Result := (FPending.Count > 0) or (FUntilCancelled.Count > 0);
end;

function THostValueExtension.RunPendingWorkForPromiseWait: Boolean;
begin
  if FSettlesReadyWorkFirst and (FPending.Count > 0) then
  begin
    Inc(FWaitCount);
    SettleValues;
    Exit(True);
  end;
  Result := inherited RunPendingWorkForPromiseWait;
end;

{ TDelayedAnswer }

constructor TDelayedAnswer.Create(const AResponder: IGocciaFetchResponder);
begin
  FResponder := AResponder;
  inherited Create(False);
end;

procedure TDelayedAnswer.Execute;
var
  Response: THTTPResponse;
begin
  Sleep(ANSWER_DELAY_MS);
  Response := Default(THTTPResponse);
  Response.StatusCode := 200;
  Response.StatusText := 'OK';
  Response.Body := TEncoding.UTF8.GetBytes('answer');
  FResponder.Respond(Response);
  FResponder := nil;
end;

{ TLaterTransport }

constructor TLaterTransport.Create;
begin
  inherited Create;
  FAnswers := TList.Create;
end;

destructor TLaterTransport.Destroy;
var
  I: Integer;
begin
  for I := 0 to FAnswers.Count - 1 do
  begin
    TDelayedAnswer(FAnswers[I]).WaitFor;
    TDelayedAnswer(FAnswers[I]).Free;
  end;
  FAnswers.Free;
  inherited Destroy;
end;

procedure TLaterTransport.Fetch(const ARequest: TGocciaFetchTransportRequest;
  const AResponder: IGocciaFetchResponder);
begin
  FAnswers.Add(TDelayedAnswer.Create(AResponder));
end;

{ TPromiseWaitTests }

procedure TPromiseWaitTests.SetupTests;
begin
  Test('a host promise awaited inside test() settles',
    TestHostPromiseInsideTest);
  Test('a host promise passed to expect().resolves settles',
    TestHostPromiseInResolves);
  Test('a host promise awaited in an async beforeEach settles',
    TestHostPromiseInHook);
  Test('a host promise the host rejects fails the test with its reason',
    TestRejectedHostPromiseFailsTheTest);
  Test('host work does not hold up a fetch the test awaits first',
    TestAwaitedFetchIsNotHeldByHostWork);
  Test('an extension that settles ready work first lets it be awaited ' +
    'before blocking work', TestBoundedStepLetsReadyWorkBeAwaitedFirst);
end;

{ Runs ASource as a module with goccia:test installed module-only and the
  host extension (and fetch over a transport that answers later, when
  AWithFetch), and returns what the module reports through
  `globalThis.outcome`, or the uncaught error. }
function TPromiseWaitTests.RunModule(const ASource: string;
  out AWaitCount: Integer; const AWithFetch: Boolean;
  const ABoundedStep: Boolean): string;
var
  Source: TStringList;
  Runtime: TGocciaRuntime;
  Host: THostValueExtension;
  Capabilities: TGocciaCapabilities;
  Transport: IGocciaFetchTransport;
begin
  Result := '';
  AWaitCount := 0;
  Source := TStringList.Create;
  Source.Text := ASource;
  if AWithFetch then
    Capabilities := TGocciaCapabilities.None.Allow(gcNet, API_HOST)
  else
    Capabilities := TGocciaCapabilities.None;
  Runtime := TGocciaRuntime.Create('main.js', Source, Capabilities);
  try
    Runtime.Engine.SourceType := stModule;
    Runtime.Install(TGocciaTestingLibraryRuntimeExtension.CreateModuleOnly);
    if AWithFetch then
    begin
      Runtime.Install(TGocciaFetchRuntimeExtension.Create);
      Transport := TLaterTransport.Create;
      Runtime.Engine.FetchTransport := Transport;
    end;
    Host := THostValueExtension.Create;
    Host.SettlesReadyWorkFirst := ABoundedStep;
    Runtime.Install(Host);
    try
      Runtime.Execute;
      Result := Runtime.Engine.GlobalScope.GetValue('outcome')
        .ToStringLiteral.Value;
    except
      on E: TGocciaThrowValue do
      begin
        if E.Value is TGocciaObjectValue then
          Result := 'uncaught: ' + TGocciaObjectValue(E.Value)
            .GetProperty('message').ToStringLiteral.Value
        else
          Result := 'uncaught: ' + E.Value.ToStringLiteral.Value;
      end;
    end;
    AWaitCount := Host.WaitCount;
  finally
    Runtime.Free;
    Transport := nil;
    Source.Free;
  end;
end;

const
  REPORT =
    'const r = await runTests({ showTestResults: false });' + LineEnding +
    'globalThis.outcome = r.passed + " passed, " + r.failed + " failed" +' +
    ' (r.failed ? ": " + r.failedTests.join("; ") : "");';

procedure TPromiseWaitTests.TestHostPromiseInsideTest;
var
  WaitCount: Integer;
begin
  Expect<string>(RunModule(
    'import { test, expect, runTests } from "goccia:test";' + LineEnding +
    'test("host value", async () => {' + LineEnding +
    '  expect(await hostValue(2)).toBe(2);' + LineEnding +
    '  expect(await hostValue(3)).toBe(3);' + LineEnding +
    '});' + LineEnding + REPORT, WaitCount)).ToBe('1 passed, 0 failed');
  Expect<Boolean>(WaitCount >= 2).ToBe(True);
end;

procedure TPromiseWaitTests.TestHostPromiseInResolves;
var
  WaitCount: Integer;
begin
  Expect<string>(RunModule(
    'import { test, expect, runTests } from "goccia:test";' + LineEnding +
    'test("resolves", async () => {' + LineEnding +
    '  await expect(hostValue(4)).resolves.toBe(4);' + LineEnding +
    '});' + LineEnding + REPORT, WaitCount)).ToBe('1 passed, 0 failed');
end;

procedure TPromiseWaitTests.TestHostPromiseInHook;
var
  WaitCount: Integer;
begin
  Expect<string>(RunModule(
    'import { test, expect, beforeEach, runTests } from "goccia:test";' +
    LineEnding +
    'let seen = 0;' + LineEnding +
    'beforeEach(async () => { seen = await hostValue(5); });' + LineEnding +
    'test("hook value", () => { expect(seen).toBe(5); });' + LineEnding +
    REPORT, WaitCount)).ToBe('1 passed, 0 failed');
end;

procedure TPromiseWaitTests.TestRejectedHostPromiseFailsTheTest;
var
  WaitCount: Integer;
  Outcome: string;
begin
  Outcome := RunModule(
    'import { test, expect, runTests } from "goccia:test";' + LineEnding +
    'test("host error", async () => { await hostValue("refused"); });' +
    LineEnding + REPORT, WaitCount);
  Expect<Boolean>(Pos('0 passed, 1 failed', Outcome) = 1).ToBe(True);
  Expect<Boolean>(Pos('refused', Outcome) > 0).ToBe(True);
  Expect<Boolean>(Pos('still pending', Outcome) = 0).ToBe(True);
end;

{ The test starts host work, awaits a fetch, and only then ends the host
  work. Served while the fetch was in flight, the host work would block until
  it gave up, and the promise would settle with "gave up". }
procedure TPromiseWaitTests.TestAwaitedFetchIsNotHeldByHostWork;
var
  WaitCount: Integer;
  Started: Int64;
begin
  Started := GetMilliseconds;
  Expect<string>(RunModule(
    'import { test, expect, runTests } from "goccia:test";' + LineEnding +
    'test("fetch, then cancel", async () => {' + LineEnding +
    '  const work = hostUntilCancelled();' + LineEnding +
    '  const response = await fetch("http://' + API_HOST + '/");' +
    LineEnding +
    '  cancelHost();' + LineEnding +
    '  expect(await response.text()).toBe("answer");' + LineEnding +
    '  expect(await work).toBe("cancelled");' + LineEnding +
    '});' + LineEnding + REPORT, WaitCount, True))
    .ToBe('1 passed, 0 failed');
  Expect<Boolean>(GetMilliseconds - Started < HOST_GIVE_UP_MS).ToBe(True);
end;

{ The test awaits a ready host value while other host work, which only the
  script can end, is pending. WaitForIdle settles both together and so
  blocks on the second; an extension that overrides
  RunPendingWorkForPromiseWait to settle the ready work alone lets the script
  go on and end the other. }
procedure TPromiseWaitTests.TestBoundedStepLetsReadyWorkBeAwaitedFirst;
var
  WaitCount: Integer;
  Started: Int64;
begin
  Started := GetMilliseconds;
  Expect<string>(RunModule(
    'import { test, expect, runTests } from "goccia:test";' + LineEnding +
    'test("value, then cancel", async () => {' + LineEnding +
    '  const work = hostUntilCancelled();' + LineEnding +
    '  expect(await hostValue(2)).toBe(2);' + LineEnding +
    '  cancelHost();' + LineEnding +
    '  expect(await work).toBe("cancelled");' + LineEnding +
    '});' + LineEnding + REPORT, WaitCount, False, True))
    .ToBe('1 passed, 0 failed');
  Expect<Boolean>(GetMilliseconds - Started < HOST_GIVE_UP_MS).ToBe(True);
end;

begin
  TestRunnerProgram.AddSuite(TPromiseWaitTests.Create('Engine promise wait'));
  RunGocciaTests;
  ExitCode := TestResultToExitCode;
end.
