program Goccia.RuntimeExtensions.Fetch.Test;

{$I Goccia.inc}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes,
  SysUtils,

  HTTPTypes,
  TestingPascalLibrary,

  Goccia.Engine,
  Goccia.Executor.Interpreter,
  Goccia.FetchManager,
  Goccia.GarbageCollector,
  Goccia.Realm,
  Goccia.Runtime,
  Goccia.RuntimeExtensions.Fetch,
  Goccia.TestSetup,
  Goccia.Values.ErrorHelper,
  Goccia.Values.ObjectValue,
  Goccia.Values.PromiseValue;

const
  OUTER_MAX_RESPONSE_BYTES = 1234;
  // Nothing listens on port 1, and with private ranges denied the request is
  // refused after resolution, before any connect: no network is needed.
  LOOPBACK_HOST = '127.0.0.1';
  CLOSED_LOOPBACK_URL = 'http://' + LOOPBACK_HOST + ':1/';
  PRIVATE_DENIAL_TEXT = 'resolves to private address';
  SETTLE_DEADLINE_MS = 10000;
  POLL_INTERVAL_MS = 1;

type
  { Engines on one thread share the fetch manager, and a sandbox runScript
    child lives and dies inside its parent's run. These pin what the child's
    end must leave alone: the parent's network policy, the shared manager,
    and the parent's in-flight requests. }
  TFetchRuntimeExtensionTests = class(TTestSuite)
  private
    FOuterEngine: TGocciaEngine;
    FOuterExecutor, FInnerExecutor: TGocciaInterpreterExecutor;
    FOuterSource, FInnerSource: TStringList;
    FOuterRealm: TGocciaRealm;
    FPromise: TGocciaPromiseValue;
    { An outer engine that denies private ranges and allows the loopback
      host, and one outer request to a closed loopback port under it. }
    procedure SetUpOuterEngine;
    procedure TearDownOuterEngine;
    procedure StartOuterRequest;
    function RejectionMessage: string;
    function CreateFetchEngine(const AName: string;
      const AExecutor: TGocciaInterpreterExecutor;
      const ASource: TStringList): TGocciaEngine;
    function FetchExtension(
      const AEngine: TGocciaEngine): TGocciaFetchRuntimeExtension;
    procedure TestEachEngineKeepsItsOwnRequestPolicy;
    procedure TestNestedEngineEndKeepsOuterPendingRequest;
    procedure TestOuterRequestSettlesInItsOwnRealm;
  public
    procedure SetupTests; override;
  end;

procedure TFetchRuntimeExtensionTests.SetupTests;
begin
  Test('each engine keeps its own fetch policy and freeing one leaves the ' +
    'other and the shared manager intact',
    TestEachEngineKeepsItsOwnRequestPolicy);
  Test('a nested engine ending keeps the outer engine''s in-flight request ' +
    'and settles it under the outer policy',
    TestNestedEngineEndKeepsOuterPendingRequest);
  Test('an outer request pumped while a nested engine is current settles in ' +
    'the outer realm',
    TestOuterRequestSettlesInItsOwnRealm);
end;

function TFetchRuntimeExtensionTests.CreateFetchEngine(const AName: string;
  const AExecutor: TGocciaInterpreterExecutor;
  const ASource: TStringList): TGocciaEngine;
begin
  Result := TGocciaEngine.Create(AName, ASource, AExecutor);
  try
    AttachRuntime(Result).Install(TGocciaFetchRuntimeExtension.Create);
  except
    Result.Free;
    raise;
  end;
end;

function TFetchRuntimeExtensionTests.FetchExtension(
  const AEngine: TGocciaEngine): TGocciaFetchRuntimeExtension;
begin
  Result := TGocciaFetchRuntimeExtension(GetRuntime(AEngine)
    .FindRuntimeExtension(TGocciaFetchRuntimeExtension));
end;

procedure TFetchRuntimeExtensionTests.TestEachEngineKeepsItsOwnRequestPolicy;
var
  OuterEngine, InnerEngine: TGocciaEngine;
  OuterExecutor, InnerExecutor: TGocciaInterpreterExecutor;
  OuterSource, InnerSource: TStringList;
  OuterPolicy, InnerPolicy, Observed: THTTPRequestPolicy;
begin
  OuterSource := TStringList.Create;
  InnerSource := TStringList.Create;
  OuterExecutor := TGocciaInterpreterExecutor.Create;
  InnerExecutor := TGocciaInterpreterExecutor.Create;
  try
    OuterEngine := CreateFetchEngine('<outer>', OuterExecutor, OuterSource);
    try
      OuterPolicy := DefaultHTTPPolicy;
      OuterPolicy.DenyPrivateRanges := True;
      OuterPolicy.MaxResponseBytes := OUTER_MAX_RESPONSE_BYTES;
      SetFetchRequestPolicy(OuterEngine, OuterPolicy);

      InnerEngine := CreateFetchEngine('<inner>', InnerExecutor, InnerSource);
      try
        InnerPolicy := DefaultHTTPPolicy;
        SetFetchRequestPolicy(InnerEngine, InnerPolicy);

        Observed := FetchExtension(InnerEngine).BuiltinFetch.RequestPolicy;
        Expect<Boolean>(Observed.DenyPrivateRanges).ToBe(False);
        Observed := FetchExtension(OuterEngine).BuiltinFetch.RequestPolicy;
        Expect<Boolean>(Observed.DenyPrivateRanges).ToBe(True);

        InnerSource.Text := '';
        InnerEngine.Execute;
      finally
        InnerEngine.Free;
      end;

      Expect<Boolean>(TGocciaFetchManager.Instance <> nil).ToBe(True);
      Observed := FetchExtension(OuterEngine).BuiltinFetch.RequestPolicy;
      Expect<Boolean>(Observed.DenyPrivateRanges).ToBe(True);
      Expect<Integer>(Observed.MaxResponseBytes).ToBe(OUTER_MAX_RESPONSE_BYTES);
    finally
      OuterEngine.Free;
    end;

    // The last user gone, the manager goes with it.
    Expect<Boolean>(TGocciaFetchManager.Instance = nil).ToBe(True);
  finally
    InnerExecutor.Free;
    OuterExecutor.Free;
    InnerSource.Free;
    OuterSource.Free;
  end;
end;

procedure TFetchRuntimeExtensionTests.SetUpOuterEngine;
var
  OuterPolicy: THTTPRequestPolicy;
  AllowedHosts: TStringList;
begin
  FOuterSource := TStringList.Create;
  FInnerSource := TStringList.Create;
  FOuterExecutor := TGocciaInterpreterExecutor.Create;
  FInnerExecutor := TGocciaInterpreterExecutor.Create;
  FOuterEngine := CreateFetchEngine('<outer>', FOuterExecutor, FOuterSource);
  OuterPolicy := DefaultHTTPPolicy;
  OuterPolicy.DenyPrivateRanges := True;
  Expect<Boolean>(SetFetchRequestPolicy(FOuterEngine, OuterPolicy)).ToBe(True);
  AllowedHosts := TStringList.Create;
  try
    AllowedHosts.Add(LOOPBACK_HOST);
    FOuterEngine.SetAllowedFetchHosts(AllowedHosts);
  finally
    AllowedHosts.Free;
  end;
  FOuterRealm := FetchExtension(FOuterEngine).BuiltinFetch.Realm;
  Expect<Boolean>(FOuterRealm = FOuterEngine.Realm).ToBe(True);
  FPromise := nil;
end;

procedure TFetchRuntimeExtensionTests.TearDownOuterEngine;
begin
  if Assigned(FPromise) and Assigned(TGarbageCollector.Instance) then
    TGarbageCollector.Instance.RemoveTempRoot(FPromise);
  FPromise := nil;
  FreeAndNil(FOuterEngine);
  FreeAndNil(FInnerExecutor);
  FreeAndNil(FOuterExecutor);
  FreeAndNil(FInnerSource);
  FreeAndNil(FOuterSource);
end;

procedure TFetchRuntimeExtensionTests.StartOuterRequest;
var
  OuterFetch: TGocciaFetchRuntimeExtension;
  Headers: THTTPHeaders;
begin
  OuterFetch := FetchExtension(FOuterEngine);
  FPromise := TGocciaPromiseValue.Create;
  // Rooted for the whole test: the manager unroots it once it settles, and
  // the test reads it after building and freeing another engine.
  TGarbageCollector.Instance.AddTempRoot(FPromise);
  SetLength(Headers, 0);
  TGocciaFetchManager.Instance.StartFetch(CLOSED_LOOPBACK_URL, 'GET', Headers,
    OuterFetch.BuiltinFetch.AllowedHosts,
    OuterFetch.BuiltinFetch.RequestPolicy, FOuterRealm, FPromise);
  Expect<Boolean>(TGocciaFetchManager.Instance.HasPendingFor(FOuterRealm))
    .ToBe(True);
end;

function TFetchRuntimeExtensionTests.RejectionMessage: string;
begin
  Result := TGocciaObjectValue(FPromise.PromiseResult)
    .GetProperty('message').ToStringLiteral.Value;
end;

procedure TFetchRuntimeExtensionTests.TestNestedEngineEndKeepsOuterPendingRequest;
var
  InnerEngine: TGocciaEngine;
  Manager: TGocciaFetchManager;
begin
  SetUpOuterEngine;
  try
    StartOuterRequest;
    Manager := TGocciaFetchManager.Instance;

    // Nothing pumps between the dispatch and the check below, so the outer
    // request is still pending however quickly its worker finishes.
    InnerEngine := CreateFetchEngine('<inner>', FInnerExecutor, FInnerSource);
    try
      // What a nested run does when it ends: discard its own pending work,
      // then release the fetch manager as its runtime detaches.
      GetRuntime(InnerEngine).DiscardPending;
    finally
      InnerEngine.Free;
    end;

    Expect<Boolean>(TGocciaFetchManager.Instance = Manager).ToBe(True);
    Expect<Boolean>(CurrentRealm = FOuterRealm).ToBe(True);
    Expect<Boolean>(Manager.HasPendingFor(FOuterRealm)).ToBe(True);

    // The inner engine kept the default policy, which allows private ranges;
    // the outer request still settles under the outer policy.
    WaitForFetchIdle(FOuterRealm);
    Expect<Boolean>(Manager.HasPendingFor(FOuterRealm)).ToBe(False);
    Expect<Boolean>(FPromise.State = gpsRejected).ToBe(True);
    Expect<Boolean>(Pos(PRIVATE_DENIAL_TEXT, RejectionMessage) > 0).ToBe(True);
  finally
    TearDownOuterEngine;
  end;
end;

procedure TFetchRuntimeExtensionTests.TestOuterRequestSettlesInItsOwnRealm;
var
  InnerEngine: TGocciaEngine;
  Manager: TGocciaFetchManager;
  InnerTypeErrorPrototype, Rejection: TGocciaObjectValue;
  WaitedMilliseconds: Integer;
begin
  SetUpOuterEngine;
  try
    StartOuterRequest;
    Manager := TGocciaFetchManager.Instance;

    InnerEngine := CreateFetchEngine('<inner>', FInnerExecutor, FInnerSource);
    try
      // Compared by identity only, after the inner realm is gone.
      InnerTypeErrorPrototype := GetErrorPrototype('TypeError');
      // Pump while the inner engine's realm is current, as its own drain
      // does, until the outer request settles.
      WaitedMilliseconds := 0;
      while (FPromise.State = gpsPending) and
            (WaitedMilliseconds < SETTLE_DEADLINE_MS) do
      begin
        if Manager.PumpCompletions = 0 then
        begin
          Sleep(POLL_INTERVAL_MS);
          Inc(WaitedMilliseconds, POLL_INTERVAL_MS);
        end;
      end;
      Expect<Boolean>(FPromise.State = gpsRejected).ToBe(True);
      Expect<Boolean>(CurrentRealm = InnerEngine.Realm).ToBe(True);
    finally
      InnerEngine.Free;
    end;

    Rejection := TGocciaObjectValue(FPromise.PromiseResult);
    Expect<Boolean>(Pos(PRIVATE_DENIAL_TEXT, RejectionMessage) > 0).ToBe(True);
    Expect<Boolean>(Rejection.Prototype = GetErrorPrototype('TypeError'))
      .ToBe(True);
    Expect<Boolean>(Rejection.Prototype <> InnerTypeErrorPrototype).ToBe(True);
  finally
    TearDownOuterEngine;
  end;
end;

begin
  TestRunnerProgram.AddSuite(TFetchRuntimeExtensionTests.Create(
    'Fetch runtime extension'));
  RunGocciaTests;
  ExitCode := TestResultToExitCode;
end.
