program Goccia.RuntimeExtensions.Fetch.Test;

{$I Goccia.inc}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes,
  SysUtils,

  HTTPTypes,
  TestingPascalLibrary,

  Goccia.Capabilities,
  Goccia.Constants.ErrorNames,
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
  PRIVATE_DENIAL_TEXT = 'net: ' + LOOPBACK_HOST + ':1';
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
      const ASource: TStringList;
      const ACapabilities: TGocciaCapabilities): TGocciaEngine;
    function OuterCapabilities: TGocciaCapabilities;
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

{ The outer engine allows the loopback host by name but denies private
  ranges, so its request to the loopback address is refused after
  resolution; the inner engine names private ranges and would allow it. }
function TFetchRuntimeExtensionTests.OuterCapabilities: TGocciaCapabilities;
begin
  Result := TGocciaCapabilities.None.Allow(gcNet, LOOPBACK_HOST)
    .Deny(gcNet, NET_PRIVATE_SCOPE);
end;

function InnerCapabilities: TGocciaCapabilities;
begin
  Result := TGocciaCapabilities.None.Allow(gcNet, LOOPBACK_HOST)
    .Allow(gcNet, NET_PRIVATE_SCOPE);
end;

function TFetchRuntimeExtensionTests.CreateFetchEngine(const AName: string;
  const AExecutor: TGocciaInterpreterExecutor;
  const ASource: TStringList;
  const ACapabilities: TGocciaCapabilities): TGocciaEngine;
begin
  Result := TGocciaEngine.Create(AName, ASource, AExecutor, ACapabilities);
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
  Observed: TGocciaCapabilities;
begin
  OuterSource := TStringList.Create;
  InnerSource := TStringList.Create;
  OuterExecutor := TGocciaInterpreterExecutor.Create;
  InnerExecutor := TGocciaInterpreterExecutor.Create;
  try
    OuterEngine := CreateFetchEngine('<outer>', OuterExecutor, OuterSource,
      OuterCapabilities);
    try
      OuterEngine.FetchMaxResponseBytes := OUTER_MAX_RESPONSE_BYTES;

      InnerEngine := CreateFetchEngine('<inner>', InnerExecutor, InnerSource,
        InnerCapabilities);
      try
        Observed := FetchExtension(InnerEngine).BuiltinFetch.Capabilities;
        Expect<Boolean>(Observed.AllowsNetAddress(LOOPBACK_HOST, 80,
          LOOPBACK_HOST)).ToBe(True);
        Observed := FetchExtension(OuterEngine).BuiltinFetch.Capabilities;
        Expect<Boolean>(Observed.AllowsNetAddress(LOOPBACK_HOST, 80,
          LOOPBACK_HOST)).ToBe(False);

        InnerSource.Text := '';
        InnerEngine.Execute;
      finally
        InnerEngine.Free;
      end;

      Expect<Boolean>(TGocciaFetchManager.Instance <> nil).ToBe(True);
      Observed := FetchExtension(OuterEngine).BuiltinFetch.Capabilities;
      Expect<Boolean>(Observed.AllowsNetAddress(LOOPBACK_HOST, 80,
          LOOPBACK_HOST)).ToBe(False);
      Expect<Integer>(OuterEngine.FetchMaxResponseBytes)
        .ToBe(OUTER_MAX_RESPONSE_BYTES);
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
begin
  FOuterSource := TStringList.Create;
  FInnerSource := TStringList.Create;
  FOuterExecutor := TGocciaInterpreterExecutor.Create;
  FInnerExecutor := TGocciaInterpreterExecutor.Create;
  FOuterEngine := CreateFetchEngine('<outer>', FOuterExecutor, FOuterSource,
    OuterCapabilities);
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
  Policy: TGocciaFetchPolicy;
begin
  OuterFetch := FetchExtension(FOuterEngine);
  FPromise := TGocciaPromiseValue.Create;
  // Rooted for the whole test: the manager unroots it once it settles, and
  // the test reads it after building and freeing another engine.
  TGarbageCollector.Instance.AddTempRoot(FPromise);
  SetLength(Headers, 0);
  Policy := Default(TGocciaFetchPolicy);
  Policy.Capabilities := OuterFetch.BuiltinFetch.Capabilities;
  Policy.MaxResponseBytes := FOuterEngine.FetchMaxResponseBytes;
  TGocciaFetchManager.Instance.StartFetch(CLOSED_LOOPBACK_URL, 'GET', Headers,
    Policy, FOuterRealm, FPromise);
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
    InnerEngine := CreateFetchEngine('<inner>', FInnerExecutor, FInnerSource,
      InnerCapabilities);
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

    // The inner engine names private ranges; the outer request still settles
    // under the outer engine's capability set.
    WaitForFetchIdle(FOuterRealm);
    Expect<Boolean>(Manager.HasPendingFor(FOuterRealm)).ToBe(False);
    Expect<Boolean>(FPromise.State = gpsRejected).ToBe(True);
    Expect<Boolean>(RejectionMessage = PRIVATE_DENIAL_TEXT).ToBe(True);
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

    InnerEngine := CreateFetchEngine('<inner>', FInnerExecutor, FInnerSource,
      InnerCapabilities);
    try
      // Compared by identity only, after the inner realm is gone.
      InnerTypeErrorPrototype := GetErrorPrototype(PERMISSION_DENIED_NAME);
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
    Expect<Boolean>(RejectionMessage = PRIVATE_DENIAL_TEXT).ToBe(True);
    Expect<Boolean>(Rejection.Prototype =
      GetErrorPrototype(PERMISSION_DENIED_NAME)).ToBe(True);
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
