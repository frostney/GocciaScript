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
  // Long enough for the worker to refuse the request and post its completion,
  // so the nested engine's own drain is what pumps it.
  WORKER_COMPLETION_GRACE_MS = 250;

type
  { Engines on one thread share the fetch manager, and a sandbox runScript
    child lives and dies inside its parent's run. These pin what the child's
    end must leave alone: the parent's network policy, the shared manager,
    and the parent's in-flight requests. }
  TFetchRuntimeExtensionTests = class(TTestSuite)
  private
    function CreateFetchEngine(const AName: string;
      const AExecutor: TGocciaInterpreterExecutor;
      const ASource: TStringList): TGocciaEngine;
    function FetchExtension(
      const AEngine: TGocciaEngine): TGocciaFetchRuntimeExtension;
    procedure TestEachEngineKeepsItsOwnRequestPolicy;
    procedure TestNestedEngineEndKeepsOuterPendingRequest;
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

procedure TFetchRuntimeExtensionTests.TestNestedEngineEndKeepsOuterPendingRequest;
var
  OuterEngine, InnerEngine: TGocciaEngine;
  OuterExecutor, InnerExecutor: TGocciaInterpreterExecutor;
  OuterSource, InnerSource: TStringList;
  OuterFetch: TGocciaFetchRuntimeExtension;
  OuterRealm: TGocciaRealm;
  Promise: TGocciaPromiseValue;
  Headers: THTTPHeaders;
  Manager: TGocciaFetchManager;
  OuterPolicy: THTTPRequestPolicy;
  InnerTypeErrorPrototype: TGocciaObjectValue;
  Rejection: TGocciaObjectValue;
  AllowedHosts: TStringList;
begin
  AllowedHosts := TStringList.Create;
  AllowedHosts.Add(LOOPBACK_HOST);
  OuterSource := TStringList.Create;
  InnerSource := TStringList.Create;
  OuterExecutor := TGocciaInterpreterExecutor.Create;
  InnerExecutor := TGocciaInterpreterExecutor.Create;
  try
    OuterEngine := CreateFetchEngine('<outer>', OuterExecutor, OuterSource);
    try
      OuterPolicy := DefaultHTTPPolicy;
      OuterPolicy.DenyPrivateRanges := True;
      SetFetchRequestPolicy(OuterEngine, OuterPolicy);
      OuterEngine.SetAllowedFetchHosts(AllowedHosts);
      OuterFetch := FetchExtension(OuterEngine);
      OuterRealm := OuterFetch.BuiltinFetch.Realm;
      Expect<Boolean>(OuterRealm = OuterEngine.Realm).ToBe(True);

      Manager := TGocciaFetchManager.Instance;
      Expect<Boolean>(Manager <> nil).ToBe(True);
      Promise := TGocciaPromiseValue.Create;
      SetLength(Headers, 0);
      Manager.StartFetch(CLOSED_LOOPBACK_URL, 'GET', Headers,
        OuterFetch.BuiltinFetch.AllowedHosts,
        OuterFetch.BuiltinFetch.RequestPolicy, OuterRealm, Promise);
      Expect<Boolean>(Manager.HasPendingFor(OuterRealm)).ToBe(True);
      Sleep(WORKER_COMPLETION_GRACE_MS);

      // The inner engine keeps the default policy, which allows private
      // ranges; the outer request must still settle under the outer policy.
      InnerEngine := CreateFetchEngine('<inner>', InnerExecutor, InnerSource);
      try
        // Compared by identity only, after the inner realm is gone.
        InnerTypeErrorPrototype := GetErrorPrototype('TypeError');
        // A nested run ends by waiting for, then discarding, its own pending
        // requests. Its drain may settle the outer request along the way.
        InnerSource.Text := '';
        InnerEngine.Execute;
      finally
        InnerEngine.Free;
      end;

      Expect<Boolean>(TGocciaFetchManager.Instance = Manager).ToBe(True);
      Expect<Boolean>(CurrentRealm = OuterRealm).ToBe(True);

      WaitForFetchIdle(OuterRealm);
      Expect<Boolean>(Manager.HasPendingFor(OuterRealm)).ToBe(False);
      Expect<Boolean>(Promise.State = gpsRejected).ToBe(True);
      Rejection := TGocciaObjectValue(Promise.PromiseResult);
      Expect<Boolean>(Pos(PRIVATE_DENIAL_TEXT,
        Rejection.GetProperty('message').ToStringLiteral.Value) > 0).ToBe(True);
      // Settled in the realm that started the request, whichever engine's
      // drain pumped it.
      Expect<Boolean>(Rejection.Prototype = GetErrorPrototype('TypeError'))
        .ToBe(True);
      Expect<Boolean>(Rejection.Prototype <> InnerTypeErrorPrototype)
        .ToBe(True);
    finally
      OuterEngine.Free;
    end;
  finally
    InnerExecutor.Free;
    OuterExecutor.Free;
    InnerSource.Free;
    OuterSource.Free;
    AllowedHosts.Free;
  end;
end;

begin
  TestRunnerProgram.AddSuite(TFetchRuntimeExtensionTests.Create(
    'Fetch runtime extension'));
  RunGocciaTests;
  ExitCode := TestResultToExitCode;
end.
