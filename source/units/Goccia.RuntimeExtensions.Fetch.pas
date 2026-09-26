unit Goccia.RuntimeExtensions.Fetch;

{$I Goccia.inc}

interface

uses
  Classes,

  HTTPTypes,

  Goccia.Builtins.GlobalAbort,
  Goccia.Builtins.GlobalEventTarget,
  Goccia.Builtins.GlobalFetch,
  Goccia.Engine,
  Goccia.Runtime;

type
  TGocciaFetchRuntimeExtension = class(TGocciaRuntimeExtension)
  private
    FBuiltinAbort: TGocciaGlobalAbort;
    FBuiltinEventTarget: TGocciaGlobalEventTarget;
    FBuiltinFetch: TGocciaGlobalFetch;
  public
    procedure Attach(const ARuntime: TGocciaRuntimeCore); override;
    procedure Detach; override;
    procedure ApplyHostRestrictions(const AAllowedHosts: TStrings); override;
    procedure WaitForIdle; override;
    procedure DiscardPending; override;

    property BuiltinFetch: TGocciaGlobalFetch read FBuiltinFetch;
  end;

{ Sets the network policy AEngine's fetch applies to every request it starts:
  resolved-address restrictions and the response-body ceiling. The policy
  belongs to that engine alone, so engines on one thread — a sandbox parent
  and its runScript child — each keep their own. Call it after the fetch
  runtime extension is installed. Returns False, changing nothing, when the
  engine has no fetch runtime extension: a later install starts from
  DefaultHTTPPolicy, so a host that needs the policy must check the result. }
function SetFetchRequestPolicy(const AEngine: TGocciaEngine;
  const APolicy: THTTPRequestPolicy): Boolean;

implementation

uses
  Goccia.Constants.ConstructorNames,
  Goccia.FetchManager,
  Goccia.ObjectModel.Engine,
  Goccia.Values.ClassValue,
  Goccia.Values.HeadersValue,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives,
  Goccia.Values.ResponseValue;

procedure ExposeHeadersPrototype(const AConstructor: TGocciaValue);
begin
  TGocciaHeadersValue.ExposePrototype(AConstructor);
end;

procedure ExposeResponsePrototype(const AConstructor: TGocciaValue);
begin
  TGocciaResponseValue.ExposePrototype(AConstructor);
end;

procedure TGocciaFetchRuntimeExtension.Attach(
  const ARuntime: TGocciaRuntimeCore);
var
  RuntimeConstructor: TGocciaClassValue;
  ObjectPrototype: TGocciaObjectValue;
  TypeDef: TGocciaTypeDefinition;
begin
  inherited Attach(ARuntime);
  // EventTarget must exist before AbortSignal so the signal's prototype and
  // constructor can be linked into the EventTarget chain (WHATWG DOM §3.2).
  FBuiltinEventTarget := TGocciaGlobalEventTarget.Create('EventTarget',
    Runtime.Engine.Interpreter.GlobalScope, Runtime.Engine.ThrowError);
  Runtime.RegisterRuntimeGlobalName(CONSTRUCTOR_EVENT_TARGET);
  Runtime.RegisterRuntimeGlobalName(CONSTRUCTOR_EVENT);
  FBuiltinAbort := TGocciaGlobalAbort.Create('Abort',
    Runtime.Engine.Interpreter.GlobalScope, Runtime.Engine.ThrowError,
    FBuiltinEventTarget.EventTargetConstructor);
  Runtime.RegisterRuntimeGlobalName(CONSTRUCTOR_ABORT_CONTROLLER);
  Runtime.RegisterRuntimeGlobalName(CONSTRUCTOR_ABORT_SIGNAL);
  FBuiltinFetch := TGocciaGlobalFetch.Create('Fetch',
    Runtime.Engine.Interpreter.GlobalScope, Runtime.Engine.ThrowError,
    Runtime.Engine.EmitCapabilityAudit, Runtime.Engine.Realm);

  if not Assigned(Runtime.Engine.ObjectConstructor) then
    Exit;

  ObjectPrototype := Runtime.Engine.ObjectConstructor.Prototype;

  TypeDef.ConstructorName := CONSTRUCTOR_HEADERS;
  TypeDef.Kind := gtdkNativeInstanceType;
  TypeDef.ClassValueClass := TGocciaHeadersClassValue;
  TypeDef.ExposePrototype := @ExposeHeadersPrototype;
  TypeDef.PrototypeProvider := nil;
  TypeDef.StaticSource := nil;
  TypeDef.PrototypeParent := ObjectPrototype;
  TypeDef.AddSpeciesGetter := False;
  RegisterTypeDefinition(Runtime.Engine.Interpreter.GlobalScope, TypeDef,
    Runtime.SpeciesGetter, RuntimeConstructor);

  TypeDef.ConstructorName := CONSTRUCTOR_RESPONSE;
  TypeDef.Kind := gtdkNativeInstanceType;
  TypeDef.ClassValueClass := TGocciaResponseClassValue;
  TypeDef.ExposePrototype := @ExposeResponsePrototype;
  TypeDef.PrototypeProvider := nil;
  TypeDef.StaticSource := nil;
  TypeDef.PrototypeParent := ObjectPrototype;
  TypeDef.AddSpeciesGetter := False;
  RegisterTypeDefinition(Runtime.Engine.Interpreter.GlobalScope, TypeDef,
    Runtime.SpeciesGetter, RuntimeConstructor);
end;

procedure TGocciaFetchRuntimeExtension.Detach;
begin
  FBuiltinFetch.Free;
  FBuiltinFetch := nil;
  FBuiltinAbort.Free;
  FBuiltinAbort := nil;
  FBuiltinEventTarget.Free;
  FBuiltinEventTarget := nil;
  inherited;
end;

procedure TGocciaFetchRuntimeExtension.ApplyHostRestrictions(
  const AAllowedHosts: TStrings);
var
  EmptyHosts: TStringList;
begin
  if not Assigned(FBuiltinFetch) then
    Exit;

  if Assigned(AAllowedHosts) then
    FBuiltinFetch.SetAllowedHosts(AAllowedHosts)
  else
  begin
    EmptyHosts := TStringList.Create;
    try
      FBuiltinFetch.SetAllowedHosts(EmptyHosts);
    finally
      EmptyHosts.Free;
    end;
  end;
end;

procedure TGocciaFetchRuntimeExtension.WaitForIdle;
begin
  if Assigned(FBuiltinFetch) then
    WaitForFetchIdle(FBuiltinFetch.Realm);
end;

procedure TGocciaFetchRuntimeExtension.DiscardPending;
begin
  if Assigned(FBuiltinFetch) then
    DiscardFetchCompletions(FBuiltinFetch.Realm);
end;

function SetFetchRequestPolicy(const AEngine: TGocciaEngine;
  const APolicy: THTTPRequestPolicy): Boolean;
var
  Runtime: TGocciaRuntimeCore;
  Extension: TGocciaFetchRuntimeExtension;
begin
  Result := False;
  Runtime := GetRuntime(AEngine);
  if not Assigned(Runtime) then
    Exit;
  Extension := TGocciaFetchRuntimeExtension(
    Runtime.FindRuntimeExtension(TGocciaFetchRuntimeExtension));
  if not Assigned(Extension) or not Assigned(Extension.BuiltinFetch) then
    Exit;
  Extension.BuiltinFetch.RequestPolicy := APolicy;
  Result := True;
end;

end.
