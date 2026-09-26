unit Goccia.RuntimeExtensions.Fetch;

{$I Goccia.inc}

interface

uses
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
    function EngineMaxResponseBytes: Integer;
  public
    procedure Attach(const ARuntime: TGocciaRuntimeCore); override;
    procedure Detach; override;
    procedure WaitForIdle; override;
    procedure DiscardPending; override;

    property BuiltinFetch: TGocciaGlobalFetch read FBuiltinFetch;
  end;

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
    Runtime.Engine.Capabilities, Runtime.Engine.EmitCapabilityAudit,
    EngineMaxResponseBytes, Runtime.Engine.Realm);

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

function TGocciaFetchRuntimeExtension.EngineMaxResponseBytes: Integer;
begin
  if Assigned(Runtime) then
    Result := Runtime.Engine.FetchMaxResponseBytes
  else
    Result := 0;
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

end.
