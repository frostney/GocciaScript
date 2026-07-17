unit Goccia.RuntimeExtensions.Fetch;

{$I Goccia.inc}

interface

uses
  Classes,

  Goccia.Builtins.GlobalFetch,
  Goccia.Runtime;

type
  TGocciaFetchRuntimeExtension = class(TGocciaRuntimeExtension)
  private
    FBuiltinFetch: TGocciaGlobalFetch;
  public
    procedure Attach(const ARuntime: TGocciaRuntimeCore); override;
    procedure Detach; override;
    procedure ApplyHostRestrictions(const AAllowedHosts: TStrings); override;
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
  TGocciaFetchManager.Initialize;
  FBuiltinFetch := TGocciaGlobalFetch.Create('Fetch',
    Runtime.Engine.Interpreter.GlobalScope, Runtime.Engine.ThrowError,
    Runtime.Engine.EmitCapabilityAudit);

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
  TGocciaFetchManager.Shutdown;
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
  WaitForFetchIdle;
end;

procedure TGocciaFetchRuntimeExtension.DiscardPending;
begin
  DiscardFetchCompletions;
end;

end.
