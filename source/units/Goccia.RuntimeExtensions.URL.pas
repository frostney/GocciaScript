unit Goccia.RuntimeExtensions.URL;

{$I Goccia.inc}

interface

uses
  Goccia.Builtins.GlobalURL,
  Goccia.Runtime;

type
  TGocciaURLRuntimeExtension = class(TGocciaRuntimeExtension)
  private
    FBuiltinURL: TGocciaGlobalURL;
    FBuiltinURLSearchParams: TGocciaGlobalURLSearchParams;
  public
    procedure Attach(const ARuntime: TGocciaRuntimeCore); override;
    procedure Detach; override;
  end;

implementation

uses
  Goccia.Builtins.Base,
  Goccia.Constants.ConstructorNames,
  Goccia.ObjectModel.Engine,
  Goccia.Values.ClassValue,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives,
  Goccia.Values.URLSearchParamsValue,
  Goccia.Values.URLValue;

function BuiltinObjectOrNil(const ABuiltin: TGocciaBuiltin): TGocciaObjectValue;
begin
  if Assigned(ABuiltin) then
    Result := ABuiltin.BuiltinObject
  else
    Result := nil;
end;

procedure ExposeURLPrototype(const AConstructor: TGocciaValue);
begin
  TGocciaURLValue.ExposePrototype(AConstructor);
end;

procedure ExposeURLSearchParamsPrototype(const AConstructor: TGocciaValue);
begin
  TGocciaURLSearchParamsValue.ExposePrototype(AConstructor);
end;

procedure TGocciaURLRuntimeExtension.Attach(const ARuntime: TGocciaRuntimeCore);
var
  RuntimeConstructor: TGocciaClassValue;
  ObjectPrototype: TGocciaObjectValue;
  TypeDef: TGocciaTypeDefinition;
begin
  inherited Attach(ARuntime);
  FBuiltinURL := TGocciaGlobalURL.Create(CONSTRUCTOR_URL,
    Runtime.Engine.Interpreter.GlobalScope, Runtime.Engine.ThrowError);
  FBuiltinURLSearchParams := TGocciaGlobalURLSearchParams.Create(
    CONSTRUCTOR_URL_SEARCH_PARAMS, Runtime.Engine.Interpreter.GlobalScope,
    Runtime.Engine.ThrowError);

  if not Assigned(Runtime.Engine.ObjectConstructor) then
    Exit;

  ObjectPrototype := Runtime.Engine.ObjectConstructor.Prototype;

  TypeDef.ConstructorName := CONSTRUCTOR_URL;
  TypeDef.Kind := gtdkCollectionLikeNativeType;
  TypeDef.ClassValueClass := TGocciaURLClassValue;
  TypeDef.ExposePrototype := @ExposeURLPrototype;
  TypeDef.PrototypeProvider := nil;
  TypeDef.StaticSource := BuiltinObjectOrNil(FBuiltinURL);
  TypeDef.PrototypeParent := ObjectPrototype;
  TypeDef.AddSpeciesGetter := False;
  RegisterTypeDefinition(Runtime.Engine.Interpreter.GlobalScope, TypeDef,
    Runtime.SpeciesGetter, RuntimeConstructor);

  TypeDef.ConstructorName := CONSTRUCTOR_URL_SEARCH_PARAMS;
  TypeDef.Kind := gtdkCollectionLikeNativeType;
  TypeDef.ClassValueClass := TGocciaURLSearchParamsClassValue;
  TypeDef.ExposePrototype := @ExposeURLSearchParamsPrototype;
  TypeDef.PrototypeProvider := nil;
  TypeDef.StaticSource := BuiltinObjectOrNil(FBuiltinURLSearchParams);
  TypeDef.PrototypeParent := ObjectPrototype;
  TypeDef.AddSpeciesGetter := False;
  RegisterTypeDefinition(Runtime.Engine.Interpreter.GlobalScope, TypeDef,
    Runtime.SpeciesGetter, RuntimeConstructor);
end;

procedure TGocciaURLRuntimeExtension.Detach;
begin
  FBuiltinURL.Free;
  FBuiltinURL := nil;
  FBuiltinURLSearchParams.Free;
  FBuiltinURLSearchParams := nil;
  inherited;
end;

end.
