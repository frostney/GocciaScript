unit Goccia.RuntimeExtensions.Blob;

{$I Goccia.inc}

{ The File API's Blob global (https://w3c.github.io/FileAPI/#blob-section),
  part of the loader runtime profile. }

interface

uses
  Goccia.Runtime;

type
  TGocciaBlobRuntimeExtension = class(TGocciaRuntimeExtension)
  public
    procedure Attach(const ARuntime: TGocciaRuntimeCore); override;
  end;

implementation

uses
  Goccia.Constants.ConstructorNames,
  Goccia.ObjectModel.Engine,
  Goccia.Values.BlobValue,
  Goccia.Values.ClassValue,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

procedure ExposeBlobPrototype(const AConstructor: TGocciaValue);
begin
  TGocciaBlobValue.ExposePrototype(AConstructor);
end;

procedure TGocciaBlobRuntimeExtension.Attach(const ARuntime: TGocciaRuntimeCore);
var
  RuntimeConstructor: TGocciaClassValue;
  TypeDef: TGocciaTypeDefinition;
begin
  inherited Attach(ARuntime);
  if not Assigned(Runtime.Engine.ObjectConstructor) then
    Exit;

  TypeDef.ConstructorName := CONSTRUCTOR_BLOB;
  TypeDef.Kind := gtdkNativeInstanceType;
  TypeDef.ClassValueClass := TGocciaBlobClassValue;
  TypeDef.ExposePrototype := @ExposeBlobPrototype;
  TypeDef.PrototypeProvider := nil;
  TypeDef.StaticSource := nil;
  TypeDef.PrototypeParent := Runtime.Engine.ObjectConstructor.Prototype;
  TypeDef.AddSpeciesGetter := False;
  RegisterTypeDefinition(Runtime.Engine.GlobalScope, TypeDef,
    Runtime.SpeciesGetter, RuntimeConstructor);
  Runtime.RegisterRuntimeGlobalName(CONSTRUCTOR_BLOB);
end;

end.
