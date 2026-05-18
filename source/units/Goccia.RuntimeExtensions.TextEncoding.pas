unit Goccia.RuntimeExtensions.TextEncoding;

{$I Goccia.inc}

interface

uses
  Goccia.Runtime;

type
  TGocciaTextEncodingRuntimeExtension = class(TGocciaRuntimeExtension)
  public
    procedure Attach(const ARuntime: TGocciaRuntimeCore); override;
  end;

implementation

uses
  Goccia.Constants.ConstructorNames,
  Goccia.ObjectModel.Engine,
  Goccia.Values.ClassValue,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives,
  Goccia.Values.TextDecoderValue,
  Goccia.Values.TextEncoderValue;

procedure ExposeTextEncoderPrototype(const AConstructor: TGocciaValue);
begin
  TGocciaTextEncoderValue.ExposePrototype(AConstructor);
end;

procedure ExposeTextDecoderPrototype(const AConstructor: TGocciaValue);
begin
  TGocciaTextDecoderValue.ExposePrototype(AConstructor);
end;

procedure TGocciaTextEncodingRuntimeExtension.Attach(
  const ARuntime: TGocciaRuntimeCore);
var
  RuntimeConstructor: TGocciaClassValue;
  ObjectPrototype: TGocciaObjectValue;
  TypeDef: TGocciaTypeDefinition;
begin
  inherited Attach(ARuntime);
  if not Assigned(Runtime.Engine.ObjectConstructor) then
    Exit;

  ObjectPrototype := Runtime.Engine.ObjectConstructor.Prototype;

  TypeDef.ConstructorName := CONSTRUCTOR_TEXT_ENCODER;
  TypeDef.Kind := gtdkNativeInstanceType;
  TypeDef.ClassValueClass := TGocciaTextEncoderClassValue;
  TypeDef.ExposePrototype := @ExposeTextEncoderPrototype;
  TypeDef.PrototypeProvider := nil;
  TypeDef.StaticSource := nil;
  TypeDef.PrototypeParent := ObjectPrototype;
  TypeDef.AddSpeciesGetter := False;
  RegisterTypeDefinition(Runtime.Engine.Interpreter.GlobalScope, TypeDef,
    Runtime.SpeciesGetter, RuntimeConstructor);

  TypeDef.ConstructorName := CONSTRUCTOR_TEXT_DECODER;
  TypeDef.Kind := gtdkNativeInstanceType;
  TypeDef.ClassValueClass := TGocciaTextDecoderClassValue;
  TypeDef.ExposePrototype := @ExposeTextDecoderPrototype;
  TypeDef.PrototypeProvider := nil;
  TypeDef.StaticSource := nil;
  TypeDef.PrototypeParent := ObjectPrototype;
  TypeDef.AddSpeciesGetter := False;
  RegisterTypeDefinition(Runtime.Engine.Interpreter.GlobalScope, TypeDef,
    Runtime.SpeciesGetter, RuntimeConstructor);
end;

end.
