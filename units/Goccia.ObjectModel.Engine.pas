unit Goccia.ObjectModel.Engine;

{$I Goccia.inc}

interface

uses
  Goccia.ObjectModel,
  Goccia.Scope,
  Goccia.Values.ClassValue,
  Goccia.Values.NativeFunction,
  Goccia.Values.NativeFunctionCallback,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaTypeDefinitionKind = (
    gtdkNativeInstanceType,
    gtdkPrototypeOnlyGlobal,
    gtdkPrimitiveWrapper,
    gtdkCollectionLikeNativeType
  );

  TGocciaExposePrototypeProc = procedure(const AConstructor: TGocciaValue);
  TGocciaPrototypeProvider = function: TGocciaObjectValue;
  TGocciaClassValueClass = class of TGocciaClassValue;

  TGocciaTypeDefinition = record
    ConstructorName: string;
    Kind: TGocciaTypeDefinitionKind;
    ClassValueClass: TGocciaClassValueClass;
    ExposePrototype: TGocciaExposePrototypeProc;
    PrototypeProvider: TGocciaPrototypeProvider;
    StaticSource: TGocciaObjectValue;
    PrototypeParent: TGocciaObjectValue;
    AddSpeciesGetter: Boolean;
  end;

procedure RegisterTypeDefinition(const AScope: TGocciaScope;
  const ATypeDefinition: TGocciaTypeDefinition;
  const ASpeciesGetter: TGocciaNativeFunctionCallback;
  out AConstructor: TGocciaClassValue);

implementation

uses
  Goccia.Constants.PropertyNames,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.SymbolValue;

procedure RegisterTypeDefinition(const AScope: TGocciaScope;
  const ATypeDefinition: TGocciaTypeDefinition;
  const ASpeciesGetter: TGocciaNativeFunctionCallback;
  out AConstructor: TGocciaClassValue);
var
  Key: string;
begin
  AConstructor := ATypeDefinition.ClassValueClass.Create(
    ATypeDefinition.ConstructorName, nil);

  if Assigned(ATypeDefinition.ExposePrototype) then
    ATypeDefinition.ExposePrototype(AConstructor)
  else if Assigned(ATypeDefinition.PrototypeProvider) then
  begin
    AConstructor.ReplacePrototype(ATypeDefinition.PrototypeProvider());
    AConstructor.Prototype.AssignProperty(PROP_CONSTRUCTOR, AConstructor);
  end
  else
    raise EGocciaObjectModelError.CreateFmt(
      'Type definition for %s has neither ExposePrototype nor PrototypeProvider',
      [ATypeDefinition.ConstructorName]);

  if Assigned(ATypeDefinition.PrototypeParent) then
    AConstructor.Prototype.Prototype := ATypeDefinition.PrototypeParent;

  if Assigned(ATypeDefinition.StaticSource) then
    for Key in ATypeDefinition.StaticSource.GetAllPropertyNames do
      AConstructor.SetProperty(Key,
        ATypeDefinition.StaticSource.GetProperty(Key));

  if ATypeDefinition.AddSpeciesGetter then
    AConstructor.DefineSymbolProperty(
      TGocciaSymbolValue.WellKnownSpecies,
      TGocciaPropertyDescriptorAccessor.Create(
        TGocciaNativeFunctionValue.CreateWithoutPrototype(
          ASpeciesGetter, 'get [Symbol.species]', 0),
        nil, [pfConfigurable]));

  AScope.DefineLexicalBinding(ATypeDefinition.ConstructorName, AConstructor, dtConst);
end;

end.
