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
  Goccia.SharedPrototype,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.SymbolValue;

procedure CopyStaticMembersToConstructor(const ASource: TGocciaObjectValue;
  const AConstructor: TGocciaClassValue);
var
  Key: string;
  Symbol: TGocciaSymbolValue;
  Descriptor: TGocciaPropertyDescriptor;
begin
  if not Assigned(ASource) then
    Exit;

  for Key in ASource.GetAllPropertyNames do
  begin
    Descriptor := ASource.GetOwnPropertyDescriptor(Key);
    if not Assigned(Descriptor) then
      Continue;

    if Descriptor is TGocciaPropertyDescriptorData then
      AConstructor.SetProperty(Key,
        TGocciaPropertyDescriptorData(Descriptor).Value)
    else
      raise EGocciaObjectModelError.CreateFmt(
        'Static source for %s contains unsupported accessor property "%s"',
        [AConstructor.Name, Key]);
  end;

  for Symbol in ASource.GetOwnSymbols do
  begin
    Descriptor := ASource.GetOwnSymbolPropertyDescriptor(Symbol);
    if not Assigned(Descriptor) then
      Continue;

    if Descriptor is TGocciaPropertyDescriptorData then
      AConstructor.DefineSymbolProperty(Symbol,
        TGocciaPropertyDescriptorData.Create(
          TGocciaPropertyDescriptorData(Descriptor).Value,
          Descriptor.Flags))
    else if Descriptor is TGocciaPropertyDescriptorAccessor then
      AConstructor.DefineSymbolProperty(Symbol,
        TGocciaPropertyDescriptorAccessor.Create(
          TGocciaPropertyDescriptorAccessor(Descriptor).Getter,
          TGocciaPropertyDescriptorAccessor(Descriptor).Setter,
          Descriptor.Flags))
    else
      raise EGocciaObjectModelError.CreateFmt(
        'Static source for %s has unsupported symbol descriptor',
        [AConstructor.Name]);
  end;
end;

procedure RegisterTypeDefinition(const AScope: TGocciaScope;
  const ATypeDefinition: TGocciaTypeDefinition;
  const ASpeciesGetter: TGocciaNativeFunctionCallback;
  out AConstructor: TGocciaClassValue);
begin
  AConstructor := ATypeDefinition.ClassValueClass.Create(
    ATypeDefinition.ConstructorName, nil);

  if Assigned(ATypeDefinition.ExposePrototype) then
    ATypeDefinition.ExposePrototype(AConstructor)
  else if Assigned(ATypeDefinition.PrototypeProvider) then
  begin
    AConstructor.ReplacePrototype(ATypeDefinition.PrototypeProvider());
    if not AConstructor.Prototype.HasProperty(PROP_CONSTRUCTOR) then
      AConstructor.Prototype.AssignProperty(PROP_CONSTRUCTOR, AConstructor);
  end
  else
    raise EGocciaObjectModelError.CreateFmt(
      'Type definition for %s has neither ExposePrototype nor PrototypeProvider',
      [ATypeDefinition.ConstructorName]);

  if Assigned(ATypeDefinition.PrototypeParent) then
    AConstructor.Prototype.Prototype := ATypeDefinition.PrototypeParent;

  if Assigned(ATypeDefinition.StaticSource) then
    CopyStaticMembersToConstructor(ATypeDefinition.StaticSource, AConstructor);

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
