unit Goccia.ObjectModel;

{$I Goccia.inc}

interface

uses
  Classes,
  Generics.Collections,
  SysUtils,
  TypInfo,

  Goccia.Arguments.Collection,
  Goccia.ObjectModel.Types,
  Goccia.Values.NativeFunction,
  Goccia.Values.NativeFunctionCallback,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.Primitives;

type
  TGocciaMethodHost = TObject;
  TGocciaMemberKind = Goccia.ObjectModel.Types.TGocciaMemberKind;
  TGocciaMemberFlag = Goccia.ObjectModel.Types.TGocciaMemberFlag;
  TGocciaMemberFlags = Goccia.ObjectModel.Types.TGocciaMemberFlags;
  TGocciaMemberDefinition = Goccia.ObjectModel.Types.TGocciaMemberDefinition;

const
  gmkPrototypeMethod = Goccia.ObjectModel.Types.gmkPrototypeMethod;
  gmkPrototypeGetter = Goccia.ObjectModel.Types.gmkPrototypeGetter;
  gmkPrototypeSetter = Goccia.ObjectModel.Types.gmkPrototypeSetter;
  gmkStaticMethod = Goccia.ObjectModel.Types.gmkStaticMethod;
  gmkStaticGetter = Goccia.ObjectModel.Types.gmkStaticGetter;
  gmkStaticSetter = Goccia.ObjectModel.Types.gmkStaticSetter;
  gmkSymbolProperty = Goccia.ObjectModel.Types.gmkSymbolProperty;
  gmkSymbolMethod = Goccia.ObjectModel.Types.gmkSymbolMethod;
  gmkSymbolAccessor = Goccia.ObjectModel.Types.gmkSymbolAccessor;
  gmkDataProperty = Goccia.ObjectModel.Types.gmkDataProperty;
  gmfNoFunctionPrototype = Goccia.ObjectModel.Types.gmfNoFunctionPrototype;
  gmfVariadic = Goccia.ObjectModel.Types.gmfVariadic;
  gmfNotConstructable = Goccia.ObjectModel.Types.gmfNotConstructable;

type
  EGocciaObjectModelError = class(Exception);

  TGocciaMemberCollection = class
  private
    FMembers: array of TGocciaMemberDefinition;
    procedure AddDefinition(const ADefinition: TGocciaMemberDefinition);
  public
    procedure AddMethod(const ACallback: TGocciaNativeFunctionCallback;
      const AArity: Integer;
      const AKind: TGocciaMemberKind = gmkPrototypeMethod;
      const AMemberFlags: TGocciaMemberFlags = []);
    procedure AddNamedMethod(const AExposedName: string;
      const ACallback: TGocciaNativeFunctionCallback; const AArity: Integer;
      const AKind: TGocciaMemberKind = gmkPrototypeMethod;
      const AMemberFlags: TGocciaMemberFlags = []);
    procedure AddAccessor(const AExposedName: string;
      const AGetter, ASetter: TGocciaNativeFunctionCallback;
      const AFlags: TPropertyFlags;
      const AKind: TGocciaMemberKind = gmkPrototypeGetter;
      const AMemberFlags: TGocciaMemberFlags = []);
    procedure AddSymbolMethod(const ASymbol: TGocciaValue;
      const AExposedName: string;
      const ACallback: TGocciaNativeFunctionCallback; const AArity: Integer;
      const AFlags: TPropertyFlags;
      const AMemberFlags: TGocciaMemberFlags = []);
    procedure AddSymbolDataProperty(const ASymbol: TGocciaValue;
      const AValue: TGocciaValue; const AFlags: TPropertyFlags;
      const AMemberFlags: TGocciaMemberFlags = []);
    procedure AddSymbolAccessor(const ASymbol: TGocciaValue;
      const AExposedName: string;
      const AGetter, ASetter: TGocciaNativeFunctionCallback;
      const AFlags: TPropertyFlags;
      const AMemberFlags: TGocciaMemberFlags = []);
    procedure AddDataProperty(const AExposedName: string;
      const AValue: TGocciaValue; const AFlags: TPropertyFlags;
      const AKind: TGocciaMemberKind = gmkDataProperty;
      const AMemberFlags: TGocciaMemberFlags = []);
    procedure AddPublishedGetter(const AInstanceClass: TClass;
      const APascalPropertyName, AExposedName: string;
      const AFlags: TPropertyFlags;
      const AKind: TGocciaMemberKind = gmkPrototypeGetter;
      const AMemberFlags: TGocciaMemberFlags = []);
    procedure AddHostedPropertyGetter(const AMethodHost: TObject;
      const APascalPropertyName, AExposedName: string;
      const AFlags: TPropertyFlags;
      const AKind: TGocciaMemberKind = gmkStaticGetter;
      const AMemberFlags: TGocciaMemberFlags = []);
    function ToDefinitions: TArray<TGocciaMemberDefinition>;
  end;

function BindNativeCallbackByName(const AMethodHost: TGocciaMethodHost;
  const AMethodName: string): TGocciaNativeFunctionCallback;

function DefineMethod(const ACallback: TGocciaNativeFunctionCallback;
  const AArity: Integer;
  const AKind: TGocciaMemberKind = gmkPrototypeMethod): TGocciaMemberDefinition;
function DefineNamedMethod(const AExposedName: string;
  const ACallback: TGocciaNativeFunctionCallback; const AArity: Integer;
  const AKind: TGocciaMemberKind = gmkPrototypeMethod): TGocciaMemberDefinition;
function DefineSymbolMethod(const ASymbol: TGocciaValue;
  const AExposedName: string;
  const ACallback: TGocciaNativeFunctionCallback; const AArity: Integer;
  const AFlags: TPropertyFlags): TGocciaMemberDefinition;
function DefineSymbolDataProperty(const ASymbol: TGocciaValue; const AValue: TGocciaValue;
  const AFlags: TPropertyFlags): TGocciaMemberDefinition;
function DefineSymbolAccessor(const ASymbol: TGocciaValue;
  const AExposedName: string;
  const AGetter, ASetter: TGocciaNativeFunctionCallback;
  const AFlags: TPropertyFlags): TGocciaMemberDefinition;
function DefineAccessor(const AExposedName: string;
  const AGetter, ASetter: TGocciaNativeFunctionCallback;
  const AFlags: TPropertyFlags;
  const AKind: TGocciaMemberKind = gmkPrototypeGetter): TGocciaMemberDefinition;
function DefineDataProperty(const AExposedName: string; const AValue: TGocciaValue;
  const AFlags: TPropertyFlags;
  const AKind: TGocciaMemberKind = gmkDataProperty): TGocciaMemberDefinition;
function DefinePublishedGetter(const AInstanceClass: TClass;
  const APascalPropertyName, AExposedName: string; const AFlags: TPropertyFlags;
  const AKind: TGocciaMemberKind = gmkPrototypeGetter): TGocciaMemberDefinition;
function DefineHostedPropertyGetter(const AMethodHost: TObject;
  const APascalPropertyName, AExposedName: string; const AFlags: TPropertyFlags;
  const AKind: TGocciaMemberKind = gmkStaticGetter): TGocciaMemberDefinition;

procedure RegisterMemberDefinitions(const ATarget: TObject;
  const AMembers: array of TGocciaMemberDefinition);
procedure CopyOwnProperties(const ASource, ATarget: TObject);
procedure ExposeSharedPrototypeOnConstructor(const AShared: TObject;
  const AConstructor: TGocciaValue);

implementation

uses
  Goccia.SharedPrototype,
  Goccia.Values.ClassValue,
  Goccia.Values.ErrorHelper,
  Goccia.Values.ObjectValue,
  Goccia.Values.SymbolValue;

type
  TGocciaPublishedGetterHost = class
  private
    FPropertyName: string;
    FTargetClass: TClass;
    FUseThisValue: Boolean;
    FMethodHost: TObject;
    function ConvertPropertyValue(const ATarget: TObject;
      const APropInfo: PPropInfo): TGocciaValue;
  public
    constructor Create(const ATargetClass: TClass;
      const APascalPropertyName: string; const AUseThisValue: Boolean;
      const AMethodHost: TObject = nil);
    function Invoke(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
  end;

threadvar
  GPublishedGetterHosts: TObjectList<TObject>;

function KindToString(const AKind: TGocciaMemberKind): string;
begin
  case AKind of
    gmkPrototypeMethod: Result := 'prototype method';
    gmkPrototypeGetter: Result := 'prototype getter';
    gmkPrototypeSetter: Result := 'prototype setter';
    gmkStaticMethod: Result := 'static method';
    gmkStaticGetter: Result := 'static getter';
    gmkStaticSetter: Result := 'static setter';
    gmkSymbolProperty: Result := 'symbol property';
    gmkSymbolMethod: Result := 'symbol method';
    gmkSymbolAccessor: Result := 'symbol accessor';
    gmkDataProperty: Result := 'data property';
  else
    Result := 'member';
  end;
end;

function StripPrefix(const AText, APrefix: string): string;
begin
  Result := AText;
  if Copy(Result, 1, Length(APrefix)) = APrefix then
    Delete(Result, 1, Length(APrefix));
end;

function StripSuffix(const AText, ASuffix: string): string;
begin
  Result := AText;
  if (Length(Result) >= Length(ASuffix)) and
     (Copy(Result, Length(Result) - Length(ASuffix) + 1, Length(ASuffix)) = ASuffix) then
    SetLength(Result, Length(Result) - Length(ASuffix));
end;

function InferTypePrefix(const AMethodHost: TGocciaMethodHost): string;
begin
  Result := AMethodHost.ClassName;
  Result := StripPrefix(Result, 'TGoccia');
  Result := StripPrefix(Result, 'Global');
  Result := StripSuffix(Result, 'SharedPrototype');
  Result := StripSuffix(Result, 'ObjectValue');
  Result := StripSuffix(Result, 'Builtin');
  Result := StripSuffix(Result, 'ClassValue');
  Result := StripSuffix(Result, 'Value');
end;

function StripFirstMatchingPrefix(const AMethodName, ATypePrefix: string): string;
var
  Candidate: string;
begin
  Result := AMethodName;

  Candidate := ATypePrefix;
  if (Candidate <> '') and (Copy(Result, 1, Length(Candidate)) = Candidate) then
  begin
    Delete(Result, 1, Length(Candidate));
    Exit;
  end;

  if Copy(ATypePrefix, 1, Length('TemporalPlain')) = 'TemporalPlain' then
  begin
    Candidate := Copy(ATypePrefix, Length('TemporalPlain') + 1, MaxInt);
    if (Candidate <> '') and (Copy(Result, 1, Length(Candidate)) = Candidate) then
    begin
      Delete(Result, 1, Length(Candidate));
      Exit;
    end;
  end;

  if Copy(ATypePrefix, 1, Length('Temporal')) = 'Temporal' then
  begin
    Candidate := Copy(ATypePrefix, Length('Temporal') + 1, MaxInt);
    if (Candidate <> '') and (Copy(Result, 1, Length(Candidate)) = Candidate) then
      Delete(Result, 1, Length(Candidate));
  end;
end;

function LowercaseFirst(const AValue: string): string;
begin
  Result := AValue;
  if Result <> '' then
    Result[1] := LowerCase(Copy(Result, 1, 1))[1];
end;

function InferExposedName(const ACallback: TGocciaNativeFunctionCallback): string;
var
  MethodData: TMethod;
  MethodHost: TGocciaMethodHost;
  MethodName: string;
  TypePrefix: string;
begin
  MethodData := TMethod(ACallback);
  MethodHost := TGocciaMethodHost(MethodData.Data);
  if not Assigned(MethodHost) then
    raise EGocciaObjectModelError.Create('Cannot infer exposed name: callback has no method host');

  MethodName := MethodHost.MethodName(MethodData.Code);
  if MethodName = '' then
    raise EGocciaObjectModelError.CreateFmt(
      'Cannot infer exposed name for %s.%p. The method must be published or use DefineNamedMethod.',
      [MethodHost.ClassName, Pointer(MethodData.Code)]);

  TypePrefix := InferTypePrefix(MethodHost);
  MethodName := StripFirstMatchingPrefix(MethodName, TypePrefix);
  Result := LowercaseFirst(MethodName);
end;

function DescribeMethodHost(const AMethodHost: TGocciaMethodHost): string;
begin
  if Assigned(AMethodHost) then
    Result := AMethodHost.ClassName
  else
    Result := '<nil>';
end;

function BindNativeCallbackByName(const AMethodHost: TGocciaMethodHost;
  const AMethodName: string): TGocciaNativeFunctionCallback;
var
  MethodData: TMethod;
begin
  if not Assigned(AMethodHost) then
    raise EGocciaObjectModelError.CreateFmt(
      'Cannot bind method "%s": method host is nil', [AMethodName]);

  MethodData.Code := AMethodHost.MethodAddress(AMethodName);
  MethodData.Data := AMethodHost;
  if not Assigned(MethodData.Code) then
    raise EGocciaObjectModelError.CreateFmt(
      'Cannot bind method "%s" on %s. The method must be published and match TGocciaNativeFunctionCallback.',
      [AMethodName, DescribeMethodHost(AMethodHost)]);

  Result := TGocciaNativeFunctionCallback(MethodData);
end;

function DefineMethod(const ACallback: TGocciaNativeFunctionCallback;
  const AArity: Integer; const AKind: TGocciaMemberKind): TGocciaMemberDefinition;
begin
  Result := DefineNamedMethod(InferExposedName(ACallback), ACallback, AArity, AKind);
end;

function DefineNamedMethod(const AExposedName: string;
  const ACallback: TGocciaNativeFunctionCallback; const AArity: Integer;
  const AKind: TGocciaMemberKind): TGocciaMemberDefinition;
begin
  Result.ExposedName := AExposedName;
  Result.Symbol := nil;
  Result.Kind := AKind;
  Result.MethodName := '';
  Result.Callback := ACallback;
  Result.SetterCallback := nil;
  Result.Arity := AArity;
  Result.PropertyFlags := [];
  Result.MemberFlags := [];
  Result.DataValue := nil;
  if AArity < 0 then
    Include(Result.MemberFlags, gmfVariadic);
end;

function DefineSymbolMethod(const ASymbol: TGocciaValue;
  const AExposedName: string;
  const ACallback: TGocciaNativeFunctionCallback; const AArity: Integer;
  const AFlags: TPropertyFlags): TGocciaMemberDefinition;
begin
  Result := DefineNamedMethod(AExposedName, ACallback, AArity, gmkSymbolMethod);
  Result.Symbol := ASymbol;
  Result.PropertyFlags := AFlags;
end;

function DefineSymbolDataProperty(const ASymbol: TGocciaValue; const AValue: TGocciaValue;
  const AFlags: TPropertyFlags): TGocciaMemberDefinition;
begin
  Result := DefineDataProperty('', AValue, AFlags, gmkSymbolProperty);
  Result.Symbol := ASymbol;
end;

function DefineSymbolAccessor(const ASymbol: TGocciaValue;
  const AExposedName: string;
  const AGetter, ASetter: TGocciaNativeFunctionCallback;
  const AFlags: TPropertyFlags): TGocciaMemberDefinition;
begin
  Result := DefineAccessor(AExposedName, AGetter, ASetter, AFlags, gmkSymbolAccessor);
  Result.Symbol := ASymbol;
end;

function DefineAccessor(const AExposedName: string;
  const AGetter, ASetter: TGocciaNativeFunctionCallback;
  const AFlags: TPropertyFlags; const AKind: TGocciaMemberKind): TGocciaMemberDefinition;
begin
  Result.ExposedName := AExposedName;
  Result.Symbol := nil;
  Result.Kind := AKind;
  Result.MethodName := '';
  Result.Callback := AGetter;
  Result.SetterCallback := ASetter;
  Result.Arity := 0;
  Result.PropertyFlags := AFlags;
  Result.MemberFlags := [];
  Result.DataValue := nil;
end;

function DefineDataProperty(const AExposedName: string; const AValue: TGocciaValue;
  const AFlags: TPropertyFlags; const AKind: TGocciaMemberKind): TGocciaMemberDefinition;
begin
  Result.ExposedName := AExposedName;
  Result.Symbol := nil;
  Result.Kind := AKind;
  Result.MethodName := '';
  Result.Callback := nil;
  Result.SetterCallback := nil;
  Result.Arity := 0;
  Result.PropertyFlags := AFlags;
  Result.MemberFlags := [];
  Result.DataValue := AValue;
end;

constructor TGocciaPublishedGetterHost.Create(const ATargetClass: TClass;
  const APascalPropertyName: string; const AUseThisValue: Boolean;
  const AMethodHost: TObject);
begin
  inherited Create;
  FTargetClass := ATargetClass;
  FPropertyName := APascalPropertyName;
  FUseThisValue := AUseThisValue;
  FMethodHost := AMethodHost;
end;

function TGocciaPublishedGetterHost.ConvertPropertyValue(const ATarget: TObject;
  const APropInfo: PPropInfo): TGocciaValue;
var
  PropType: PTypeInfo;
  PropKind: TTypeKind;
  PropObject: TObject;
begin
  PropType := APropInfo^.PropType;
  PropKind := PropType^.Kind;

  case PropKind of
    tkInteger, tkInt64:
      Result := TGocciaNumberLiteralValue.Create(GetInt64Prop(ATarget, APropInfo));
    tkFloat:
      Result := TGocciaNumberLiteralValue.Create(GetFloatProp(ATarget, APropInfo));
    tkAString, tkLString, tkSString, tkWString, tkUString:
      Result := TGocciaStringLiteralValue.Create(GetStrProp(ATarget, APropInfo));
    tkEnumeration:
      if SameText(String(PropType^.Name), 'Boolean') then
      begin
        if GetOrdProp(ATarget, APropInfo) <> 0 then
          Result := TGocciaBooleanLiteralValue.TrueValue
        else
          Result := TGocciaBooleanLiteralValue.FalseValue;
      end
      else
        Result := TGocciaNumberLiteralValue.Create(GetOrdProp(ATarget, APropInfo));
    tkClass:
      begin
        PropObject := GetObjectProp(ATarget, APropInfo);
        if PropObject is TGocciaValue then
          Result := TGocciaValue(PropObject)
        else
          Result := TGocciaUndefinedLiteralValue.UndefinedValue;
      end;
  else
    raise EGocciaObjectModelError.CreateFmt(
      'Published property "%s" on %s has unsupported RTTI kind %d',
      [FPropertyName, ATarget.ClassName, Ord(PropKind)]);
  end;
end;

function ClonePropertyDescriptor(
  const ADescriptor: TGocciaPropertyDescriptor): TGocciaPropertyDescriptor;
begin
  if ADescriptor is TGocciaPropertyDescriptorData then
    Result := TGocciaPropertyDescriptorData.Create(
      TGocciaPropertyDescriptorData(ADescriptor).Value,
      ADescriptor.Flags)
  else if ADescriptor is TGocciaPropertyDescriptorAccessor then
    Result := TGocciaPropertyDescriptorAccessor.Create(
      TGocciaPropertyDescriptorAccessor(ADescriptor).Getter,
      TGocciaPropertyDescriptorAccessor(ADescriptor).Setter,
      ADescriptor.Flags)
  else
    raise EGocciaObjectModelError.CreateFmt(
      'Unsupported property descriptor type %s',
      [ADescriptor.ClassName]);
end;

function TGocciaPublishedGetterHost.Invoke(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  PropInfo: PPropInfo;
  TargetObject: TObject;
begin
  if FUseThisValue then
  begin
    if not (AThisValue is FTargetClass) then
      ThrowTypeError(Format(
        'Published getter for %s requires instance of %s',
        [FPropertyName, FTargetClass.ClassName]));
    TargetObject := TObject(AThisValue);
  end
  else
    TargetObject := FMethodHost;

  PropInfo := GetPropInfo(TargetObject.ClassInfo, FPropertyName);
  if not Assigned(PropInfo) then
    raise EGocciaObjectModelError.CreateFmt(
      'Published property "%s" not found on %s',
      [FPropertyName, TargetObject.ClassName]);

  Result := ConvertPropertyValue(TargetObject, PropInfo);
end;

function DefinePublishedGetter(const AInstanceClass: TClass;
  const APascalPropertyName, AExposedName: string; const AFlags: TPropertyFlags;
  const AKind: TGocciaMemberKind): TGocciaMemberDefinition;
var
  GetterHost: TGocciaPublishedGetterHost;
begin
  GetterHost := TGocciaPublishedGetterHost.Create(
    AInstanceClass, APascalPropertyName, True);
  if not Assigned(GPublishedGetterHosts) then
    GPublishedGetterHosts := TObjectList<TObject>.Create(True);
  GPublishedGetterHosts.Add(GetterHost);
  Result := DefineAccessor(AExposedName, GetterHost.Invoke, nil, AFlags, AKind);
end;

function DefineHostedPropertyGetter(const AMethodHost: TObject;
  const APascalPropertyName, AExposedName: string; const AFlags: TPropertyFlags;
  const AKind: TGocciaMemberKind): TGocciaMemberDefinition;
var
  GetterHost: TGocciaPublishedGetterHost;
begin
  GetterHost := TGocciaPublishedGetterHost.Create(
    AMethodHost.ClassType, APascalPropertyName, False, AMethodHost);
  if not Assigned(GPublishedGetterHosts) then
    GPublishedGetterHosts := TObjectList<TObject>.Create(True);
  GPublishedGetterHosts.Add(GetterHost);
  Result := DefineAccessor(AExposedName, GetterHost.Invoke, nil, AFlags, AKind);
end;

procedure TGocciaMemberCollection.AddDefinition(
  const ADefinition: TGocciaMemberDefinition);
var
  Index: Integer;
begin
  Index := Length(FMembers);
  SetLength(FMembers, Index + 1);
  FMembers[Index] := ADefinition;
end;

procedure TGocciaMemberCollection.AddMethod(
  const ACallback: TGocciaNativeFunctionCallback; const AArity: Integer;
  const AKind: TGocciaMemberKind; const AMemberFlags: TGocciaMemberFlags);
var
  Definition: TGocciaMemberDefinition;
begin
  Definition := DefineMethod(ACallback, AArity, AKind);
  Definition.MemberFlags := Definition.MemberFlags + AMemberFlags;
  AddDefinition(Definition);
end;

procedure TGocciaMemberCollection.AddNamedMethod(const AExposedName: string;
  const ACallback: TGocciaNativeFunctionCallback; const AArity: Integer;
  const AKind: TGocciaMemberKind; const AMemberFlags: TGocciaMemberFlags);
var
  Definition: TGocciaMemberDefinition;
begin
  Definition := DefineNamedMethod(AExposedName, ACallback, AArity, AKind);
  Definition.MemberFlags := Definition.MemberFlags + AMemberFlags;
  AddDefinition(Definition);
end;

procedure TGocciaMemberCollection.AddAccessor(const AExposedName: string;
  const AGetter, ASetter: TGocciaNativeFunctionCallback;
  const AFlags: TPropertyFlags; const AKind: TGocciaMemberKind;
  const AMemberFlags: TGocciaMemberFlags);
var
  Definition: TGocciaMemberDefinition;
begin
  Definition := DefineAccessor(AExposedName, AGetter, ASetter, AFlags, AKind);
  Definition.MemberFlags := Definition.MemberFlags + AMemberFlags;
  AddDefinition(Definition);
end;

procedure TGocciaMemberCollection.AddSymbolMethod(const ASymbol: TGocciaValue;
  const AExposedName: string;
  const ACallback: TGocciaNativeFunctionCallback; const AArity: Integer;
  const AFlags: TPropertyFlags; const AMemberFlags: TGocciaMemberFlags);
var
  Definition: TGocciaMemberDefinition;
begin
  Definition := DefineSymbolMethod(ASymbol, AExposedName, ACallback, AArity, AFlags);
  Definition.MemberFlags := Definition.MemberFlags + AMemberFlags;
  AddDefinition(Definition);
end;

procedure TGocciaMemberCollection.AddSymbolDataProperty(
  const ASymbol: TGocciaValue; const AValue: TGocciaValue;
  const AFlags: TPropertyFlags; const AMemberFlags: TGocciaMemberFlags);
var
  Definition: TGocciaMemberDefinition;
begin
  Definition := DefineSymbolDataProperty(ASymbol, AValue, AFlags);
  Definition.MemberFlags := Definition.MemberFlags + AMemberFlags;
  AddDefinition(Definition);
end;

procedure TGocciaMemberCollection.AddSymbolAccessor(const ASymbol: TGocciaValue;
  const AExposedName: string;
  const AGetter, ASetter: TGocciaNativeFunctionCallback;
  const AFlags: TPropertyFlags; const AMemberFlags: TGocciaMemberFlags);
var
  Definition: TGocciaMemberDefinition;
begin
  Definition := DefineSymbolAccessor(ASymbol, AExposedName, AGetter, ASetter, AFlags);
  Definition.MemberFlags := Definition.MemberFlags + AMemberFlags;
  AddDefinition(Definition);
end;

procedure TGocciaMemberCollection.AddDataProperty(const AExposedName: string;
  const AValue: TGocciaValue; const AFlags: TPropertyFlags;
  const AKind: TGocciaMemberKind; const AMemberFlags: TGocciaMemberFlags);
var
  Definition: TGocciaMemberDefinition;
begin
  Definition := DefineDataProperty(AExposedName, AValue, AFlags, AKind);
  Definition.MemberFlags := Definition.MemberFlags + AMemberFlags;
  AddDefinition(Definition);
end;

procedure TGocciaMemberCollection.AddPublishedGetter(
  const AInstanceClass: TClass;
  const APascalPropertyName, AExposedName: string;
  const AFlags: TPropertyFlags; const AKind: TGocciaMemberKind;
  const AMemberFlags: TGocciaMemberFlags);
var
  Definition: TGocciaMemberDefinition;
begin
  Definition := DefinePublishedGetter(
    AInstanceClass, APascalPropertyName, AExposedName, AFlags, AKind);
  Definition.MemberFlags := Definition.MemberFlags + AMemberFlags;
  AddDefinition(Definition);
end;

procedure TGocciaMemberCollection.AddHostedPropertyGetter(
  const AMethodHost: TObject;
  const APascalPropertyName, AExposedName: string;
  const AFlags: TPropertyFlags; const AKind: TGocciaMemberKind;
  const AMemberFlags: TGocciaMemberFlags);
var
  Definition: TGocciaMemberDefinition;
begin
  Definition := DefineHostedPropertyGetter(
    AMethodHost, APascalPropertyName, AExposedName, AFlags, AKind);
  Definition.MemberFlags := Definition.MemberFlags + AMemberFlags;
  AddDefinition(Definition);
end;

function TGocciaMemberCollection.ToDefinitions: TArray<TGocciaMemberDefinition>;
begin
  Result := FMembers;
end;

function CreateNativeFunctionFromDef(
  const ADefinition: TGocciaMemberDefinition): TGocciaNativeFunctionValue;
begin
  if gmfNoFunctionPrototype in ADefinition.MemberFlags then
    Result := TGocciaNativeFunctionValue.CreateWithoutPrototype(
      ADefinition.Callback, ADefinition.ExposedName, ADefinition.Arity)
  else
    Result := TGocciaNativeFunctionValue.Create(
      ADefinition.Callback, ADefinition.ExposedName, ADefinition.Arity);
  if gmfNotConstructable in ADefinition.MemberFlags then
    Result.NotConstructable := True;
end;

procedure ValidateMemberDefinition(const ADefinition: TGocciaMemberDefinition);
begin
  case ADefinition.Kind of
    gmkPrototypeSetter, gmkStaticSetter:
      if ADefinition.Arity <> 1 then
        raise EGocciaObjectModelError.CreateFmt(
          'Invalid %s "%s": setters must have arity 1',
          [KindToString(ADefinition.Kind), ADefinition.ExposedName]);
    gmkPrototypeGetter, gmkStaticGetter:
      if pfWritable in ADefinition.PropertyFlags then
        raise EGocciaObjectModelError.CreateFmt(
          'Invalid %s "%s": getters cannot be writable',
          [KindToString(ADefinition.Kind), ADefinition.ExposedName]);
    gmkSymbolMethod, gmkSymbolProperty, gmkSymbolAccessor:
      if not Assigned(ADefinition.Symbol) then
        raise EGocciaObjectModelError.CreateFmt(
          'Invalid %s "%s": symbol members require a symbol value',
          [KindToString(ADefinition.Kind), ADefinition.ExposedName]);
  end;
end;

procedure RegisterMemberDefinitions(const ATarget: TObject;
  const AMembers: array of TGocciaMemberDefinition);
var
  I: Integer;
  Def: TGocciaMemberDefinition;
  GetterFn, SetterFn: TGocciaNativeFunctionValue;
  TargetObject: TGocciaObjectValue;
begin
  if not (ATarget is TGocciaObjectValue) then
    raise EGocciaObjectModelError.Create('RegisterMemberDefinitions target must be a TGocciaObjectValue');

  TargetObject := TGocciaObjectValue(ATarget);

  for I := 0 to High(AMembers) do
  begin
    Def := AMembers[I];
    ValidateMemberDefinition(Def);

    case Def.Kind of
      gmkPrototypeMethod, gmkStaticMethod:
        TargetObject.RegisterNativeMethod(CreateNativeFunctionFromDef(Def));
      gmkPrototypeGetter, gmkStaticGetter,
      gmkPrototypeSetter, gmkStaticSetter:
      begin
        if Assigned(Def.Callback) then
          GetterFn := TGocciaNativeFunctionValue.CreateWithoutPrototype(
            Def.Callback, 'get ' + Def.ExposedName, 0)
        else
          GetterFn := nil;

        if Assigned(Def.SetterCallback) then
          SetterFn := TGocciaNativeFunctionValue.CreateWithoutPrototype(
            Def.SetterCallback, 'set ' + Def.ExposedName, 1)
        else
          SetterFn := nil;

        TargetObject.DefineProperty(Def.ExposedName,
          TGocciaPropertyDescriptorAccessor.Create(GetterFn, SetterFn, Def.PropertyFlags));
      end;
      gmkSymbolMethod:
        TargetObject.DefineSymbolProperty(TGocciaSymbolValue(Def.Symbol),
          TGocciaPropertyDescriptorData.Create(
            TGocciaNativeFunctionValue.CreateWithoutPrototype(
              Def.Callback, Def.ExposedName, Def.Arity),
            Def.PropertyFlags));
      gmkSymbolAccessor:
      begin
        if Assigned(Def.Callback) then
          GetterFn := TGocciaNativeFunctionValue.CreateWithoutPrototype(
            Def.Callback, 'get ' + Def.ExposedName, 0)
        else
          GetterFn := nil;

        if Assigned(Def.SetterCallback) then
          SetterFn := TGocciaNativeFunctionValue.CreateWithoutPrototype(
            Def.SetterCallback, 'set ' + Def.ExposedName, 1)
        else
          SetterFn := nil;

        TargetObject.DefineSymbolProperty(TGocciaSymbolValue(Def.Symbol),
          TGocciaPropertyDescriptorAccessor.Create(GetterFn, SetterFn, Def.PropertyFlags));
      end;
      gmkSymbolProperty:
        TargetObject.DefineSymbolProperty(TGocciaSymbolValue(Def.Symbol),
          TGocciaPropertyDescriptorData.Create(Def.DataValue, Def.PropertyFlags));
      gmkDataProperty:
        TargetObject.DefineProperty(Def.ExposedName,
          TGocciaPropertyDescriptorData.Create(Def.DataValue, Def.PropertyFlags));
    end;
  end;
end;

procedure CopyOwnProperties(const ASource, ATarget: TObject);
var
  Key: string;
  Symbol: TGocciaSymbolValue;
  Descriptor: TGocciaPropertyDescriptor;
  SourceObject, TargetObject: TGocciaObjectValue;
begin
  if not Assigned(ASource) then
    Exit;

  if not (ASource is TGocciaObjectValue) or not (ATarget is TGocciaObjectValue) then
    raise EGocciaObjectModelError.Create('CopyOwnProperties source and target must be TGocciaObjectValue instances');

  SourceObject := TGocciaObjectValue(ASource);
  TargetObject := TGocciaObjectValue(ATarget);

  for Key in SourceObject.GetAllPropertyNames do
  begin
    Descriptor := SourceObject.GetOwnPropertyDescriptor(Key);
    if Assigned(Descriptor) then
      TargetObject.DefineProperty(Key, ClonePropertyDescriptor(Descriptor));
  end;

  for Symbol in SourceObject.GetOwnSymbols do
  begin
    Descriptor := SourceObject.GetOwnSymbolPropertyDescriptor(Symbol);
    if Assigned(Descriptor) then
      TargetObject.DefineSymbolProperty(Symbol, ClonePropertyDescriptor(Descriptor));
  end;
end;

procedure ExposeSharedPrototypeOnConstructor(const AShared: TObject;
  const AConstructor: TGocciaValue);
begin
  if not (AShared is TGocciaSharedPrototype) then
    raise EGocciaObjectModelError.Create('ExposeSharedPrototypeOnConstructor requires a TGocciaSharedPrototype');

  TGocciaSharedPrototype(AShared).ExposeOnConstructor(AConstructor);
end;

initialization
  GPublishedGetterHosts := TObjectList<TObject>.Create(True);

end.
