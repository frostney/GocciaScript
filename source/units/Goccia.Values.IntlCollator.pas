unit Goccia.Values.IntlCollator;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.ObjectModel,
  Goccia.SharedPrototype,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaIntlCollatorValue = class(TGocciaObjectValue)
  private
    FLocale: string;
    FSensitivity: string;
    FUsage: string;
    FIgnorePunctuation: Boolean;
    FNumeric: Boolean;
    FCaseFirst: string;
    FCollation: string;

    procedure InitializePrototype;
    function BoundCompare(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  public
    constructor Create(const ALocale: string; const AOptions: TGocciaObjectValue = nil);

    function ToStringTag: string; override;
    class procedure ExposePrototype(const AConstructor: TGocciaObjectValue);
  published
    function IntlCollatorCompare(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function IntlCollatorResolvedOptions(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  end;

implementation

uses
  SysUtils,

  IntlICU,
  IntlLocaleResolver,
  IntlTypes,

  Goccia.Error.Messages,
  Goccia.Intl.Helpers,
  Goccia.ObjectModel.Types,
  Goccia.Realm,
  Goccia.Values.ErrorHelper,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.SymbolValue;

var
  GIntlCollatorSharedSlot: TGocciaRealmOwnedSlotId;

threadvar
  FPrototypeMembers: TArray<TGocciaMemberDefinition>;

function GetIntlCollatorShared: TGocciaSharedPrototype; inline;
begin
  if Assigned(CurrentRealm) then
    Result := TGocciaSharedPrototype(CurrentRealm.GetOwnedSlot(GIntlCollatorSharedSlot))
  else
    Result := nil;
end;

function AsCollator(const AValue: TGocciaValue; const AMethod: string): TGocciaIntlCollatorValue;
begin
  if not (AValue is TGocciaIntlCollatorValue) then
    ThrowTypeError(AMethod + ' called on non-Collator');
  Result := TGocciaIntlCollatorValue(AValue);
end;

function SensitivityStringToEnum(const AValue: string): TIntlCollatorSensitivity;
begin
  if AValue = 'base' then
    Result := icsBase
  else if AValue = 'accent' then
    Result := icsAccent
  else if AValue = 'case' then
    Result := icsCase
  else
    Result := icsVariant;
end;

{ TGocciaIntlCollatorValue }

constructor TGocciaIntlCollatorValue.Create(const ALocale: string; const AOptions: TGocciaObjectValue);
var
  Canonical: string;
  V: TGocciaValue;
  Ignored: string;
begin
  inherited Create;
  Canonical := CanonicalizeUnicodeLocaleId(ALocale);
  if Canonical = '' then
    FLocale := DefaultLocale
  else
    FLocale := Canonical;

  // Defaults
  FSensitivity := 'variant';
  FUsage := 'sort';
  FIgnorePunctuation := False;
  FNumeric := False;
  FCaseFirst := 'false';
  FCollation := 'default';

  if Assigned(AOptions) then
  begin
    ReadValidatedStringOption(AOptions, 'sensitivity', FSensitivity);
    ReadValidatedStringOption(AOptions, 'usage', FUsage);
    V := AOptions.GetProperty('ignorePunctuation');
    if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
      FIgnorePunctuation := V.ToBooleanLiteral.Value;
    V := AOptions.GetProperty('numeric');
    if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
      FNumeric := V.ToBooleanLiteral.Value;
    ReadValidatedStringOption(AOptions, 'caseFirst', FCaseFirst);
    TryReadStringOption(AOptions, 'collation', FCollation);
    ReadValidatedStringOption(AOptions, 'localeMatcher', Ignored);
  end;

  InitializePrototype;
  if Assigned(GetIntlCollatorShared) then
    FPrototype := GetIntlCollatorShared.Prototype;

  // ES2024 §10.2.1: Intl.Collator.prototype.compare is a getter that returns
  // a bound compare function.  Define an own 'compare' property so that
  // extracting `collator.compare` as a callback works without losing `this`.
  AssignProperty('compare',
    TGocciaNativeFunctionValue.CreateWithoutPrototype(BoundCompare, 'compare', 2));
end;

function TGocciaIntlCollatorValue.ToStringTag: string;
begin
  Result := 'Intl.Collator';
end;

procedure TGocciaIntlCollatorValue.InitializePrototype;
var
  Members: TGocciaMemberCollection;
  Shared: TGocciaSharedPrototype;
begin
  if not Assigned(CurrentRealm) then Exit;
  if Assigned(GetIntlCollatorShared) then Exit;

  Shared := TGocciaSharedPrototype.Create(Self);
  CurrentRealm.SetOwnedSlot(GIntlCollatorSharedSlot, Shared);
  if Length(FPrototypeMembers) = 0 then
  begin
    Members := TGocciaMemberCollection.Create;
    try
      Members.AddNamedMethod('compare', IntlCollatorCompare, 2,
        gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddNamedMethod('resolvedOptions', IntlCollatorResolvedOptions, 0,
        gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddSymbolDataProperty(
        TGocciaSymbolValue.WellKnownToStringTag,
        TGocciaStringLiteralValue.Create('Intl.Collator'),
        [pfConfigurable]);
      FPrototypeMembers := Members.ToDefinitions;
    finally
      Members.Free;
    end;
  end;
  RegisterMemberDefinitions(Shared.Prototype, FPrototypeMembers);
end;

class procedure TGocciaIntlCollatorValue.ExposePrototype(const AConstructor: TGocciaObjectValue);
var
  Shared: TGocciaSharedPrototype;
begin
  Shared := GetIntlCollatorShared;
  if not Assigned(Shared) then
  begin
    TGocciaIntlCollatorValue.Create(DefaultLocale);
    Shared := GetIntlCollatorShared;
  end;
  if Assigned(Shared) then
    ExposeSharedPrototypeOnConstructor(Shared, AConstructor);
end;

function TGocciaIntlCollatorValue.BoundCompare(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Str1, Str2: UnicodeString;
  CompareResult: Integer;
begin
  // Bound compare uses Self directly, so it works when detached from the
  // Collator (e.g. passed as a callback to Array.prototype.sort).
  if AArgs.Length < 2 then
    ThrowTypeError('Intl.Collator.prototype.compare requires two arguments');
  Str1 := UnicodeString(AArgs.GetElement(0).ToStringLiteral.Value);
  Str2 := UnicodeString(AArgs.GetElement(1).ToStringLiteral.Value);

  if TryICUCompareStrings(FLocale, Str1, Str2,
    SensitivityStringToEnum(FSensitivity), FIgnorePunctuation, CompareResult) then
    Result := TGocciaNumberLiteralValue.Create(CompareResult)
  else
    Result := TGocciaNumberLiteralValue.Create(CompareStr(string(Str1), string(Str2)));
end;

function TGocciaIntlCollatorValue.IntlCollatorCompare(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  C: TGocciaIntlCollatorValue;
  Str1, Str2: UnicodeString;
  CompareResult: Integer;
begin
  C := AsCollator(AThisValue, 'Intl.Collator.prototype.compare');
  if AArgs.Length < 2 then
    ThrowTypeError('Intl.Collator.prototype.compare requires two arguments');
  Str1 := UnicodeString(AArgs.GetElement(0).ToStringLiteral.Value);
  Str2 := UnicodeString(AArgs.GetElement(1).ToStringLiteral.Value);

  if TryICUCompareStrings(C.FLocale, Str1, Str2,
    SensitivityStringToEnum(C.FSensitivity), C.FIgnorePunctuation, CompareResult) then
    Result := TGocciaNumberLiteralValue.Create(CompareResult)
  else
    Result := TGocciaNumberLiteralValue.Create(CompareStr(string(Str1), string(Str2)));
end;

function TGocciaIntlCollatorValue.IntlCollatorResolvedOptions(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  C: TGocciaIntlCollatorValue;
  Obj: TGocciaObjectValue;
begin
  C := AsCollator(AThisValue, 'Intl.Collator.prototype.resolvedOptions');
  Obj := TGocciaObjectValue.Create(TGocciaObjectValue.SharedObjectPrototype);
  Obj.AssignProperty('locale', TGocciaStringLiteralValue.Create(C.FLocale));
  Obj.AssignProperty('usage', TGocciaStringLiteralValue.Create(C.FUsage));
  Obj.AssignProperty('sensitivity', TGocciaStringLiteralValue.Create(C.FSensitivity));
  Obj.AssignProperty('ignorePunctuation', TGocciaBooleanLiteralValue.Create(C.FIgnorePunctuation));
  Obj.AssignProperty('numeric', TGocciaBooleanLiteralValue.Create(C.FNumeric));
  Obj.AssignProperty('caseFirst', TGocciaStringLiteralValue.Create(C.FCaseFirst));
  Obj.AssignProperty('collation', TGocciaStringLiteralValue.Create(C.FCollation));
  Result := Obj;
end;

initialization
  GIntlCollatorSharedSlot := RegisterRealmOwnedSlot('Intl.Collator.shared');

end.
