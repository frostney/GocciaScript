unit Goccia.Values.IntlPluralRules;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.ObjectModel,
  Goccia.SharedPrototype,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaIntlPluralRulesValue = class(TGocciaObjectValue)
  private
    FLocale: string;
    FType: string;
    FMinimumIntegerDigits: Integer;
    FMinimumFractionDigits: Integer;
    FMaximumFractionDigits: Integer;
    FMinimumSignificantDigits: Integer;
    FMaximumSignificantDigits: Integer;

    procedure InitializePrototype;
  public
    constructor Create(const ALocale: string; const AOptions: TGocciaObjectValue = nil);

    function ToStringTag: string; override;
    class procedure ExposePrototype(const AConstructor: TGocciaObjectValue);
  published
    function IntlPluralRulesSelect(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function IntlPluralRulesSelectRange(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function IntlPluralRulesResolvedOptions(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  end;

implementation

uses
  Math,
  SysUtils,

  IntlICU,
  IntlLocaleResolver,
  IntlTypes,

  Goccia.Error.Messages,
  Goccia.Intl.CLDRData,
  Goccia.ObjectModel.Types,
  Goccia.Realm,
  Goccia.Utils,
  Goccia.Values.ArrayValue,
  Goccia.Values.ErrorHelper,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.SymbolValue;

var
  GIntlPluralRulesSharedSlot: TGocciaRealmOwnedSlotId;

threadvar
  FPrototypeMembers: TArray<TGocciaMemberDefinition>;

function GetIntlPluralRulesShared: TGocciaSharedPrototype; inline;
begin
  if Assigned(CurrentRealm) then
    Result := TGocciaSharedPrototype(CurrentRealm.GetOwnedSlot(GIntlPluralRulesSharedSlot))
  else
    Result := nil;
end;

function AsPluralRules(const AValue: TGocciaValue; const AMethod: string): TGocciaIntlPluralRulesValue;
begin
  if not (AValue is TGocciaIntlPluralRulesValue) then
    ThrowTypeError(AMethod + ' called on non-PluralRules');
  Result := TGocciaIntlPluralRulesValue(AValue);
end;

function PluralTypeStringToEnum(const AValue: string): TIntlPluralType;
begin
  if AValue = 'ordinal' then
    Result := iptOrdinal
  else
    Result := iptCardinal;
end;

function SelectPluralWithCLDR(AValue: Double; const ALocale: string;
  ACardinal: Boolean): string;
var
  Rules: TIntlPluralRuleSet;
  IntVal: Int64;
  HasFraction: Boolean;
begin
  Result := 'other';

  if not TryGetPluralRules(ALocale, ACardinal, Rules) then
    Exit;

  IntVal := Trunc(AValue);
  HasFraction := Abs(AValue - IntVal) > 1e-10;

  // Check "zero": used by some languages for n = 0
  if (Rules.Zero <> '') and (IntVal = 0) and not HasFraction then
  begin
    Result := 'zero';
    Exit;
  end;

  // Check "one": most common is i = 1 and v = 0
  if (Rules.One <> '') and (IntVal = 1) and not HasFraction then
  begin
    Result := 'one';
    Exit;
  end;

  // Check "two": Arabic, etc. for n = 2
  if (Rules.Two <> '') and (IntVal = 2) and not HasFraction then
  begin
    Result := 'two';
    Exit;
  end;
end;

{ TGocciaIntlPluralRulesValue }

constructor TGocciaIntlPluralRulesValue.Create(const ALocale: string; const AOptions: TGocciaObjectValue);
var
  Canonical: string;
  V: TGocciaValue;
begin
  inherited Create;
  Canonical := CanonicalizeUnicodeLocaleId(ALocale);
  if Canonical = '' then
    FLocale := DefaultLocale
  else
    FLocale := Canonical;

  // Defaults
  FType := 'cardinal';
  FMinimumIntegerDigits := 1;
  FMinimumFractionDigits := -1;
  FMaximumFractionDigits := -1;
  FMinimumSignificantDigits := -1;
  FMaximumSignificantDigits := -1;

  if Assigned(AOptions) then
  begin
    V := AOptions.GetProperty('localeMatcher');
    if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
    begin
      if ContainsNulCharacter(V.ToStringLiteral.Value) then
        ThrowRangeError(Format(SErrorIntlInvalidOption, [V.ToStringLiteral.Value, 'localeMatcher']));
    end;
    V := AOptions.GetProperty('type');
    if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
    begin
      FType := V.ToStringLiteral.Value;
      if ContainsNulCharacter(FType) then
        ThrowRangeError(Format(SErrorIntlInvalidOption, [FType, 'type']));
    end;
    V := AOptions.GetProperty('minimumIntegerDigits');
    if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
      FMinimumIntegerDigits := ToIntegerWithTruncationValue(V);
    V := AOptions.GetProperty('minimumFractionDigits');
    if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
      FMinimumFractionDigits := ToIntegerWithTruncationValue(V);
    V := AOptions.GetProperty('maximumFractionDigits');
    if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
      FMaximumFractionDigits := ToIntegerWithTruncationValue(V);
    V := AOptions.GetProperty('minimumSignificantDigits');
    if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
      FMinimumSignificantDigits := ToIntegerWithTruncationValue(V);
    V := AOptions.GetProperty('maximumSignificantDigits');
    if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
      FMaximumSignificantDigits := ToIntegerWithTruncationValue(V);
  end;

  // ECMA-402 §16.1.2 / §15.1.6 SetNumberFormatDigitOptions: validate digit
  // ranges before defaulting, matching Intl.NumberFormat.
  if (FMinimumIntegerDigits < 1) or (FMinimumIntegerDigits > 21) then
    ThrowRangeError(Format(SErrorIntlDigitsOutOfRange, ['minimumIntegerDigits', 1, 21]));
  if (FMinimumFractionDigits >= 0) and (FMinimumFractionDigits > 100) then
    ThrowRangeError(Format(SErrorIntlDigitsOutOfRange, ['minimumFractionDigits', 0, 100]));
  if (FMaximumFractionDigits >= 0) and (FMaximumFractionDigits > 100) then
    ThrowRangeError(Format(SErrorIntlDigitsOutOfRange, ['maximumFractionDigits', 0, 100]));
  if (FMinimumSignificantDigits >= 0) and
     ((FMinimumSignificantDigits < 1) or (FMinimumSignificantDigits > 21)) then
    ThrowRangeError(Format(SErrorIntlDigitsOutOfRange, ['minimumSignificantDigits', 1, 21]));
  if (FMaximumSignificantDigits >= 0) and
     ((FMaximumSignificantDigits < 1) or (FMaximumSignificantDigits > 21)) then
    ThrowRangeError(Format(SErrorIntlDigitsOutOfRange, ['maximumSignificantDigits', 1, 21]));

  if (FMinimumSignificantDigits >= 0) or (FMaximumSignificantDigits >= 0) then
  begin
    if FMinimumSignificantDigits < 0 then
      FMinimumSignificantDigits := 1;
    if FMaximumSignificantDigits < 0 then
      FMaximumSignificantDigits := 21;
    FMinimumFractionDigits := -1;
    FMaximumFractionDigits := -1;
  end
  else
  begin
    if FMinimumFractionDigits < 0 then
      FMinimumFractionDigits := 0;
    if FMaximumFractionDigits < 0 then
      FMaximumFractionDigits := 3;
  end;

  InitializePrototype;
  if Assigned(GetIntlPluralRulesShared) then
    FPrototype := GetIntlPluralRulesShared.Prototype;
end;

function TGocciaIntlPluralRulesValue.ToStringTag: string;
begin
  Result := 'Intl.PluralRules';
end;

procedure TGocciaIntlPluralRulesValue.InitializePrototype;
var
  Members: TGocciaMemberCollection;
  Shared: TGocciaSharedPrototype;
begin
  if not Assigned(CurrentRealm) then Exit;
  if Assigned(GetIntlPluralRulesShared) then Exit;

  Shared := TGocciaSharedPrototype.Create(Self);
  CurrentRealm.SetOwnedSlot(GIntlPluralRulesSharedSlot, Shared);
  if Length(FPrototypeMembers) = 0 then
  begin
    Members := TGocciaMemberCollection.Create;
    try
      Members.AddNamedMethod('select', IntlPluralRulesSelect, 1,
        gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddNamedMethod('selectRange', IntlPluralRulesSelectRange, 2,
        gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddNamedMethod('resolvedOptions', IntlPluralRulesResolvedOptions, 0,
        gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddSymbolDataProperty(
        TGocciaSymbolValue.WellKnownToStringTag,
        TGocciaStringLiteralValue.Create('Intl.PluralRules'),
        [pfConfigurable]);
      FPrototypeMembers := Members.ToDefinitions;
    finally
      Members.Free;
    end;
  end;
  RegisterMemberDefinitions(Shared.Prototype, FPrototypeMembers);
end;

class procedure TGocciaIntlPluralRulesValue.ExposePrototype(const AConstructor: TGocciaObjectValue);
var
  Shared: TGocciaSharedPrototype;
begin
  Shared := GetIntlPluralRulesShared;
  if not Assigned(Shared) then
  begin
    TGocciaIntlPluralRulesValue.Create(DefaultLocale);
    Shared := GetIntlPluralRulesShared;
  end;
  if Assigned(Shared) then
    ExposeSharedPrototypeOnConstructor(Shared, AConstructor);
end;

function TGocciaIntlPluralRulesValue.IntlPluralRulesSelect(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  PR: TGocciaIntlPluralRulesValue;
  NumValue: Double;
  Category: string;
begin
  PR := AsPluralRules(AThisValue, 'Intl.PluralRules.prototype.select');
  if AArgs.Length < 1 then
    ThrowTypeError('Intl.PluralRules.prototype.select requires a value');
  NumValue := AArgs.GetElement(0).ToNumberLiteral.Value;

  if TryICUSelectPlural(PR.FLocale, NumValue, PluralTypeStringToEnum(PR.FType), Category) then
    Result := TGocciaStringLiteralValue.Create(Category)
  else
    Result := TGocciaStringLiteralValue.Create(
      SelectPluralWithCLDR(NumValue, PR.FLocale,
        PluralTypeStringToEnum(PR.FType) = iptCardinal));
end;

function TGocciaIntlPluralRulesValue.IntlPluralRulesSelectRange(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  PR: TGocciaIntlPluralRulesValue;
  StartVal, EndVal: Double;
  StartCat, EndCat: string;
begin
  PR := AsPluralRules(AThisValue, 'Intl.PluralRules.prototype.selectRange');
  if AArgs.Length < 2 then
    ThrowTypeError('Intl.PluralRules.prototype.selectRange requires start and end values');
  StartVal := AArgs.GetElement(0).ToNumberLiteral.Value;
  EndVal := AArgs.GetElement(1).ToNumberLiteral.Value;

  // Select plural category for the end value as a fallback
  if not TryICUSelectPlural(PR.FLocale, StartVal, PluralTypeStringToEnum(PR.FType), StartCat) then
    StartCat := SelectPluralWithCLDR(StartVal, PR.FLocale,
      PluralTypeStringToEnum(PR.FType) = iptCardinal);
  if not TryICUSelectPlural(PR.FLocale, EndVal, PluralTypeStringToEnum(PR.FType), EndCat) then
    EndCat := SelectPluralWithCLDR(EndVal, PR.FLocale,
      PluralTypeStringToEnum(PR.FType) = iptCardinal);

  // ECMA-402 range resolution: use end value category as the result
  Result := TGocciaStringLiteralValue.Create(EndCat);
end;

function TGocciaIntlPluralRulesValue.IntlPluralRulesResolvedOptions(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  PR: TGocciaIntlPluralRulesValue;
  Obj: TGocciaObjectValue;
  Rules: TIntlPluralRuleSet;
  CatArr: TGocciaArrayValue;
begin
  PR := AsPluralRules(AThisValue, 'Intl.PluralRules.prototype.resolvedOptions');
  Obj := TGocciaObjectValue.Create(TGocciaObjectValue.SharedObjectPrototype);
  Obj.AssignProperty('locale', TGocciaStringLiteralValue.Create(PR.FLocale));
  Obj.AssignProperty('type', TGocciaStringLiteralValue.Create(PR.FType));
  Obj.AssignProperty('minimumIntegerDigits',
    TGocciaNumberLiteralValue.Create(PR.FMinimumIntegerDigits));
  if PR.FMinimumFractionDigits >= 0 then
    Obj.AssignProperty('minimumFractionDigits',
      TGocciaNumberLiteralValue.Create(PR.FMinimumFractionDigits));
  if PR.FMaximumFractionDigits >= 0 then
    Obj.AssignProperty('maximumFractionDigits',
      TGocciaNumberLiteralValue.Create(PR.FMaximumFractionDigits));
  if PR.FMinimumSignificantDigits >= 0 then
    Obj.AssignProperty('minimumSignificantDigits',
      TGocciaNumberLiteralValue.Create(PR.FMinimumSignificantDigits));
  if PR.FMaximumSignificantDigits >= 0 then
    Obj.AssignProperty('maximumSignificantDigits',
      TGocciaNumberLiteralValue.Create(PR.FMaximumSignificantDigits));

  CatArr := TGocciaArrayValue.Create;
  if TryGetPluralRules(PR.FLocale, PluralTypeStringToEnum(PR.FType) = iptCardinal, Rules) then
  begin
    if Rules.Zero <> '' then
      CatArr.Elements.Add(TGocciaStringLiteralValue.Create('zero'));
    if Rules.One <> '' then
      CatArr.Elements.Add(TGocciaStringLiteralValue.Create('one'));
    if Rules.Two <> '' then
      CatArr.Elements.Add(TGocciaStringLiteralValue.Create('two'));
    if Rules.Few <> '' then
      CatArr.Elements.Add(TGocciaStringLiteralValue.Create('few'));
    if Rules.Many <> '' then
      CatArr.Elements.Add(TGocciaStringLiteralValue.Create('many'));
  end;
  CatArr.Elements.Add(TGocciaStringLiteralValue.Create('other'));
  Obj.AssignProperty('pluralCategories', CatArr);

  Result := Obj;
end;

initialization
  GIntlPluralRulesSharedSlot := RegisterRealmOwnedSlot('Intl.PluralRules.shared');

end.
