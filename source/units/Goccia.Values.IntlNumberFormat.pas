unit Goccia.Values.IntlNumberFormat;

{$I Goccia.inc}

interface

uses
  IntlTypes,

  Goccia.Arguments.Collection,
  Goccia.ObjectModel,
  Goccia.SharedPrototype,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaIntlNumberFormatValue = class(TGocciaObjectValue)
  private
    FLocale: string;
    FStyle: string;
    FCurrency: string;
    FCurrencyDisplay: string;
    FCurrencySign: string;
    FUnitIdentifier: string;
    FUnitDisplay: string;
    FNotation: string;
    FCompactDisplay: string;
    FSignDisplay: string;
    FUseGrouping: string;
    FRoundingMode: string;
    FMinimumIntegerDigits: Integer;
    FMinimumFractionDigits: Integer;
    FMaximumFractionDigits: Integer;
    FMinimumSignificantDigits: Integer;
    FMaximumSignificantDigits: Integer;
    FNumberingSystem: string;
    FResolvedOptions: TIntlNumberFormatOptions;

    procedure InitializePrototype;
    procedure ReadOptions(const AOptions: TGocciaObjectValue);
  public
    constructor Create(const ALocale: string; const AOptions: TGocciaObjectValue = nil);

    function ToStringTag: string; override;
    class procedure ExposePrototype(const AConstructor: TGocciaObjectValue);
  published
    function IntlNumberFormatFormat(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function IntlNumberFormatFormatToParts(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function IntlNumberFormatResolvedOptions(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  end;

implementation

uses
  SysUtils,

  IntlICU,
  IntlLocaleResolver,

  Goccia.Error.Messages,
  Goccia.ObjectModel.Types,
  Goccia.Realm,
  Goccia.Values.ArrayValue,
  Goccia.Values.ErrorHelper,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.SymbolValue;

var
  GIntlNumberFormatSharedSlot: TGocciaRealmOwnedSlotId;

threadvar
  FPrototypeMembers: TArray<TGocciaMemberDefinition>;

function GetIntlNumberFormatShared: TGocciaSharedPrototype; inline;
begin
  if Assigned(CurrentRealm) then
    Result := TGocciaSharedPrototype(CurrentRealm.GetOwnedSlot(GIntlNumberFormatSharedSlot))
  else
    Result := nil;
end;

function AsNumberFormat(const AValue: TGocciaValue; const AMethod: string): TGocciaIntlNumberFormatValue;
begin
  if not (AValue is TGocciaIntlNumberFormatValue) then
    ThrowTypeError(AMethod + ' called on non-NumberFormat');
  Result := TGocciaIntlNumberFormatValue(AValue);
end;

function StyleStringToEnum(const AValue: string): TIntlNumberStyle;
begin
  if AValue = 'currency' then
    Result := insCurrency
  else if AValue = 'percent' then
    Result := insPercent
  else if AValue = 'unit' then
    Result := insUnit
  else
    Result := insDecimal;
end;

function CurrencyDisplayStringToEnum(const AValue: string): TIntlNumberCurrencyDisplay;
begin
  if AValue = 'narrowSymbol' then
    Result := incdNarrowSymbol
  else if AValue = 'code' then
    Result := incdCode
  else if AValue = 'name' then
    Result := incdName
  else
    Result := incdSymbol;
end;

function SignDisplayStringToEnum(const AValue: string): TIntlNumberSignDisplay;
begin
  if AValue = 'never' then
    Result := insdNever
  else if AValue = 'always' then
    Result := insdAlways
  else if AValue = 'exceptZero' then
    Result := insdExceptZero
  else if AValue = 'negative' then
    Result := insdNegative
  else
    Result := insdAuto;
end;

function NotationStringToEnum(const AValue: string): TIntlNumberNotation;
begin
  if AValue = 'scientific' then
    Result := innScientific
  else if AValue = 'engineering' then
    Result := innEngineering
  else if AValue = 'compact' then
    Result := innCompact
  else
    Result := innStandard;
end;

function UnitDisplayStringToEnum(const AValue: string): TIntlNumberUnitDisplay;
begin
  if AValue = 'long' then
    Result := inudLong
  else if AValue = 'narrow' then
    Result := inudNarrow
  else
    Result := inudShort;
end;

function FormatPartsToArray(const AParts: TIntlFormatPartArray): TGocciaArrayValue;
var
  I: Integer;
  PartObj: TGocciaObjectValue;
begin
  Result := TGocciaArrayValue.Create;
  for I := 0 to Length(AParts) - 1 do
  begin
    PartObj := TGocciaObjectValue.Create(TGocciaObjectValue.SharedObjectPrototype);
    PartObj.AssignProperty('type', TGocciaStringLiteralValue.Create(AParts[I].PartType));
    PartObj.AssignProperty('value', TGocciaStringLiteralValue.Create(AParts[I].Value));
    Result.Elements.Add(PartObj);
  end;
end;

{ TGocciaIntlNumberFormatValue }

procedure TGocciaIntlNumberFormatValue.ReadOptions(const AOptions: TGocciaObjectValue);
var
  V: TGocciaValue;
begin
  if not Assigned(AOptions) then Exit;

  V := AOptions.GetProperty('style');
  if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
    FStyle := V.ToStringLiteral.Value;
  V := AOptions.GetProperty('currency');
  if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
    FCurrency := V.ToStringLiteral.Value;
  V := AOptions.GetProperty('currencyDisplay');
  if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
    FCurrencyDisplay := V.ToStringLiteral.Value;
  V := AOptions.GetProperty('currencySign');
  if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
    FCurrencySign := V.ToStringLiteral.Value;
  V := AOptions.GetProperty('unit');
  if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
    FUnitIdentifier := V.ToStringLiteral.Value;
  V := AOptions.GetProperty('unitDisplay');
  if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
    FUnitDisplay := V.ToStringLiteral.Value;
  V := AOptions.GetProperty('notation');
  if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
    FNotation := V.ToStringLiteral.Value;
  V := AOptions.GetProperty('compactDisplay');
  if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
    FCompactDisplay := V.ToStringLiteral.Value;
  V := AOptions.GetProperty('signDisplay');
  if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
    FSignDisplay := V.ToStringLiteral.Value;
  V := AOptions.GetProperty('useGrouping');
  if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
    FUseGrouping := V.ToStringLiteral.Value;
  V := AOptions.GetProperty('roundingMode');
  if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
    FRoundingMode := V.ToStringLiteral.Value;
  V := AOptions.GetProperty('minimumIntegerDigits');
  if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
    FMinimumIntegerDigits := Trunc(V.ToNumberLiteral.Value);
  V := AOptions.GetProperty('minimumFractionDigits');
  if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
    FMinimumFractionDigits := Trunc(V.ToNumberLiteral.Value);
  V := AOptions.GetProperty('maximumFractionDigits');
  if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
    FMaximumFractionDigits := Trunc(V.ToNumberLiteral.Value);
  V := AOptions.GetProperty('minimumSignificantDigits');
  if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
    FMinimumSignificantDigits := Trunc(V.ToNumberLiteral.Value);
  V := AOptions.GetProperty('maximumSignificantDigits');
  if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
    FMaximumSignificantDigits := Trunc(V.ToNumberLiteral.Value);
  V := AOptions.GetProperty('numberingSystem');
  if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
    FNumberingSystem := V.ToStringLiteral.Value;
end;

constructor TGocciaIntlNumberFormatValue.Create(const ALocale: string; const AOptions: TGocciaObjectValue);
var
  Canonical: string;
begin
  inherited Create;
  Canonical := CanonicalizeUnicodeLocaleId(ALocale);
  if Canonical = '' then
    FLocale := DefaultLocale
  else
    FLocale := Canonical;

  // Defaults
  FStyle := 'decimal';
  FCurrencyDisplay := 'symbol';
  FCurrencySign := 'standard';
  FUnitDisplay := 'short';
  FNotation := 'standard';
  FCompactDisplay := 'short';
  FSignDisplay := 'auto';
  FUseGrouping := 'auto';
  FRoundingMode := 'halfExpand';
  FMinimumIntegerDigits := 1;
  FMinimumFractionDigits := -1;
  FMaximumFractionDigits := -1;
  FMinimumSignificantDigits := -1;
  FMaximumSignificantDigits := -1;

  ReadOptions(AOptions);

  // Validate style-dependent required options
  if (FStyle = 'currency') and (FCurrency = '') then
    ThrowTypeError(SErrorIntlMissingCurrency);
  if (FStyle = 'unit') and (FUnitIdentifier = '') then
    ThrowTypeError(SErrorIntlMissingUnit);

  // Validate digit ranges
  if (FMinimumIntegerDigits < 1) or (FMinimumIntegerDigits > 21) then
    ThrowRangeError(Format(SErrorIntlDigitsOutOfRange, ['minimumIntegerDigits', 1, 21]));
  if (FMinimumFractionDigits >= 0) and
     ((FMinimumFractionDigits < 0) or (FMinimumFractionDigits > 100)) then
    ThrowRangeError(Format(SErrorIntlDigitsOutOfRange, ['minimumFractionDigits', 0, 100]));
  if (FMaximumFractionDigits >= 0) and
     ((FMaximumFractionDigits < 0) or (FMaximumFractionDigits > 100)) then
    ThrowRangeError(Format(SErrorIntlDigitsOutOfRange, ['maximumFractionDigits', 0, 100]));
  if (FMinimumSignificantDigits >= 0) and
     ((FMinimumSignificantDigits < 1) or (FMinimumSignificantDigits > 21)) then
    ThrowRangeError(Format(SErrorIntlDigitsOutOfRange, ['minimumSignificantDigits', 1, 21]));
  if (FMaximumSignificantDigits >= 0) and
     ((FMaximumSignificantDigits < 1) or (FMaximumSignificantDigits > 21)) then
    ThrowRangeError(Format(SErrorIntlDigitsOutOfRange, ['maximumSignificantDigits', 1, 21]));

  // Build resolved ICU options
  FResolvedOptions := DefaultNumberFormatOptions;
  FResolvedOptions.Style := StyleStringToEnum(FStyle);
  FResolvedOptions.Currency := FCurrency;
  FResolvedOptions.CurrencyDisplay := CurrencyDisplayStringToEnum(FCurrencyDisplay);
  FResolvedOptions.UnitIdentifier := FUnitIdentifier;
  FResolvedOptions.UnitDisplay := UnitDisplayStringToEnum(FUnitDisplay);
  FResolvedOptions.Notation := NotationStringToEnum(FNotation);
  FResolvedOptions.SignDisplay := SignDisplayStringToEnum(FSignDisplay);
  FResolvedOptions.MinimumIntegerDigits := FMinimumIntegerDigits;
  FResolvedOptions.MinimumFractionDigits := FMinimumFractionDigits;
  FResolvedOptions.MaximumFractionDigits := FMaximumFractionDigits;
  FResolvedOptions.MinimumSignificantDigits := FMinimumSignificantDigits;
  FResolvedOptions.MaximumSignificantDigits := FMaximumSignificantDigits;
  FResolvedOptions.NumberingSystem := FNumberingSystem;

  InitializePrototype;
  if Assigned(GetIntlNumberFormatShared) then
    FPrototype := GetIntlNumberFormatShared.Prototype;
end;

function TGocciaIntlNumberFormatValue.ToStringTag: string;
begin
  Result := 'Intl.NumberFormat';
end;

procedure TGocciaIntlNumberFormatValue.InitializePrototype;
var
  Members: TGocciaMemberCollection;
  Shared: TGocciaSharedPrototype;
begin
  if not Assigned(CurrentRealm) then Exit;
  if Assigned(GetIntlNumberFormatShared) then Exit;

  Shared := TGocciaSharedPrototype.Create(Self);
  CurrentRealm.SetOwnedSlot(GIntlNumberFormatSharedSlot, Shared);
  if Length(FPrototypeMembers) = 0 then
  begin
    Members := TGocciaMemberCollection.Create;
    try
      Members.AddNamedMethod('format', IntlNumberFormatFormat, 1,
        gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddNamedMethod('formatToParts', IntlNumberFormatFormatToParts, 1,
        gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddNamedMethod('resolvedOptions', IntlNumberFormatResolvedOptions, 0,
        gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddSymbolDataProperty(
        TGocciaSymbolValue.WellKnownToStringTag,
        TGocciaStringLiteralValue.Create('Intl.NumberFormat'),
        [pfConfigurable]);
      FPrototypeMembers := Members.ToDefinitions;
    finally
      Members.Free;
    end;
  end;
  RegisterMemberDefinitions(Shared.Prototype, FPrototypeMembers);
end;

class procedure TGocciaIntlNumberFormatValue.ExposePrototype(const AConstructor: TGocciaObjectValue);
var
  Shared: TGocciaSharedPrototype;
begin
  Shared := GetIntlNumberFormatShared;
  if not Assigned(Shared) then
  begin
    TGocciaIntlNumberFormatValue.Create(DefaultLocale);
    Shared := GetIntlNumberFormatShared;
  end;
  if Assigned(Shared) then
    ExposeSharedPrototypeOnConstructor(Shared, AConstructor);
end;

function TGocciaIntlNumberFormatValue.IntlNumberFormatFormat(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  NF: TGocciaIntlNumberFormatValue;
  NumValue: Double;
  Formatted: string;
begin
  NF := AsNumberFormat(AThisValue, 'Intl.NumberFormat.prototype.format');
  if AArgs.Length < 1 then
    ThrowTypeError('Intl.NumberFormat.prototype.format requires a value');
  NumValue := AArgs.GetElement(0).ToNumberLiteral.Value;

  if TryICUFormatNumber(NF.FLocale, NumValue, NF.FResolvedOptions, Formatted) then
    Result := TGocciaStringLiteralValue.Create(Formatted)
  else
    Result := TGocciaStringLiteralValue.Create(FloatToStr(NumValue));
end;

function TGocciaIntlNumberFormatValue.IntlNumberFormatFormatToParts(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  NF: TGocciaIntlNumberFormatValue;
  NumValue: Double;
  Parts: TIntlFormatPartArray;
begin
  NF := AsNumberFormat(AThisValue, 'Intl.NumberFormat.prototype.formatToParts');
  if AArgs.Length < 1 then
    ThrowTypeError('Intl.NumberFormat.prototype.formatToParts requires a value');
  NumValue := AArgs.GetElement(0).ToNumberLiteral.Value;

  if TryICUFormatNumberToParts(NF.FLocale, NumValue, NF.FResolvedOptions, Parts) then
    Result := FormatPartsToArray(Parts)
  else
    Result := TGocciaArrayValue.Create;
end;

function TGocciaIntlNumberFormatValue.IntlNumberFormatResolvedOptions(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  NF: TGocciaIntlNumberFormatValue;
  Obj: TGocciaObjectValue;
begin
  NF := AsNumberFormat(AThisValue, 'Intl.NumberFormat.prototype.resolvedOptions');
  Obj := TGocciaObjectValue.Create(TGocciaObjectValue.SharedObjectPrototype);
  Obj.AssignProperty('locale', TGocciaStringLiteralValue.Create(NF.FLocale));
  Obj.AssignProperty('style', TGocciaStringLiteralValue.Create(NF.FStyle));
  if NF.FCurrency <> '' then
    Obj.AssignProperty('currency', TGocciaStringLiteralValue.Create(NF.FCurrency));
  if NF.FStyle = 'currency' then
  begin
    Obj.AssignProperty('currencyDisplay', TGocciaStringLiteralValue.Create(NF.FCurrencyDisplay));
    Obj.AssignProperty('currencySign', TGocciaStringLiteralValue.Create(NF.FCurrencySign));
  end;
  if NF.FUnitIdentifier <> '' then
    Obj.AssignProperty('unit', TGocciaStringLiteralValue.Create(NF.FUnitIdentifier));
  if NF.FStyle = 'unit' then
    Obj.AssignProperty('unitDisplay', TGocciaStringLiteralValue.Create(NF.FUnitDisplay));
  Obj.AssignProperty('notation', TGocciaStringLiteralValue.Create(NF.FNotation));
  Obj.AssignProperty('signDisplay', TGocciaStringLiteralValue.Create(NF.FSignDisplay));
  Obj.AssignProperty('useGrouping', TGocciaStringLiteralValue.Create(NF.FUseGrouping));
  Obj.AssignProperty('roundingMode', TGocciaStringLiteralValue.Create(NF.FRoundingMode));
  Obj.AssignProperty('minimumIntegerDigits',
    TGocciaNumberLiteralValue.Create(NF.FMinimumIntegerDigits));
  if NF.FMinimumFractionDigits >= 0 then
    Obj.AssignProperty('minimumFractionDigits',
      TGocciaNumberLiteralValue.Create(NF.FMinimumFractionDigits));
  if NF.FMaximumFractionDigits >= 0 then
    Obj.AssignProperty('maximumFractionDigits',
      TGocciaNumberLiteralValue.Create(NF.FMaximumFractionDigits));
  if NF.FMinimumSignificantDigits >= 0 then
    Obj.AssignProperty('minimumSignificantDigits',
      TGocciaNumberLiteralValue.Create(NF.FMinimumSignificantDigits));
  if NF.FMaximumSignificantDigits >= 0 then
    Obj.AssignProperty('maximumSignificantDigits',
      TGocciaNumberLiteralValue.Create(NF.FMaximumSignificantDigits));
  if NF.FNumberingSystem <> '' then
    Obj.AssignProperty('numberingSystem', TGocciaStringLiteralValue.Create(NF.FNumberingSystem));
  Result := Obj;
end;

initialization
  GIntlNumberFormatSharedSlot := RegisterRealmOwnedSlot('Intl.NumberFormat.shared');

end.
