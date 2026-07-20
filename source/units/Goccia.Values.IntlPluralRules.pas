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
    FNotation: string;
    FCompactDisplay: string;
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

const
  FRENCH_COMPACT_PLURAL_MANY_THRESHOLD = 1000000;

function GetIntlPluralRulesShared: TGocciaSharedPrototype;
{$IFDEF FPC}inline;{$ENDIF}
begin
  if (CurrentRealm <> nil) then
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

function ReadPluralRulesStringOption(const AOptions: TGocciaObjectValue;
  const AName, ADefault: string; const AAllowed: array of string): string;
var
  V: TGocciaValue;
  I: Integer;
begin
  Result := ADefault;
  V := AOptions.GetProperty(AName);
  if not Assigned(V) or (V is TGocciaUndefinedLiteralValue) then
    Exit;

  Result := V.ToStringLiteral.Value;
  for I := Low(AAllowed) to High(AAllowed) do
  begin
    if Result = AAllowed[I] then
      Exit;
  end;

  ThrowRangeError(Format(SErrorIntlInvalidOption, [Result, AName]));
end;

function IsPluralRulesRoundingIncrement(const AValue: Integer): Boolean;
begin
  case AValue of
    1, 2, 5, 10, 20, 25, 50, 100, 200, 250, 500, 1000, 2000, 2500, 5000:
      Result := True;
  else
    Result := False;
  end;
end;

procedure DefinePluralRulesDataProperty(const AObject: TGocciaObjectValue;
  const AName: string; const AValue: TGocciaValue);
begin
  AObject.DefineProperty(AName, TGocciaPropertyDescriptorData.Create(AValue,
    [pfEnumerable, pfConfigurable, pfWritable]));
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
  RoundingIncrement: Integer;
  HasMinFrac, HasMaxFrac, HasMinSig, HasMaxSig: Boolean;
begin
  inherited Create;
  Canonical := CanonicalizeUnicodeLocaleId(ALocale);
  if Canonical = '' then
    FLocale := DefaultLocale
  else
    FLocale := Canonical;

  // Defaults
  FType := 'cardinal';
  FNotation := 'standard';
  FCompactDisplay := '';
  FMinimumIntegerDigits := 1;
  FMinimumFractionDigits := -1;
  FMaximumFractionDigits := -1;
  FMinimumSignificantDigits := -1;
  FMaximumSignificantDigits := -1;
  HasMinFrac := False;
  HasMaxFrac := False;
  HasMinSig := False;
  HasMaxSig := False;

  if Assigned(AOptions) then
  begin
    V := nil;
    ReadPluralRulesStringOption(AOptions, 'localeMatcher', 'best fit',
      ['lookup', 'best fit']);
    FType := ReadPluralRulesStringOption(AOptions, 'type', FType,
      ['cardinal', 'ordinal']);
    FNotation := ReadPluralRulesStringOption(AOptions, 'notation', FNotation,
      ['standard', 'scientific', 'engineering', 'compact']);
    if FNotation = 'compact' then
      FCompactDisplay := ReadPluralRulesStringOption(AOptions,
        'compactDisplay', 'short', ['short', 'long'])
    else
      ReadPluralRulesStringOption(AOptions, 'compactDisplay', 'short',
        ['short', 'long']);
    V := AOptions.GetProperty('minimumIntegerDigits');
    if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
      FMinimumIntegerDigits := ToIntegerWithTruncationValue(V);
    V := AOptions.GetProperty('minimumFractionDigits');
    if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
    begin
      FMinimumFractionDigits := ToIntegerWithTruncationValue(V);
      HasMinFrac := True;
    end;
    V := AOptions.GetProperty('maximumFractionDigits');
    if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
    begin
      FMaximumFractionDigits := ToIntegerWithTruncationValue(V);
      HasMaxFrac := True;
    end;
    V := AOptions.GetProperty('minimumSignificantDigits');
    if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
    begin
      FMinimumSignificantDigits := ToIntegerWithTruncationValue(V);
      HasMinSig := True;
    end;
    V := AOptions.GetProperty('maximumSignificantDigits');
    if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
    begin
      FMaximumSignificantDigits := ToIntegerWithTruncationValue(V);
      HasMaxSig := True;
    end;
  end;

  // ECMA-402 §15.1.6 SetNumberFormatDigitOptions: every explicitly provided
  // value is range-validated (presence tracked separately so negative inputs
  // are rejected rather than mistaken for the unset sentinel), and provided
  // minimum/maximum pairs must be ordered.
  if (FMinimumIntegerDigits < 1) or (FMinimumIntegerDigits > 21) then
    ThrowRangeError(Format(SErrorIntlDigitsOutOfRange, ['minimumIntegerDigits', 1, 21]));
  if HasMinFrac and ((FMinimumFractionDigits < 0) or (FMinimumFractionDigits > 100)) then
    ThrowRangeError(Format(SErrorIntlDigitsOutOfRange, ['minimumFractionDigits', 0, 100]));
  if HasMaxFrac and ((FMaximumFractionDigits < 0) or (FMaximumFractionDigits > 100)) then
    ThrowRangeError(Format(SErrorIntlDigitsOutOfRange, ['maximumFractionDigits', 0, 100]));
  if HasMinSig and
     ((FMinimumSignificantDigits < 1) or (FMinimumSignificantDigits > 21)) then
    ThrowRangeError(Format(SErrorIntlDigitsOutOfRange, ['minimumSignificantDigits', 1, 21]));
    if HasMaxSig and
       ((FMaximumSignificantDigits < 1) or (FMaximumSignificantDigits > 21)) then
    ThrowRangeError(Format(SErrorIntlDigitsOutOfRange, ['maximumSignificantDigits', 1, 21]));
  if HasMinFrac and HasMaxFrac and
     (FMinimumFractionDigits > FMaximumFractionDigits) then
    ThrowRangeError(Format(SErrorIntlDigitsOutOfRange, ['maximumFractionDigits', FMinimumFractionDigits, 100]));
  if HasMinSig and HasMaxSig and
     (FMinimumSignificantDigits > FMaximumSignificantDigits) then
    ThrowRangeError(Format(SErrorIntlDigitsOutOfRange, ['maximumSignificantDigits', FMinimumSignificantDigits, 21]));

  if HasMinSig or HasMaxSig then
  begin
    if not HasMinSig then
      FMinimumSignificantDigits := 1;
    if not HasMaxSig then
      FMaximumSignificantDigits := 21;
    FMinimumFractionDigits := -1;
    FMaximumFractionDigits := -1;
  end
  else
  begin
    if not HasMinFrac then
      FMinimumFractionDigits := 0;
    if not HasMaxFrac then
      FMaximumFractionDigits := Max(3, FMinimumFractionDigits);
  end;

  if Assigned(AOptions) then
  begin
    V := AOptions.GetProperty('roundingIncrement');
    if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
    begin
      RoundingIncrement := ToIntegerWithTruncationValue(V);
      if not IsPluralRulesRoundingIncrement(RoundingIncrement) then
        ThrowRangeError(Format(SErrorIntlInvalidOption,
          [IntToStr(RoundingIncrement), 'roundingIncrement']));
    end;
    ReadPluralRulesStringOption(AOptions, 'roundingMode', 'halfExpand',
      ['ceil', 'floor', 'expand', 'trunc', 'halfCeil', 'halfFloor',
       'halfExpand', 'halfTrunc', 'halfEven']);
    ReadPluralRulesStringOption(AOptions, 'roundingPriority', 'auto',
      ['auto', 'morePrecision', 'lessPrecision']);
    ReadPluralRulesStringOption(AOptions, 'trailingZeroDisplay', 'auto',
      ['auto', 'stripIfInteger']);
  end;

  InitializePrototype;
  if (GetIntlPluralRulesShared <> nil) then
    FPrototype := GetIntlPluralRulesShared.Prototype;
end;

function TGocciaIntlPluralRulesValue.ToStringTag: string;
begin
  Result := 'Object';
end;

procedure TGocciaIntlPluralRulesValue.InitializePrototype;
var
  Members: TGocciaMemberCollection;
  Shared: TGocciaSharedPrototype;
  PrototypeMembers: TArray<TGocciaMemberDefinition>;
begin
  if (CurrentRealm = nil) then Exit;
  if (GetIntlPluralRulesShared <> nil) then Exit;

  Shared := TGocciaSharedPrototype.Create(Self);
  CurrentRealm.SetOwnedSlot(GIntlPluralRulesSharedSlot, Shared);
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
    PrototypeMembers := Members.ToDefinitions;
  finally
    Members.Free;
  end;
  RegisterMemberDefinitions(Shared.Prototype, PrototypeMembers);
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

  if (PR.FNotation = 'compact') and (Copy(PR.FLocale, 1, 2) = 'fr') and
     (Abs(NumValue) >= FRENCH_COMPACT_PLURAL_MANY_THRESHOLD) then
  begin
    Result := TGocciaStringLiteralValue.Create('many');
    Exit;
  end;

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
  if (AArgs.GetElement(0) is TGocciaUndefinedLiteralValue) or
     (AArgs.GetElement(1) is TGocciaUndefinedLiteralValue) then
    ThrowTypeError('Intl.PluralRules.prototype.selectRange requires defined start and end values');
  StartVal := AArgs.GetElement(0).ToNumberLiteral.Value;
  EndVal := AArgs.GetElement(1).ToNumberLiteral.Value;
  if IsNan(StartVal) or IsNan(EndVal) then
    ThrowRangeError('Intl.PluralRules.prototype.selectRange requires non-NaN values');

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
  DefinePluralRulesDataProperty(Obj, 'locale',
    TGocciaStringLiteralValue.Create(PR.FLocale));
  DefinePluralRulesDataProperty(Obj, 'type',
    TGocciaStringLiteralValue.Create(PR.FType));
  DefinePluralRulesDataProperty(Obj, 'notation',
    TGocciaStringLiteralValue.Create(PR.FNotation));
  if PR.FCompactDisplay <> '' then
    DefinePluralRulesDataProperty(Obj, 'compactDisplay',
      TGocciaStringLiteralValue.Create(PR.FCompactDisplay));
  DefinePluralRulesDataProperty(Obj, 'minimumIntegerDigits',
    TGocciaNumberLiteralValue.Create(PR.FMinimumIntegerDigits));
  if PR.FMinimumFractionDigits >= 0 then
    DefinePluralRulesDataProperty(Obj, 'minimumFractionDigits',
      TGocciaNumberLiteralValue.Create(PR.FMinimumFractionDigits));
  if PR.FMaximumFractionDigits >= 0 then
    DefinePluralRulesDataProperty(Obj, 'maximumFractionDigits',
      TGocciaNumberLiteralValue.Create(PR.FMaximumFractionDigits));
  if PR.FMinimumSignificantDigits >= 0 then
    DefinePluralRulesDataProperty(Obj, 'minimumSignificantDigits',
      TGocciaNumberLiteralValue.Create(PR.FMinimumSignificantDigits));
  if PR.FMaximumSignificantDigits >= 0 then
    DefinePluralRulesDataProperty(Obj, 'maximumSignificantDigits',
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
  DefinePluralRulesDataProperty(Obj, 'pluralCategories', CatArr);

  Result := Obj;
end;

initialization
  GIntlPluralRulesSharedSlot := RegisterRealmOwnedSlot('Intl.PluralRules.shared');

end.
