unit Goccia.Values.IntlRelativeTimeFormat;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.ObjectModel,
  Goccia.SharedPrototype,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaIntlRelativeTimeFormatValue = class(TGocciaObjectValue)
  private
    FLocale: string;
    FNumeric: string;
    FStyle: string;
    FNumberingSystem: string;

    procedure InitializePrototype;
  public
    constructor Create(const ALocale: string; const AOptions: TGocciaObjectValue = nil);

    function ToStringTag: string; override;
    class procedure ExposePrototype(const AConstructor: TGocciaObjectValue);
  published
    function IntlRelativeTimeFormatFormat(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function IntlRelativeTimeFormatFormatToParts(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function IntlRelativeTimeFormatResolvedOptions(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  end;

implementation

uses
  StrUtils,
  SysUtils,

  IntlICU,
  IntlLocaleResolver,
  IntlTypes,

  Goccia.Error.Messages,
  Goccia.Intl.CLDRData,
  Goccia.Intl.Helpers,
  Goccia.ObjectModel.Types,
  Goccia.Realm,
  Goccia.Values.ArrayValue,
  Goccia.Values.ErrorHelper,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.SymbolValue;

var
  GIntlRelativeTimeFormatSharedSlot: TGocciaRealmOwnedSlotId;

threadvar
  FPrototypeMembers: TArray<TGocciaMemberDefinition>;

function GetIntlRelativeTimeFormatShared: TGocciaSharedPrototype; inline;
begin
  if Assigned(CurrentRealm) then
    Result := TGocciaSharedPrototype(CurrentRealm.GetOwnedSlot(GIntlRelativeTimeFormatSharedSlot))
  else
    Result := nil;
end;

function AsRelativeTimeFormat(const AValue: TGocciaValue; const AMethod: string): TGocciaIntlRelativeTimeFormatValue;
begin
  if not (AValue is TGocciaIntlRelativeTimeFormatValue) then
    ThrowTypeError(AMethod + ' called on non-RelativeTimeFormat');
  Result := TGocciaIntlRelativeTimeFormatValue(AValue);
end;

function UnitStringToEnum(const AValue: string): TIntlRelativeTimeUnit;
begin
  if (AValue = 'year') or (AValue = 'years') then
    Result := irtuYear
  else if (AValue = 'quarter') or (AValue = 'quarters') then
    Result := irtuQuarter
  else if (AValue = 'month') or (AValue = 'months') then
    Result := irtuMonth
  else if (AValue = 'week') or (AValue = 'weeks') then
    Result := irtuWeek
  else if (AValue = 'day') or (AValue = 'days') then
    Result := irtuDay
  else if (AValue = 'hour') or (AValue = 'hours') then
    Result := irtuHour
  else if (AValue = 'minute') or (AValue = 'minutes') then
    Result := irtuMinute
  else if (AValue = 'second') or (AValue = 'seconds') then
    Result := irtuSecond
  else
  begin
    ThrowRangeError('Invalid unit for Intl.RelativeTimeFormat: ' + AValue);
    Result := irtuSecond;
  end;
end;

function NumericStringToEnum(const AValue: string): TIntlRelativeTimeNumeric;
begin
  if AValue = 'auto' then
    Result := irtnAuto
  else
    Result := irtnAlways;
end;

function TryFormatRelativeTimeAutoName(AValue: Double; const ALocale: string;
  AUnit: TIntlRelativeTimeUnit; out AFormatted: string): Boolean;
var
  Offset: Integer;
begin
  Result := False;
  AFormatted := '';
  if (Copy(ALocale, 1, 2) <> 'en') or (Frac(AValue) <> 0) then
    Exit;

  Offset := Trunc(AValue);
  if (Offset < -1) or (Offset > 1) then
    Exit;

  case AUnit of
    irtuSecond:
      if Offset = 0 then AFormatted := 'now';
    irtuMinute:
      if Offset = 0 then AFormatted := 'this minute';
    irtuHour:
      if Offset = 0 then AFormatted := 'this hour';
    irtuDay:
      case Offset of
        -1: AFormatted := 'yesterday';
        0: AFormatted := 'today';
        1: AFormatted := 'tomorrow';
      end;
    irtuWeek:
      case Offset of
        -1: AFormatted := 'last week';
        0: AFormatted := 'this week';
        1: AFormatted := 'next week';
      end;
    irtuMonth:
      case Offset of
        -1: AFormatted := 'last month';
        0: AFormatted := 'this month';
        1: AFormatted := 'next month';
      end;
    irtuQuarter:
      case Offset of
        -1: AFormatted := 'last quarter';
        0: AFormatted := 'this quarter';
        1: AFormatted := 'next quarter';
      end;
    irtuYear:
      case Offset of
        -1: AFormatted := 'last year';
        0: AFormatted := 'this year';
        1: AFormatted := 'next year';
      end;
  end;

  Result := AFormatted <> '';
end;

function FormatRelativeTimeWithCLDR(AValue: Double; const ALocale: string;
  AUnit: TIntlRelativeTimeUnit): string;
var
  Pattern: TIntlRelativeTimePattern;
  Template, ValueStr: string;
  AbsValue: Double;
begin
  AbsValue := Abs(AValue);
  if Frac(AbsValue) = 0 then
    ValueStr := IntToStr(Trunc(AbsValue))
  else
    ValueStr := FloatToStr(AbsValue);

  if not TryGetRelativeTimePattern(ALocale, AUnit, Pattern) then
  begin
    Result := FloatToStr(AValue);
    Exit;
  end;

  if AValue < 0 then
    Template := Pattern.Past
  else
    Template := Pattern.Future;

  if Template = '' then
  begin
    if AValue < 0 then
      Template := Pattern.Future
    else
      Template := Pattern.Past;
  end;

  if Template <> '' then
    Result := StringReplace(Template, '{0}', ValueStr, [])
  else
    Result := FloatToStr(AValue);
end;

function NormalizeRelativeTimeUnit(const AUnit: string): string;
begin
  if (AUnit = 'years') then Result := 'year'
  else if (AUnit = 'quarters') then Result := 'quarter'
  else if (AUnit = 'months') then Result := 'month'
  else if (AUnit = 'weeks') then Result := 'week'
  else if (AUnit = 'days') then Result := 'day'
  else if (AUnit = 'hours') then Result := 'hour'
  else if (AUnit = 'minutes') then Result := 'minute'
  else if (AUnit = 'seconds') then Result := 'second'
  else
    Result := AUnit;
end;

function FormatPartsString(const AParts: TIntlFormatPartArray): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to Length(AParts) - 1 do
    Result := Result + AParts[I].Value;
end;

function FormatRelativeTimePartsFallback(const AFormatted: string;
  const ALocale: string; AValue: Double; const AUnit: string): TIntlFormatPartArray;
var
  NumberOptions: TIntlNumberFormatOptions;
  NumberParts: TIntlFormatPartArray;
  NumberText: string;
  NumberStart, I, Index: Integer;

  procedure AppendPart(const AType, AValue, AUnitIdentifier: string);
  begin
    if AValue = '' then Exit;
    SetLength(Result, Length(Result) + 1);
    Index := Length(Result) - 1;
    Result[Index].PartType := AType;
    Result[Index].Value := AValue;
    Result[Index].Source := '';
    Result[Index].UnitIdentifier := AUnitIdentifier;
  end;

begin
  SetLength(Result, 0);
  NumberOptions := DefaultNumberFormatOptions;
  if not TryICUFormatNumberToParts(ALocale, Abs(AValue), NumberOptions, NumberParts) then
  begin
    AppendPart('literal', AFormatted, '');
    Exit;
  end;

  NumberText := FormatPartsString(NumberParts);
  NumberStart := PosEx(NumberText, AFormatted, 1);
  if NumberStart = 0 then
  begin
    NumberOptions.UseGrouping := inugFalse;
    if TryICUFormatNumberToParts(ALocale, Abs(AValue), NumberOptions, NumberParts) then
    begin
      NumberText := FormatPartsString(NumberParts);
      NumberStart := PosEx(NumberText, AFormatted, 1);
    end;
  end;
  if NumberStart = 0 then
  begin
    AppendPart('literal', AFormatted, '');
    Exit;
  end;

  AppendPart('literal', Copy(AFormatted, 1, NumberStart - 1), '');
  for I := 0 to Length(NumberParts) - 1 do
    AppendPart(NumberParts[I].PartType, NumberParts[I].Value,
      NormalizeRelativeTimeUnit(AUnit));
  AppendPart('literal', Copy(AFormatted, NumberStart + Length(NumberText), MaxInt), '');
end;

{ TGocciaIntlRelativeTimeFormatValue }

constructor TGocciaIntlRelativeTimeFormatValue.Create(const ALocale: string; const AOptions: TGocciaObjectValue);
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
  FNumeric := 'always';
  FStyle := 'long';

  if Assigned(AOptions) then
  begin
    V := AOptions.GetProperty('localeMatcher');
    if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
    begin
      if ContainsNulCharacter(V.ToStringLiteral.Value) then
        ThrowRangeError(Format(SErrorIntlInvalidOption, [V.ToStringLiteral.Value, 'localeMatcher']));
    end;
    V := AOptions.GetProperty('numeric');
    if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
    begin
      FNumeric := V.ToStringLiteral.Value;
      if ContainsNulCharacter(FNumeric) then
        ThrowRangeError(Format(SErrorIntlInvalidOption, [FNumeric, 'numeric']));
    end;
    V := AOptions.GetProperty('style');
    if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
    begin
      FStyle := V.ToStringLiteral.Value;
      if ContainsNulCharacter(FStyle) then
        ThrowRangeError(Format(SErrorIntlInvalidOption, [FStyle, 'style']));
    end;
    V := AOptions.GetProperty('numberingSystem');
    if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
      FNumberingSystem := V.ToStringLiteral.Value;
  end;

  if FNumberingSystem = '' then
    FNumberingSystem := 'latn';

  InitializePrototype;
  if Assigned(GetIntlRelativeTimeFormatShared) then
    FPrototype := GetIntlRelativeTimeFormatShared.Prototype;
end;

function TGocciaIntlRelativeTimeFormatValue.ToStringTag: string;
begin
  Result := 'Intl.RelativeTimeFormat';
end;

procedure TGocciaIntlRelativeTimeFormatValue.InitializePrototype;
var
  Members: TGocciaMemberCollection;
  Shared: TGocciaSharedPrototype;
begin
  if not Assigned(CurrentRealm) then Exit;
  if Assigned(GetIntlRelativeTimeFormatShared) then Exit;

  Shared := TGocciaSharedPrototype.Create(Self);
  CurrentRealm.SetOwnedSlot(GIntlRelativeTimeFormatSharedSlot, Shared);
  if Length(FPrototypeMembers) = 0 then
  begin
    Members := TGocciaMemberCollection.Create;
    try
      Members.AddNamedMethod('format', IntlRelativeTimeFormatFormat, 2,
        gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddNamedMethod('formatToParts', IntlRelativeTimeFormatFormatToParts, 2,
        gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddNamedMethod('resolvedOptions', IntlRelativeTimeFormatResolvedOptions, 0,
        gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddSymbolDataProperty(
        TGocciaSymbolValue.WellKnownToStringTag,
        TGocciaStringLiteralValue.Create('Intl.RelativeTimeFormat'),
        [pfConfigurable]);
      FPrototypeMembers := Members.ToDefinitions;
    finally
      Members.Free;
    end;
  end;
  RegisterMemberDefinitions(Shared.Prototype, FPrototypeMembers);
end;

class procedure TGocciaIntlRelativeTimeFormatValue.ExposePrototype(const AConstructor: TGocciaObjectValue);
var
  Shared: TGocciaSharedPrototype;
begin
  Shared := GetIntlRelativeTimeFormatShared;
  if not Assigned(Shared) then
  begin
    TGocciaIntlRelativeTimeFormatValue.Create(DefaultLocale);
    Shared := GetIntlRelativeTimeFormatShared;
  end;
  if Assigned(Shared) then
    ExposeSharedPrototypeOnConstructor(Shared, AConstructor);
end;

function TGocciaIntlRelativeTimeFormatValue.IntlRelativeTimeFormatFormat(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  RTF: TGocciaIntlRelativeTimeFormatValue;
  NumValue: Double;
  UnitStr: string;
  Formatted: string;
begin
  RTF := AsRelativeTimeFormat(AThisValue, 'Intl.RelativeTimeFormat.prototype.format');
  if AArgs.Length < 2 then
    ThrowTypeError('Intl.RelativeTimeFormat.prototype.format requires value and unit');
  NumValue := AArgs.GetElement(0).ToNumberLiteral.Value;
  UnitStr := AArgs.GetElement(1).ToStringLiteral.Value;
  if ContainsNulCharacter(UnitStr) then
    ThrowRangeError('Invalid unit for Intl.RelativeTimeFormat: ' + UnitStr);

  if TryICUFormatRelativeTime(RTF.FLocale, NumValue, UnitStringToEnum(UnitStr),
    NumericStringToEnum(RTF.FNumeric), Formatted) then
    Result := TGocciaStringLiteralValue.Create(Formatted)
  else if (RTF.FNumeric = 'auto') and
          TryFormatRelativeTimeAutoName(NumValue, RTF.FLocale,
            UnitStringToEnum(UnitStr), Formatted) then
    Result := TGocciaStringLiteralValue.Create(Formatted)
  else
    Result := TGocciaStringLiteralValue.Create(
      FormatRelativeTimeWithCLDR(NumValue, RTF.FLocale, UnitStringToEnum(UnitStr)));
end;

function TGocciaIntlRelativeTimeFormatValue.IntlRelativeTimeFormatFormatToParts(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  RTF: TGocciaIntlRelativeTimeFormatValue;
  NumValue: Double;
  UnitStr, Formatted: string;
  Parts: TIntlFormatPartArray;
begin
  RTF := AsRelativeTimeFormat(AThisValue, 'Intl.RelativeTimeFormat.prototype.formatToParts');
  if AArgs.Length < 2 then
    ThrowTypeError('Intl.RelativeTimeFormat.prototype.formatToParts requires value and unit');
  NumValue := AArgs.GetElement(0).ToNumberLiteral.Value;
  UnitStr := AArgs.GetElement(1).ToStringLiteral.Value;
  if ContainsNulCharacter(UnitStr) then
    ThrowRangeError('Invalid unit for Intl.RelativeTimeFormat: ' + UnitStr);

  if TryICUFormatRelativeTimeToParts(RTF.FLocale, NumValue, UnitStringToEnum(UnitStr),
    NumericStringToEnum(RTF.FNumeric), Parts) then
    Exit(FormatPartsToArray(Parts));

  if not TryICUFormatRelativeTime(RTF.FLocale, NumValue, UnitStringToEnum(UnitStr),
    NumericStringToEnum(RTF.FNumeric), Formatted) then
  begin
    if (RTF.FNumeric <> 'auto') or
       not TryFormatRelativeTimeAutoName(NumValue, RTF.FLocale,
         UnitStringToEnum(UnitStr), Formatted) then
      Formatted := FormatRelativeTimeWithCLDR(NumValue, RTF.FLocale,
        UnitStringToEnum(UnitStr));
  end;

  Result := FormatPartsToArray(FormatRelativeTimePartsFallback(Formatted,
    RTF.FLocale, NumValue, UnitStr));
end;

function TGocciaIntlRelativeTimeFormatValue.IntlRelativeTimeFormatResolvedOptions(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  RTF: TGocciaIntlRelativeTimeFormatValue;
  Obj: TGocciaObjectValue;
begin
  RTF := AsRelativeTimeFormat(AThisValue, 'Intl.RelativeTimeFormat.prototype.resolvedOptions');
  Obj := TGocciaObjectValue.Create(TGocciaObjectValue.SharedObjectPrototype);
  Obj.AssignProperty('locale', TGocciaStringLiteralValue.Create(RTF.FLocale));
  Obj.AssignProperty('numberingSystem', TGocciaStringLiteralValue.Create(RTF.FNumberingSystem));
  Obj.AssignProperty('style', TGocciaStringLiteralValue.Create(RTF.FStyle));
  Obj.AssignProperty('numeric', TGocciaStringLiteralValue.Create(RTF.FNumeric));
  Result := Obj;
end;

initialization
  GIntlRelativeTimeFormatSharedSlot := RegisterRealmOwnedSlot('Intl.RelativeTimeFormat.shared');

end.
