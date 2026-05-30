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
  Math,
  StrUtils,
  SysUtils,

  BCP47,
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

function StyleStringToEnum(const AValue: string): TIntlRelativeTimeStyle;
begin
  if AValue = 'short' then
    Result := irtsShort
  else if AValue = 'narrow' then
    Result := irtsNarrow
  else
    Result := irtsLong;
end;

procedure ValidateRelativeTimeOption(const AValue, AName: string;
  const AAllowed: array of string);
var
  I: Integer;
begin
  if ContainsNulCharacter(AValue) then
    ThrowRangeError(Format(SErrorIntlInvalidOption, [AValue, AName]));

  for I := Low(AAllowed) to High(AAllowed) do
    if AValue = AAllowed[I] then
      Exit;

  ThrowRangeError(Format(SErrorIntlInvalidOption, [AValue, AName]));
end;

function IsValidUnicodeLocaleType(const AValue: string): Boolean;
var
  I, SubtagLength: Integer;
  Ch: Char;
begin
  Result := AValue <> '';
  if not Result then
    Exit;

  SubtagLength := 0;
  for I := 1 to Length(AValue) do
  begin
    Ch := AValue[I];
    if Ch = '-' then
    begin
      if (SubtagLength < 3) or (SubtagLength > 8) then
        Exit(False);
      SubtagLength := 0;
      Continue;
    end;

    if not (((Ch >= 'a') and (Ch <= 'z')) or
            ((Ch >= 'A') and (Ch <= 'Z')) or
            ((Ch >= '0') and (Ch <= '9'))) then
      Exit(False);

    Inc(SubtagLength);
  end;

  Result := (SubtagLength >= 3) and (SubtagLength <= 8);
end;

function TryGetLocaleNumberingSystemExtension(const ALocale: string;
  out ANumberingSystem: string): Boolean;
begin
  Result := TryGetUnicodeLocaleExtensionKeyword(ALocale, 'nu', ANumberingSystem);
end;

function IsLocaleLanguage(const ALocale, ALanguage: string): Boolean;
var
  Parsed: TBcp47Tag;
  FirstHyphen: Integer;
  PrimaryLanguage: string;
begin
  Parsed := ParseBcp47Tag(ALocale);
  if Parsed.IsValid then
    Exit(SameText(Parsed.Language, ALanguage));

  FirstHyphen := Pos('-', ALocale);
  if FirstHyphen = 0 then
    PrimaryLanguage := ALocale
  else
    PrimaryLanguage := Copy(ALocale, 1, FirstHyphen - 1);
  Result := SameText(PrimaryLanguage, ALanguage);
end;

function IsEnglishLocale(const ALocale: string): Boolean;
begin
  Result := IsLocaleLanguage(ALocale, 'en');
end;

function IsPolishLocale(const ALocale: string): Boolean;
begin
  Result := IsLocaleLanguage(ALocale, 'pl');
end;

function RelativeTimeNumberOptions(const ANumberingSystem: string): TIntlNumberFormatOptions;
begin
  Result := DefaultNumberFormatOptions;
  Result.NumberingSystem := ANumberingSystem;
end;

function RelativeTimeUnitToPartUnit(AUnit: TIntlRelativeTimeUnit): string;
begin
  case AUnit of
    irtuYear: Result := 'year';
    irtuQuarter: Result := 'quarter';
    irtuMonth: Result := 'month';
    irtuWeek: Result := 'week';
    irtuDay: Result := 'day';
    irtuHour: Result := 'hour';
    irtuMinute: Result := 'minute';
    irtuSecond: Result := 'second';
  else
    Result := 'second';
  end;
end;

function EnglishRelativeUnitDisplay(AUnit: TIntlRelativeTimeUnit;
  AStyle: TIntlRelativeTimeStyle; const APluralCategory: string): string;
var
  IsSingular: Boolean;
begin
  IsSingular := APluralCategory = 'one';
  case AStyle of
    irtsShort:
      case AUnit of
        irtuSecond: Result := 'sec.';
        irtuMinute: Result := 'min.';
        irtuHour: Result := 'hr.';
        irtuDay:
          if IsSingular then Result := 'day' else Result := 'days';
        irtuWeek: Result := 'wk.';
        irtuMonth: Result := 'mo.';
        irtuQuarter:
          if IsSingular then Result := 'qtr.' else Result := 'qtrs.';
        irtuYear: Result := 'yr.';
      end;
    irtsNarrow:
      case AUnit of
        irtuSecond: Result := 's';
        irtuMinute: Result := 'm';
        irtuHour: Result := 'h';
        irtuDay: Result := 'd';
        irtuWeek: Result := 'w';
        irtuMonth: Result := 'mo';
        irtuQuarter: Result := 'q';
        irtuYear: Result := 'y';
      end;
  else
    case AUnit of
      irtuSecond:
        if IsSingular then Result := 'second' else Result := 'seconds';
      irtuMinute:
        if IsSingular then Result := 'minute' else Result := 'minutes';
      irtuHour:
        if IsSingular then Result := 'hour' else Result := 'hours';
      irtuDay:
        if IsSingular then Result := 'day' else Result := 'days';
      irtuWeek:
        if IsSingular then Result := 'week' else Result := 'weeks';
      irtuMonth:
        if IsSingular then Result := 'month' else Result := 'months';
      irtuQuarter:
        if IsSingular then Result := 'quarter' else Result := 'quarters';
      irtuYear:
        if IsSingular then Result := 'year' else Result := 'years';
    end;
  end;
end;

function PolishRelativeUnitDisplay(AUnit: TIntlRelativeTimeUnit;
  AStyle: TIntlRelativeTimeStyle; const APluralCategory: string): string;
begin
  case AStyle of
    irtsShort:
      case AUnit of
        irtuSecond: Result := 'sek.';
        irtuMinute: Result := 'min';
        irtuHour: Result := 'godz.';
        irtuDay:
          if APluralCategory = 'one' then Result := 'dzień'
          else if APluralCategory = 'other' then Result := 'dnia'
          else Result := 'dni';
        irtuWeek:
          if APluralCategory = 'one' then Result := 'tydz.' else Result := 'tyg.';
        irtuMonth: Result := 'mies.';
        irtuQuarter: Result := 'kw.';
        irtuYear:
          if APluralCategory = 'one' then Result := 'rok'
          else if APluralCategory = 'few' then Result := 'lata'
          else if APluralCategory = 'other' then Result := 'roku'
          else Result := 'lat';
      end;
    irtsNarrow:
      case AUnit of
        irtuSecond: Result := 's';
        irtuMinute: Result := 'min';
        irtuHour: Result := 'g.';
        irtuDay:
          if APluralCategory = 'one' then Result := 'dzień'
          else if APluralCategory = 'other' then Result := 'dnia'
          else Result := 'dni';
        irtuWeek:
          if APluralCategory = 'one' then Result := 'tydz.' else Result := 'tyg.';
        irtuMonth: Result := 'mies.';
        irtuQuarter: Result := 'kw.';
        irtuYear:
          if APluralCategory = 'one' then Result := 'rok'
          else if APluralCategory = 'few' then Result := 'lata'
          else if APluralCategory = 'other' then Result := 'roku'
          else Result := 'lat';
      end;
  else
    case AUnit of
      irtuSecond:
        if APluralCategory = 'one' then Result := 'sekundę'
        else if (APluralCategory = 'few') or (APluralCategory = 'other') then Result := 'sekundy'
        else Result := 'sekund';
      irtuMinute:
        if APluralCategory = 'one' then Result := 'minutę'
        else if (APluralCategory = 'few') or (APluralCategory = 'other') then Result := 'minuty'
        else Result := 'minut';
      irtuHour:
        if APluralCategory = 'one' then Result := 'godzinę'
        else if (APluralCategory = 'few') or (APluralCategory = 'other') then Result := 'godziny'
        else Result := 'godzin';
      irtuDay:
        if APluralCategory = 'one' then Result := 'dzień'
        else if APluralCategory = 'other' then Result := 'dnia'
        else Result := 'dni';
      irtuWeek:
        if APluralCategory = 'one' then Result := 'tydzień'
        else if APluralCategory = 'few' then Result := 'tygodnie'
        else if APluralCategory = 'other' then Result := 'tygodnia'
        else Result := 'tygodni';
      irtuMonth:
        if APluralCategory = 'one' then Result := 'miesiąc'
        else if APluralCategory = 'few' then Result := 'miesiące'
        else if APluralCategory = 'other' then Result := 'miesiąca'
        else Result := 'miesięcy';
      irtuQuarter:
        if APluralCategory = 'one' then Result := 'kwartał'
        else if APluralCategory = 'few' then Result := 'kwartały'
        else if APluralCategory = 'other' then Result := 'kwartału'
        else Result := 'kwartałów';
      irtuYear:
        if APluralCategory = 'one' then Result := 'rok'
        else if APluralCategory = 'few' then Result := 'lata'
        else if APluralCategory = 'other' then Result := 'roku'
        else Result := 'lat';
    end;
  end;
end;

function TryGetRelativeUnitDisplay(const ALocale: string; AUnit: TIntlRelativeTimeUnit;
  AStyle: TIntlRelativeTimeStyle; const APluralCategory: string;
  out AUnitDisplay: string): Boolean;
begin
  Result := True;
  if IsEnglishLocale(ALocale) then
    AUnitDisplay := EnglishRelativeUnitDisplay(AUnit, AStyle, APluralCategory)
  else if IsPolishLocale(ALocale) then
    AUnitDisplay := PolishRelativeUnitDisplay(AUnit, AStyle, APluralCategory)
  else
  begin
    AUnitDisplay := '';
    Result := False;
  end;
end;

function TryFormatRelativeTimeWithLocalePattern(AValue: Double; AIsPast: Boolean;
  const ALocale, ANumberingSystem: string; AUnit: TIntlRelativeTimeUnit;
  AStyle: TIntlRelativeTimeStyle; out AFormatted: string): Boolean;
var
  NumberOptions: TIntlNumberFormatOptions;
  NumberText, PluralCategory, UnitDisplay: string;
begin
  Result := False;
  AFormatted := '';

  if not TryICUSelectPlural(ALocale, Abs(AValue), iptCardinal, PluralCategory) then
    Exit;
  if not TryGetRelativeUnitDisplay(ALocale, AUnit, AStyle, PluralCategory, UnitDisplay) then
    Exit;

  NumberOptions := RelativeTimeNumberOptions(ANumberingSystem);
  if not TryICUFormatNumber(ALocale, Abs(AValue), NumberOptions, NumberText) then
    Exit;

  if IsPolishLocale(ALocale) then
  begin
    if AIsPast then
      AFormatted := NumberText + ' ' + UnitDisplay + ' temu'
    else
      AFormatted := 'za ' + NumberText + ' ' + UnitDisplay;
  end
  else
  begin
    if AIsPast then
      AFormatted := NumberText + ' ' + UnitDisplay + ' ago'
    else
      AFormatted := 'in ' + NumberText + ' ' + UnitDisplay;
  end;

  Result := True;
end;

function TryFormatRelativeTimePartsWithLocalePattern(AValue: Double;
  AIsPast: Boolean; const ALocale, ANumberingSystem: string;
  AUnit: TIntlRelativeTimeUnit; AStyle: TIntlRelativeTimeStyle;
  out AParts: TIntlFormatPartArray): Boolean;
var
  NumberOptions: TIntlNumberFormatOptions;
  NumberParts: TIntlFormatPartArray;
  PluralCategory, UnitDisplay, UnitIdentifier: string;
  I, Index: Integer;

  procedure AppendPart(const AType, AValue, AUnitIdentifier: string);
  begin
    if AValue = '' then Exit;
    SetLength(AParts, Length(AParts) + 1);
    Index := Length(AParts) - 1;
    AParts[Index].PartType := AType;
    AParts[Index].Value := AValue;
    AParts[Index].Source := '';
    AParts[Index].UnitIdentifier := AUnitIdentifier;
  end;

begin
  Result := False;
  SetLength(AParts, 0);

  if not TryICUSelectPlural(ALocale, Abs(AValue), iptCardinal, PluralCategory) then
    Exit;
  if not TryGetRelativeUnitDisplay(ALocale, AUnit, AStyle, PluralCategory, UnitDisplay) then
    Exit;

  NumberOptions := RelativeTimeNumberOptions(ANumberingSystem);
  if not TryICUFormatNumberToParts(ALocale, Abs(AValue), NumberOptions, NumberParts) then
    Exit;

  UnitIdentifier := RelativeTimeUnitToPartUnit(AUnit);
  if (not AIsPast) then
  begin
    if IsPolishLocale(ALocale) then
      AppendPart('literal', 'za ', '')
    else
      AppendPart('literal', 'in ', '');
  end;

  for I := 0 to Length(NumberParts) - 1 do
    AppendPart(NumberParts[I].PartType, NumberParts[I].Value, UnitIdentifier);

  if AIsPast then
  begin
    if IsPolishLocale(ALocale) then
      AppendPart('literal', ' ' + UnitDisplay + ' temu', '')
    else
      AppendPart('literal', ' ' + UnitDisplay + ' ago', '');
  end
  else
    AppendPart('literal', ' ' + UnitDisplay, '');

  Result := True;
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
  Ignored, NumberingSystemOption, LocaleNumberingSystem: string;
  HasNumberingSystemOption: Boolean;
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
  FNumberingSystem := 'latn';
  NumberingSystemOption := '';
  HasNumberingSystemOption := False;

  if Assigned(AOptions) then
  begin
    V := AOptions.GetProperty('localeMatcher');
    if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
    begin
      Ignored := V.ToStringLiteral.Value;
      ValidateRelativeTimeOption(Ignored, 'localeMatcher', ['lookup', 'best fit']);
    end;
    V := AOptions.GetProperty('numberingSystem');
    if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
    begin
      NumberingSystemOption := V.ToStringLiteral.Value;
      if ContainsNulCharacter(NumberingSystemOption) or
         not IsValidUnicodeLocaleType(NumberingSystemOption) then
        ThrowRangeError(Format(SErrorIntlInvalidOption, [NumberingSystemOption, 'numberingSystem']));
      HasNumberingSystemOption := True;
    end;
    V := AOptions.GetProperty('style');
    if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
    begin
      FStyle := V.ToStringLiteral.Value;
      ValidateRelativeTimeOption(FStyle, 'style', ['long', 'short', 'narrow']);
    end;
    V := AOptions.GetProperty('numeric');
    if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
    begin
      FNumeric := V.ToStringLiteral.Value;
      ValidateRelativeTimeOption(FNumeric, 'numeric', ['always', 'auto']);
    end;
  end;

  if HasNumberingSystemOption and IsSupportedNumberingSystem(NumberingSystemOption) then
  begin
    FNumberingSystem := NumberingSystemOption;
    if not (TryGetLocaleNumberingSystemExtension(FLocale, LocaleNumberingSystem) and
            (LocaleNumberingSystem = NumberingSystemOption)) then
      FLocale := LocaleWithoutUnicodeExtension(FLocale);
  end
  else if TryGetLocaleNumberingSystemExtension(FLocale, LocaleNumberingSystem) then
  begin
    if IsSupportedNumberingSystem(LocaleNumberingSystem) then
      FNumberingSystem := LocaleNumberingSystem
    else
      FLocale := LocaleWithoutUnicodeExtension(FLocale);
  end;

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
  NumberArg: TGocciaNumberLiteralValue;
  NumValue: Double;
  UnitStr: string;
  UnitValue: TIntlRelativeTimeUnit;
  NumericValue: TIntlRelativeTimeNumeric;
  StyleValue: TIntlRelativeTimeStyle;
  IsPast: Boolean;
  Formatted: string;
begin
  RTF := AsRelativeTimeFormat(AThisValue, 'Intl.RelativeTimeFormat.prototype.format');
  if AArgs.Length < 2 then
    ThrowTypeError('Intl.RelativeTimeFormat.prototype.format requires value and unit');
  NumberArg := AArgs.GetElement(0).ToNumberLiteral;
  NumValue := NumberArg.Value;
  if IsNan(NumValue) or IsInfinite(NumValue) then
    ThrowRangeError('Invalid value for Intl.RelativeTimeFormat.prototype.format');
  UnitStr := AArgs.GetElement(1).ToStringLiteral.Value;
  if ContainsNulCharacter(UnitStr) then
    ThrowRangeError('Invalid unit for Intl.RelativeTimeFormat: ' + UnitStr);

  UnitValue := UnitStringToEnum(UnitStr);
  NumericValue := NumericStringToEnum(RTF.FNumeric);
  StyleValue := StyleStringToEnum(RTF.FStyle);
  IsPast := (NumValue < 0) or NumberArg.IsNegativeZero;

  if (NumericValue = irtnAuto) and
     TryFormatRelativeTimeAutoName(NumValue, RTF.FLocale, UnitValue, Formatted) then
    Result := TGocciaStringLiteralValue.Create(Formatted)
  else if TryFormatRelativeTimeWithLocalePattern(NumValue, IsPast, RTF.FLocale,
    RTF.FNumberingSystem, UnitValue, StyleValue, Formatted) then
    Result := TGocciaStringLiteralValue.Create(Formatted)
  else if TryICUFormatRelativeTime(RTF.FLocale, NumValue, UnitValue,
    NumericValue, StyleValue, Formatted) then
    Result := TGocciaStringLiteralValue.Create(Formatted)
  else
    Result := TGocciaStringLiteralValue.Create(
      FormatRelativeTimeWithCLDR(NumValue, RTF.FLocale, UnitValue));
end;

function TGocciaIntlRelativeTimeFormatValue.IntlRelativeTimeFormatFormatToParts(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  RTF: TGocciaIntlRelativeTimeFormatValue;
  NumberArg: TGocciaNumberLiteralValue;
  NumValue: Double;
  UnitStr, Formatted: string;
  UnitValue: TIntlRelativeTimeUnit;
  NumericValue: TIntlRelativeTimeNumeric;
  StyleValue: TIntlRelativeTimeStyle;
  IsPast: Boolean;
  Parts: TIntlFormatPartArray;
begin
  RTF := AsRelativeTimeFormat(AThisValue, 'Intl.RelativeTimeFormat.prototype.formatToParts');
  if AArgs.Length < 2 then
    ThrowTypeError('Intl.RelativeTimeFormat.prototype.formatToParts requires value and unit');
  NumberArg := AArgs.GetElement(0).ToNumberLiteral;
  NumValue := NumberArg.Value;
  if IsNan(NumValue) or IsInfinite(NumValue) then
    ThrowRangeError('Invalid value for Intl.RelativeTimeFormat.prototype.formatToParts');
  UnitStr := AArgs.GetElement(1).ToStringLiteral.Value;
  if ContainsNulCharacter(UnitStr) then
    ThrowRangeError('Invalid unit for Intl.RelativeTimeFormat: ' + UnitStr);

  UnitValue := UnitStringToEnum(UnitStr);
  NumericValue := NumericStringToEnum(RTF.FNumeric);
  StyleValue := StyleStringToEnum(RTF.FStyle);
  IsPast := (NumValue < 0) or NumberArg.IsNegativeZero;

  if (NumericValue = irtnAuto) and
     TryFormatRelativeTimeAutoName(NumValue, RTF.FLocale, UnitValue, Formatted) then
  begin
    SetLength(Parts, 1);
    Parts[0].PartType := 'literal';
    Parts[0].Value := Formatted;
    Parts[0].Source := '';
    Parts[0].UnitIdentifier := '';
    Exit(FormatPartsToArray(Parts));
  end;

  if TryFormatRelativeTimePartsWithLocalePattern(NumValue, IsPast, RTF.FLocale,
    RTF.FNumberingSystem, UnitValue, StyleValue, Parts) then
    Exit(FormatPartsToArray(Parts));

  if TryICUFormatRelativeTimeToParts(RTF.FLocale, NumValue, UnitValue,
    NumericValue, StyleValue, Parts) then
    Exit(FormatPartsToArray(Parts));

  if not TryICUFormatRelativeTime(RTF.FLocale, NumValue, UnitValue,
    NumericValue, StyleValue, Formatted) then
  begin
    if (NumericValue <> irtnAuto) or
       not TryFormatRelativeTimeAutoName(NumValue, RTF.FLocale, UnitValue, Formatted) then
      Formatted := FormatRelativeTimeWithCLDR(NumValue, RTF.FLocale, UnitValue);
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
  Obj.AssignProperty('style', TGocciaStringLiteralValue.Create(RTF.FStyle));
  Obj.AssignProperty('numeric', TGocciaStringLiteralValue.Create(RTF.FNumeric));
  Obj.AssignProperty('numberingSystem', TGocciaStringLiteralValue.Create(RTF.FNumberingSystem));
  Result := Obj;
end;

initialization
  GIntlRelativeTimeFormatSharedSlot := RegisterRealmOwnedSlot('Intl.RelativeTimeFormat.shared');

end.
