unit Goccia.Values.IntlDateTimeFormat;

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
  TGocciaIntlDateTimeFormatValue = class(TGocciaObjectValue)
  private
    FLocale: string;
    FDateStyle: string;
    FTimeStyle: string;
    FCalendar: string;
    FNumberingSystem: string;
    FTimeZone: string;
    FHour12: Integer;
    FHourCycle: string;
    FWeekday: string;
    FEra: string;
    FYear: string;
    FMonth: string;
    FDay: string;
    FDayPeriod: string;
    FHour: string;
    FMinute: string;
    FSecond: string;
    FFractionalSecondDigits: Integer;
    FTimeZoneName: string;
    FResolvedOptions: TIntlDateTimeFormatOptions;
    FHasExplicitCoreOptions: Boolean;
    FBoundFormat: TGocciaValue;

    procedure InitializePrototype;
    procedure ReadOptions(const AOptions: TGocciaObjectValue);
  public
    constructor Create(const ALocale: string; const AOptions: TGocciaObjectValue = nil);

    function ToStringTag: string; override;
    procedure MarkReferences; override;
    class procedure ExposePrototype(const AConstructor: TGocciaObjectValue);
  published
    function IntlDateTimeFormatFormatGetter(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function IntlDateTimeFormatFormat(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function IntlDateTimeFormatFormatToParts(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function IntlDateTimeFormatFormatRange(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function IntlDateTimeFormatFormatRangeToParts(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function IntlDateTimeFormatResolvedOptions(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  end;

function FormatTemporalValueToLocaleString(const AValue, ALocales,
  AOptions: TGocciaValue): TGocciaValue;

implementation

uses
  Math,
  SysUtils,

  IntlICU,
  IntlLocaleResolver,
  TimingUtils,

  Goccia.Builtins.Intl,
  Goccia.Error.Messages,
  Goccia.Intl.Helpers,
  Goccia.ObjectModel.Types,
  Goccia.Realm,
  Goccia.Temporal.Calendar,
  Goccia.Temporal.TimeZone,
  Goccia.Temporal.Utils,
  Goccia.Utils,
  Goccia.Values.ArrayValue,
  Goccia.Values.ErrorHelper,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.SymbolValue,
  Goccia.Values.TemporalInstant,
  Goccia.Values.TemporalPlainDate,
  Goccia.Values.TemporalPlainDateTime,
  Goccia.Values.TemporalPlainMonthDay,
  Goccia.Values.TemporalPlainTime,
  Goccia.Values.TemporalPlainYearMonth,
  Goccia.Values.TemporalZonedDateTime,
  Goccia.Values.ToObject;

var
  GIntlDateTimeFormatSharedSlot: TGocciaRealmOwnedSlotId;

threadvar
  FPrototypeMembers: TArray<TGocciaMemberDefinition>;

const
  GREGORIAN_CYCLE_YEARS = 400;
  NANOSECONDS_PER_MILLISECOND = 1000000;
  TEMPORAL_SURROGATE_YEAR_BASE = 2000;
  TEMPORAL_DIRECT_ICU_YEAR_LIMIT = 9999;
  TWO_DIGIT_YEAR_MODULUS = 100;
  MIN_UNICODE_LOCALE_TYPE_SUBTAG_LENGTH = 3;
  MAX_UNICODE_LOCALE_TYPE_SUBTAG_LENGTH = 8;
  DEFAULT_CALENDAR = 'gregory';
  DEFAULT_NUMBERING_SYSTEM = 'latn';
  DEFAULT_HOUR_CYCLE_12 = 'h12';
  DEFAULT_HOUR_CYCLE_24 = 'h23';
  JAPANESE_HOUR_CYCLE_12 = 'h11';

type
  TGocciaIntlDateTimeFormatBoundFormatValue = class(TGocciaNativeFunctionValue)
  private
    FDateTimeFormat: TGocciaIntlDateTimeFormatValue;
    function Format(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  public
    constructor Create(const ADateTimeFormat: TGocciaIntlDateTimeFormatValue);
    procedure MarkReferences; override;
  end;

  TDateTimeFormattableKind = (dtfkNumber, dtfkPlainDate, dtfkPlainDateTime,
    dtfkPlainTime, dtfkPlainYearMonth, dtfkPlainMonthDay, dtfkInstant,
    dtfkZonedDateTime);

  TDateTimeFormattable = record
    Kind: TDateTimeFormattableKind;
    Millis: Double;
    Year: Integer;
    Month: Integer;
    Day: Integer;
    Hour: Integer;
    Minute: Integer;
    Second: Integer;
    Millisecond: Integer;
  end;

function GetIntlDateTimeFormatShared: TGocciaSharedPrototype; inline;
begin
  if Assigned(CurrentRealm) then
    Result := TGocciaSharedPrototype(CurrentRealm.GetOwnedSlot(GIntlDateTimeFormatSharedSlot))
  else
    Result := nil;
end;

function DefaultTimeZone: string;
begin
  Result := 'UTC';
end;

function IsAsciiDigit(const AChar: Char): Boolean;
begin
  Result := (AChar >= '0') and (AChar <= '9');
end;

function ReadTwoDigits(const AValue: string; const APosition: Integer;
  out ANumber: Integer): Boolean;
begin
  Result := False;
  ANumber := 0;
  if APosition + 1 > Length(AValue) then
    Exit;
  if not IsAsciiDigit(AValue[APosition]) or not IsAsciiDigit(AValue[APosition + 1]) then
    Exit;
  ANumber := (Ord(AValue[APosition]) - Ord('0')) * 10 +
    (Ord(AValue[APosition + 1]) - Ord('0'));
  Result := True;
end;

function TryParseOffsetTimeZoneIdentifier(const AValue: string;
  out AOffsetMinutes: Integer): Boolean;
var
  Hours, Minutes, Position, Sign: Integer;
begin
  Result := False;
  AOffsetMinutes := 0;

  if Length(AValue) < 3 then
    Exit;

  case AValue[1] of
    '+': Sign := 1;
    '-': Sign := -1;
  else
    Exit;
  end;

  if not ReadTwoDigits(AValue, 2, Hours) then
    Exit;

  Position := 4;
  Minutes := 0;
  if Position <= Length(AValue) then
  begin
    if AValue[Position] = ':' then
      Inc(Position);

    if not ReadTwoDigits(AValue, Position, Minutes) then
      Exit;
    Inc(Position, 2);

    if Position <= Length(AValue) then
      Exit;
  end;

  if (Hours > 23) or (Minutes > 59) then
    Exit;

  AOffsetMinutes := Sign * ((Hours * 60) + Minutes);
  Result := True;
end;

function IsAsciiTimeZoneIdentifier(const AValue: string): Boolean;
var
  I: Integer;
  Ch: Char;
begin
  Result := False;
  if AValue = '' then
    Exit;

  for I := 1 to Length(AValue) do
  begin
    Ch := AValue[I];
    if not (((Ch >= 'A') and (Ch <= 'Z')) or
            ((Ch >= 'a') and (Ch <= 'z')) or
            ((Ch >= '0') and (Ch <= '9')) or
            (Ch = '/') or (Ch = '_') or (Ch = '-') or (Ch = '+')) then
      Exit;
  end;

  Result := True;
end;

function DateTimeAsciiLower(const AValue: string): string;
var
  I: Integer;
  Ch: Char;
begin
  SetLength(Result, Length(AValue));
  for I := 1 to Length(AValue) do
  begin
    Ch := AValue[I];
    if (Ch >= 'A') and (Ch <= 'Z') then
      Result[I] := Char(Ord(Ch) + Ord('a') - Ord('A'))
    else
      Result[I] := Ch;
  end;
end;

function IsAsciiAlphaNumeric(const AChar: Char): Boolean;
begin
  Result := ((AChar >= '0') and (AChar <= '9')) or
    ((AChar >= 'A') and (AChar <= 'Z')) or
    ((AChar >= 'a') and (AChar <= 'z'));
end;

function IsUnicodeLocaleTypeIdentifier(const AValue: string): Boolean;
var
  I, SubtagLength: Integer;
begin
  Result := False;
  if AValue = '' then
    Exit;

  SubtagLength := 0;
  for I := 1 to Length(AValue) + 1 do
  begin
    if (I > Length(AValue)) or (AValue[I] = '-') then
    begin
      if (SubtagLength < MIN_UNICODE_LOCALE_TYPE_SUBTAG_LENGTH) or
         (SubtagLength > MAX_UNICODE_LOCALE_TYPE_SUBTAG_LENGTH) then
        Exit;
      SubtagLength := 0;
      Continue;
    end;

    if not IsAsciiAlphaNumeric(AValue[I]) then
      Exit;
    Inc(SubtagLength);
  end;

  Result := True;
end;

function TryGetDateTimeUnicodeExtensionKeyword(const ALocale, AKey: string;
  out AValue: string): Boolean;
var
  Index, NextDash: Integer;
  InUnicodeExtension, ReadingValue: Boolean;
  Subtag, KeyLower: string;
begin
  Result := False;
  AValue := '';
  InUnicodeExtension := False;
  ReadingValue := False;
  KeyLower := DateTimeAsciiLower(AKey);
  Index := 1;

  while Index <= Length(ALocale) do
  begin
    NextDash := Pos('-', Copy(ALocale, Index, MaxInt));
    if NextDash = 0 then
      NextDash := Length(ALocale) + 1
    else
      NextDash := Index + NextDash - 1;
    Subtag := DateTimeAsciiLower(Copy(ALocale, Index, NextDash - Index));
    Index := NextDash + 1;

    if Length(Subtag) = 1 then
    begin
      if Subtag = 'u' then
      begin
        InUnicodeExtension := True;
        ReadingValue := False;
        Continue;
      end;
      if InUnicodeExtension then
        Exit;
    end;

    if not InUnicodeExtension then
      Continue;

    if Length(Subtag) = 2 then
    begin
      if ReadingValue then
        Exit;
      ReadingValue := Subtag = KeyLower;
      if ReadingValue then
        Result := True;
      Continue;
    end;

    if ReadingValue then
    begin
      if AValue <> '' then
        AValue := AValue + '-';
      AValue := AValue + Subtag;
    end;
  end;
end;

function IsAvailableDateTimeCalendar(const ACalendar: string): Boolean;
begin
  Result := (ACalendar = 'buddhist') or
    (ACalendar = 'chinese') or
    (ACalendar = 'coptic') or
    (ACalendar = 'dangi') or
    (ACalendar = 'ethioaa') or
    (ACalendar = 'ethiopic') or
    (ACalendar = DEFAULT_CALENDAR) or
    (ACalendar = 'hebrew') or
    (ACalendar = 'indian') or
    (ACalendar = 'islamic-civil') or
    (ACalendar = 'islamic-tbla') or
    (ACalendar = 'islamic-umalqura') or
    (ACalendar = 'iso8601') or
    (ACalendar = 'japanese') or
    (ACalendar = 'persian') or
    (ACalendar = 'roc');
end;

function TryResolveDateTimeCalendarIdentifier(const AValue: string;
  out ACalendar: string): Boolean;
var
  Canonical: string;
begin
  ACalendar := '';
  Canonical := CanonicalizeTemporalCalendarIdentifier(DateTimeAsciiLower(AValue));
  if (Canonical = 'islamic') or (Canonical = 'islamic-rgsa') then
    Canonical := 'islamic-civil';
  if not IsAvailableDateTimeCalendar(Canonical) then
    Exit(False);

  ACalendar := Canonical;
  Result := True;
end;

function TryResolveDateTimeCalendarOption(const AValue, AOptionName: string;
  out ACalendar: string): Boolean;
begin
  if not IsUnicodeLocaleTypeIdentifier(AValue) then
    ThrowRangeError(Format(SErrorIntlInvalidOption, [AValue, AOptionName]));

  Result := TryResolveDateTimeCalendarIdentifier(AValue, ACalendar);
end;

function TryResolveDateTimeNumberingSystemOption(const AValue,
  AOptionName: string; out ANumberingSystem: string): Boolean;
var
  LowerValue: string;
begin
  if not IsUnicodeLocaleTypeIdentifier(AValue) then
    ThrowRangeError(Format(SErrorIntlInvalidOption, [AValue, AOptionName]));

  LowerValue := DateTimeAsciiLower(AValue);
  Result := IsSupportedNumberingSystem(LowerValue);
  if Result then
    ANumberingSystem := LowerValue
  else
    ANumberingSystem := '';
end;

function TryResolveDateTimeNumberingSystemIdentifier(const AValue: string;
  out ANumberingSystem: string): Boolean;
var
  LowerValue: string;
begin
  ANumberingSystem := '';
  if AValue = '' then
    Exit(False);

  LowerValue := DateTimeAsciiLower(AValue);
  Result := IsSupportedNumberingSystem(LowerValue);
  if Result then
    ANumberingSystem := LowerValue;
end;

function IsDateTimeHourCycle(const AValue: string): Boolean;
begin
  Result := (AValue = 'h11') or (AValue = 'h12') or
    (AValue = 'h23') or (AValue = 'h24');
end;

function TryResolveDateTimeHourCycleUnicodeExtension(const ALocale: string;
  out AHourCycle: string): Boolean;
var
  Value: string;
begin
  Result := False;
  AHourCycle := '';
  if not TryGetUnicodeLocaleExtensionKeyword(ALocale, 'hc', Value) then
    Exit;
  Value := DateTimeAsciiLower(Value);
  if not IsDateTimeHourCycle(Value) then
    Exit;
  AHourCycle := Value;
  Result := True;
end;

function IsDateTimeLocaleJapanese(const ALocale: string): Boolean;
var
  LowerLocale: string;
begin
  LowerLocale := DateTimeAsciiLower(LocaleWithoutUnicodeExtension(ALocale));
  Result := (LowerLocale = 'ja') or (Copy(LowerLocale, 1, 3) = 'ja-');
end;

function DateTimeHourCycle12(const ALocale: string): string;
begin
  if IsDateTimeLocaleJapanese(ALocale) then
    Result := JAPANESE_HOUR_CYCLE_12
  else
    Result := DEFAULT_HOUR_CYCLE_12;
end;

function DateTimeHourCycle24: string;
begin
  Result := DEFAULT_HOUR_CYCLE_24;
end;

function DateTimeLocaleUses12HourDefault(const ALocale: string): Boolean;
var
  LowerLocale: string;
begin
  LowerLocale := DateTimeAsciiLower(LocaleWithoutUnicodeExtension(ALocale));
  Result :=
    (LowerLocale = 'en') or
    (LowerLocale = 'en-us') or (Copy(LowerLocale, 1, 6) = 'en-us-') or
    (LowerLocale = 'en-ca') or (Copy(LowerLocale, 1, 6) = 'en-ca-') or
    (LowerLocale = 'en-au') or (Copy(LowerLocale, 1, 6) = 'en-au-') or
    (LowerLocale = 'en-nz') or (Copy(LowerLocale, 1, 6) = 'en-nz-') or
    (LowerLocale = 'en-in') or (Copy(LowerLocale, 1, 6) = 'en-in-') or
    (LowerLocale = 'en-ph') or (Copy(LowerLocale, 1, 6) = 'en-ph-') or
    (LowerLocale = 'en-sg') or (Copy(LowerLocale, 1, 6) = 'en-sg-') or
    (LowerLocale = 'ar') or (Copy(LowerLocale, 1, 3) = 'ar-') or
    (LowerLocale = 'hi') or (Copy(LowerLocale, 1, 3) = 'hi-');
end;

function DateTimeDefaultHourCycle(const ALocale: string): string;
begin
  if DateTimeLocaleUses12HourDefault(ALocale) then
    Result := DateTimeHourCycle12(ALocale)
  else
    Result := DateTimeHourCycle24;
end;

function DateTimeDefaultNumberingSystem(const ALocale: string): string;
var
  LowerLocale: string;
begin
  LowerLocale := DateTimeAsciiLower(LocaleWithoutUnicodeExtension(ALocale));
  if (LowerLocale = 'ar') or (Copy(LowerLocale, 1, 3) = 'ar-') then
    Result := 'arab'
  else
    Result := DEFAULT_NUMBERING_SYSTEM;
end;

function TryReadDateTimeStringOption(const AOptions: TGocciaObjectValue;
  const AName: string; out AValue: string): Boolean;
var
  V: TGocciaValue;
begin
  Result := False;
  AValue := '';
  if not Assigned(AOptions) then
    Exit;
  V := AOptions.GetProperty(AName);
  if (not Assigned(V)) or (V is TGocciaUndefinedLiteralValue) then
    Exit;

  AValue := V.ToStringLiteral.Value;
  if ContainsNulCharacter(AValue) then
    ThrowRangeError(Format(SErrorIntlInvalidOption, [AValue, AName]));
  Result := True;
end;

function ReadDateTimeStringOption(const AOptions: TGocciaObjectValue;
  const AName, AAllowedValues: string; const ADefault: string): string;
var
  Value: string;
begin
  if not TryReadDateTimeStringOption(AOptions, AName, Value) then
    Exit(ADefault);

  if (AAllowedValues <> '') and
     (Pos('|' + Value + '|', '|' + AAllowedValues + '|') = 0) then
    ThrowRangeError(Format(SErrorIntlInvalidOption, [Value, AName]));
  Result := Value;
end;

function TryReadDateTimeIntegerOption(const AOptions: TGocciaObjectValue;
  const AName: string; const AMinimum, AMaximum: Integer;
  out AValue: Integer): Boolean;
var
  NumberValue: Double;
  V: TGocciaValue;
begin
  Result := False;
  if not Assigned(AOptions) then
    Exit;
  V := AOptions.GetProperty(AName);
  if (not Assigned(V)) or (V is TGocciaUndefinedLiteralValue) then
    Exit;

  NumberValue := V.ToNumberLiteral.Value;
  if IsNan(NumberValue) or Math.IsInfinite(NumberValue) or
     (NumberValue < AMinimum) or (NumberValue > AMaximum) then
    ThrowRangeError(Format(SErrorIntlDigitsOutOfRange,
      [AName, AMinimum, AMaximum]));

  AValue := Trunc(Floor(NumberValue));
  Result := True;
end;

// ECMA-402 §11.1.3 FormatOffsetTimeZoneIdentifier(offsetMinutes).
function FormatOffsetTimeZoneIdentifier(const AOffsetMinutes: Integer): string;
var
  AbsoluteMinutes, Hours, Minutes: Integer;
  Sign: Char;
begin
  if AOffsetMinutes >= 0 then
    Sign := '+'
  else
    Sign := '-';
  AbsoluteMinutes := Abs(AOffsetMinutes);
  Hours := AbsoluteMinutes div 60;
  Minutes := AbsoluteMinutes mod 60;
  Result := Sign + Format('%.2d:%.2d', [Hours, Minutes]);
end;

function NormalizeDateTimeFormatTimeZone(const ATimeZone: string): string;
var
  OffsetMinutes: Integer;
begin
  Result := ATimeZone;
  if ATimeZone = '' then
    Exit;

  if not ((ATimeZone[1] = '+') or (ATimeZone[1] = '-')) then
    Exit;

  // ECMA-402 §11.1.2 normalizes offset time zone identifiers before storing
  // [[TimeZone]], so resolvedOptions() exposes ±HH:MM even for ±HH input.
  if not TryParseOffsetTimeZoneIdentifier(ATimeZone, OffsetMinutes) then
    ThrowRangeError(Format(SErrorIntlInvalidOption, [ATimeZone, 'timeZone']));

  Result := FormatOffsetTimeZoneIdentifier(OffsetMinutes);
  Exit;
end;

function CanonicalizeDateTimeFormatTimeZone(const ATimeZone: string): string;
var
  CanonicalTimeZone: string;
begin
  Result := NormalizeDateTimeFormatTimeZone(ATimeZone);
  if Result = '' then
    Exit;
  if (Result[1] = '+') or (Result[1] = '-') then
    Exit;

  if SameText(Result, 'UTC') then
    Exit('UTC');

  if not IsAsciiTimeZoneIdentifier(Result) then
    ThrowRangeError(Format(SErrorUnknownTimezone, [ATimeZone]));

  if TryCanonicalizeTimeZoneIdentifierCase(Result, CanonicalTimeZone) and
     IsValidTimeZone(CanonicalTimeZone) then
    Exit(CanonicalTimeZone);

  if IsValidTimeZone(Result) then
    Exit(Result);

  ThrowRangeError(Format(SErrorUnknownTimezone, [ATimeZone]));
end;

procedure AddResolvedLocaleKeyword(var ALocale: string; const AKey,
  AValue: string);
begin
  ALocale := AddUnicodeLocaleExtensionKeyword(ALocale, AKey, AValue);
end;

procedure ResolveDateTimeFormatLocaleOptions(
  var ALocale, ACalendar, ANumberingSystem, AHourCycle: string;
  const AHour12: Integer);
var
  BaseLocale, LocaleCalendar, LocaleNumberingSystem, LocaleHourCycle, RawKeywordValue: string;
  ResolvedLocale: string;
  HasLocaleCalendar, HasLocaleNumberingSystem, HasLocaleHourCycle: Boolean;
begin
  BaseLocale := LocaleWithoutUnicodeExtension(ALocale);
  if BaseLocale = '' then
    BaseLocale := ALocale;
  ResolvedLocale := BaseLocale;

  HasLocaleCalendar := False;
  if TryGetDateTimeUnicodeExtensionKeyword(ALocale, 'ca', LocaleCalendar) then
  begin
    RawKeywordValue := LocaleCalendar;
    HasLocaleCalendar := TryResolveDateTimeCalendarIdentifier(RawKeywordValue,
      LocaleCalendar);
  end;
  if ACalendar <> '' then
  begin
    if HasLocaleCalendar and (LocaleCalendar = ACalendar) then
      AddResolvedLocaleKeyword(ResolvedLocale, 'ca', ACalendar);
  end
  else if HasLocaleCalendar then
  begin
    ACalendar := LocaleCalendar;
    AddResolvedLocaleKeyword(ResolvedLocale, 'ca', ACalendar);
  end
  else
    ACalendar := DEFAULT_CALENDAR;

  HasLocaleNumberingSystem := False;
  if TryGetDateTimeUnicodeExtensionKeyword(ALocale, 'nu', LocaleNumberingSystem) then
  begin
    RawKeywordValue := LocaleNumberingSystem;
    HasLocaleNumberingSystem := TryResolveDateTimeNumberingSystemIdentifier(
      RawKeywordValue, LocaleNumberingSystem);
  end;
  if ANumberingSystem <> '' then
  begin
    if HasLocaleNumberingSystem and
       (LocaleNumberingSystem = ANumberingSystem) then
      AddResolvedLocaleKeyword(ResolvedLocale, 'nu', ANumberingSystem);
  end
  else if HasLocaleNumberingSystem then
  begin
    ANumberingSystem := LocaleNumberingSystem;
    AddResolvedLocaleKeyword(ResolvedLocale, 'nu', ANumberingSystem);
  end
  else
    ANumberingSystem := DateTimeDefaultNumberingSystem(ALocale);

  HasLocaleHourCycle := TryResolveDateTimeHourCycleUnicodeExtension(ALocale,
    LocaleHourCycle);
  if AHour12 = 1 then
    AHourCycle := DateTimeHourCycle12(ALocale)
  else if AHour12 = 0 then
    AHourCycle := DateTimeHourCycle24
  else if AHourCycle <> '' then
  begin
    if HasLocaleHourCycle and (LocaleHourCycle = AHourCycle) then
      AddResolvedLocaleKeyword(ResolvedLocale, 'hc', AHourCycle);
  end
  else if HasLocaleHourCycle then
  begin
    AHourCycle := LocaleHourCycle;
    AddResolvedLocaleKeyword(ResolvedLocale, 'hc', AHourCycle);
  end
  else
    AHourCycle := DateTimeDefaultHourCycle(ALocale);

  ALocale := ResolvedLocale;
end;

function ICUDateTimeFormatTimeZone(const ATimeZone: string): string;
begin
  Result := ATimeZone;
  if (ATimeZone <> '') and ((ATimeZone[1] = '+') or (ATimeZone[1] = '-')) then
    Result := 'GMT' + ATimeZone;
end;

function AsDateTimeFormat(const AValue: TGocciaValue; const AMethod: string): TGocciaIntlDateTimeFormatValue;
var
  FallbackValue: TGocciaValue;
begin
  if AValue is TGocciaIntlDateTimeFormatValue then
    Exit(TGocciaIntlDateTimeFormatValue(AValue));

  if AValue is TGocciaObjectValue then
  begin
    FallbackValue := TGocciaObjectValue(AValue).GetSymbolProperty(
      IntlFallbackSymbol);
    if FallbackValue is TGocciaIntlDateTimeFormatValue then
      Exit(TGocciaIntlDateTimeFormatValue(FallbackValue));
  end;

  ThrowTypeError(AMethod + ' called on non-DateTimeFormat');
  Result := nil;
end;

{ TGocciaIntlDateTimeFormatBoundFormatValue }

constructor TGocciaIntlDateTimeFormatBoundFormatValue.Create(
  const ADateTimeFormat: TGocciaIntlDateTimeFormatValue);
begin
  inherited CreateWithoutPrototype(Format, '', 1);
  FDateTimeFormat := ADateTimeFormat;
end;

function TGocciaIntlDateTimeFormatBoundFormatValue.Format(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := FDateTimeFormat.IntlDateTimeFormatFormat(AArgs, FDateTimeFormat);
end;

procedure TGocciaIntlDateTimeFormatBoundFormatValue.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FDateTimeFormat) then
    FDateTimeFormat.MarkReferences;
end;

function DateStyleStringToEnum(const AValue: string): TIntlDateTimeStyle;
begin
  if AValue = 'full' then
    Result := idtsFull
  else if AValue = 'long' then
    Result := idtsLong
  else if AValue = 'medium' then
    Result := idtsMedium
  else if AValue = 'short' then
    Result := idtsShort
  else
    Result := idtsNone;
end;

function TimeClipMillis(AValue: Double): Double;
begin
  if IsNan(AValue) or Math.IsInfinite(AValue) or (Abs(AValue) > 8.64e15) then
    Exit(Math.NaN);
  Result := Trunc(AValue);
end;

function CurrentEpochMilliseconds: Double;
begin
  Result := GetEpochNanoseconds div NANOSECONDS_PER_MILLISECOND;
end;

function DateTimeFormatArgumentMilliseconds(
  const AArgs: TGocciaArgumentsCollection): Double;
var
  DateValue: TGocciaValue;
begin
  DateValue := AArgs.GetElement(0);
  if DateValue is TGocciaUndefinedLiteralValue then
    Result := CurrentEpochMilliseconds
  else
    Result := TimeClipMillis(DateValue.ToNumberLiteral.Value);
end;

function EpochMillisFromDateTimeParts(const AYear, AMonth, ADay, AHour,
  AMinute, ASecond, AMillisecond: Integer): Double;
begin
  Result := DateToEpochDays(AYear, AMonth, ADay) * Int64(86400000) +
    Int64(AHour) * 3600000 + Int64(AMinute) * 60000 +
    Int64(ASecond) * 1000 + AMillisecond;
end;

procedure SetDateTimeFormattableFields(var AInput: TDateTimeFormattable;
  const AYear, AMonth, ADay, AHour, AMinute, ASecond, AMillisecond: Integer);
begin
  AInput.Year := AYear;
  AInput.Month := AMonth;
  AInput.Day := ADay;
  AInput.Hour := AHour;
  AInput.Minute := AMinute;
  AInput.Second := ASecond;
  AInput.Millisecond := AMillisecond;
end;

function IsTemporalDateTimeKind(const AKind: TDateTimeFormattableKind): Boolean;
begin
  Result := AKind in [dtfkPlainDate, dtfkPlainDateTime, dtfkPlainTime,
    dtfkPlainYearMonth, dtfkPlainMonthDay, dtfkInstant, dtfkZonedDateTime];
end;

function IsTemporalPlainKind(const AKind: TDateTimeFormattableKind): Boolean;
begin
  Result := AKind in [dtfkPlainDate, dtfkPlainDateTime, dtfkPlainTime,
    dtfkPlainYearMonth, dtfkPlainMonthDay];
end;

function HasDateFields(const AOptions: TIntlDateTimeFormatOptions): Boolean;
begin
  Result := (AOptions.DateStyle <> idtsNone) or (AOptions.Weekday <> '') or
    (AOptions.Year <> '') or (AOptions.Month <> '') or (AOptions.Day <> '');
end;

function HasTimeFields(const AOptions: TIntlDateTimeFormatOptions): Boolean;
begin
  Result := (AOptions.TimeStyle <> idtsNone) or (AOptions.DayPeriod <> '') or
    (AOptions.Hour <> '') or (AOptions.Minute <> '') or
    (AOptions.Second <> '') or (AOptions.FractionalSecondDigits >= 0);
end;

function HasAnyCoreFields(const AOptions: TIntlDateTimeFormatOptions): Boolean;
begin
  Result := HasDateFields(AOptions) or HasTimeFields(AOptions);
end;

procedure ExpandDateStyle(var AOptions: TIntlDateTimeFormatOptions);
begin
  case AOptions.DateStyle of
    idtsFull:
      begin
        AOptions.Weekday := 'long';
        AOptions.Year := 'numeric';
        AOptions.Month := 'long';
        AOptions.Day := 'numeric';
      end;
    idtsLong:
      begin
        AOptions.Year := 'numeric';
        AOptions.Month := 'long';
        AOptions.Day := 'numeric';
      end;
    idtsMedium:
      begin
        AOptions.Year := 'numeric';
        AOptions.Month := 'short';
        AOptions.Day := 'numeric';
      end;
    idtsShort:
      begin
        AOptions.Year := 'numeric';
        AOptions.Month := 'numeric';
        AOptions.Day := 'numeric';
      end;
  end;
  AOptions.DateStyle := idtsNone;
end;

procedure ExpandTimeStyle(var AOptions: TIntlDateTimeFormatOptions);
begin
  case AOptions.TimeStyle of
    idtsFull,
    idtsLong:
      begin
        AOptions.Hour := 'numeric';
        AOptions.Minute := 'numeric';
        AOptions.Second := 'numeric';
        AOptions.TimeZoneName := 'long';
      end;
    idtsMedium:
      begin
        AOptions.Hour := 'numeric';
        AOptions.Minute := 'numeric';
        AOptions.Second := 'numeric';
      end;
    idtsShort:
      begin
        AOptions.Hour := 'numeric';
        AOptions.Minute := 'numeric';
      end;
  end;
  AOptions.TimeStyle := idtsNone;
end;

procedure ClearDateFields(var AOptions: TIntlDateTimeFormatOptions);
begin
  AOptions.DateStyle := idtsNone;
  AOptions.Weekday := '';
  AOptions.Era := '';
  AOptions.Year := '';
  AOptions.Month := '';
  AOptions.Day := '';
end;

procedure ClearTimeFields(var AOptions: TIntlDateTimeFormatOptions);
begin
  AOptions.TimeStyle := idtsNone;
  AOptions.DayPeriod := '';
  AOptions.Hour := '';
  AOptions.Minute := '';
  AOptions.Second := '';
  AOptions.FractionalSecondDigits := -1;
  AOptions.TimeZoneName := '';
end;

procedure ApplyTemporalDefaultFields(var AOptions: TIntlDateTimeFormatOptions;
  const AKind: TDateTimeFormattableKind);
var
  Era, TimeZoneName: string;
begin
  Era := AOptions.Era;
  TimeZoneName := AOptions.TimeZoneName;
  ClearDateFields(AOptions);
  ClearTimeFields(AOptions);

  case AKind of
    dtfkPlainDate:
      begin
        AOptions.Era := Era;
        AOptions.Year := 'numeric';
        AOptions.Month := 'numeric';
        AOptions.Day := 'numeric';
      end;
    dtfkPlainDateTime,
    dtfkInstant:
      begin
        AOptions.Era := Era;
        AOptions.Year := 'numeric';
        AOptions.Month := 'numeric';
        AOptions.Day := 'numeric';
        AOptions.Hour := 'numeric';
        AOptions.Minute := 'numeric';
        AOptions.Second := 'numeric';
        if (AKind = dtfkInstant) and (TimeZoneName <> '') then
          AOptions.TimeZoneName := TimeZoneName;
      end;
    dtfkZonedDateTime:
      begin
        AOptions.Era := Era;
        AOptions.Year := 'numeric';
        AOptions.Month := 'numeric';
        AOptions.Day := 'numeric';
        AOptions.Hour := 'numeric';
        AOptions.Minute := 'numeric';
        AOptions.Second := 'numeric';
        if TimeZoneName <> '' then
          AOptions.TimeZoneName := TimeZoneName
        else
          AOptions.TimeZoneName := 'short';
      end;
    dtfkPlainTime:
      begin
        AOptions.Hour := 'numeric';
        AOptions.Minute := 'numeric';
        AOptions.Second := 'numeric';
      end;
    dtfkPlainYearMonth:
      begin
        AOptions.Era := Era;
        AOptions.Year := 'numeric';
        AOptions.Month := 'numeric';
      end;
    dtfkPlainMonthDay:
      begin
        AOptions.Month := 'numeric';
        AOptions.Day := 'numeric';
      end;
  end;
end;

function FilterTemporalDateTimeOptions(const AOptions: TIntlDateTimeFormatOptions;
  const AKind: TDateTimeFormattableKind; const AHasExplicitCoreOptions: Boolean;
  out AEffectiveOptions: TIntlDateTimeFormatOptions): Boolean;
var
  HadExplicitCore, HadRelevantCore: Boolean;
begin
  AEffectiveOptions := AOptions;
  HadExplicitCore := AHasExplicitCoreOptions;

  case AKind of
    dtfkPlainDate,
    dtfkPlainYearMonth,
    dtfkPlainMonthDay:
      if AEffectiveOptions.TimeStyle <> idtsNone then
        Exit(False);
    dtfkPlainTime:
      if AEffectiveOptions.DateStyle <> idtsNone then
        Exit(False);
  end;

  if (AEffectiveOptions.DateStyle <> idtsNone) and
     not ((AKind = dtfkInstant) or (AKind = dtfkPlainDate) or
       (AKind = dtfkPlainDateTime) or (AKind = dtfkZonedDateTime)) then
    ExpandDateStyle(AEffectiveOptions);
  if (AEffectiveOptions.TimeStyle <> idtsNone) and
     not ((AKind = dtfkInstant) or (AKind = dtfkPlainTime) or
       (AKind = dtfkPlainDateTime) or (AKind = dtfkZonedDateTime)) then
    ExpandTimeStyle(AEffectiveOptions);

  if not HadExplicitCore then
  begin
    ApplyTemporalDefaultFields(AEffectiveOptions, AKind);
    if IsTemporalPlainKind(AKind) then
    begin
      AEffectiveOptions.TimeZone := 'UTC';
      AEffectiveOptions.TimeZoneName := '';
    end;
    Exit(True);
  end;

  if IsTemporalPlainKind(AKind) then
  begin
    AEffectiveOptions.TimeZone := 'UTC';
    AEffectiveOptions.TimeZoneName := '';
  end;

  case AKind of
    dtfkPlainDate:
      begin
        ClearTimeFields(AEffectiveOptions);
      end;
    dtfkPlainYearMonth:
      begin
        ClearTimeFields(AEffectiveOptions);
        AEffectiveOptions.Weekday := '';
        AEffectiveOptions.Day := '';
      end;
    dtfkPlainMonthDay:
      begin
        ClearTimeFields(AEffectiveOptions);
        AEffectiveOptions.Weekday := '';
        AEffectiveOptions.Era := '';
        AEffectiveOptions.Year := '';
      end;
    dtfkPlainTime:
      begin
        ClearDateFields(AEffectiveOptions);
      end;
  end;

  HadRelevantCore := HasAnyCoreFields(AEffectiveOptions);
  if not HadRelevantCore then
  begin
    Exit(False);
  end;

  Result := True;
end;

function PositiveModulo(const AValue, AModulo: Integer): Integer;
begin
  Result := AValue mod AModulo;
  if Result < 0 then
    Inc(Result, AModulo);
end;

function CompareInteger(const ALeft, ARight: Integer): Integer;
begin
  if ALeft < ARight then
    Result := -1
  else if ALeft > ARight then
    Result := 1
  else
    Result := 0;
end;

function CompareDateTimeFormattableFields(const ALeft,
  ARight: TDateTimeFormattable): Integer;
begin
  Result := CompareInteger(ALeft.Year, ARight.Year);
  if Result <> 0 then Exit;
  Result := CompareInteger(ALeft.Month, ARight.Month);
  if Result <> 0 then Exit;
  Result := CompareInteger(ALeft.Day, ARight.Day);
  if Result <> 0 then Exit;
  Result := CompareInteger(ALeft.Hour, ARight.Hour);
  if Result <> 0 then Exit;
  Result := CompareInteger(ALeft.Minute, ARight.Minute);
  if Result <> 0 then Exit;
  Result := CompareInteger(ALeft.Second, ARight.Second);
  if Result <> 0 then Exit;
  Result := CompareInteger(ALeft.Millisecond, ARight.Millisecond);
end;

function SurrogateTemporalYear(const AYear: Integer): Integer;
begin
  Result := TEMPORAL_SURROGATE_YEAR_BASE +
    PositiveModulo(AYear - TEMPORAL_SURROGATE_YEAR_BASE,
      GREGORIAN_CYCLE_YEARS);
end;

procedure RecomputeDateTimeFormattableMillis(var AInput: TDateTimeFormattable);
begin
  AInput.Millis := EpochMillisFromDateTimeParts(AInput.Year, AInput.Month,
    AInput.Day, AInput.Hour, AInput.Minute, AInput.Second, AInput.Millisecond);
end;

procedure AlignSurrogateTemporalRangeOrder(const AStartInput,
  AEndInput: TDateTimeFormattable; var AStartSurrogate,
  AEndSurrogate: TDateTimeFormattable);
var
  SourceOrder: Integer;
begin
  SourceOrder := CompareDateTimeFormattableFields(AStartInput, AEndInput);
  if SourceOrder <= 0 then
    while (SourceOrder < 0) and
      (CompareDateTimeFormattableFields(AStartSurrogate,
        AEndSurrogate) >= 0) do
      Inc(AEndSurrogate.Year, GREGORIAN_CYCLE_YEARS)
  else
    while CompareDateTimeFormattableFields(AStartSurrogate,
      AEndSurrogate) <= 0 do
      Dec(AEndSurrogate.Year, GREGORIAN_CYCLE_YEARS);
end;

function NeedsTemporalSurrogateRange(const AStartInput,
  AEndInput: TDateTimeFormattable): Boolean;
begin
  Result := IsTemporalPlainKind(AStartInput.Kind) and
    ((Abs(AStartInput.Year) > TEMPORAL_DIRECT_ICU_YEAR_LIMIT) or
     (Abs(AEndInput.Year) > TEMPORAL_DIRECT_ICU_YEAR_LIMIT));
end;

function LocalizedTemporalYearString(const ALocale: string; const AYear: Integer;
  const AOptions: TIntlDateTimeFormatOptions): string;
var
  NumberOptions: TIntlNumberFormatOptions;
  YearValue: Integer;
  Formatted: string;
begin
  NumberOptions := DefaultNumberFormatOptions;
  NumberOptions.UseGrouping := inugFalse;
  NumberOptions.MinimumFractionDigits := 0;
  NumberOptions.MaximumFractionDigits := 0;
  NumberOptions.NumberingSystem := AOptions.NumberingSystem;
  YearValue := AYear;

  if AOptions.Year = '2-digit' then
  begin
    YearValue := Abs(AYear) mod TWO_DIGIT_YEAR_MODULUS;
    NumberOptions.MinimumIntegerDigits := 2;
  end;

  if TryICUFormatNumber(ALocale, YearValue, NumberOptions, Formatted) then
    Exit(Formatted);

  Result := IntToStr(YearValue);
  if (AOptions.Year = '2-digit') and (YearValue < 10) then
    Result := '0' + Result;
end;

function SameTemporalEra(const AStartYear, AEndYear: Integer): Boolean;
begin
  Result := (AStartYear <= 0) = (AEndYear <= 0);
end;

function LocalizedTemporalEraString(const ALocale: string; const AYear: Integer;
  const AOptions: TIntlDateTimeFormatOptions): string;
var
  EraOptions: TIntlDateTimeFormatOptions;
  EraParts: TIntlFormatPartArray;
  EraMillis: Double;
  I: Integer;
begin
  if AOptions.Era = '' then
    Exit('');

  EraOptions := DefaultDateTimeFormatOptions;
  EraOptions.Calendar := AOptions.Calendar;
  EraOptions.NumberingSystem := AOptions.NumberingSystem;
  EraOptions.TimeZone := 'UTC';
  EraOptions.Era := AOptions.Era;
  EraOptions.Year := 'numeric';

  if AYear <= 0 then
    EraMillis := EpochMillisFromDateTimeParts(-1, 1, 1, 0, 0, 0, 0)
  else
    EraMillis := EpochMillisFromDateTimeParts(1, 1, 1, 0, 0, 0, 0);

  if TryICUFormatDateTimeToParts(ALocale, EraMillis, EraOptions, EraParts) then
    for I := 0 to Length(EraParts) - 1 do
      if EraParts[I].PartType = 'era' then
        Exit(EraParts[I].Value);

  if AYear <= 0 then
    Result := 'BC'
  else
    Result := 'AD';
end;

procedure SetFormatPartSource(var AParts: TIntlFormatPartArray;
  const ASource: string);
var
  I: Integer;
begin
  for I := 0 to Length(AParts) - 1 do
    AParts[I].Source := ASource;
end;

procedure AppendFormatPartArray(var ADest: TIntlFormatPartArray;
  const ASrc: TIntlFormatPartArray);
var
  I, OldLength: Integer;
begin
  OldLength := Length(ADest);
  SetLength(ADest, OldLength + Length(ASrc));
  for I := 0 to Length(ASrc) - 1 do
    ADest[OldLength + I] := ASrc[I];
end;

procedure ReplaceTemporalSurrogateParts(var AParts: TIntlFormatPartArray;
  const ALocale: string; const AStartInput, AEndInput: TDateTimeFormattable;
  const AOptions: TIntlDateTimeFormatOptions);
var
  I: Integer;
  StartYear, EndYear, StartEra, EndEra: string;
begin
  StartYear := LocalizedTemporalYearString(ALocale, AStartInput.Year, AOptions);
  EndYear := LocalizedTemporalYearString(ALocale, AEndInput.Year, AOptions);
  StartEra := LocalizedTemporalEraString(ALocale, AStartInput.Year, AOptions);
  EndEra := LocalizedTemporalEraString(ALocale, AEndInput.Year, AOptions);

  for I := 0 to Length(AParts) - 1 do
    if AParts[I].PartType = 'year' then
    begin
      if AParts[I].Source = 'endRange' then
        AParts[I].Value := EndYear
      else
        AParts[I].Value := StartYear;
    end
    else if AParts[I].PartType = 'era' then
    begin
      if AParts[I].Source = 'endRange' then
        AParts[I].Value := EndEra
      else if AParts[I].Source = 'startRange' then
        AParts[I].Value := StartEra
      else if SameTemporalEra(AStartInput.Year, AEndInput.Year) then
        AParts[I].Value := StartEra;
    end;
end;

function TryICUTemporalSurrogateSingleToParts(const ALocale: string;
  const AInput: TDateTimeFormattable; const AOptions: TIntlDateTimeFormatOptions;
  const ASource: string; out AParts: TIntlFormatPartArray): Boolean;
var
  Surrogate: TDateTimeFormattable;
begin
  Surrogate := AInput;
  Surrogate.Year := SurrogateTemporalYear(AInput.Year);
  RecomputeDateTimeFormattableMillis(Surrogate);
  Result := TryICUFormatDateTimeToParts(ALocale, Surrogate.Millis, AOptions,
    AParts);
  if not Result then
    Exit;

  ReplaceTemporalSurrogateParts(AParts, ALocale, AInput, AInput, AOptions);
  SetFormatPartSource(AParts, ASource);
end;

function TryICUTemporalMixedEraRangeToParts(const ALocale: string;
  const AStartInput, AEndInput: TDateTimeFormattable;
  const AOptions: TIntlDateTimeFormatOptions;
  out AParts: TIntlFormatPartArray): Boolean;
var
  StartParts, EndParts: TIntlFormatPartArray;
  LiteralIndex: Integer;
begin
  Result := False;
  SetLength(AParts, 0);
  if (AOptions.Era = '') or SameTemporalEra(AStartInput.Year, AEndInput.Year) then
    Exit;

  if not TryICUTemporalSurrogateSingleToParts(ALocale, AStartInput, AOptions,
    'startRange', StartParts) then
    Exit;
  if not TryICUTemporalSurrogateSingleToParts(ALocale, AEndInput, AOptions,
    'endRange', EndParts) then
    Exit;

  AppendFormatPartArray(AParts, StartParts);
  SetLength(AParts, Length(AParts) + 1);
  LiteralIndex := Length(AParts) - 1;
  AParts[LiteralIndex].PartType := 'literal';
  AParts[LiteralIndex].Value := ' – ';
  AParts[LiteralIndex].Source := 'shared';
  AppendFormatPartArray(AParts, EndParts);
  Result := True;
end;

function TryICUTemporalSurrogateRangeToParts(const ALocale: string;
  const AStartInput, AEndInput: TDateTimeFormattable;
  const AOptions: TIntlDateTimeFormatOptions;
  out AParts: TIntlFormatPartArray): Boolean;
var
  StartSurrogate, EndSurrogate: TDateTimeFormattable;
begin
  Result := False;
  SetLength(AParts, 0);
  if not NeedsTemporalSurrogateRange(AStartInput, AEndInput) then
    Exit;

  if TryICUTemporalMixedEraRangeToParts(ALocale, AStartInput, AEndInput,
    AOptions, AParts) then
    Exit(True);

  StartSurrogate := AStartInput;
  EndSurrogate := AEndInput;
  StartSurrogate.Year := SurrogateTemporalYear(AStartInput.Year);
  EndSurrogate.Year := SurrogateTemporalYear(AEndInput.Year);
  AlignSurrogateTemporalRangeOrder(AStartInput, AEndInput, StartSurrogate,
    EndSurrogate);
  RecomputeDateTimeFormattableMillis(StartSurrogate);
  RecomputeDateTimeFormattableMillis(EndSurrogate);

  if not TryICUFormatDateTimeRangeToParts(ALocale, StartSurrogate.Millis,
    EndSurrogate.Millis, AOptions, AParts) then
    Exit;

  ReplaceTemporalSurrogateParts(AParts, ALocale, AStartInput, AEndInput,
    AOptions);
  Result := True;
end;

function TryICUTemporalSurrogateToParts(const ALocale: string;
  const AInput: TDateTimeFormattable; const AOptions: TIntlDateTimeFormatOptions;
  out AParts: TIntlFormatPartArray): Boolean;
begin
  Result := False;
  SetLength(AParts, 0);
  if not (IsTemporalPlainKind(AInput.Kind) and
          (Abs(AInput.Year) > TEMPORAL_DIRECT_ICU_YEAR_LIMIT)) then
    Exit;

  Result := TryICUTemporalSurrogateSingleToParts(ALocale, AInput, AOptions,
    '', AParts);
end;

function FormatPartArrayValue(const AParts: TIntlFormatPartArray): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to Length(AParts) - 1 do
    Result := Result + AParts[I].Value;
end;

function ToDateTimeFormattable(const AValue: TGocciaValue): TDateTimeFormattable;
var
  PlainDate: TGocciaTemporalPlainDateValue;
  PlainDateTime: TGocciaTemporalPlainDateTimeValue;
  PlainTime: TGocciaTemporalPlainTimeValue;
  PlainYearMonth: TGocciaTemporalPlainYearMonthValue;
  PlainMonthDay: TGocciaTemporalPlainMonthDayValue;
  Instant: TGocciaTemporalInstantValue;
  ZonedDateTime: TGocciaTemporalZonedDateTimeValue;
begin
  if AValue is TGocciaTemporalPlainDateValue then
  begin
    PlainDate := TGocciaTemporalPlainDateValue(AValue);
    Result.Kind := dtfkPlainDate;
    SetDateTimeFormattableFields(Result, PlainDate.Year, PlainDate.Month,
      PlainDate.Day, 0, 0, 0, 0);
    Result.Millis := EpochMillisFromDateTimeParts(PlainDate.Year,
      PlainDate.Month, PlainDate.Day, 0, 0, 0, 0);
  end
  else if AValue is TGocciaTemporalPlainDateTimeValue then
  begin
    PlainDateTime := TGocciaTemporalPlainDateTimeValue(AValue);
    Result.Kind := dtfkPlainDateTime;
    SetDateTimeFormattableFields(Result, PlainDateTime.Year,
      PlainDateTime.Month, PlainDateTime.Day, PlainDateTime.Hour,
      PlainDateTime.Minute, PlainDateTime.Second, PlainDateTime.Millisecond);
    Result.Millis := EpochMillisFromDateTimeParts(PlainDateTime.Year,
      PlainDateTime.Month, PlainDateTime.Day, PlainDateTime.Hour,
      PlainDateTime.Minute, PlainDateTime.Second, PlainDateTime.Millisecond);
  end
  else if AValue is TGocciaTemporalPlainTimeValue then
  begin
    PlainTime := TGocciaTemporalPlainTimeValue(AValue);
    Result.Kind := dtfkPlainTime;
    SetDateTimeFormattableFields(Result, 1970, 1, 1, PlainTime.Hour,
      PlainTime.Minute, PlainTime.Second, PlainTime.Millisecond);
    Result.Millis := EpochMillisFromDateTimeParts(1970, 1, 1,
      PlainTime.Hour, PlainTime.Minute, PlainTime.Second, PlainTime.Millisecond);
  end
  else if AValue is TGocciaTemporalPlainYearMonthValue then
  begin
    PlainYearMonth := TGocciaTemporalPlainYearMonthValue(AValue);
    Result.Kind := dtfkPlainYearMonth;
    SetDateTimeFormattableFields(Result, PlainYearMonth.Year,
      PlainYearMonth.Month, PlainYearMonth.ReferenceDay, 0, 0, 0, 0);
    Result.Millis := EpochMillisFromDateTimeParts(PlainYearMonth.Year,
      PlainYearMonth.Month, PlainYearMonth.ReferenceDay, 0, 0, 0, 0);
  end
  else if AValue is TGocciaTemporalPlainMonthDayValue then
  begin
    PlainMonthDay := TGocciaTemporalPlainMonthDayValue(AValue);
    Result.Kind := dtfkPlainMonthDay;
    SetDateTimeFormattableFields(Result, PlainMonthDay.ReferenceYear,
      PlainMonthDay.Month, PlainMonthDay.Day, 0, 0, 0, 0);
    Result.Millis := EpochMillisFromDateTimeParts(PlainMonthDay.ReferenceYear,
      PlainMonthDay.Month, PlainMonthDay.Day, 0, 0, 0, 0);
  end
  else if AValue is TGocciaTemporalInstantValue then
  begin
    Instant := TGocciaTemporalInstantValue(AValue);
    Result.Kind := dtfkInstant;
    SetDateTimeFormattableFields(Result, 1970, 1, 1, 0, 0, 0, 0);
    Result.Millis := Instant.EpochMilliseconds;
  end
  else if AValue is TGocciaTemporalZonedDateTimeValue then
  begin
    ZonedDateTime := TGocciaTemporalZonedDateTimeValue(AValue);
    Result.Kind := dtfkZonedDateTime;
    SetDateTimeFormattableFields(Result, 1970, 1, 1, 0, 0, 0, 0);
    Result.Millis := ZonedDateTime.EpochMilliseconds;
  end
  else
  begin
    Result.Kind := dtfkNumber;
    SetDateTimeFormattableFields(Result, 1970, 1, 1, 0, 0, 0, 0);
    Result.Millis := AValue.ToNumberLiteral.Value;
  end;
end;

procedure SetDatePartsSource(var AParts: TIntlFormatPartArray; const ASource: string);
var
  I: Integer;
begin
  for I := 0 to Length(AParts) - 1 do
    AParts[I].Source := ASource;
end;

function TryGetDatePartsWithFallback(const ALocale: string; AMillis: Double;
  const AOptions: TIntlDateTimeFormatOptions; const ASource: string;
  out AParts: TIntlFormatPartArray): Boolean;
var
  Formatted: string;
begin
  Result := True;
  if not TryICUFormatDateTimeToParts(ALocale, AMillis, AOptions, AParts) then
  begin
    if TryICUFormatDateTime(ALocale, AMillis, AOptions, Formatted) then
    begin
      SetLength(AParts, 1);
      AParts[0].PartType := 'literal';
      AParts[0].Value := Formatted;
    end
    else
    begin
      SetLength(AParts, 1);
      AParts[0].PartType := 'literal';
      AParts[0].Value := FloatToStr(AMillis);
    end;
  end;
  SetDatePartsSource(AParts, ASource);
end;

function IsUndefinedDateTimeFormatValue(const AValue: TGocciaValue): Boolean;
begin
  Result := (not Assigned(AValue)) or (AValue is TGocciaUndefinedLiteralValue);
end;

function DateTimeFormatHasOnlyYearEra(const AOptions: TIntlDateTimeFormatOptions): Boolean;
begin
  Result := (AOptions.Calendar = DEFAULT_CALENDAR) and
    (AOptions.Era <> '') and (AOptions.Year <> '') and
    (AOptions.Weekday = '') and (AOptions.Month = '') and
    (AOptions.Day = '') and (AOptions.DayPeriod = '') and
    (AOptions.Hour = '') and (AOptions.Minute = '') and
    (AOptions.Second = '') and (AOptions.FractionalSecondDigits < 0) and
    (AOptions.TimeZoneName = '') and (AOptions.DateStyle = idtsNone) and
    (AOptions.TimeStyle = idtsNone);
end;

function ReplaceFirstAsciiDigitRun(const AValue, AReplacement: string): string;
var
  StartIndex, EndIndex: Integer;
begin
  StartIndex := 1;
  while (StartIndex <= Length(AValue)) and
        not (AValue[StartIndex] in ['0'..'9']) do
    Inc(StartIndex);
  if StartIndex > Length(AValue) then
    Exit(AValue);

  EndIndex := StartIndex;
  while (EndIndex <= Length(AValue)) and (AValue[EndIndex] in ['0'..'9']) do
    Inc(EndIndex);

  Result := Copy(AValue, 1, StartIndex - 1) + AReplacement +
    Copy(AValue, EndIndex, MaxInt);
end;

function AdjustProlepticGregorianYearEra(const AFormatted: string;
  const AMillis: Double; const AOptions: TIntlDateTimeFormatOptions): string;
const
  MS_PER_DAY_DOUBLE = 86400000.0;
var
  DateRec: TTemporalDateRecord;
  EpochDays: Int64;
  ExpectedYear: Int64;
begin
  Result := AFormatted;
  if not DateTimeFormatHasOnlyYearEra(AOptions) then
    Exit;

  EpochDays := Trunc(Floor(AMillis / MS_PER_DAY_DOUBLE));
  DateRec := EpochDaysToDate(EpochDays);
  if DateRec.Year <= 0 then
    ExpectedYear := 1 - Int64(DateRec.Year)
  else
    ExpectedYear := DateRec.Year;

  Result := ReplaceFirstAsciiDigitRun(AFormatted, IntToStr(ExpectedYear));
end;

function CopticEraFallbackValue(const AOptions: TIntlDateTimeFormatOptions): string;
begin
  if AOptions.Era = 'long' then
    Result := 'Anno Martyrum'
  else
    Result := 'AM';
end;

function NeedsCopticEraFallback(const AOptions: TIntlDateTimeFormatOptions): Boolean;
begin
  Result := (AOptions.Calendar = 'coptic') and (AOptions.Era <> '') and
    (AOptions.Year <> '');
end;

function AdjustCopticEraFormatted(const AFormatted: string;
  const AOptions: TIntlDateTimeFormatOptions): string;
begin
  Result := AFormatted;
  if not NeedsCopticEraFallback(AOptions) then
    Exit;
  Result := StringReplace(Result, 'ERA0', '', [rfReplaceAll]);
  Result := StringReplace(Result, 'ERA1', '', [rfReplaceAll]);
  if Pos(CopticEraFallbackValue(AOptions), Result) = 0 then
    Result := Result + CopticEraFallbackValue(AOptions);
end;

procedure EnsureCopticEraPart(var AParts: TIntlFormatPartArray;
  const AOptions: TIntlDateTimeFormatOptions);
var
  I, Index: Integer;
begin
  if not NeedsCopticEraFallback(AOptions) then
    Exit;

  for I := 0 to Length(AParts) - 1 do
    if AParts[I].PartType = 'era' then
    begin
      AParts[I].Value := CopticEraFallbackValue(AOptions);
      Exit;
    end;

  Index := Length(AParts);
  SetLength(AParts, Index + 1);
  AParts[Index].PartType := 'era';
  AParts[Index].Value := CopticEraFallbackValue(AOptions);
  AParts[Index].Source := '';
end;

function NeedsIslamicEraFallback(const AOptions: TIntlDateTimeFormatOptions): Boolean;
begin
  Result := ((AOptions.Calendar = 'islamic-civil') or
    (AOptions.Calendar = 'islamic-tbla') or
    (AOptions.Calendar = 'islamic-umalqura')) and
    (AOptions.Era <> '') and (AOptions.Year <> '');
end;

function AdjustIslamicEraFormatted(const AFormatted: string;
  const AOptions: TIntlDateTimeFormatOptions): string;
begin
  Result := AFormatted;
  if not NeedsIslamicEraFallback(AOptions) then
    Exit;
  if (Pos('-', AFormatted) > 0) and
     (Pos('Anno Hegirae', AFormatted) > 0) then
    Result := StringReplace(AFormatted, 'Anno Hegirae',
      'Before Anno Hegirae', [rfReplaceAll])
  else if (Pos('-', AFormatted) > 0) and (Pos('AH', AFormatted) > 0) and
          (Pos('Before AH', AFormatted) = 0) then
    Result := StringReplace(AFormatted, 'AH', 'Before AH', [rfReplaceAll]);
end;

procedure AdjustIslamicEraParts(var AParts: TIntlFormatPartArray;
  const AOptions: TIntlDateTimeFormatOptions);
var
  I: Integer;
  HasNegativeYear: Boolean;
begin
  if not NeedsIslamicEraFallback(AOptions) then
    Exit;

  HasNegativeYear := False;
  for I := 0 to Length(AParts) - 1 do
    if (AParts[I].PartType = 'year') and
       (Length(AParts[I].Value) > 0) and (AParts[I].Value[1] = '-') then
    begin
      HasNegativeYear := True;
      Break;
    end;

  if not HasNegativeYear then
    Exit;

  for I := 0 to Length(AParts) - 1 do
    if (AParts[I].PartType = 'era') and
       (AParts[I].Value = 'Anno Hegirae') then
      AParts[I].Value := 'Before Anno Hegirae'
    else if (AParts[I].PartType = 'era') and
            (AParts[I].Value = 'AH') then
      AParts[I].Value := 'Before AH';
end;

function IsAsciiDigitString(const AValue: string): Boolean;
var
  I: Integer;
begin
  Result := AValue <> '';
  for I := 1 to Length(AValue) do
    if not (AValue[I] in ['0'..'9']) then
      Exit(False);
end;

function IsAsciiDigitOrDigitBisString(const AValue: string): Boolean;
const
  BIS_SUFFIX = 'bis';
var
  DigitLength: Integer;
begin
  if IsAsciiDigitString(AValue) then
    Exit(True);

  if (Length(AValue) <= Length(BIS_SUFFIX)) or
     (Copy(AValue, Length(AValue) - Length(BIS_SUFFIX) + 1,
       Length(BIS_SUFFIX)) <> BIS_SUFFIX) then
    Exit(False);

  DigitLength := Length(AValue) - Length(BIS_SUFFIX);
  Result := IsAsciiDigitString(Copy(AValue, 1, DigitLength));
end;

procedure AdjustLunisolarLeapMonthParts(var AParts: TIntlFormatPartArray;
  const AMillis: Double; const AOptions: TIntlDateTimeFormatOptions);
const
  MS_PER_DAY_DOUBLE = 86400000.0;
var
  DateRec: TTemporalDateRecord;
  EpochDays: Int64;
  Info: TTemporalCalendarDateInfo;
  MonthCodeMonth, I: Integer;
  ExpectedMonthValue: string;
  IsLeapMonth: Boolean;
begin
  if not ((AOptions.Calendar = 'chinese') or
          (AOptions.Calendar = 'dangi')) or
     (AOptions.TimeZone <> 'UTC') then
    Exit;

  EpochDays := Trunc(Floor(AMillis / MS_PER_DAY_DOUBLE));
  DateRec := EpochDaysToDate(EpochDays);
  if not TryGetCalendarDateInfo(AOptions.Calendar, DateRec.Year,
     DateRec.Month, DateRec.Day, Info) or
     not TryParseTemporalMonthCode(Info.MonthCode, MonthCodeMonth,
       IsLeapMonth) then
    Exit;

  ExpectedMonthValue := IntToStr(MonthCodeMonth);
  if IsLeapMonth then
    ExpectedMonthValue := ExpectedMonthValue + 'bis';

  for I := 0 to Length(AParts) - 1 do
    if (AParts[I].PartType = 'month') and
       (AParts[I].Value <> ExpectedMonthValue) and
       IsAsciiDigitOrDigitBisString(AParts[I].Value) then
    begin
      AParts[I].Value := ExpectedMonthValue;
      Exit;
    end;
end;

function DateTimeFormatLocaleArgumentToLocale(const AArg: TGocciaValue): string;
var
  RequestedLocales: TStringArray;
begin
  RequestedLocales := CanonicalizeLocaleListFromValue(AArg);
  Result := ResolveRequestedLocale(RequestedLocales);
  if Result = '' then
    Exit;
end;

function DateTimeFormatOptionsArgumentToObject(
  const AArg: TGocciaValue): TGocciaObjectValue;
begin
  if IsUndefinedDateTimeFormatValue(AArg) then
    Result := nil
  else
    Result := ToObject(AArg);
end;

function HasDateTimeStyleConflict(const ADateTimeFormat: TGocciaIntlDateTimeFormatValue): Boolean;
begin
  Result := (ADateTimeFormat.FWeekday <> '') or
    (ADateTimeFormat.FEra <> '') or
    (ADateTimeFormat.FYear <> '') or
    (ADateTimeFormat.FMonth <> '') or
    (ADateTimeFormat.FDay <> '') or
    (ADateTimeFormat.FDayPeriod <> '') or
    (ADateTimeFormat.FHour <> '') or
    (ADateTimeFormat.FMinute <> '') or
    (ADateTimeFormat.FSecond <> '') or
    (ADateTimeFormat.FFractionalSecondDigits >= 0) or
    (ADateTimeFormat.FTimeZoneName <> '');
end;

function TemporalLocaleCalendarMatches(const AValueCalendar,
  ALocaleCalendar: string; const AAllowISOCalendar: Boolean): Boolean;
begin
  Result := (AValueCalendar = '') or (AValueCalendar = ALocaleCalendar) or
    (AAllowISOCalendar and (AValueCalendar = 'iso8601'));
end;

function TemporalDateTimeValueCalendar(const AValue: TGocciaValue): string;
begin
  if AValue is TGocciaTemporalPlainDateValue then
    Result := TGocciaTemporalPlainDateValue(AValue).CalendarId
  else if AValue is TGocciaTemporalPlainDateTimeValue then
    Result := TGocciaTemporalPlainDateTimeValue(AValue).CalendarId
  else if AValue is TGocciaTemporalPlainYearMonthValue then
    Result := TGocciaTemporalPlainYearMonthValue(AValue).CalendarId
  else if AValue is TGocciaTemporalPlainMonthDayValue then
    Result := TGocciaTemporalPlainMonthDayValue(AValue).CalendarId
  else if AValue is TGocciaTemporalZonedDateTimeValue then
    Result := TGocciaTemporalZonedDateTimeValue(AValue).CalendarId
  else
    Result := '';
end;

procedure ValidateTemporalLocaleCalendar(
  const ADateTimeFormat: TGocciaIntlDateTimeFormatValue;
  const AValue: TGocciaValue);
var
  ValueCalendar: string;
  AllowISOCalendar: Boolean;
begin
  ValueCalendar := '';
  AllowISOCalendar := False;

  if AValue is TGocciaTemporalPlainDateValue then
  begin
    ValueCalendar := TGocciaTemporalPlainDateValue(AValue).CalendarId;
    AllowISOCalendar := True;
  end
  else if AValue is TGocciaTemporalPlainDateTimeValue then
  begin
    ValueCalendar := TGocciaTemporalPlainDateTimeValue(AValue).CalendarId;
    AllowISOCalendar := True;
  end
  else if AValue is TGocciaTemporalPlainYearMonthValue then
  begin
    ValueCalendar := TGocciaTemporalPlainYearMonthValue(AValue).CalendarId;
    AllowISOCalendar := True;
  end
  else if AValue is TGocciaTemporalPlainMonthDayValue then
  begin
    ValueCalendar := TGocciaTemporalPlainMonthDayValue(AValue).CalendarId;
    AllowISOCalendar := True;
  end
  else if AValue is TGocciaTemporalZonedDateTimeValue then
  begin
    ValueCalendar := TGocciaTemporalZonedDateTimeValue(AValue).CalendarId;
    AllowISOCalendar := True;
  end;

  if not TemporalLocaleCalendarMatches(ValueCalendar, ADateTimeFormat.FCalendar,
    AllowISOCalendar) then
    ThrowRangeError('Temporal value calendar does not match the locale calendar');
end;

function FormatTemporalValueWithDateTimeFormat(
  const ADTF: TGocciaIntlDateTimeFormatValue; const AValue: TGocciaValue;
  const AAllowZonedDateTime: Boolean): TGocciaValue;
var
  Millis: Double;
  Formatted: string;
  EffectiveOptions: TIntlDateTimeFormatOptions;
  Input: TDateTimeFormattable;
  Parts: TIntlFormatPartArray;
begin
  EffectiveOptions := ADTF.FResolvedOptions;
  Input := ToDateTimeFormattable(AValue);
  if IsTemporalDateTimeKind(Input.Kind) then
  begin
    ValidateTemporalLocaleCalendar(ADTF, AValue);
    if (Input.Kind = dtfkZonedDateTime) and not AAllowZonedDateTime then
      ThrowTypeError('Intl.DateTimeFormat.prototype.format does not support Temporal.ZonedDateTime');
    if not FilterTemporalDateTimeOptions(ADTF.FResolvedOptions, Input.Kind,
      ADTF.FHasExplicitCoreOptions, EffectiveOptions) then
      ThrowTypeError('Intl.DateTimeFormat.prototype.format has no fields compatible with the Temporal value');
  if TryICUTemporalSurrogateToParts(ADTF.FLocale, Input, EffectiveOptions,
      Parts) then
    begin
      EnsureCopticEraPart(Parts, EffectiveOptions);
      AdjustIslamicEraParts(Parts, EffectiveOptions);
      Exit(TGocciaStringLiteralValue.Create(FormatPartArrayValue(Parts)));
    end;
    Millis := Input.Millis;
    if not IsTemporalPlainKind(Input.Kind) then
      Millis := TimeClipMillis(Millis);
  end
  else
    Millis := TimeClipMillis(Input.Millis);

  if IsNan(Millis) then
    ThrowRangeError('Invalid time value');

  if TryICUFormatDateTime(ADTF.FLocale, Millis, EffectiveOptions, Formatted) then
  begin
    Formatted := AdjustProlepticGregorianYearEra(Formatted, Millis,
      EffectiveOptions);
    Formatted := AdjustCopticEraFormatted(Formatted, EffectiveOptions);
    Formatted := AdjustIslamicEraFormatted(Formatted, EffectiveOptions);
    Result := TGocciaStringLiteralValue.Create(Formatted)
  end
  else
    Result := TGocciaStringLiteralValue.Create(FloatToStr(Millis));
end;

function FormatTemporalValueToLocaleString(const AValue, ALocales,
  AOptions: TGocciaValue): TGocciaValue;
var
  Locale: string;
  OptionsObj: TGocciaObjectValue;
  DTF: TGocciaIntlDateTimeFormatValue;
  ZDT: TGocciaTemporalZonedDateTimeValue;
  DateStyleOption, TimeStyleOption, TimeZoneOption: TGocciaValue;
  TimeZoneId: string;
begin
  Locale := DateTimeFormatLocaleArgumentToLocale(ALocales);
  OptionsObj := DateTimeFormatOptionsArgumentToObject(AOptions);

  if Assigned(OptionsObj) then
  begin
    if (AValue is TGocciaTemporalPlainDateValue) or
       (AValue is TGocciaTemporalPlainYearMonthValue) or
       (AValue is TGocciaTemporalPlainMonthDayValue) then
    begin
      TimeStyleOption := OptionsObj.GetProperty('timeStyle');
      if Assigned(TimeStyleOption) and
         not (TimeStyleOption is TGocciaUndefinedLiteralValue) then
        ThrowTypeError('Temporal date toLocaleString does not accept a timeStyle option');
    end
    else if AValue is TGocciaTemporalPlainTimeValue then
    begin
      DateStyleOption := OptionsObj.GetProperty('dateStyle');
      if Assigned(DateStyleOption) and
         not (DateStyleOption is TGocciaUndefinedLiteralValue) then
        ThrowTypeError('Temporal.PlainTime.prototype.toLocaleString does not accept a dateStyle option');
    end
    else if AValue is TGocciaTemporalZonedDateTimeValue then
    begin
      TimeZoneOption := OptionsObj.GetProperty('timeZone');
      if Assigned(TimeZoneOption) and
         not (TimeZoneOption is TGocciaUndefinedLiteralValue) then
        ThrowTypeError('Temporal.ZonedDateTime.prototype.toLocaleString does not accept a timeZone option');
    end;
  end;

  DTF := TGocciaIntlDateTimeFormatValue.Create(Locale, OptionsObj);
  ValidateTemporalLocaleCalendar(DTF, AValue);
  if AValue is TGocciaTemporalZonedDateTimeValue then
  begin
    ZDT := TGocciaTemporalZonedDateTimeValue(AValue);
    TimeZoneId := NormalizeDateTimeFormatTimeZone(ZDT.TimeZone);
    DTF.FTimeZone := TimeZoneId;
    DTF.FResolvedOptions.TimeZone := ICUDateTimeFormatTimeZone(TimeZoneId);
  end;

  Result := FormatTemporalValueWithDateTimeFormat(DTF, AValue, True);
end;

function DateTimeFormatHasResolvedHour(
  const ADateTimeFormat: TGocciaIntlDateTimeFormatValue): Boolean;
begin
  Result := (ADateTimeFormat.FHour <> '') or
    (ADateTimeFormat.FTimeStyle <> '');
end;

function DateTimeFormatResolvedHour12(
  const ADateTimeFormat: TGocciaIntlDateTimeFormatValue): Boolean;
begin
  Result := (ADateTimeFormat.FHourCycle = 'h11') or
    (ADateTimeFormat.FHourCycle = 'h12');
end;

{ TGocciaIntlDateTimeFormatValue }

procedure TGocciaIntlDateTimeFormatValue.ReadOptions(const AOptions: TGocciaObjectValue);
var
  V: TGocciaValue;
  CalendarOption, NumberingSystemOption: string;
  Ignored: string;
  TimeZoneOption: string;
begin
  if not Assigned(AOptions) then Exit;

  Ignored := ReadDateTimeStringOption(AOptions, 'localeMatcher',
    'lookup|best fit', '');
  if TryReadDateTimeStringOption(AOptions, 'calendar', CalendarOption) then
    TryResolveDateTimeCalendarOption(CalendarOption, 'calendar', FCalendar);
  if TryReadDateTimeStringOption(AOptions, 'numberingSystem',
    NumberingSystemOption) then
    TryResolveDateTimeNumberingSystemOption(NumberingSystemOption,
      'numberingSystem', FNumberingSystem);
  V := AOptions.GetProperty('hour12');
  if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
  begin
    if V.ToBooleanLiteral.Value then
      FHour12 := 1
    else
      FHour12 := 0;
  end;
  FHourCycle := ReadDateTimeStringOption(AOptions, 'hourCycle',
    'h11|h12|h23|h24', FHourCycle);
  if TryReadDateTimeStringOption(AOptions, 'timeZone', TimeZoneOption) then
  begin
    if TimeZoneOption = '' then
      ThrowRangeError(Format(SErrorUnknownTimezone, [TimeZoneOption]));
    FTimeZone := CanonicalizeDateTimeFormatTimeZone(TimeZoneOption);
  end;
  FWeekday := ReadDateTimeStringOption(AOptions, 'weekday',
    'narrow|short|long', FWeekday);
  FEra := ReadDateTimeStringOption(AOptions, 'era', 'narrow|short|long', FEra);
  FYear := ReadDateTimeStringOption(AOptions, 'year', '2-digit|numeric', FYear);
  FMonth := ReadDateTimeStringOption(AOptions, 'month',
    '2-digit|numeric|narrow|short|long', FMonth);
  FDay := ReadDateTimeStringOption(AOptions, 'day', '2-digit|numeric', FDay);
  FDayPeriod := ReadDateTimeStringOption(AOptions, 'dayPeriod',
    'narrow|short|long', FDayPeriod);
  FHour := ReadDateTimeStringOption(AOptions, 'hour', '2-digit|numeric', FHour);
  FMinute := ReadDateTimeStringOption(AOptions, 'minute',
    '2-digit|numeric', FMinute);
  FSecond := ReadDateTimeStringOption(AOptions, 'second',
    '2-digit|numeric', FSecond);
  TryReadDateTimeIntegerOption(AOptions, 'fractionalSecondDigits', 1, 3,
    FFractionalSecondDigits);
  FTimeZoneName := ReadDateTimeStringOption(AOptions, 'timeZoneName',
    'short|long|shortOffset|longOffset|shortGeneric|longGeneric',
    FTimeZoneName);
  Ignored := ReadDateTimeStringOption(AOptions, 'formatMatcher',
    'basic|best fit', '');
  FDateStyle := ReadDateTimeStringOption(AOptions, 'dateStyle',
    'full|long|medium|short', FDateStyle);
  FTimeStyle := ReadDateTimeStringOption(AOptions, 'timeStyle',
    'full|long|medium|short', FTimeStyle);
end;

constructor TGocciaIntlDateTimeFormatValue.Create(const ALocale: string; const AOptions: TGocciaObjectValue);
var
  Canonical: string;
begin
  inherited Create;
  Canonical := CanonicalizeUnicodeLocaleId(ALocale);
  if (Canonical <> '') and (Pos('-u-', LowerCase(ALocale)) > 0) and
     (Pos('-u-', LowerCase(Canonical)) = 0) then
    Canonical := ALocale;
  if Canonical = '' then
  begin
    Canonical := CanonicalizeUnicodeLocaleId(DefaultLocale);
    if Canonical = '' then
      Canonical := 'en';
  end;
  FLocale := Canonical;

  // Defaults
  FHour12 := -1;
  FFractionalSecondDigits := -1;
  FTimeZone := DefaultTimeZone;

  ReadOptions(AOptions);
  ResolveDateTimeFormatLocaleOptions(FLocale, FCalendar, FNumberingSystem,
    FHourCycle, FHour12);
  if ((FDateStyle <> '') or (FTimeStyle <> '')) and
     HasDateTimeStyleConflict(Self) then
    ThrowTypeError('dateStyle/timeStyle may not be used with component options');
  FHasExplicitCoreOptions := (FDateStyle <> '') or (FTimeStyle <> '') or
    (FWeekday <> '') or (FYear <> '') or (FMonth <> '') or (FDay <> '') or
    (FDayPeriod <> '') or (FHour <> '') or (FMinute <> '') or
    (FSecond <> '') or (FFractionalSecondDigits >= 0);

  if (FFractionalSecondDigits >= 0) and
     ((FFractionalSecondDigits < 1) or (FFractionalSecondDigits > 3)) then
    ThrowRangeError(Format(SErrorIntlDigitsOutOfRange,
      ['fractionalSecondDigits', 1, 3]));

  // ECMA-402: when no dateStyle/timeStyle and no component properties,
  // default to { year: "numeric", month: "numeric", day: "numeric" }
  if not FHasExplicitCoreOptions then
  begin
    FYear := 'numeric';
    FMonth := 'numeric';
    FDay := 'numeric';
  end;

  // Build resolved ICU options
  FResolvedOptions := DefaultDateTimeFormatOptions;
  FResolvedOptions.DateStyle := DateStyleStringToEnum(FDateStyle);
  FResolvedOptions.TimeStyle := DateStyleStringToEnum(FTimeStyle);
  FResolvedOptions.Calendar := FCalendar;
  FResolvedOptions.NumberingSystem := FNumberingSystem;
  FResolvedOptions.TimeZone := ICUDateTimeFormatTimeZone(FTimeZone);
  FResolvedOptions.Hour12 := FHour12;
  FResolvedOptions.HourCycle := FHourCycle;
  FResolvedOptions.Weekday := FWeekday;
  FResolvedOptions.Era := FEra;
  FResolvedOptions.Year := FYear;
  FResolvedOptions.Month := FMonth;
  FResolvedOptions.Day := FDay;
  FResolvedOptions.DayPeriod := FDayPeriod;
  FResolvedOptions.Hour := FHour;
  FResolvedOptions.Minute := FMinute;
  FResolvedOptions.Second := FSecond;
  FResolvedOptions.FractionalSecondDigits := FFractionalSecondDigits;
  FResolvedOptions.TimeZoneName := FTimeZoneName;

  InitializePrototype;
  if Assigned(GetIntlDateTimeFormatShared) then
    FPrototype := GetIntlDateTimeFormatShared.Prototype;
end;

function TGocciaIntlDateTimeFormatValue.ToStringTag: string;
begin
  Result := 'Object';
end;

procedure TGocciaIntlDateTimeFormatValue.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FBoundFormat) then
    FBoundFormat.MarkReferences;
end;

procedure TGocciaIntlDateTimeFormatValue.InitializePrototype;
var
  Members: TGocciaMemberCollection;
  Shared: TGocciaSharedPrototype;
begin
  if not Assigned(CurrentRealm) then Exit;
  if Assigned(GetIntlDateTimeFormatShared) then Exit;

  Shared := TGocciaSharedPrototype.Create(Self);
  CurrentRealm.SetOwnedSlot(GIntlDateTimeFormatSharedSlot, Shared);
  if Length(FPrototypeMembers) = 0 then
  begin
    Members := TGocciaMemberCollection.Create;
    try
      Members.AddAccessor('format', IntlDateTimeFormatFormatGetter, nil,
        [pfConfigurable]);
      Members.AddNamedMethod('formatToParts', IntlDateTimeFormatFormatToParts, 1,
        gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddNamedMethod('formatRange', IntlDateTimeFormatFormatRange, 2,
        gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddNamedMethod('formatRangeToParts', IntlDateTimeFormatFormatRangeToParts, 2,
        gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddNamedMethod('resolvedOptions', IntlDateTimeFormatResolvedOptions, 0,
        gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddSymbolDataProperty(
        TGocciaSymbolValue.WellKnownToStringTag,
        TGocciaStringLiteralValue.Create('Intl.DateTimeFormat'),
        [pfConfigurable]);
      FPrototypeMembers := Members.ToDefinitions;
    finally
      Members.Free;
    end;
  end;
  RegisterMemberDefinitions(Shared.Prototype, FPrototypeMembers);
end;

class procedure TGocciaIntlDateTimeFormatValue.ExposePrototype(const AConstructor: TGocciaObjectValue);
var
  Shared: TGocciaSharedPrototype;
begin
  Shared := GetIntlDateTimeFormatShared;
  if not Assigned(Shared) then
  begin
    TGocciaIntlDateTimeFormatValue.Create(DefaultLocale);
    Shared := GetIntlDateTimeFormatShared;
  end;
  if Assigned(Shared) then
    ExposeSharedPrototypeOnConstructor(Shared, AConstructor);
end;

function TGocciaIntlDateTimeFormatValue.IntlDateTimeFormatFormatGetter(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  DTF: TGocciaIntlDateTimeFormatValue;
begin
  DTF := AsDateTimeFormat(AThisValue, 'get Intl.DateTimeFormat.prototype.format');
  if not Assigned(DTF.FBoundFormat) then
    DTF.FBoundFormat := TGocciaIntlDateTimeFormatBoundFormatValue.Create(DTF);
  Result := DTF.FBoundFormat;
end;

function TGocciaIntlDateTimeFormatValue.IntlDateTimeFormatFormat(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  DTF: TGocciaIntlDateTimeFormatValue;
  Millis: Double;
  Formatted: string;
begin
  DTF := AsDateTimeFormat(AThisValue, 'Intl.DateTimeFormat.prototype.format');
  if AArgs.GetElement(0) is TGocciaUndefinedLiteralValue then
  begin
    Millis := CurrentEpochMilliseconds;
    if TryICUFormatDateTime(DTF.FLocale, Millis, DTF.FResolvedOptions, Formatted) then
    begin
      Formatted := AdjustProlepticGregorianYearEra(Formatted, Millis,
        DTF.FResolvedOptions);
      Formatted := AdjustCopticEraFormatted(Formatted, DTF.FResolvedOptions);
      Formatted := AdjustIslamicEraFormatted(Formatted, DTF.FResolvedOptions);
      Result := TGocciaStringLiteralValue.Create(Formatted)
    end
    else
      Result := TGocciaStringLiteralValue.Create(FloatToStr(Millis));
    Exit;
  end;

  Result := FormatTemporalValueWithDateTimeFormat(DTF, AArgs.GetElement(0), False);
end;

function TGocciaIntlDateTimeFormatValue.IntlDateTimeFormatFormatToParts(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  DTF: TGocciaIntlDateTimeFormatValue;
  Millis: Double;
  Parts: TIntlFormatPartArray;
  EffectiveOptions: TIntlDateTimeFormatOptions;
  Input: TDateTimeFormattable;
begin
  DTF := AsDateTimeFormat(AThisValue, 'Intl.DateTimeFormat.prototype.formatToParts');
  EffectiveOptions := DTF.FResolvedOptions;
  if AArgs.GetElement(0) is TGocciaUndefinedLiteralValue then
    Millis := CurrentEpochMilliseconds
  else
  begin
    Input := ToDateTimeFormattable(AArgs.GetElement(0));
    if IsTemporalDateTimeKind(Input.Kind) then
    begin
      ValidateTemporalLocaleCalendar(DTF, AArgs.GetElement(0));
      if Input.Kind = dtfkZonedDateTime then
        ThrowTypeError('Intl.DateTimeFormat.prototype.formatToParts does not support Temporal.ZonedDateTime');
      if not FilterTemporalDateTimeOptions(DTF.FResolvedOptions, Input.Kind,
        DTF.FHasExplicitCoreOptions, EffectiveOptions) then
        ThrowTypeError('Intl.DateTimeFormat.prototype.formatToParts has no fields compatible with the Temporal value');
      if TryICUTemporalSurrogateToParts(DTF.FLocale, Input, EffectiveOptions,
        Parts) then
      begin
        EnsureCopticEraPart(Parts, EffectiveOptions);
        AdjustIslamicEraParts(Parts, EffectiveOptions);
        Exit(FormatPartsToArray(Parts));
      end;
      Millis := Input.Millis;
      if not IsTemporalPlainKind(Input.Kind) then
        Millis := TimeClipMillis(Millis);
    end
    else
      Millis := TimeClipMillis(Input.Millis);

    if IsNan(Millis) then
      ThrowRangeError('Invalid time value');
  end;

  if TryICUFormatDateTimeToParts(DTF.FLocale, Millis, EffectiveOptions, Parts) then
  begin
    AdjustLunisolarLeapMonthParts(Parts, Millis, EffectiveOptions);
    EnsureCopticEraPart(Parts, EffectiveOptions);
    AdjustIslamicEraParts(Parts, EffectiveOptions);
    Result := FormatPartsToArray(Parts)
  end
  else
    Result := TGocciaArrayValue.Create;
end;

function TGocciaIntlDateTimeFormatValue.IntlDateTimeFormatFormatRange(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  DTF: TGocciaIntlDateTimeFormatValue;
  StartMillis, EndMillis: Double;
  StartFormatted, EndFormatted: string;
  Parts: TIntlFormatPartArray;
  EffectiveOptions: TIntlDateTimeFormatOptions;
  StartInput, EndInput: TDateTimeFormattable;
begin
  DTF := AsDateTimeFormat(AThisValue, 'Intl.DateTimeFormat.prototype.formatRange');
  if AArgs.Length < 2 then
    ThrowTypeError('Intl.DateTimeFormat.prototype.formatRange requires start and end dates');
  if (AArgs.GetElement(0) is TGocciaUndefinedLiteralValue) or
     (AArgs.GetElement(1) is TGocciaUndefinedLiteralValue) then
    ThrowTypeError('Intl.DateTimeFormat.prototype.formatRange requires defined start and end dates');
  StartInput := ToDateTimeFormattable(AArgs.GetElement(0));
  EndInput := ToDateTimeFormattable(AArgs.GetElement(1));
  if StartInput.Kind <> EndInput.Kind then
    ThrowTypeError('Intl.DateTimeFormat.prototype.formatRange requires matching date-time value kinds');
  if IsTemporalDateTimeKind(StartInput.Kind) and
     (TemporalDateTimeValueCalendar(AArgs.GetElement(0)) <>
      TemporalDateTimeValueCalendar(AArgs.GetElement(1))) then
    ThrowRangeError('Temporal values must use the same calendar');
  if IsTemporalDateTimeKind(StartInput.Kind) then
    ValidateTemporalLocaleCalendar(DTF, AArgs.GetElement(0));
  if StartInput.Kind = dtfkZonedDateTime then
    ThrowTypeError('Intl.DateTimeFormat.prototype.formatRange does not support Temporal.ZonedDateTime');

  EffectiveOptions := DTF.FResolvedOptions;
  if IsTemporalDateTimeKind(StartInput.Kind) then
  begin
    if not FilterTemporalDateTimeOptions(DTF.FResolvedOptions, StartInput.Kind,
      DTF.FHasExplicitCoreOptions, EffectiveOptions) then
      ThrowTypeError('Intl.DateTimeFormat.prototype.formatRange has no fields compatible with the Temporal value');
    StartMillis := StartInput.Millis;
    EndMillis := EndInput.Millis;
    if not IsTemporalPlainKind(StartInput.Kind) then
    begin
      StartMillis := TimeClipMillis(StartMillis);
      EndMillis := TimeClipMillis(EndMillis);
      if IsNan(StartMillis) or IsNan(EndMillis) then
        ThrowRangeError('Invalid time value');
    end;
  end
  else
  begin
    StartMillis := TimeClipMillis(StartInput.Millis);
    EndMillis := TimeClipMillis(EndInput.Millis);
    if IsNan(StartMillis) or IsNan(EndMillis) then
      ThrowRangeError('Invalid time value');
  end;

  if TryICUTemporalSurrogateRangeToParts(DTF.FLocale, StartInput, EndInput,
    EffectiveOptions, Parts) then
    Exit(TGocciaStringLiteralValue.Create(FormatPartArrayValue(Parts)));

  if TryICUFormatDateTimeRange(DTF.FLocale, StartMillis, EndMillis,
    EffectiveOptions, StartFormatted) then
    Exit(TGocciaStringLiteralValue.Create(StartFormatted));

  if not TryICUFormatDateTime(DTF.FLocale, StartMillis, EffectiveOptions, StartFormatted) then
    StartFormatted := FloatToStr(StartMillis);
  if not TryICUFormatDateTime(DTF.FLocale, EndMillis, EffectiveOptions, EndFormatted) then
    EndFormatted := FloatToStr(EndMillis);

  if StartFormatted = EndFormatted then
    Result := TGocciaStringLiteralValue.Create(StartFormatted)
  else
    Result := TGocciaStringLiteralValue.Create(StartFormatted + ' – ' + EndFormatted);
end;

function TGocciaIntlDateTimeFormatValue.IntlDateTimeFormatFormatRangeToParts(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  DTF: TGocciaIntlDateTimeFormatValue;
  StartMillis, EndMillis: Double;
  StartFormatted, EndFormatted: string;
  Parts, StartParts, EndParts: TIntlFormatPartArray;
  Index: Integer;
  EffectiveOptions: TIntlDateTimeFormatOptions;
  StartInput, EndInput: TDateTimeFormattable;
begin
  DTF := AsDateTimeFormat(AThisValue, 'Intl.DateTimeFormat.prototype.formatRangeToParts');
  if AArgs.Length < 2 then
    ThrowTypeError('Intl.DateTimeFormat.prototype.formatRangeToParts requires start and end dates');
  if (AArgs.GetElement(0) is TGocciaUndefinedLiteralValue) or
     (AArgs.GetElement(1) is TGocciaUndefinedLiteralValue) then
    ThrowTypeError('Intl.DateTimeFormat.prototype.formatRangeToParts requires defined start and end dates');
  StartInput := ToDateTimeFormattable(AArgs.GetElement(0));
  EndInput := ToDateTimeFormattable(AArgs.GetElement(1));
  if StartInput.Kind <> EndInput.Kind then
    ThrowTypeError('Intl.DateTimeFormat.prototype.formatRangeToParts requires matching date-time value kinds');
  if IsTemporalDateTimeKind(StartInput.Kind) and
     (TemporalDateTimeValueCalendar(AArgs.GetElement(0)) <>
      TemporalDateTimeValueCalendar(AArgs.GetElement(1))) then
    ThrowRangeError('Temporal values must use the same calendar');
  if IsTemporalDateTimeKind(StartInput.Kind) then
    ValidateTemporalLocaleCalendar(DTF, AArgs.GetElement(0));
  if StartInput.Kind = dtfkZonedDateTime then
    ThrowTypeError('Intl.DateTimeFormat.prototype.formatRangeToParts does not support Temporal.ZonedDateTime');

  EffectiveOptions := DTF.FResolvedOptions;
  if IsTemporalDateTimeKind(StartInput.Kind) then
  begin
    if not FilterTemporalDateTimeOptions(DTF.FResolvedOptions, StartInput.Kind,
      DTF.FHasExplicitCoreOptions, EffectiveOptions) then
      ThrowTypeError('Intl.DateTimeFormat.prototype.formatRangeToParts has no fields compatible with the Temporal value');
    StartMillis := StartInput.Millis;
    EndMillis := EndInput.Millis;
    if not IsTemporalPlainKind(StartInput.Kind) then
    begin
      StartMillis := TimeClipMillis(StartMillis);
      EndMillis := TimeClipMillis(EndMillis);
      if IsNan(StartMillis) or IsNan(EndMillis) then
        ThrowRangeError('Invalid time value');
    end;
  end
  else
  begin
    StartMillis := TimeClipMillis(StartInput.Millis);
    EndMillis := TimeClipMillis(EndInput.Millis);
    if IsNan(StartMillis) or IsNan(EndMillis) then
      ThrowRangeError('Invalid time value');
  end;

  if TryICUTemporalSurrogateRangeToParts(DTF.FLocale, StartInput, EndInput,
    EffectiveOptions, Parts) then
    Exit(FormatPartsToArray(Parts));

  if TryICUFormatDateTimeRangeToParts(DTF.FLocale, StartMillis, EndMillis,
    EffectiveOptions, Parts) then
    Exit(FormatPartsToArray(Parts));

  if not TryICUFormatDateTime(DTF.FLocale, StartMillis, EffectiveOptions, StartFormatted) then
    StartFormatted := FloatToStr(StartMillis);
  if not TryICUFormatDateTime(DTF.FLocale, EndMillis, EffectiveOptions, EndFormatted) then
    EndFormatted := FloatToStr(EndMillis);

  if StartFormatted = EndFormatted then
  begin
    TryGetDatePartsWithFallback(DTF.FLocale, StartMillis, EffectiveOptions,
      'shared', Parts);
  end
  else
  begin
    TryGetDatePartsWithFallback(DTF.FLocale, StartMillis, EffectiveOptions,
      'startRange', StartParts);
    TryGetDatePartsWithFallback(DTF.FLocale, EndMillis, EffectiveOptions,
      'endRange', EndParts);
    SetLength(Parts, 0);
    AppendFormatPartArray(Parts, StartParts);
    SetLength(Parts, Length(Parts) + 1);
    Index := Length(Parts) - 1;
    Parts[Index].PartType := 'literal';
    Parts[Index].Value := ' – ';
    Parts[Index].Source := 'shared';
    AppendFormatPartArray(Parts, EndParts);
  end;

  Result := FormatPartsToArray(Parts);
end;

function TGocciaIntlDateTimeFormatValue.IntlDateTimeFormatResolvedOptions(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  DTF: TGocciaIntlDateTimeFormatValue;
  Obj: TGocciaObjectValue;

  procedure AddStringOption(const AName, AValue: string);
  begin
    Obj.CreateDataPropertyOrThrow(AName, TGocciaStringLiteralValue.Create(AValue));
  end;

begin
  DTF := AsDateTimeFormat(AThisValue, 'Intl.DateTimeFormat.prototype.resolvedOptions');
  Obj := TGocciaObjectValue.Create(TGocciaObjectValue.SharedObjectPrototype);
  AddStringOption('locale', DTF.FLocale);
  AddStringOption('calendar', DTF.FCalendar);
  AddStringOption('numberingSystem', DTF.FNumberingSystem);
  AddStringOption('timeZone', DTF.FTimeZone);
  if DateTimeFormatHasResolvedHour(DTF) and (DTF.FHourCycle <> '') then
  begin
    AddStringOption('hourCycle', DTF.FHourCycle);
    Obj.CreateDataPropertyOrThrow('hour12',
      TGocciaBooleanLiteralValue.Create(DateTimeFormatResolvedHour12(DTF)));
  end;
  if (DTF.FDateStyle = '') and (DTF.FTimeStyle = '') then
  begin
    if DTF.FWeekday <> '' then
      AddStringOption('weekday', DTF.FWeekday);
    if DTF.FEra <> '' then
      AddStringOption('era', DTF.FEra);
    if DTF.FYear <> '' then
      AddStringOption('year', DTF.FYear);
    if DTF.FMonth <> '' then
      AddStringOption('month', DTF.FMonth);
    if DTF.FDay <> '' then
      AddStringOption('day', DTF.FDay);
    if DTF.FDayPeriod <> '' then
      AddStringOption('dayPeriod', DTF.FDayPeriod);
    if DTF.FHour <> '' then
      AddStringOption('hour', DTF.FHour);
    if DTF.FMinute <> '' then
      AddStringOption('minute', DTF.FMinute);
    if DTF.FSecond <> '' then
      AddStringOption('second', DTF.FSecond);
    if DTF.FFractionalSecondDigits >= 0 then
      Obj.CreateDataPropertyOrThrow('fractionalSecondDigits',
        TGocciaNumberLiteralValue.Create(DTF.FFractionalSecondDigits));
    if DTF.FTimeZoneName <> '' then
      AddStringOption('timeZoneName', DTF.FTimeZoneName);
  end;
  if DTF.FDateStyle <> '' then
    AddStringOption('dateStyle', DTF.FDateStyle);
  if DTF.FTimeStyle <> '' then
    AddStringOption('timeStyle', DTF.FTimeStyle);
  Result := Obj;
end;

initialization
  GIntlDateTimeFormatSharedSlot := RegisterRealmOwnedSlot('Intl.DateTimeFormat.shared');

end.
