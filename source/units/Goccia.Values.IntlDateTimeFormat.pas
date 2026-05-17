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

    procedure InitializePrototype;
    procedure ReadOptions(const AOptions: TGocciaObjectValue);
  public
    constructor Create(const ALocale: string; const AOptions: TGocciaObjectValue = nil);

    function ToStringTag: string; override;
    class procedure ExposePrototype(const AConstructor: TGocciaObjectValue);
  published
    function IntlDateTimeFormatFormat(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function IntlDateTimeFormatFormatToParts(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function IntlDateTimeFormatFormatRange(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function IntlDateTimeFormatFormatRangeToParts(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function IntlDateTimeFormatResolvedOptions(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  end;

implementation

uses
  Math,
  SysUtils,

  IntlICU,
  IntlLocaleResolver,

  Goccia.Error.Messages,
  Goccia.Intl.Helpers,
  Goccia.ObjectModel.Types,
  Goccia.Realm,
  Goccia.Temporal.Utils,
  Goccia.Values.ArrayValue,
  Goccia.Values.ErrorHelper,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.SymbolValue,
  Goccia.Values.TemporalInstant,
  Goccia.Values.TemporalPlainDate,
  Goccia.Values.TemporalPlainDateTime,
  Goccia.Values.TemporalPlainMonthDay,
  Goccia.Values.TemporalPlainTime,
  Goccia.Values.TemporalPlainYearMonth,
  Goccia.Values.TemporalZonedDateTime;

var
  GIntlDateTimeFormatSharedSlot: TGocciaRealmOwnedSlotId;

threadvar
  FPrototypeMembers: TArray<TGocciaMemberDefinition>;

type
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
end;

function ICUDateTimeFormatTimeZone(const ATimeZone: string): string;
begin
  Result := ATimeZone;
  if (ATimeZone <> '') and ((ATimeZone[1] = '+') or (ATimeZone[1] = '-')) then
    Result := 'GMT' + ATimeZone;
end;

function AsDateTimeFormat(const AValue: TGocciaValue; const AMethod: string): TGocciaIntlDateTimeFormatValue;
begin
  if not (AValue is TGocciaIntlDateTimeFormatValue) then
    ThrowTypeError(AMethod + ' called on non-DateTimeFormat');
  Result := TGocciaIntlDateTimeFormatValue(AValue);
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
  if Result = 0 then
    Result := 0;
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
  Era: string;
begin
  Era := AOptions.Era;
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

  if AEffectiveOptions.DateStyle <> idtsNone then
    ExpandDateStyle(AEffectiveOptions);
  if AEffectiveOptions.TimeStyle <> idtsNone then
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

function Pad2(const AValue: Integer): string;
begin
  if (AValue >= 0) and (AValue < 10) then
    Result := '0' + IntToStr(AValue)
  else
    Result := IntToStr(AValue);
end;

function OptionIntegerString(const AValue: Integer; const AOption: string): string;
begin
  if AOption = '2-digit' then
    Result := Pad2(AValue)
  else
    Result := IntToStr(AValue);
end;

function ISOWeekdayName(const AYear, AMonth, ADay: Integer): string;
begin
  case DayOfWeek(AYear, AMonth, ADay) of
    1: Result := 'Monday';
    2: Result := 'Tuesday';
    3: Result := 'Wednesday';
    4: Result := 'Thursday';
    5: Result := 'Friday';
    6: Result := 'Saturday';
  else
    Result := 'Sunday';
  end;
end;

procedure AppendManualPart(var AParts: TIntlFormatPartArray; const AType,
  AValue, ASource: string);
var
  Index: Integer;
begin
  if AValue = '' then
    Exit;
  SetLength(AParts, Length(AParts) + 1);
  Index := Length(AParts) - 1;
  AParts[Index].PartType := AType;
  AParts[Index].Value := AValue;
  AParts[Index].Source := ASource;
end;

procedure AppendManualLiteral(var AParts: TIntlFormatPartArray; const AValue,
  ASource: string);
begin
  AppendManualPart(AParts, 'literal', AValue, ASource);
end;

procedure BuildManualTemporalParts(const AInput: TDateTimeFormattable;
  const AOptions: TIntlDateTimeFormatOptions; const ASource: string;
  var AParts: TIntlFormatPartArray);
var
  HasPreviousDatePart, HasDate, HasTime: Boolean;
begin
  HasPreviousDatePart := False;
  HasDate := (AOptions.Weekday <> '') or (AOptions.Year <> '') or
    (AOptions.Month <> '') or (AOptions.Day <> '');
  HasTime := (AOptions.Hour <> '') or (AOptions.Minute <> '') or
    (AOptions.Second <> '');

  if AOptions.Weekday <> '' then
  begin
    AppendManualPart(AParts, 'weekday',
      ISOWeekdayName(AInput.Year, AInput.Month, AInput.Day), ASource);
    HasPreviousDatePart := True;
  end;
  if AOptions.Month <> '' then
  begin
    if HasPreviousDatePart then
      AppendManualLiteral(AParts, ', ', ASource);
    AppendManualPart(AParts, 'month',
      OptionIntegerString(AInput.Month, AOptions.Month), ASource);
    HasPreviousDatePart := True;
  end;
  if AOptions.Day <> '' then
  begin
    if HasPreviousDatePart then
      AppendManualLiteral(AParts, '/', ASource);
    AppendManualPart(AParts, 'day', OptionIntegerString(AInput.Day,
      AOptions.Day), ASource);
    HasPreviousDatePart := True;
  end;
  if AOptions.Year <> '' then
  begin
    if HasPreviousDatePart then
      AppendManualLiteral(AParts, '/', ASource);
    AppendManualPart(AParts, 'year', IntToStr(AInput.Year), ASource);
    HasPreviousDatePart := True;
  end;

  if HasDate and HasTime then
    AppendManualLiteral(AParts, ', ', ASource);
  if AOptions.Hour <> '' then
    AppendManualPart(AParts, 'hour', OptionIntegerString(AInput.Hour,
      AOptions.Hour), ASource);
  if AOptions.Minute <> '' then
  begin
    if AOptions.Hour <> '' then
      AppendManualLiteral(AParts, ':', ASource);
    AppendManualPart(AParts, 'minute', OptionIntegerString(AInput.Minute,
      AOptions.Minute), ASource);
  end;
  if AOptions.Second <> '' then
  begin
    if (AOptions.Hour <> '') or (AOptions.Minute <> '') then
      AppendManualLiteral(AParts, ':', ASource);
    AppendManualPart(AParts, 'second', OptionIntegerString(AInput.Second,
      AOptions.Second), ASource);
  end;
end;

function NeedsManualTemporalRange(const AStartInput,
  AEndInput: TDateTimeFormattable): Boolean;
begin
  Result := IsTemporalPlainKind(AStartInput.Kind) and
    ((Abs(AStartInput.Year) > 9999) or (Abs(AEndInput.Year) > 9999));
end;

function TryManualTemporalRangeToParts(const AStartInput,
  AEndInput: TDateTimeFormattable; const AOptions: TIntlDateTimeFormatOptions;
  out AParts: TIntlFormatPartArray): Boolean;
begin
  Result := NeedsManualTemporalRange(AStartInput, AEndInput);
  if not Result then
    Exit;

  SetLength(AParts, 0);
  BuildManualTemporalParts(AStartInput, AOptions, 'startRange', AParts);
  AppendManualLiteral(AParts, ' – ', 'shared');
  BuildManualTemporalParts(AEndInput, AOptions, 'endRange', AParts);
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

procedure AppendDatePartArray(var ADest: TIntlFormatPartArray;
  const ASrc: TIntlFormatPartArray);
var
  I, Index: Integer;
begin
  for I := 0 to Length(ASrc) - 1 do
  begin
    SetLength(ADest, Length(ADest) + 1);
    Index := Length(ADest) - 1;
    ADest[Index] := ASrc[I];
  end;
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

{ TGocciaIntlDateTimeFormatValue }

procedure TGocciaIntlDateTimeFormatValue.ReadOptions(const AOptions: TGocciaObjectValue);
var
  V: TGocciaValue;
  Ignored: string;
begin
  if not Assigned(AOptions) then Exit;

  ReadValidatedStringOption(AOptions, 'localeMatcher', Ignored);
  ReadValidatedStringOption(AOptions, 'formatMatcher', Ignored);
  TryReadStringOption(AOptions, 'dateStyle', FDateStyle);
  TryReadStringOption(AOptions, 'timeStyle', FTimeStyle);
  TryReadStringOption(AOptions, 'calendar', FCalendar);
  TryReadStringOption(AOptions, 'numberingSystem', FNumberingSystem);
  ReadValidatedStringOption(AOptions, 'timeZone', FTimeZone);
  FTimeZone := NormalizeDateTimeFormatTimeZone(FTimeZone);
  V := AOptions.GetProperty('hour12');
  if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
  begin
    if V.ToBooleanLiteral.Value then
      FHour12 := 1
    else
      FHour12 := 0;
  end;
  ReadValidatedStringOption(AOptions, 'hourCycle', FHourCycle);
  ReadValidatedStringOption(AOptions, 'weekday', FWeekday);
  ReadValidatedStringOption(AOptions, 'era', FEra);
  ReadValidatedStringOption(AOptions, 'year', FYear);
  ReadValidatedStringOption(AOptions, 'month', FMonth);
  ReadValidatedStringOption(AOptions, 'day', FDay);
  TryReadStringOption(AOptions, 'dayPeriod', FDayPeriod);
  ReadValidatedStringOption(AOptions, 'hour', FHour);
  ReadValidatedStringOption(AOptions, 'minute', FMinute);
  ReadValidatedStringOption(AOptions, 'second', FSecond);
  V := AOptions.GetProperty('fractionalSecondDigits');
  if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
    FFractionalSecondDigits := Trunc(V.ToNumberLiteral.Value);
  TryReadStringOption(AOptions, 'timeZoneName', FTimeZoneName);
end;

constructor TGocciaIntlDateTimeFormatValue.Create(const ALocale: string; const AOptions: TGocciaObjectValue);
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
  FHour12 := -1;
  FFractionalSecondDigits := -1;
  FTimeZone := DefaultTimeZone;

  ReadOptions(AOptions);
  FHasExplicitCoreOptions := (FDateStyle <> '') or (FTimeStyle <> '') or
    (FWeekday <> '') or (FYear <> '') or (FMonth <> '') or (FDay <> '') or
    (FDayPeriod <> '') or (FHour <> '') or (FMinute <> '') or
    (FSecond <> '') or (FFractionalSecondDigits >= 0);

  // Default calendar and numberingSystem
  if FCalendar = '' then
    FCalendar := 'gregory';
  if FNumberingSystem = '' then
    FNumberingSystem := 'latn';
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
  Result := 'Intl.DateTimeFormat';
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
      Members.AddNamedMethod('format', IntlDateTimeFormatFormat, 1,
        gmkPrototypeMethod, [gmfNoFunctionPrototype]);
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

function TGocciaIntlDateTimeFormatValue.IntlDateTimeFormatFormat(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  DTF: TGocciaIntlDateTimeFormatValue;
  Millis: Double;
  Formatted: string;
begin
  DTF := AsDateTimeFormat(AThisValue, 'Intl.DateTimeFormat.prototype.format');
  if AArgs.Length < 1 then
    ThrowTypeError('Intl.DateTimeFormat.prototype.format requires a date value');
  Millis := TimeClipMillis(AArgs.GetElement(0).ToNumberLiteral.Value);
  if IsNan(Millis) then
    ThrowRangeError('Invalid time value');

  if TryICUFormatDateTime(DTF.FLocale, Millis, DTF.FResolvedOptions, Formatted) then
    Result := TGocciaStringLiteralValue.Create(Formatted)
  else
    Result := TGocciaStringLiteralValue.Create(FloatToStr(Millis));
end;

function TGocciaIntlDateTimeFormatValue.IntlDateTimeFormatFormatToParts(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  DTF: TGocciaIntlDateTimeFormatValue;
  Millis: Double;
  Parts: TIntlFormatPartArray;
begin
  DTF := AsDateTimeFormat(AThisValue, 'Intl.DateTimeFormat.prototype.formatToParts');
  if AArgs.Length < 1 then
    ThrowTypeError('Intl.DateTimeFormat.prototype.formatToParts requires a date value');
  Millis := TimeClipMillis(AArgs.GetElement(0).ToNumberLiteral.Value);
  if IsNan(Millis) then
    ThrowRangeError('Invalid time value');

  if TryICUFormatDateTimeToParts(DTF.FLocale, Millis, DTF.FResolvedOptions, Parts) then
    Result := FormatPartsToArray(Parts)
  else
    Result := TGocciaArrayValue.Create;
end;

function TGocciaIntlDateTimeFormatValue.IntlDateTimeFormatFormatRange(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  DTF: TGocciaIntlDateTimeFormatValue;
  StartMillis, EndMillis: Double;
  StartFormatted, EndFormatted: string;
  ManualParts: TIntlFormatPartArray;
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

  if TryManualTemporalRangeToParts(StartInput, EndInput, EffectiveOptions,
    ManualParts) then
    Exit(TGocciaStringLiteralValue.Create(FormatPartArrayValue(ManualParts)));

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

  if TryManualTemporalRangeToParts(StartInput, EndInput, EffectiveOptions,
    Parts) then
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
    AppendDatePartArray(Parts, StartParts);
    SetLength(Parts, Length(Parts) + 1);
    Index := Length(Parts) - 1;
    Parts[Index].PartType := 'literal';
    Parts[Index].Value := ' – ';
    Parts[Index].Source := 'shared';
    AppendDatePartArray(Parts, EndParts);
  end;

  Result := FormatPartsToArray(Parts);
end;

function TGocciaIntlDateTimeFormatValue.IntlDateTimeFormatResolvedOptions(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  DTF: TGocciaIntlDateTimeFormatValue;
  Obj: TGocciaObjectValue;
begin
  DTF := AsDateTimeFormat(AThisValue, 'Intl.DateTimeFormat.prototype.resolvedOptions');
  Obj := TGocciaObjectValue.Create(TGocciaObjectValue.SharedObjectPrototype);
  Obj.AssignProperty('locale', TGocciaStringLiteralValue.Create(DTF.FLocale));
  Obj.AssignProperty('calendar', TGocciaStringLiteralValue.Create(DTF.FCalendar));
  Obj.AssignProperty('numberingSystem', TGocciaStringLiteralValue.Create(DTF.FNumberingSystem));
  Obj.AssignProperty('timeZone', TGocciaStringLiteralValue.Create(DTF.FTimeZone));
  if DTF.FHour12 >= 0 then
    Obj.AssignProperty('hour12', TGocciaBooleanLiteralValue.Create(DTF.FHour12 = 1));
  if DTF.FHourCycle <> '' then
    Obj.AssignProperty('hourCycle', TGocciaStringLiteralValue.Create(DTF.FHourCycle));
  if DTF.FDateStyle <> '' then
    Obj.AssignProperty('dateStyle', TGocciaStringLiteralValue.Create(DTF.FDateStyle));
  if DTF.FTimeStyle <> '' then
    Obj.AssignProperty('timeStyle', TGocciaStringLiteralValue.Create(DTF.FTimeStyle));
  if (DTF.FDateStyle = '') and (DTF.FTimeStyle = '') then
  begin
    if DTF.FWeekday <> '' then
      Obj.AssignProperty('weekday', TGocciaStringLiteralValue.Create(DTF.FWeekday));
    if DTF.FEra <> '' then
      Obj.AssignProperty('era', TGocciaStringLiteralValue.Create(DTF.FEra));
    if DTF.FYear <> '' then
      Obj.AssignProperty('year', TGocciaStringLiteralValue.Create(DTF.FYear));
    if DTF.FMonth <> '' then
      Obj.AssignProperty('month', TGocciaStringLiteralValue.Create(DTF.FMonth));
    if DTF.FDay <> '' then
      Obj.AssignProperty('day', TGocciaStringLiteralValue.Create(DTF.FDay));
    if DTF.FDayPeriod <> '' then
      Obj.AssignProperty('dayPeriod', TGocciaStringLiteralValue.Create(DTF.FDayPeriod));
    if DTF.FHour <> '' then
      Obj.AssignProperty('hour', TGocciaStringLiteralValue.Create(DTF.FHour));
    if DTF.FMinute <> '' then
      Obj.AssignProperty('minute', TGocciaStringLiteralValue.Create(DTF.FMinute));
    if DTF.FSecond <> '' then
      Obj.AssignProperty('second', TGocciaStringLiteralValue.Create(DTF.FSecond));
    if DTF.FFractionalSecondDigits >= 0 then
      Obj.AssignProperty('fractionalSecondDigits',
        TGocciaNumberLiteralValue.Create(DTF.FFractionalSecondDigits));
    if DTF.FTimeZoneName <> '' then
      Obj.AssignProperty('timeZoneName', TGocciaStringLiteralValue.Create(DTF.FTimeZoneName));
  end;
  Result := Obj;
end;

initialization
  GIntlDateTimeFormatSharedSlot := RegisterRealmOwnedSlot('Intl.DateTimeFormat.shared');

end.
