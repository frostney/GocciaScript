unit Goccia.Values.TemporalZonedDateTime;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.ObjectModel,
  Goccia.SharedPrototype,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaTemporalZonedDateTimeValue = class(TGocciaObjectValue)
  private
    FEpochMilliseconds: Int64;
    FSubMillisecondNanoseconds: Integer;
    FTimeZone: string;
    FCalendarId: string;

    procedure InitializePrototype;
  published
    function GetCalendarId(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetTimeZoneId(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetEra(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetEraYear(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetYear(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetMonth(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetMonthCode(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetDay(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetDayOfWeek(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetDayOfYear(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetWeekOfYear(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetYearOfWeek(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetDaysInWeek(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetDaysInMonth(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetDaysInYear(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetMonthsInYear(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetInLeapYear(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetHoursInDay(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetHour(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetMinute(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetSecond(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetMillisecond(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetMicrosecond(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetNanosecond(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetOffset(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetOffsetNanoseconds(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetEpochMilliseconds(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetEpochNanoseconds(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;

    function ZonedDateTimeWith(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ZonedDateTimeWithPlainTime(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ZonedDateTimeWithTimeZone(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ZonedDateTimeWithCalendar(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ZonedDateTimeAdd(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ZonedDateTimeSubtract(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ZonedDateTimeUntil(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ZonedDateTimeSince(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ZonedDateTimeRound(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ZonedDateTimeEquals(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ZonedDateTimeGetTimeZoneTransition(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ZonedDateTimeStartOfDay(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ZonedDateTimeToInstant(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ZonedDateTimeToPlainDate(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ZonedDateTimeToPlainTime(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ZonedDateTimeToPlainDateTime(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ZonedDateTimeToString(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ZonedDateTimeToJSON(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ZonedDateTimeValueOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ZonedDateTimeToLocaleString(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  public
    constructor Create(const AEpochMilliseconds: Int64; const ASubMillisecondNanoseconds: Integer;
      const ATimeZone: string; const ACalendarId: string = 'iso8601'); overload;

    function ToStringTag: string; override;
    class procedure ExposePrototype(const AConstructor: TGocciaObjectValue);

    property EpochMilliseconds: Int64 read FEpochMilliseconds;
    property SubMillisecondNanoseconds: Integer read FSubMillisecondNanoseconds;
    property TimeZone: string read FTimeZone;
    property CalendarId: string read FCalendarId;
  end;

function CoerceTemporalZonedDateTime(const AValue: TGocciaValue;
  const AMethod: string; const AOptions: TGocciaValue = nil): TGocciaTemporalZonedDateTimeValue;
function CanonicalizeTemporalTimeZoneIdentifier(const AValue: string): string;
function TryParseTemporalOffsetString(const AValue: string; out AOffsetSeconds: Integer;
  out AHasUTCDesignator, AHasSubMinuteSyntax: Boolean): Boolean;

implementation

uses
  SysUtils,

  BigInteger,

  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.Realm,
  Goccia.Temporal.Calendar,
  Goccia.Temporal.DurationMath,
  Goccia.Temporal.Options,
  Goccia.Temporal.TimeZone,
  Goccia.Temporal.Utils,
  Goccia.Utils,
  Goccia.Values.BigIntValue,
  Goccia.Values.ErrorHelper,
  Goccia.Values.IntlDateTimeFormat,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.SymbolValue,
  Goccia.Values.TemporalDuration,
  Goccia.Values.TemporalInstant,
  Goccia.Values.TemporalPlainDate,
  Goccia.Values.TemporalPlainDateTime,
  Goccia.Values.TemporalPlainMonthDay,
  Goccia.Values.TemporalPlainTime,
  Goccia.Values.TemporalPlainYearMonth;

var
  GTemporalZonedDateTimeSharedSlot: TGocciaRealmOwnedSlotId;

function GetTemporalZonedDateTimeShared: TGocciaSharedPrototype;
{$IFDEF FPC}inline;{$ENDIF}
begin
  if (CurrentRealm <> nil) then
    Result := TGocciaSharedPrototype(CurrentRealm.GetOwnedSlot(GTemporalZonedDateTimeSharedSlot))
  else
    Result := nil;
end;

const
  MILLISECONDS_PER_SECOND = 1000;
  MILLISECONDS_PER_MINUTE = 60000;
  MILLISECONDS_PER_HOUR = 3600000;
  MILLISECONDS_PER_DAY = Int64(86400000);
  SUB_MS_NANOSECOND_LIMIT = 1000000;
  HOURS_PER_DAY = 24;
  MONTHS_PER_YEAR = 12;
  DAYS_PER_WEEK = 7;
  PROPERTY_CALENDAR = 'calendar';
  PROPERTY_DAY = 'day';
  PROPERTY_HOUR = 'hour';
  PROPERTY_MICROSECOND = 'microsecond';
  PROPERTY_MILLISECOND = 'millisecond';
  PROPERTY_MINUTE = 'minute';
  PROPERTY_MONTH = 'month';
  PROPERTY_MONTH_CODE = 'monthCode';
  PROPERTY_NANOSECOND = 'nanosecond';
  PROPERTY_TIME_ZONE = 'timeZone';
  PROPERTY_OFFSET = 'offset';
  PROPERTY_SECOND = 'second';
  PROPERTY_YEAR = 'year';

type
  TTemporalDisambiguationOption = (tdoCompatible, tdoEarlier, tdoLater, tdoReject);
  TTemporalOffsetOption = (tooPrefer, tooUse, tooIgnore, tooReject);
  TZonedDateTimeParseResult = record
    Date: TTemporalDateRecord;
    Time: TTemporalTimeRecord;
    TimeZone: string;
    OffsetSeconds: Integer;
    HasOffset: Boolean;
    HasExplicitTime: Boolean;
    HasUTCDesignator: Boolean;
    OffsetHasSubMinuteSyntax: Boolean;
    TimeZoneFromOffset: Boolean;
    CalendarId: string;
  end;

function AsZonedDateTime(const AValue: TGocciaValue; const AMethod: string): TGocciaTemporalZonedDateTimeValue;
begin
  if not (AValue is TGocciaTemporalZonedDateTimeValue) then
    ThrowTypeError(AMethod + ' called on non-ZonedDateTime', SSuggestTemporalThisType);
  Result := TGocciaTemporalZonedDateTimeValue(AValue);
end;

function LocalToEpochMs(const AYear, AMonth, ADay, AHour, AMinute, ASecond, AMs: Integer;
  const ATimeZone: string): Int64; forward;
function FormatOffsetString(const AOffsetSeconds: Integer): string; forward;
procedure SplitEpochNanoseconds(const AEpochNanoseconds: TBigInteger;
  out AEpochMilliseconds: Int64; out ASubMillisecondNanoseconds: Integer); forward;

function EpochMillisecondsToSeconds(const AEpochMilliseconds: Int64): Int64;
{$IFDEF FPC}inline;{$ENDIF}
begin
  Result := AEpochMilliseconds div MILLISECONDS_PER_SECOND;
  if (AEpochMilliseconds < 0) and
     (AEpochMilliseconds mod MILLISECONDS_PER_SECOND <> 0) then
    Dec(Result);
end;

function IsUndefinedValue(const AValue: TGocciaValue): Boolean;
{$IFDEF FPC}inline;{$ENDIF}
begin
  Result := (AValue = nil) or (AValue is TGocciaUndefinedLiteralValue);
end;

function ASCIIEqualsIgnoreCase(const ALeft, ARight: string): Boolean;
var
  I: Integer;
  LeftChar, RightChar: Char;

  function UpperASCII(const AChar: Char): Char;
  {$IFDEF FPC}inline;{$ENDIF}
  begin
    if (AChar >= 'a') and (AChar <= 'z') then
      Result := Chr(Ord(AChar) - Ord('a') + Ord('A'))
    else
      Result := AChar;
  end;
begin
  if Length(ALeft) <> Length(ARight) then
    Exit(False);

  for I := 1 to Length(ALeft) do
  begin
    LeftChar := UpperASCII(ALeft[I]);
    RightChar := UpperASCII(ARight[I]);
    if LeftChar <> RightChar then
      Exit(False);
  end;

  Result := True;
end;

function ASCIIIsLowercaseKey(const AValue: string): Boolean;
var
  I: Integer;
  C: Char;
begin
  Result := AValue <> '';
  for I := 1 to Length(AValue) do
  begin
    C := AValue[I];
    if (C >= 'A') and (C <= 'Z') then
      Exit(False);
  end;
end;

function RequireOptionsObject(const AOptions: TGocciaValue; const AMethod: string): TGocciaObjectValue;
begin
  if IsUndefinedValue(AOptions) then
    Exit(nil);
  if not (AOptions is TGocciaObjectValue) then
    ThrowTypeError(AMethod + ' options must be an object', SSuggestTemporalFromArg);
  Result := TGocciaObjectValue(AOptions);
end;

function GetDisambiguationOption(const AOptions: TGocciaObjectValue;
  const AMethod: string): TTemporalDisambiguationOption;
var
  Value: string;
begin
  Value := GetOptionString(AOptions, 'disambiguation', 'compatible');
  if Value = 'compatible' then
    Result := tdoCompatible
  else if Value = 'earlier' then
    Result := tdoEarlier
  else if Value = 'later' then
    Result := tdoLater
  else if Value = 'reject' then
    Result := tdoReject
  else
    ThrowRangeError(AMethod + ' invalid disambiguation option: ' + Value,
      SSuggestTemporalRoundArg);
end;

function GetOffsetOption(const AOptions: TGocciaObjectValue;
  const AMethod: string): TTemporalOffsetOption;
var
  Value: string;
begin
  Value := GetOptionString(AOptions, 'offset', 'reject');
  if Value = 'prefer' then
    Result := tooPrefer
  else if Value = 'use' then
    Result := tooUse
  else if Value = 'ignore' then
    Result := tooIgnore
  else if Value = 'reject' then
    Result := tooReject
  else
    ThrowRangeError(AMethod + ' invalid offset option: ' + Value,
      SSuggestTemporalRoundArg);
end;

procedure ReadZonedDateTimeOptions(const AOptions: TGocciaValue; const AMethod: string;
  out AOverflow: TTemporalOverflow; out AOffsetOption: TTemporalOffsetOption;
  out ADisambiguation: TTemporalDisambiguationOption);
var
  OptionsObj: TGocciaObjectValue;
begin
  OptionsObj := RequireOptionsObject(AOptions, AMethod);
  ADisambiguation := GetDisambiguationOption(OptionsObj, AMethod);
  AOffsetOption := GetOffsetOption(OptionsObj, AMethod);
  AOverflow := GetOverflowOption(OptionsObj);
end;

function ToTimeZoneDisambiguation(
  const ADisambiguation: TTemporalDisambiguationOption): TTemporalTimeZoneDisambiguation;
begin
  case ADisambiguation of
    tdoEarlier: Result := ttzdEarlier;
    tdoLater: Result := ttzdLater;
    tdoReject: Result := ttzdReject;
  else
    Result := ttzdCompatible;
  end;
end;

function NormalizeLeapSecondForTemporalDateTime(const AValue: string): string;
var
  SecondsSeparator: Integer;
  TimeSeparator: Integer;
begin
  Result := AValue;
  TimeSeparator := Pos('T', Result);
  if TimeSeparator = 0 then
    TimeSeparator := Pos('t', Result);
  if TimeSeparator = 0 then
    Exit;

  SecondsSeparator := TimeSeparator + 6;
  if (Length(Result) >= SecondsSeparator + 2) and
     (Result[TimeSeparator + 3] = ':') and
     (Result[SecondsSeparator] = ':') and
     (Result[SecondsSeparator + 1] = '6') and
     (Result[SecondsSeparator + 2] = '0') then
  begin
    Result[SecondsSeparator + 1] := '5';
    Result[SecondsSeparator + 2] := '9';
  end;
end;

function IsASCIIDigit(const AChar: Char): Boolean;
{$IFDEF FPC}inline;{$ENDIF}
begin
  Result := (AChar >= '0') and (AChar <= '9');
end;

function ReadFixedDigits(const AValue: string; var APos: Integer;
  const ACount: Integer; out ANumber: Integer): Boolean;
var
  I: Integer;
begin
  Result := False;
  ANumber := 0;
  if APos + ACount - 1 > Length(AValue) then
    Exit;

  for I := 0 to ACount - 1 do
  begin
    if not IsASCIIDigit(AValue[APos + I]) then
      Exit;
    ANumber := ANumber * 10 + Ord(AValue[APos + I]) - Ord('0');
  end;
  Inc(APos, ACount);
  Result := True;
end;

function ReadFractionalNanoseconds(const AValue: string; var APos: Integer;
  out AMillisecond, AMicrosecond, ANanosecond: Integer): Boolean;
var
  Digits: Integer;
  Fraction: Int64;
begin
  Result := False;
  AMillisecond := 0;
  AMicrosecond := 0;
  ANanosecond := 0;

  if (APos > Length(AValue)) or ((AValue[APos] <> '.') and (AValue[APos] <> ',')) then
  begin
    Result := True;
    Exit;
  end;

  Inc(APos);
  Digits := 0;
  Fraction := 0;
  while (APos <= Length(AValue)) and IsASCIIDigit(AValue[APos]) do
  begin
    if Digits >= 9 then
      Exit;
    Fraction := Fraction * 10 + Ord(AValue[APos]) - Ord('0');
    Inc(Digits);
    Inc(APos);
  end;

  if Digits = 0 then
    Exit;
  while Digits < 9 do
  begin
    Fraction := Fraction * 10;
    Inc(Digits);
  end;

  AMillisecond := Integer(Fraction div 1000000);
  AMicrosecond := Integer((Fraction div 1000) mod 1000);
  ANanosecond := Integer(Fraction mod 1000);
  Result := True;
end;

function ParseZonedDateTimeDatePart(const AValue: string; out ADate: TTemporalDateRecord): Boolean;
var
  Pos, Sign, Year, Month, Day: Integer;
  Extended: Boolean;
begin
  Result := False;
  Pos := 1;
  Sign := 1;

  if Pos > Length(AValue) then
    Exit;

  if AValue[Pos] = '+' then
  begin
    Inc(Pos);
    if not ReadFixedDigits(AValue, Pos, 6, Year) then
      Exit;
  end
  else if AValue[Pos] = '-' then
  begin
    Inc(Pos);
    Sign := -1;
    if not ReadFixedDigits(AValue, Pos, 6, Year) then
      Exit;
    if Year = 0 then
      Exit;
  end
  else if not ReadFixedDigits(AValue, Pos, 4, Year) then
    Exit;

  Year := Year * Sign;
  Extended := (Pos <= Length(AValue)) and (AValue[Pos] = '-');
  if Extended then
    Inc(Pos);
  if not ReadFixedDigits(AValue, Pos, 2, Month) then
    Exit;
  if Extended then
  begin
    if (Pos > Length(AValue)) or (AValue[Pos] <> '-') then
      Exit;
    Inc(Pos);
  end;
  if not ReadFixedDigits(AValue, Pos, 2, Day) then
    Exit;
  if Pos <= Length(AValue) then
    Exit;
  if not IsValidDate(Year, Month, Day) then
    Exit;

  ADate.Year := Year;
  ADate.Month := Month;
  ADate.Day := Day;
  Result := True;
end;

function ParseZonedDateTimeTimePart(const AValue: string; out ATime: TTemporalTimeRecord): Boolean;
var
  Pos, Hour, Minute, Second: Integer;
  Extended: Boolean;
begin
  Result := False;
  FillChar(ATime, SizeOf(ATime), 0);
  Pos := 1;

  if not ReadFixedDigits(AValue, Pos, 2, Hour) then
    Exit;
  Minute := 0;
  Second := 0;

  if Pos <= Length(AValue) then
  begin
    Extended := AValue[Pos] = ':';
    if Extended then
      Inc(Pos);

    if not ReadFixedDigits(AValue, Pos, 2, Minute) then
      Exit;

    if Pos <= Length(AValue) then
    begin
      if Extended then
      begin
        if AValue[Pos] = ':' then
          Inc(Pos)
        else if (AValue[Pos] = '.') or (AValue[Pos] = ',') then
        begin
          if not ReadFractionalNanoseconds(AValue, Pos,
            ATime.Millisecond, ATime.Microsecond, ATime.Nanosecond) then
            Exit;
          if Pos <= Length(AValue) then
            Exit;
          if not IsValidTime(Hour, Minute, Second, ATime.Millisecond,
            ATime.Microsecond, ATime.Nanosecond) then
            Exit;
          ATime.Hour := Hour;
          ATime.Minute := Minute;
          ATime.Second := Second;
          Result := True;
          Exit;
        end
        else
          Exit;
      end
      else if (AValue[Pos] = '.') or (AValue[Pos] = ',') then
      begin
        if not ReadFractionalNanoseconds(AValue, Pos,
          ATime.Millisecond, ATime.Microsecond, ATime.Nanosecond) then
          Exit;
        if Pos <= Length(AValue) then
          Exit;
        if not IsValidTime(Hour, Minute, Second, ATime.Millisecond,
          ATime.Microsecond, ATime.Nanosecond) then
          Exit;
        ATime.Hour := Hour;
        ATime.Minute := Minute;
        ATime.Second := Second;
        Result := True;
        Exit;
      end;

      if not ReadFixedDigits(AValue, Pos, 2, Second) then
        Exit;
      if not ReadFractionalNanoseconds(AValue, Pos,
        ATime.Millisecond, ATime.Microsecond, ATime.Nanosecond) then
        Exit;
    end;
  end;

  if Pos <= Length(AValue) then
    Exit;
  if Second = 60 then
    Second := 59;
  if not IsValidTime(Hour, Minute, Second, ATime.Millisecond,
    ATime.Microsecond, ATime.Nanosecond) then
    Exit;

  ATime.Hour := Hour;
  ATime.Minute := Minute;
  ATime.Second := Second;
  Result := True;
end;

function ParseZonedDateTimeOffsetPart(const AValue: string; out AOffsetSeconds: Integer;
  out AHasUTCDesignator, AHasSubMinuteSyntax: Boolean): Boolean;
var
  Pos, Sign, Hour, Minute, Second, Ms, Us, Ns: Integer;
  Extended: Boolean;
begin
  Result := False;
  AOffsetSeconds := 0;
  AHasUTCDesignator := False;
  AHasSubMinuteSyntax := False;

  if (AValue = 'Z') or (AValue = 'z') then
  begin
    AHasUTCDesignator := True;
    Result := True;
    Exit;
  end;

  if Length(AValue) = 0 then
    Exit;
  case AValue[1] of
    '+': Sign := 1;
    '-': Sign := -1;
  else
    Exit;
  end;

  Pos := 2;
  if not ReadFixedDigits(AValue, Pos, 2, Hour) then
    Exit;
  Minute := 0;
  Second := 0;
  Extended := (Pos <= Length(AValue)) and (AValue[Pos] = ':');
  if Extended then
    Inc(Pos);

  if Pos <= Length(AValue) then
  begin
    if not ReadFixedDigits(AValue, Pos, 2, Minute) then
      Exit;
    if Pos <= Length(AValue) then
    begin
      if Extended then
      begin
        if AValue[Pos] <> ':' then
          Exit;
        Inc(Pos);
      end;
      if not ReadFixedDigits(AValue, Pos, 2, Second) then
        Exit;
      AHasSubMinuteSyntax := True;
      Ms := 0;
      Us := 0;
      Ns := 0;
      if not ReadFractionalNanoseconds(AValue, Pos, Ms, Us, Ns) then
        Exit;
      if (Ms <> 0) or (Us <> 0) or (Ns <> 0) then
        Exit;
    end;
  end;

  if Pos <= Length(AValue) then
    Exit;
  if (Hour > 23) or (Minute > 59) or (Second > 59) then
    Exit;

  AOffsetSeconds := Sign * (Hour * 3600 + Minute * 60 + Second);
  Result := True;
end;

function SplitZonedDateTimeOffset(const AValue: string; out ATimePart, AOffsetPart: string): Boolean;
var
  I: Integer;
begin
  ATimePart := AValue;
  AOffsetPart := '';
  Result := True;

  if Length(AValue) = 0 then
    Exit(False);

  if (AValue[Length(AValue)] = 'Z') or (AValue[Length(AValue)] = 'z') then
  begin
    ATimePart := Copy(AValue, 1, Length(AValue) - 1);
    AOffsetPart := Copy(AValue, Length(AValue), 1);
    Exit;
  end;

  for I := 1 to Length(AValue) do
  begin
    if (AValue[I] = '+') or (AValue[I] = '-') then
    begin
      ATimePart := Copy(AValue, 1, I - 1);
      AOffsetPart := Copy(AValue, I, Length(AValue) - I + 1);
      Exit;
    end;
  end;
end;

function ExtractZonedDateTimeAnnotations(var AValue: string; out ATimeZone, ACalendarId: string): Boolean;
var
  BracketStart, BracketEnd: Integer;
  Annotation, Key, Value, Calendar, Base: string;
  EqualsPos, Count, I: Integer;
  Critical, CalendarCriticalSeen: Boolean;
  CalendarCount: Integer;
  Annotations: array of string;
  CriticalAnnotations: array of Boolean;
begin
  Result := False;
  ATimeZone := '';
  ACalendarId := 'iso8601';
  Base := AValue;
  Count := 0;

  while (Length(Base) > 0) and (Base[Length(Base)] = ']') do
  begin
    BracketEnd := Length(Base);
    BracketStart := BracketEnd - 1;
    while (BracketStart > 0) and (Base[BracketStart] <> '[') do
      Dec(BracketStart);
    if BracketStart = 0 then
      Exit;

    Annotation := Copy(Base, BracketStart + 1, BracketEnd - BracketStart - 1);
    Critical := (Length(Annotation) > 0) and (Annotation[1] = '!');
    if Critical then
      Annotation := Copy(Annotation, 2, Length(Annotation) - 1);
    if Annotation = '' then
      Exit;

    SetLength(Annotations, Count + 1);
    SetLength(CriticalAnnotations, Count + 1);
    Annotations[Count] := Annotation;
    CriticalAnnotations[Count] := Critical;
    Inc(Count);
    Base := Copy(Base, 1, BracketStart - 1);
  end;

  CalendarCount := 0;
  CalendarCriticalSeen := False;

  for I := Count - 1 downto 0 do
  begin
    Annotation := Annotations[I];
    Critical := CriticalAnnotations[I];
    EqualsPos := System.Pos('=', Annotation);
    if EqualsPos > 0 then
    begin
      Key := Copy(Annotation, 1, EqualsPos - 1);
      Value := Copy(Annotation, EqualsPos + 1, Length(Annotation) - EqualsPos);
      if not ASCIIIsLowercaseKey(Key) then
        Exit;

      if Key = 'u-ca' then
      begin
        Inc(CalendarCount);
        if Critical then
          CalendarCriticalSeen := True;
        if (CalendarCount > 1) and (CalendarCriticalSeen or Critical) then
          Exit;
        if CalendarCount = 1 then
        begin
          Calendar := CanonicalizeTemporalCalendarIdentifier(Value);
          if Calendar = '' then
            Exit;
          ACalendarId := Calendar;
        end;
      end
      else if Critical then
        Exit;
    end
    else
    begin
      if ATimeZone <> '' then
        Exit;
      ATimeZone := Annotation;
    end;
  end;

  AValue := Base;
  Result := True;
end;

function TryParseZonedDateTimeString(const AValue: string; out AParsed: TZonedDateTimeParseResult): Boolean;
var
  Base, DatePart, TimeAndOffset, TimePart, OffsetPart: string;
  SepPos, I: Integer;
begin
  Result := False;
  AParsed.Date.Year := 0;
  AParsed.Date.Month := 0;
  AParsed.Date.Day := 0;
  AParsed.Time.Hour := 0;
  AParsed.Time.Minute := 0;
  AParsed.Time.Second := 0;
  AParsed.Time.Millisecond := 0;
  AParsed.Time.Microsecond := 0;
  AParsed.Time.Nanosecond := 0;
  AParsed.TimeZone := '';
  AParsed.OffsetSeconds := 0;
  AParsed.HasOffset := False;
  AParsed.HasExplicitTime := False;
  AParsed.HasUTCDesignator := False;
  AParsed.OffsetHasSubMinuteSyntax := False;
  AParsed.TimeZoneFromOffset := False;
  AParsed.CalendarId := 'iso8601';
  Base := AValue;
  if Base = '' then
    Exit;
  if not ExtractZonedDateTimeAnnotations(Base, AParsed.TimeZone, AParsed.CalendarId) then
    Exit;

  SepPos := 0;
  for I := 1 to Length(Base) do
  begin
    if (Base[I] = 'T') or (Base[I] = 't') or (Base[I] = ' ') then
    begin
      SepPos := I;
      Break;
    end;
  end;

  if SepPos = 0 then
  begin
    if not ParseZonedDateTimeDatePart(Base, AParsed.Date) then
      Exit;
    AParsed.Time.Hour := 0;
    AParsed.Time.Minute := 0;
    AParsed.Time.Second := 0;
    AParsed.Time.Millisecond := 0;
    AParsed.Time.Microsecond := 0;
    AParsed.Time.Nanosecond := 0;
  end
  else
  begin
    AParsed.HasExplicitTime := True;
    DatePart := Copy(Base, 1, SepPos - 1);
    TimeAndOffset := Copy(Base, SepPos + 1, Length(Base) - SepPos);
    if not ParseZonedDateTimeDatePart(DatePart, AParsed.Date) then
      Exit;
    if not SplitZonedDateTimeOffset(TimeAndOffset, TimePart, OffsetPart) then
      Exit;
    if TimePart = '' then
      Exit;
    if OffsetPart <> '' then
    begin
      if not ParseZonedDateTimeOffsetPart(OffsetPart, AParsed.OffsetSeconds,
        AParsed.HasUTCDesignator, AParsed.OffsetHasSubMinuteSyntax) then
        Exit;
      AParsed.HasOffset := True;
      if (AParsed.TimeZone = '') and AParsed.HasUTCDesignator then
      begin
        AParsed.TimeZone := 'UTC';
        AParsed.TimeZoneFromOffset := True;
      end
      else if AParsed.TimeZone = '' then
      begin
        AParsed.TimeZone := FormatOffsetString(AParsed.OffsetSeconds);
        AParsed.TimeZoneFromOffset := True;
      end;
    end;
    if not ParseZonedDateTimeTimePart(TimePart, AParsed.Time) then
      Exit;
  end;

  if AParsed.TimeZone = '' then
    Exit;
  Result := True;
end;

function StripPlainTimeAnnotations(var AValue: string): Boolean;
var
  BracketStart, BracketEnd, EqualsPos: Integer;
  Annotation, Key: string;
  Critical, CalendarCriticalSeen: Boolean;
  CalendarCount, TimeZoneCount: Integer;
begin
  Result := False;
  CalendarCriticalSeen := False;
  CalendarCount := 0;
  TimeZoneCount := 0;

  while (Length(AValue) > 0) and (AValue[Length(AValue)] = ']') do
  begin
    BracketEnd := Length(AValue);
    BracketStart := BracketEnd - 1;
    while (BracketStart > 0) and (AValue[BracketStart] <> '[') do
      Dec(BracketStart);
    if BracketStart = 0 then
      Exit;

    Annotation := Copy(AValue, BracketStart + 1, BracketEnd - BracketStart - 1);
    Critical := (Length(Annotation) > 0) and (Annotation[1] = '!');
    if Critical then
      Annotation := Copy(Annotation, 2, Length(Annotation) - 1);
    if Annotation = '' then
      Exit;

    EqualsPos := System.Pos('=', Annotation);
    if EqualsPos > 0 then
    begin
      Key := Copy(Annotation, 1, EqualsPos - 1);
      if not ASCIIIsLowercaseKey(Key) then
        Exit;
      if Key = 'u-ca' then
      begin
        Inc(CalendarCount);
        if Critical then
          CalendarCriticalSeen := True;
        if (CalendarCount > 1) and (CalendarCriticalSeen or Critical) then
          Exit;
      end
      else if Critical then
        Exit;
    end
    else
    begin
      Inc(TimeZoneCount);
      if TimeZoneCount > 1 then
        Exit;
    end;

    AValue := Copy(AValue, 1, BracketStart - 1);
  end;

  Result := True;
end;

function TryParsePlainTimeLikeString(const AValue: string; out ATime: TTemporalTimeRecord): Boolean;
var
  Base, DatePart, TimeAndOffset, TimePart, OffsetPart: string;
  SepPos, I, OffsetSeconds: Integer;
  DateRec: TTemporalDateRecord;
  HasUTCDesignator, HasSubMinuteSyntax: Boolean;

  function AllDigits(const S: string): Boolean;
  var
    J: Integer;
  begin
    Result := S <> '';
    for J := 1 to Length(S) do
    begin
      if (S[J] < '0') or (S[J] > '9') then
        Exit(False);
    end;
  end;

  function TwoDigitValue(const S: string; const AStart: Integer): Integer;
  begin
    Result := (Ord(S[AStart]) - Ord('0')) * 10 + Ord(S[AStart + 1]) - Ord('0');
  end;

  function IsAmbiguousBarePlainTime(const S: string): Boolean;
  var
    Month, Day: Integer;
  begin
    Result := False;
    if (Length(S) = 7) and AllDigits(Copy(S, 1, 4)) and
       (S[5] = '-') and AllDigits(Copy(S, 6, 2)) then
    begin
      Month := TwoDigitValue(S, 6);
      Exit((Month >= 1) and (Month <= 12));
    end;
    if (Length(S) = 6) and AllDigits(S) then
    begin
      Month := TwoDigitValue(S, 5);
      Exit((Month >= 1) and (Month <= 12));
    end;
    if (Length(S) = 5) and AllDigits(Copy(S, 1, 2)) and
       (S[3] = '-') and AllDigits(Copy(S, 4, 2)) then
    begin
      Month := TwoDigitValue(S, 1);
      Day := TwoDigitValue(S, 4);
      Exit((Month >= 1) and (Month <= 12) and
        (Day >= 1) and (Day <= DaysInMonth(2020, Month)));
    end;
    if (Length(S) = 4) and AllDigits(S) then
    begin
      Month := TwoDigitValue(S, 1);
      Day := TwoDigitValue(S, 3);
      Exit((Month >= 1) and (Month <= 12) and
        (Day >= 1) and (Day <= DaysInMonth(2020, Month)));
    end;
  end;

begin
  Result := False;
  Base := AValue;
  if Base = '' then
    Exit;
  if not StripPlainTimeAnnotations(Base) then
    Exit;

  SepPos := 0;
  for I := 1 to Length(Base) do
  begin
    if (Base[I] = 'T') or (Base[I] = 't') or (Base[I] = ' ') then
    begin
      SepPos := I;
      Break;
    end;
  end;

  if SepPos = 1 then
  begin
    if Base[1] = ' ' then
      Exit;
    TimeAndOffset := Copy(Base, 2, Length(Base) - 1)
  end
  else if SepPos > 1 then
  begin
    DatePart := Copy(Base, 1, SepPos - 1);
    if not ParseZonedDateTimeDatePart(DatePart, DateRec) then
      Exit;
    TimeAndOffset := Copy(Base, SepPos + 1, Length(Base) - SepPos);
  end
  else
  begin
    if IsAmbiguousBarePlainTime(Base) then
      Exit;
    TimeAndOffset := Base;
  end;

  if not SplitZonedDateTimeOffset(TimeAndOffset, TimePart, OffsetPart) then
    Exit;
  if TimePart = '' then
    Exit;
  if OffsetPart <> '' then
  begin
    if not ParseZonedDateTimeOffsetPart(OffsetPart, OffsetSeconds,
      HasUTCDesignator, HasSubMinuteSyntax) then
      Exit;
    if HasUTCDesignator then
      Exit;
  end;

  Result := ParseZonedDateTimeTimePart(TimePart, ATime);
end;

procedure ValidateZonedDateTimeEpoch(const AEpochMilliseconds: Int64;
  const ASubMillisecondNanoseconds: Integer; const AMethod: string);
var
  EpochNs: TBigInteger;
begin
  EpochNs := EpochNanosecondsFromParts(AEpochMilliseconds, ASubMillisecondNanoseconds);
  if not IsValidEpochNanoseconds(EpochNs) then
    ThrowRangeError(Format(SErrorTemporalInvalidISOStringFor, ['ZonedDateTime', AMethod]),
      SSuggestTemporalDateRange);
end;

procedure ValidateZonedDateTimeLocalRange(const AYear, AMonth, ADay: Integer;
  const ATime: TTemporalTimeRecord; const ASubMillisecondNanoseconds: Integer;
  const AMethod: string);

  function DateBefore(const AY, AM, AD, ABY, ABM, ABD: Integer): Boolean;
  begin
    Result := (AY < ABY) or ((AY = ABY) and
      ((AM < ABM) or ((AM = ABM) and (AD < ABD))));
  end;

begin
  if DateBefore(AYear, AMonth, ADay, -271821, 4, 20) or
     DateBefore(275760, 9, 13, AYear, AMonth, ADay) then
    ThrowRangeError(Format(SErrorTemporalInvalidISOStringFor, ['ZonedDateTime', AMethod]),
      SSuggestTemporalDateRange);
end;

function CalendarStringHasNonISOAnnotation(const AValue: string): Boolean;
var
  BracketStart, BracketEnd: Integer;
  Annotation: string;
begin
  Result := False;
  BracketEnd := Length(AValue);
  while BracketEnd > 0 do
  begin
    while (BracketEnd > 0) and (AValue[BracketEnd] <> ']') do
      Dec(BracketEnd);
    if BracketEnd = 0 then
      Exit;

    BracketStart := BracketEnd - 1;
    while (BracketStart > 0) and (AValue[BracketStart] <> '[') do
      Dec(BracketStart);
    if BracketStart = 0 then
      Exit;

    Annotation := Copy(AValue, BracketStart + 1, BracketEnd - BracketStart - 1);
    if (Length(Annotation) > 0) and (Annotation[1] = '!') then
      Annotation := Copy(Annotation, 2, Length(Annotation) - 1);
    if Copy(Annotation, 1, 5) = 'u-ca=' then
    begin
      if not ASCIIEqualsIgnoreCase(Copy(Annotation, 6, Length(Annotation) - 5), 'iso8601') then
        Exit(True);
    end;
    BracketEnd := BracketStart - 1;
  end;
end;

function CalendarStringIsISO(const AValue: string): Boolean;
var
  DateRec: TTemporalDateRecord;
  TimeRec: TTemporalTimeRecord;
  Year, Month, Day: Integer;
  OffsetSeconds: Integer;
  TimeZone: string;
  Normalized: string;
begin
  if AValue = '' then
    Exit(False);
  if Copy(AValue, 1, 7) = '-000000' then
    Exit(False);
  if ASCIIEqualsIgnoreCase(AValue, 'iso8601') then
    Exit(True);
  if CalendarStringHasNonISOAnnotation(AValue) then
    Exit(False);

  Normalized := NormalizeLeapSecondForTemporalDateTime(AValue);
  Result := CoerceToISODate(Normalized, DateRec) or
    CoerceToISODateTime(Normalized, DateRec, TimeRec) or
    TryParsePlainTimeLikeString(Normalized, TimeRec) or
    CoerceToISOYearMonth(Normalized, Year, Month) or
    CoerceToISOMonthDay(Normalized, Month, Day) or
    TryParseISODateTimeWithOffset(Normalized, DateRec, TimeRec, OffsetSeconds, TimeZone);
end;

function ClampTemporalInteger(const AValue, AMin, AMax: Integer): Integer;
{$IFDEF FPC}inline;{$ENDIF}
begin
  if AValue < AMin then
    Result := AMin
  else if AValue > AMax then
    Result := AMax
  else
    Result := AValue;
end;

procedure ConstrainTemporalTimeFields(var AHour, AMinute, ASecond,
  AMillisecond, AMicrosecond, ANanosecond: Integer);
  {$IFDEF FPC}inline;{$ENDIF}
begin
  AHour := ClampTemporalInteger(AHour, 0, 23);
  AMinute := ClampTemporalInteger(AMinute, 0, 59);
  ASecond := ClampTemporalInteger(ASecond, 0, 59);
  AMillisecond := ClampTemporalInteger(AMillisecond, 0, 999);
  AMicrosecond := ClampTemporalInteger(AMicrosecond, 0, 999);
  ANanosecond := ClampTemporalInteger(ANanosecond, 0, 999);
end;

function CalendarFromProperty(const AValue: TGocciaValue): string;
var
  CalendarString: string;
  CalendarId: string;
begin
  if IsUndefinedValue(AValue) then
    Exit('iso8601');

  if AValue is TGocciaTemporalPlainDateValue then
    Exit(TGocciaTemporalPlainDateValue(AValue).CalendarId);
  if AValue is TGocciaTemporalPlainDateTimeValue then
    Exit(TGocciaTemporalPlainDateTimeValue(AValue).CalendarId);
  if AValue is TGocciaTemporalPlainMonthDayValue then
    Exit(TGocciaTemporalPlainMonthDayValue(AValue).CalendarId);
  if AValue is TGocciaTemporalPlainYearMonthValue then
    Exit(TGocciaTemporalPlainYearMonthValue(AValue).CalendarId);
  if AValue is TGocciaTemporalZonedDateTimeValue then
    Exit(TGocciaTemporalZonedDateTimeValue(AValue).CalendarId);

  if not (AValue is TGocciaStringLiteralValue) then
    ThrowTypeError('Temporal.ZonedDateTime calendar must be a string or Temporal calendar-carrying object',
      SSuggestTemporalFromArg);

  CalendarString := TGocciaStringLiteralValue(AValue).Value;
  CalendarId := CanonicalizeTemporalCalendarIdentifier(CalendarString);
  if CalendarId <> '' then
    Exit(CalendarId);
  if TryExtractTemporalCalendarAnnotation(CalendarString, CalendarId) and (CalendarId <> 'iso8601') then
    Exit(CalendarId);
  if CalendarStringIsISO(CalendarString) then
    Exit('iso8601');
  ThrowRangeError('Temporal.ZonedDateTime calendar must identify a supported calendar',
    SSuggestTemporalDateRange);
  Result := 'iso8601';
end;

function CanonicalizeTimeZoneIdentifier(const AValue: string): string; forward;
function IsOffsetTimeZoneIdentifier(const AValue: string): Boolean; forward;

function TryDateTimeStringTimeZoneIdentifier(const AValue: string; out ATimeZone: string): Boolean;
var
  Parsed: TZonedDateTimeParseResult;
begin
  ATimeZone := '';
  if (System.Pos('T', AValue) = 0) and (System.Pos('t', AValue) = 0) and
     (System.Pos(' ', AValue) = 0) then
    Exit(False);

  if not TryParseZonedDateTimeString(AValue, Parsed) then
    Exit(False);
  if Parsed.TimeZone = '' then
    Exit(False);
  if Parsed.TimeZoneFromOffset and Parsed.OffsetHasSubMinuteSyntax then
    Exit(False);

  ATimeZone := CanonicalizeTimeZoneIdentifier(Parsed.TimeZone);
  Result := True;
end;

function IsOffsetTimeZoneIdentifier(const AValue: string): Boolean;
var
  OffsetSeconds: Integer;
begin
  Result := ParseOffsetString(AValue, OffsetSeconds);
end;

function CanonicalizeTimeZoneIdentifier(const AValue: string): string;
var
  OffsetSeconds: Integer;
  HasUTCDesignator, HasSubMinuteSyntax: Boolean;
  CaseNormalized: string;
begin
  if AValue = '' then
    ThrowRangeError(Format(SErrorUnknownTimezone, [AValue]), SSuggestTemporalTimezone);

  if TryDateTimeStringTimeZoneIdentifier(AValue, Result) then
    Exit;

  if ASCIIEqualsIgnoreCase(AValue, 'UTC') then
    Exit('UTC');

  if TryParseTemporalOffsetString(AValue, OffsetSeconds, HasUTCDesignator,
    HasSubMinuteSyntax) then
  begin
    if HasSubMinuteSyntax then
      ThrowRangeError(Format(SErrorUnknownTimezone, [AValue]), SSuggestTemporalTimezone);
    Exit(FormatOffsetString(OffsetSeconds));
  end;

  if IsSupportedCanonicalTimeZoneIdentifier(AValue) and IsValidTimeZone(AValue) then
    Exit(AValue);

  if TryCanonicalizeTimeZoneIdentifierCase(AValue, CaseNormalized) and
     IsValidTimeZone(CaseNormalized) then
    Exit(CaseNormalized);

  if IsValidTimeZone(AValue) then
    Exit(AValue);

  ThrowRangeError(Format(SErrorUnknownTimezone, [AValue]), SSuggestTemporalTimezone);
end;

function TimeZoneFromProperty(const AValue: TGocciaValue; const AMethod: string): string;
begin
  if IsUndefinedValue(AValue) then
    ThrowTypeError(AMethod + ' requires timeZone property', SSuggestTemporalTimezone);

  if AValue is TGocciaTemporalZonedDateTimeValue then
    Exit(TGocciaTemporalZonedDateTimeValue(AValue).TimeZone);

  if not (AValue is TGocciaStringLiteralValue) then
    ThrowTypeError(AMethod + ' timeZone must be a string or ZonedDateTime',
      SSuggestTemporalTimezone);

  Result := CanonicalizeTimeZoneIdentifier(TGocciaStringLiteralValue(AValue).Value);
end;

function CanonicalizeTemporalTimeZoneIdentifier(const AValue: string): string;
begin
  Result := CanonicalizeTimeZoneIdentifier(AValue);
end;

function TryParseTemporalOffsetString(const AValue: string; out AOffsetSeconds: Integer;
  out AHasUTCDesignator, AHasSubMinuteSyntax: Boolean): Boolean;
begin
  Result := ParseZonedDateTimeOffsetPart(AValue, AOffsetSeconds,
    AHasUTCDesignator, AHasSubMinuteSyntax);
end;

function IsMonthCodeSyntaxValid(const AMonthCode: string): Boolean; forward;

function OffsetStringFromProperty(const AValue: TGocciaValue; const AMethod: string): string;
begin
  if AValue is TGocciaStringLiteralValue then
    Exit(TGocciaStringLiteralValue(AValue).Value);
  if AValue is TGocciaObjectValue then
    Exit(AValue.ToStringLiteral.Value);

  ThrowTypeError(AMethod + ' offset must be a string', SSuggestTemporalTimezone);
end;

function MonthCodeStringFromProperty(const AValue: TGocciaValue;
  const AMethod: string): string;
begin
  Result := AValue.ToStringLiteral.Value;
  if not (AValue is TGocciaStringLiteralValue) and not IsMonthCodeSyntaxValid(Result) then
    ThrowTypeError(AMethod + ' monthCode must be a string', SSuggestTemporalMonthCode);
end;

function IsMonthCodeSyntaxValid(const AMonthCode: string): Boolean;
begin
  Result := ((Length(AMonthCode) = 3) or (Length(AMonthCode) = 4)) and
    (AMonthCode[1] = 'M') and
    (AMonthCode[2] in ['0'..'9']) and
    (AMonthCode[3] in ['0'..'9']) and
    ((Length(AMonthCode) = 3) or (AMonthCode[4] = 'L'));
end;

function MonthFromMonthCode(const AMonthCode: string; out AMonth: Integer): Boolean;
var
  MonthPart: Integer;
begin
  Result := False;
  AMonth := 0;
  if (Length(AMonthCode) <> 3) or not TryStrToInt(Copy(AMonthCode, 2, 2), MonthPart) then
    Exit;
  if (MonthPart < 1) or (MonthPart > 12) then
    Exit;
  AMonth := MonthPart;
  Result := True;
end;

function EpochFromLocalAndOffset(const AYear, AMonth, ADay: Integer;
  const ATime: TTemporalTimeRecord; const AOffsetSeconds: Integer): Int64;
begin
  Result := DateToEpochDays(AYear, AMonth, ADay) * MILLISECONDS_PER_DAY +
            Int64(ATime.Hour) * MILLISECONDS_PER_HOUR +
            Int64(ATime.Minute) * MILLISECONDS_PER_MINUTE +
            Int64(ATime.Second) * MILLISECONDS_PER_SECOND +
            ATime.Millisecond -
            Int64(AOffsetSeconds) * MILLISECONDS_PER_SECOND;
end;

function RoundOffsetSecondsToMinute(const AOffsetSeconds: Integer): Integer;
var
  Sign, AbsOffset, RoundedMinutes: Integer;
begin
  if AOffsetSeconds < 0 then
    Sign := -1
  else
    Sign := 1;
  AbsOffset := Abs(AOffsetSeconds);
  RoundedMinutes := (AbsOffset + 30) div 60;
  Result := Sign * RoundedMinutes * 60;
end;

function ApplyOffsetOption(const AYear, AMonth, ADay: Integer;
  const ATime: TTemporalTimeRecord; const ATimeZone: string;
  const AHasOffset: Boolean; const AOffsetSeconds: Integer;
  const AHasUTCDesignator, AOffsetHasSubMinuteSyntax,
  AAllowOffsetMinuteFuzzyMatch: Boolean;
  const AOffsetOption: TTemporalOffsetOption;
  const ADisambiguation: TTemporalDisambiguationOption;
  const AMethod: string): Int64;
var
  OffsetEpochMs, LocalEpochMs: Int64;
  ActualOffsetSeconds: Integer;
begin
  if AHasOffset then
  begin
    OffsetEpochMs := EpochFromLocalAndOffset(AYear, AMonth, ADay, ATime, AOffsetSeconds);
    if AHasUTCDesignator then
      Exit(OffsetEpochMs);
  end
  else
    OffsetEpochMs := 0;

  if not AHasOffset or (AOffsetOption = tooIgnore) then
    Exit(LocalDateTimeToEpochMillisecondsWithDisambiguation(AYear, AMonth,
      ADay, ATime.Hour, ATime.Minute, ATime.Second, ATime.Millisecond,
      ATimeZone, ToTimeZoneDisambiguation(ADisambiguation)));

  if AOffsetOption = tooUse then
    Exit(OffsetEpochMs);

  if AAllowOffsetMinuteFuzzyMatch and (not AOffsetHasSubMinuteSyntax) and
     (not IsOffsetTimeZoneIdentifier(ATimeZone)) then
  begin
    LocalEpochMs := LocalDateTimeToEpochMillisecondsWithDisambiguation(AYear,
      AMonth, ADay, ATime.Hour, ATime.Minute, ATime.Second,
      ATime.Millisecond, ATimeZone, ToTimeZoneDisambiguation(ADisambiguation));
    ActualOffsetSeconds := GetUtcOffsetSeconds(ATimeZone,
      EpochMillisecondsToSeconds(LocalEpochMs));
    if (ActualOffsetSeconds mod 60 <> 0) and
       (RoundOffsetSecondsToMinute(ActualOffsetSeconds) = AOffsetSeconds) then
      Exit(LocalEpochMs);
  end;

  ActualOffsetSeconds := GetUtcOffsetSeconds(ATimeZone,
    EpochMillisecondsToSeconds(OffsetEpochMs));
  if ActualOffsetSeconds = AOffsetSeconds then
    Exit(OffsetEpochMs);

  if AOffsetOption = tooReject then
    ThrowRangeError(Format(SErrorTemporalInvalidISOStringFor, ['ZonedDateTime', AMethod]),
      SSuggestTemporalTimezone);

  Result := LocalDateTimeToEpochMillisecondsWithDisambiguation(AYear, AMonth,
    ADay, ATime.Hour, ATime.Minute, ATime.Second, ATime.Millisecond,
    ATimeZone, ToTimeZoneDisambiguation(ADisambiguation));
end;

function HasDateTimeOffsetDesignator(const AValue: string): Boolean;
var
  TPos, I: Integer;
  Rest: string;
begin
  TPos := System.Pos('T', AValue);
  if TPos = 0 then
    TPos := System.Pos('t', AValue);
  if TPos = 0 then
    Exit(False);

  Rest := Copy(AValue, TPos + 1, Length(AValue) - TPos);
  for I := 1 to Length(Rest) do
  begin
    if Rest[I] = '[' then
      Break;
    if (Rest[I] = 'Z') or (Rest[I] = 'z') or (Rest[I] = '+') or (Rest[I] = '-') then
      Exit(True);
  end;
  Result := False;
end;

function CoerceTemporalZonedDateTime(const AValue: TGocciaValue;
  const AMethod: string; const AOptions: TGocciaValue): TGocciaTemporalZonedDateTimeValue;
var
  TimeRec: TTemporalTimeRecord;
  OffsetSeconds: Integer;
  TimeZoneStr: string;
  EpochMs: Int64;
  SubMs: Integer;
  Obj: TGocciaObjectValue;
  VCalendar, VDay, VEra, VEraYear, VHour, VMicrosecond, VMillisecond, VMinute, VMonth,
  VMonthCode, VNanosecond, VOffset, VSecond, VTimeZone, VYear: TGocciaValue;
  Overflow: TTemporalOverflow;
  Year, Month, Day, MaxDay, SuppliedMonth: Integer;
  MonthPart: Integer;
  ISODate: TTemporalDateRecord;
  Info: TTemporalCalendarDateInfo;
  CalendarId, MonthCodeStr, OffsetStr: string;
  CalendarHasEra, HasEra, HasEraYear, HasMonth, HasMonthCode, HasOffset: Boolean;
  IsLeapMonth: Boolean;
  EraResolvedYear: Integer;
  OffsetOption: TTemporalOffsetOption;
  Disambiguation: TTemporalDisambiguationOption;
  OffsetHasUTCDesignator, OffsetHasSubMinuteSyntax: Boolean;
  Parsed: TZonedDateTimeParseResult;

  function RequiredIntegerField(const AValue: TGocciaValue; const AName: string): Integer;
  begin
    if IsUndefinedValue(AValue) then
      ThrowTypeError(AMethod + ' requires ' + AName + ' property', SSuggestTemporalFromArg);
    Result := ToIntegerWithTruncationValue(AValue);
  end;

  function OptionalIntegerField(const AValue: TGocciaValue; const ADefault: Integer): Integer;
  begin
    if IsUndefinedValue(AValue) then
      Result := ADefault
    else
      Result := ToIntegerWithTruncationValue(AValue);
  end;
begin
  if AValue is TGocciaTemporalZonedDateTimeValue then
  begin
    ReadZonedDateTimeOptions(AOptions, AMethod, Overflow, OffsetOption,
      Disambiguation);
    Result := TGocciaTemporalZonedDateTimeValue(AValue);
  end
  else if AValue is TGocciaStringLiteralValue then
  begin
    if not TryParseZonedDateTimeString(
      NormalizeLeapSecondForTemporalDateTime(TGocciaStringLiteralValue(AValue).Value), Parsed) then
      ThrowRangeError(Format(SErrorTemporalInvalidISOStringFor, ['ZonedDateTime', AMethod]), SSuggestTemporalISOFormat);
    if Parsed.TimeZoneFromOffset then
      ThrowRangeError(Format(SErrorTemporalInvalidISOStringFor, ['ZonedDateTime', AMethod]), SSuggestTemporalISOFormat);
    TimeZoneStr := CanonicalizeTimeZoneIdentifier(Parsed.TimeZone);

    ReadZonedDateTimeOptions(AOptions, AMethod, Overflow, OffsetOption,
      Disambiguation);
    if Parsed.HasOffset and not (OffsetOption in [tooUse, tooIgnore]) then
      ValidateZonedDateTimeLocalRange(Parsed.Date.Year, Parsed.Date.Month,
        Parsed.Date.Day, Parsed.Time,
        Parsed.Time.Microsecond * 1000 + Parsed.Time.Nanosecond, AMethod);
    if (not Parsed.HasExplicitTime) and (not Parsed.HasOffset) then
      EpochMs := StartOfDayToEpochMilliseconds(Parsed.Date.Year,
        Parsed.Date.Month, Parsed.Date.Day, TimeZoneStr)
    else
      EpochMs := ApplyOffsetOption(Parsed.Date.Year, Parsed.Date.Month, Parsed.Date.Day,
        Parsed.Time, TimeZoneStr, Parsed.HasOffset, Parsed.OffsetSeconds,
        Parsed.HasUTCDesignator, Parsed.OffsetHasSubMinuteSyntax, True,
        OffsetOption, Disambiguation, AMethod);
    SubMs := Parsed.Time.Microsecond * 1000 + Parsed.Time.Nanosecond;
    ValidateZonedDateTimeEpoch(EpochMs, SubMs, AMethod);
    Result := TGocciaTemporalZonedDateTimeValue.Create(EpochMs, SubMs, TimeZoneStr, Parsed.CalendarId);
  end
  else if AValue is TGocciaObjectValue then
  begin
    Obj := TGocciaObjectValue(AValue);

    VCalendar := Obj.GetProperty(PROPERTY_CALENDAR);
    CalendarId := CalendarFromProperty(VCalendar);
    if (CalendarId = 'islamic') or (CalendarId = 'islamic-rgsa') then
      ThrowRangeError(Format(SErrorTemporalInvalidISOStringFor, ['ZonedDateTime', AMethod]),
        SSuggestTemporalDateRange);
    IsLeapMonth := False;
    HasEra := False;
    HasEraYear := False;
    CalendarHasEra := TemporalCalendarUsesEra(CalendarId);
    EraResolvedYear := 0;
    if CalendarHasEra then
    begin
      VEra := Obj.GetProperty('era');
      VEraYear := Obj.GetProperty('eraYear');
      HasEra := not IsUndefinedValue(VEra);
      HasEraYear := not IsUndefinedValue(VEraYear);
      if HasEra <> HasEraYear then
        ThrowTypeError(AMethod + ' requires era and eraYear together',
          SSuggestTemporalFromArg);
      if HasEra and not TryCalendarYearFromEra(CalendarId,
        VEra.ToStringLiteral.Value, ToIntegerWithTruncationValue(VEraYear),
        EraResolvedYear) then
        ThrowRangeError(Format(SErrorTemporalInvalidISOStringFor, ['ZonedDateTime', AMethod]),
          SSuggestTemporalDateRange);
    end;

    VDay := Obj.GetProperty(PROPERTY_DAY);
    Day := RequiredIntegerField(VDay, PROPERTY_DAY);
    VHour := Obj.GetProperty(PROPERTY_HOUR);
    TimeRec.Hour := OptionalIntegerField(VHour, 0);
    VMicrosecond := Obj.GetProperty(PROPERTY_MICROSECOND);
    TimeRec.Microsecond := OptionalIntegerField(VMicrosecond, 0);
    VMillisecond := Obj.GetProperty(PROPERTY_MILLISECOND);
    TimeRec.Millisecond := OptionalIntegerField(VMillisecond, 0);
    VMinute := Obj.GetProperty(PROPERTY_MINUTE);
    TimeRec.Minute := OptionalIntegerField(VMinute, 0);
    VMonth := Obj.GetProperty(PROPERTY_MONTH);
    HasMonth := not IsUndefinedValue(VMonth);
    SuppliedMonth := 0;
    if HasMonth then
    begin
      Month := ToIntegerWithTruncationValue(VMonth);
      SuppliedMonth := Month;
    end
    else
      Month := 0;

    VMonthCode := Obj.GetProperty(PROPERTY_MONTH_CODE);
    HasMonthCode := not IsUndefinedValue(VMonthCode);
    MonthCodeStr := '';
    if HasMonthCode then
    begin
      MonthCodeStr := MonthCodeStringFromProperty(VMonthCode, AMethod);
      if not IsMonthCodeSyntaxValid(MonthCodeStr) then
        ThrowRangeError(SErrorInvalidMonthCodeYearMonth, SSuggestTemporalMonthCode);
    end;

    VNanosecond := Obj.GetProperty(PROPERTY_NANOSECOND);
    TimeRec.Nanosecond := OptionalIntegerField(VNanosecond, 0);
    VOffset := Obj.GetProperty(PROPERTY_OFFSET);
    HasOffset := not IsUndefinedValue(VOffset);
    OffsetStr := '';
    if HasOffset then
    begin
      OffsetStr := OffsetStringFromProperty(VOffset, AMethod);
      if (not ParseZonedDateTimeOffsetPart(OffsetStr, OffsetSeconds,
        OffsetHasUTCDesignator, OffsetHasSubMinuteSyntax)) or OffsetHasUTCDesignator then
        ThrowRangeError(Format(SErrorTemporalInvalidISOStringFor, ['ZonedDateTime', AMethod]), SSuggestTemporalISOFormat);
    end
    else
      OffsetSeconds := 0;
    VSecond := Obj.GetProperty(PROPERTY_SECOND);
    TimeRec.Second := OptionalIntegerField(VSecond, 0);
    VTimeZone := Obj.GetProperty(PROPERTY_TIME_ZONE);
    TimeZoneStr := TimeZoneFromProperty(VTimeZone, AMethod);
    VYear := Obj.GetProperty(PROPERTY_YEAR);
    if not IsUndefinedValue(VYear) then
      Year := ToIntegerWithTruncationValue(VYear)
    else if CalendarHasEra and HasEra then
      Year := EraResolvedYear
    else
      ThrowTypeError(AMethod + ' requires year property', SSuggestTemporalFromArg);

    ReadZonedDateTimeOptions(AOptions, AMethod, Overflow, OffsetOption,
      Disambiguation);

    if HasMonthCode then
    begin
      if not TryParseTemporalMonthCode(MonthCodeStr, MonthPart, IsLeapMonth) then
        ThrowRangeError(SErrorMonthCodeOutOfRange, SSuggestTemporalMonthCode);
      if (CalendarId = 'iso8601') and (IsLeapMonth or (MonthPart < 1) or
         (MonthPart > 12)) then
        ThrowRangeError(SErrorMonthCodeOutOfRange, SSuggestTemporalMonthCode);
      if (CalendarId = 'iso8601') and HasMonth and (Month <> MonthPart) then
        ThrowRangeError(SErrorMonthCodeMismatch, SSuggestTemporalMonthCode);
      Month := MonthPart;
    end
    else if not HasMonth then
      ThrowTypeError(AMethod + ' requires month property', SSuggestTemporalFromArg);

    if Month < 0 then
      ThrowRangeError(Format(SErrorTemporalMonthOutOfRange, [Month]), SSuggestTemporalDateRange);
    if (CalendarId = 'iso8601') and ((Month < 1) or (Month > 12)) then
    begin
      if Overflow = toReject then
        ThrowRangeError(Format(SErrorTemporalMonthOutOfRange, [Month]), SSuggestTemporalDateRange);
      if Month < 1 then
        Month := 1
      else
        Month := 12;
    end;

    if Day < 0 then
      ThrowRangeError(Format(SErrorTemporalDayOutOfRange, [Day]), SSuggestTemporalDateRange);
    if Day < 1 then
    begin
      if Overflow = toReject then
        ThrowRangeError(Format(SErrorTemporalDayOutOfRange, [Day]), SSuggestTemporalDateRange);
      Day := 1;
    end;
    if CalendarId = 'iso8601' then
    begin
      MaxDay := DaysInMonth(Year, Month);
      if Day > MaxDay then
      begin
        if Overflow = toReject then
          ThrowRangeError(Format(SErrorTemporalDayOutOfRangeForMonth, [Day, Month]), SSuggestTemporalDateRange);
        Day := MaxDay;
      end;
    end;

    if CalendarId <> 'iso8601' then
    begin
      if not TryResolveCalendarDateToISO(CalendarId, Year, Month, Day,
        HasMonthCode, IsLeapMonth, Overflow = toConstrain, ISODate) then
        ThrowRangeError(Format(SErrorTemporalInvalidISOStringFor, ['ZonedDateTime', AMethod]),
          SSuggestTemporalDateRange);
      if HasMonth and HasMonthCode and
         ((not TryGetCalendarDateInfo(CalendarId, ISODate.Year,
           ISODate.Month, ISODate.Day, Info)) or
          (Info.Date.Month <> SuppliedMonth)) then
        ThrowRangeError(SErrorMonthCodeMismatch, SSuggestTemporalMonthCode);
      Year := ISODate.Year;
      Month := ISODate.Month;
      Day := ISODate.Day;
    end;

    if not IsValidTime(TimeRec.Hour, TimeRec.Minute, TimeRec.Second,
      TimeRec.Millisecond, TimeRec.Microsecond, TimeRec.Nanosecond) then
      ThrowRangeError(SErrorInvalidISOTime, SSuggestTemporalDateRange);

    EpochMs := ApplyOffsetOption(Year, Month, Day, TimeRec, TimeZoneStr,
      HasOffset, OffsetSeconds, False, OffsetHasSubMinuteSyntax, False,
      OffsetOption, Disambiguation, AMethod);

    SubMs := TimeRec.Microsecond * 1000 + TimeRec.Nanosecond;
    if HasOffset and not (OffsetOption in [tooUse, tooIgnore]) then
      ValidateZonedDateTimeLocalRange(Year, Month, Day, TimeRec, SubMs, AMethod);
    ValidateZonedDateTimeEpoch(EpochMs, SubMs, AMethod);
    Result := TGocciaTemporalZonedDateTimeValue.Create(EpochMs, SubMs, TimeZoneStr, CalendarId);
  end
  else
  begin
    ThrowTypeError(AMethod + ' requires a ZonedDateTime, string, or object', SSuggestTemporalFromArg);
    Result := nil;
  end;
end;

procedure ComputeLocalComponentsFromEpoch(const AEpochMilliseconds: Int64;
  const ASubMillisecondNanoseconds: Integer; const ATimeZone: string;
  out AYear, AMonth, ADay, AHour, AMinute, ASecond, AMs, AUs, ANs: Integer);
var
  OffsetSeconds: Integer;
  LocalEpochMs, EpochDays, RemainingMs: Int64;
  DateRec: TTemporalDateRecord;
begin
  OffsetSeconds := GetUtcOffsetSeconds(ATimeZone,
    EpochMillisecondsToSeconds(AEpochMilliseconds));
  LocalEpochMs := AEpochMilliseconds + Int64(OffsetSeconds) * MILLISECONDS_PER_SECOND;

  EpochDays := LocalEpochMs div MILLISECONDS_PER_DAY;
  RemainingMs := LocalEpochMs mod MILLISECONDS_PER_DAY;
  if RemainingMs < 0 then
  begin
    Dec(EpochDays);
    Inc(RemainingMs, MILLISECONDS_PER_DAY);
  end;

  DateRec := EpochDaysToDate(EpochDays);
  AYear := DateRec.Year;
  AMonth := DateRec.Month;
  ADay := DateRec.Day;

  AHour := Integer(RemainingMs div MILLISECONDS_PER_HOUR);
  RemainingMs := RemainingMs mod MILLISECONDS_PER_HOUR;
  AMinute := Integer(RemainingMs div MILLISECONDS_PER_MINUTE);
  RemainingMs := RemainingMs mod MILLISECONDS_PER_MINUTE;
  ASecond := Integer(RemainingMs div MILLISECONDS_PER_SECOND);
  AMs := Integer(RemainingMs mod MILLISECONDS_PER_SECOND);

  AUs := ASubMillisecondNanoseconds div 1000;
  ANs := ASubMillisecondNanoseconds mod 1000;
end;

procedure ComputeLocalComponents(const AZdt: TGocciaTemporalZonedDateTimeValue;
  out AYear, AMonth, ADay, AHour, AMinute, ASecond, AMs, AUs, ANs: Integer);
begin
  ComputeLocalComponentsFromEpoch(AZdt.FEpochMilliseconds,
    AZdt.FSubMillisecondNanoseconds, AZdt.FTimeZone,
    AYear, AMonth, ADay, AHour, AMinute, ASecond, AMs, AUs, ANs);
end;

function RoundingStartOfDayEpochMilliseconds(const AYear, AMonth, ADay: Integer;
  const ATimeZone: string; const APreferRepeatedMidnightLater: Boolean): Int64;
var
  LaterEpochMs: Int64;
  LocalYear, LocalMonth, LocalDay, LocalHour, LocalMinute, LocalSecond,
    LocalMs, LocalUs, LocalNs: Integer;
begin
  Result := StartOfDayToEpochMilliseconds(AYear, AMonth, ADay, ATimeZone);
  ValidateZonedDateTimeEpoch(Result, 0, 'ZonedDateTime.prototype.round');
  if not APreferRepeatedMidnightLater then
    Exit;

  LaterEpochMs := LocalDateTimeToEpochMillisecondsWithDisambiguation(AYear,
    AMonth, ADay, 0, 0, 0, 0, ATimeZone, ttzdLater);
  ComputeLocalComponentsFromEpoch(LaterEpochMs, 0, ATimeZone, LocalYear,
    LocalMonth, LocalDay, LocalHour, LocalMinute, LocalSecond, LocalMs,
    LocalUs, LocalNs);
  if (LocalYear = AYear) and (LocalMonth = AMonth) and (LocalDay = ADay) and
     (LocalHour = 0) and (LocalMinute = 0) and (LocalSecond = 0) and
     (LocalMs = 0) and (LocalUs = 0) and (LocalNs = 0) then
  begin
    ValidateZonedDateTimeEpoch(LaterEpochMs, 0,
      'ZonedDateTime.prototype.round');
    Result := LaterEpochMs;
  end;
end;

function CalendarInfoForZonedDateTime(const AZdt: TGocciaTemporalZonedDateTimeValue;
  const AMethod: string): TTemporalCalendarDateInfo;
var
  LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs: Integer;
begin
  ComputeLocalComponents(AZdt, LYear, LMonth, LDay, LHour, LMinute, LSecond,
    LMs, LUs, LNs);
  if not TryGetCalendarDateInfo(AZdt.FCalendarId, LYear, LMonth, LDay, Result) then
    ThrowRangeError(Format(SErrorTemporalInvalidISOStringFor, ['ZonedDateTime', AMethod]),
      SSuggestTemporalDateRange);
end;

function LocalToEpochMs(const AYear, AMonth, ADay, AHour, AMinute, ASecond, AMs: Integer;
  const ATimeZone: string): Int64;
begin
  Result := LocalDateTimeToEpochMilliseconds(AYear, AMonth, ADay, AHour,
    AMinute, ASecond, AMs, ATimeZone);
end;

function FormatOffsetString(const AOffsetSeconds: Integer): string;
var
  AbsOffset, OffsetH, OffsetM, OffsetS: Integer;
  Sign: Char;
begin
  if AOffsetSeconds >= 0 then
    Sign := '+'
  else
    Sign := '-';
  AbsOffset := Abs(AOffsetSeconds);
  OffsetH := AbsOffset div 3600;
  OffsetM := (AbsOffset mod 3600) div 60;
  OffsetS := AbsOffset mod 60;
  if OffsetS = 0 then
    Result := Sign + Format('%.2d:%.2d', [OffsetH, OffsetM])
  else
    Result := Sign + Format('%.2d:%.2d:%.2d', [OffsetH, OffsetM, OffsetS]);
end;

function DurationPow10(const ACount: Integer): TBigInteger;
var
  I: Integer;
begin
  Result := TBigInteger.One;
  for I := 1 to ACount do
    Result := Result.Multiply(TBigInteger.FromInt64(10));
end;

function FractionalDurationNanoseconds(const AFractionDigits: string;
  const AUnitNanoseconds: Int64): TBigInteger;
var
  Numerator, Scale, Product, Quotient, Remainder: TBigInteger;
begin
  if AFractionDigits = '' then
    Exit(TBigInteger.Zero);
  Numerator := TBigInteger.FromDecimalString(AFractionDigits);
  Scale := DurationPow10(Length(AFractionDigits));
  Product := Numerator.Multiply(TBigInteger.FromInt64(AUnitNanoseconds));
  Quotient := Product.Divide(Scale);
  Remainder := Product.Modulo(Scale);
  if Remainder.Multiply(TBigInteger.FromInt64(2)).Compare(Scale) >= 0 then
    Quotient := Quotient.Add(TBigInteger.One);
  Result := Quotient;
end;

function TryParseDurationStringAsBig(const AValue: string;
  out ADuration: TGocciaTemporalDurationValue): Boolean;
var
  Pos, NumberStart, FractionStart, Sign: Integer;
  InTimePart, HasAny: Boolean;
  C: Char;
  NumberText, FractionText: string;
  Whole, FractionNs: TBigInteger;
  Y, Mo, W, Da, H, Mi, S, Ms, Us, Ns: TBigInteger;

  function IsDigit(const AChar: Char): Boolean;
  {$IFDEF FPC}inline;{$ENDIF}
  begin
    Result := (AChar >= '0') and (AChar <= '9');
  end;

  procedure ApplySign(var AField: TBigInteger);
  begin
    if Sign < 0 then
      AField := AField.Negate;
  end;

begin
  Result := False;
  ADuration := nil;
  if Length(AValue) < 2 then
    Exit;
  Pos := 1;
  Sign := 1;
  if AValue[Pos] = '-' then
  begin
    Sign := -1;
    Inc(Pos);
  end
  else if AValue[Pos] = '+' then
    Inc(Pos);
  if (Pos > Length(AValue)) or (UpCase(AValue[Pos]) <> 'P') then
    Exit;
  Inc(Pos);

  InTimePart := False;
  HasAny := False;
  Y := TBigInteger.Zero; Mo := TBigInteger.Zero; W := TBigInteger.Zero;
  Da := TBigInteger.Zero; H := TBigInteger.Zero; Mi := TBigInteger.Zero;
  S := TBigInteger.Zero; Ms := TBigInteger.Zero; Us := TBigInteger.Zero;
  Ns := TBigInteger.Zero;

  while Pos <= Length(AValue) do
  begin
    C := UpCase(AValue[Pos]);
    if C = 'T' then
    begin
      if InTimePart then
        Exit;
      InTimePart := True;
      Inc(Pos);
      Continue;
    end;

    NumberStart := Pos;
    while (Pos <= Length(AValue)) and IsDigit(AValue[Pos]) do
      Inc(Pos);
    if NumberStart = Pos then
      Exit;
    NumberText := Copy(AValue, NumberStart, Pos - NumberStart);
    FractionText := '';
    if (Pos <= Length(AValue)) and ((AValue[Pos] = '.') or (AValue[Pos] = ',')) then
    begin
      Inc(Pos);
      FractionStart := Pos;
      while (Pos <= Length(AValue)) and IsDigit(AValue[Pos]) do
        Inc(Pos);
      if FractionStart = Pos then
        Exit;
      FractionText := Copy(AValue, FractionStart, Pos - FractionStart);
    end;
    if Pos > Length(AValue) then
      Exit;

    Whole := TBigInteger.FromDecimalString(NumberText);
    C := UpCase(AValue[Pos]);
    Inc(Pos);
    HasAny := True;

    if not InTimePart then
    begin
      if FractionText <> '' then
        Exit;
      case C of
        'Y': Y := Whole;
        'M': Mo := Whole;
        'W': W := Whole;
        'D': Da := Whole;
      else
        Exit;
      end;
    end
    else
    begin
      case C of
        'H':
        begin
          H := Whole;
          Ns := Ns.Add(FractionalDurationNanoseconds(FractionText,
            NANOSECONDS_PER_HOUR));
        end;
        'M':
        begin
          Mi := Whole;
          Ns := Ns.Add(FractionalDurationNanoseconds(FractionText,
            NANOSECONDS_PER_MINUTE));
        end;
        'S':
        begin
          S := Whole;
          Ns := Ns.Add(FractionalDurationNanoseconds(FractionText,
            NANOSECONDS_PER_SECOND));
        end;
      else
        Exit;
      end;
    end;
  end;

  if not HasAny then
    Exit;
  ApplySign(Y); ApplySign(Mo); ApplySign(W); ApplySign(Da); ApplySign(H);
  ApplySign(Mi); ApplySign(S); ApplySign(Ms); ApplySign(Us); ApplySign(Ns);
  ADuration := TGocciaTemporalDurationValue.CreateFromBigIntegers(Y, Mo, W,
    Da, H, Mi, S, Ms, Us, Ns);
  Result := True;
end;

function CoerceDuration(const AArg: TGocciaValue; const AMethod: string): TGocciaTemporalDurationValue;
var
  DurRec: TTemporalDurationRecord;
  Obj: TGocciaObjectValue;
  VF: TGocciaValue;
  Seen: Boolean;
  Y, Mo, W, Da, H, Mi, S, Ms, Us, Ns: TBigInteger;

  procedure ReadField(const AName: string; var ATarget: TBigInteger);
  begin
    VF := Obj.GetProperty(AName);
    if Assigned(VF) and not (VF is TGocciaUndefinedLiteralValue) then
    begin
      ATarget := DurationFieldToBigInteger(VF);
      Seen := True;
    end;
  end;

begin
  if AArg is TGocciaTemporalDurationValue then
    Result := TGocciaTemporalDurationValue(AArg)
  else if AArg is TGocciaStringLiteralValue then
  begin
    if TryParseDurationStringAsBig(TGocciaStringLiteralValue(AArg).Value, Result) then
      Exit;
    if not TryParseISODuration(TGocciaStringLiteralValue(AArg).Value, DurRec) then
      ThrowRangeError(SErrorInvalidDurationString, SSuggestTemporalDurationArg);
    Result := TGocciaTemporalDurationValue.Create(
      DurRec.Years, DurRec.Months, DurRec.Weeks, DurRec.Days,
      DurRec.Hours, DurRec.Minutes, DurRec.Seconds,
      DurRec.Milliseconds, DurRec.Microseconds, DurRec.Nanoseconds);
  end
  else if AArg is TGocciaObjectValue then
  begin
    Obj := TGocciaObjectValue(AArg);
    Seen := False;
    Y := TBigInteger.Zero;
    Mo := TBigInteger.Zero;
    W := TBigInteger.Zero;
    Da := TBigInteger.Zero;
    H := TBigInteger.Zero;
    Mi := TBigInteger.Zero;
    S := TBigInteger.Zero;
    Ms := TBigInteger.Zero;
    Us := TBigInteger.Zero;
    Ns := TBigInteger.Zero;

    ReadField('days', Da);
    ReadField('hours', H);
    ReadField('microseconds', Us);
    ReadField('milliseconds', Ms);
    ReadField('minutes', Mi);
    ReadField('months', Mo);
    ReadField('nanoseconds', Ns);
    ReadField('seconds', S);
    ReadField('weeks', W);
    ReadField('years', Y);

    if not Seen then
      ThrowTypeError(AMethod + ' requires at least one duration field',
        SSuggestTemporalDurationArg);
    Result := TGocciaTemporalDurationValue.CreateFromBigIntegers(Y, Mo, W, Da,
      H, Mi, S, Ms, Us, Ns);
  end
  else
  begin
    ThrowTypeError(AMethod + ' requires a Duration, string, or object', SSuggestTemporalDurationArg);
    Result := nil;
  end;
end;

function DurationHasDateUnits(const ADuration: TGocciaTemporalDurationValue): Boolean;
begin
  Result := (not ADuration.YearsBig.IsZero) or
    (not ADuration.MonthsBig.IsZero) or
    (not ADuration.WeeksBig.IsZero) or
    (not ADuration.DaysBig.IsZero);
end;

function DurationSubDayNanosecondsForZonedDateTime(
  const ADuration: TGocciaTemporalDurationValue): TBigInteger;
begin
  Result := ADuration.HoursBig.Multiply(TBigInteger.FromInt64(NANOSECONDS_PER_HOUR))
    .Add(ADuration.MinutesBig.Multiply(TBigInteger.FromInt64(NANOSECONDS_PER_MINUTE)))
    .Add(ADuration.SecondsBig.Multiply(TBigInteger.FromInt64(NANOSECONDS_PER_SECOND)))
    .Add(ADuration.MillisecondsBig.Multiply(TBigInteger.FromInt64(NANOSECONDS_PER_MILLISECOND)))
    .Add(ADuration.MicrosecondsBig.Multiply(TBigInteger.FromInt64(NANOSECONDS_PER_MICROSECOND)))
    .Add(ADuration.NanosecondsBig);
end;

{ TGocciaTemporalZonedDateTimeValue }

constructor TGocciaTemporalZonedDateTimeValue.Create(const AEpochMilliseconds: Int64;
  const ASubMillisecondNanoseconds: Integer; const ATimeZone: string;
  const ACalendarId: string = 'iso8601');
var
  Shared: TGocciaSharedPrototype;
  TimeZoneId: string;
begin
  inherited Create(nil);
  if ATimeZone = '' then
    ThrowRangeError(SErrorZonedDateTimeRequiresTZ, SSuggestTemporalTimezone);
  TimeZoneId := CanonicalizeTimeZoneIdentifier(ATimeZone);

  FEpochMilliseconds := AEpochMilliseconds;
  FSubMillisecondNanoseconds := ASubMillisecondNanoseconds;
  FTimeZone := TimeZoneId;
  FCalendarId := CanonicalizeTemporalCalendarIdentifier(ACalendarId);
  if FCalendarId = '' then
    ThrowRangeError('Unknown calendar: ' + ACalendarId, SSuggestTemporalDateRange);

  // Normalize sub-ms nanoseconds into 0..999999 range
  if (FSubMillisecondNanoseconds >= SUB_MS_NANOSECOND_LIMIT) or (FSubMillisecondNanoseconds < 0) then
  begin
    if FSubMillisecondNanoseconds >= 0 then
    begin
      Inc(FEpochMilliseconds, FSubMillisecondNanoseconds div SUB_MS_NANOSECOND_LIMIT);
      FSubMillisecondNanoseconds := FSubMillisecondNanoseconds mod SUB_MS_NANOSECOND_LIMIT;
    end
    else
    begin
      Inc(FEpochMilliseconds, (FSubMillisecondNanoseconds - (SUB_MS_NANOSECOND_LIMIT - 1)) div SUB_MS_NANOSECOND_LIMIT);
      FSubMillisecondNanoseconds := FSubMillisecondNanoseconds -
        ((FSubMillisecondNanoseconds - (SUB_MS_NANOSECOND_LIMIT - 1)) div SUB_MS_NANOSECOND_LIMIT) * SUB_MS_NANOSECOND_LIMIT;
    end;
  end;

  if not IsValidEpochNanoseconds(EpochNanosecondsFromParts(FEpochMilliseconds,
    FSubMillisecondNanoseconds)) then
    ThrowRangeError(SErrorTemporalInstantOutOfRange, SSuggestTemporalInstantRange);

  InitializePrototype;
  Shared := GetTemporalZonedDateTimeShared;
  if Assigned(Shared) then
    FPrototype := Shared.Prototype;
end;

procedure TGocciaTemporalZonedDateTimeValue.InitializePrototype;
var
  Members: TGocciaMemberCollection;
  Shared: TGocciaSharedPrototype;
  PrototypeMembers: TArray<TGocciaMemberDefinition>;
begin
  if (CurrentRealm = nil) then Exit;
  if (GetTemporalZonedDateTimeShared <> nil) then Exit;
  Shared := TGocciaSharedPrototype.Create(Self);
  CurrentRealm.SetOwnedSlot(GTemporalZonedDateTimeSharedSlot, Shared);
  Members := TGocciaMemberCollection.Create;
  try
    Members.AddAccessor('calendarId', GetCalendarId, nil, [pfConfigurable]);
    Members.AddAccessor('timeZoneId', GetTimeZoneId, nil, [pfConfigurable]);
    Members.AddAccessor('era', GetEra, nil, [pfConfigurable]);
    Members.AddAccessor('eraYear', GetEraYear, nil, [pfConfigurable]);
    Members.AddAccessor('year', GetYear, nil, [pfConfigurable]);
    Members.AddAccessor('month', GetMonth, nil, [pfConfigurable]);
    Members.AddAccessor('monthCode', GetMonthCode, nil, [pfConfigurable]);
    Members.AddAccessor('day', GetDay, nil, [pfConfigurable]);
    Members.AddAccessor('dayOfWeek', GetDayOfWeek, nil, [pfConfigurable]);
    Members.AddAccessor('dayOfYear', GetDayOfYear, nil, [pfConfigurable]);
    Members.AddAccessor('weekOfYear', GetWeekOfYear, nil, [pfConfigurable]);
    Members.AddAccessor('yearOfWeek', GetYearOfWeek, nil, [pfConfigurable]);
    Members.AddAccessor('daysInWeek', GetDaysInWeek, nil, [pfConfigurable]);
    Members.AddAccessor('daysInMonth', GetDaysInMonth, nil, [pfConfigurable]);
    Members.AddAccessor('daysInYear', GetDaysInYear, nil, [pfConfigurable]);
    Members.AddAccessor('monthsInYear', GetMonthsInYear, nil, [pfConfigurable]);
    Members.AddAccessor('inLeapYear', GetInLeapYear, nil, [pfConfigurable]);
    Members.AddAccessor('hoursInDay', GetHoursInDay, nil, [pfConfigurable]);
    Members.AddAccessor('hour', GetHour, nil, [pfConfigurable]);
    Members.AddAccessor('minute', GetMinute, nil, [pfConfigurable]);
    Members.AddAccessor('second', GetSecond, nil, [pfConfigurable]);
    Members.AddAccessor('millisecond', GetMillisecond, nil, [pfConfigurable]);
    Members.AddAccessor('microsecond', GetMicrosecond, nil, [pfConfigurable]);
    Members.AddAccessor('nanosecond', GetNanosecond, nil, [pfConfigurable]);
    Members.AddAccessor('offset', GetOffset, nil, [pfConfigurable]);
    Members.AddAccessor('offsetNanoseconds', GetOffsetNanoseconds, nil, [pfConfigurable]);
    Members.AddAccessor('epochMilliseconds', GetEpochMilliseconds, nil, [pfConfigurable]);
    Members.AddAccessor('epochNanoseconds', GetEpochNanoseconds, nil, [pfConfigurable]);
    Members.AddMethod(ZonedDateTimeWith, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(ZonedDateTimeWithPlainTime, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(ZonedDateTimeWithTimeZone, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(ZonedDateTimeWithCalendar, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(ZonedDateTimeAdd, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(ZonedDateTimeSubtract, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(ZonedDateTimeUntil, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(ZonedDateTimeSince, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(ZonedDateTimeRound, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(ZonedDateTimeEquals, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(ZonedDateTimeGetTimeZoneTransition, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(ZonedDateTimeStartOfDay, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(ZonedDateTimeToInstant, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(ZonedDateTimeToPlainDate, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(ZonedDateTimeToPlainTime, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(ZonedDateTimeToPlainDateTime, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(ZonedDateTimeToString, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(ZonedDateTimeToJSON, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(ZonedDateTimeValueOf, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(ZonedDateTimeToLocaleString, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddSymbolDataProperty(
      TGocciaSymbolValue.WellKnownToStringTag,
      TGocciaStringLiteralValue.Create('Temporal.ZonedDateTime'),
      [pfConfigurable]);
    PrototypeMembers := Members.ToDefinitions;
  finally
    Members.Free;
  end;
  RegisterMemberDefinitions(Shared.Prototype, PrototypeMembers);
end;

class procedure TGocciaTemporalZonedDateTimeValue.ExposePrototype(const AConstructor: TGocciaObjectValue);
var
  Shared: TGocciaSharedPrototype;
begin
  Shared := GetTemporalZonedDateTimeShared;
  if not Assigned(Shared) then
  begin
    TGocciaTemporalZonedDateTimeValue.Create(0, 0, 'UTC');
    Shared := GetTemporalZonedDateTimeShared;
  end;
  ExposeSharedPrototypeOnConstructor(Shared, AConstructor);
end;

function TGocciaTemporalZonedDateTimeValue.ToStringTag: string;
begin
  Result := 'Temporal.ZonedDateTime';
end;

{ Getters }

function TGocciaTemporalZonedDateTimeValue.GetCalendarId(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Zdt: TGocciaTemporalZonedDateTimeValue;
begin
  Zdt := AsZonedDateTime(AThisValue, 'get ZonedDateTime.calendarId');
  Result := TGocciaStringLiteralValue.Create(Zdt.FCalendarId);
end;

function TGocciaTemporalZonedDateTimeValue.GetTimeZoneId(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaStringLiteralValue.Create(AsZonedDateTime(AThisValue, 'get ZonedDateTime.timeZoneId').FTimeZone);
end;

function TGocciaTemporalZonedDateTimeValue.GetEra(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Zdt: TGocciaTemporalZonedDateTimeValue;
  Info: TTemporalCalendarDateInfo;
begin
  Zdt := AsZonedDateTime(AThisValue, 'get ZonedDateTime.era');
  Info := CalendarInfoForZonedDateTime(Zdt, 'get ZonedDateTime.era');
  if not Info.HasEra then
    Exit(TGocciaUndefinedLiteralValue.UndefinedValue);
  Result := TGocciaStringLiteralValue.Create(Info.Era);
end;

function TGocciaTemporalZonedDateTimeValue.GetEraYear(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Zdt: TGocciaTemporalZonedDateTimeValue;
  Info: TTemporalCalendarDateInfo;
begin
  Zdt := AsZonedDateTime(AThisValue, 'get ZonedDateTime.eraYear');
  Info := CalendarInfoForZonedDateTime(Zdt, 'get ZonedDateTime.eraYear');
  if not Info.HasEra then
    Exit(TGocciaUndefinedLiteralValue.UndefinedValue);
  Result := TGocciaNumberLiteralValue.Create(Info.EraYear);
end;

function TGocciaTemporalZonedDateTimeValue.GetYear(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Zdt: TGocciaTemporalZonedDateTimeValue;
begin
  Zdt := AsZonedDateTime(AThisValue, 'get ZonedDateTime.year');
  Result := TGocciaNumberLiteralValue.Create(
    CalendarInfoForZonedDateTime(Zdt, 'get ZonedDateTime.year').Date.Year);
end;

function TGocciaTemporalZonedDateTimeValue.GetMonth(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Zdt: TGocciaTemporalZonedDateTimeValue;
begin
  Zdt := AsZonedDateTime(AThisValue, 'get ZonedDateTime.month');
  Result := TGocciaNumberLiteralValue.Create(
    CalendarInfoForZonedDateTime(Zdt, 'get ZonedDateTime.month').Date.Month);
end;

function TGocciaTemporalZonedDateTimeValue.GetMonthCode(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Zdt: TGocciaTemporalZonedDateTimeValue;
begin
  Zdt := AsZonedDateTime(AThisValue, 'get ZonedDateTime.monthCode');
  Result := TGocciaStringLiteralValue.Create(
    CalendarInfoForZonedDateTime(Zdt, 'get ZonedDateTime.monthCode').MonthCode);
end;

function TGocciaTemporalZonedDateTimeValue.GetDay(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Zdt: TGocciaTemporalZonedDateTimeValue;
  LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs: Integer;
  Day: Integer;
begin
  Zdt := AsZonedDateTime(AThisValue, 'get ZonedDateTime.day');
  ComputeLocalComponents(Zdt, LYear, LMonth, LDay, LHour, LMinute, LSecond,
    LMs, LUs, LNs);
  if not TryGetCalendarDateDay(Zdt.FCalendarId, LYear, LMonth, LDay, Day) then
    ThrowRangeError(Format(SErrorTemporalInvalidISOStringFor, ['date-time',
      'get ZonedDateTime.day']), SSuggestTemporalDateRange);
  Result := TGocciaNumberLiteralValue.Create(Day);
end;

function TGocciaTemporalZonedDateTimeValue.GetDayOfWeek(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Zdt: TGocciaTemporalZonedDateTimeValue;
begin
  Zdt := AsZonedDateTime(AThisValue, 'get ZonedDateTime.dayOfWeek');
  Result := TGocciaNumberLiteralValue.Create(
    CalendarInfoForZonedDateTime(Zdt, 'get ZonedDateTime.dayOfWeek').DayOfWeek);
end;

function TGocciaTemporalZonedDateTimeValue.GetDayOfYear(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Zdt: TGocciaTemporalZonedDateTimeValue;
begin
  Zdt := AsZonedDateTime(AThisValue, 'get ZonedDateTime.dayOfYear');
  Result := TGocciaNumberLiteralValue.Create(
    CalendarInfoForZonedDateTime(Zdt, 'get ZonedDateTime.dayOfYear').DayOfYear);
end;

function TGocciaTemporalZonedDateTimeValue.GetWeekOfYear(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Zdt: TGocciaTemporalZonedDateTimeValue;
  LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs: Integer;
begin
  Zdt := AsZonedDateTime(AThisValue, 'get ZonedDateTime.weekOfYear');
  if Zdt.FCalendarId <> 'iso8601' then
    Exit(TGocciaUndefinedLiteralValue.UndefinedValue);
  ComputeLocalComponents(Zdt, LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs);
  Result := TGocciaNumberLiteralValue.Create(Goccia.Temporal.Utils.WeekOfYear(LYear, LMonth, LDay));
end;

function TGocciaTemporalZonedDateTimeValue.GetYearOfWeek(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Zdt: TGocciaTemporalZonedDateTimeValue;
  LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs: Integer;
begin
  Zdt := AsZonedDateTime(AThisValue, 'get ZonedDateTime.yearOfWeek');
  if Zdt.FCalendarId <> 'iso8601' then
    Exit(TGocciaUndefinedLiteralValue.UndefinedValue);
  ComputeLocalComponents(Zdt, LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs);
  Result := TGocciaNumberLiteralValue.Create(Goccia.Temporal.Utils.YearOfWeek(LYear, LMonth, LDay));
end;

function TGocciaTemporalZonedDateTimeValue.GetDaysInWeek(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  AsZonedDateTime(AThisValue, 'get ZonedDateTime.daysInWeek');
  Result := TGocciaNumberLiteralValue.Create(DAYS_PER_WEEK);
end;

function TGocciaTemporalZonedDateTimeValue.GetDaysInMonth(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Zdt: TGocciaTemporalZonedDateTimeValue;
  LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs: Integer;
  DaysInMonth: Integer;
begin
  Zdt := AsZonedDateTime(AThisValue, 'get ZonedDateTime.daysInMonth');
  ComputeLocalComponents(Zdt, LYear, LMonth, LDay, LHour, LMinute, LSecond,
    LMs, LUs, LNs);
  if not TryGetCalendarDateDaysInMonth(Zdt.FCalendarId, LYear, LMonth, LDay,
    DaysInMonth) then
    ThrowRangeError(Format(SErrorTemporalInvalidISOStringFor, ['date-time',
      'get ZonedDateTime.daysInMonth']), SSuggestTemporalDateRange);
  Result := TGocciaNumberLiteralValue.Create(DaysInMonth);
end;

function TGocciaTemporalZonedDateTimeValue.GetDaysInYear(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Zdt: TGocciaTemporalZonedDateTimeValue;
begin
  Zdt := AsZonedDateTime(AThisValue, 'get ZonedDateTime.daysInYear');
  Result := TGocciaNumberLiteralValue.Create(
    CalendarInfoForZonedDateTime(Zdt, 'get ZonedDateTime.daysInYear').DaysInYear);
end;

function TGocciaTemporalZonedDateTimeValue.GetMonthsInYear(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Zdt: TGocciaTemporalZonedDateTimeValue;
begin
  Zdt := AsZonedDateTime(AThisValue, 'get ZonedDateTime.monthsInYear');
  Result := TGocciaNumberLiteralValue.Create(
    CalendarInfoForZonedDateTime(Zdt, 'get ZonedDateTime.monthsInYear').MonthsInYear);
end;

function TGocciaTemporalZonedDateTimeValue.GetInLeapYear(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Zdt: TGocciaTemporalZonedDateTimeValue;
begin
  Zdt := AsZonedDateTime(AThisValue, 'get ZonedDateTime.inLeapYear');
  if CalendarInfoForZonedDateTime(Zdt, 'get ZonedDateTime.inLeapYear').InLeapYear then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;

function TGocciaTemporalZonedDateTimeValue.GetHoursInDay(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Zdt: TGocciaTemporalZonedDateTimeValue;
  LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs: Integer;
  StartOfDayMs, StartOfNextDayMs: Int64;
  NextDayRec: TTemporalDateRecord;
begin
  Zdt := AsZonedDateTime(AThisValue, 'get ZonedDateTime.hoursInDay');
  ComputeLocalComponents(Zdt, LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs);
  StartOfDayMs := StartOfDayToEpochMilliseconds(LYear, LMonth, LDay,
    Zdt.FTimeZone);
  ValidateZonedDateTimeEpoch(StartOfDayMs, 0, 'get ZonedDateTime.hoursInDay');
  NextDayRec := AddDaysToDate(LYear, LMonth, LDay, 1);
  StartOfNextDayMs := StartOfDayToEpochMilliseconds(NextDayRec.Year,
    NextDayRec.Month, NextDayRec.Day, Zdt.FTimeZone);
  ValidateZonedDateTimeEpoch(StartOfNextDayMs, 0,
    'get ZonedDateTime.hoursInDay');
  Result := TGocciaNumberLiteralValue.Create((StartOfNextDayMs - StartOfDayMs) / 3600000);
end;

function TGocciaTemporalZonedDateTimeValue.GetHour(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Zdt: TGocciaTemporalZonedDateTimeValue;
  LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs: Integer;
begin
  Zdt := AsZonedDateTime(AThisValue, 'get ZonedDateTime.hour');
  ComputeLocalComponents(Zdt, LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs);
  Result := TGocciaNumberLiteralValue.Create(LHour);
end;

function TGocciaTemporalZonedDateTimeValue.GetMinute(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Zdt: TGocciaTemporalZonedDateTimeValue;
  LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs: Integer;
begin
  Zdt := AsZonedDateTime(AThisValue, 'get ZonedDateTime.minute');
  ComputeLocalComponents(Zdt, LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs);
  Result := TGocciaNumberLiteralValue.Create(LMinute);
end;

function TGocciaTemporalZonedDateTimeValue.GetSecond(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Zdt: TGocciaTemporalZonedDateTimeValue;
  LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs: Integer;
begin
  Zdt := AsZonedDateTime(AThisValue, 'get ZonedDateTime.second');
  ComputeLocalComponents(Zdt, LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs);
  Result := TGocciaNumberLiteralValue.Create(LSecond);
end;

function TGocciaTemporalZonedDateTimeValue.GetMillisecond(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Zdt: TGocciaTemporalZonedDateTimeValue;
  LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs: Integer;
begin
  Zdt := AsZonedDateTime(AThisValue, 'get ZonedDateTime.millisecond');
  ComputeLocalComponents(Zdt, LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs);
  Result := TGocciaNumberLiteralValue.Create(LMs);
end;

function TGocciaTemporalZonedDateTimeValue.GetMicrosecond(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Zdt: TGocciaTemporalZonedDateTimeValue;
  LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs: Integer;
begin
  Zdt := AsZonedDateTime(AThisValue, 'get ZonedDateTime.microsecond');
  ComputeLocalComponents(Zdt, LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs);
  Result := TGocciaNumberLiteralValue.Create(LUs);
end;

function TGocciaTemporalZonedDateTimeValue.GetNanosecond(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Zdt: TGocciaTemporalZonedDateTimeValue;
  LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs: Integer;
begin
  Zdt := AsZonedDateTime(AThisValue, 'get ZonedDateTime.nanosecond');
  ComputeLocalComponents(Zdt, LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs);
  Result := TGocciaNumberLiteralValue.Create(LNs);
end;

function TGocciaTemporalZonedDateTimeValue.GetOffset(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Zdt: TGocciaTemporalZonedDateTimeValue;
  OffsetSeconds: Integer;
begin
  Zdt := AsZonedDateTime(AThisValue, 'get ZonedDateTime.offset');
  OffsetSeconds := GetUtcOffsetSeconds(Zdt.FTimeZone,
    EpochMillisecondsToSeconds(Zdt.FEpochMilliseconds));
  Result := TGocciaStringLiteralValue.Create(FormatOffsetString(OffsetSeconds));
end;

function TGocciaTemporalZonedDateTimeValue.GetOffsetNanoseconds(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Zdt: TGocciaTemporalZonedDateTimeValue;
  OffsetSeconds: Integer;
begin
  Zdt := AsZonedDateTime(AThisValue, 'get ZonedDateTime.offsetNanoseconds');
  OffsetSeconds := GetUtcOffsetSeconds(Zdt.FTimeZone,
    EpochMillisecondsToSeconds(Zdt.FEpochMilliseconds));
  Result := TGocciaNumberLiteralValue.Create(Int64(OffsetSeconds) * NANOSECONDS_PER_SECOND);
end;

function TGocciaTemporalZonedDateTimeValue.GetEpochMilliseconds(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaNumberLiteralValue.Create(AsZonedDateTime(AThisValue, 'get ZonedDateTime.epochMilliseconds').FEpochMilliseconds);
end;

function TGocciaTemporalZonedDateTimeValue.GetEpochNanoseconds(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Zdt: TGocciaTemporalZonedDateTimeValue;
  BigNs: TBigInteger;
begin
  Zdt := AsZonedDateTime(AThisValue, 'get ZonedDateTime.epochNanoseconds');
  BigNs := TBigInteger.FromInt64(Zdt.FEpochMilliseconds)
    .Multiply(TBigInteger.FromInt64(1000000))
    .Add(TBigInteger.FromInt64(Zdt.FSubMillisecondNanoseconds));
  Result := TGocciaBigIntValue.Create(BigNs);
end;

{ Methods }

// TC39 Temporal §6.3.22 Temporal.ZonedDateTime.prototype.with(temporalZonedDateTimeLike [, options])
function TGocciaTemporalZonedDateTimeValue.ZonedDateTimeWith(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Zdt: TGocciaTemporalZonedDateTimeValue;
  Obj: TGocciaObjectValue;
  V, VCalendar, VTimeZone, VYear, VMonth, VMonthCode, VDay, VEra,
  VEraYear, VHour, VMicrosecond, VMillisecond, VMinute, VNanosecond,
  VOffset, VSecond: TGocciaValue;
  DateRec: TTemporalDateRecord;
  NewTime: TTemporalTimeRecord;
  Info: TTemporalCalendarDateInfo;
  LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs: Integer;
  NewYear, NewMonth, NewDay, NewHour, NewMinute, NewSecond, NewMs, NewUs,
  NewNs, SuppliedMonth: Integer;
  MonthCodeStr, OffsetStr: string;
  HasMonth, HasMonthCode, HasOffset, HasRecognizedField, IsLeapMonth,
  UseMonthCode: Boolean;
  OffsetHasUTCDesignator, OffsetHasSubMinuteSyntax: Boolean;
  OffsetSeconds: Integer;
  NewEpochMs: Int64;
  Overflow: TTemporalOverflow;
  OffsetOption: TTemporalOffsetOption;
  Disambiguation: TTemporalDisambiguationOption;
  OptionsObj: TGocciaObjectValue;

  function GetWithOffsetOption(const AOptions: TGocciaObjectValue): TTemporalOffsetOption;
  var
    Value: string;
  begin
    Value := GetOptionString(AOptions, 'offset', 'prefer');
    if Value = 'prefer' then
      Result := tooPrefer
    else if Value = 'use' then
      Result := tooUse
    else if Value = 'ignore' then
      Result := tooIgnore
    else if Value = 'reject' then
      Result := tooReject
    else
      ThrowRangeError('ZonedDateTime.prototype.with invalid offset option: ' + Value,
        SSuggestTemporalRoundArg);
  end;

begin
  Zdt := AsZonedDateTime(AThisValue, 'ZonedDateTime.prototype.with');
  V := AArgs.GetElement(0);
  if not (V is TGocciaObjectValue) then
    ThrowTypeError(Format(SErrorTemporalWithRequiresObject, ['ZonedDateTime']), SSuggestTemporalWithObject);
  if (V is TGocciaTemporalPlainDateValue) or
     (V is TGocciaTemporalPlainDateTimeValue) or
     (V is TGocciaTemporalPlainMonthDayValue) or
     (V is TGocciaTemporalPlainTimeValue) or
     (V is TGocciaTemporalPlainYearMonthValue) or
     (V is TGocciaTemporalZonedDateTimeValue) then
    ThrowTypeError(Format(SErrorTemporalWithRequiresObject, ['ZonedDateTime']),
      SSuggestTemporalWithObject);
  Obj := TGocciaObjectValue(V);

  ComputeLocalComponents(Zdt, LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs);
  Info := CalendarInfoForZonedDateTime(Zdt, 'ZonedDateTime.prototype.with');

  VCalendar := Obj.GetProperty(PROPERTY_CALENDAR);
  if not IsUndefinedValue(VCalendar) then
    ThrowTypeError('ZonedDateTime.prototype.with does not accept calendar',
      SSuggestTemporalWithObject);
  VTimeZone := Obj.GetProperty(PROPERTY_TIME_ZONE);
  if not IsUndefinedValue(VTimeZone) then
    ThrowTypeError('ZonedDateTime.prototype.with does not accept timeZone',
      SSuggestTemporalWithObject);

  VDay := Obj.GetProperty(PROPERTY_DAY);
  if not IsUndefinedValue(VDay) then
    NewDay := ToIntegerWithTruncationValue(VDay)
  else
    NewDay := Info.Date.Day;
  HasRecognizedField := not IsUndefinedValue(VDay);

  VHour := Obj.GetProperty('hour');
  if not IsUndefinedValue(VHour) then
    NewHour := ToIntegerWithTruncationValue(VHour)
  else
    NewHour := LHour;
  HasRecognizedField := HasRecognizedField or not IsUndefinedValue(VHour);

  VMicrosecond := Obj.GetProperty('microsecond');
  if not IsUndefinedValue(VMicrosecond) then
    NewUs := ToIntegerWithTruncationValue(VMicrosecond)
  else
    NewUs := LUs;
  HasRecognizedField := HasRecognizedField or not IsUndefinedValue(VMicrosecond);

  VMillisecond := Obj.GetProperty('millisecond');
  if not IsUndefinedValue(VMillisecond) then
    NewMs := ToIntegerWithTruncationValue(VMillisecond)
  else
    NewMs := LMs;
  HasRecognizedField := HasRecognizedField or not IsUndefinedValue(VMillisecond);

  VMinute := Obj.GetProperty('minute');
  if not IsUndefinedValue(VMinute) then
    NewMinute := ToIntegerWithTruncationValue(VMinute)
  else
    NewMinute := LMinute;
  HasRecognizedField := HasRecognizedField or not IsUndefinedValue(VMinute);

  VMonth := Obj.GetProperty(PROPERTY_MONTH);
  HasMonth := not IsUndefinedValue(VMonth);
  if HasMonth then
  begin
    NewMonth := ToIntegerWithTruncationValue(VMonth);
    SuppliedMonth := NewMonth;
  end
  else
  begin
    NewMonth := Info.Date.Month;
    SuppliedMonth := 0;
  end;
  HasRecognizedField := HasRecognizedField or HasMonth;

  VMonthCode := Obj.GetProperty(PROPERTY_MONTH_CODE);
  HasMonthCode := not IsUndefinedValue(VMonthCode);
  MonthCodeStr := '';
  if HasMonthCode then
    MonthCodeStr := MonthCodeStringFromProperty(VMonthCode,
      'ZonedDateTime.prototype.with');
  HasRecognizedField := HasRecognizedField or HasMonthCode;

  VNanosecond := Obj.GetProperty('nanosecond');
  if not IsUndefinedValue(VNanosecond) then
    NewNs := ToIntegerWithTruncationValue(VNanosecond)
  else
    NewNs := LNs;
  HasRecognizedField := HasRecognizedField or not IsUndefinedValue(VNanosecond);

  VOffset := Obj.GetProperty(PROPERTY_OFFSET);
  HasOffset := not IsUndefinedValue(VOffset);
  OffsetHasSubMinuteSyntax := False;
  if HasOffset then
    OffsetStr := OffsetStringFromProperty(VOffset, 'ZonedDateTime.prototype.with')
  else
    OffsetSeconds := 0;
  HasRecognizedField := HasRecognizedField or HasOffset;

  VSecond := Obj.GetProperty('second');
  if not IsUndefinedValue(VSecond) then
    NewSecond := ToIntegerWithTruncationValue(VSecond)
  else
    NewSecond := LSecond;
  HasRecognizedField := HasRecognizedField or not IsUndefinedValue(VSecond);

  VYear := Obj.GetProperty(PROPERTY_YEAR);
  if not IsUndefinedValue(VYear) then
    NewYear := ToIntegerWithTruncationValue(VYear)
  else
    NewYear := Info.Date.Year;
  HasRecognizedField := HasRecognizedField or not IsUndefinedValue(VYear);

  if (Zdt.FCalendarId <> 'iso8601') and IsUndefinedValue(VYear) then
  begin
    VEra := Obj.GetProperty('era');
    VEraYear := Obj.GetProperty('eraYear');
    HasRecognizedField := HasRecognizedField or not IsUndefinedValue(VEra) or
      not IsUndefinedValue(VEraYear);
    if (not IsUndefinedValue(VEra)) or (not IsUndefinedValue(VEraYear)) then
    begin
      if IsUndefinedValue(VEra) or IsUndefinedValue(VEraYear) or
         not TryCalendarYearFromEra(Zdt.FCalendarId, VEra.ToStringLiteral.Value,
           ToIntegerWithTruncationValue(VEraYear), NewYear) then
        ThrowTypeError('ZonedDateTime.prototype.with requires era and eraYear together',
          SSuggestTemporalFromArg);
    end;
  end;

  if not HasRecognizedField then
    ThrowTypeError(Format(SErrorTemporalWithRequiresObject, ['ZonedDateTime']),
      SSuggestTemporalWithObject);
  if (HasMonth and (SuppliedMonth < 0)) or (NewDay < 0) then
    ThrowRangeError(Format(SErrorTemporalInvalidISOStringFor, ['ZonedDateTime',
      'ZonedDateTime.prototype.with']), SSuggestTemporalDateRange);

  OptionsObj := RequireOptionsObject(AArgs.GetElement(1),
    'ZonedDateTime.prototype.with');
  Disambiguation := GetDisambiguationOption(OptionsObj,
    'ZonedDateTime.prototype.with');
  OffsetOption := GetWithOffsetOption(OptionsObj);
  Overflow := GetOverflowOption(OptionsObj);

  if HasOffset and
     ((not ParseZonedDateTimeOffsetPart(OffsetStr, OffsetSeconds,
       OffsetHasUTCDesignator, OffsetHasSubMinuteSyntax)) or
      OffsetHasUTCDesignator) then
    ThrowRangeError(Format(SErrorTemporalInvalidISOStringFor, ['ZonedDateTime',
      'ZonedDateTime.prototype.with']), SSuggestTemporalISOFormat);

  IsLeapMonth := False;
  UseMonthCode := False;
  if HasMonthCode then
  begin
    if not TryParseTemporalMonthCode(MonthCodeStr, NewMonth, IsLeapMonth) then
      ThrowRangeError(SErrorMonthCodeOutOfRange, SSuggestTemporalMonthCode);
    if (Zdt.FCalendarId = 'iso8601') and (IsLeapMonth or (NewMonth < 1) or
       (NewMonth > 12)) then
      ThrowRangeError(SErrorMonthCodeOutOfRange, SSuggestTemporalMonthCode);
    if HasMonth and (NewMonth <> SuppliedMonth) then
      ThrowRangeError(SErrorMonthCodeMismatch, SSuggestTemporalMonthCode);
    UseMonthCode := not HasMonth;
  end
  else if (not HasMonth) and (Zdt.FCalendarId <> 'iso8601') then
  begin
    MonthCodeStr := Info.MonthCode;
    if not TryParseTemporalMonthCode(MonthCodeStr, NewMonth, IsLeapMonth) then
      ThrowRangeError(Format(SErrorTemporalInvalidISOStringFor, ['ZonedDateTime',
        'ZonedDateTime.prototype.with']), SSuggestTemporalDateRange);
    UseMonthCode := True;
  end;

  if not TryResolveCalendarDateToISO(Zdt.FCalendarId, NewYear, NewMonth, NewDay,
    UseMonthCode, IsLeapMonth, Overflow = toConstrain, DateRec) then
    ThrowRangeError(Format(SErrorTemporalInvalidISOStringFor, ['ZonedDateTime',
      'ZonedDateTime.prototype.with']), SSuggestTemporalDateRange);

  if (not HasOffset) and (OffsetOption <> tooIgnore) then
  begin
    HasOffset := True;
    OffsetSeconds := GetUtcOffsetSeconds(Zdt.FTimeZone,
      EpochMillisecondsToSeconds(Zdt.FEpochMilliseconds));
  end;

  if Overflow = toConstrain then
    ConstrainTemporalTimeFields(NewHour, NewMinute, NewSecond, NewMs, NewUs,
      NewNs)
  else if not IsValidTime(NewHour, NewMinute, NewSecond, NewMs, NewUs, NewNs) then
    ThrowRangeError(SErrorInvalidISOTime, SSuggestTemporalDateRange);

  NewTime.Hour := NewHour;
  NewTime.Minute := NewMinute;
  NewTime.Second := NewSecond;
  NewTime.Millisecond := NewMs;
  NewTime.Microsecond := NewUs;
  NewTime.Nanosecond := NewNs;
  NewEpochMs := ApplyOffsetOption(DateRec.Year, DateRec.Month, DateRec.Day,
    NewTime, Zdt.FTimeZone, HasOffset, OffsetSeconds, False,
    OffsetHasSubMinuteSyntax, False, OffsetOption, Disambiguation,
    'ZonedDateTime.prototype.with');
  Result := TGocciaTemporalZonedDateTimeValue.Create(NewEpochMs, NewUs * 1000 + NewNs, Zdt.FTimeZone,
    Zdt.FCalendarId);
end;

// TC39 Temporal §6.3.24 Temporal.ZonedDateTime.prototype.withPlainTime([plainTimeLike])
function TGocciaTemporalZonedDateTimeValue.ZonedDateTimeWithPlainTime(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Zdt: TGocciaTemporalZonedDateTimeValue;
  Arg: TGocciaValue;
  Obj: TGocciaObjectValue;
  V: TGocciaValue;
  PlainTime: TGocciaTemporalPlainTimeValue;
  TimeRec: TTemporalTimeRecord;
  LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs: Integer;
  NewHour, NewMinute, NewSecond, NewMs, NewUs, NewNs: Integer;
  NewEpochMs: Int64;
  Seen: Boolean;

  procedure ReadTimeField(const AName: string; var ATarget: Integer);
  begin
    V := Obj.GetProperty(AName);
    if not IsUndefinedValue(V) then
    begin
      ATarget := ToIntegerWithTruncationValue(V);
      Seen := True;
    end;
  end;

begin
  Zdt := AsZonedDateTime(AThisValue, 'ZonedDateTime.prototype.withPlainTime');
  Arg := AArgs.GetElement(0);

  ComputeLocalComponents(Zdt, LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs);

  NewHour := 0; NewMinute := 0; NewSecond := 0; NewMs := 0; NewUs := 0; NewNs := 0;

  if Assigned(Arg) and not (Arg is TGocciaUndefinedLiteralValue) then
  begin
    if Arg is TGocciaTemporalPlainTimeValue then
    begin
      PlainTime := TGocciaTemporalPlainTimeValue(Arg);
      NewHour := PlainTime.Hour; NewMinute := PlainTime.Minute; NewSecond := PlainTime.Second;
      NewMs := PlainTime.Millisecond; NewUs := PlainTime.Microsecond; NewNs := PlainTime.Nanosecond;
    end
    else if Arg is TGocciaStringLiteralValue then
    begin
      if not TryParsePlainTimeLikeString(TGocciaStringLiteralValue(Arg).Value, TimeRec) then
        ThrowRangeError(SErrorInvalidTimeStringForZDT, SSuggestTemporalISOFormat);
      NewHour := TimeRec.Hour; NewMinute := TimeRec.Minute; NewSecond := TimeRec.Second;
      NewMs := TimeRec.Millisecond; NewUs := TimeRec.Microsecond; NewNs := TimeRec.Nanosecond;
    end
    else if Arg is TGocciaObjectValue then
    begin
      Obj := TGocciaObjectValue(Arg);
      Seen := False;
      ReadTimeField('hour', NewHour);
      ReadTimeField('microsecond', NewUs);
      ReadTimeField('millisecond', NewMs);
      ReadTimeField('minute', NewMinute);
      ReadTimeField('nanosecond', NewNs);
      ReadTimeField('second', NewSecond);
      if not Seen then
        ThrowTypeError(SErrorZDTWithPlainTimeArg, SSuggestTemporalFromArg);
      if NewSecond = 60 then
        NewSecond := 59;
      if not IsValidTime(NewHour, NewMinute, NewSecond, NewMs, NewUs, NewNs) then
        ThrowRangeError(SErrorInvalidISOTime, SSuggestTemporalDateRange);
    end
    else
      ThrowTypeError(SErrorZDTWithPlainTimeArg, SSuggestTemporalFromArg);
  end
  else
  begin
    NewEpochMs := StartOfDayToEpochMilliseconds(LYear, LMonth, LDay,
      Zdt.FTimeZone);
    Result := TGocciaTemporalZonedDateTimeValue.Create(NewEpochMs, 0,
      Zdt.FTimeZone, Zdt.FCalendarId);
    Exit;
  end;

  NewEpochMs := LocalToEpochMs(LYear, LMonth, LDay, NewHour, NewMinute, NewSecond, NewMs, Zdt.FTimeZone);
  Result := TGocciaTemporalZonedDateTimeValue.Create(NewEpochMs, NewUs * 1000 + NewNs, Zdt.FTimeZone,
    Zdt.FCalendarId);
end;

// TC39 Temporal §6.3.25 Temporal.ZonedDateTime.prototype.withTimeZone(timeZoneLike)
function TGocciaTemporalZonedDateTimeValue.ZonedDateTimeWithTimeZone(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Zdt: TGocciaTemporalZonedDateTimeValue;
  Arg: TGocciaValue;
  NewTimeZone: string;
begin
  Zdt := AsZonedDateTime(AThisValue, 'ZonedDateTime.prototype.withTimeZone');
  Arg := AArgs.GetElement(0);

  if (Arg = nil) or (Arg is TGocciaUndefinedLiteralValue) then
    ThrowTypeError(SErrorZDTWithTimeZoneArg, SSuggestTemporalTimezone);
  if not (Arg is TGocciaStringLiteralValue) then
    ThrowTypeError(SErrorZDTWithTimeZoneArg, SSuggestTemporalTimezone);

  NewTimeZone := TGocciaStringLiteralValue(Arg).Value;
  if NewTimeZone = '' then
    ThrowRangeError(SErrorZDTWithTimeZoneNonEmpty, SSuggestTemporalTimezone);
  NewTimeZone := CanonicalizeTimeZoneIdentifier(NewTimeZone);

  // Keep the same epoch instant, just change the timezone
  Result := TGocciaTemporalZonedDateTimeValue.Create(
    Zdt.FEpochMilliseconds, Zdt.FSubMillisecondNanoseconds, NewTimeZone, Zdt.FCalendarId);
end;

// TC39 Temporal §6.3.26 Temporal.ZonedDateTime.prototype.withCalendar(calendarLike)
function TGocciaTemporalZonedDateTimeValue.ZonedDateTimeWithCalendar(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Zdt: TGocciaTemporalZonedDateTimeValue;
  Arg: TGocciaValue;
  CalendarId: string;
begin
  Zdt := AsZonedDateTime(AThisValue, 'ZonedDateTime.prototype.withCalendar');
  Arg := AArgs.GetElement(0);
  if (Arg = nil) or (Arg is TGocciaUndefinedLiteralValue) then
    ThrowTypeError('ZonedDateTime.prototype.withCalendar requires a calendar argument', SSuggestTemporalFromArg);

  CalendarId := CalendarFromProperty(Arg);
  Result := TGocciaTemporalZonedDateTimeValue.Create(
    Zdt.FEpochMilliseconds, Zdt.FSubMillisecondNanoseconds, Zdt.FTimeZone, CalendarId);
end;

// TC39 Temporal §6.3.26 Temporal.ZonedDateTime.prototype.add(temporalDurationLike [, options])
function TGocciaTemporalZonedDateTimeValue.ZonedDateTimeAdd(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Zdt: TGocciaTemporalZonedDateTimeValue;
  Dur: TGocciaTemporalDurationValue;
  LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs: Integer;
  DateRec: TTemporalDateRecord;
  NewEpochMs: Int64;
  NewSubMs: Integer;
  NewEpochNs: TBigInteger;
  Arg: TGocciaValue;
  OptionsObj: TGocciaObjectValue;
  Overflow: TTemporalOverflow;
begin
  try
  Zdt := AsZonedDateTime(AThisValue, 'ZonedDateTime.prototype.add');
  Dur := CoerceDuration(AArgs.GetElement(0), 'ZonedDateTime.prototype.add');
  Arg := AArgs.GetElement(1);
  OptionsObj := RequireOptionsObject(Arg, 'ZonedDateTime.prototype.add');
  Overflow := GetOverflowOption(OptionsObj);

  if not DurationHasDateUnits(Dur) then
  begin
    NewEpochNs := EpochNanosecondsFromParts(Zdt.FEpochMilliseconds,
      Zdt.FSubMillisecondNanoseconds)
      .Add(DurationSubDayNanosecondsForZonedDateTime(Dur));
    SplitEpochNanoseconds(NewEpochNs, NewEpochMs, NewSubMs);
    Result := TGocciaTemporalZonedDateTimeValue.Create(NewEpochMs, NewSubMs,
      Zdt.FTimeZone, Zdt.FCalendarId);
    Exit;
  end;

  // Compute local wall-clock time
  ComputeLocalComponents(Zdt, LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs);

  if not TryAddCalendarDate(Zdt.FCalendarId, LYear, LMonth, LDay,
    Dur.Years, Dur.Months, Dur.Weeks, Dur.Days,
    Overflow = toConstrain, DateRec) then
    ThrowRangeError(Format(SErrorTemporalInvalidISOStringFor, ['date-time',
      'ZonedDateTime.prototype.add']), SSuggestTemporalDateRange);

  // Calendar units are resolved in local time; sub-day units are then exact.
  NewEpochMs := LocalToEpochMs(DateRec.Year, DateRec.Month, DateRec.Day,
    LHour, LMinute, LSecond, LMs, Zdt.FTimeZone);
  NewSubMs := LUs * 1000 + LNs;
  NewEpochNs := EpochNanosecondsFromParts(NewEpochMs, NewSubMs)
    .Add(DurationSubDayNanosecondsForZonedDateTime(Dur));
  SplitEpochNanoseconds(NewEpochNs, NewEpochMs, NewSubMs);

  Result := TGocciaTemporalZonedDateTimeValue.Create(NewEpochMs, NewSubMs,
    Zdt.FTimeZone, Zdt.FCalendarId);
  except
    on E: ETemporalDurationInt64Overflow do
      ThrowRangeError(E.Message, SSuggestTemporalDurationRange);
  end;
end;

// TC39 Temporal §6.3.27 Temporal.ZonedDateTime.prototype.subtract(temporalDurationLike [, options])
function TGocciaTemporalZonedDateTimeValue.ZonedDateTimeSubtract(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Dur: TGocciaTemporalDurationValue;
  NegDur: TGocciaTemporalDurationValue;
  NewArgs: TGocciaArgumentsCollection;
begin
  try
  Dur := CoerceDuration(AArgs.GetElement(0), 'ZonedDateTime.prototype.subtract');

  NegDur := TGocciaTemporalDurationValue.CreateFromBigIntegers(
    Dur.YearsBig.Negate, Dur.MonthsBig.Negate, Dur.WeeksBig.Negate,
    Dur.DaysBig.Negate, Dur.HoursBig.Negate, Dur.MinutesBig.Negate,
    Dur.SecondsBig.Negate, Dur.MillisecondsBig.Negate,
    Dur.MicrosecondsBig.Negate, Dur.NanosecondsBig.Negate);

  NewArgs := TGocciaArgumentsCollection.Create([NegDur, AArgs.GetElement(1)]);
  try
    Result := ZonedDateTimeAdd(NewArgs, AThisValue);
  finally
    NewArgs.Free;
  end;
  except
    on E: ETemporalDurationInt64Overflow do
      ThrowRangeError(E.Message, SSuggestTemporalDurationRange);
  end;
end;

function ZonedDiffToUnits(const ADiffNs: TBigInteger;
  const ALargestUnit: TTemporalUnit): TGocciaValue;
var
  Hours, Minutes, Seconds, Milliseconds, Microseconds, Nanoseconds: TBigInteger;
  Days: Int64;

  function ToFloat64Representable(const AValue: TBigInteger): TBigInteger;
  begin
    Result := TBigInteger.FromDouble(AValue.ToDouble);
  end;

begin
  BalanceTimeDurationToFields(ADiffNs, ALargestUnit,
    Hours, Minutes, Seconds, Milliseconds, Microseconds, Nanoseconds, Days);
  Result := TGocciaTemporalDurationValue.CreateFromBigIntegers(
    TBigInteger.Zero, TBigInteger.Zero, TBigInteger.Zero, TBigInteger.FromInt64(Days),
    ToFloat64Representable(Hours), ToFloat64Representable(Minutes),
    ToFloat64Representable(Seconds), ToFloat64Representable(Milliseconds),
    ToFloat64Representable(Microseconds), ToFloat64Representable(Nanoseconds));
end;

procedure ZeroZonedDateTimeDiffFields(
  out AYears, AMonths, AWeeks, ADays,
      AHours, AMinutes, ASeconds,
      AMilliseconds, AMicroseconds, ANanoseconds: Int64);
begin
  AYears := 0;
  AMonths := 0;
  AWeeks := 0;
  ADays := 0;
  AHours := 0;
  AMinutes := 0;
  ASeconds := 0;
  AMilliseconds := 0;
  AMicroseconds := 0;
  ANanoseconds := 0;
end;

procedure SplitEpochNanoseconds(const AEpochNanoseconds: TBigInteger;
  out AEpochMilliseconds: Int64; out ASubMillisecondNanoseconds: Integer);
var
  UnitNs, MillisecondsBig, SubMillisecondBig: TBigInteger;
begin
  UnitNs := TBigInteger.FromInt64(NANOSECONDS_PER_MILLISECOND);
  MillisecondsBig := AEpochNanoseconds.Divide(UnitNs);
  SubMillisecondBig := AEpochNanoseconds.Modulo(UnitNs);
  if SubMillisecondBig.IsNegative then
  begin
    MillisecondsBig := MillisecondsBig.Subtract(TBigInteger.One);
    SubMillisecondBig := SubMillisecondBig.Add(UnitNs);
  end;

  AEpochMilliseconds := MillisecondsBig.ToInt64;
  ASubMillisecondNanoseconds := Integer(SubMillisecondBig.ToInt64);
end;

function GetZonedBoundaryEpochNanoseconds(const AStartYear, AStartMonth,
  AStartDay, AStartHour, AStartMinute, AStartSecond, AStartMillisecond: Integer;
  const AStartSubMillisecondNanoseconds: Integer; const ACalendarId,
  ATimeZone: string; const AUnit: TTemporalUnit; const AUnitCount: Int64): TBigInteger;
var
  BoundaryDate: TTemporalDateRecord;
  BoundaryEpochMs: Int64;
begin
  case AUnit of
    tuYear:
      begin
        if (ACalendarId <> 'iso8601') and
           not TryAddCalendarDate(ACalendarId, AStartYear, AStartMonth,
             AStartDay, AUnitCount, 0, 0, 0, True, BoundaryDate) then
          ThrowRangeError('Unable to resolve calendar date for ' + ACalendarId,
            SSuggestTemporalDateRange);
        if ACalendarId = 'iso8601' then
          BoundaryDate := AddMonthsToDate(AStartYear, AStartMonth, AStartDay,
            AUnitCount * MONTHS_PER_YEAR);
      end;
    tuMonth:
      begin
        if (ACalendarId <> 'iso8601') and
           not TryAddCalendarDate(ACalendarId, AStartYear, AStartMonth,
             AStartDay, 0, AUnitCount, 0, 0, True, BoundaryDate) then
          ThrowRangeError('Unable to resolve calendar date for ' + ACalendarId,
            SSuggestTemporalDateRange);
        if ACalendarId = 'iso8601' then
          BoundaryDate := AddMonthsToDate(AStartYear, AStartMonth, AStartDay,
            AUnitCount);
      end;
    tuWeek:
      BoundaryDate := AddDaysToDate(AStartYear, AStartMonth, AStartDay,
        AUnitCount * DAYS_PER_WEEK);
    tuDay:
      BoundaryDate := AddDaysToDate(AStartYear, AStartMonth, AStartDay, AUnitCount);
  else
    BoundaryDate.Year := AStartYear;
    BoundaryDate.Month := AStartMonth;
    BoundaryDate.Day := AStartDay;
  end;

  BoundaryEpochMs := LocalToEpochMs(BoundaryDate.Year, BoundaryDate.Month,
    BoundaryDate.Day, AStartHour, AStartMinute, AStartSecond,
    AStartMillisecond, ATimeZone);
  ValidateZonedDateTimeEpoch(BoundaryEpochMs, AStartSubMillisecondNanoseconds,
    'ZonedDateTime.prototype.until');
  Result := EpochNanosecondsFromParts(BoundaryEpochMs, AStartSubMillisecondNanoseconds);
end;

function GetAdjustedEndDateForWholeUnits(const ASign: Integer;
  const AEndYear, AEndMonth, AEndDay, AEndHour, AEndMinute, AEndSecond,
  AEndMillisecond, AEndMicrosecond, AEndNanosecond: Integer;
  const AStartHour, AStartMinute, AStartSecond, AStartMillisecond,
  AStartMicrosecond, AStartNanosecond: Integer): TTemporalDateRecord;
var
  TimeComparison: Integer;
begin
  Result.Year := AEndYear;
  Result.Month := AEndMonth;
  Result.Day := AEndDay;

  TimeComparison := CompareTimes(AEndHour, AEndMinute, AEndSecond,
    AEndMillisecond, AEndMicrosecond, AEndNanosecond,
    AStartHour, AStartMinute, AStartSecond, AStartMillisecond,
    AStartMicrosecond, AStartNanosecond);
  if (ASign > 0) and (TimeComparison < 0) then
    Result := AddDaysToDate(AEndYear, AEndMonth, AEndDay, -1)
  else if (ASign < 0) and (TimeComparison > 0) then
    Result := AddDaysToDate(AEndYear, AEndMonth, AEndDay, 1);
end;

function GetWholeCalendarUnitsBetween(
  const ACalendarId: string;
  const AStartYear, AStartMonth, AStartDay, AStartHour, AStartMinute,
  AStartSecond, AStartMillisecond, AStartMicrosecond, AStartNanosecond: Integer;
  const AEndYear, AEndMonth, AEndDay, AEndHour, AEndMinute, AEndSecond,
  AEndMillisecond, AEndMicrosecond, AEndNanosecond: Integer;
  const ASign: Integer; const AUnit: TTemporalUnit): Int64;
var
  AdjustedEndDate: TTemporalDateRecord;
  Years, Months, Weeks, Days: Int64;
  TotalDays: Int64;
begin
  AdjustedEndDate := GetAdjustedEndDateForWholeUnits(ASign,
    AEndYear, AEndMonth, AEndDay, AEndHour, AEndMinute, AEndSecond,
    AEndMillisecond, AEndMicrosecond, AEndNanosecond,
    AStartHour, AStartMinute, AStartSecond, AStartMillisecond,
    AStartMicrosecond, AStartNanosecond);

  case AUnit of
    tuYear:
    begin
      CalendarDateUntil(ACalendarId, AStartYear, AStartMonth, AStartDay,
        AdjustedEndDate.Year, AdjustedEndDate.Month, AdjustedEndDate.Day,
        tuYear, Years, Months, Weeks, Days);
      Result := Years;
    end;
    tuMonth:
    begin
      CalendarDateUntil(ACalendarId, AStartYear, AStartMonth, AStartDay,
        AdjustedEndDate.Year, AdjustedEndDate.Month, AdjustedEndDate.Day,
        tuMonth, Years, Months, Weeks, Days);
      Result := Months;
    end;
    tuWeek:
    begin
      TotalDays := DateToEpochDays(AdjustedEndDate.Year, AdjustedEndDate.Month,
        AdjustedEndDate.Day) - DateToEpochDays(AStartYear, AStartMonth, AStartDay);
      Result := TotalDays div DAYS_PER_WEEK;
    end;
    tuDay:
    begin
      Result := DateToEpochDays(AdjustedEndDate.Year, AdjustedEndDate.Month,
        AdjustedEndDate.Day) - DateToEpochDays(AStartYear, AStartMonth, AStartDay);
    end;
  else
    Result := 0;
  end;
end;

function SelectRoundedCalendarUnits(const AScaledValue: Int64;
  const AIncrement, ASign: Integer; const APosition, APeriod: TBigInteger;
  const AMode: TTemporalRoundingMode): Int64;
var
  AwayFromZero, Comparison: Int64;
  TwicePosition: TBigInteger;
begin
  AwayFromZero := AScaledValue + Int64(ASign) * AIncrement;
  Result := AScaledValue;
  if APosition.IsZero then
    Exit(AScaledValue);

  case AMode of
    rmTrunc:
      Exit(AScaledValue);
    rmExpand:
      Exit(AwayFromZero);
    rmCeil:
    begin
      if ASign > 0 then
        Exit(AwayFromZero)
      else
        Exit(AScaledValue);
    end;
    rmFloor:
    begin
      if ASign < 0 then
        Exit(AwayFromZero)
      else
        Exit(AScaledValue);
    end;
  else
    ;
  end;

  TwicePosition := APosition.Multiply(TBigInteger.FromInt64(2));
  Comparison := TwicePosition.Compare(APeriod);

  case AMode of
    rmHalfExpand:
      if Comparison >= 0 then Result := AwayFromZero;
    rmHalfTrunc:
      if Comparison > 0 then Result := AwayFromZero;
    rmHalfCeil:
    begin
      if Comparison > 0 then
        Result := AwayFromZero
      else if (Comparison = 0) and (ASign > 0) then
        Result := AwayFromZero;
    end;
    rmHalfFloor:
    begin
      if Comparison > 0 then
        Result := AwayFromZero
      else if (Comparison = 0) and (ASign < 0) then
        Result := AwayFromZero;
    end;
    rmHalfEven:
    begin
      if Comparison > 0 then
        Result := AwayFromZero
      else if (Comparison = 0) and Odd(AScaledValue div AIncrement) then
        Result := AwayFromZero;
    end;
  end;
end;

function ZonedDateTimeDiffIsExactAtLargestUnit(const ALargestUnit: TTemporalUnit;
  const AYears, AMonths, AWeeks, ADays, AHours, AMinutes, ASeconds,
  AMilliseconds, AMicroseconds, ANanoseconds: Int64): Boolean;
begin
  Result := False;
  case ALargestUnit of
    tuYear:
      Result := (AMonths = 0) and (AWeeks = 0) and (ADays = 0);
    tuMonth:
      Result := (AWeeks = 0) and (ADays = 0);
    tuWeek:
      Result := ADays = 0;
    tuDay:
      Result := True;
  else
    Exit;
  end;
  Result := Result and (AHours = 0) and (AMinutes = 0) and (ASeconds = 0) and
    (AMilliseconds = 0) and (AMicroseconds = 0) and (ANanoseconds = 0);
end;

procedure ComputeZonedDateTimeDiffFields(
  const AStartEpochMilliseconds: Int64; const AStartSubMillisecondNanoseconds: Integer;
  const AEndEpochMilliseconds: Int64; const AEndSubMillisecondNanoseconds: Integer;
  const ATimeZone, ACalendarId: string; const ALargestUnit: TTemporalUnit;
  out AYears, AMonths, AWeeks, ADays,
      AHours, AMinutes, ASeconds,
      AMilliseconds, AMicroseconds, ANanoseconds: Int64);
var
  StartNs, EndNs, IntermediateNs, RemainderBig: TBigInteger;
  CorrectionSign, RemainderSign: Integer;
  StartYear, StartMonth, StartDay, StartHour, StartMinute, StartSecond: Integer;
  StartMillisecond, StartMicrosecond, StartNanosecond: Integer;
  EndYear, EndMonth, EndDay, EndHour, EndMinute, EndSecond: Integer;
  EndMillisecond, EndMicrosecond, EndNanosecond: Integer;
  CandidateDate, IntermediateDate: TTemporalDateRecord;
  IntermediateEpochMs, RemainderNs: Int64;
  DayCorrection, MaxDayCorrection: Integer;
begin
  ZeroZonedDateTimeDiffFields(AYears, AMonths, AWeeks, ADays,
    AHours, AMinutes, ASeconds, AMilliseconds, AMicroseconds, ANanoseconds);

  StartNs := EpochNanosecondsFromParts(AStartEpochMilliseconds,
    AStartSubMillisecondNanoseconds);
  EndNs := EpochNanosecondsFromParts(AEndEpochMilliseconds,
    AEndSubMillisecondNanoseconds);
  if EndNs.Compare(StartNs) = 0 then Exit;

  if EndNs.Compare(StartNs) < 0 then
    CorrectionSign := 1
  else
    CorrectionSign := -1;
  if CorrectionSign = -1 then
    MaxDayCorrection := 2
  else
    MaxDayCorrection := 1;

  ComputeLocalComponentsFromEpoch(AStartEpochMilliseconds,
    AStartSubMillisecondNanoseconds, ATimeZone,
    StartYear, StartMonth, StartDay, StartHour, StartMinute, StartSecond,
    StartMillisecond, StartMicrosecond, StartNanosecond);
  ComputeLocalComponentsFromEpoch(AEndEpochMilliseconds,
    AEndSubMillisecondNanoseconds, ATimeZone,
    EndYear, EndMonth, EndDay, EndHour, EndMinute, EndSecond,
    EndMillisecond, EndMicrosecond, EndNanosecond);

  if CompareDates(StartYear, StartMonth, StartDay, EndYear, EndMonth, EndDay) = 0 then
  begin
    RemainderBig := EndNs.Subtract(StartNs);
    if RemainderBig.IsNegative then
      RemainderSign := -1
    else
      RemainderSign := 1;
    RemainderNs := RemainderBig.AbsValue.ToInt64;

    AHours := RemainderSign * (RemainderNs div NANOSECONDS_PER_HOUR);
    RemainderNs := RemainderNs mod NANOSECONDS_PER_HOUR;
    AMinutes := RemainderSign * (RemainderNs div NANOSECONDS_PER_MINUTE);
    RemainderNs := RemainderNs mod NANOSECONDS_PER_MINUTE;
    ASeconds := RemainderSign * (RemainderNs div NANOSECONDS_PER_SECOND);
    RemainderNs := RemainderNs mod NANOSECONDS_PER_SECOND;
    AMilliseconds := RemainderSign * (RemainderNs div NANOSECONDS_PER_MILLISECOND);
    RemainderNs := RemainderNs mod NANOSECONDS_PER_MILLISECOND;
    AMicroseconds := RemainderSign * (RemainderNs div NANOSECONDS_PER_MICROSECOND);
    ANanoseconds := RemainderSign * (RemainderNs mod NANOSECONDS_PER_MICROSECOND);
    Exit;
  end;

  DayCorrection := 0;
  if CompareTimes(EndHour, EndMinute, EndSecond, EndMillisecond,
    EndMicrosecond, EndNanosecond, StartHour, StartMinute, StartSecond,
    StartMillisecond, StartMicrosecond, StartNanosecond) = CorrectionSign then
    Inc(DayCorrection);

  RemainderBig := TBigInteger.Zero;
  IntermediateDate.Year := EndYear;
  IntermediateDate.Month := EndMonth;
  IntermediateDate.Day := EndDay;
  while DayCorrection <= MaxDayCorrection do
  begin
    CandidateDate := AddDaysToDate(EndYear, EndMonth, EndDay,
      Int64(DayCorrection) * CorrectionSign);
    IntermediateEpochMs := LocalToEpochMs(CandidateDate.Year,
      CandidateDate.Month, CandidateDate.Day, StartHour, StartMinute,
      StartSecond, StartMillisecond, ATimeZone);
    IntermediateNs := EpochNanosecondsFromParts(IntermediateEpochMs,
      AStartSubMillisecondNanoseconds);

    RemainderBig := EndNs.Subtract(IntermediateNs);
    if RemainderBig.IsZero then
    begin
      IntermediateDate := CandidateDate;
      Break;
    end;

    if RemainderBig.IsNegative then
      RemainderSign := -1
    else
      RemainderSign := 1;
    if RemainderSign <> CorrectionSign then
    begin
      IntermediateDate := CandidateDate;
      Break;
    end;
    Inc(DayCorrection);
  end;

  CalendarDateUntil(ACalendarId, StartYear, StartMonth, StartDay,
    IntermediateDate.Year, IntermediateDate.Month, IntermediateDate.Day,
    ALargestUnit, AYears, AMonths, AWeeks, ADays);
  if RemainderBig.IsZero then Exit;

  if RemainderBig.IsNegative then
    RemainderSign := -1
  else
    RemainderSign := 1;
  RemainderNs := RemainderBig.AbsValue.ToInt64;

  AHours := RemainderSign * (RemainderNs div NANOSECONDS_PER_HOUR);
  RemainderNs := RemainderNs mod NANOSECONDS_PER_HOUR;
  AMinutes := RemainderSign * (RemainderNs div NANOSECONDS_PER_MINUTE);
  RemainderNs := RemainderNs mod NANOSECONDS_PER_MINUTE;
  ASeconds := RemainderSign * (RemainderNs div NANOSECONDS_PER_SECOND);
  RemainderNs := RemainderNs mod NANOSECONDS_PER_SECOND;
  AMilliseconds := RemainderSign * (RemainderNs div NANOSECONDS_PER_MILLISECOND);
  RemainderNs := RemainderNs mod NANOSECONDS_PER_MILLISECOND;
  AMicroseconds := RemainderSign * (RemainderNs div NANOSECONDS_PER_MICROSECOND);
  ANanoseconds := RemainderSign * (RemainderNs mod NANOSECONDS_PER_MICROSECOND);
end;

procedure RoundZonedDateTimeDiffFields(
  const AStartEpochMilliseconds: Int64; const AStartSubMillisecondNanoseconds: Integer;
  const AEndEpochMilliseconds: Int64; const AEndSubMillisecondNanoseconds: Integer;
  const ATimeZone, ACalendarId: string; const ALargestUnit, ASmallestUnit: TTemporalUnit;
  const AMode: TTemporalRoundingMode; const AIncrement: Integer;
  out AYears, AMonths, AWeeks, ADays,
      AHours, AMinutes, ASeconds,
      AMilliseconds, AMicroseconds, ANanoseconds: Int64);
var
  StartNs, EndNs, DiffNs, RoundedEndNs: TBigInteger;
  IncrementNs, WalkNs, NextNs, PositionNs, PeriodNs: TBigInteger;
  StartYear, StartMonth, StartDay, StartHour, StartMinute, StartSecond: Integer;
  StartMillisecond, StartMicrosecond, StartNanosecond: Integer;
  EndYear, EndMonth, EndDay, EndHour, EndMinute, EndSecond: Integer;
  EndMillisecond, EndMicrosecond, EndNanosecond: Integer;
  RoundedEpochMs: Int64;
  RoundedSubMillisecondNs: Integer;
  Sign: Integer;
  WholeUnits, ScaledUnits, RoundedUnits, LargestUnits: Int64;
begin
  StartNs := EpochNanosecondsFromParts(AStartEpochMilliseconds,
    AStartSubMillisecondNanoseconds);
  EndNs := EpochNanosecondsFromParts(AEndEpochMilliseconds,
    AEndSubMillisecondNanoseconds);
  if EndNs.Compare(StartNs) = 0 then
  begin
    ZeroZonedDateTimeDiffFields(AYears, AMonths, AWeeks, ADays,
      AHours, AMinutes, ASeconds, AMilliseconds, AMicroseconds, ANanoseconds);
    Exit;
  end;

  if EndNs.Compare(StartNs) > 0 then
    Sign := 1
  else
    Sign := -1;

  ComputeZonedDateTimeDiffFields(AStartEpochMilliseconds,
    AStartSubMillisecondNanoseconds, AEndEpochMilliseconds,
    AEndSubMillisecondNanoseconds, ATimeZone, ACalendarId, ALargestUnit,
    AYears, AMonths, AWeeks, ADays, AHours, AMinutes, ASeconds,
    AMilliseconds, AMicroseconds, ANanoseconds);
  if ZonedDateTimeDiffIsExactAtLargestUnit(ALargestUnit, AYears, AMonths,
    AWeeks, ADays, AHours, AMinutes, ASeconds, AMilliseconds, AMicroseconds,
    ANanoseconds) then
    Exit;

  if Ord(ASmallestUnit) >= Ord(tuHour) then
  begin
    DiffNs := EndNs.Subtract(StartNs);
    IncrementNs := TBigInteger.FromInt64(UnitToNanoseconds(ASmallestUnit))
      .Multiply(TBigInteger.FromInt64(AIncrement));
    RoundedEndNs := StartNs.Add(RoundBigIntWithMode(DiffNs, IncrementNs, AMode));
  end
  else
  begin
    ComputeLocalComponentsFromEpoch(AStartEpochMilliseconds,
      AStartSubMillisecondNanoseconds, ATimeZone,
      StartYear, StartMonth, StartDay, StartHour, StartMinute, StartSecond,
      StartMillisecond, StartMicrosecond, StartNanosecond);
    ComputeLocalComponentsFromEpoch(AEndEpochMilliseconds,
      AEndSubMillisecondNanoseconds, ATimeZone,
      EndYear, EndMonth, EndDay, EndHour, EndMinute, EndSecond,
      EndMillisecond, EndMicrosecond, EndNanosecond);

    LargestUnits := GetWholeCalendarUnitsBetween(ACalendarId,
      StartYear, StartMonth, StartDay, StartHour, StartMinute, StartSecond,
      StartMillisecond, StartMicrosecond, StartNanosecond,
      EndYear, EndMonth, EndDay, EndHour, EndMinute, EndSecond,
      EndMillisecond, EndMicrosecond, EndNanosecond, Sign, ALargestUnit);
    if GetZonedBoundaryEpochNanoseconds(StartYear, StartMonth, StartDay,
      StartHour, StartMinute, StartSecond, StartMillisecond,
      AStartSubMillisecondNanoseconds, ACalendarId, ATimeZone, ALargestUnit,
      LargestUnits).Compare(EndNs) = 0 then
    begin
      ZeroZonedDateTimeDiffFields(AYears, AMonths, AWeeks, ADays,
        AHours, AMinutes, ASeconds, AMilliseconds, AMicroseconds,
        ANanoseconds);
      case ALargestUnit of
        tuYear:  AYears := LargestUnits;
        tuMonth: AMonths := LargestUnits;
        tuWeek:  AWeeks := LargestUnits;
        tuDay:   ADays := LargestUnits;
      else
        ;
      end;
      Exit;
    end;

    WholeUnits := GetWholeCalendarUnitsBetween(ACalendarId,
      StartYear, StartMonth, StartDay, StartHour, StartMinute, StartSecond,
      StartMillisecond, StartMicrosecond, StartNanosecond,
      EndYear, EndMonth, EndDay, EndHour, EndMinute, EndSecond,
      EndMillisecond, EndMicrosecond, EndNanosecond, Sign, ASmallestUnit);
    if Sign > 0 then
      ScaledUnits := (WholeUnits div AIncrement) * AIncrement
    else
      ScaledUnits := -(((-WholeUnits) div AIncrement) * AIncrement);

    WalkNs := GetZonedBoundaryEpochNanoseconds(StartYear, StartMonth, StartDay,
      StartHour, StartMinute, StartSecond, StartMillisecond,
      AStartSubMillisecondNanoseconds, ACalendarId, ATimeZone, ASmallestUnit,
      ScaledUnits);
    NextNs := GetZonedBoundaryEpochNanoseconds(StartYear, StartMonth, StartDay,
      StartHour, StartMinute, StartSecond, StartMillisecond,
      AStartSubMillisecondNanoseconds, ACalendarId, ATimeZone, ASmallestUnit,
      ScaledUnits + Int64(Sign) * AIncrement);
    PositionNs := EndNs.Subtract(WalkNs).AbsValue;
    PeriodNs := NextNs.Subtract(WalkNs).AbsValue;
    RoundedUnits := SelectRoundedCalendarUnits(ScaledUnits, AIncrement, Sign,
      PositionNs, PeriodNs, AMode);
    if ALargestUnit = ASmallestUnit then
    begin
      ZeroZonedDateTimeDiffFields(AYears, AMonths, AWeeks, ADays,
        AHours, AMinutes, ASeconds, AMilliseconds, AMicroseconds,
        ANanoseconds);
      case ASmallestUnit of
        tuYear:  AYears := RoundedUnits;
        tuMonth: AMonths := RoundedUnits;
        tuWeek:  AWeeks := RoundedUnits;
        tuDay:   ADays := RoundedUnits;
      else
        ;
      end;
      Exit;
    end;
    RoundedEndNs := GetZonedBoundaryEpochNanoseconds(StartYear, StartMonth,
      StartDay, StartHour, StartMinute, StartSecond, StartMillisecond,
      AStartSubMillisecondNanoseconds, ACalendarId, ATimeZone, ASmallestUnit,
      RoundedUnits);
  end;

  SplitEpochNanoseconds(RoundedEndNs, RoundedEpochMs, RoundedSubMillisecondNs);
  ComputeZonedDateTimeDiffFields(AStartEpochMilliseconds,
    AStartSubMillisecondNanoseconds, RoundedEpochMs, RoundedSubMillisecondNs,
    ATimeZone, ACalendarId, ALargestUnit, AYears, AMonths, AWeeks, ADays,
    AHours, AMinutes, ASeconds, AMilliseconds, AMicroseconds, ANanoseconds);
end;

procedure ValidateZonedDateTimeDiffRoundingIncrement(const AIncrement: Integer;
  const ASmallestUnit, ALargestUnit: TTemporalUnit); forward;

// TC39 Temporal §6.3.28 Temporal.ZonedDateTime.prototype.until(other [, options])
function TGocciaTemporalZonedDateTimeValue.ZonedDateTimeUntil(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Zdt, Other: TGocciaTemporalZonedDateTimeValue;
  OptionsObj: TGocciaObjectValue;
  LargestUnit, SmallestUnit: TTemporalUnit;
  RMode: TTemporalRoundingMode;
  RIncrement: Integer;
  StartNs, EndNs, DiffNs, IncrementNs: TBigInteger;
  Years, Months, Weeks, Days: Int64;
  RH, RM, RS, RMs, RUs, RNs: Int64;
begin
  Zdt := AsZonedDateTime(AThisValue, 'ZonedDateTime.prototype.until');
  Other := CoerceTemporalZonedDateTime(AArgs.GetElement(0), 'ZonedDateTime.prototype.until');

  OptionsObj := GetDiffOptions(AArgs, 1);
  LargestUnit := GetLargestUnit(OptionsObj, tuAuto);
  RIncrement := GetRoundingIncrement(OptionsObj, 1);
  RMode := GetRoundingMode(OptionsObj, rmTrunc);
  SmallestUnit := GetSmallestUnit(OptionsObj, tuNanosecond);
  if LargestUnit = tuAuto then
  begin
    if Ord(tuHour) > Ord(SmallestUnit) then
      LargestUnit := SmallestUnit
    else
      LargestUnit := tuHour;
  end;
  if Ord(LargestUnit) > Ord(SmallestUnit) then
    ThrowRangeError(SErrorDurationRoundLargestSmallerThanSmallest, SSuggestTemporalRoundArg);
  ValidateZonedDateTimeDiffRoundingIncrement(RIncrement, SmallestUnit,
    LargestUnit);

  if Zdt.FCalendarId <> Other.FCalendarId then
    ThrowRangeError('Temporal.ZonedDateTime calendars must match',
      SSuggestTemporalDateRange);

  if LargestUnit in [tuYear, tuMonth, tuWeek, tuDay] then
  begin
    if not TimeZoneIdentifiersEqual(Zdt.FTimeZone, Other.FTimeZone) then
      ThrowRangeError('Temporal.ZonedDateTime time zones must match',
        SSuggestTemporalTimezone);
    if (SmallestUnit <> tuNanosecond) or (RIncrement <> 1) then
      RoundZonedDateTimeDiffFields(Zdt.FEpochMilliseconds,
        Zdt.FSubMillisecondNanoseconds, Other.FEpochMilliseconds,
        Other.FSubMillisecondNanoseconds, Zdt.FTimeZone, Zdt.FCalendarId, LargestUnit,
        SmallestUnit, RMode, RIncrement, Years, Months, Weeks, Days,
        RH, RM, RS, RMs, RUs, RNs)
    else
      ComputeZonedDateTimeDiffFields(Zdt.FEpochMilliseconds,
        Zdt.FSubMillisecondNanoseconds, Other.FEpochMilliseconds,
        Other.FSubMillisecondNanoseconds, Zdt.FTimeZone, Zdt.FCalendarId, LargestUnit,
        Years, Months, Weeks, Days, RH, RM, RS, RMs, RUs, RNs);

    Result := TGocciaTemporalDurationValue.Create(Years, Months, Weeks, Days,
      RH, RM, RS, RMs, RUs, RNs);
  end
  else
  begin
    // Sub-day: use exact epoch nanosecond difference.
    StartNs := EpochNanosecondsFromParts(Zdt.FEpochMilliseconds, Zdt.FSubMillisecondNanoseconds);
    EndNs := EpochNanosecondsFromParts(Other.FEpochMilliseconds, Other.FSubMillisecondNanoseconds);
    DiffNs := TimeDurationFromEpochNanosecondsDifference(EndNs, StartNs);
    if (SmallestUnit <> tuNanosecond) or (RIncrement <> 1) then
    begin
      IncrementNs := TBigInteger.FromInt64(UnitToNanoseconds(SmallestUnit))
        .Multiply(TBigInteger.FromInt64(RIncrement));
      DiffNs := RoundTimeDurationToIncrement(DiffNs, IncrementNs, RMode);
    end;
    Result := ZonedDiffToUnits(DiffNs, LargestUnit);
  end;
end;

// TC39 Temporal §6.3.29 Temporal.ZonedDateTime.prototype.since(other [, options])
function InvertSinceRoundingMode(const AMode: TTemporalRoundingMode): TTemporalRoundingMode;
begin
  case AMode of
    rmCeil: Result := rmFloor;
    rmFloor: Result := rmCeil;
    rmHalfCeil: Result := rmHalfFloor;
    rmHalfFloor: Result := rmHalfCeil;
  else
    Result := AMode;
  end;
end;

function TGocciaTemporalZonedDateTimeValue.ZonedDateTimeSince(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Zdt, Other: TGocciaTemporalZonedDateTimeValue;
  OptionsObj: TGocciaObjectValue;
  LargestUnit, SmallestUnit: TTemporalUnit;
  RMode: TTemporalRoundingMode;
  RIncrement: Integer;
  StartNs, EndNs, DiffNs, IncrementNs: TBigInteger;
  Years, Months, Weeks, Days: Int64;
  RH, RM, RS, RMs, RUs, RNs: Int64;
begin
  Zdt := AsZonedDateTime(AThisValue, 'ZonedDateTime.prototype.since');
  Other := CoerceTemporalZonedDateTime(AArgs.GetElement(0), 'ZonedDateTime.prototype.since');

  OptionsObj := GetDiffOptions(AArgs, 1);
  LargestUnit := GetLargestUnit(OptionsObj, tuAuto);
  RIncrement := GetRoundingIncrement(OptionsObj, 1);
  RMode := GetRoundingMode(OptionsObj, rmTrunc);
  RMode := InvertSinceRoundingMode(RMode);
  SmallestUnit := GetSmallestUnit(OptionsObj, tuNanosecond);
  if LargestUnit = tuAuto then
  begin
    if Ord(tuHour) > Ord(SmallestUnit) then
      LargestUnit := SmallestUnit
    else
      LargestUnit := tuHour;
  end;
  if Ord(LargestUnit) > Ord(SmallestUnit) then
    ThrowRangeError(SErrorDurationRoundLargestSmallerThanSmallest, SSuggestTemporalRoundArg);
  ValidateZonedDateTimeDiffRoundingIncrement(RIncrement, SmallestUnit,
    LargestUnit);

  if Zdt.FCalendarId <> Other.FCalendarId then
    ThrowRangeError('Temporal.ZonedDateTime calendars must match',
      SSuggestTemporalDateRange);

  if LargestUnit in [tuYear, tuMonth, tuWeek, tuDay] then
  begin
    if not TimeZoneIdentifiersEqual(Zdt.FTimeZone, Other.FTimeZone) then
      ThrowRangeError('Temporal.ZonedDateTime time zones must match',
        SSuggestTemporalTimezone);
    if (SmallestUnit <> tuNanosecond) or (RIncrement <> 1) then
      RoundZonedDateTimeDiffFields(Zdt.FEpochMilliseconds,
        Zdt.FSubMillisecondNanoseconds, Other.FEpochMilliseconds,
        Other.FSubMillisecondNanoseconds, Zdt.FTimeZone, Zdt.FCalendarId, LargestUnit,
        SmallestUnit, RMode, RIncrement, Years, Months, Weeks, Days,
        RH, RM, RS, RMs, RUs, RNs)
    else
      ComputeZonedDateTimeDiffFields(Zdt.FEpochMilliseconds,
        Zdt.FSubMillisecondNanoseconds, Other.FEpochMilliseconds,
        Other.FSubMillisecondNanoseconds, Zdt.FTimeZone, Zdt.FCalendarId, LargestUnit,
        Years, Months, Weeks, Days, RH, RM, RS, RMs, RUs, RNs);

    Result := TGocciaTemporalDurationValue.Create(-Years, -Months, -Weeks,
      -Days, -RH, -RM, -RS, -RMs, -RUs, -RNs);
  end
  else
  begin
    StartNs := EpochNanosecondsFromParts(Zdt.FEpochMilliseconds,
      Zdt.FSubMillisecondNanoseconds);
    EndNs := EpochNanosecondsFromParts(Other.FEpochMilliseconds,
      Other.FSubMillisecondNanoseconds);
    DiffNs := TimeDurationFromEpochNanosecondsDifference(EndNs, StartNs);
    if (SmallestUnit <> tuNanosecond) or (RIncrement <> 1) then
    begin
      IncrementNs := TBigInteger.FromInt64(UnitToNanoseconds(SmallestUnit))
        .Multiply(TBigInteger.FromInt64(RIncrement));
      DiffNs := RoundTimeDurationToIncrement(DiffNs, IncrementNs, RMode);
    end;
    Result := ZonedDiffToUnits(DiffNs.Negate, LargestUnit);
  end;
end;

procedure ValidateZonedDateTimeRoundIncrement(const AIncrement: Integer;
  const ASmallestUnit: TTemporalUnit);
var
  MaxVal: Integer;
begin
  if AIncrement <= 1 then
    Exit;

  case ASmallestUnit of
    tuDay: MaxVal := 1;
    tuHour: MaxVal := 24;
    tuMinute,
    tuSecond: MaxVal := 60;
    tuMillisecond,
    tuMicrosecond,
    tuNanosecond: MaxVal := 1000;
  else
    Exit;
  end;

  if (AIncrement >= MaxVal) or (MaxVal mod AIncrement <> 0) then
    ThrowRangeError(Format(SErrorRoundingIncrementDivisor, [AIncrement, MaxVal]),
      SSuggestTemporalRoundArg);
end;

procedure ValidateZonedDateTimeDiffRoundingIncrement(const AIncrement: Integer;
  const ASmallestUnit, ALargestUnit: TTemporalUnit);
var
  MaxVal: Integer;
begin
  if AIncrement <= 1 then
    Exit;

  case ASmallestUnit of
    tuHour: MaxVal := 24;
    tuMinute,
    tuSecond: MaxVal := 60;
    tuMillisecond,
    tuMicrosecond,
    tuNanosecond: MaxVal := 1000;
  else
    begin
      ValidateRoundingIncrement(AIncrement, ASmallestUnit);
      Exit;
    end;
  end;

  if (AIncrement >= MaxVal) or (MaxVal mod AIncrement <> 0) then
    ThrowRangeError(Format(SErrorRoundingIncrementDivisor, [AIncrement, MaxVal]),
      SSuggestTemporalRoundArg);
end;

// TC39 Temporal §6.3.30 Temporal.ZonedDateTime.prototype.round(roundTo)
function TGocciaTemporalZonedDateTimeValue.ZonedDateTimeRound(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Zdt: TGocciaTemporalZonedDateTimeValue;
  UnitStr: string;
  Arg: TGocciaValue;
  SmallestUnit: TTemporalUnit;
  Mode: TTemporalRoundingMode;
  Increment: Integer;
  OptionsObj: TGocciaObjectValue;
  LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs: Integer;
  TotalNs, Divisor, Rounded, ExtraDays: Int64;
  Balanced: TTemporalTimeRecord;
  DateRec: TTemporalDateRecord;
  NewEpochMs: Int64;
  NewSubMs: Integer;
  StartEpochMs, NextEpochMs, DayNs: Int64;
begin
  Zdt := AsZonedDateTime(AThisValue, 'ZonedDateTime.prototype.round');
  Arg := AArgs.GetElement(0);

  Mode := rmHalfExpand;
  Increment := 1;

  if Arg is TGocciaStringLiteralValue then
  begin
    UnitStr := TGocciaStringLiteralValue(Arg).Value;
    if not GetTemporalUnitFromString(UnitStr, SmallestUnit) then
      ThrowRangeError(Format(SErrorTemporalInvalidUnitFor, ['ZonedDateTime.prototype.round', UnitStr]), SSuggestTemporalValidUnits);
  end
  else if Arg is TGocciaObjectValue then
  begin
    OptionsObj := TGocciaObjectValue(Arg);
    Increment := GetRoundingIncrement(OptionsObj, 1);
    Mode := GetRoundingMode(OptionsObj, rmHalfExpand);
    SmallestUnit := GetSmallestUnit(OptionsObj, tuNone);
    if SmallestUnit = tuNone then
      ThrowRangeError(SErrorTemporalRoundRequiresSmallestUnit, SSuggestTemporalRoundArg);
    // Convert unit enum to string for the existing unit dispatch
    case SmallestUnit of
      tuDay: UnitStr := 'day';
      tuHour: UnitStr := 'hour';
      tuMinute: UnitStr := 'minute';
      tuSecond: UnitStr := 'second';
      tuMillisecond: UnitStr := 'millisecond';
      tuMicrosecond: UnitStr := 'microsecond';
      tuNanosecond: UnitStr := 'nanosecond';
    else
      ThrowRangeError(SErrorInvalidZDTRoundUnit, SSuggestTemporalValidUnits);
      UnitStr := '';
    end;
  end
  else
  begin
    ThrowTypeError(Format(SErrorTemporalRoundRequiresStringOrOptions, ['ZonedDateTime']), SSuggestTemporalRoundArg);
    SmallestUnit := tuNone;
  end;

  case SmallestUnit of
    tuDay: UnitStr := 'day';
    tuHour: UnitStr := 'hour';
    tuMinute: UnitStr := 'minute';
    tuSecond: UnitStr := 'second';
    tuMillisecond: UnitStr := 'millisecond';
    tuMicrosecond: UnitStr := 'microsecond';
    tuNanosecond: UnitStr := 'nanosecond';
  else
    ThrowRangeError(SErrorInvalidZDTRoundUnit, SSuggestTemporalValidUnits);
    UnitStr := '';
  end;

  ValidateZonedDateTimeRoundIncrement(Increment, SmallestUnit);

  ComputeLocalComponents(Zdt, LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs);

  if UnitStr = 'day' then
  begin
    // Compute actual day length in nanoseconds (DST-aware)
    StartEpochMs := RoundingStartOfDayEpochMilliseconds(LYear, LMonth, LDay,
      Zdt.FTimeZone, False);
    DateRec := AddDaysToDate(LYear, LMonth, LDay, 1);
    NextEpochMs := RoundingStartOfDayEpochMilliseconds(DateRec.Year,
      DateRec.Month, DateRec.Day, Zdt.FTimeZone, True);
    DayNs := (NextEpochMs - StartEpochMs) * NANOSECONDS_PER_MILLISECOND;
    if DayNs <= 0 then
      DayNs := NANOSECONDS_PER_DAY;  // Fallback for edge cases

    TotalNs := (Zdt.FEpochMilliseconds - StartEpochMs) *
      NANOSECONDS_PER_MILLISECOND + Zdt.FSubMillisecondNanoseconds;
    Divisor := DayNs * Increment;
    Rounded := RoundWithMode(TotalNs, Divisor, Mode);
    ExtraDays := Rounded div DayNs;
    if ExtraDays > 0 then
      DateRec := AddDaysToDate(LYear, LMonth, LDay, ExtraDays)
    else
    begin
      DateRec.Year := LYear;
      DateRec.Month := LMonth;
      DateRec.Day := LDay;
    end;
    NewEpochMs := StartOfDayToEpochMilliseconds(DateRec.Year, DateRec.Month,
      DateRec.Day, Zdt.FTimeZone);
    Result := TGocciaTemporalZonedDateTimeValue.Create(NewEpochMs, 0,
      Zdt.FTimeZone, Zdt.FCalendarId);
    Exit;
  end;

  TotalNs := Int64(LNs) + Int64(LUs) * NANOSECONDS_PER_MICROSECOND +
             Int64(LMs) * NANOSECONDS_PER_MILLISECOND +
             Int64(LSecond) * NANOSECONDS_PER_SECOND +
             Int64(LMinute) * NANOSECONDS_PER_MINUTE +
             Int64(LHour) * NANOSECONDS_PER_HOUR;

  if UnitStr = 'hour' then Divisor := NANOSECONDS_PER_HOUR
  else if UnitStr = 'minute' then Divisor := NANOSECONDS_PER_MINUTE
  else if UnitStr = 'second' then Divisor := NANOSECONDS_PER_SECOND
  else if UnitStr = 'millisecond' then Divisor := NANOSECONDS_PER_MILLISECOND
  else if UnitStr = 'microsecond' then Divisor := NANOSECONDS_PER_MICROSECOND
  else if UnitStr = 'nanosecond' then Divisor := 1
  else
  begin
    ThrowRangeError(Format(SErrorTemporalInvalidUnitFor, ['ZonedDateTime.prototype.round', UnitStr]), SSuggestTemporalValidUnits);
    Divisor := 1;
  end;

  Divisor := Divisor * Increment;
  Rounded := RoundWithMode(TotalNs, Divisor, Mode);
  Balanced := BalanceTime(0, 0, 0, 0, 0, Rounded, ExtraDays);
  DateRec := AddDaysToDate(LYear, LMonth, LDay, ExtraDays);

  NewEpochMs := LocalToEpochMs(DateRec.Year, DateRec.Month, DateRec.Day,
    Balanced.Hour, Balanced.Minute, Balanced.Second, Balanced.Millisecond, Zdt.FTimeZone);
  NewSubMs := Balanced.Microsecond * 1000 + Balanced.Nanosecond;

  Result := TGocciaTemporalZonedDateTimeValue.Create(NewEpochMs, NewSubMs, Zdt.FTimeZone, Zdt.FCalendarId);
end;

// TC39 Temporal §6.3.31 Temporal.ZonedDateTime.prototype.equals(other)
function TGocciaTemporalZonedDateTimeValue.ZonedDateTimeEquals(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Zdt, Other: TGocciaTemporalZonedDateTimeValue;
begin
  Zdt := AsZonedDateTime(AThisValue, 'ZonedDateTime.prototype.equals');
  Other := CoerceTemporalZonedDateTime(AArgs.GetElement(0), 'ZonedDateTime.prototype.equals');

  if (Zdt.FEpochMilliseconds = Other.FEpochMilliseconds) and
     (Zdt.FSubMillisecondNanoseconds = Other.FSubMillisecondNanoseconds) and
     TimeZoneIdentifiersEqual(Zdt.FTimeZone, Other.FTimeZone) and
     (Zdt.FCalendarId = Other.FCalendarId) then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;

// TC39 Temporal §6.3.32 Temporal.ZonedDateTime.prototype.getTimeZoneTransition(directionParam)
function TGocciaTemporalZonedDateTimeValue.ZonedDateTimeGetTimeZoneTransition(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Zdt: TGocciaTemporalZonedDateTimeValue;
  DirectionParam, DirectionValue: TGocciaValue;
  Direction: string;
  Next: Boolean;
  TransitionEpochMs: Int64;
begin
  Zdt := AsZonedDateTime(AThisValue, 'ZonedDateTime.prototype.getTimeZoneTransition');
  DirectionParam := AArgs.GetElement(0);
  if IsUndefinedValue(DirectionParam) then
    ThrowTypeError('ZonedDateTime.prototype.getTimeZoneTransition requires a direction argument',
      SSuggestTemporalFromArg);

  if DirectionParam is TGocciaStringLiteralValue then
    Direction := TGocciaStringLiteralValue(DirectionParam).Value
  else if DirectionParam is TGocciaObjectValue then
  begin
    DirectionValue := TGocciaObjectValue(DirectionParam).GetProperty('direction');
    if IsUndefinedValue(DirectionValue) then
      Direction := ''
    else
      Direction := DirectionValue.ToStringLiteral.Value;
  end
  else
    ThrowTypeError('ZonedDateTime.prototype.getTimeZoneTransition direction must be a string or object',
      SSuggestTemporalFromArg);

  if Direction = 'next' then
    Next := True
  else if Direction = 'previous' then
    Next := False
  else
    ThrowRangeError('ZonedDateTime.prototype.getTimeZoneTransition direction must be next or previous',
      SSuggestTemporalRoundArg);

  if not TryGetTimeZoneTransitionMilliseconds(Zdt.FTimeZone,
    Zdt.FEpochMilliseconds, Zdt.FSubMillisecondNanoseconds, Next,
    TransitionEpochMs) then
    Exit(TGocciaNullLiteralValue.NullValue);

  Result := TGocciaTemporalZonedDateTimeValue.Create(TransitionEpochMs, 0,
    Zdt.FTimeZone, Zdt.FCalendarId);
end;

// TC39 Temporal §6.3.32 Temporal.ZonedDateTime.prototype.startOfDay()
function TGocciaTemporalZonedDateTimeValue.ZonedDateTimeStartOfDay(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Zdt: TGocciaTemporalZonedDateTimeValue;
  LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs: Integer;
  NewEpochMs: Int64;
begin
  Zdt := AsZonedDateTime(AThisValue, 'ZonedDateTime.prototype.startOfDay');
  ComputeLocalComponents(Zdt, LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs);

  NewEpochMs := StartOfDayToEpochMilliseconds(LYear, LMonth, LDay,
    Zdt.FTimeZone);
  Result := TGocciaTemporalZonedDateTimeValue.Create(NewEpochMs, 0, Zdt.FTimeZone, Zdt.FCalendarId);
end;

// TC39 Temporal §6.3.33 Temporal.ZonedDateTime.prototype.toInstant()
function TGocciaTemporalZonedDateTimeValue.ZonedDateTimeToInstant(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Zdt: TGocciaTemporalZonedDateTimeValue;
begin
  Zdt := AsZonedDateTime(AThisValue, 'ZonedDateTime.prototype.toInstant');
  Result := TGocciaTemporalInstantValue.Create(Zdt.FEpochMilliseconds, Zdt.FSubMillisecondNanoseconds);
end;

// TC39 Temporal §6.3.34 Temporal.ZonedDateTime.prototype.toPlainDate()
function TGocciaTemporalZonedDateTimeValue.ZonedDateTimeToPlainDate(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Zdt: TGocciaTemporalZonedDateTimeValue;
  LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs: Integer;
begin
  Zdt := AsZonedDateTime(AThisValue, 'ZonedDateTime.prototype.toPlainDate');
  ComputeLocalComponents(Zdt, LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs);
  Result := TGocciaTemporalPlainDateValue.Create(LYear, LMonth, LDay, Zdt.FCalendarId);
end;

// TC39 Temporal §6.3.35 Temporal.ZonedDateTime.prototype.toPlainTime()
function TGocciaTemporalZonedDateTimeValue.ZonedDateTimeToPlainTime(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Zdt: TGocciaTemporalZonedDateTimeValue;
  LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs: Integer;
begin
  Zdt := AsZonedDateTime(AThisValue, 'ZonedDateTime.prototype.toPlainTime');
  ComputeLocalComponents(Zdt, LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs);
  Result := TGocciaTemporalPlainTimeValue.Create(LHour, LMinute, LSecond, LMs, LUs, LNs);
end;

// TC39 Temporal §6.3.36 Temporal.ZonedDateTime.prototype.toPlainDateTime()
function TGocciaTemporalZonedDateTimeValue.ZonedDateTimeToPlainDateTime(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Zdt: TGocciaTemporalZonedDateTimeValue;
  LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs: Integer;
begin
  Zdt := AsZonedDateTime(AThisValue, 'ZonedDateTime.prototype.toPlainDateTime');
  ComputeLocalComponents(Zdt, LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs);
  Result := TGocciaTemporalPlainDateTimeValue.Create(LYear, LMonth, LDay,
    LHour, LMinute, LSecond, LMs, LUs, LNs, Zdt.FCalendarId);
end;

function TrimAutoFractionalSecondZeros(const ATime: string): string;
var
  DotPos: Integer;
begin
  Result := ATime;
  DotPos := Pos('.', Result);
  if DotPos = 0 then
    Exit;

  while (Length(Result) > DotPos) and (Result[Length(Result)] = '0') do
    Delete(Result, Length(Result), 1);
  if Length(Result) = DotPos then
    Delete(Result, DotPos, 1);
end;

function FractionalSecondDigitDivisor(const AFractionalDigits: Integer): Int64;
begin
  case AFractionalDigits of
    0: Result := NANOSECONDS_PER_SECOND;
    1: Result := 100000000;
    2: Result := 10000000;
    3: Result := NANOSECONDS_PER_MILLISECOND;
    4: Result := 100000;
    5: Result := 10000;
    6: Result := NANOSECONDS_PER_MICROSECOND;
    7: Result := 100;
    8: Result := 10;
  else
    Result := 1;
  end;
end;

procedure RoundZonedDateTimeForToString(
  var AHour, AMinute, ASecond, AMs, AUs, ANs: Integer;
  var AExtraDays: Integer;
  const AFractionalDigits: Integer;
  const ARoundingMode: TTemporalRoundingMode);
var
  TotalNs, Divisor, Rounded: Int64;
  ExDays: Int64;
  BalancedTime: TTemporalTimeRecord;
begin
  AExtraDays := 0;
  if (AFractionalDigits < 0) and (AFractionalDigits <> -2) then
    Exit;
  if AFractionalDigits = 9 then
    Exit;

  TotalNs := Int64(AHour) * NANOSECONDS_PER_HOUR +
             Int64(AMinute) * NANOSECONDS_PER_MINUTE +
             Int64(ASecond) * NANOSECONDS_PER_SECOND +
             Int64(AMs) * NANOSECONDS_PER_MILLISECOND +
             Int64(AUs) * NANOSECONDS_PER_MICROSECOND +
             Int64(ANs);

  if AFractionalDigits = -2 then
    Divisor := NANOSECONDS_PER_MINUTE
  else
    Divisor := FractionalSecondDigitDivisor(AFractionalDigits);

  Rounded := RoundWithMode(TotalNs, Divisor, ARoundingMode);
  BalancedTime := BalanceTime(0, 0, 0, 0, 0, Rounded, ExDays);

  AHour := BalancedTime.Hour;
  AMinute := BalancedTime.Minute;
  ASecond := BalancedTime.Second;
  AMs := BalancedTime.Millisecond;
  AUs := BalancedTime.Microsecond;
  ANs := BalancedTime.Nanosecond;
  AExtraDays := Integer(ExDays);
end;

function GetZonedDateTimeFractionalSecondDigits(const AOptions: TGocciaObjectValue): Integer;
var
  V: TGocciaValue;
  Num: TGocciaNumberLiteralValue;
  S: string;
begin
  Result := -1;
  if AOptions = nil then
    Exit;
  V := AOptions.GetProperty('fractionalSecondDigits');
  if IsUndefinedValue(V) then
    Exit;
  if V is TGocciaNumberLiteralValue then
  begin
    Num := TGocciaNumberLiteralValue(V);
    if Num.IsNaN or Num.IsInfinity or (Num.Value < 0) or (Num.Value >= 10) then
      ThrowRangeError(SErrorFractionalDigitsRange, SSuggestTemporalRoundArg);
    Result := Trunc(Num.Value);
  end
  else
  begin
    S := V.ToStringLiteral.Value;
    if S = 'auto' then
      Result := -1
    else
      ThrowRangeError(Format(SErrorInvalidFractionalDigits, [S]),
        SSuggestTemporalRoundArg);
  end;
end;

procedure ReadZonedDateTimeToStringOptions(const AOptions: TGocciaObjectValue;
  out ACalendarDisplay: TTemporalCalendarDisplay; out AFractionalDigits: Integer;
  out AOffsetOption: string; out ARoundingMode: TTemporalRoundingMode;
  out ASmallestUnit: string; out AHasSmallestUnit: Boolean;
  out ATimeZoneNameOption: string);
var
  V: TGocciaValue;
begin
  ACalendarDisplay := GetCalendarDisplay(AOptions);
  AFractionalDigits := GetZonedDateTimeFractionalSecondDigits(AOptions);
  AOffsetOption := GetOptionString(AOptions, 'offset', 'auto');
  ARoundingMode := GetRoundingMode(AOptions, rmTrunc);
  ASmallestUnit := '';
  AHasSmallestUnit := False;
  if AOptions <> nil then
  begin
    V := AOptions.GetProperty('smallestUnit');
    if not IsUndefinedValue(V) then
    begin
      ASmallestUnit := V.ToStringLiteral.Value;
      AHasSmallestUnit := True;
    end;
  end;
  ATimeZoneNameOption := GetOptionString(AOptions, 'timeZoneName', 'auto');
end;

procedure ApplyZonedDateTimeToStringSmallestUnit(const ASmallestUnit: string;
  const AHasSmallestUnit: Boolean; var AFractionalDigits: Integer);
var
  SmallestUnit: TTemporalUnit;
begin
  if not AHasSmallestUnit then
    Exit;
  if not GetTemporalUnitFromString(ASmallestUnit, SmallestUnit) then
    ThrowRangeError(Format(SErrorInvalidSmallestUnit, [ASmallestUnit]),
      SSuggestTemporalValidUnits);
  case SmallestUnit of
    tuMinute: AFractionalDigits := -2;
    tuSecond: AFractionalDigits := 0;
    tuMillisecond: AFractionalDigits := 3;
    tuMicrosecond: AFractionalDigits := 6;
    tuNanosecond: AFractionalDigits := 9;
  else
    ThrowRangeError(Format(SErrorInvalidSmallestUnit, [ASmallestUnit]),
      SSuggestTemporalValidUnits);
  end;
end;

// TC39 Temporal §6.3.37 Temporal.ZonedDateTime.prototype.toString([options])
function TGocciaTemporalZonedDateTimeValue.ZonedDateTimeToString(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Zdt: TGocciaTemporalZonedDateTimeValue;
  LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs: Integer;
  SavedHour, SavedMinute, SavedSecond, SavedMs, SavedUs, SavedNs: Integer;
  OffsetSeconds: Integer;
  Arg: TGocciaValue;
  OptionsObj: TGocciaObjectValue;
  FracDigits, ExtraDays: Integer;
  Mode: TTemporalRoundingMode;
  CalDisp: TTemporalCalendarDisplay;
  DateRec: TTemporalDateRecord;
  RoundedEpochMs: Int64;
  RoundedSubMs: Integer;
  OffsetOption, TimeZoneNameOption, TimeStr, S: string;
  SmallestUnitStr: string;
  HasSmallestUnit: Boolean;
begin
  Zdt := AsZonedDateTime(AThisValue, 'ZonedDateTime.prototype.toString');
  ComputeLocalComponents(Zdt, LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs);

  OptionsObj := nil;
  Arg := AArgs.GetElement(0);
  if Assigned(Arg) and not (Arg is TGocciaUndefinedLiteralValue) then
  begin
    if not (Arg is TGocciaObjectValue) then
      ThrowTypeError('options must be an object or undefined', SSuggestTemporalFromArg);
    OptionsObj := TGocciaObjectValue(Arg);
  end;
  ReadZonedDateTimeToStringOptions(OptionsObj, CalDisp, FracDigits,
    OffsetOption, Mode, SmallestUnitStr, HasSmallestUnit, TimeZoneNameOption);
  if (OffsetOption <> 'auto') and (OffsetOption <> 'never') then
    ThrowRangeError('Invalid offset option: ' + OffsetOption, SSuggestTemporalRoundArg);
  ApplyZonedDateTimeToStringSmallestUnit(SmallestUnitStr, HasSmallestUnit,
    FracDigits);
  if (TimeZoneNameOption <> 'auto') and (TimeZoneNameOption <> 'never') and
     (TimeZoneNameOption <> 'critical') then
    ThrowRangeError('Invalid timeZoneName option: ' + TimeZoneNameOption, SSuggestTemporalRoundArg);
  SavedHour := LHour;
  SavedMinute := LMinute;
  SavedSecond := LSecond;
  SavedMs := LMs;
  SavedUs := LUs;
  SavedNs := LNs;
  ExtraDays := 0;
  RoundZonedDateTimeForToString(LHour, LMinute, LSecond, LMs, LUs, LNs,
    ExtraDays, FracDigits, Mode);
  if ExtraDays <> 0 then
    DateRec := AddDaysToDate(LYear, LMonth, LDay, ExtraDays)
  else
  begin
    DateRec.Year := LYear;
    DateRec.Month := LMonth;
    DateRec.Day := LDay;
  end;

  // Recompute local fields and UTC offset only when rounding changed the exact time.
  // Unconditional recomputation would lose fold information during DST fall-back.
  if (ExtraDays <> 0) or (LHour <> SavedHour) or (LMinute <> SavedMinute) or
     (LSecond <> SavedSecond) or (LMs <> SavedMs) or (LUs <> SavedUs) or
     (LNs <> SavedNs) then
  begin
    RoundedEpochMs := LocalToEpochMs(DateRec.Year, DateRec.Month, DateRec.Day,
      LHour, LMinute, LSecond, LMs, Zdt.FTimeZone);
    RoundedSubMs := LUs * 1000 + LNs;
    ComputeLocalComponentsFromEpoch(RoundedEpochMs, RoundedSubMs, Zdt.FTimeZone,
      LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs);
    DateRec.Year := LYear;
    DateRec.Month := LMonth;
    DateRec.Day := LDay;
    OffsetSeconds := GetUtcOffsetSeconds(Zdt.FTimeZone,
      EpochMillisecondsToSeconds(RoundedEpochMs));
  end
  else
    OffsetSeconds := GetUtcOffsetSeconds(Zdt.FTimeZone,
      EpochMillisecondsToSeconds(Zdt.FEpochMilliseconds));

  if FracDigits = -2 then // smallestUnit: minute
    TimeStr := PadTwo(LHour) + ':' + PadTwo(LMinute)
  else
  begin
    TimeStr := FormatTimeWithPrecision(LHour, LMinute, LSecond, LMs, LUs, LNs,
      FracDigits);
    if FracDigits < 0 then
      TimeStr := TrimAutoFractionalSecondZeros(TimeStr);
  end;

  S := FormatDateString(DateRec.Year, DateRec.Month, DateRec.Day) + 'T' + TimeStr;
  if OffsetOption <> 'never' then
    S := S + FormatDateTimeUTCOffsetRounded(OffsetSeconds);
  if TimeZoneNameOption = 'critical' then
    S := S + '[!' + Zdt.FTimeZone + ']'
  else if TimeZoneNameOption <> 'never' then
    S := S + '[' + Zdt.FTimeZone + ']';
  S := S + FormatCalendarAnnotation(CalDisp, Zdt.FCalendarId);

  Result := TGocciaStringLiteralValue.Create(S);
end;

// TC39 Temporal §6.3.38 Temporal.ZonedDateTime.prototype.toJSON()
function TGocciaTemporalZonedDateTimeValue.ZonedDateTimeToJSON(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Zdt: TGocciaTemporalZonedDateTimeValue;
  LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs: Integer;
  OffsetSeconds: Integer;
begin
  Zdt := AsZonedDateTime(AThisValue, 'ZonedDateTime.prototype.toJSON');
  ComputeLocalComponents(Zdt, LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs);
  OffsetSeconds := GetUtcOffsetSeconds(Zdt.FTimeZone,
    EpochMillisecondsToSeconds(Zdt.FEpochMilliseconds));
  Result := TGocciaStringLiteralValue.Create(
    FormatDateString(LYear, LMonth, LDay) + 'T' +
    FormatTimeString(LHour, LMinute, LSecond, LMs, LUs, LNs) +
    FormatOffsetString(OffsetSeconds) +
    '[' + Zdt.FTimeZone + ']');
end;

// TC39 Temporal §6.3.40 Temporal.ZonedDateTime.prototype.valueOf()
function TGocciaTemporalZonedDateTimeValue.ZonedDateTimeValueOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  ThrowTypeError(Format(SErrorTemporalValueOf, ['ZonedDateTime', 'epochMilliseconds, epochNanoseconds, or compare']), SSuggestTemporalNoValueOf);
  Result := nil;
end;

function TGocciaTemporalZonedDateTimeValue.ZonedDateTimeToLocaleString(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  AsZonedDateTime(AThisValue, 'ZonedDateTime.prototype.toLocaleString');
  Result := FormatTemporalValueToLocaleString(AThisValue, AArgs.GetElement(0),
    AArgs.GetElement(1));
end;

initialization
  GTemporalZonedDateTimeSharedSlot := RegisterRealmOwnedSlot('Temporal.ZonedDateTime.shared');

end.
