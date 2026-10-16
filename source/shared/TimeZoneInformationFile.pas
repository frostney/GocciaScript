unit TimeZoneInformationFile;

{$I Shared.inc}

interface

uses
  SysUtils;

type
  TTimeZoneInformationTransition = record
    TransitionEpochSeconds: Int64;
    UtcOffsetSeconds: Integer;
  end;

  TTimeZoneInformationTransitionArray = array of TTimeZoneInformationTransition;

  { The three date forms a POSIX TZ rule can name: Jn (1..365, February 29
    never counted), n (0..365, February 29 counted) and Mm.w.d (weekday d of
    week w of month m, where week 5 is the last). }
  TTimeZoneRuleDateKind = (tzrdJulianDay, tzrdZeroBasedDay, tzrdMonthWeekDay);

  TTimeZoneRuleDate = record
    Kind: TTimeZoneRuleDateKind;
    Day: Integer;
    Month: Integer;
    Week: Integer;
    { Local wall-clock time of the change, in the time that is in effect
      just before it. }
    TimeSeconds: Integer;
  end;

  { A POSIX TZ string such as PST8PDT,M3.2.0,M11.1.0. Offsets are UTC
    offsets (positive east of UTC), the opposite sign of the TZ string. }
  TTimeZoneRule = record
    Present: Boolean;
    StandardOffsetSeconds: Integer;
    HasDaylightSaving: Boolean;
    DaylightOffsetSeconds: Integer;
    DaylightStart: TTimeZoneRuleDate;
    DaylightEnd: TTimeZoneRuleDate;
  end;

  TTimeZoneInformationData = record
    Identifier: string;
    Transitions: TTimeZoneInformationTransitionArray;
    DefaultOffsetSeconds: Integer;
    { The footer rule, which governs every instant after the last
      transition, or every instant when there are no transitions. }
    Rule: TTimeZoneRule;
  end;

function TryParseTimeZoneInformationFile(const AIdentifier: string;
  const ABytes: TBytes; out AData: TTimeZoneInformationData): Boolean;
procedure ClearTimeZoneRule(out ARule: TTimeZoneRule);
function TryParseTimeZoneRule(const AText: string; out ARule: TTimeZoneRule): Boolean;
{ The UTC offset the rule gives at an instant. }
function TimeZoneRuleOffsetSeconds(const ARule: TTimeZoneRule;
  const AEpochSeconds: Int64): Integer;
{ The first instant after (ANext) or the last instant before (not ANext)
  AEpochSeconds at which the rule changes the UTC offset. False when the
  rule never changes it. }
function TryGetTimeZoneRuleTransition(const ARule: TTimeZoneRule;
  const AEpochSeconds: Int64; const ANext: Boolean;
  out ATransitionEpochSeconds: Int64): Boolean;

implementation

const
  TIMEZONE_INFORMATION_MAGIC_LENGTH = 4;
  TIMEZONE_INFORMATION_VERSION_OFFSET = 4;
  TIMEZONE_INFORMATION_COUNT_SIZE = 4;
  TIMEZONE_INFORMATION_HEADER_SIZE = 44;
  TIMEZONE_INFORMATION_V1_TRANSITION_SIZE = 4;
  TIMEZONE_INFORMATION_V2_TRANSITION_SIZE = 8;
  TIMEZONE_INFORMATION_TYPE_INDEX_SIZE = 1;
  TIMEZONE_INFORMATION_TYPE_INFORMATION_SIZE = 6;
  TIMEZONE_INFORMATION_TYPE_INFORMATION_OFFSET_SIZE = 4;
  TIMEZONE_INFORMATION_HEADER_COUNT_OFFSET = 20;
  TIMEZONE_INFORMATION_LEAP_V1_RECORD_SIZE = 8;
  TIMEZONE_INFORMATION_LEAP_V2_RECORD_SIZE = 12;
  TIMEZONE_INFORMATION_MAGIC_T = Ord('T');
  TIMEZONE_INFORMATION_MAGIC_Z = Ord('Z');
  TIMEZONE_INFORMATION_MAGIC_I = Ord('i');
  TIMEZONE_INFORMATION_MAGIC_F = Ord('f');
  TIMEZONE_INFORMATION_FOOTER_DELIMITER = 10;
  SECONDS_PER_MINUTE = 60;
  SECONDS_PER_HOUR = 60 * SECONDS_PER_MINUTE;
  SECONDS_PER_DAY = 24 * SECONDS_PER_HOUR;
  DAYS_PER_WEEK = 7;
  DAYS_PER_COMMON_YEAR = 365;
  { 1970-01-01, epoch day 0, was a Thursday (0 = Sunday). }
  EPOCH_WEEKDAY = 4;
  { POSIX: std and dst offsets have hours 0..24. }
  RULE_MAX_OFFSET_HOURS = 24;
  { RFC 9636 §3.3.2: rule times have signed hours -167..167. }
  RULE_MAX_TIME_HOURS = 167;
  RULE_DEFAULT_TIME_SECONDS = 2 * SECONDS_PER_HOUR;
  RULE_MIN_NAME_LENGTH = 3;
  RULE_LAST_WEEK = 5;
  { A rule change happens within about a week of its calendar date, so the
    changes around an instant always come from the two years either side of
    the instant's UTC year. }
  RULE_EVENT_YEAR_MARGIN = 2;
  RULE_EVENT_CAPACITY = 2 * (2 * RULE_EVENT_YEAR_MARGIN + 1);

function ReadBigEndianInt32(const ABuffer: TBytes; const AOffset: Integer): Int32;
begin
  Result := Int32((Int32(ABuffer[AOffset]) shl 24) or
                  (Int32(ABuffer[AOffset + 1]) shl 16) or
                  (Int32(ABuffer[AOffset + 2]) shl 8) or
                  Int32(ABuffer[AOffset + 3]));
end;

function ReadBigEndianUInt32(const ABuffer: TBytes; const AOffset: Integer): UInt32;
begin
  Result := (UInt32(ABuffer[AOffset]) shl 24) or
            (UInt32(ABuffer[AOffset + 1]) shl 16) or
            (UInt32(ABuffer[AOffset + 2]) shl 8) or
            UInt32(ABuffer[AOffset + 3]);
end;

function ReadBigEndianInt64(const ABuffer: TBytes; const AOffset: Integer): Int64;
var
  High32, Low32: UInt32;
begin
  High32 := ReadBigEndianUInt32(ABuffer, AOffset);
  Low32 := ReadBigEndianUInt32(ABuffer, AOffset + TIMEZONE_INFORMATION_COUNT_SIZE);
  Result := (Int64(High32) shl 32) or Int64(Low32);
end;

function HasBytesAvailable(const ABuffer: TBytes; const AOffset, ALength: Integer): Boolean;
begin
  Result := False;

  if (AOffset < 0) or (ALength < 0) then
    Exit;

  if AOffset > Length(ABuffer) then
    Exit;

  Result := ALength <= Length(ABuffer) - AOffset;
end;

function HasTimeZoneInformationMagic(const ABuffer: TBytes; const AOffset: Integer): Boolean;
begin
  Result := HasBytesAvailable(ABuffer, AOffset, TIMEZONE_INFORMATION_MAGIC_LENGTH) and
            (ABuffer[AOffset] = TIMEZONE_INFORMATION_MAGIC_T) and
            (ABuffer[AOffset + 1] = TIMEZONE_INFORMATION_MAGIC_Z) and
            (ABuffer[AOffset + 2] = TIMEZONE_INFORMATION_MAGIC_I) and
            (ABuffer[AOffset + 3] = TIMEZONE_INFORMATION_MAGIC_F);
end;

function AreTimeZoneInformationCountsValid(const ATimeCount, ATypeCount, ACharacterCount,
  ALeapCount, AIsStandardCount, AIsUtCount: Int32): Boolean;
begin
  Result := (ATimeCount >= 0) and
            (ATypeCount >= 0) and
            (ACharacterCount >= 0) and
            (ALeapCount >= 0) and
            (AIsStandardCount >= 0) and
            (AIsUtCount >= 0);
end;

function TryComputeTimeZoneInformationDataSize(const ATimeCount, ATypeCount,
  ACharacterCount, ALeapCount, AIsStandardCount, AIsUtCount: Int32;
  const ATransitionSize, ALeapRecordSize: Integer; out ADataSize: Integer): Boolean;
var
  DataSize: Int64;
begin
  Result := False;
  ADataSize := 0;

  if not AreTimeZoneInformationCountsValid(ATimeCount, ATypeCount, ACharacterCount,
    ALeapCount, AIsStandardCount, AIsUtCount) then
    Exit;

  DataSize := Int64(ATimeCount) * ATransitionSize +
              Int64(ATimeCount) * TIMEZONE_INFORMATION_TYPE_INDEX_SIZE +
              Int64(ATypeCount) * TIMEZONE_INFORMATION_TYPE_INFORMATION_SIZE +
              Int64(ACharacterCount) +
              Int64(ALeapCount) * ALeapRecordSize +
              Int64(AIsStandardCount) +
              Int64(AIsUtCount);

  if (DataSize < 0) or (DataSize > High(Integer)) then
    Exit;

  ADataSize := Integer(DataSize);
  Result := True;
end;

function BytesToASCIIString(const ABytes: TBytes; const AOffset,
  ALength: Integer): string;
var
  I: Integer;
begin
  SetLength(Result, ALength);
  for I := 1 to ALength do
    Result[I] := Char(ABytes[AOffset + I - 1]);
end;

function TryParseTimeZoneInformationFile(const AIdentifier: string;
  const ABytes: TBytes; out AData: TTimeZoneInformationData): Boolean;
var
  Offset: Integer;
  Version: Char;
  TimeCount, TypeCount, CharacterCount, LeapCount, IsStandardCount, IsUtCount: Int32;
  V1DataSize, DataBlockSize, FooterOffset, FooterEnd: Integer;
  TransitionTimes: array of Int64;
  TypeIndices: array of Byte;
  UtcOffsets: array of Int32;
  I: Integer;
  UseV2: Boolean;
begin
  Result := False;

  if not HasBytesAvailable(ABytes, 0, TIMEZONE_INFORMATION_HEADER_SIZE) then
    Exit;

  if not HasTimeZoneInformationMagic(ABytes, 0) then
    Exit;

  Version := Char(ABytes[TIMEZONE_INFORMATION_VERSION_OFFSET]);
  { RFC 9636 §3.1: versions 2, 3 and 4 share the 64-bit data block layout. }
  UseV2 := (Version = '2') or (Version = '3') or (Version = '4');
  FooterOffset := -1;

  Offset := TIMEZONE_INFORMATION_HEADER_COUNT_OFFSET;
  IsUtCount := ReadBigEndianInt32(ABytes, Offset); Inc(Offset, TIMEZONE_INFORMATION_COUNT_SIZE);
  IsStandardCount := ReadBigEndianInt32(ABytes, Offset); Inc(Offset, TIMEZONE_INFORMATION_COUNT_SIZE);
  LeapCount := ReadBigEndianInt32(ABytes, Offset); Inc(Offset, TIMEZONE_INFORMATION_COUNT_SIZE);
  TimeCount := ReadBigEndianInt32(ABytes, Offset); Inc(Offset, TIMEZONE_INFORMATION_COUNT_SIZE);
  TypeCount := ReadBigEndianInt32(ABytes, Offset); Inc(Offset, TIMEZONE_INFORMATION_COUNT_SIZE);
  CharacterCount := ReadBigEndianInt32(ABytes, Offset); Inc(Offset, TIMEZONE_INFORMATION_COUNT_SIZE);

  if not TryComputeTimeZoneInformationDataSize(TimeCount, TypeCount, CharacterCount,
    LeapCount, IsStandardCount, IsUtCount, TIMEZONE_INFORMATION_V1_TRANSITION_SIZE,
    TIMEZONE_INFORMATION_LEAP_V1_RECORD_SIZE, V1DataSize) then
    Exit;

  if not HasBytesAvailable(ABytes, TIMEZONE_INFORMATION_HEADER_SIZE, V1DataSize) then
    Exit;

  if UseV2 then
  begin
    Offset := TIMEZONE_INFORMATION_HEADER_SIZE + V1DataSize;

    if not HasBytesAvailable(ABytes, Offset, TIMEZONE_INFORMATION_HEADER_SIZE) then
      Exit;

    if not HasTimeZoneInformationMagic(ABytes, Offset) then
      Exit;

    Offset := Offset + TIMEZONE_INFORMATION_HEADER_COUNT_OFFSET;
    IsUtCount := ReadBigEndianInt32(ABytes, Offset); Inc(Offset, TIMEZONE_INFORMATION_COUNT_SIZE);
    IsStandardCount := ReadBigEndianInt32(ABytes, Offset); Inc(Offset, TIMEZONE_INFORMATION_COUNT_SIZE);
    LeapCount := ReadBigEndianInt32(ABytes, Offset); Inc(Offset, TIMEZONE_INFORMATION_COUNT_SIZE);
    TimeCount := ReadBigEndianInt32(ABytes, Offset); Inc(Offset, TIMEZONE_INFORMATION_COUNT_SIZE);
    TypeCount := ReadBigEndianInt32(ABytes, Offset); Inc(Offset, TIMEZONE_INFORMATION_COUNT_SIZE);
    CharacterCount := ReadBigEndianInt32(ABytes, Offset); Inc(Offset, TIMEZONE_INFORMATION_COUNT_SIZE);

    if not TryComputeTimeZoneInformationDataSize(TimeCount, TypeCount, CharacterCount,
      LeapCount, IsStandardCount, IsUtCount, TIMEZONE_INFORMATION_V2_TRANSITION_SIZE,
      TIMEZONE_INFORMATION_LEAP_V2_RECORD_SIZE, DataBlockSize) then
      Exit;

    if not HasBytesAvailable(ABytes, Offset, DataBlockSize) then
      Exit;
    FooterOffset := Offset + DataBlockSize;

    SetLength(TransitionTimes, TimeCount);
    for I := 0 to TimeCount - 1 do
    begin
      TransitionTimes[I] := ReadBigEndianInt64(ABytes, Offset);
      Inc(Offset, TIMEZONE_INFORMATION_V2_TRANSITION_SIZE);
    end;
  end
  else
  begin
    Offset := TIMEZONE_INFORMATION_HEADER_SIZE;

    if not HasBytesAvailable(ABytes, Offset, V1DataSize) then
      Exit;

    SetLength(TransitionTimes, TimeCount);
    for I := 0 to TimeCount - 1 do
    begin
      TransitionTimes[I] := Int64(ReadBigEndianInt32(ABytes, Offset));
      Inc(Offset, TIMEZONE_INFORMATION_V1_TRANSITION_SIZE);
    end;
  end;

  SetLength(TypeIndices, TimeCount);
  for I := 0 to TimeCount - 1 do
  begin
    TypeIndices[I] := ABytes[Offset];
    Inc(Offset, TIMEZONE_INFORMATION_TYPE_INDEX_SIZE);
  end;

  SetLength(UtcOffsets, TypeCount);
  for I := 0 to TypeCount - 1 do
  begin
    UtcOffsets[I] := ReadBigEndianInt32(ABytes, Offset);
    Inc(Offset, TIMEZONE_INFORMATION_TYPE_INFORMATION_OFFSET_SIZE);
    Inc(Offset, TIMEZONE_INFORMATION_TYPE_INFORMATION_SIZE -
      TIMEZONE_INFORMATION_TYPE_INFORMATION_OFFSET_SIZE);
  end;

  AData.Identifier := AIdentifier;

  if TypeCount > 0 then
    AData.DefaultOffsetSeconds := UtcOffsets[0]
  else
    AData.DefaultOffsetSeconds := 0;

  SetLength(AData.Transitions, TimeCount);
  for I := 0 to TimeCount - 1 do
  begin
    AData.Transitions[I].TransitionEpochSeconds := TransitionTimes[I];
    if TypeIndices[I] < TypeCount then
      AData.Transitions[I].UtcOffsetSeconds := UtcOffsets[TypeIndices[I]]
    else
      AData.Transitions[I].UtcOffsetSeconds := 0;
  end;

  // RFC 9636 §3.3: the footer is NL, a TZ string, NL. A missing, empty or
  // unparsable string leaves the zone without a rule.
  ClearTimeZoneRule(AData.Rule);
  if (FooterOffset >= 0) and HasBytesAvailable(ABytes, FooterOffset, 1) and
     (ABytes[FooterOffset] = TIMEZONE_INFORMATION_FOOTER_DELIMITER) then
  begin
    FooterEnd := FooterOffset + 1;
    while (FooterEnd < Length(ABytes)) and
          (ABytes[FooterEnd] <> TIMEZONE_INFORMATION_FOOTER_DELIMITER) do
      Inc(FooterEnd);
    if FooterEnd < Length(ABytes) then
      TryParseTimeZoneRule(BytesToASCIIString(ABytes, FooterOffset + 1,
        FooterEnd - FooterOffset - 1), AData.Rule);
  end;

  Result := True;
end;

procedure ClearTimeZoneRuleDate(out ADate: TTimeZoneRuleDate);
begin
  ADate.Kind := tzrdJulianDay;
  ADate.Day := 0;
  ADate.Month := 0;
  ADate.Week := 0;
  ADate.TimeSeconds := RULE_DEFAULT_TIME_SECONDS;
end;

procedure ClearTimeZoneRule(out ARule: TTimeZoneRule);
begin
  ARule.Present := False;
  ARule.StandardOffsetSeconds := 0;
  ARule.HasDaylightSaving := False;
  ARule.DaylightOffsetSeconds := 0;
  ClearTimeZoneRuleDate(ARule.DaylightStart);
  ClearTimeZoneRuleDate(ARule.DaylightEnd);
end;

function IsRuleDigit(const AChar: Char): Boolean;
begin
  Result := (AChar >= '0') and (AChar <= '9');
end;

function IsRuleLetter(const AChar: Char): Boolean;
begin
  Result := ((AChar >= 'A') and (AChar <= 'Z')) or
    ((AChar >= 'a') and (AChar <= 'z'));
end;

{ A zone abbreviation: at least three letters, or <...> around at least
  three letters, digits, '+' or '-'. Only its extent matters here. }
function TrySkipRuleName(const AText: string; var APosition: Integer): Boolean;
var
  Start: Integer;
begin
  Result := False;
  if APosition > Length(AText) then
    Exit;

  if AText[APosition] = '<' then
  begin
    Inc(APosition);
    Start := APosition;
    while (APosition <= Length(AText)) and
          (IsRuleLetter(AText[APosition]) or IsRuleDigit(AText[APosition]) or
           (AText[APosition] = '+') or (AText[APosition] = '-')) do
      Inc(APosition);
    if (APosition > Length(AText)) or (AText[APosition] <> '>') or
       (APosition - Start < RULE_MIN_NAME_LENGTH) then
      Exit;
    Inc(APosition);
    Exit(True);
  end;

  Start := APosition;
  while (APosition <= Length(AText)) and IsRuleLetter(AText[APosition]) do
    Inc(APosition);
  Result := APosition - Start >= RULE_MIN_NAME_LENGTH;
end;

function TryReadRuleNumber(const AText: string; var APosition: Integer;
  const AMaxDigits: Integer; out AValue: Integer): Boolean;
var
  Start: Integer;
begin
  AValue := 0;
  Start := APosition;
  while (APosition <= Length(AText)) and IsRuleDigit(AText[APosition]) and
        (APosition - Start < AMaxDigits) do
  begin
    AValue := AValue * 10 + (Ord(AText[APosition]) - Ord('0'));
    Inc(APosition);
  end;
  Result := APosition > Start;
end;

{ [+|-]hh[:mm[:ss]], as seconds. }
function TryReadRuleClock(const AText: string; var APosition: Integer;
  const AMaxHours: Integer; out ASeconds: Integer): Boolean;
var
  Sign, Hours, Minutes, Seconds: Integer;
begin
  Result := False;
  ASeconds := 0;
  Sign := 1;
  if (APosition <= Length(AText)) and
     ((AText[APosition] = '+') or (AText[APosition] = '-')) then
  begin
    if AText[APosition] = '-' then
      Sign := -1;
    Inc(APosition);
  end;

  if not TryReadRuleNumber(AText, APosition, 3, Hours) or (Hours > AMaxHours) then
    Exit;
  Minutes := 0;
  Seconds := 0;
  if (APosition <= Length(AText)) and (AText[APosition] = ':') then
  begin
    Inc(APosition);
    if not TryReadRuleNumber(AText, APosition, 2, Minutes) or (Minutes > 59) then
      Exit;
    if (APosition <= Length(AText)) and (AText[APosition] = ':') then
    begin
      Inc(APosition);
      if not TryReadRuleNumber(AText, APosition, 2, Seconds) or (Seconds > 59) then
        Exit;
    end;
  end;

  ASeconds := Sign * (Hours * SECONDS_PER_HOUR + Minutes * SECONDS_PER_MINUTE +
    Seconds);
  Result := True;
end;

{ Jn, n or Mm.w.d, then an optional /time. }
function TryReadRuleDate(const AText: string; var APosition: Integer;
  out ADate: TTimeZoneRuleDate): Boolean;
begin
  Result := False;
  ClearTimeZoneRuleDate(ADate);
  if APosition > Length(AText) then
    Exit;

  if AText[APosition] = 'J' then
  begin
    Inc(APosition);
    ADate.Kind := tzrdJulianDay;
    if not TryReadRuleNumber(AText, APosition, 3, ADate.Day) or
       (ADate.Day < 1) or (ADate.Day > DAYS_PER_COMMON_YEAR) then
      Exit;
  end
  else if AText[APosition] = 'M' then
  begin
    Inc(APosition);
    ADate.Kind := tzrdMonthWeekDay;
    if not TryReadRuleNumber(AText, APosition, 2, ADate.Month) or
       (ADate.Month < 1) or (ADate.Month > 12) then
      Exit;
    if (APosition > Length(AText)) or (AText[APosition] <> '.') then
      Exit;
    Inc(APosition);
    if not TryReadRuleNumber(AText, APosition, 1, ADate.Week) or
       (ADate.Week < 1) or (ADate.Week > RULE_LAST_WEEK) then
      Exit;
    if (APosition > Length(AText)) or (AText[APosition] <> '.') then
      Exit;
    Inc(APosition);
    if not TryReadRuleNumber(AText, APosition, 1, ADate.Day) or
       (ADate.Day >= DAYS_PER_WEEK) then
      Exit;
  end
  else
  begin
    ADate.Kind := tzrdZeroBasedDay;
    if not TryReadRuleNumber(AText, APosition, 3, ADate.Day) or
       (ADate.Day > DAYS_PER_COMMON_YEAR) then
      Exit;
  end;

  if (APosition <= Length(AText)) and (AText[APosition] = '/') then
  begin
    Inc(APosition);
    if not TryReadRuleClock(AText, APosition, RULE_MAX_TIME_HOURS,
      ADate.TimeSeconds) then
      Exit;
  end;
  Result := True;
end;

// POSIX.1-2024 Base Definitions §8.3 TZ: std offset [dst [offset]
// [,start[/time],end[/time]]], with the RFC 9636 §3.3.2 time extension.
function TryParseTimeZoneRule(const AText: string; out ARule: TTimeZoneRule): Boolean;
var
  Position, OffsetSeconds: Integer;
begin
  Result := False;
  ClearTimeZoneRule(ARule);
  Position := 1;

  if not TrySkipRuleName(AText, Position) or
     not TryReadRuleClock(AText, Position, RULE_MAX_OFFSET_HOURS, OffsetSeconds) then
    Exit;
  ARule.StandardOffsetSeconds := -OffsetSeconds;
  ARule.DaylightOffsetSeconds := ARule.StandardOffsetSeconds;

  if Position <= Length(AText) then
  begin
    if not TrySkipRuleName(AText, Position) then
      Exit;
    ARule.HasDaylightSaving := True;
    // POSIX: daylight time defaults to one hour ahead of standard time.
    ARule.DaylightOffsetSeconds := ARule.StandardOffsetSeconds + SECONDS_PER_HOUR;
    if (Position <= Length(AText)) and (AText[Position] <> ',') then
    begin
      if not TryReadRuleClock(AText, Position, RULE_MAX_OFFSET_HOURS,
        OffsetSeconds) then
        Exit;
      ARule.DaylightOffsetSeconds := -OffsetSeconds;
    end;

    // A rule without dates leaves the change dates to the implementation;
    // TZif footers always spell them out, so treat it as unusable.
    if (Position > Length(AText)) or (AText[Position] <> ',') then
      Exit;
    Inc(Position);
    if not TryReadRuleDate(AText, Position, ARule.DaylightStart) then
      Exit;
    if (Position > Length(AText)) or (AText[Position] <> ',') then
      Exit;
    Inc(Position);
    if not TryReadRuleDate(AText, Position, ARule.DaylightEnd) then
      Exit;
    if Position <= Length(AText) then
      Exit;
  end;

  ARule.Present := True;
  Result := True;
end;

function FloorDivide(const ANumerator, ADenominator: Int64): Int64;
begin
  Result := ANumerator div ADenominator;
  if (ANumerator mod ADenominator <> 0) and
     ((ANumerator < 0) <> (ADenominator < 0)) then
    Dec(Result);
end;

function FloorModulo(const ANumerator, ADenominator: Int64): Int64;
begin
  Result := ANumerator - FloorDivide(ANumerator, ADenominator) * ADenominator;
end;

function IsLeapYear(const AYear: Int64): Boolean;
begin
  Result := (AYear mod 4 = 0) and ((AYear mod 100 <> 0) or (AYear mod 400 = 0));
end;

function DaysInMonth(const AYear: Int64; const AMonth: Integer): Integer;
const
  MONTH_DAYS: array[1..12] of Integer = (31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);
begin
  Result := MONTH_DAYS[AMonth];
  if (AMonth = 2) and IsLeapYear(AYear) then
    Inc(Result);
end;

{ Days since 1970-01-01 of a proleptic Gregorian date. }
function DaysFromCivil(const AYear: Int64; const AMonth, ADay: Integer): Int64;
var
  Year, Era, YearOfEra, DayOfYear: Int64;
  ShiftedMonth: Integer;
begin
  Year := AYear;
  if AMonth <= 2 then
    Dec(Year);
  Era := FloorDivide(Year, 400);
  YearOfEra := Year - Era * 400;
  if AMonth > 2 then
    ShiftedMonth := AMonth - 3
  else
    ShiftedMonth := AMonth + 9;
  DayOfYear := (153 * ShiftedMonth + 2) div 5 + ADay - 1;
  Result := Era * 146097 + YearOfEra * 365 + YearOfEra div 4 -
    YearOfEra div 100 + DayOfYear - 719468;
end;

{ The proleptic Gregorian year containing a day since 1970-01-01. }
function YearFromDays(const ADays: Int64): Int64;
var
  ShiftedDays, Era, DayOfEra, YearOfEra, DayOfYear: Int64;
begin
  ShiftedDays := ADays + 719468;
  Era := FloorDivide(ShiftedDays, 146097);
  DayOfEra := ShiftedDays - Era * 146097;
  YearOfEra := (DayOfEra - DayOfEra div 1460 + DayOfEra div 36524 -
    DayOfEra div 146096) div 365;
  DayOfYear := DayOfEra - (365 * YearOfEra + YearOfEra div 4 - YearOfEra div 100);
  Result := YearOfEra + Era * 400;
  // The era year starts in March; January and February belong to the next.
  if (5 * DayOfYear + 2) div 153 >= 10 then
    Inc(Result);
end;

function RuleDateEpochDays(const ADate: TTimeZoneRuleDate; const AYear: Int64): Int64;
var
  FirstDayOfMonth, LastDayOfMonth: Int64;
begin
  case ADate.Kind of
    tzrdJulianDay:
      begin
        Result := DaysFromCivil(AYear, 1, 1) + ADate.Day - 1;
        // Jn never counts February 29 (day 60 in a leap year).
        if IsLeapYear(AYear) and (ADate.Day >= 60) then
          Inc(Result);
      end;
    tzrdZeroBasedDay:
      Result := DaysFromCivil(AYear, 1, 1) + ADate.Day;
  else
    begin
      FirstDayOfMonth := DaysFromCivil(AYear, ADate.Month, 1);
      Result := FirstDayOfMonth + FloorModulo(ADate.Day -
        (FirstDayOfMonth + EPOCH_WEEKDAY), DAYS_PER_WEEK) +
        (ADate.Week - 1) * DAYS_PER_WEEK;
      LastDayOfMonth := FirstDayOfMonth + DaysInMonth(AYear, ADate.Month) - 1;
      while Result > LastDayOfMonth do
        Dec(Result, DAYS_PER_WEEK);
    end;
  end;
end;

type
  TTimeZoneRuleEvent = record
    EpochSeconds: Int64;
    OffsetSeconds: Integer;
  end;

  { The rule's changes over the years around one instant; a fixed buffer
    keeps offset lookups free of heap allocation. }
  TTimeZoneRuleEvents = record
    Items: array[0..RULE_EVENT_CAPACITY - 1] of TTimeZoneRuleEvent;
    Count: Integer;
  end;

{ The rule's changes in the years within RULE_EVENT_YEAR_MARGIN of the UTC
  year of AEpochSeconds, in time order, one per instant. When two changes
  fall on the same instant the later one in rule order wins, so an all-year
  daylight-saving rule (RFC 9636 §3.3.1), whose end meets the next start,
  never shows standard time. }
procedure BuildTimeZoneRuleEvents(const ARule: TTimeZoneRule;
  const AEpochSeconds: Int64; out AEvents: TTimeZoneRuleEvents);
var
  Year, LastYear: Int64;
  Count, I, J: Integer;
  Event: TTimeZoneRuleEvent;
begin
  Year := YearFromDays(FloorDivide(AEpochSeconds, SECONDS_PER_DAY));
  LastYear := Year + RULE_EVENT_YEAR_MARGIN;
  Year := Year - RULE_EVENT_YEAR_MARGIN;
  Count := 0;
  while Year <= LastYear do
  begin
    // The start is written in standard time and the end in daylight time.
    AEvents.Items[Count].EpochSeconds := RuleDateEpochDays(ARule.DaylightStart,
      Year) * SECONDS_PER_DAY + ARule.DaylightStart.TimeSeconds -
      ARule.StandardOffsetSeconds;
    AEvents.Items[Count].OffsetSeconds := ARule.DaylightOffsetSeconds;
    Inc(Count);
    AEvents.Items[Count].EpochSeconds := RuleDateEpochDays(ARule.DaylightEnd,
      Year) * SECONDS_PER_DAY + ARule.DaylightEnd.TimeSeconds -
      ARule.DaylightOffsetSeconds;
    AEvents.Items[Count].OffsetSeconds := ARule.StandardOffsetSeconds;
    Inc(Count);
    Inc(Year);
  end;

  // Stable insertion sort keeps rule order among equal instants.
  for I := 1 to Count - 1 do
  begin
    Event := AEvents.Items[I];
    J := I - 1;
    while (J >= 0) and (AEvents.Items[J].EpochSeconds > Event.EpochSeconds) do
    begin
      AEvents.Items[J + 1] := AEvents.Items[J];
      Dec(J);
    end;
    AEvents.Items[J + 1] := Event;
  end;

  J := 0;
  for I := 1 to Count - 1 do
  begin
    if AEvents.Items[I].EpochSeconds <> AEvents.Items[J].EpochSeconds then
      Inc(J);
    AEvents.Items[J] := AEvents.Items[I];
  end;
  AEvents.Count := J + 1;
end;

function TimeZoneRuleOffsetSeconds(const ARule: TTimeZoneRule;
  const AEpochSeconds: Int64): Integer;
var
  Events: TTimeZoneRuleEvents;
  I: Integer;
begin
  Result := ARule.StandardOffsetSeconds;
  if not ARule.HasDaylightSaving then
    Exit;

  BuildTimeZoneRuleEvents(ARule, AEpochSeconds, Events);
  for I := 0 to Events.Count - 1 do
  begin
    if Events.Items[I].EpochSeconds > AEpochSeconds then
      Break;
    Result := Events.Items[I].OffsetSeconds;
  end;
end;

function TryGetTimeZoneRuleTransition(const ARule: TTimeZoneRule;
  const AEpochSeconds: Int64; const ANext: Boolean;
  out ATransitionEpochSeconds: Int64): Boolean;
var
  Events: TTimeZoneRuleEvents;
  I: Integer;
begin
  Result := False;
  ATransitionEpochSeconds := 0;
  if not ARule.Present or not ARule.HasDaylightSaving then
    Exit;

  BuildTimeZoneRuleEvents(ARule, AEpochSeconds, Events);
  // The window's first event has no known predecessor and its last event may
  // be missing the next year's start that would coincide with it, so
  // neither counts as a change; the margin keeps both well away from
  // AEpochSeconds.
  if ANext then
  begin
    for I := 1 to Events.Count - 2 do
      if (Events.Items[I].EpochSeconds > AEpochSeconds) and
         (Events.Items[I].OffsetSeconds <> Events.Items[I - 1].OffsetSeconds) then
      begin
        ATransitionEpochSeconds := Events.Items[I].EpochSeconds;
        Exit(True);
      end;
  end
  else
  begin
    for I := Events.Count - 2 downto 1 do
      if (Events.Items[I].EpochSeconds < AEpochSeconds) and
         (Events.Items[I].OffsetSeconds <> Events.Items[I - 1].OffsetSeconds) then
      begin
        ATransitionEpochSeconds := Events.Items[I].EpochSeconds;
        Exit(True);
      end;
  end;
end;

end.
