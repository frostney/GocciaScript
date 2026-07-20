unit Goccia.Temporal.Utils;

{$I Goccia.inc}

interface

type
  TTemporalDateRecord = record
    Year: Integer;
    Month: Integer;
    Day: Integer;
  end;

  TTemporalTimeRecord = record
    Hour: Integer;
    Minute: Integer;
    Second: Integer;
    Millisecond: Integer;
    Microsecond: Integer;
    Nanosecond: Integer;
  end;

  TTemporalDurationRecord = record
    Years: Int64;
    Months: Int64;
    Weeks: Int64;
    Days: Int64;
    Hours: Int64;
    Minutes: Int64;
    Seconds: Int64;
    Milliseconds: Int64;
    Microseconds: Int64;
    Nanoseconds: Int64;
  end;

function IsLeapYear(const AYear: Integer): Boolean;
function DaysInMonth(const AYear, AMonth: Integer): Integer;
function DaysInYear(const AYear: Integer): Integer;
function IsValidDate(const AYear, AMonth, ADay: Integer): Boolean;
function IsValidTime(const AHour, AMinute, ASecond, AMillisecond, AMicrosecond, ANanosecond: Integer): Boolean;

function DayOfYear(const AYear, AMonth, ADay: Integer): Integer;
function DayOfWeek(const AYear, AMonth, ADay: Integer): Integer;
function WeekOfYear(const AYear, AMonth, ADay: Integer): Integer;
function YearOfWeek(const AYear, AMonth, ADay: Integer): Integer;

function AddDaysToDate(const AYear, AMonth, ADay: Integer; const ADays: Int64): TTemporalDateRecord;
function AddMonthsToDate(const AYear, AMonth, ADay: Integer; const AMonths: Int64): TTemporalDateRecord;
function DateToEpochDays(const AYear, AMonth, ADay: Integer): Int64;
function EpochDaysToDate(const ADays: Int64): TTemporalDateRecord;

function BalanceTime(const AHour, AMinute, ASecond, AMillisecond, AMicrosecond, ANanosecond: Int64;
  out AExtraDays: Int64): TTemporalTimeRecord;

function PadISOYear(const AYear: Integer): string;
function PadTwo(const AValue: Integer): string;
function FormatTimeString(const AHour, AMinute, ASecond, AMillisecond, AMicrosecond, ANanosecond: Integer): string;
function FormatDateString(const AYear, AMonth, ADay: Integer): string;
function IsValidTemporalCalendarIdentifier(const AValue: string): Boolean;
function CanonicalizeTemporalCalendarIdentifier(const AValue: string): string;
function ParseTemporalCalendarStringIdentifier(const AValue: string): string;
function TryExtractTemporalCalendarAnnotation(const AStr: string; out ACalendarId: string): Boolean;

function TryParseISODate(const AStr: string; out ADate: TTemporalDateRecord): Boolean;
function TryParseISOTime(const AStr: string; out ATime: TTemporalTimeRecord): Boolean;
function TryParseISODateTime(const AStr: string; out ADate: TTemporalDateRecord; out ATime: TTemporalTimeRecord): Boolean;
function TryParseISODuration(const AStr: string; out ADuration: TTemporalDurationRecord): Boolean;

function TryParseISOYearMonth(const AStr: string; out AYear, AMonth: Integer): Boolean;
function TryParseISOMonthDay(const AStr: string; out AMonth, ADay: Integer): Boolean;
function TryParseISODateTimeWithOffset(const AStr: string; out ADate: TTemporalDateRecord;
  out ATime: TTemporalTimeRecord; out AOffsetSeconds: Integer; out ATimeZone: string): Boolean;
function FormatYearMonthString(const AYear, AMonth: Integer): string;
function FormatMonthDayString(const AMonth, ADay: Integer): string;

function StripAnnotations(const AStr: string): string;
function StripOffsetAndAnnotations(const AStr: string): string;

{ Unified Temporal string coercion — use these from CoerceXxx functions }
function CoerceToISODate(const AStr: string; out ADate: TTemporalDateRecord): Boolean;
function CoerceToISOTime(const AStr: string; out ATime: TTemporalTimeRecord): Boolean;
function CoerceToISODateTime(const AStr: string; out ADate: TTemporalDateRecord; out ATime: TTemporalTimeRecord): Boolean;
function CoerceToISOInstant(const AStr: string; out ADate: TTemporalDateRecord;
  out ATime: TTemporalTimeRecord; out AOffsetSeconds: Integer): Boolean;
function CoerceToISOYearMonth(const AStr: string; out AYear, AMonth: Integer): Boolean;
function CoerceToISOMonthDay(const AStr: string; out AMonth, ADay: Integer): Boolean;

function CompareIntegers(const A, B: Integer): Integer; {$IFDEF FPC}inline;{$ENDIF}
function CompareDates(const AYear1, AMonth1, ADay1, AYear2, AMonth2, ADay2: Integer): Integer;
function CompareTimes(const AHour1, AMinute1, ASecond1, AMs1, AUs1, ANs1,
  AHour2, AMinute2, ASecond2, AMs2, AUs2, ANs2: Integer): Integer;

implementation

uses
  SysUtils;

function IsLeapYear(const AYear: Integer): Boolean;
begin
  Result := ((AYear mod 4) = 0) and (((AYear mod 100) <> 0) or ((AYear mod 400) = 0));
end;

function DaysInMonth(const AYear, AMonth: Integer): Integer;
const
  DaysPerMonth: array[1..12] of Integer = (31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);
begin
  if (AMonth < 1) or (AMonth > 12) then
    Result := 0
  else if (AMonth = 2) and IsLeapYear(AYear) then
    Result := 29
  else
    Result := DaysPerMonth[AMonth];
end;

function DaysInYear(const AYear: Integer): Integer;
begin
  if IsLeapYear(AYear) then
    Result := 366
  else
    Result := 365;
end;

function IsValidDate(const AYear, AMonth, ADay: Integer): Boolean;
begin
  Result := (AMonth >= 1) and (AMonth <= 12) and
            (ADay >= 1) and (ADay <= DaysInMonth(AYear, AMonth));
end;

function IsValidTime(const AHour, AMinute, ASecond, AMillisecond, AMicrosecond, ANanosecond: Integer): Boolean;
begin
  Result := (AHour >= 0) and (AHour <= 23) and
            (AMinute >= 0) and (AMinute <= 59) and
            (ASecond >= 0) and (ASecond <= 59) and
            (AMillisecond >= 0) and (AMillisecond <= 999) and
            (AMicrosecond >= 0) and (AMicrosecond <= 999) and
            (ANanosecond >= 0) and (ANanosecond <= 999);
end;

function DayOfYear(const AYear, AMonth, ADay: Integer): Integer;
var
  M: Integer;
begin
  Result := 0;
  for M := 1 to AMonth - 1 do
    Result := Result + DaysInMonth(AYear, M);
  Result := Result + ADay;
end;

function DayOfWeek(const AYear, AMonth, ADay: Integer): Integer;
var
  EpochDays: Int64;
  D: Integer;
begin
  // Use epoch days: 1970-01-01 was Thursday (ISO day 4)
  EpochDays := DateToEpochDays(AYear, AMonth, ADay);
  D := Integer(((EpochDays mod 7) + 7 + 3) mod 7) + 1;
  Result := D;
end;

function WeekOfYear(const AYear, AMonth, ADay: Integer): Integer;
var
  Jan1DayOfWeek, DOY, WeekDay: Integer;
  PrevYearDays: Integer;
begin
  DOY := DayOfYear(AYear, AMonth, ADay);
  WeekDay := DayOfWeek(AYear, AMonth, ADay);
  Jan1DayOfWeek := DayOfWeek(AYear, 1, 1);

  // ISO week calculation
  Result := (DOY - WeekDay + 10) div 7;

  if Result < 1 then
  begin
    // Belongs to last week of previous year
    PrevYearDays := DaysInYear(AYear - 1);
    Jan1DayOfWeek := DayOfWeek(AYear - 1, 1, 1);
    if (Jan1DayOfWeek = 4) or ((Jan1DayOfWeek = 3) and IsLeapYear(AYear - 1)) then
      Result := 53
    else
      Result := 52;
  end
  else if Result = 53 then
  begin
    // Check if week 53 is valid for this year
    if (Jan1DayOfWeek = 4) or ((Jan1DayOfWeek = 3) and IsLeapYear(AYear)) then
      Result := 53
    else
      Result := 1;
  end;
end;

function YearOfWeek(const AYear, AMonth, ADay: Integer): Integer;
var
  Week: Integer;
begin
  Week := WeekOfYear(AYear, AMonth, ADay);
  if (Week >= 52) and (AMonth = 1) then
    Result := AYear - 1
  else if (Week = 1) and (AMonth = 12) then
    Result := AYear + 1
  else
    Result := AYear;
end;

function FloorDivide(const ADividend, ADivisor: Int64): Int64;
begin
  Result := ADividend div ADivisor;
  if ((ADividend < 0) <> (ADivisor < 0)) and ((ADividend mod ADivisor) <> 0) then
    Dec(Result);
end;

function DateToEpochDays(const AYear, AMonth, ADay: Integer): Int64;
var
  Y, M: Integer;
  EpochDays: Int64;
begin
  // Algorithm to convert a date to days since Unix epoch (1970-01-01)
  Y := AYear;
  M := AMonth;
  if M <= 2 then
  begin
    Dec(Y);
    Inc(M, 9);
  end
  else
    Dec(M, 3);

  // Days from year
  EpochDays := Int64(365) * Y + FloorDivide(Y, 4) - FloorDivide(Y, 100) +
    FloorDivide(Y, 400);
  // Days from month
  EpochDays := EpochDays + (M * 153 + 2) div 5;
  // Days
  EpochDays := EpochDays + ADay;
  // Adjust to Unix epoch
  EpochDays := EpochDays - 719469;

  Result := EpochDays;
end;

function EpochDaysToDate(const ADays: Int64): TTemporalDateRecord;
var
  Z, Era, DOE, YOE, Y, DOY, MP, D, M: Int64;
begin
  // Algorithm from http://howardhinnant.github.io/date_algorithms.html
  Z := ADays + 719468;
  if Z >= 0 then
    Era := Z div 146097
  else
    Era := (Z - 146096) div 146097;
  DOE := Z - Era * 146097;
  YOE := (DOE - DOE div 1460 + DOE div 36524 - DOE div 146096) div 365;
  Y := YOE + Era * 400;
  DOY := DOE - (365 * YOE + YOE div 4 - YOE div 100);
  MP := (5 * DOY + 2) div 153;
  D := DOY - (153 * MP + 2) div 5 + 1;
  if MP < 10 then
    M := MP + 3
  else
    M := MP - 9;
  if M <= 2 then
    Inc(Y);

  Result.Year := Integer(Y);
  Result.Month := Integer(M);
  Result.Day := Integer(D);
end;

function AddDaysToDate(const AYear, AMonth, ADay: Integer; const ADays: Int64): TTemporalDateRecord;
var
  EpochDays: Int64;
begin
  EpochDays := DateToEpochDays(AYear, AMonth, ADay);
  Result := EpochDaysToDate(EpochDays + ADays);
end;

function AddMonthsToDate(const AYear, AMonth, ADay: Integer; const AMonths: Int64): TTemporalDateRecord;
var
  TotalMonths: Int64;
  NewYear: Int64;
  NewMonth: Integer;
  MaxDay: Integer;
begin
  TotalMonths := Int64(AYear) * 12 + (AMonth - 1) + AMonths;
  NewYear := TotalMonths div 12;
  NewMonth := Integer(TotalMonths mod 12) + 1;
  if NewMonth < 1 then
  begin
    Inc(NewMonth, 12);
    Dec(NewYear);
  end;
  Result.Year := Integer(NewYear);
  Result.Month := NewMonth;
  MaxDay := DaysInMonth(Result.Year, Result.Month);
  if ADay > MaxDay then
    Result.Day := MaxDay
  else
    Result.Day := ADay;
end;

function BalanceTime(const AHour, AMinute, ASecond, AMillisecond, AMicrosecond, ANanosecond: Int64;
  out AExtraDays: Int64): TTemporalTimeRecord;
var
  TotalNs, Remainder: Int64;
begin
  TotalNs := ANanosecond +
             AMicrosecond * 1000 +
             AMillisecond * 1000000 +
             ASecond * Int64(1000000000) +
             AMinute * Int64(60000000000) +
             AHour * Int64(3600000000000);

  if TotalNs >= 0 then
  begin
    AExtraDays := TotalNs div Int64(86400000000000);
    Remainder := TotalNs mod Int64(86400000000000);
  end
  else
  begin
    AExtraDays := -(-TotalNs + Int64(86400000000000) - 1) div Int64(86400000000000);
    Remainder := TotalNs - AExtraDays * Int64(86400000000000);
  end;

  Result.Hour := Integer(Remainder div Int64(3600000000000));
  Remainder := Remainder mod Int64(3600000000000);
  Result.Minute := Integer(Remainder div Int64(60000000000));
  Remainder := Remainder mod Int64(60000000000);
  Result.Second := Integer(Remainder div Int64(1000000000));
  Remainder := Remainder mod Int64(1000000000);
  Result.Millisecond := Integer(Remainder div 1000000);
  Remainder := Remainder mod 1000000;
  Result.Microsecond := Integer(Remainder div 1000);
  Result.Nanosecond := Integer(Remainder mod 1000);
end;

function PadISOYear(const AYear: Integer): string;
begin
  if (AYear >= 0) and (AYear <= 9999) then
    Result := Format('%.4d', [AYear])
  else if AYear > 9999 then
    Result := '+' + Format('%.6d', [AYear])
  else
    Result := '-' + Format('%.6d', [-AYear]);
end;

function PadTwo(const AValue: Integer): string;
begin
  Result := Format('%.2d', [AValue]);
end;

function FormatDateString(const AYear, AMonth, ADay: Integer): string;
begin
  Result := PadISOYear(AYear) + '-' + PadTwo(AMonth) + '-' + PadTwo(ADay);
end;

function FormatTimeString(const AHour, AMinute, ASecond, AMillisecond, AMicrosecond, ANanosecond: Integer): string;
var
  Fraction: string;
begin
  Result := PadTwo(AHour) + ':' + PadTwo(AMinute) + ':' + PadTwo(ASecond);

  if (AMillisecond <> 0) or (AMicrosecond <> 0) or (ANanosecond <> 0) then
  begin
    Fraction := Format('%.3d%.3d%.3d', [AMillisecond, AMicrosecond, ANanosecond]);
    while (Length(Fraction) > 0) and (Fraction[Length(Fraction)] = '0') do
      Delete(Fraction, Length(Fraction), 1);
    Result := Result + '.' + Fraction;
  end;
end;

function TryParseDigits(const AStr: string; var APos: Integer; const ACount: Integer; out AValue: Integer): Boolean;
var
  I: Integer;
  C: Char;
begin
  AValue := 0;
  if APos + ACount - 1 > Length(AStr) then
  begin
    Result := False;
    Exit;
  end;
  for I := 0 to ACount - 1 do
  begin
    C := AStr[APos + I];
    if (C < '0') or (C > '9') then
    begin
      Result := False;
      Exit;
    end;
    AValue := AValue * 10 + (Ord(C) - Ord('0'));
  end;
  Inc(APos, ACount);
  Result := True;
end;

function TryParseVariableDigits(const AStr: string; var APos: Integer; out AValue: Int64; out ADigitCount: Integer): Boolean;
var
  C: Char;
begin
  AValue := 0;
  ADigitCount := 0;
  while APos <= Length(AStr) do
  begin
    C := AStr[APos];
    if (C < '0') or (C > '9') then
      Break;
    AValue := AValue * 10 + (Ord(C) - Ord('0'));
    Inc(APos);
    Inc(ADigitCount);
  end;
  Result := ADigitCount > 0;
end;

function HasFractionWithMoreThanNineDigits(const AStr: string): Boolean;
var
  I, Count: Integer;
begin
  Result := False;
  I := 1;
  while I <= Length(AStr) do
  begin
    if (AStr[I] = '.') or (AStr[I] = ',') then
    begin
      Count := 0;
      Inc(I);
      while (I <= Length(AStr)) and (AStr[I] >= '0') and (AStr[I] <= '9') do
      begin
        Inc(Count);
        if Count > 9 then
          Exit(True);
        Inc(I);
      end;
    end
    else
      Inc(I);
  end;
end;

function IsValidAnnotationKey(const AKey: string): Boolean;
var
  I: Integer;
  C: Char;
  InSegment: Boolean;
begin
  Result := False;
  if Length(AKey) = 0 then Exit;
  if not (((AKey[1] >= 'a') and (AKey[1] <= 'z')) or (AKey[1] = '_')) then Exit;
  InSegment := True;
  for I := 2 to Length(AKey) do
  begin
    C := AKey[I];
    if C = '-' then
    begin
      if not InSegment then Exit; // double dash
      InSegment := False;
    end
    else if ((C >= 'a') and (C <= 'z')) or ((C >= '0') and (C <= '9')) or
            (C = '_') then
      InSegment := True
    else
      Exit; // invalid char
  end;
  if not InSegment then Exit; // trailing dash
  Result := True;
end;

function IsValidAnnotationValue(const AValue: string): Boolean;
var
  I: Integer;
  Ch: Char;
  ComponentHasCharacter: Boolean;
begin
  Result := False;
  if AValue = '' then
    Exit;

  ComponentHasCharacter := False;
  for I := 1 to Length(AValue) do
  begin
    Ch := AValue[I];
    if Ch = '-' then
    begin
      if not ComponentHasCharacter then
        Exit;
      ComponentHasCharacter := False;
    end
    else if ((Ch >= 'a') and (Ch <= 'z')) or
            ((Ch >= 'A') and (Ch <= 'Z')) or
            ((Ch >= '0') and (Ch <= '9')) then
      ComponentHasCharacter := True
    else
      Exit;
  end;

  Result := ComponentHasCharacter;
end;

function IsValidTemporalCalendarIdentifier(const AValue: string): Boolean;
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
    if not (((Ch >= 'a') and (Ch <= 'z')) or
            ((Ch >= '0') and (Ch <= '9')) or
            (Ch = '-')) then
      Exit;
  end;

  Result := True;
end;

function CanonicalizeTemporalCalendarIdentifier(const AValue: string): string;
var
  LowerValue: string;
begin
  LowerValue := LowerCase(AValue);
  if not IsValidTemporalCalendarIdentifier(LowerValue) then
    Exit('');

  if LowerValue = 'gregorian' then
    Result := 'gregory'
  else if LowerValue = 'islamicc' then
    Result := 'islamic-civil'
  else if LowerValue = 'ethiopic-amete-alem' then
    Result := 'ethioaa'
  else if (LowerValue = 'buddhist') or
          (LowerValue = 'chinese') or
          (LowerValue = 'coptic') or
          (LowerValue = 'dangi') or
          (LowerValue = 'ethioaa') or
          (LowerValue = 'ethiopic') or
          (LowerValue = 'gregory') or
          (LowerValue = 'hebrew') or
          (LowerValue = 'indian') or
          (LowerValue = 'islamic') or
          (LowerValue = 'islamic-civil') or
          (LowerValue = 'islamic-rgsa') or
          (LowerValue = 'islamic-tbla') or
          (LowerValue = 'islamic-umalqura') or
          (LowerValue = 'iso8601') or
          (LowerValue = 'japanese') or
          (LowerValue = 'persian') or
          (LowerValue = 'roc') then
    Result := LowerValue
  else
    Result := '';
end;

// TC39 Temporal §13.36 ParseTemporalCalendarString(string)
function ParseTemporalCalendarStringIdentifier(const AValue: string): string;
var
  CalendarId: string;
  DateRec: TTemporalDateRecord;
  TimeRec: TTemporalTimeRecord;
  TimeValue: string;
  Year, Month, Day: Integer;
  OffsetSeconds: Integer;
  TimeZone: string;
  HasDateTimeSeparator: Boolean;
begin
  Result := CanonicalizeTemporalCalendarIdentifier(AValue);
  if Result <> '' then
    Exit;

  CalendarId := 'iso8601';
  if not TryExtractTemporalCalendarAnnotation(AValue, CalendarId) then
    Exit;

  HasDateTimeSeparator := (System.Pos('T', AValue) > 1) or
    (System.Pos('t', AValue) > 1) or (System.Pos(' ', AValue) > 1);
  TimeValue := AValue;
  if (Length(TimeValue) > 1) and
     ((TimeValue[1] = 'T') or (TimeValue[1] = 't')) then
    TimeValue := Copy(TimeValue, 2, Length(TimeValue) - 1);

  if CoerceToISODate(AValue, DateRec) or
     CoerceToISODateTime(AValue, DateRec, TimeRec) or
     ((not HasDateTimeSeparator) and CoerceToISOTime(TimeValue, TimeRec)) or
     CoerceToISOYearMonth(AValue, Year, Month) or
     CoerceToISOMonthDay(AValue, Month, Day) or
     TryParseISODateTimeWithOffset(AValue, DateRec, TimeRec, OffsetSeconds, TimeZone) then
    Result := CalendarId
  else
    Result := '';
end;

function TryStripTemporalAnnotations(const AStr: string; out AStripped: string;
  out ACalendarId: string): Boolean;
var
  Value: string;
  BracketStart, BracketEnd, EqualsPos: Integer;
  Annotation, Key, CalendarValue: string;
  Critical: Boolean;
  Annotations: array of string;
  CriticalAnnotations: array of Boolean;
  Count, I: Integer;
  CalendarSeen, CalendarCriticalSeen, TimeZoneSeen: Boolean;
  CalendarCount: Integer;
  CanonicalCalendar: string;
begin
  Result := False;
  AStripped := AStr;
  ACalendarId := 'iso8601';
  Value := AStr;
  Count := 0;

  while (Length(Value) > 0) and (Value[Length(Value)] = ']') do
  begin
    BracketEnd := Length(Value);
    BracketStart := BracketEnd - 1;
    while (BracketStart > 0) and (Value[BracketStart] <> '[') do
      Dec(BracketStart);
    if BracketStart = 0 then
      Exit;

    Annotation := Copy(Value, BracketStart + 1, BracketEnd - BracketStart - 1);
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

    Value := Copy(Value, 1, BracketStart - 1);
  end;

  CalendarSeen := False;
  CalendarCriticalSeen := False;
  TimeZoneSeen := False;
  CalendarCount := 0;

  for I := Count - 1 downto 0 do
  begin
    Annotation := Annotations[I];
    Critical := CriticalAnnotations[I];
    EqualsPos := System.Pos('=', Annotation);
    if EqualsPos > 0 then
    begin
      Key := Copy(Annotation, 1, EqualsPos - 1);
      CalendarValue := Copy(Annotation, EqualsPos + 1, Length(Annotation) - EqualsPos);
      if not IsValidAnnotationKey(Key) then
        Exit;

      if Key = 'u-ca' then
      begin
        Inc(CalendarCount);
        if Critical then
          CalendarCriticalSeen := True;
        if (CalendarCount > 1) and (CalendarCriticalSeen or Critical) then
          Exit;
        if not CalendarSeen then
        begin
          CanonicalCalendar := CanonicalizeTemporalCalendarIdentifier(CalendarValue);
          if CanonicalCalendar = '' then
            Exit;
          ACalendarId := CanonicalCalendar;
          CalendarSeen := True;
        end;
      end
      else if Critical then
        Exit;
    end
    else
    begin
      if TimeZoneSeen then
        Exit;
      TimeZoneSeen := True;
    end;
  end;

  AStripped := Value;
  Result := True;
end;

function TryExtractTemporalCalendarAnnotation(const AStr: string; out ACalendarId: string): Boolean;
var
  Stripped: string;
begin
  Result := TryStripTemporalAnnotations(AStr, Stripped, ACalendarId);
end;

function StripAnnotations(const AStr: string): string;
var
  CalendarId: string;
begin
  if not TryStripTemporalAnnotations(AStr, Result, CalendarId) then
    Result := AStr;
end;

function IsValidTemporalOffsetSuffix(const AStr: string): Boolean;
var
  Pos, Hour, Minute, Second: Integer;
  FracVal: Int64;
  FracDigits: Integer;
  ExtendedMinute: Boolean;
begin
  Result := False;
  if Length(AStr) < 3 then
    Exit;

  Pos := 1;
  if (AStr[Pos] <> '+') and (AStr[Pos] <> '-') then
    Exit;
  Inc(Pos);

  if not TryParseDigits(AStr, Pos, 2, Hour) then
    Exit;
  if (Hour < 0) or (Hour > 23) then
    Exit;
  if Pos > Length(AStr) then
    Exit(True);

  ExtendedMinute := False;
  if AStr[Pos] = ':' then
  begin
    ExtendedMinute := True;
    Inc(Pos);
  end;
  if not TryParseDigits(AStr, Pos, 2, Minute) then
    Exit;
  if (Minute < 0) or (Minute > 59) then
    Exit;
  if Pos > Length(AStr) then
    Exit(True);

  if ExtendedMinute then
  begin
    if AStr[Pos] <> ':' then
      Exit;
    Inc(Pos);
  end;
  if not TryParseDigits(AStr, Pos, 2, Second) then
    Exit;
  if (Second < 0) or (Second > 59) then
    Exit;

  if (Pos <= Length(AStr)) and ((AStr[Pos] = '.') or (AStr[Pos] = ',')) then
  begin
    Inc(Pos);
    if not TryParseVariableDigits(AStr, Pos, FracVal, FracDigits) then
      Exit;
    if FracDigits > 9 then
      Exit;
  end;

  Result := Pos > Length(AStr);
end;

function TemporalOffsetIdentifierHasSubMinuteSyntax(const AStr: string): Boolean;
var
  Pos, Hour, Minute, Second: Integer;
  FracVal: Int64;
  FracDigits: Integer;
  Extended: Boolean;
begin
  Result := False;
  if Length(AStr) < 6 then
    Exit;
  if not (AStr[1] in ['+', '-']) then
    Exit;

  Pos := 2;
  if not TryParseDigits(AStr, Pos, 2, Hour) then
    Exit;
  Extended := (Pos <= Length(AStr)) and (AStr[Pos] = ':');
  if Extended then
    Inc(Pos);
  if not TryParseDigits(AStr, Pos, 2, Minute) then
    Exit;
  if Pos > Length(AStr) then
    Exit;

  if Extended then
  begin
    if AStr[Pos] <> ':' then
      Exit;
    Inc(Pos);
  end;
  if not TryParseDigits(AStr, Pos, 2, Second) then
    Exit;
  if (Pos <= Length(AStr)) and ((AStr[Pos] = '.') or (AStr[Pos] = ',')) then
  begin
    Inc(Pos);
    if not TryParseVariableDigits(AStr, Pos, FracVal, FracDigits) then
      Exit;
  end;

  Result := (Hour <= 23) and (Minute <= 59) and (Second <= 59) and
    (Pos > Length(AStr));
end;

function StripOffsetAndAnnotations(const AStr: string): string;
var
  I, TPos, ScanStart, OffsetStart: Integer;
begin
  Result := StripAnnotations(AStr);

  TPos := System.Pos('T', Result);
  if TPos = 0 then
    TPos := System.Pos('t', Result);
  if TPos = 0 then
    TPos := System.Pos(' ', Result);
  if TPos > 0 then
    ScanStart := TPos + 1
  else
    ScanStart := 1;

  OffsetStart := 0;
  I := Length(Result);
  while I >= ScanStart do
  begin
    if (Result[I] = '+') or (Result[I] = '-') then
    begin
      OffsetStart := I;
      Break;
    end;
    Dec(I);
  end;

  if (OffsetStart > 0) and
     IsValidTemporalOffsetSuffix(Copy(Result, OffsetStart, Length(Result) - OffsetStart + 1)) then
    Result := Copy(Result, 1, OffsetStart - 1);
end;

function TryStripTemporalAnnotationsForTime(const AStr: string; out AStripped: string): Boolean;
var
  Value: string;
  BracketStart, BracketEnd, EqualsPos: Integer;
  Annotation, Key, CalendarValue: string;
  Critical: Boolean;
  TimeZoneSeen: Boolean;
  CalendarCount: Integer;
  CalendarCriticalSeen: Boolean;
begin
  Result := False;
  AStripped := AStr;
  Value := AStr;
  TimeZoneSeen := False;
  CalendarCount := 0;
  CalendarCriticalSeen := False;

  while (Length(Value) > 0) and (Value[Length(Value)] = ']') do
  begin
    BracketEnd := Length(Value);
    BracketStart := BracketEnd - 1;
    while (BracketStart > 0) and (Value[BracketStart] <> '[') do
      Dec(BracketStart);
    if BracketStart = 0 then
      Exit;

    Annotation := Copy(Value, BracketStart + 1, BracketEnd - BracketStart - 1);
    Critical := (Length(Annotation) > 0) and (Annotation[1] = '!');
    if Critical then
      Annotation := Copy(Annotation, 2, Length(Annotation) - 1);
    if Annotation = '' then
      Exit;

    EqualsPos := System.Pos('=', Annotation);
    if EqualsPos > 0 then
    begin
      Key := Copy(Annotation, 1, EqualsPos - 1);
      if not IsValidAnnotationKey(Key) then
        Exit;
      if Key = 'u-ca' then
      begin
        CalendarValue := Copy(Annotation, EqualsPos + 1,
          Length(Annotation) - EqualsPos);
        // ParseISODateTime records the annotation, but ToTemporalTime ignores
        // its calendar value. Unknown identifiers are valid here, but the
        // value must still match the RFC 9557 AnnotationValue grammar.
        if not IsValidAnnotationValue(CalendarValue) then
          Exit;
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
      if TimeZoneSeen then
        Exit;
      TimeZoneSeen := True;
    end;

    Value := Copy(Value, 1, BracketStart - 1);
  end;

  AStripped := Value;
  Result := True;
end;

function StripOffsetAndAnnotationsForTime(const AStr: string): string;
var
  I, TPos, ScanStart, OffsetStart: Integer;
begin
  if not TryStripTemporalAnnotationsForTime(AStr, Result) then
    Result := AStr;

  TPos := System.Pos('T', Result);
  if TPos = 0 then
    TPos := System.Pos('t', Result);
  if TPos = 0 then
    TPos := System.Pos(' ', Result);
  if TPos > 0 then
    ScanStart := TPos + 1
  else
    ScanStart := 1;

  OffsetStart := 0;
  I := Length(Result);
  while I >= ScanStart do
  begin
    if (Result[I] = '+') or (Result[I] = '-') then
    begin
      OffsetStart := I;
      Break;
    end;
    Dec(I);
  end;

  if (OffsetStart > 0) and
     IsValidTemporalOffsetSuffix(Copy(Result, OffsetStart, Length(Result) - OffsetStart + 1)) then
    Result := Copy(Result, 1, OffsetStart - 1);
end;

function TemporalStringHasUTCDesignator(const AStr: string): Boolean;
var
  S: string;
begin
  S := StripAnnotations(AStr);
  Result := (Length(S) > 0) and ((S[Length(S)] = 'Z') or (S[Length(S)] = 'z'));
end;

function PlainTimeStringRequiresDesignator(const AStr: string): Boolean;
var
  S: string;
  Year, Month, Day: Integer;

  function ParseCompactDigits(const AValue: string; const AStart, ACount: Integer;
    out ANumber: Integer): Boolean;
  var
    I: Integer;
    Ch: Char;
  begin
    ANumber := 0;
    if AStart + ACount - 1 > Length(AValue) then
      Exit(False);
    for I := 0 to ACount - 1 do
    begin
      Ch := AValue[AStart + I];
      if (Ch < '0') or (Ch > '9') then
        Exit(False);
      ANumber := ANumber * 10 + Ord(Ch) - Ord('0');
    end;
    Result := True;
  end;

  function IsCompactYearMonth(const AValue: string): Boolean;
  begin
    Result := (Length(AValue) = 6) and
      ParseCompactDigits(AValue, 1, 4, Year) and
      ParseCompactDigits(AValue, 5, 2, Month) and
      (Month >= 1) and (Month <= 12);
  end;

  function IsCompactMonthDay(const AValue: string): Boolean;
  begin
    Result := (Length(AValue) = 4) and
      ParseCompactDigits(AValue, 1, 2, Month) and
      ParseCompactDigits(AValue, 3, 2, Day) and
      (Month >= 1) and (Month <= 12) and
      (Day >= 1) and (Day <= DaysInMonth(1972, Month));
  end;
begin
  S := StripAnnotations(AStr);
  if S = '' then
    Exit(False);
  if (S[1] = 'T') or (S[1] = 't') then
    Exit(False);
  if (System.Pos('T', S) > 0) or (System.Pos('t', S) > 0) then
    Exit(False);
  if (System.Pos(' ', S) > 1) then
    Exit(False);
  Result := (S[1] = ' ') or TryParseISOYearMonth(S, Year, Month) or
    TryParseISOMonthDay(S, Month, Day) or IsCompactYearMonth(S) or
    IsCompactMonthDay(S);
end;

function TryParseISODate(const AStr: string; out ADate: TTemporalDateRecord): Boolean;
var
  Pos, Sign: Integer;
  Year, Month, Day: Integer;
  C: Char;
  S: string;
  Extended: Boolean;
begin
  Result := False;
  S := StripAnnotations(AStr);
  Pos := 1;
  Sign := 1;
  Extended := False;

  // Check for sign prefix
  if Pos <= Length(S) then
  begin
    C := S[Pos];
    if C = '+' then
    begin
      Inc(Pos);
      if not TryParseDigits(S, Pos, 6, Year) then Exit;
    end
    else if C = '-' then
    begin
      Inc(Pos);
      Sign := -1;
      if not TryParseDigits(S, Pos, 6, Year) then Exit;
      if Year = 0 then Exit;
    end
    else
    begin
      if not TryParseDigits(S, Pos, 4, Year) then Exit;
    end;
  end
  else
    Exit;

  Year := Year * Sign;

  if (Pos <= Length(S)) and (S[Pos] = '-') then
  begin
    Extended := True;
    Inc(Pos);
  end;

  if not TryParseDigits(S, Pos, 2, Month) then Exit;

  if Extended then
  begin
    if (Pos > Length(S)) or (S[Pos] <> '-') then Exit;
    Inc(Pos);
  end;

  if not TryParseDigits(S, Pos, 2, Day) then Exit;

  // Must be at end of string (for pure date parsing)
  if Pos <= Length(S) then Exit;

  if not IsValidDate(Year, Month, Day) then Exit;

  ADate.Year := Year;
  ADate.Month := Month;
  ADate.Day := Day;
  Result := True;
end;

function TryParseISOTime(const AStr: string; out ATime: TTemporalTimeRecord): Boolean;
var
  Pos: Integer;
  Hour, Minute, Second: Integer;
  FracVal: Int64;
  FracDigits: Integer;
  HasSecond: Boolean;
  ExtendedMinute: Boolean;
begin
  Result := False;
  ATime.Hour := 0;
  ATime.Minute := 0;
  ATime.Second := 0;
  ATime.Millisecond := 0;
  ATime.Microsecond := 0;
  ATime.Nanosecond := 0;

  Pos := 1;

  if not TryParseDigits(AStr, Pos, 2, Hour) then Exit;
  Minute := 0;
  ExtendedMinute := False;
  if Pos <= Length(AStr) then
  begin
    if AStr[Pos] = ':' then
    begin
      ExtendedMinute := True;
      Inc(Pos);
      if not TryParseDigits(AStr, Pos, 2, Minute) then Exit;
    end
    else if (Pos + 1 <= Length(AStr)) and
            (AStr[Pos] >= '0') and (AStr[Pos] <= '9') and
            (AStr[Pos + 1] >= '0') and (AStr[Pos + 1] <= '9') then
    begin
      if not TryParseDigits(AStr, Pos, 2, Minute) then Exit;
    end
    else if (AStr[Pos] <> '.') and (AStr[Pos] <> ',') then
      Exit;
  end;

  Second := 0;
  HasSecond := False;
  if (Pos <= Length(AStr)) and (AStr[Pos] = ':') then
  begin
    if not ExtendedMinute then Exit;
    Inc(Pos);
    if not TryParseDigits(AStr, Pos, 2, Second) then Exit;
    HasSecond := True;
  end
  else if (not ExtendedMinute) and (Pos + 1 <= Length(AStr)) and
          (AStr[Pos] >= '0') and (AStr[Pos] <= '9') and
          (AStr[Pos + 1] >= '0') and (AStr[Pos + 1] <= '9') then
  begin
    if not TryParseDigits(AStr, Pos, 2, Second) then Exit;
    HasSecond := True;
  end;

  // Fractional seconds
  if HasSecond and (Pos <= Length(AStr)) and ((AStr[Pos] = '.') or (AStr[Pos] = ',')) then
  begin
    Inc(Pos);
    if not TryParseVariableDigits(AStr, Pos, FracVal, FracDigits) then Exit;
    if FracDigits > 9 then Exit;

    // Pad to 9 digits (nanosecond precision).
    while FracDigits < 9 do
    begin
      FracVal := FracVal * 10;
      Inc(FracDigits);
    end;

    ATime.Millisecond := Integer(FracVal div 1000000);
    ATime.Microsecond := Integer((FracVal div 1000) mod 1000);
    ATime.Nanosecond := Integer(FracVal mod 1000);
  end;

  // Clamp leap second (60) to 59
  if Second = 60 then
    Second := 59;

  if not IsValidTime(Hour, Minute, Second, ATime.Millisecond, ATime.Microsecond, ATime.Nanosecond) then Exit;

  ATime.Hour := Hour;
  ATime.Minute := Minute;
  ATime.Second := Second;
  Result := Pos > Length(AStr);
end;

function TryParseISODateTime(const AStr: string; out ADate: TTemporalDateRecord; out ATime: TTemporalTimeRecord): Boolean;
var
  TPos: Integer;
  DatePart, TimePart, S: string;
begin
  Result := False;
  ATime.Hour := 0;
  ATime.Minute := 0;
  ATime.Second := 0;
  ATime.Millisecond := 0;
  ATime.Microsecond := 0;
  ATime.Nanosecond := 0;

  if HasFractionWithMoreThanNineDigits(AStr) then Exit;

  S := AStr;

  TPos := System.Pos('T', S);
  if TPos = 0 then
    TPos := System.Pos('t', S);
  if TPos = 0 then
    TPos := System.Pos(' ', S);

  if TPos = 0 then
  begin
    // Try as date only (StripAnnotations handled inside TryParseISODate)
    Result := TryParseISODate(S, ADate);
    Exit;
  end;

  DatePart := Copy(S, 1, TPos - 1);
  TimePart := Copy(S, TPos + 1, Length(S) - TPos);

  // Strip offset and annotations from time part before parsing
  TimePart := StripOffsetAndAnnotations(TimePart);
  if Length(TimePart) = 0 then
    Exit;

  if not TryParseISODate(DatePart, ADate) then Exit;
  if not TryParseISOTime(TimePart, ATime) then Exit;

  Result := True;
end;

function TryParseISODuration(const AStr: string; out ADuration: TTemporalDurationRecord): Boolean;
var
  Pos: Integer;
  Sign: Int64;
  InTimePart: Boolean;
  Val: Int64;
  DigitCount: Integer;
  C: Char;
  FracVal: Int64;
  FracDigits: Integer;
  HasAnyComponent: Boolean;
begin
  Result := False;
  FillChar(ADuration, SizeOf(ADuration), 0);

  if Length(AStr) < 2 then Exit;

  Pos := 1;
  Sign := 1;

  // Optional sign
  if AStr[Pos] = '-' then
  begin
    Sign := -1;
    Inc(Pos);
  end
  else if AStr[Pos] = '+' then
    Inc(Pos);

  // Must start with 'P'
  if (Pos > Length(AStr)) or (UpCase(AStr[Pos]) <> 'P') then Exit;
  Inc(Pos);

  InTimePart := False;
  HasAnyComponent := False;

  while Pos <= Length(AStr) do
  begin
    C := UpCase(AStr[Pos]);

    if C = 'T' then
    begin
      InTimePart := True;
      Inc(Pos);
      Continue;
    end;

    // Parse a number
    if not TryParseVariableDigits(AStr, Pos, Val, DigitCount) then Exit;

    // Check for fractional part
    FracVal := 0;
    FracDigits := 0;
    if (Pos <= Length(AStr)) and ((AStr[Pos] = '.') or (AStr[Pos] = ',')) then
    begin
      Inc(Pos);
      if not TryParseVariableDigits(AStr, Pos, FracVal, FracDigits) then Exit;
    end;

    if Pos > Length(AStr) then Exit;

    C := UpCase(AStr[Pos]);
    Inc(Pos);
    HasAnyComponent := True;

    if not InTimePart then
    begin
      case C of
        'Y': ADuration.Years := Val * Sign;
        'M': ADuration.Months := Val * Sign;
        'W': ADuration.Weeks := Val * Sign;
        'D': ADuration.Days := Val * Sign;
      else
        Exit;
      end;
    end
    else
    begin
      case C of
        'H': ADuration.Hours := Val * Sign;
        'M': ADuration.Minutes := Val * Sign;
        'S':
        begin
          ADuration.Seconds := Val * Sign;
          // Handle fractional seconds
          if FracDigits > 0 then
          begin
            while FracDigits < 9 do
            begin
              FracVal := FracVal * 10;
              Inc(FracDigits);
            end;
            while FracDigits > 9 do
            begin
              FracVal := FracVal div 10;
              Dec(FracDigits);
            end;
            ADuration.Milliseconds := (FracVal div 1000000) * Sign;
            ADuration.Microseconds := ((FracVal div 1000) mod 1000) * Sign;
            ADuration.Nanoseconds := (FracVal mod 1000) * Sign;
          end;
        end;
      else
        Exit;
      end;
    end;
  end;

  Result := HasAnyComponent;
end;

function CompareIntegers(const A, B: Integer): Integer;
begin
  if A < B then
    Result := -1
  else if A > B then
    Result := 1
  else
    Result := 0;
end;

function CompareDates(const AYear1, AMonth1, ADay1, AYear2, AMonth2, ADay2: Integer): Integer;
begin
  Result := CompareIntegers(AYear1, AYear2);
  if Result <> 0 then Exit;
  Result := CompareIntegers(AMonth1, AMonth2);
  if Result <> 0 then Exit;
  Result := CompareIntegers(ADay1, ADay2);
end;

function CompareTimes(const AHour1, AMinute1, ASecond1, AMs1, AUs1, ANs1,
  AHour2, AMinute2, ASecond2, AMs2, AUs2, ANs2: Integer): Integer;
begin
  Result := CompareIntegers(AHour1, AHour2);
  if Result <> 0 then Exit;
  Result := CompareIntegers(AMinute1, AMinute2);
  if Result <> 0 then Exit;
  Result := CompareIntegers(ASecond1, ASecond2);
  if Result <> 0 then Exit;
  Result := CompareIntegers(AMs1, AMs2);
  if Result <> 0 then Exit;
  Result := CompareIntegers(AUs1, AUs2);
  if Result <> 0 then Exit;
  Result := CompareIntegers(ANs1, ANs2);
end;

function FormatYearMonthString(const AYear, AMonth: Integer): string;
begin
  Result := PadISOYear(AYear) + '-' + PadTwo(AMonth);
end;

function FormatMonthDayString(const AMonth, ADay: Integer): string;
begin
  Result := PadTwo(AMonth) + '-' + PadTwo(ADay);
end;

function TryParseISOYearMonth(const AStr: string; out AYear, AMonth: Integer): Boolean;
var
  Pos, Sign: Integer;
  C: Char;
  S: string;
begin
  Result := False;
  AYear := 0;
  AMonth := 0;
  S := StripAnnotations(AStr);
  Pos := 1;
  Sign := 1;

  if Pos > Length(S) then Exit;

  C := S[Pos];
  if C = '+' then
  begin
    Inc(Pos);
    if not TryParseDigits(S, Pos, 6, AYear) then Exit;
  end
  else if C = '-' then
  begin
    Inc(Pos);
    Sign := -1;
    if not TryParseDigits(S, Pos, 6, AYear) then Exit;
    if AYear = 0 then Exit;
  end
  else
  begin
    if not TryParseDigits(S, Pos, 4, AYear) then Exit;
  end;

  AYear := AYear * Sign;

  if (Pos > Length(S)) or (S[Pos] <> '-') then Exit;
  Inc(Pos);

  if not TryParseDigits(S, Pos, 2, AMonth) then Exit;

  if Pos <= Length(S) then Exit;

  if (AMonth < 1) or (AMonth > 12) then Exit;

  Result := True;
end;

function TryParseISOMonthDay(const AStr: string; out AMonth, ADay: Integer): Boolean;
var
  Pos: Integer;
  S: string;
begin
  Result := False;
  AMonth := 0;
  ADay := 0;
  S := StripAnnotations(AStr);
  Pos := 1;

  // Skip optional leading '--' (ISO 8601 month-day format)
  if (Length(S) >= 2) and (S[1] = '-') and (S[2] = '-') then
    Pos := 3;

  if not TryParseDigits(S, Pos, 2, AMonth) then Exit;

  if (Pos > Length(S)) or (S[Pos] <> '-') then Exit;
  Inc(Pos);

  if not TryParseDigits(S, Pos, 2, ADay) then Exit;

  if Pos <= Length(S) then Exit;

  if (AMonth < 1) or (AMonth > 12) then Exit;
  // Validate day against month in a leap year (1972) to allow Feb 29
  if (ADay < 1) or (ADay > DaysInMonth(1972, AMonth)) then Exit;

  Result := True;
end;

function TryParseISODateTimeWithOffset(const AStr: string; out ADate: TTemporalDateRecord;
  out ATime: TTemporalTimeRecord; out AOffsetSeconds: Integer; out ATimeZone: string): Boolean;
var
  TPos, BracketStart, BracketEnd, I: Integer;
  DatePart, Rest, TimePart, OffsetPart, AnnotContent: string;
  OffsetSign, OffsetH, OffsetM, OffsetS: Integer;
  ParsePos: Integer;
  HasExplicitOffset, HasColon: Boolean;
begin
  Result := False;
  AOffsetSeconds := 0;
  ATimeZone := '';
  ATime.Hour := 0;
  ATime.Minute := 0;
  ATime.Second := 0;
  ATime.Millisecond := 0;
  ATime.Microsecond := 0;
  ATime.Nanosecond := 0;
  HasExplicitOffset := False;

  if HasFractionWithMoreThanNineDigits(AStr) then Exit;

  TPos := System.Pos('T', AStr);
  if TPos = 0 then
    TPos := System.Pos('t', AStr);
  if TPos = 0 then
    TPos := System.Pos(' ', AStr);
  if TPos = 0 then Exit;

  DatePart := Copy(AStr, 1, TPos - 1);
  Rest := Copy(AStr, TPos + 1, Length(AStr) - TPos);

  if not TryParseISODate(DatePart, ADate) then Exit;

  // Extract all [...] annotations from end, in reverse order
  // The first non-calendar annotation is the timezone; calendar annotations start with u-ca=
  while (Length(Rest) > 0) and (Rest[Length(Rest)] = ']') do
  begin
    BracketEnd := Length(Rest);
    BracketStart := BracketEnd - 1;
    while (BracketStart > 0) and (Rest[BracketStart] <> '[') do
      Dec(BracketStart);
    if BracketStart > 0 then
    begin
      AnnotContent := Copy(Rest, BracketStart + 1, BracketEnd - BracketStart - 1);
      // Strip critical flag prefix '!'
      if (Length(AnnotContent) > 0) and (AnnotContent[1] = '!') then
        AnnotContent := Copy(AnnotContent, 2, Length(AnnotContent) - 1);
      // Reject empty annotations
      if Length(AnnotContent) = 0 then
        Exit;
      // Calendar annotations start with 'u-ca='
      if (Length(AnnotContent) > 5) and (Copy(AnnotContent, 1, 5) = 'u-ca=') then
        // Calendar annotation - ignore (we only support iso8601)
      else if ATimeZone = '' then
      begin
        if TemporalOffsetIdentifierHasSubMinuteSyntax(AnnotContent) then
          Exit;
        ATimeZone := AnnotContent
      end
      else
        Exit; // Multiple timezone annotations - reject
      Rest := Copy(Rest, 1, BracketStart - 1);
    end
    else
      Break;
  end;

  // Find offset: look for Z, +, or - after time digits
  OffsetPart := '';
  if (Length(Rest) > 0) and ((Rest[Length(Rest)] = 'Z') or (Rest[Length(Rest)] = 'z')) then
  begin
    HasExplicitOffset := True;
    AOffsetSeconds := 0;
    Rest := Copy(Rest, 1, Length(Rest) - 1);
    if ATimeZone = '' then
      ATimeZone := 'UTC';
  end
  else
  begin
    // Look for +/- offset after time portion
    I := Length(Rest);
    while I > 0 do
    begin
      if (Rest[I] = '+') or (Rest[I] = '-') then
      begin
        OffsetPart := Copy(Rest, I, Length(Rest) - I + 1);
        Rest := Copy(Rest, 1, I - 1);
        Break;
      end;
      Dec(I);
    end;

    if OffsetPart <> '' then
    begin
      HasExplicitOffset := True;
      if OffsetPart[1] = '+' then OffsetSign := 1 else OffsetSign := -1;
      ParsePos := 2;
      if not TryParseDigits(OffsetPart, ParsePos, 2, OffsetH) then Exit;
      OffsetM := 0;
      OffsetS := 0;
      HasColon := False;
      if (ParsePos <= Length(OffsetPart)) and (OffsetPart[ParsePos] = ':') then
      begin
        HasColon := True;
        Inc(ParsePos);
        if not TryParseDigits(OffsetPart, ParsePos, 2, OffsetM) then Exit;
      end
      else if ParsePos + 1 <= Length(OffsetPart) then
      begin
        if not TryParseDigits(OffsetPart, ParsePos, 2, OffsetM) then Exit;
      end;
      if ParsePos <= Length(OffsetPart) then
      begin
        if HasColon then
        begin
          if OffsetPart[ParsePos] <> ':' then
            Exit;
          Inc(ParsePos);
        end;
        if not TryParseDigits(OffsetPart, ParsePos, 2, OffsetS) then Exit;
      end;
      if ParsePos <= Length(OffsetPart) then
        Exit;
      // Validate ranges
      if (OffsetH > 23) or (OffsetM > 59) or (OffsetS > 59) then Exit;
      AOffsetSeconds := OffsetSign * (OffsetH * 3600 + OffsetM * 60 + OffsetS);
      // Set timezone to offset if no annotation timezone
      if ATimeZone = '' then
        ATimeZone := OffsetPart;
    end;
  end;

  // Parse time portion from Rest
  TimePart := Rest;
  if Length(TimePart) = 0 then
    Exit;
  if not TryParseISOTime(TimePart, ATime) then Exit;

  Result := HasExplicitOffset;
end;

{ --------------------------------------------------------------------------- }
{ Unified Temporal string coercion functions                                   }
{ --------------------------------------------------------------------------- }

function CoerceToISODate(const AStr: string; out ADate: TTemporalDateRecord): Boolean;
var
  TimeRec: TTemporalTimeRecord;
  S: string;
begin
  if HasFractionWithMoreThanNineDigits(AStr) then
  begin
    Result := False;
    Exit;
  end;
  if TemporalStringHasUTCDesignator(AStr) then
  begin
    Result := False;
    Exit;
  end;
  // Validate annotations once up front — if invalid, reject immediately
  S := StripAnnotations(AStr);
  if (Length(AStr) > 0) and (AStr[Length(AStr)] = ']') and (S = AStr) then
  begin
    Result := False;
    Exit;
  end;
  // Try date-only
  if TryParseISODate(S, ADate) then
  begin
    Result := True;
    Exit;
  end;
  // Try full datetime
  if TryParseISODateTime(AStr, ADate, TimeRec) then
  begin
    Result := True;
    Exit;
  end;
  Result := False;
end;

function CoerceToISOTime(const AStr: string; out ATime: TTemporalTimeRecord): Boolean;
var
  S, DatePart: string;
  TPos: Integer;
  DateRec: TTemporalDateRecord;
begin
  if HasFractionWithMoreThanNineDigits(AStr) then
  begin
    Result := False;
    Exit;
  end;
  if TemporalStringHasUTCDesignator(AStr) or PlainTimeStringRequiresDesignator(AStr) then
  begin
    Result := False;
    Exit;
  end;
  // Strip offset and annotations
  S := StripOffsetAndAnnotationsForTime(AStr);
  // Try time-only
  if TryParseISOTime(S, ATime) then
  begin
    Result := True;
    Exit;
  end;
  // If string has T separator, extract and parse the time part only
  // (but do NOT accept date-only strings — no implicit midnight)
  TPos := System.Pos('T', S);
  if TPos = 0 then
    TPos := System.Pos('t', S);
  if TPos = 0 then
    TPos := System.Pos(' ', S);
  if TPos > 0 then
  begin
    if TPos > 1 then
    begin
      DatePart := Copy(S, 1, TPos - 1);
      if not TryParseISODate(DatePart, DateRec) then
      begin
        Result := False;
        Exit;
      end;
    end;
    S := Copy(S, TPos + 1, Length(S) - TPos);
    if (Length(S) > 0) and TryParseISOTime(S, ATime) then
    begin
      Result := True;
      Exit;
    end;
  end;
  Result := False;
end;

function CoerceToISODateTime(const AStr: string; out ADate: TTemporalDateRecord; out ATime: TTemporalTimeRecord): Boolean;
var
  S: string;
begin
  if HasFractionWithMoreThanNineDigits(AStr) then
  begin
    Result := False;
    Exit;
  end;
  if TemporalStringHasUTCDesignator(AStr) then
  begin
    Result := False;
    Exit;
  end;
  // Validate annotations up front
  S := StripAnnotations(AStr);
  if (Length(AStr) > 0) and (AStr[Length(AStr)] = ']') and (S = AStr) then
  begin
    Result := False;
    Exit;
  end;
  if TryParseISODateTime(AStr, ADate, ATime) then
  begin
    Result := True;
    Exit;
  end;
  Result := False;
end;

function CoerceToISOInstant(const AStr: string; out ADate: TTemporalDateRecord;
  out ATime: TTemporalTimeRecord; out AOffsetSeconds: Integer): Boolean;
var
  TimeZone, S: string;
begin
  if HasFractionWithMoreThanNineDigits(AStr) then
  begin
    Result := False;
    Exit;
  end;
  // Validate annotations up front
  S := StripAnnotations(AStr);
  if (Length(AStr) > 0) and (AStr[Length(AStr)] = ']') and (S = AStr) then
  begin
    Result := False;
    Exit;
  end;
  // Instant requires an explicit UTC offset (Z or ±HH:MM) — no fallback
  Result := TryParseISODateTimeWithOffset(AStr, ADate, ATime, AOffsetSeconds, TimeZone);
end;

function CoerceToISOYearMonth(const AStr: string; out AYear, AMonth: Integer): Boolean;
var
  DateRec: TTemporalDateRecord;
  TimeRec: TTemporalTimeRecord;
begin
  // Try short form YYYY-MM (with annotation stripping)
  if TryParseISOYearMonth(AStr, AYear, AMonth) then
  begin
    Result := True;
    Exit;
  end;
  // Try full date or datetime, extract year+month
  if TryParseISODate(AStr, DateRec) or TryParseISODateTime(AStr, DateRec, TimeRec) then
  begin
    AYear := DateRec.Year;
    AMonth := DateRec.Month;
    Result := True;
    Exit;
  end;
  Result := False;
end;

function CoerceToISOMonthDay(const AStr: string; out AMonth, ADay: Integer): Boolean;
var
  DateRec: TTemporalDateRecord;
  TimeRec: TTemporalTimeRecord;
begin
  // Try short form MM-DD or --MM-DD (with annotation stripping)
  if TryParseISOMonthDay(AStr, AMonth, ADay) then
  begin
    Result := True;
    Exit;
  end;
  // Try full date or datetime, extract month+day
  if TryParseISODate(AStr, DateRec) or TryParseISODateTime(AStr, DateRec, TimeRec) then
  begin
    AMonth := DateRec.Month;
    ADay := DateRec.Day;
    Result := True;
    Exit;
  end;
  Result := False;
end;

end.
