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

function CompareIntegers(const A, B: Integer): Integer; inline;
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
  EpochDays := Int64(365) * Y + (Y div 4) - (Y div 100) + (Y div 400);
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
begin
  Result := PadTwo(AHour) + ':' + PadTwo(AMinute) + ':' + PadTwo(ASecond);

  if ANanosecond <> 0 then
    Result := Result + '.' + Format('%.3d%.3d%.3d', [AMillisecond, AMicrosecond, ANanosecond])
  else if AMicrosecond <> 0 then
    Result := Result + '.' + Format('%.3d%.3d', [AMillisecond, AMicrosecond])
  else if AMillisecond <> 0 then
    Result := Result + '.' + Format('%.3d', [AMillisecond])
  // else no fractional seconds
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

function IsValidAnnotationKey(const AKey: string): Boolean;
var
  I: Integer;
  C: Char;
  InSegment: Boolean;
begin
  Result := False;
  if Length(AKey) = 0 then Exit;
  // First char must be lowercase letter
  if (AKey[1] < 'a') or (AKey[1] > 'z') then Exit;
  InSegment := True;
  for I := 2 to Length(AKey) do
  begin
    C := AKey[I];
    if C = '-' then
    begin
      if not InSegment then Exit; // double dash
      InSegment := False;
    end
    else if ((C >= 'a') and (C <= 'z')) or ((C >= '0') and (C <= '9')) then
      InSegment := True
    else
      Exit; // invalid char
  end;
  if not InSegment then Exit; // trailing dash
  Result := True;
end;

function StripAnnotations(const AStr: string): string;
var
  I: Integer;
  AnnotContent: string;
  IsCritical: Boolean;
  KeyEnd: Integer;
  Key: string;
  CalendarSeen, TimeZoneSeen: Boolean;
begin
  Result := AStr;
  CalendarSeen := False;
  TimeZoneSeen := False;

  // Remove all trailing [...] annotations, validating them
  while (Length(Result) > 0) and (Result[Length(Result)] = ']') do
  begin
    I := Length(Result) - 1;
    while (I > 0) and (Result[I] <> '[') do
      Dec(I);
    if I > 0 then
    begin
      AnnotContent := Copy(Result, I + 1, Length(Result) - I - 1);
      IsCritical := (Length(AnnotContent) > 0) and (AnnotContent[1] = '!');
      if IsCritical then
        AnnotContent := Copy(AnnotContent, 2, Length(AnnotContent) - 1);

      // Check for key=value format
      KeyEnd := System.Pos('=', AnnotContent);
      if KeyEnd > 0 then
      begin
        Key := Copy(AnnotContent, 1, KeyEnd - 1);
        // Validate key format (must be lowercase alphanumeric with dashes)
        if not IsValidAnnotationKey(Key) then
          Exit; // Invalid key - return unchanged so parser rejects

        if Key = 'u-ca' then
        begin
          // Calendar annotation
          if CalendarSeen then
            Exit; // Multiple calendar annotations - reject
          CalendarSeen := True;
        end
        else if IsCritical then
          Exit; // Unknown critical annotation - reject
        // Non-critical unknown key=value - strip silently
      end
      else
      begin
        // No key=value format: this is a timezone annotation
        if TimeZoneSeen then
          Exit; // Multiple timezone annotations - reject
        TimeZoneSeen := True;
      end;

      Result := Copy(Result, 1, I - 1);
    end
    else
      Break;
  end;
end;

function StripOffsetAndAnnotations(const AStr: string): string;
var
  I: Integer;
begin
  Result := StripAnnotations(AStr);
  // Remove trailing UTC offset (+HH:MM, -HH:MM, +HHMM, -HHMM, +HH, -HH, Z)
  if (Length(Result) > 0) and ((Result[Length(Result)] = 'Z') or (Result[Length(Result)] = 'z')) then
  begin
    Result := Copy(Result, 1, Length(Result) - 1);
    Exit;
  end;
  // Look for +/- offset at end
  I := Length(Result);
  while (I > 0) and (Result[I] >= '0') and (Result[I] <= '9') do
    Dec(I);
  if (I > 0) and (Result[I] = ':') then
    Dec(I);
  while (I > 0) and (Result[I] >= '0') and (Result[I] <= '9') do
    Dec(I);
  if (I > 0) and ((Result[I] = '+') or (Result[I] = '-')) then
    Result := Copy(Result, 1, I - 1);
end;

function TryParseISODate(const AStr: string; out ADate: TTemporalDateRecord): Boolean;
var
  Pos, Sign: Integer;
  Year, Month, Day: Integer;
  C: Char;
  S: string;
begin
  Result := False;
  S := StripAnnotations(AStr);
  Pos := 1;
  Sign := 1;

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
    end
    else
    begin
      if not TryParseDigits(S, Pos, 4, Year) then Exit;
    end;
  end
  else
    Exit;

  Year := Year * Sign;

  // Expect '-'
  if (Pos > Length(S)) or (S[Pos] <> '-') then Exit;
  Inc(Pos);

  if not TryParseDigits(S, Pos, 2, Month) then Exit;

  // Expect '-'
  if (Pos > Length(S)) or (S[Pos] <> '-') then Exit;
  Inc(Pos);

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
  if (Pos > Length(AStr)) or (AStr[Pos] <> ':') then Exit;
  Inc(Pos);
  if not TryParseDigits(AStr, Pos, 2, Minute) then Exit;

  Second := 0;
  if (Pos <= Length(AStr)) and (AStr[Pos] = ':') then
  begin
    Inc(Pos);
    if not TryParseDigits(AStr, Pos, 2, Second) then Exit;

    // Fractional seconds
    if (Pos <= Length(AStr)) and (AStr[Pos] = '.') then
    begin
      Inc(Pos);
      if not TryParseVariableDigits(AStr, Pos, FracVal, FracDigits) then Exit;

      // Pad or truncate to 9 digits (nanosecond precision)
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

      ATime.Millisecond := Integer(FracVal div 1000000);
      ATime.Microsecond := Integer((FracVal div 1000) mod 1000);
      ATime.Nanosecond := Integer(FracVal mod 1000);
    end;
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

  S := AStr;

  TPos := System.Pos('T', S);
  if TPos = 0 then
    TPos := System.Pos('t', S);

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

  if not TryParseISODate(DatePart, ADate) then Exit;
  if Length(TimePart) > 0 then
  begin
    if not TryParseISOTime(TimePart, ATime) then Exit;
  end;

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
  OffsetSign, OffsetH, OffsetM: Integer;
  ParsePos: Integer;
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

  TPos := System.Pos('T', AStr);
  if TPos = 0 then
    TPos := System.Pos('t', AStr);
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
      // Calendar annotations start with 'u-ca='
      if (Length(AnnotContent) > 5) and (Copy(AnnotContent, 1, 5) = 'u-ca=') then
        // Calendar annotation - ignore (we only support iso8601)
      else if ATimeZone = '' then
        ATimeZone := AnnotContent
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
      if OffsetPart[1] = '+' then OffsetSign := 1 else OffsetSign := -1;
      ParsePos := 2;
      if not TryParseDigits(OffsetPart, ParsePos, 2, OffsetH) then Exit;
      OffsetM := 0;
      if (ParsePos <= Length(OffsetPart)) and (OffsetPart[ParsePos] = ':') then
      begin
        Inc(ParsePos);
        if not TryParseDigits(OffsetPart, ParsePos, 2, OffsetM) then Exit;
      end
      else if ParsePos + 1 <= Length(OffsetPart) then
      begin
        if not TryParseDigits(OffsetPart, ParsePos, 2, OffsetM) then Exit;
      end;
      // Validate ranges
      if (OffsetH > 23) or (OffsetM > 59) then Exit;
      AOffsetSeconds := OffsetSign * (OffsetH * 3600 + OffsetM * 60);
      // Set timezone to offset if no annotation timezone
      if ATimeZone = '' then
        ATimeZone := OffsetPart;
    end;
  end;

  // Parse time portion from Rest
  TimePart := Rest;
  if Length(TimePart) > 0 then
  begin
    if not TryParseISOTime(TimePart, ATime) then Exit;
  end;

  Result := True;
end;

{ --------------------------------------------------------------------------- }
{ Unified Temporal string coercion functions                                   }
{ --------------------------------------------------------------------------- }

function CoerceToISODate(const AStr: string; out ADate: TTemporalDateRecord): Boolean;
var
  TimeRec: TTemporalTimeRecord;
  S: string;
begin
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
  S: string;
  TPos: Integer;
begin
  // Strip offset and annotations
  S := StripOffsetAndAnnotations(AStr);
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
  if TPos > 0 then
  begin
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
