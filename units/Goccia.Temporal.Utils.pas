unit Goccia.Temporal.Utils;

{$I Goccia.inc}

interface

uses
  SysUtils, Math;

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

function IsLeapYear(AYear: Integer): Boolean;
function DaysInMonth(AYear, AMonth: Integer): Integer;
function DaysInYear(AYear: Integer): Integer;
function IsValidDate(AYear, AMonth, ADay: Integer): Boolean;
function IsValidTime(AHour, AMinute, ASecond, AMillisecond, AMicrosecond, ANanosecond: Integer): Boolean;

function DayOfYear(AYear, AMonth, ADay: Integer): Integer;
function DayOfWeek(AYear, AMonth, ADay: Integer): Integer;
function WeekOfYear(AYear, AMonth, ADay: Integer): Integer;
function YearOfWeek(AYear, AMonth, ADay: Integer): Integer;

function AddDaysToDate(AYear, AMonth, ADay: Integer; ADays: Int64): TTemporalDateRecord;
function DateToEpochDays(AYear, AMonth, ADay: Integer): Int64;
function EpochDaysToDate(ADays: Int64): TTemporalDateRecord;

function BalanceTime(AHour, AMinute, ASecond, AMillisecond, AMicrosecond, ANanosecond: Int64;
  out ExtraDays: Int64): TTemporalTimeRecord;

function PadISOYear(AYear: Integer): string;
function PadTwo(AValue: Integer): string;
function FormatTimeString(AHour, AMinute, ASecond, AMillisecond, AMicrosecond, ANanosecond: Integer): string;
function FormatDateString(AYear, AMonth, ADay: Integer): string;

function TryParseISODate(const AStr: string; out ADate: TTemporalDateRecord): Boolean;
function TryParseISOTime(const AStr: string; out ATime: TTemporalTimeRecord): Boolean;
function TryParseISODateTime(const AStr: string; out ADate: TTemporalDateRecord; out ATime: TTemporalTimeRecord): Boolean;
function TryParseISODuration(const AStr: string; out ADuration: TTemporalDurationRecord): Boolean;

function CompareIntegers(A, B: Integer): Integer; inline;
function CompareDates(AYear1, AMonth1, ADay1, AYear2, AMonth2, ADay2: Integer): Integer;
function CompareTimes(AHour1, AMinute1, ASecond1, AMs1, AUs1, ANs1,
  AHour2, AMinute2, ASecond2, AMs2, AUs2, ANs2: Integer): Integer;

implementation

function IsLeapYear(AYear: Integer): Boolean;
begin
  Result := ((AYear mod 4) = 0) and (((AYear mod 100) <> 0) or ((AYear mod 400) = 0));
end;

function DaysInMonth(AYear, AMonth: Integer): Integer;
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

function DaysInYear(AYear: Integer): Integer;
begin
  if IsLeapYear(AYear) then
    Result := 366
  else
    Result := 365;
end;

function IsValidDate(AYear, AMonth, ADay: Integer): Boolean;
begin
  Result := (AMonth >= 1) and (AMonth <= 12) and
            (ADay >= 1) and (ADay <= DaysInMonth(AYear, AMonth));
end;

function IsValidTime(AHour, AMinute, ASecond, AMillisecond, AMicrosecond, ANanosecond: Integer): Boolean;
begin
  Result := (AHour >= 0) and (AHour <= 23) and
            (AMinute >= 0) and (AMinute <= 59) and
            (ASecond >= 0) and (ASecond <= 59) and
            (AMillisecond >= 0) and (AMillisecond <= 999) and
            (AMicrosecond >= 0) and (AMicrosecond <= 999) and
            (ANanosecond >= 0) and (ANanosecond <= 999);
end;

function DayOfYear(AYear, AMonth, ADay: Integer): Integer;
var
  M: Integer;
begin
  Result := 0;
  for M := 1 to AMonth - 1 do
    Result := Result + DaysInMonth(AYear, M);
  Result := Result + ADay;
end;

function DayOfWeek(AYear, AMonth, ADay: Integer): Integer;
var
  EpochDays: Int64;
  D: Integer;
begin
  // Use epoch days: 1970-01-01 was Thursday (ISO day 4)
  EpochDays := DateToEpochDays(AYear, AMonth, ADay);
  D := Integer(((EpochDays mod 7) + 7 + 3) mod 7) + 1;
  Result := D;
end;

function WeekOfYear(AYear, AMonth, ADay: Integer): Integer;
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

function YearOfWeek(AYear, AMonth, ADay: Integer): Integer;
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

function DateToEpochDays(AYear, AMonth, ADay: Integer): Int64;
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

function EpochDaysToDate(ADays: Int64): TTemporalDateRecord;
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

function AddDaysToDate(AYear, AMonth, ADay: Integer; ADays: Int64): TTemporalDateRecord;
var
  EpochDays: Int64;
begin
  EpochDays := DateToEpochDays(AYear, AMonth, ADay);
  Result := EpochDaysToDate(EpochDays + ADays);
end;

function BalanceTime(AHour, AMinute, ASecond, AMillisecond, AMicrosecond, ANanosecond: Int64;
  out ExtraDays: Int64): TTemporalTimeRecord;
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
    ExtraDays := TotalNs div Int64(86400000000000);
    Remainder := TotalNs mod Int64(86400000000000);
  end
  else
  begin
    ExtraDays := -(-TotalNs + Int64(86400000000000) - 1) div Int64(86400000000000);
    Remainder := TotalNs - ExtraDays * Int64(86400000000000);
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

function PadISOYear(AYear: Integer): string;
begin
  if (AYear >= 0) and (AYear <= 9999) then
    Result := Format('%.4d', [AYear])
  else if AYear > 9999 then
    Result := '+' + Format('%.6d', [AYear])
  else
    Result := '-' + Format('%.6d', [-AYear]);
end;

function PadTwo(AValue: Integer): string;
begin
  Result := Format('%.2d', [AValue]);
end;

function FormatDateString(AYear, AMonth, ADay: Integer): string;
begin
  Result := PadISOYear(AYear) + '-' + PadTwo(AMonth) + '-' + PadTwo(ADay);
end;

function FormatTimeString(AHour, AMinute, ASecond, AMillisecond, AMicrosecond, ANanosecond: Integer): string;
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

function TryParseDigits(const AStr: string; var APos: Integer; ACount: Integer; out AValue: Integer): Boolean;
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

function TryParseISODate(const AStr: string; out ADate: TTemporalDateRecord): Boolean;
var
  Pos, Sign: Integer;
  Year, Month, Day: Integer;
  C: Char;
begin
  Result := False;
  Pos := 1;
  Sign := 1;

  // Check for sign prefix
  if Pos <= Length(AStr) then
  begin
    C := AStr[Pos];
    if C = '+' then
    begin
      Inc(Pos);
      if not TryParseDigits(AStr, Pos, 6, Year) then Exit;
    end
    else if C = '-' then
    begin
      Inc(Pos);
      Sign := -1;
      if not TryParseDigits(AStr, Pos, 6, Year) then Exit;
    end
    else
    begin
      if not TryParseDigits(AStr, Pos, 4, Year) then Exit;
    end;
  end
  else
    Exit;

  Year := Year * Sign;

  // Expect '-'
  if (Pos > Length(AStr)) or (AStr[Pos] <> '-') then Exit;
  Inc(Pos);

  if not TryParseDigits(AStr, Pos, 2, Month) then Exit;

  // Expect '-'
  if (Pos > Length(AStr)) or (AStr[Pos] <> '-') then Exit;
  Inc(Pos);

  if not TryParseDigits(AStr, Pos, 2, Day) then Exit;

  // Must be at end of string (for pure date parsing)
  if Pos <= Length(AStr) then Exit;

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

  if not IsValidTime(Hour, Minute, Second, ATime.Millisecond, ATime.Microsecond, ATime.Nanosecond) then Exit;

  ATime.Hour := Hour;
  ATime.Minute := Minute;
  ATime.Second := Second;
  Result := Pos > Length(AStr);
end;

function TryParseISODateTime(const AStr: string; out ADate: TTemporalDateRecord; out ATime: TTemporalTimeRecord): Boolean;
var
  TPos: Integer;
  DatePart, TimePart: string;
begin
  Result := False;
  ATime.Hour := 0;
  ATime.Minute := 0;
  ATime.Second := 0;
  ATime.Millisecond := 0;
  ATime.Microsecond := 0;
  ATime.Nanosecond := 0;

  TPos := System.Pos('T', AStr);
  if TPos = 0 then
    TPos := System.Pos('t', AStr);

  if TPos = 0 then
  begin
    // Try as date only
    Result := TryParseISODate(AStr, ADate);
    Exit;
  end;

  DatePart := Copy(AStr, 1, TPos - 1);
  TimePart := Copy(AStr, TPos + 1, Length(AStr) - TPos);

  // Strip trailing timezone info (Z or +/-HH:MM) from time part
  if (Length(TimePart) > 0) and (TimePart[Length(TimePart)] = 'Z') then
    TimePart := Copy(TimePart, 1, Length(TimePart) - 1);

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

function CompareIntegers(A, B: Integer): Integer;
begin
  if A < B then
    Result := -1
  else if A > B then
    Result := 1
  else
    Result := 0;
end;

function CompareDates(AYear1, AMonth1, ADay1, AYear2, AMonth2, ADay2: Integer): Integer;
begin
  Result := CompareIntegers(AYear1, AYear2);
  if Result <> 0 then Exit;
  Result := CompareIntegers(AMonth1, AMonth2);
  if Result <> 0 then Exit;
  Result := CompareIntegers(ADay1, ADay2);
end;

function CompareTimes(AHour1, AMinute1, ASecond1, AMs1, AUs1, ANs1,
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

end.
