unit Goccia.Temporal.Options;

{$I Goccia.inc}

interface

uses
  BigInteger,

  Goccia.Arguments.Collection,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TTemporalRoundingMode = (
    rmHalfExpand,
    rmCeil,
    rmFloor,
    rmTrunc,
    rmHalfCeil,
    rmHalfFloor,
    rmHalfTrunc,
    rmHalfEven,
    rmExpand
  );

  TTemporalOverflow = (toConstrain, toReject);

  TTemporalUnit = (
    tuYear,
    tuMonth,
    tuWeek,
    tuDay,
    tuHour,
    tuMinute,
    tuSecond,
    tuMillisecond,
    tuMicrosecond,
    tuNanosecond,
    tuAuto,
    tuNone
  );

const
  NANOSECONDS_PER_MICROSECOND = 1000;
  NANOSECONDS_PER_MILLISECOND = 1000000;
  NANOSECONDS_PER_SECOND = Int64(1000000000);
  NANOSECONDS_PER_MINUTE = Int64(60000000000);
  NANOSECONDS_PER_HOUR = Int64(3600000000000);
  NANOSECONDS_PER_DAY = Int64(86400000000000);

function GetTemporalUnitFromString(const AStr: string; out AUnit: TTemporalUnit): Boolean;
function GetRoundingModeFromString(const AStr: string; out AMode: TTemporalRoundingMode): Boolean;
function GetOptionString(const AOptions: TGocciaObjectValue; const AName: string;
  const ADefault: string): string;
function GetOptionInteger(const AOptions: TGocciaObjectValue; const AName: string;
  const ADefault: Integer): Integer;
function GetSmallestUnit(const AOptions: TGocciaObjectValue; const ADefault: TTemporalUnit): TTemporalUnit;
function GetLargestUnit(const AOptions: TGocciaObjectValue; const ADefault: TTemporalUnit): TTemporalUnit;
function GetRoundingMode(const AOptions: TGocciaObjectValue; const ADefault: TTemporalRoundingMode): TTemporalRoundingMode;
function GetRoundingIncrement(const AOptions: TGocciaObjectValue; const ADefault: Integer): Integer;
function GetOverflowOption(const AOptions: TGocciaObjectValue): TTemporalOverflow;
function GetFractionalSecondDigits(const AOptions: TGocciaObjectValue): Integer;
function UnitToNanoseconds(const AUnit: TTemporalUnit): Int64;
function RoundWithMode(const AValue: Int64; const ADivisor: Int64; const AMode: TTemporalRoundingMode): Int64;
function RoundBigIntWithMode(const AValue: TBigInteger; const ADivisor: TBigInteger; const AMode: TTemporalRoundingMode): TBigInteger;
function FormatTimeWithPrecision(const AHour, AMinute, ASecond, AMillisecond,
  AMicrosecond, ANanosecond: Integer; const AFractionalDigits: Integer): string;

type
  TTemporalCalendarDisplay = (tcdAuto, tcdAlways, tcdNever, tcdCritical);

{ Shared toString options — call from any Temporal toString method }
procedure ResolveTemporalToStringOptions(
  const AOptions: TGocciaObjectValue;
  out AFractionalDigits: Integer;
  out ARoundingMode: TTemporalRoundingMode);
function GetCalendarDisplay(const AOptions: TGocciaObjectValue): TTemporalCalendarDisplay;
procedure RoundTimeForToString(
  var AHour, AMinute, ASecond, AMs, AUs, ANs: Integer;
  var AExtraDays: Integer;
  const AFractionalDigits: Integer;
  const ARoundingMode: TTemporalRoundingMode);
function FormatCalendarAnnotation(const ACalendarDisplay: TTemporalCalendarDisplay): string;

{ Calendar-aware date differencing — implements CalendarDateUntil from TC39.
  Computes the difference (Y2,M2,D2) - (Y1,M1,D1) broken into years, months,
  weeks, and days based on ALargestUnit (tuYear, tuMonth, tuWeek, or tuDay). }
procedure CalendarDateUntil(
  const AY1, AM1, AD1, AY2, AM2, AD2: Integer;
  const ALargestUnit: TTemporalUnit;
  out AYears, AMonths, AWeeks, ADays: Int64);

{ Round a diff duration produced by until/since.  Implements the TC39
  RoundRelativeDuration algorithm for calendar-unit rounding (year/month)
  and simple nanosecond rounding for sub-month units.
  AStartY/M/D is the "start" date of the diff (needed for year/month walking).
  For types without a date component pass 0,0,0 and ensure smallestUnit is
  sub-day. }
procedure RoundDiffDuration(
  var AYears, AMonths, AWeeks, ADays,
      AHours, AMinutes, ASeconds,
      AMilliseconds, AMicroseconds, ANanoseconds: Int64;
  const AStartY, AStartM, AStartD: Integer;
  const ALargestUnit, ASmallestUnit: TTemporalUnit;
  const AMode: TTemporalRoundingMode;
  const AIncrement: Integer);

{ Extract diff options from an arguments collection at a given index.
  Returns nil if the argument is undefined/missing. }
function GetDiffOptions(const AArgs: TGocciaArgumentsCollection;
  const AIndex: Integer): TGocciaObjectValue;

implementation

uses
  SysUtils,

  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.Temporal.Utils,
  Goccia.Values.ErrorHelper;

function GetTemporalUnitFromString(const AStr: string; out AUnit: TTemporalUnit): Boolean;
begin
  Result := True;
  if (AStr = 'year') or (AStr = 'years') then AUnit := tuYear
  else if (AStr = 'month') or (AStr = 'months') then AUnit := tuMonth
  else if (AStr = 'week') or (AStr = 'weeks') then AUnit := tuWeek
  else if (AStr = 'day') or (AStr = 'days') then AUnit := tuDay
  else if (AStr = 'hour') or (AStr = 'hours') then AUnit := tuHour
  else if (AStr = 'minute') or (AStr = 'minutes') then AUnit := tuMinute
  else if (AStr = 'second') or (AStr = 'seconds') then AUnit := tuSecond
  else if (AStr = 'millisecond') or (AStr = 'milliseconds') then AUnit := tuMillisecond
  else if (AStr = 'microsecond') or (AStr = 'microseconds') then AUnit := tuMicrosecond
  else if (AStr = 'nanosecond') or (AStr = 'nanoseconds') then AUnit := tuNanosecond
  else if AStr = 'auto' then AUnit := tuAuto
  else Result := False;
end;

function GetRoundingModeFromString(const AStr: string; out AMode: TTemporalRoundingMode): Boolean;
begin
  Result := True;
  if AStr = 'halfExpand' then AMode := rmHalfExpand
  else if AStr = 'ceil' then AMode := rmCeil
  else if AStr = 'floor' then AMode := rmFloor
  else if AStr = 'trunc' then AMode := rmTrunc
  else if AStr = 'halfCeil' then AMode := rmHalfCeil
  else if AStr = 'halfFloor' then AMode := rmHalfFloor
  else if AStr = 'halfTrunc' then AMode := rmHalfTrunc
  else if AStr = 'halfEven' then AMode := rmHalfEven
  else if AStr = 'expand' then AMode := rmExpand
  else Result := False;
end;

function GetOptionString(const AOptions: TGocciaObjectValue; const AName: string;
  const ADefault: string): string;
var
  V: TGocciaValue;
begin
  if AOptions = nil then
  begin
    Result := ADefault;
    Exit;
  end;
  V := AOptions.GetProperty(AName);
  if (V = nil) or (V is TGocciaUndefinedLiteralValue) then
    Result := ADefault
  else
    Result := V.ToStringLiteral.Value;
end;

function GetOptionInteger(const AOptions: TGocciaObjectValue; const AName: string;
  const ADefault: Integer): Integer;
var
  V: TGocciaValue;
begin
  if AOptions = nil then
  begin
    Result := ADefault;
    Exit;
  end;
  V := AOptions.GetProperty(AName);
  if (V = nil) or (V is TGocciaUndefinedLiteralValue) then
    Result := ADefault
  else
    Result := Trunc(V.ToNumberLiteral.Value);
end;

function GetSmallestUnit(const AOptions: TGocciaObjectValue; const ADefault: TTemporalUnit): TTemporalUnit;
var
  S: string;
begin
  S := GetOptionString(AOptions, 'smallestUnit', '');
  if S = '' then
  begin
    Result := ADefault;
    Exit;
  end;
  if not GetTemporalUnitFromString(S, Result) then
    ThrowRangeError(Format(SErrorInvalidSmallestUnit, [S]), SSuggestTemporalValidUnits);
end;

function GetLargestUnit(const AOptions: TGocciaObjectValue; const ADefault: TTemporalUnit): TTemporalUnit;
var
  S: string;
begin
  S := GetOptionString(AOptions, 'largestUnit', '');
  if S = '' then
  begin
    Result := ADefault;
    Exit;
  end;
  if not GetTemporalUnitFromString(S, Result) then
    ThrowRangeError(Format(SErrorInvalidLargestUnit, [S]), SSuggestTemporalValidUnits);
end;

function GetRoundingMode(const AOptions: TGocciaObjectValue; const ADefault: TTemporalRoundingMode): TTemporalRoundingMode;
var
  S: string;
begin
  S := GetOptionString(AOptions, 'roundingMode', '');
  if S = '' then
  begin
    Result := ADefault;
    Exit;
  end;
  if not GetRoundingModeFromString(S, Result) then
    ThrowRangeError(Format(SErrorInvalidRoundingMode, [S]), SSuggestTemporalRoundingMode);
end;

function GetRoundingIncrement(const AOptions: TGocciaObjectValue; const ADefault: Integer): Integer;
begin
  Result := GetOptionInteger(AOptions, 'roundingIncrement', ADefault);
  if Result < 1 then
    ThrowRangeError(SErrorRoundingIncrementMin, SSuggestTemporalRoundArg);
end;

function GetOverflowOption(const AOptions: TGocciaObjectValue): TTemporalOverflow;
var
  S: string;
begin
  S := GetOptionString(AOptions, 'overflow', 'constrain');
  if S = 'constrain' then
    Result := toConstrain
  else if S = 'reject' then
    Result := toReject
  else
    ThrowRangeError(Format(SErrorInvalidOverflow, [S]), SSuggestTemporalOverflow);
end;

function GetFractionalSecondDigits(const AOptions: TGocciaObjectValue): Integer;
var
  V: TGocciaValue;
  S: string;
begin
  if AOptions = nil then
  begin
    Result := -1; // auto
    Exit;
  end;
  V := AOptions.GetProperty('fractionalSecondDigits');
  if (V = nil) or (V is TGocciaUndefinedLiteralValue) then
  begin
    Result := -1; // auto
    Exit;
  end;
  if V is TGocciaStringLiteralValue then
  begin
    S := TGocciaStringLiteralValue(V).Value;
    if S = 'auto' then
      Result := -1
    else
      ThrowRangeError(Format(SErrorInvalidFractionalDigits, [S]), SSuggestTemporalRoundArg);
  end
  else
  begin
    Result := Trunc(V.ToNumberLiteral.Value);
    if (Result < 0) or (Result > 9) then
      ThrowRangeError(SErrorFractionalDigitsRange, SSuggestTemporalRoundArg);
  end;
end;

function UnitToNanoseconds(const AUnit: TTemporalUnit): Int64;
begin
  case AUnit of
    tuHour: Result := NANOSECONDS_PER_HOUR;
    tuMinute: Result := NANOSECONDS_PER_MINUTE;
    tuSecond: Result := NANOSECONDS_PER_SECOND;
    tuMillisecond: Result := NANOSECONDS_PER_MILLISECOND;
    tuMicrosecond: Result := NANOSECONDS_PER_MICROSECOND;
    tuNanosecond: Result := 1;
    tuDay: Result := NANOSECONDS_PER_DAY;
  else
    ThrowRangeError(SErrorCannotConvertCalendarUnit, SSuggestTemporalValidUnits);
    Result := 1;
  end;
end;

function RoundWithMode(const AValue: Int64; const ADivisor: Int64; const AMode: TTemporalRoundingMode): Int64;
var
  Quotient, Remainder: Int64;
  AbsRemainder: Int64;
begin
  if ADivisor = 1 then
  begin
    Result := AValue;
    Exit;
  end;

  Quotient := AValue div ADivisor;
  Remainder := AValue mod ADivisor;

  if Remainder = 0 then
  begin
    Result := Quotient * ADivisor;
    Exit;
  end;

  AbsRemainder := Abs(Remainder);

  case AMode of
    rmTrunc:
      Result := Quotient * ADivisor;
    rmFloor:
    begin
      if AValue < 0 then
        Result := (Quotient - 1) * ADivisor
      else
        Result := Quotient * ADivisor;
    end;
    rmCeil:
    begin
      if AValue > 0 then
        Result := (Quotient + 1) * ADivisor
      else
        Result := Quotient * ADivisor;
    end;
    rmHalfExpand:
    begin
      if AbsRemainder * 2 >= ADivisor then
      begin
        if AValue > 0 then
          Result := (Quotient + 1) * ADivisor
        else
          Result := (Quotient - 1) * ADivisor;
      end
      else
        Result := Quotient * ADivisor;
    end;
    rmHalfTrunc:
    begin
      if AbsRemainder * 2 > ADivisor then
      begin
        if AValue > 0 then
          Result := (Quotient + 1) * ADivisor
        else
          Result := (Quotient - 1) * ADivisor;
      end
      else
        Result := Quotient * ADivisor;
    end;
    rmHalfCeil:
    begin
      if AbsRemainder * 2 >= ADivisor then
      begin
        if AValue > 0 then
          Result := (Quotient + 1) * ADivisor
        else if AbsRemainder * 2 > ADivisor then
          Result := (Quotient - 1) * ADivisor
        else
          Result := Quotient * ADivisor;
      end
      else
        Result := Quotient * ADivisor;
    end;
    rmHalfFloor:
    begin
      if AbsRemainder * 2 >= ADivisor then
      begin
        if AValue < 0 then
          Result := (Quotient - 1) * ADivisor
        else if AbsRemainder * 2 > ADivisor then
          Result := (Quotient + 1) * ADivisor
        else
          Result := Quotient * ADivisor;
      end
      else
        Result := Quotient * ADivisor;
    end;
    rmHalfEven:
    begin
      if AbsRemainder * 2 > ADivisor then
      begin
        if AValue > 0 then
          Result := (Quotient + 1) * ADivisor
        else
          Result := (Quotient - 1) * ADivisor;
      end
      else if AbsRemainder * 2 = ADivisor then
      begin
        // Round to even
        if Odd(Quotient) then
        begin
          if AValue > 0 then
            Result := (Quotient + 1) * ADivisor
          else
            Result := (Quotient - 1) * ADivisor;
        end
        else
          Result := Quotient * ADivisor;
      end
      else
        Result := Quotient * ADivisor;
    end;
    rmExpand:
    begin
      // Always round away from zero
      if AValue > 0 then
        Result := (Quotient + 1) * ADivisor
      else
        Result := (Quotient - 1) * ADivisor;
    end;
  else
    Result := Quotient * ADivisor;
  end;
end;

function RoundBigIntWithMode(const AValue: TBigInteger; const ADivisor: TBigInteger;
  const AMode: TTemporalRoundingMode): TBigInteger;
var
  Quotient, Remainder, AbsRemainder, Half, AbsDivisor, Two: TBigInteger;
begin
  if ADivisor.IsOne then
    Exit(AValue);

  Quotient := AValue.Divide(ADivisor);
  Remainder := AValue.Modulo(ADivisor);

  if Remainder.IsZero then
    Exit(Quotient.Multiply(ADivisor));

  AbsRemainder := Remainder.AbsValue;
  AbsDivisor := ADivisor.AbsValue;
  Two := TBigInteger.FromInt64(2);

  case AMode of
    rmTrunc:
      Result := Quotient.Multiply(ADivisor);
    rmFloor:
    begin
      if AValue.IsNegative then
        Result := Quotient.Subtract(TBigInteger.One).Multiply(ADivisor)
      else
        Result := Quotient.Multiply(ADivisor);
    end;
    rmCeil:
    begin
      if AValue.IsPositive then
        Result := Quotient.Add(TBigInteger.One).Multiply(ADivisor)
      else
        Result := Quotient.Multiply(ADivisor);
    end;
    rmHalfExpand:
    begin
      Half := AbsRemainder.Multiply(Two);
      if Half.Compare(AbsDivisor) >= 0 then
      begin
        if AValue.IsPositive then
          Result := Quotient.Add(TBigInteger.One).Multiply(ADivisor)
        else
          Result := Quotient.Subtract(TBigInteger.One).Multiply(ADivisor);
      end
      else
        Result := Quotient.Multiply(ADivisor);
    end;
    rmHalfTrunc:
    begin
      Half := AbsRemainder.Multiply(Two);
      if Half.Compare(AbsDivisor) > 0 then
      begin
        if AValue.IsPositive then
          Result := Quotient.Add(TBigInteger.One).Multiply(ADivisor)
        else
          Result := Quotient.Subtract(TBigInteger.One).Multiply(ADivisor);
      end
      else
        Result := Quotient.Multiply(ADivisor);
    end;
    rmHalfCeil:
    begin
      Half := AbsRemainder.Multiply(Two);
      if Half.Compare(AbsDivisor) >= 0 then
      begin
        if AValue.IsPositive then
          Result := Quotient.Add(TBigInteger.One).Multiply(ADivisor)
        else if Half.Compare(AbsDivisor) > 0 then
          Result := Quotient.Subtract(TBigInteger.One).Multiply(ADivisor)
        else
          Result := Quotient.Multiply(ADivisor);
      end
      else
        Result := Quotient.Multiply(ADivisor);
    end;
    rmHalfFloor:
    begin
      Half := AbsRemainder.Multiply(Two);
      if Half.Compare(AbsDivisor) >= 0 then
      begin
        if AValue.IsNegative then
          Result := Quotient.Subtract(TBigInteger.One).Multiply(ADivisor)
        else if Half.Compare(AbsDivisor) > 0 then
          Result := Quotient.Add(TBigInteger.One).Multiply(ADivisor)
        else
          Result := Quotient.Multiply(ADivisor);
      end
      else
        Result := Quotient.Multiply(ADivisor);
    end;
    rmHalfEven:
    begin
      Half := AbsRemainder.Multiply(Two);
      if Half.Compare(AbsDivisor) > 0 then
      begin
        if AValue.IsPositive then
          Result := Quotient.Add(TBigInteger.One).Multiply(ADivisor)
        else
          Result := Quotient.Subtract(TBigInteger.One).Multiply(ADivisor);
      end
      else if Half.Compare(AbsDivisor) = 0 then
      begin
        if Quotient.GetBit(0) then
        begin
          if AValue.IsPositive then
            Result := Quotient.Add(TBigInteger.One).Multiply(ADivisor)
          else
            Result := Quotient.Subtract(TBigInteger.One).Multiply(ADivisor);
        end
        else
          Result := Quotient.Multiply(ADivisor);
      end
      else
        Result := Quotient.Multiply(ADivisor);
    end;
    rmExpand:
    begin
      // Always round away from zero
      if AValue.IsPositive then
        Result := Quotient.Add(TBigInteger.One).Multiply(ADivisor)
      else
        Result := Quotient.Subtract(TBigInteger.One).Multiply(ADivisor);
    end;
  else
    Result := Quotient.Multiply(ADivisor);
  end;
end;

function FormatTimeWithPrecision(const AHour, AMinute, ASecond, AMillisecond,
  AMicrosecond, ANanosecond: Integer; const AFractionalDigits: Integer): string;
var
  Frac: string;
begin
  Result := PadTwo(AHour) + ':' + PadTwo(AMinute) + ':' + PadTwo(ASecond);

  if AFractionalDigits < 0 then
  begin
    // auto - same as existing FormatTimeString behavior
    if ANanosecond <> 0 then
      Result := Result + '.' + Format('%.3d%.3d%.3d', [AMillisecond, AMicrosecond, ANanosecond])
    else if AMicrosecond <> 0 then
      Result := Result + '.' + Format('%.3d%.3d', [AMillisecond, AMicrosecond])
    else if AMillisecond <> 0 then
      Result := Result + '.' + Format('%.3d', [AMillisecond]);
  end
  else if AFractionalDigits = 0 then
    // no fractional seconds
  else
  begin
    Frac := Format('%.3d%.3d%.3d', [AMillisecond, AMicrosecond, ANanosecond]);
    Result := Result + '.' + Copy(Frac, 1, AFractionalDigits);
  end;
end;

{ --------------------------------------------------------------------------- }
{ Shared toString options                                                      }
{ --------------------------------------------------------------------------- }

procedure ResolveTemporalToStringOptions(
  const AOptions: TGocciaObjectValue;
  out AFractionalDigits: Integer;
  out ARoundingMode: TTemporalRoundingMode);
var
  SmallestUnit: TTemporalUnit;
  SmallestStr: string;
begin
  AFractionalDigits := -1; // auto
  ARoundingMode := rmTrunc;

  if AOptions = nil then Exit;

  // Check smallestUnit first — overrides fractionalSecondDigits
  SmallestStr := GetOptionString(AOptions, 'smallestUnit', '');
  if SmallestStr <> '' then
  begin
    if not GetTemporalUnitFromString(SmallestStr, SmallestUnit) then
      ThrowRangeError(Format(SErrorInvalidSmallestUnit, [SmallestStr]), SSuggestTemporalValidUnits);

    // smallestUnit takes precedence over fractionalSecondDigits
    case SmallestUnit of
      tuMinute:       AFractionalDigits := -2; // special: truncate seconds entirely
      tuSecond:       AFractionalDigits := 0;
      tuMillisecond:  AFractionalDigits := 3;
      tuMicrosecond:  AFractionalDigits := 6;
      tuNanosecond:   AFractionalDigits := 9;
    else
      ThrowRangeError(Format(SErrorInvalidSmallestUnit, [SmallestStr]), SSuggestTemporalValidUnits);
    end;
  end
  else
    AFractionalDigits := GetFractionalSecondDigits(AOptions);

  ARoundingMode := GetRoundingMode(AOptions, rmTrunc);
end;

function GetCalendarDisplay(const AOptions: TGocciaObjectValue): TTemporalCalendarDisplay;
var
  S: string;
begin
  Result := tcdAuto;
  if AOptions = nil then Exit;
  S := GetOptionString(AOptions, 'calendarName', 'auto');
  if S = 'auto' then
    Result := tcdAuto
  else if S = 'always' then
    Result := tcdAlways
  else if S = 'never' then
    Result := tcdNever
  else if S = 'critical' then
    Result := tcdCritical
  else
    ThrowRangeError('Invalid calendarName option: ' + S, SSuggestTemporalRoundArg);
end;

procedure RoundTimeForToString(
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

  // No rounding needed for auto or nanosecond precision
  if (AFractionalDigits < 0) and (AFractionalDigits <> -2) then Exit;
  if AFractionalDigits = 9 then Exit;

  // Convert time to total nanoseconds
  TotalNs := Int64(AHour) * NANOSECONDS_PER_HOUR +
             Int64(AMinute) * NANOSECONDS_PER_MINUTE +
             Int64(ASecond) * NANOSECONDS_PER_SECOND +
             Int64(AMs) * NANOSECONDS_PER_MILLISECOND +
             Int64(AUs) * NANOSECONDS_PER_MICROSECOND +
             Int64(ANs);

  // Determine divisor based on precision
  if AFractionalDigits = -2 then
    Divisor := NANOSECONDS_PER_MINUTE  // smallestUnit: 'minute'
  else if AFractionalDigits = 0 then
    Divisor := NANOSECONDS_PER_SECOND
  else if AFractionalDigits <= 3 then
    Divisor := NANOSECONDS_PER_MILLISECOND
  else if AFractionalDigits <= 6 then
    Divisor := NANOSECONDS_PER_MICROSECOND
  else
    Divisor := 1;

  Rounded := RoundWithMode(TotalNs, Divisor, ARoundingMode);

  // Balance back into time fields
  BalancedTime := BalanceTime(Rounded div NANOSECONDS_PER_HOUR,
    (Rounded mod NANOSECONDS_PER_HOUR) div NANOSECONDS_PER_MINUTE,
    (Rounded mod NANOSECONDS_PER_MINUTE) div NANOSECONDS_PER_SECOND,
    Integer((Rounded mod NANOSECONDS_PER_SECOND) div NANOSECONDS_PER_MILLISECOND),
    Integer((Rounded mod NANOSECONDS_PER_MILLISECOND) div NANOSECONDS_PER_MICROSECOND),
    Integer(Rounded mod NANOSECONDS_PER_MICROSECOND),
    ExDays);

  AHour := BalancedTime.Hour;
  AMinute := BalancedTime.Minute;
  ASecond := BalancedTime.Second;
  AMs := BalancedTime.Millisecond;
  AUs := BalancedTime.Microsecond;
  ANs := BalancedTime.Nanosecond;
  AExtraDays := Integer(ExDays);
end;

function FormatCalendarAnnotation(const ACalendarDisplay: TTemporalCalendarDisplay): string;
begin
  case ACalendarDisplay of
    tcdAlways:   Result := '[u-ca=iso8601]';
    tcdCritical: Result := '[!u-ca=iso8601]';
  else
    Result := ''; // auto and never: omit for iso8601
  end;
end;

{ --------------------------------------------------------------------------- }
{ CalendarDateUntil                                                            }
{ --------------------------------------------------------------------------- }

procedure CalendarDateUntil(
  const AY1, AM1, AD1, AY2, AM2, AD2: Integer;
  const ALargestUnit: TTemporalUnit;
  out AYears, AMonths, AWeeks, ADays: Int64);
var
  Sign: Integer;
  FromY, FromM, FromD, ToY, ToM, ToD: Integer;
  TotalMonths: Int64;
  Mid: TTemporalDateRecord;
  TotalDays: Int64;
begin
  AYears := 0;
  AMonths := 0;
  AWeeks := 0;
  ADays := 0;

  case ALargestUnit of
    tuDay:
    begin
      ADays := DateToEpochDays(AY2, AM2, AD2) - DateToEpochDays(AY1, AM1, AD1);
    end;

    tuWeek:
    begin
      TotalDays := DateToEpochDays(AY2, AM2, AD2) - DateToEpochDays(AY1, AM1, AD1);
      AWeeks := TotalDays div 7;
      ADays := TotalDays mod 7;
    end;

    tuMonth, tuYear:
    begin
      Sign := CompareDates(AY2, AM2, AD2, AY1, AM1, AD1);
      if Sign = 0 then Exit;

      // Always work from earlier to later, sign-correct at end
      if Sign > 0 then
      begin
        FromY := AY1; FromM := AM1; FromD := AD1;
        ToY := AY2; ToM := AM2; ToD := AD2;
      end
      else
      begin
        FromY := AY2; FromM := AM2; FromD := AD2;
        ToY := AY1; ToM := AM1; ToD := AD1;
      end;

      // Estimate total months from year/month components
      TotalMonths := Int64(ToY - FromY) * 12 + Int64(ToM - FromM);

      // Check if the estimate overshoots due to day clamping
      Mid := AddMonthsToDate(FromY, FromM, FromD, TotalMonths);
      if CompareDates(Mid.Year, Mid.Month, Mid.Day, ToY, ToM, ToD) > 0 then
      begin
        Dec(TotalMonths);
        Mid := AddMonthsToDate(FromY, FromM, FromD, TotalMonths);
      end;

      // Remainder in days
      ADays := DateToEpochDays(ToY, ToM, ToD) -
               DateToEpochDays(Mid.Year, Mid.Month, Mid.Day);

      if ALargestUnit = tuYear then
      begin
        AYears := TotalMonths div 12;
        AMonths := TotalMonths mod 12;
      end
      else
        AMonths := TotalMonths;

      // Apply sign
      if Sign < 0 then
      begin
        AYears := -AYears;
        AMonths := -AMonths;
        ADays := -ADays;
      end;
    end;
  end;
end;

{ --------------------------------------------------------------------------- }
{ RoundDiffDuration                                                            }
{ --------------------------------------------------------------------------- }

procedure RoundDiffDuration(
  var AYears, AMonths, AWeeks, ADays,
      AHours, AMinutes, ASeconds,
      AMilliseconds, AMicroseconds, ANanoseconds: Int64;
  const AStartY, AStartM, AStartD: Integer;
  const ALargestUnit, ASmallestUnit: TTemporalUnit;
  const AMode: TTemporalRoundingMode;
  const AIncrement: Integer);
var
  TimeNs, TotalNs, Divisor: Int64;
  RemNs: Int64;
  Sign: Integer;
  WholeUnits, ScaledValue, PeriodNs, RoundedValue: Int64;
  WalkDate, NextDate, EndDate: TTemporalDateRecord;
  StartEpoch, EndEpoch, WalkEpoch, NextEpoch: Int64;
  ResultYears, ResultMonths, ResultDays: Int64;
  ResultHours, ResultMinutes, ResultSeconds: Int64;
  ResultMs, ResultUs, ResultNs: Int64;
begin
  // Nothing to do when smallestUnit equals the finest granularity and increment is 1
  if (ASmallestUnit = tuNanosecond) and (AIncrement = 1) then Exit;

  // Compute the overall sign of the duration
  if AYears > 0 then Sign := 1
  else if AYears < 0 then Sign := -1
  else if AMonths > 0 then Sign := 1
  else if AMonths < 0 then Sign := -1
  else if AWeeks > 0 then Sign := 1
  else if AWeeks < 0 then Sign := -1
  else if ADays > 0 then Sign := 1
  else if ADays < 0 then Sign := -1
  else if AHours > 0 then Sign := 1
  else if AHours < 0 then Sign := -1
  else if AMinutes > 0 then Sign := 1
  else if AMinutes < 0 then Sign := -1
  else if ASeconds > 0 then Sign := 1
  else if ASeconds < 0 then Sign := -1
  else if AMilliseconds > 0 then Sign := 1
  else if AMilliseconds < 0 then Sign := -1
  else if AMicroseconds > 0 then Sign := 1
  else if AMicroseconds < 0 then Sign := -1
  else if ANanoseconds > 0 then Sign := 1
  else if ANanoseconds < 0 then Sign := -1
  else
    Exit; // zero duration, nothing to round

  // Collect sub-day time as nanoseconds
  TimeNs := ANanoseconds +
            AMicroseconds * NANOSECONDS_PER_MICROSECOND +
            AMilliseconds * NANOSECONDS_PER_MILLISECOND +
            ASeconds * NANOSECONDS_PER_SECOND +
            AMinutes * NANOSECONDS_PER_MINUTE +
            AHours * NANOSECONDS_PER_HOUR;

  // --- Calendar-unit rounding (year / month) ---
  if (ASmallestUnit = tuYear) or (ASmallestUnit = tuMonth) then
  begin
    // Resolve duration to concrete end date via calendar walking
    EndDate.Year := AStartY; EndDate.Month := AStartM; EndDate.Day := AStartD;
    if AYears <> 0 then
      EndDate := AddMonthsToDate(EndDate.Year, EndDate.Month, EndDate.Day, AYears * 12);
    if AMonths <> 0 then
      EndDate := AddMonthsToDate(EndDate.Year, EndDate.Month, EndDate.Day, AMonths);
    EndDate := AddDaysToDate(EndDate.Year, EndDate.Month, EndDate.Day,
      AWeeks * 7 + ADays + TimeNs div NANOSECONDS_PER_DAY);
    TimeNs := TimeNs mod NANOSECONDS_PER_DAY;

    StartEpoch := DateToEpochDays(AStartY, AStartM, AStartD);
    EndEpoch := DateToEpochDays(EndDate.Year, EndDate.Month, EndDate.Day);

    if ASmallestUnit = tuYear then
    begin
      // Walk years from start
      WholeUnits := 0;
      if Sign > 0 then
      begin
        repeat
          NextDate := AddMonthsToDate(AStartY, AStartM, AStartD, (WholeUnits + 1) * 12);
          NextEpoch := DateToEpochDays(NextDate.Year, NextDate.Month, NextDate.Day);
          if (NextEpoch > EndEpoch) or ((NextEpoch = EndEpoch) and (TimeNs <= 0)) then Break;
          Inc(WholeUnits);
        until False;
      end
      else
      begin
        repeat
          NextDate := AddMonthsToDate(AStartY, AStartM, AStartD, (WholeUnits - 1) * 12);
          NextEpoch := DateToEpochDays(NextDate.Year, NextDate.Month, NextDate.Day);
          if (NextEpoch < EndEpoch) or ((NextEpoch = EndEpoch) and (TimeNs >= 0)) then Break;
          Dec(WholeUnits);
        until False;
      end;

      // Align WholeUnits to the nearest increment boundary toward zero,
      // then use the fractional distance to decide whether rounding carries
      // us to the next bucket (sign-aware).
      if Sign > 0 then
        ScaledValue := (WholeUnits div AIncrement) * AIncrement
      else
        ScaledValue := -(((-WholeUnits) div AIncrement) * AIncrement);

      WalkDate := AddMonthsToDate(AStartY, AStartM, AStartD, ScaledValue * 12);
      WalkEpoch := DateToEpochDays(WalkDate.Year, WalkDate.Month, WalkDate.Day);
      NextDate := AddMonthsToDate(AStartY, AStartM, AStartD, (ScaledValue + Sign * AIncrement) * 12);
      NextEpoch := DateToEpochDays(NextDate.Year, NextDate.Month, NextDate.Day);
      PeriodNs := Abs(NextEpoch - WalkEpoch) * NANOSECONDS_PER_DAY;
      RoundedValue := RoundWithMode(
        Abs((EndEpoch - WalkEpoch) * NANOSECONDS_PER_DAY + TimeNs), PeriodNs, AMode);
      if RoundedValue >= PeriodNs then
        WholeUnits := ScaledValue + Sign * AIncrement
      else
        WholeUnits := ScaledValue;

      AYears := WholeUnits; AMonths := 0; AWeeks := 0; ADays := 0;
      AHours := 0; AMinutes := 0; ASeconds := 0;
      AMilliseconds := 0; AMicroseconds := 0; ANanoseconds := 0;
    end
    else // tuMonth
    begin
      WholeUnits := 0;
      if Sign > 0 then
      begin
        repeat
          NextDate := AddMonthsToDate(AStartY, AStartM, AStartD, WholeUnits + 1);
          NextEpoch := DateToEpochDays(NextDate.Year, NextDate.Month, NextDate.Day);
          if (NextEpoch > EndEpoch) or ((NextEpoch = EndEpoch) and (TimeNs <= 0)) then Break;
          Inc(WholeUnits);
        until False;
      end
      else
      begin
        repeat
          NextDate := AddMonthsToDate(AStartY, AStartM, AStartD, WholeUnits - 1);
          NextEpoch := DateToEpochDays(NextDate.Year, NextDate.Month, NextDate.Day);
          if (NextEpoch < EndEpoch) or ((NextEpoch = EndEpoch) and (TimeNs >= 0)) then Break;
          Dec(WholeUnits);
        until False;
      end;

      if Sign > 0 then
        ScaledValue := (WholeUnits div AIncrement) * AIncrement
      else
        ScaledValue := -(((-WholeUnits) div AIncrement) * AIncrement);

      WalkDate := AddMonthsToDate(AStartY, AStartM, AStartD, ScaledValue);
      WalkEpoch := DateToEpochDays(WalkDate.Year, WalkDate.Month, WalkDate.Day);
      NextDate := AddMonthsToDate(AStartY, AStartM, AStartD, ScaledValue + Sign * AIncrement);
      NextEpoch := DateToEpochDays(NextDate.Year, NextDate.Month, NextDate.Day);
      PeriodNs := Abs(NextEpoch - WalkEpoch) * NANOSECONDS_PER_DAY;
      RoundedValue := RoundWithMode(
        Abs((EndEpoch - WalkEpoch) * NANOSECONDS_PER_DAY + TimeNs), PeriodNs, AMode);
      if RoundedValue >= PeriodNs then
        WholeUnits := ScaledValue + Sign * AIncrement
      else
        WholeUnits := ScaledValue;

      if Ord(ALargestUnit) <= Ord(tuYear) then
      begin
        AYears := WholeUnits div 12;
        AMonths := WholeUnits mod 12;
      end
      else
      begin
        AYears := 0;
        AMonths := WholeUnits;
      end;
      AWeeks := 0; ADays := 0;
      AHours := 0; AMinutes := 0; ASeconds := 0;
      AMilliseconds := 0; AMicroseconds := 0; ANanoseconds := 0;
    end;
    Exit;
  end;

  // --- Week / day / time-unit rounding ---
  // Convert everything to total nanoseconds from the start date
  if (ASmallestUnit = tuWeek) or (ASmallestUnit = tuDay) or
     (Ord(ASmallestUnit) >= Ord(tuHour)) then
  begin
    // For calendar types that have year/month, resolve to epoch days first
    if (AYears <> 0) or (AMonths <> 0) then
    begin
      EndDate.Year := AStartY; EndDate.Month := AStartM; EndDate.Day := AStartD;
      if AYears <> 0 then
        EndDate := AddMonthsToDate(EndDate.Year, EndDate.Month, EndDate.Day, AYears * 12);
      if AMonths <> 0 then
        EndDate := AddMonthsToDate(EndDate.Year, EndDate.Month, EndDate.Day, AMonths);
      EndDate := AddDaysToDate(EndDate.Year, EndDate.Month, EndDate.Day, AWeeks * 7 + ADays);
      StartEpoch := DateToEpochDays(AStartY, AStartM, AStartD);
      EndEpoch := DateToEpochDays(EndDate.Year, EndDate.Month, EndDate.Day);
      TotalNs := (EndEpoch - StartEpoch) * NANOSECONDS_PER_DAY + TimeNs;
    end
    else
      TotalNs := (AWeeks * 7 + ADays) * NANOSECONDS_PER_DAY + TimeNs;

    // Round
    if ASmallestUnit = tuWeek then
      Divisor := NANOSECONDS_PER_DAY * 7 * AIncrement
    else
      Divisor := UnitToNanoseconds(ASmallestUnit) * AIncrement;
    TotalNs := RoundWithMode(TotalNs, Divisor, AMode);

    // Rebalance into largestUnit fields
    if (AYears <> 0) or (AMonths <> 0) or
       (Ord(ALargestUnit) <= Ord(tuMonth)) then
    begin
      // Need calendar rebalance back to year/month/day
      ResultDays := TotalNs div NANOSECONDS_PER_DAY;
      RemNs := TotalNs mod NANOSECONDS_PER_DAY;

      StartEpoch := DateToEpochDays(AStartY, AStartM, AStartD);
      EndEpoch := StartEpoch + ResultDays;

      if Ord(ALargestUnit) <= Ord(tuMonth) then
      begin
        if TotalNs > 0 then Sign := 1
        else if TotalNs < 0 then Sign := -1
        else Sign := 0;

        WholeUnits := 0;
        if Sign > 0 then
        begin
          repeat
            NextDate := AddMonthsToDate(AStartY, AStartM, AStartD, WholeUnits + 1);
            NextEpoch := DateToEpochDays(NextDate.Year, NextDate.Month, NextDate.Day);
            if NextEpoch > EndEpoch then Break;
            Inc(WholeUnits);
          until False;
        end
        else if Sign < 0 then
        begin
          repeat
            NextDate := AddMonthsToDate(AStartY, AStartM, AStartD, WholeUnits - 1);
            NextEpoch := DateToEpochDays(NextDate.Year, NextDate.Month, NextDate.Day);
            if NextEpoch < EndEpoch then Break;
            Dec(WholeUnits);
          until False;
        end;

        WalkDate := AddMonthsToDate(AStartY, AStartM, AStartD, WholeUnits);
        WalkEpoch := DateToEpochDays(WalkDate.Year, WalkDate.Month, WalkDate.Day);
        ResultDays := EndEpoch - WalkEpoch;

        // When smallestUnit is "week", the day remainder must be divisible
        // by 7.  Back off months until that holds (or fall to zero months).
        if (ASmallestUnit = tuWeek) and (WholeUnits <> 0) and
           (ResultDays mod 7 <> 0) then
        begin
          if Sign > 0 then
          begin
            while (WholeUnits > 0) and (ResultDays mod 7 <> 0) do
            begin
              Dec(WholeUnits);
              WalkDate := AddMonthsToDate(AStartY, AStartM, AStartD, WholeUnits);
              WalkEpoch := DateToEpochDays(WalkDate.Year, WalkDate.Month, WalkDate.Day);
              ResultDays := EndEpoch - WalkEpoch;
            end;
          end
          else
          begin
            while (WholeUnits < 0) and (ResultDays mod 7 <> 0) do
            begin
              Inc(WholeUnits);
              WalkDate := AddMonthsToDate(AStartY, AStartM, AStartD, WholeUnits);
              WalkEpoch := DateToEpochDays(WalkDate.Year, WalkDate.Month, WalkDate.Day);
              ResultDays := EndEpoch - WalkEpoch;
            end;
          end;
        end;

        if Ord(ALargestUnit) <= Ord(tuYear) then
        begin
          ResultYears := WholeUnits div 12;
          ResultMonths := WholeUnits mod 12;
        end
        else
        begin
          ResultYears := 0;
          ResultMonths := WholeUnits;
        end;
      end
      else
      begin
        ResultYears := 0;
        ResultMonths := 0;
      end;

      // Decompose time
      ResultHours := RemNs div NANOSECONDS_PER_HOUR;
      RemNs := RemNs mod NANOSECONDS_PER_HOUR;
      ResultMinutes := RemNs div NANOSECONDS_PER_MINUTE;
      RemNs := RemNs mod NANOSECONDS_PER_MINUTE;
      ResultSeconds := RemNs div NANOSECONDS_PER_SECOND;
      RemNs := RemNs mod NANOSECONDS_PER_SECOND;
      ResultMs := RemNs div NANOSECONDS_PER_MILLISECOND;
      RemNs := RemNs mod NANOSECONDS_PER_MILLISECOND;
      ResultUs := RemNs div NANOSECONDS_PER_MICROSECOND;
      ResultNs := RemNs mod NANOSECONDS_PER_MICROSECOND;

      AYears := ResultYears; AMonths := ResultMonths;
      if Ord(ALargestUnit) <= Ord(tuWeek) then
      begin
        AWeeks := ResultDays div 7;
        ADays := ResultDays mod 7;
      end
      else
      begin
        AWeeks := 0;
        ADays := ResultDays;
      end;
      AHours := ResultHours; AMinutes := ResultMinutes;
      ASeconds := ResultSeconds; AMilliseconds := ResultMs;
      AMicroseconds := ResultUs; ANanoseconds := ResultNs;
    end
    else
    begin
      // Non-calendar breakdown
      RemNs := TotalNs;
      ResultDays := 0;
      ResultHours := 0;
      ResultMinutes := 0;
      ResultSeconds := 0;
      ResultMs := 0;
      ResultUs := 0;
      ResultNs := 0;

      if Ord(ALargestUnit) <= Ord(tuWeek) then
      begin
        ResultDays := RemNs div NANOSECONDS_PER_DAY;
        RemNs := RemNs mod NANOSECONDS_PER_DAY;
      end
      else if Ord(ALargestUnit) <= Ord(tuDay) then
      begin
        ResultDays := RemNs div NANOSECONDS_PER_DAY;
        RemNs := RemNs mod NANOSECONDS_PER_DAY;
      end;
      if Ord(ALargestUnit) <= Ord(tuHour) then
      begin
        ResultHours := RemNs div NANOSECONDS_PER_HOUR;
        RemNs := RemNs mod NANOSECONDS_PER_HOUR;
      end;
      if Ord(ALargestUnit) <= Ord(tuMinute) then
      begin
        ResultMinutes := RemNs div NANOSECONDS_PER_MINUTE;
        RemNs := RemNs mod NANOSECONDS_PER_MINUTE;
      end;
      if Ord(ALargestUnit) <= Ord(tuSecond) then
      begin
        ResultSeconds := RemNs div NANOSECONDS_PER_SECOND;
        RemNs := RemNs mod NANOSECONDS_PER_SECOND;
      end;
      if Ord(ALargestUnit) <= Ord(tuMillisecond) then
      begin
        ResultMs := RemNs div NANOSECONDS_PER_MILLISECOND;
        RemNs := RemNs mod NANOSECONDS_PER_MILLISECOND;
      end;
      if Ord(ALargestUnit) <= Ord(tuMicrosecond) then
      begin
        ResultUs := RemNs div NANOSECONDS_PER_MICROSECOND;
        RemNs := RemNs mod NANOSECONDS_PER_MICROSECOND;
      end;
      ResultNs := RemNs;

      AYears := 0; AMonths := 0;
      if Ord(ALargestUnit) <= Ord(tuWeek) then
      begin
        AWeeks := ResultDays div 7;
        ADays := ResultDays mod 7;
      end
      else
      begin
        AWeeks := 0;
        ADays := ResultDays;
      end;
      AHours := ResultHours; AMinutes := ResultMinutes;
      ASeconds := ResultSeconds; AMilliseconds := ResultMs;
      AMicroseconds := ResultUs; ANanoseconds := ResultNs;
    end;
  end;
end;

{ --------------------------------------------------------------------------- }
{ GetDiffOptions                                                               }
{ --------------------------------------------------------------------------- }

function GetDiffOptions(const AArgs: TGocciaArgumentsCollection;
  const AIndex: Integer): TGocciaObjectValue;
var
  Arg: TGocciaValue;
begin
  Result := nil;
  Arg := AArgs.GetElement(AIndex);
  if (Arg = nil) or (Arg is TGocciaUndefinedLiteralValue) then
    Exit;
  if Arg is TGocciaObjectValue then
    Result := TGocciaObjectValue(Arg)
  else
    ThrowTypeError('Options argument must be an object or undefined', '');
end;

end.
