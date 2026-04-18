unit Goccia.Temporal.Options;

{$I Goccia.inc}

interface

uses
  BigInteger,

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
    rmHalfEven
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

end.
