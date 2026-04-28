unit Goccia.Temporal.DurationMath;

{$I Goccia.inc}

interface

uses
  BigInteger,

  Goccia.Temporal.Options;

function EpochNanosecondsFromParts(const AEpochMilliseconds: Int64;
  const ASubMillisecondNanoseconds: Integer): TBigInteger;
function TimeDurationFromEpochNanosecondsDifference(const AOne, ATwo: TBigInteger): TBigInteger;
function TimeDurationFromComponents(const AHours, AMinutes, ASeconds, AMilliseconds,
  AMicroseconds, ANanoseconds: TBigInteger): TBigInteger;
function RoundTimeDurationToIncrement(const ATimeDuration, AIncrement: TBigInteger;
  const AMode: TTemporalRoundingMode): TBigInteger;
function IsValidEpochNanoseconds(const AValue: TBigInteger): Boolean;
function IsValidTimeDuration(const AValue: TBigInteger): Boolean;
procedure BalanceTimeDurationToFields(const ATimeDuration: TBigInteger;
  const ALargestUnit: TTemporalUnit;
  out AHours, AMinutes, ASeconds, AMilliseconds, AMicroseconds, ANanoseconds: TBigInteger;
  out ADays: Int64);

implementation

const
  MAX_EPOCH_NANOSECONDS_DECIMAL = '8640000000000000000000';
  MAX_TIME_DURATION_DECIMAL = '9007199254740991999999999';

function BigInt(const AValue: Int64): TBigInteger; inline;
begin
  Result := TBigInteger.FromInt64(AValue);
end;

function BigIntFromDecimal(const AValue: string): TBigInteger; inline;
begin
  Result := TBigInteger.FromDecimalString(AValue);
end;

function BigIntForUnit(const AUnit: TTemporalUnit): TBigInteger;
begin
  case AUnit of
    tuDay: Result := BigInt(NANOSECONDS_PER_DAY);
    tuHour: Result := BigInt(NANOSECONDS_PER_HOUR);
    tuMinute: Result := BigInt(NANOSECONDS_PER_MINUTE);
    tuSecond: Result := BigInt(NANOSECONDS_PER_SECOND);
    tuMillisecond: Result := BigInt(NANOSECONDS_PER_MILLISECOND);
    tuMicrosecond: Result := BigInt(NANOSECONDS_PER_MICROSECOND);
  else
    Result := TBigInteger.One;
  end;
end;

// TC39 Temporal §8.3.4 get Temporal.Instant.prototype.epochNanoseconds
function EpochNanosecondsFromParts(const AEpochMilliseconds: Int64;
  const ASubMillisecondNanoseconds: Integer): TBigInteger;
begin
  Result := BigInt(AEpochMilliseconds)
    .Multiply(BigInt(NANOSECONDS_PER_MILLISECOND))
    .Add(BigInt(ASubMillisecondNanoseconds));
end;

// TC39 Temporal §7.5.26 TimeDurationFromEpochNanosecondsDifference(one, two)
function TimeDurationFromEpochNanosecondsDifference(const AOne, ATwo: TBigInteger): TBigInteger;
begin
  Result := AOne.Subtract(ATwo);
end;

// TC39 Temporal §7.5.21 TimeDurationFromComponents(hours, minutes, seconds, milliseconds, microseconds, nanoseconds)
function TimeDurationFromComponents(const AHours, AMinutes, ASeconds, AMilliseconds,
  AMicroseconds, ANanoseconds: TBigInteger): TBigInteger;
var
  Minutes, Seconds, Milliseconds, Microseconds: TBigInteger;
begin
  Minutes := AMinutes.Add(AHours.Multiply(BigInt(60)));
  Seconds := ASeconds.Add(Minutes.Multiply(BigInt(60)));
  Milliseconds := AMilliseconds.Add(Seconds.Multiply(BigInt(1000)));
  Microseconds := AMicroseconds.Add(Milliseconds.Multiply(BigInt(1000)));
  Result := ANanoseconds.Add(Microseconds.Multiply(BigInt(1000)));
end;

// TC39 Temporal §7.5.27 RoundTimeDurationToIncrement(d, increment, roundingMode)
function RoundTimeDurationToIncrement(const ATimeDuration, AIncrement: TBigInteger;
  const AMode: TTemporalRoundingMode): TBigInteger;
begin
  Result := RoundBigIntWithMode(ATimeDuration, AIncrement, AMode);
end;

// TC39 Temporal §8.5.2 IsValidEpochNanoseconds(epochNanoseconds)
function IsValidEpochNanoseconds(const AValue: TBigInteger): Boolean;
begin
  Result := AValue.AbsValue.Compare(BigIntFromDecimal(MAX_EPOCH_NANOSECONDS_DECIMAL)) <= 0;
end;

// TC39 Temporal §7.5.3 time duration
function IsValidTimeDuration(const AValue: TBigInteger): Boolean;
begin
  Result := AValue.AbsValue.Compare(BigIntFromDecimal(MAX_TIME_DURATION_DECIMAL)) <= 0;
end;

procedure TakeUnit(var ARemaining: TBigInteger; const AUnit: TBigInteger; out AField: TBigInteger);
begin
  AField := ARemaining.Divide(AUnit);
  ARemaining := ARemaining.Modulo(AUnit);
end;

// TC39 Temporal §7.5.8 TemporalDurationFromInternal(internalDuration, largestUnit)
procedure BalanceTimeDurationToFields(const ATimeDuration: TBigInteger;
  const ALargestUnit: TTemporalUnit;
  out AHours, AMinutes, ASeconds, AMilliseconds, AMicroseconds, ANanoseconds: TBigInteger;
  out ADays: Int64);
var
  Sign: Integer;
  Remaining, DaysBig: TBigInteger;
begin
  AHours := TBigInteger.Zero;
  AMinutes := TBigInteger.Zero;
  ASeconds := TBigInteger.Zero;
  AMilliseconds := TBigInteger.Zero;
  AMicroseconds := TBigInteger.Zero;
  ANanoseconds := TBigInteger.Zero;
  ADays := 0;

  if ATimeDuration.IsZero then
    Exit;

  if ATimeDuration.IsNegative then
    Sign := -1
  else
    Sign := 1;
  Remaining := ATimeDuration.AbsValue;

  if ALargestUnit in [tuYear, tuMonth, tuWeek, tuDay] then
  begin
    DaysBig := Remaining.Divide(BigIntForUnit(tuDay));
    ADays := DaysBig.ToInt64 * Sign;
    Remaining := Remaining.Modulo(BigIntForUnit(tuDay));
  end;

  if Ord(ALargestUnit) <= Ord(tuHour) then
    TakeUnit(Remaining, BigIntForUnit(tuHour), AHours);
  if Ord(ALargestUnit) <= Ord(tuMinute) then
    TakeUnit(Remaining, BigIntForUnit(tuMinute), AMinutes);
  if Ord(ALargestUnit) <= Ord(tuSecond) then
    TakeUnit(Remaining, BigIntForUnit(tuSecond), ASeconds);
  if Ord(ALargestUnit) <= Ord(tuMillisecond) then
    TakeUnit(Remaining, BigIntForUnit(tuMillisecond), AMilliseconds);
  if Ord(ALargestUnit) <= Ord(tuMicrosecond) then
    TakeUnit(Remaining, BigIntForUnit(tuMicrosecond), AMicroseconds);
  ANanoseconds := Remaining;

  if Sign < 0 then
  begin
    AHours := AHours.Negate;
    AMinutes := AMinutes.Negate;
    ASeconds := ASeconds.Negate;
    AMilliseconds := AMilliseconds.Negate;
    AMicroseconds := AMicroseconds.Negate;
    ANanoseconds := ANanoseconds.Negate;
  end;
end;

end.
