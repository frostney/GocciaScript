unit Goccia.Values.TemporalDuration;

{$I Goccia.inc}

interface

uses
  SysUtils,

  BigInteger,

  Goccia.Arguments.Collection,
  Goccia.ObjectModel,
  Goccia.SharedPrototype,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  ETemporalDurationInt64Overflow = class(Exception);

  TGocciaTemporalDurationValue = class(TGocciaObjectValue)
  private
    FYearsBig: TBigInteger;
    FMonthsBig: TBigInteger;
    FWeeksBig: TBigInteger;
    FDaysBig: TBigInteger;
    FHoursBig: TBigInteger;
    FMinutesBig: TBigInteger;
    FSecondsBig: TBigInteger;
    FMillisecondsBig: TBigInteger;
    FMicrosecondsBig: TBigInteger;
    FNanosecondsBig: TBigInteger;

    procedure InitializePrototype;
    procedure SetDurationFields(const AYears, AMonths, AWeeks, ADays, AHours, AMinutes, ASeconds,
      AMilliseconds, AMicroseconds, ANanoseconds: TBigInteger);

    function GetYearsInt64: Int64;
    function GetMonthsInt64: Int64;
    function GetWeeksInt64: Int64;
    function GetDaysInt64: Int64;
    function GetHoursInt64: Int64;
    function GetMinutesInt64: Int64;
    function GetSecondsInt64: Int64;
    function GetMillisecondsInt64: Int64;
    function GetMicrosecondsInt64: Int64;
    function GetNanosecondsInt64: Int64;
    function ComputeSign: Integer;
    function IsBlank: Boolean;
    function ToISOString: string;
  public
    constructor Create(const AYears, AMonths, AWeeks, ADays, AHours, AMinutes, ASeconds,
      AMilliseconds, AMicroseconds, ANanoseconds: Int64); overload;
    constructor CreateFromBigIntegers(const AYears, AMonths, AWeeks, ADays, AHours, AMinutes, ASeconds,
      AMilliseconds, AMicroseconds, ANanoseconds: TBigInteger); overload;

    function ToStringTag: string; override;
    class procedure ExposePrototype(const AConstructor: TGocciaObjectValue);

    property Years: Int64 read GetYearsInt64;
    property Months: Int64 read GetMonthsInt64;
    property Weeks: Int64 read GetWeeksInt64;
    property Days: Int64 read GetDaysInt64;
    property Hours: Int64 read GetHoursInt64;
    property Minutes: Int64 read GetMinutesInt64;
    property Seconds: Int64 read GetSecondsInt64;
    property Milliseconds: Int64 read GetMillisecondsInt64;
    property Microseconds: Int64 read GetMicrosecondsInt64;
    property Nanoseconds: Int64 read GetNanosecondsInt64;
    property YearsBig: TBigInteger read FYearsBig;
    property MonthsBig: TBigInteger read FMonthsBig;
    property WeeksBig: TBigInteger read FWeeksBig;
    property DaysBig: TBigInteger read FDaysBig;
    property HoursBig: TBigInteger read FHoursBig;
    property MinutesBig: TBigInteger read FMinutesBig;
    property SecondsBig: TBigInteger read FSecondsBig;
    property MillisecondsBig: TBigInteger read FMillisecondsBig;
    property MicrosecondsBig: TBigInteger read FMicrosecondsBig;
    property NanosecondsBig: TBigInteger read FNanosecondsBig;
  published
    function GetYears(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetMonths(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetWeeks(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetDays(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetHours(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetMinutes(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetSeconds(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetMilliseconds(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetMicroseconds(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetNanoseconds(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetSign(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetBlank(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DurationNegated(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DurationAbs(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DurationAdd(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DurationSubtract(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DurationWith(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DurationRound(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DurationToString(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DurationToJSON(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DurationValueOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DurationTotal(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DurationToLocaleString(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  end;

// Temporal §13.20 ToIntegerIfIntegral for a Duration field: throws RangeError
// for non-finite or non-integral numbers, preserves the full float64 integer
// range via TBigInteger (duration components may exceed Int64, e.g.
// nanoseconds up to ~9e24 within the 2^53-second cap).
function DurationFieldToBigInteger(const AValue: TGocciaValue): TBigInteger;

// Builds a Duration from a property-bag object, reading every component with
// DurationFieldToBigInteger. Shared by Duration.prototype.with/add/subtract
// and the Temporal.Duration constructor/from/compare entry points.
function DurationFromObject(const AObj: TGocciaObjectValue): TGocciaTemporalDurationValue;
function DurationFromString(const AValue, AErrorMessage: string): TGocciaTemporalDurationValue;
function CompareTemporalDurations(const AOne, ATwo: TGocciaTemporalDurationValue;
  const AOptions: TGocciaValue): Integer;

implementation

uses
  Math,

  Goccia.Constants.NumericLimits,
  Goccia.Constants.PropertyNames,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.Realm,
  Goccia.Temporal.Calendar,
  Goccia.Temporal.DurationMath,
  Goccia.Temporal.Options,
  Goccia.Temporal.TimeZone,
  Goccia.Temporal.Utils,
  Goccia.ThreadCleanupRegistry,
  Goccia.Utils,
  Goccia.Values.ErrorHelper,
  Goccia.Values.IntlDurationFormat,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.SymbolValue,
  Goccia.Values.TemporalPlainDate,
  Goccia.Values.TemporalPlainDateTime,
  Goccia.Values.TemporalPlainMonthDay,
  Goccia.Values.TemporalPlainYearMonth,
  Goccia.Values.TemporalZonedDateTime;

var
  GTemporalDurationSharedSlot: TGocciaRealmOwnedSlotId;

threadvar
  FPrototypeMembers: TArray<TGocciaMemberDefinition>;

procedure ClearThreadvarMembers;
begin
  SetLength(FPrototypeMembers, 0);
end;

type
  TDurationRelativeToRecord = record
    HasRelativeTo: Boolean;
    IsZoned: Boolean;
    Date: TTemporalDateRecord;
    Hour: Integer;
    Minute: Integer;
    Second: Integer;
    Millisecond: Integer;
    Microsecond: Integer;
    Nanosecond: Integer;
    EpochMilliseconds: Int64;
    SubMillisecondNanoseconds: Integer;
    TimeZone: string;
    CalendarId: string;
  end;

  TDurationFieldsBig = record
    Years: TBigInteger;
    Months: TBigInteger;
    Weeks: TBigInteger;
    Days: TBigInteger;
    Hours: TBigInteger;
    Minutes: TBigInteger;
    Seconds: TBigInteger;
    Milliseconds: TBigInteger;
    Microseconds: TBigInteger;
    Nanoseconds: TBigInteger;
    HasAny: Boolean;
  end;

function GetTemporalDurationShared: TGocciaSharedPrototype; inline;
begin
  if Assigned(CurrentRealm) then
    Result := TGocciaSharedPrototype(CurrentRealm.GetOwnedSlot(GTemporalDurationSharedSlot))
  else
    Result := nil;
end;

function AsDuration(const AValue: TGocciaValue; const AMethod: string): TGocciaTemporalDurationValue;
begin
  if not (AValue is TGocciaTemporalDurationValue) then
    ThrowTypeError(AMethod + ' called on non-Duration', SSuggestTemporalThisType);
  Result := TGocciaTemporalDurationValue(AValue);
end;

function ParseRelativeTo(const AValue: TGocciaValue; const AMethod: string): TTemporalDateRecord; forward;
function ParseRelativeToRecord(const AValue: TGocciaValue;
  const AMethod: string): TDurationRelativeToRecord; forward;
function RoundBigToFloat64(const AValue: TBigInteger): TBigInteger; forward;
function RoundingDivisorBig(const ASmallestUnit: TTemporalUnit;
  const AIncrement: Integer): TBigInteger; forward;

function ZeroDurationFields: TDurationFieldsBig;
begin
  Result.Years := TBigInteger.Zero;
  Result.Months := TBigInteger.Zero;
  Result.Weeks := TBigInteger.Zero;
  Result.Days := TBigInteger.Zero;
  Result.Hours := TBigInteger.Zero;
  Result.Minutes := TBigInteger.Zero;
  Result.Seconds := TBigInteger.Zero;
  Result.Milliseconds := TBigInteger.Zero;
  Result.Microseconds := TBigInteger.Zero;
  Result.Nanoseconds := TBigInteger.Zero;
  Result.HasAny := False;
end;

function DurationFieldsFromValue(const AValue: TGocciaTemporalDurationValue): TDurationFieldsBig;
begin
  Result.Years := AValue.FYearsBig;
  Result.Months := AValue.FMonthsBig;
  Result.Weeks := AValue.FWeeksBig;
  Result.Days := AValue.FDaysBig;
  Result.Hours := AValue.FHoursBig;
  Result.Minutes := AValue.FMinutesBig;
  Result.Seconds := AValue.FSecondsBig;
  Result.Milliseconds := AValue.FMillisecondsBig;
  Result.Microseconds := AValue.FMicrosecondsBig;
  Result.Nanoseconds := AValue.FNanosecondsBig;
  Result.HasAny := False;
end;

function ReadDurationFieldsFromObject(const AObj: TGocciaObjectValue;
  const ADefaults: TDurationFieldsBig): TDurationFieldsBig;
var
  SawAny: Boolean;
  Fields: TDurationFieldsBig;

  function ReadField(const AName: string; const ADefault: TBigInteger): TBigInteger;
  var
    V: TGocciaValue;
  begin
    V := AObj.GetProperty(AName);
    if (V = nil) or (V is TGocciaUndefinedLiteralValue) then
      Result := ADefault
    else
    begin
      SawAny := True;
      Result := DurationFieldToBigInteger(V);
    end;
  end;

begin
  SawAny := False;
  Fields := ADefaults;
  Fields.HasAny := False;
  Fields.Days := ReadField('days', ADefaults.Days);
  Fields.Hours := ReadField('hours', ADefaults.Hours);
  Fields.Microseconds := ReadField('microseconds', ADefaults.Microseconds);
  Fields.Milliseconds := ReadField('milliseconds', ADefaults.Milliseconds);
  Fields.Minutes := ReadField('minutes', ADefaults.Minutes);
  Fields.Months := ReadField('months', ADefaults.Months);
  Fields.Nanoseconds := ReadField('nanoseconds', ADefaults.Nanoseconds);
  Fields.Seconds := ReadField('seconds', ADefaults.Seconds);
  Fields.Weeks := ReadField('weeks', ADefaults.Weeks);
  Fields.Years := ReadField('years', ADefaults.Years);

  Fields.HasAny := SawAny;
  if not Fields.HasAny then
    ThrowTypeError('Duration-like object must contain at least one Duration field',
      SSuggestTemporalDurationArg);
  Result := Fields;
end;

function DurationFromObject(const AObj: TGocciaObjectValue): TGocciaTemporalDurationValue;
var
  Fields: TDurationFieldsBig;
begin
  Fields := ReadDurationFieldsFromObject(AObj, ZeroDurationFields);
  Result := TGocciaTemporalDurationValue.CreateFromBigIntegers(
    Fields.Years, Fields.Months, Fields.Weeks, Fields.Days, Fields.Hours,
    Fields.Minutes, Fields.Seconds, Fields.Milliseconds, Fields.Microseconds,
    Fields.Nanoseconds);
end;

{ TGocciaTemporalDurationValue }

function BigIntSign(const AValue: TBigInteger): Integer;
begin
  if AValue.IsPositive then
    Result := 1
  else if AValue.IsNegative then
    Result := -1
  else
    Result := 0;
end;

function BigIntToCheckedInt64(const AValue: TBigInteger; const AFieldName: string): Int64;
begin
  if (AValue.Compare(TBigInteger.FromInt64(Low(Int64))) < 0) or
     (AValue.Compare(TBigInteger.FromInt64(High(Int64))) > 0) then
    raise ETemporalDurationInt64Overflow.CreateFmt('Temporal.Duration.%s is outside Int64 range', [AFieldName]);
  Result := AValue.ToInt64;
end;

function BigIntFieldToNumber(const AValue: TBigInteger): TGocciaNumberLiteralValue;
begin
  Result := TGocciaNumberLiteralValue.Create(AValue.ToDouble);
end;

function NumberToBigInteger(const AValue: Double): TBigInteger;
begin
  if IsNaN(AValue) or IsInfinite(AValue) then
    ThrowRangeError('Temporal.Duration field must be a finite integer',
      SSuggestTemporalDurationRange);
  if Frac(AValue) <> 0 then
    ThrowRangeError('Temporal.Duration field must be an integer',
      SSuggestTemporalDurationRange);
  // FromDouble decomposes the IEEE 754 bits, so integers beyond ~17
  // significant decimal digits convert exactly (Str/FormatFloat round).
  Result := TBigInteger.FromDouble(AValue);
end;

function DurationFieldToBigInteger(const AValue: TGocciaValue): TBigInteger;
begin
  if AValue is TGocciaNumberLiteralValue then
    Result := NumberToBigInteger(TGocciaNumberLiteralValue(AValue).Value)
  else
    Result := NumberToBigInteger(AValue.ToNumberLiteral.Value);
end;

function BigIntUnit(const AValue: Int64): TBigInteger; inline;
begin
  Result := TBigInteger.FromInt64(AValue);
end;

function UnitToNanosecondsBig(const AUnit: TTemporalUnit): TBigInteger;
begin
  Result := BigIntUnit(UnitToNanoseconds(AUnit));
end;

function BigRationalToEngineDouble(const ANumerator, ADenominator: TBigInteger): Double;
const
  DECIMAL_PRECISION = 22;
var
  Divisor, Whole, Remainder, Digit: TBigInteger;
  I: Integer;
  DecimalText, FractionText: string;
  Negative: Boolean;
begin
  Negative := ANumerator.IsNegative;
  Divisor := ADenominator.AbsValue;
  Whole := ANumerator.AbsValue.Divide(Divisor);
  Remainder := ANumerator.AbsValue.Modulo(Divisor);
  if Remainder.IsZero then
  begin
    Result := Whole.ToDouble;
    if Negative then
      Result := -Result;
    Exit;
  end;

  FractionText := '';
  for I := 1 to DECIMAL_PRECISION do
  begin
    if Remainder.IsZero then
      FractionText := FractionText + '0'
    else
    begin
      Remainder := Remainder.Multiply(BigIntUnit(10));
      Digit := Remainder.Divide(Divisor);
      FractionText := FractionText + Digit.ToString;
      Remainder := Remainder.Modulo(Divisor);
    end;
  end;

  DecimalText := Whole.ToString;
  if FractionText <> '' then
    DecimalText := DecimalText + '.' + FractionText;

  Result := TGocciaStringLiteralValue.Create(DecimalText).ToNumberLiteral.Value;
  if Negative then
    Result := -Result;
end;

function TotalNanosecondsToUnitDouble(const ATotalNanoseconds: TBigInteger;
  const AUnitNanoseconds: Int64): Double;
begin
  Result := BigRationalToEngineDouble(ATotalNanoseconds,
    BigIntUnit(AUnitNanoseconds));
end;

function DurationSubDayNanosecondsBig(const D: TGocciaTemporalDurationValue): TBigInteger;
begin
  Result := TimeDurationFromComponents(D.FHoursBig, D.FMinutesBig, D.FSecondsBig,
    D.FMillisecondsBig, D.FMicrosecondsBig, D.FNanosecondsBig);
end;

function DurationTotalNanosecondsBig(const D: TGocciaTemporalDurationValue): TBigInteger;
begin
  Result := DurationSubDayNanosecondsBig(D)
    .Add(D.FDaysBig.Multiply(BigIntUnit(NANOSECONDS_PER_DAY)))
    .Add(D.FWeeksBig.Multiply(BigIntUnit(7)).Multiply(BigIntUnit(NANOSECONDS_PER_DAY)));
end;

function IsDurationDigit(const AChar: Char): Boolean; inline;
begin
  Result := (AChar >= '0') and (AChar <= '9');
end;

function ParseUnsignedDurationDigits(const AValue: string; var APosition: Integer;
  out ADigits: string): Boolean;
var
  Start: Integer;
begin
  Start := APosition;
  while (APosition <= Length(AValue)) and IsDurationDigit(AValue[APosition]) do
    Inc(APosition);
  Result := APosition > Start;
  if Result then
    ADigits := Copy(AValue, Start, APosition - Start)
  else
    ADigits := '';
end;

function BigIntPowerOfTen(const AExponent: Integer): TBigInteger;
var
  I: Integer;
begin
  Result := TBigInteger.One;
  for I := 1 to AExponent do
    Result := Result.Multiply(BigIntUnit(10));
end;

function BigIntFromUnsignedDigits(const ADigits: string): TBigInteger;
begin
  Result := TBigInteger.FromDecimalString(ADigits);
end;

function FractionToNanoseconds(const AFractionDigits: string;
  const AUnitNanoseconds: Int64): TBigInteger;
var
  Scaled, Divisor, Quotient, Remainder: TBigInteger;
begin
  Scaled := BigIntFromUnsignedDigits(AFractionDigits)
    .Multiply(BigIntUnit(AUnitNanoseconds));
  Divisor := BigIntPowerOfTen(Length(AFractionDigits));
  Quotient := Scaled.Divide(Divisor);
  Remainder := Scaled.Modulo(Divisor);
  if Remainder.Multiply(BigIntUnit(2)).Compare(Divisor) >= 0 then
    Quotient := Quotient.Add(TBigInteger.One);
  Result := Quotient;
end;

procedure AddFractionalTimeNanoseconds(var AFields: TDurationFieldsBig;
  const ANanoseconds: TBigInteger; const ALargestUnit: TTemporalUnit);
var
  Hours, Minutes, Seconds, Milliseconds, Microseconds, Nanoseconds: TBigInteger;
  ExtraDays: Int64;
begin
  BalanceTimeDurationToFields(ANanoseconds, ALargestUnit, Hours, Minutes,
    Seconds, Milliseconds, Microseconds, Nanoseconds, ExtraDays);
  AFields.Days := AFields.Days.Add(BigIntUnit(ExtraDays));
  AFields.Hours := AFields.Hours.Add(Hours);
  AFields.Minutes := AFields.Minutes.Add(Minutes);
  AFields.Seconds := AFields.Seconds.Add(Seconds);
  AFields.Milliseconds := AFields.Milliseconds.Add(Milliseconds);
  AFields.Microseconds := AFields.Microseconds.Add(Microseconds);
  AFields.Nanoseconds := AFields.Nanoseconds.Add(Nanoseconds);
end;

procedure NegateDurationFields(var AFields: TDurationFieldsBig);
begin
  AFields.Years := AFields.Years.Negate;
  AFields.Months := AFields.Months.Negate;
  AFields.Weeks := AFields.Weeks.Negate;
  AFields.Days := AFields.Days.Negate;
  AFields.Hours := AFields.Hours.Negate;
  AFields.Minutes := AFields.Minutes.Negate;
  AFields.Seconds := AFields.Seconds.Negate;
  AFields.Milliseconds := AFields.Milliseconds.Negate;
  AFields.Microseconds := AFields.Microseconds.Negate;
  AFields.Nanoseconds := AFields.Nanoseconds.Negate;
end;

function TryParseDurationStringFields(const AValue: string;
  out AFields: TDurationFieldsBig): Boolean;
var
  Position, Sign, LastDateOrder, LastTimeOrder, UnitOrder: Integer;
  InTimePart, HasFraction: Boolean;
  WholeDigits, FractionDigits: string;
  UnitChar: Char;
  WholeValue, FractionNanoseconds: TBigInteger;
begin
  Result := False;
  AFields := ZeroDurationFields;
  if Length(AValue) < 2 then
    Exit;

  Position := 1;
  Sign := 1;
  if AValue[Position] = '-' then
  begin
    Sign := -1;
    Inc(Position);
  end
  else if AValue[Position] = '+' then
    Inc(Position);

  if (Position > Length(AValue)) or (UpCase(AValue[Position]) <> 'P') then
    Exit;
  Inc(Position);

  InTimePart := False;
  LastDateOrder := 0;
  LastTimeOrder := 0;
  while Position <= Length(AValue) do
  begin
    if UpCase(AValue[Position]) = 'T' then
    begin
      if InTimePart then
        Exit;
      InTimePart := True;
      Inc(Position);
      if Position > Length(AValue) then
        Exit;
      Continue;
    end;

    if not ParseUnsignedDurationDigits(AValue, Position, WholeDigits) then
      Exit;

    HasFraction := False;
    FractionDigits := '';
    if (Position <= Length(AValue)) and
       ((AValue[Position] = '.') or (AValue[Position] = ',')) then
    begin
      HasFraction := True;
      Inc(Position);
      if not ParseUnsignedDurationDigits(AValue, Position, FractionDigits) then
        Exit;
    end;

    if Position > Length(AValue) then
      Exit;
    UnitChar := UpCase(AValue[Position]);
    Inc(Position);

    WholeValue := BigIntFromUnsignedDigits(WholeDigits);
    if not InTimePart then
    begin
      if HasFraction then
        Exit;
      case UnitChar of
        'Y':
          begin
            UnitOrder := 1;
            AFields.Years := WholeValue;
          end;
        'M':
          begin
            UnitOrder := 2;
            AFields.Months := WholeValue;
          end;
        'W':
          begin
            UnitOrder := 3;
            AFields.Weeks := WholeValue;
          end;
        'D':
          begin
            UnitOrder := 4;
            AFields.Days := WholeValue;
          end;
      else
        Exit;
      end;
      if UnitOrder <= LastDateOrder then
        Exit;
      LastDateOrder := UnitOrder;
    end
    else
    begin
      case UnitChar of
        'H':
          begin
            UnitOrder := 1;
            AFields.Hours := WholeValue;
            if HasFraction then
            begin
              if Position <= Length(AValue) then
                Exit;
              FractionNanoseconds := FractionToNanoseconds(FractionDigits,
                NANOSECONDS_PER_HOUR);
              AddFractionalTimeNanoseconds(AFields, FractionNanoseconds, tuHour);
            end;
          end;
        'M':
          begin
            UnitOrder := 2;
            AFields.Minutes := WholeValue;
            if HasFraction then
            begin
              if Position <= Length(AValue) then
                Exit;
              FractionNanoseconds := FractionToNanoseconds(FractionDigits,
                NANOSECONDS_PER_MINUTE);
              AddFractionalTimeNanoseconds(AFields, FractionNanoseconds, tuMinute);
            end;
          end;
        'S':
          begin
            UnitOrder := 3;
            AFields.Seconds := WholeValue;
            if HasFraction then
            begin
              if Length(FractionDigits) > 9 then
                Exit;
              FractionNanoseconds := FractionToNanoseconds(FractionDigits,
                NANOSECONDS_PER_SECOND);
              AddFractionalTimeNanoseconds(AFields, FractionNanoseconds, tuSecond);
            end;
          end;
      else
        Exit;
      end;
      if UnitOrder <= LastTimeOrder then
        Exit;
      LastTimeOrder := UnitOrder;
    end;
    AFields.HasAny := True;
  end;

  if not AFields.HasAny then
    Exit;
  if Sign < 0 then
    NegateDurationFields(AFields);
  Result := True;
end;

function DurationFromString(const AValue, AErrorMessage: string): TGocciaTemporalDurationValue;
var
  Fields: TDurationFieldsBig;
begin
  if not TryParseDurationStringFields(AValue, Fields) then
    ThrowRangeError(AErrorMessage, SSuggestTemporalDurationArg);
  Result := TGocciaTemporalDurationValue.CreateFromBigIntegers(
    Fields.Years, Fields.Months, Fields.Weeks, Fields.Days, Fields.Hours,
    Fields.Minutes, Fields.Seconds, Fields.Milliseconds, Fields.Microseconds,
    Fields.Nanoseconds);
end;

function DurationBigSign(const AValue: TBigInteger): Integer;
begin
  if AValue.IsPositive then
    Result := 1
  else if AValue.IsNegative then
    Result := -1
  else
    Result := 0;
end;

procedure SplitDurationEpochNanoseconds(const AEpochNanoseconds: TBigInteger;
  out AEpochMilliseconds: Int64; out ASubMillisecondNanoseconds: Integer);
var
  UnitNs, MillisecondsBig, SubMillisecondBig: TBigInteger;
begin
  UnitNs := BigIntUnit(NANOSECONDS_PER_MILLISECOND);
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

procedure DurationLocalComponentsFromEpoch(const AEpochMilliseconds: Int64;
  const ASubMillisecondNanoseconds: Integer; const ATimeZone: string;
  out AYear, AMonth, ADay, AHour, AMinute, ASecond, AMillisecond,
      AMicrosecond, ANanosecond: Integer);
var
  OffsetSeconds: Integer;
  LocalEpochMilliseconds, EpochDays, RemainingMilliseconds: Int64;
  DateRec: TTemporalDateRecord;
begin
  OffsetSeconds := GetUtcOffsetSeconds(ATimeZone,
    AEpochMilliseconds div 1000);
  LocalEpochMilliseconds := AEpochMilliseconds +
    Int64(OffsetSeconds) * 1000;

  EpochDays := LocalEpochMilliseconds div 86400000;
  RemainingMilliseconds := LocalEpochMilliseconds mod 86400000;
  if RemainingMilliseconds < 0 then
  begin
    Dec(EpochDays);
    Inc(RemainingMilliseconds, 86400000);
  end;

  DateRec := EpochDaysToDate(EpochDays);
  AYear := DateRec.Year;
  AMonth := DateRec.Month;
  ADay := DateRec.Day;
  AHour := Integer(RemainingMilliseconds div 3600000);
  RemainingMilliseconds := RemainingMilliseconds mod 3600000;
  AMinute := Integer(RemainingMilliseconds div 60000);
  RemainingMilliseconds := RemainingMilliseconds mod 60000;
  ASecond := Integer(RemainingMilliseconds div 1000);
  AMillisecond := Integer(RemainingMilliseconds mod 1000);
  AMicrosecond := ASubMillisecondNanoseconds div 1000;
  ANanosecond := ASubMillisecondNanoseconds mod 1000;
end;

function DurationZonedBoundaryEpochNanoseconds(
  const ARelativeTo: TDurationRelativeToRecord; const AUnit: TTemporalUnit;
  const AUnitCount: Int64): TBigInteger;
var
  BoundaryDate: TTemporalDateRecord;
  BoundaryEpochMilliseconds: Int64;
begin
  if AUnitCount = 0 then
    Exit(EpochNanosecondsFromParts(ARelativeTo.EpochMilliseconds,
      ARelativeTo.SubMillisecondNanoseconds));

  case AUnit of
    tuYear:
      begin
        if ARelativeTo.CalendarId = 'iso8601' then
          BoundaryDate := AddMonthsToDate(ARelativeTo.Date.Year,
            ARelativeTo.Date.Month, ARelativeTo.Date.Day, AUnitCount * 12)
        else if not TryAddCalendarDate(ARelativeTo.CalendarId,
          ARelativeTo.Date.Year, ARelativeTo.Date.Month, ARelativeTo.Date.Day,
          AUnitCount, 0, 0, 0, True, BoundaryDate) then
          ThrowRangeError('Unable to resolve calendar date for ' +
            ARelativeTo.CalendarId, SSuggestTemporalDateRange);
      end;
    tuMonth:
      begin
        if ARelativeTo.CalendarId = 'iso8601' then
          BoundaryDate := AddMonthsToDate(ARelativeTo.Date.Year,
            ARelativeTo.Date.Month, ARelativeTo.Date.Day, AUnitCount)
        else if not TryAddCalendarDate(ARelativeTo.CalendarId,
          ARelativeTo.Date.Year, ARelativeTo.Date.Month, ARelativeTo.Date.Day,
          0, AUnitCount, 0, 0, True, BoundaryDate) then
          ThrowRangeError('Unable to resolve calendar date for ' +
            ARelativeTo.CalendarId, SSuggestTemporalDateRange);
      end;
    tuWeek:
      BoundaryDate := AddDaysToDate(ARelativeTo.Date.Year,
        ARelativeTo.Date.Month, ARelativeTo.Date.Day, AUnitCount * 7);
    tuDay:
      BoundaryDate := AddDaysToDate(ARelativeTo.Date.Year,
        ARelativeTo.Date.Month, ARelativeTo.Date.Day, AUnitCount);
  else
    BoundaryDate := ARelativeTo.Date;
  end;

  BoundaryEpochMilliseconds := LocalDateTimeToEpochMilliseconds(
    BoundaryDate.Year, BoundaryDate.Month, BoundaryDate.Day,
    ARelativeTo.Hour, ARelativeTo.Minute, ARelativeTo.Second,
    ARelativeTo.Millisecond, ARelativeTo.TimeZone);
  Result := EpochNanosecondsFromParts(BoundaryEpochMilliseconds,
    ARelativeTo.SubMillisecondNanoseconds);
end;

function DurationEstimateWholeZonedUnits(
  const ARelativeTo: TDurationRelativeToRecord; const AEndNanoseconds: TBigInteger;
  const AUnit: TTemporalUnit; const ASign: Integer): Int64;
var
  EndMilliseconds: Int64;
  EndSubMillisecondNanoseconds: Integer;
  EndYear, EndMonth, EndDay, EndHour, EndMinute, EndSecond: Integer;
  EndMillisecond, EndMicrosecond, EndNanosecond: Integer;
  MonthDifference, DayDifference: Int64;
  Boundary, NextBoundary: TBigInteger;
begin
  SplitDurationEpochNanoseconds(AEndNanoseconds, EndMilliseconds,
    EndSubMillisecondNanoseconds);
  DurationLocalComponentsFromEpoch(EndMilliseconds,
    EndSubMillisecondNanoseconds, ARelativeTo.TimeZone, EndYear, EndMonth,
    EndDay, EndHour, EndMinute, EndSecond, EndMillisecond, EndMicrosecond,
    EndNanosecond);

  case AUnit of
    tuYear:
      begin
        MonthDifference := Int64(EndYear - ARelativeTo.Date.Year) * 12 +
          Int64(EndMonth - ARelativeTo.Date.Month);
        Result := MonthDifference div 12;
      end;
    tuMonth:
      Result := Int64(EndYear - ARelativeTo.Date.Year) * 12 +
        Int64(EndMonth - ARelativeTo.Date.Month);
    tuWeek:
      begin
        DayDifference := DateToEpochDays(EndYear, EndMonth, EndDay) -
          DateToEpochDays(ARelativeTo.Date.Year, ARelativeTo.Date.Month,
            ARelativeTo.Date.Day);
        Result := DayDifference div 7;
      end;
    tuDay:
      Result := DateToEpochDays(EndYear, EndMonth, EndDay) -
        DateToEpochDays(ARelativeTo.Date.Year, ARelativeTo.Date.Month,
          ARelativeTo.Date.Day);
  else
    Result := 0;
  end;

  if ASign > 0 then
  begin
    Boundary := DurationZonedBoundaryEpochNanoseconds(ARelativeTo, AUnit,
      Result);
    while Boundary.Compare(AEndNanoseconds) > 0 do
    begin
      Dec(Result);
      Boundary := DurationZonedBoundaryEpochNanoseconds(ARelativeTo, AUnit,
        Result);
    end;
    while Result < High(Int64) do
    begin
      NextBoundary := DurationZonedBoundaryEpochNanoseconds(ARelativeTo,
        AUnit, Result + 1);
      if NextBoundary.Compare(AEndNanoseconds) > 0 then
        Break;
      Inc(Result);
    end;
  end
  else
  begin
    Boundary := DurationZonedBoundaryEpochNanoseconds(ARelativeTo, AUnit,
      Result);
    while Boundary.Compare(AEndNanoseconds) < 0 do
    begin
      Inc(Result);
      Boundary := DurationZonedBoundaryEpochNanoseconds(ARelativeTo, AUnit,
        Result);
    end;
    while Result > Low(Int64) do
    begin
      NextBoundary := DurationZonedBoundaryEpochNanoseconds(ARelativeTo,
        AUnit, Result - 1);
      if NextBoundary.Compare(AEndNanoseconds) < 0 then
        Break;
      Dec(Result);
    end;
  end;
end;

function DurationSelectRoundedCalendarUnits(const AScaledValue: Int64;
  const AIncrement, ASign: Integer; const APosition, APeriod: TBigInteger;
  const AMode: TTemporalRoundingMode): Int64;
var
  AwayFromZero, Comparison: Int64;
  TwicePosition: TBigInteger;
begin
  if APosition.IsZero then
    Exit(AScaledValue);

  AwayFromZero := AScaledValue + Int64(ASign) * AIncrement;
  Result := AScaledValue;
  case AMode of
    rmTrunc:
      Exit(AScaledValue);
    rmExpand:
      Exit(AwayFromZero);
    rmCeil:
      if ASign > 0 then Exit(AwayFromZero) else Exit(AScaledValue);
    rmFloor:
      if ASign < 0 then Exit(AwayFromZero) else Exit(AScaledValue);
  else
    ;
  end;

  TwicePosition := APosition.Multiply(BigIntUnit(2));
  Comparison := TwicePosition.Compare(APeriod);
  case AMode of
    rmHalfExpand:
      if Comparison >= 0 then Result := AwayFromZero;
    rmHalfTrunc:
      if Comparison > 0 then Result := AwayFromZero;
    rmHalfCeil:
      if (Comparison > 0) or ((Comparison = 0) and (ASign > 0)) then
        Result := AwayFromZero;
    rmHalfFloor:
      if (Comparison > 0) or ((Comparison = 0) and (ASign < 0)) then
        Result := AwayFromZero;
    rmHalfEven:
      if (Comparison > 0) or
         ((Comparison = 0) and Odd(AScaledValue div AIncrement)) then
        Result := AwayFromZero;
  end;
end;

function DurationAddZonedDateTime(const D: TGocciaTemporalDurationValue;
  const ARelativeTo: TDurationRelativeToRecord): TBigInteger;
var
  DateRec: TTemporalDateRecord;
  IntermediateEpochMilliseconds: Int64;
begin
  if D.FYearsBig.IsZero and D.FMonthsBig.IsZero and D.FWeeksBig.IsZero and
     D.FDaysBig.IsZero then
  begin
    Result := EpochNanosecondsFromParts(ARelativeTo.EpochMilliseconds,
      ARelativeTo.SubMillisecondNanoseconds)
      .Add(DurationSubDayNanosecondsBig(D));
    if not IsValidEpochNanoseconds(Result) then
      ThrowRangeError(Format(SErrorTemporalInvalidRelativeTo,
        ['Duration relativeTo']), SSuggestTemporalRelativeTo);
    Exit;
  end;

  if not TryAddCalendarDate(ARelativeTo.CalendarId, ARelativeTo.Date.Year,
    ARelativeTo.Date.Month, ARelativeTo.Date.Day,
    BigIntToCheckedInt64(D.FYearsBig, 'years'),
    BigIntToCheckedInt64(D.FMonthsBig, 'months'),
    BigIntToCheckedInt64(D.FWeeksBig, 'weeks'), BigIntToCheckedInt64(D.FDaysBig,
    'days'), True, DateRec) then
    ThrowRangeError(Format(SErrorTemporalInvalidRelativeTo,
      ['Duration relativeTo']), SSuggestTemporalRelativeTo);

  IntermediateEpochMilliseconds := LocalDateTimeToEpochMilliseconds(
    DateRec.Year, DateRec.Month, DateRec.Day, ARelativeTo.Hour,
    ARelativeTo.Minute, ARelativeTo.Second, ARelativeTo.Millisecond,
    ARelativeTo.TimeZone);
  Result := EpochNanosecondsFromParts(IntermediateEpochMilliseconds,
    ARelativeTo.SubMillisecondNanoseconds).Add(DurationSubDayNanosecondsBig(D));
  if not IsValidEpochNanoseconds(Result) then
    ThrowRangeError(Format(SErrorTemporalInvalidRelativeTo,
      ['Duration relativeTo']), SSuggestTemporalRelativeTo);
end;

procedure DurationZonedDifferenceFields(
  const ARelativeTo: TDurationRelativeToRecord; const AEndNanoseconds: TBigInteger;
  const ALargestUnit: TTemporalUnit; out AYears, AMonths, AWeeks, ADays,
      AHours, AMinutes, ASeconds, AMilliseconds, AMicroseconds,
      ANanoseconds: Int64);
var
  StartNanoseconds, IntermediateNanoseconds, RemainderBig: TBigInteger;
  EndEpochMilliseconds, IntermediateEpochMilliseconds, RemainderNanoseconds: Int64;
  EndSubMillisecondNanoseconds: Integer;
  CorrectionSign, RemainderSign: Integer;
  EndYear, EndMonth, EndDay, EndHour, EndMinute, EndSecond: Integer;
  EndMillisecond, EndMicrosecond, EndNanosecond: Integer;
  CandidateDate, IntermediateDate: TTemporalDateRecord;
  DayCorrection, MaxDayCorrection: Integer;
begin
  AYears := 0; AMonths := 0; AWeeks := 0; ADays := 0;
  AHours := 0; AMinutes := 0; ASeconds := 0;
  AMilliseconds := 0; AMicroseconds := 0; ANanoseconds := 0;

  StartNanoseconds := EpochNanosecondsFromParts(ARelativeTo.EpochMilliseconds,
    ARelativeTo.SubMillisecondNanoseconds);
  if AEndNanoseconds.Compare(StartNanoseconds) = 0 then
    Exit;

  if AEndNanoseconds.Compare(StartNanoseconds) < 0 then
    CorrectionSign := 1
  else
    CorrectionSign := -1;
  if CorrectionSign = -1 then
    MaxDayCorrection := 2
  else
    MaxDayCorrection := 1;

  SplitDurationEpochNanoseconds(AEndNanoseconds, EndEpochMilliseconds,
    EndSubMillisecondNanoseconds);
  DurationLocalComponentsFromEpoch(EndEpochMilliseconds,
    EndSubMillisecondNanoseconds, ARelativeTo.TimeZone, EndYear, EndMonth,
    EndDay, EndHour, EndMinute, EndSecond, EndMillisecond, EndMicrosecond,
    EndNanosecond);

  if CompareDates(ARelativeTo.Date.Year, ARelativeTo.Date.Month,
    ARelativeTo.Date.Day, EndYear, EndMonth, EndDay) = 0 then
  begin
    RemainderBig := AEndNanoseconds.Subtract(StartNanoseconds);
    RemainderSign := DurationBigSign(RemainderBig);
    RemainderNanoseconds := RemainderBig.AbsValue.ToInt64;
    AHours := RemainderSign * (RemainderNanoseconds div NANOSECONDS_PER_HOUR);
    RemainderNanoseconds := RemainderNanoseconds mod NANOSECONDS_PER_HOUR;
    AMinutes := RemainderSign * (RemainderNanoseconds div NANOSECONDS_PER_MINUTE);
    RemainderNanoseconds := RemainderNanoseconds mod NANOSECONDS_PER_MINUTE;
    ASeconds := RemainderSign * (RemainderNanoseconds div NANOSECONDS_PER_SECOND);
    RemainderNanoseconds := RemainderNanoseconds mod NANOSECONDS_PER_SECOND;
    AMilliseconds := RemainderSign * (RemainderNanoseconds div NANOSECONDS_PER_MILLISECOND);
    RemainderNanoseconds := RemainderNanoseconds mod NANOSECONDS_PER_MILLISECOND;
    AMicroseconds := RemainderSign * (RemainderNanoseconds div NANOSECONDS_PER_MICROSECOND);
    ANanoseconds := RemainderSign * (RemainderNanoseconds mod NANOSECONDS_PER_MICROSECOND);
    Exit;
  end;

  DayCorrection := 0;
  if CompareTimes(EndHour, EndMinute, EndSecond, EndMillisecond,
    EndMicrosecond, EndNanosecond, ARelativeTo.Hour, ARelativeTo.Minute,
    ARelativeTo.Second, ARelativeTo.Millisecond, ARelativeTo.Microsecond,
    ARelativeTo.Nanosecond) = CorrectionSign then
    Inc(DayCorrection);

  RemainderBig := TBigInteger.Zero;
  IntermediateDate.Year := EndYear;
  IntermediateDate.Month := EndMonth;
  IntermediateDate.Day := EndDay;
  while DayCorrection <= MaxDayCorrection do
  begin
    CandidateDate := AddDaysToDate(EndYear, EndMonth, EndDay,
      Int64(DayCorrection) * CorrectionSign);
    IntermediateEpochMilliseconds := LocalDateTimeToEpochMilliseconds(
      CandidateDate.Year, CandidateDate.Month, CandidateDate.Day,
      ARelativeTo.Hour, ARelativeTo.Minute, ARelativeTo.Second,
      ARelativeTo.Millisecond, ARelativeTo.TimeZone);
    IntermediateNanoseconds := EpochNanosecondsFromParts(
      IntermediateEpochMilliseconds, ARelativeTo.SubMillisecondNanoseconds);
    RemainderBig := AEndNanoseconds.Subtract(IntermediateNanoseconds);
    if RemainderBig.IsZero then
    begin
      IntermediateDate := CandidateDate;
      Break;
    end;
    RemainderSign := DurationBigSign(RemainderBig);
    if RemainderSign <> CorrectionSign then
    begin
      IntermediateDate := CandidateDate;
      Break;
    end;
    Inc(DayCorrection);
  end;

  CalendarDateUntil(ARelativeTo.CalendarId, ARelativeTo.Date.Year,
    ARelativeTo.Date.Month, ARelativeTo.Date.Day, IntermediateDate.Year,
    IntermediateDate.Month, IntermediateDate.Day, ALargestUnit, AYears,
    AMonths, AWeeks, ADays);
  if RemainderBig.IsZero then
    Exit;

  RemainderSign := DurationBigSign(RemainderBig);
  RemainderNanoseconds := RemainderBig.AbsValue.ToInt64;
  AHours := RemainderSign * (RemainderNanoseconds div NANOSECONDS_PER_HOUR);
  RemainderNanoseconds := RemainderNanoseconds mod NANOSECONDS_PER_HOUR;
  AMinutes := RemainderSign * (RemainderNanoseconds div NANOSECONDS_PER_MINUTE);
  RemainderNanoseconds := RemainderNanoseconds mod NANOSECONDS_PER_MINUTE;
  ASeconds := RemainderSign * (RemainderNanoseconds div NANOSECONDS_PER_SECOND);
  RemainderNanoseconds := RemainderNanoseconds mod NANOSECONDS_PER_SECOND;
  AMilliseconds := RemainderSign * (RemainderNanoseconds div NANOSECONDS_PER_MILLISECOND);
  RemainderNanoseconds := RemainderNanoseconds mod NANOSECONDS_PER_MILLISECOND;
  AMicroseconds := RemainderSign * (RemainderNanoseconds div NANOSECONDS_PER_MICROSECOND);
  ANanoseconds := RemainderSign * (RemainderNanoseconds mod NANOSECONDS_PER_MICROSECOND);
end;

function DurationTotalZoned(const D: TGocciaTemporalDurationValue;
  const ARelativeTo: TDurationRelativeToRecord; const AUnit: TTemporalUnit): Double;
var
  StartNanoseconds, EndNanoseconds, BoundaryNanoseconds, NextBoundaryNanoseconds: TBigInteger;
  DifferenceNanoseconds, PositionNanoseconds, PeriodNanoseconds: TBigInteger;
  Sign: Integer;
  WholeUnits: Int64;
begin
  StartNanoseconds := EpochNanosecondsFromParts(ARelativeTo.EpochMilliseconds,
    ARelativeTo.SubMillisecondNanoseconds);
  EndNanoseconds := DurationAddZonedDateTime(D, ARelativeTo);
  DifferenceNanoseconds := EndNanoseconds.Subtract(StartNanoseconds);

  if AUnit in [tuHour, tuMinute, tuSecond, tuMillisecond, tuMicrosecond,
    tuNanosecond] then
  begin
    if AUnit = tuNanosecond then
      Exit(DifferenceNanoseconds.ToDouble);
    Exit(TotalNanosecondsToUnitDouble(DifferenceNanoseconds,
      UnitToNanoseconds(AUnit)));
  end;

  Sign := DurationBigSign(DifferenceNanoseconds);
  if Sign = 0 then
    Exit(0);

  WholeUnits := DurationEstimateWholeZonedUnits(ARelativeTo, EndNanoseconds,
    AUnit, Sign);
  BoundaryNanoseconds := DurationZonedBoundaryEpochNanoseconds(ARelativeTo,
    AUnit, WholeUnits);
  if BoundaryNanoseconds.Compare(EndNanoseconds) = 0 then
    Exit(WholeUnits);

  NextBoundaryNanoseconds := DurationZonedBoundaryEpochNanoseconds(ARelativeTo,
    AUnit, WholeUnits + Sign);
  PositionNanoseconds := EndNanoseconds.Subtract(BoundaryNanoseconds);
  PeriodNanoseconds := NextBoundaryNanoseconds.Subtract(BoundaryNanoseconds).AbsValue;
  Result := WholeUnits + PositionNanoseconds.ToDouble /
    PeriodNanoseconds.ToDouble;
end;

function CreateDurationFromZonedFields(const AYears, AMonths, AWeeks, ADays,
  AHours, AMinutes, ASeconds, AMilliseconds, AMicroseconds,
  ANanoseconds: Int64): TGocciaTemporalDurationValue;
begin
  Result := TGocciaTemporalDurationValue.Create(AYears, AMonths, AWeeks,
    ADays, AHours, AMinutes, ASeconds, AMilliseconds, AMicroseconds,
    ANanoseconds);
end;

function DurationDateRelativeTimeNudge(const D: TGocciaTemporalDurationValue;
  const ARelativeTo: TDurationRelativeToRecord; const ALargestUnit,
  ASmallestUnit: TTemporalUnit; const AMode: TTemporalRoundingMode;
  const AIncrement: Integer): TGocciaTemporalDurationValue;
var
  Sign, BeyondSign, DayDelta: Integer;
  DateRec, NextDay: TTemporalDateRecord;
  StartEpochMilliseconds, NextEpochMilliseconds: Int64;
  EndNanoseconds, StartNanoseconds, DaySpan, TimeDuration, RoundedTime,
  BeyondDaySpan: TBigInteger;
  Divisor: TBigInteger;
  Years, Months, Weeks, Days, HoursInt, MinutesInt, SecondsInt,
  MillisecondsInt, MicrosecondsInt, NanosecondsInt: Int64;
  Hours, Minutes, Seconds, Milliseconds, Microseconds, Nanoseconds: TBigInteger;
  ExtraDays: Int64;
begin
  StartNanoseconds := EpochNanosecondsFromParts(ARelativeTo.EpochMilliseconds,
    ARelativeTo.SubMillisecondNanoseconds);
  EndNanoseconds := DurationAddZonedDateTime(D, ARelativeTo);
  Sign := DurationBigSign(EndNanoseconds.Subtract(StartNanoseconds));
  if Sign = 0 then
    Exit(TGocciaTemporalDurationValue.Create(0, 0, 0, 0, 0, 0, 0, 0, 0, 0));

  DurationZonedDifferenceFields(ARelativeTo, EndNanoseconds, ALargestUnit,
    Years, Months, Weeks, Days, HoursInt, MinutesInt, SecondsInt,
    MillisecondsInt, MicrosecondsInt, NanosecondsInt);

  if not TryAddCalendarDate(ARelativeTo.CalendarId, ARelativeTo.Date.Year,
    ARelativeTo.Date.Month, ARelativeTo.Date.Day, Years, Months, Weeks, Days,
    True, DateRec) then
    ThrowRangeError(Format(SErrorTemporalInvalidRelativeTo,
      ['Duration.prototype.round']), SSuggestTemporalRelativeTo);

  StartEpochMilliseconds := LocalDateTimeToEpochMilliseconds(DateRec.Year,
    DateRec.Month, DateRec.Day, ARelativeTo.Hour, ARelativeTo.Minute,
    ARelativeTo.Second, ARelativeTo.Millisecond, ARelativeTo.TimeZone);
  StartNanoseconds := EpochNanosecondsFromParts(StartEpochMilliseconds,
    ARelativeTo.SubMillisecondNanoseconds);

  NextDay := AddDaysToDate(DateRec.Year, DateRec.Month, DateRec.Day, Sign);
  NextEpochMilliseconds := LocalDateTimeToEpochMilliseconds(NextDay.Year,
    NextDay.Month, NextDay.Day, ARelativeTo.Hour, ARelativeTo.Minute,
    ARelativeTo.Second, ARelativeTo.Millisecond, ARelativeTo.TimeZone);
  DaySpan := EpochNanosecondsFromParts(NextEpochMilliseconds,
    ARelativeTo.SubMillisecondNanoseconds).Subtract(StartNanoseconds);

  TimeDuration := TimeDurationFromComponents(BigIntUnit(HoursInt),
    BigIntUnit(MinutesInt), BigIntUnit(SecondsInt),
    BigIntUnit(MillisecondsInt), BigIntUnit(MicrosecondsInt),
    BigIntUnit(NanosecondsInt));
  Divisor := RoundingDivisorBig(ASmallestUnit, AIncrement);
  RoundedTime := RoundTimeDurationToIncrement(TimeDuration, Divisor, AMode);
  BeyondDaySpan := RoundedTime.Subtract(DaySpan);
  BeyondSign := DurationBigSign(BeyondDaySpan);

  if BeyondSign <> -Sign then
  begin
    DayDelta := Sign;
    RoundedTime := RoundTimeDurationToIncrement(BeyondDaySpan, Divisor, AMode);
  end
  else
    DayDelta := 0;

  BalanceTimeDurationToFields(RoundedTime, tuHour, Hours, Minutes, Seconds,
    Milliseconds, Microseconds, Nanoseconds, ExtraDays);

  Days := Days + DayDelta + ExtraDays;

  Result := TGocciaTemporalDurationValue.CreateFromBigIntegers(
    BigIntUnit(Years), BigIntUnit(Months), BigIntUnit(Weeks), BigIntUnit(Days),
    RoundBigToFloat64(Hours), RoundBigToFloat64(Minutes),
    RoundBigToFloat64(Seconds), RoundBigToFloat64(Milliseconds),
    RoundBigToFloat64(Microseconds), RoundBigToFloat64(Nanoseconds));
end;

function RoundZonedDuration(const D: TGocciaTemporalDurationValue;
  const ARelativeTo: TDurationRelativeToRecord; const ALargestUnit,
  ASmallestUnit: TTemporalUnit; const AMode: TTemporalRoundingMode;
  const AIncrement: Integer): TGocciaTemporalDurationValue;
var
  StartNanoseconds, EndNanoseconds, DifferenceNanoseconds, RoundedEndNanoseconds: TBigInteger;
  IncrementNanoseconds, BoundaryNanoseconds, NextBoundaryNanoseconds: TBigInteger;
  PositionNanoseconds, PeriodNanoseconds: TBigInteger;
  Sign: Integer;
  WholeUnits, ScaledUnits, RoundedUnits, LargestWholeUnits: Int64;
  Years, Months, Weeks, Days, Hours, Minutes, Seconds, Milliseconds,
  Microseconds, Nanoseconds: Int64;
  HoursBig, MinutesBig, SecondsBig, MillisecondsBig, MicrosecondsBig,
  NanosecondsBig: TBigInteger;
begin
  StartNanoseconds := EpochNanosecondsFromParts(ARelativeTo.EpochMilliseconds,
    ARelativeTo.SubMillisecondNanoseconds);
  EndNanoseconds := DurationAddZonedDateTime(D, ARelativeTo);
  DifferenceNanoseconds := EndNanoseconds.Subtract(StartNanoseconds);
  Sign := DurationBigSign(DifferenceNanoseconds);
  if Sign = 0 then
    Exit(TGocciaTemporalDurationValue.Create(0, 0, 0, 0, 0, 0, 0, 0, 0, 0));

  if ALargestUnit in [tuHour, tuMinute, tuSecond, tuMillisecond,
    tuMicrosecond, tuNanosecond] then
  begin
    IncrementNanoseconds := UnitToNanosecondsBig(ASmallestUnit)
      .Multiply(BigIntUnit(AIncrement));
    RoundedEndNanoseconds := StartNanoseconds.Add(
      RoundTimeDurationToIncrement(DifferenceNanoseconds,
        IncrementNanoseconds, AMode));
    DifferenceNanoseconds := RoundedEndNanoseconds.Subtract(StartNanoseconds);
    BalanceTimeDurationToFields(DifferenceNanoseconds, ALargestUnit,
      HoursBig, MinutesBig, SecondsBig, MillisecondsBig, MicrosecondsBig,
      NanosecondsBig, Days);
    Exit(TGocciaTemporalDurationValue.CreateFromBigIntegers(
      TBigInteger.Zero, TBigInteger.Zero, TBigInteger.Zero,
      BigIntUnit(Days), RoundBigToFloat64(HoursBig),
      RoundBigToFloat64(MinutesBig), RoundBigToFloat64(SecondsBig),
      RoundBigToFloat64(MillisecondsBig), RoundBigToFloat64(MicrosecondsBig),
      RoundBigToFloat64(NanosecondsBig)));
  end;

  if ASmallestUnit in [tuHour, tuMinute, tuSecond, tuMillisecond,
    tuMicrosecond, tuNanosecond] then
    Exit(DurationDateRelativeTimeNudge(D, ARelativeTo, ALargestUnit,
      ASmallestUnit, AMode, AIncrement));

  if ALargestUnit <> ASmallestUnit then
  begin
    LargestWholeUnits := DurationEstimateWholeZonedUnits(ARelativeTo,
      EndNanoseconds, ALargestUnit, Sign);
    BoundaryNanoseconds := DurationZonedBoundaryEpochNanoseconds(ARelativeTo,
      ALargestUnit, LargestWholeUnits);
    if EndNanoseconds.Compare(BoundaryNanoseconds) = 0 then
    begin
      DurationZonedDifferenceFields(ARelativeTo, EndNanoseconds,
        ALargestUnit, Years, Months, Weeks, Days, Hours, Minutes, Seconds,
        Milliseconds, Microseconds, Nanoseconds);
      if ASmallestUnit = tuWeek then
      begin
        Weeks := Weeks + (Days div 7);
        Days := 0;
      end;
      Exit(CreateDurationFromZonedFields(Years, Months, Weeks, Days, Hours,
        Minutes, Seconds, Milliseconds, Microseconds, Nanoseconds));
    end;
  end;

  WholeUnits := DurationEstimateWholeZonedUnits(ARelativeTo, EndNanoseconds,
    ASmallestUnit, Sign);
  if Sign > 0 then
    ScaledUnits := (WholeUnits div AIncrement) * AIncrement
  else
    ScaledUnits := -(((-WholeUnits) div AIncrement) * AIncrement);

  BoundaryNanoseconds := DurationZonedBoundaryEpochNanoseconds(ARelativeTo,
    ASmallestUnit, ScaledUnits);
  if EndNanoseconds.Compare(BoundaryNanoseconds) = 0 then
    RoundedUnits := ScaledUnits
  else
  begin
  NextBoundaryNanoseconds := DurationZonedBoundaryEpochNanoseconds(ARelativeTo,
    ASmallestUnit, ScaledUnits + Int64(Sign) * AIncrement);
  PositionNanoseconds := EndNanoseconds.Subtract(BoundaryNanoseconds).AbsValue;
  PeriodNanoseconds := NextBoundaryNanoseconds.Subtract(BoundaryNanoseconds).AbsValue;
  RoundedUnits := DurationSelectRoundedCalendarUnits(ScaledUnits, AIncrement,
    Sign, PositionNanoseconds, PeriodNanoseconds, AMode);
  end;

  RoundedEndNanoseconds := DurationZonedBoundaryEpochNanoseconds(ARelativeTo,
    ASmallestUnit, RoundedUnits);
  if ALargestUnit = ASmallestUnit then
  begin
    case ASmallestUnit of
      tuYear:
        Exit(CreateDurationFromZonedFields(RoundedUnits, 0, 0, 0, 0, 0, 0, 0, 0, 0));
      tuMonth:
        Exit(CreateDurationFromZonedFields(0, RoundedUnits, 0, 0, 0, 0, 0, 0, 0, 0));
      tuWeek:
        Exit(CreateDurationFromZonedFields(0, 0, RoundedUnits, 0, 0, 0, 0, 0, 0, 0));
      tuDay:
        Exit(CreateDurationFromZonedFields(0, 0, 0, RoundedUnits, 0, 0, 0, 0, 0, 0));
    else
      ;
    end;
  end;

  DurationZonedDifferenceFields(ARelativeTo, RoundedEndNanoseconds,
    ALargestUnit, Years, Months, Weeks, Days, Hours, Minutes, Seconds,
    Milliseconds, Microseconds, Nanoseconds);
  if ASmallestUnit = tuWeek then
  begin
    Weeks := Weeks + (Days div 7);
    Days := 0;
  end;
  Result := CreateDurationFromZonedFields(Years, Months, Weeks, Days, Hours,
    Minutes, Seconds, Milliseconds, Microseconds, Nanoseconds);
end;

// TC39 Temporal §7.5.4 DefaultTemporalLargestUnit: the unit of the largest
// non-zero component, defaulting to nanosecond for the zero duration.
function DurationDefaultLargestUnit(const D: TGocciaTemporalDurationValue): TTemporalUnit;
begin
  if not D.FYearsBig.IsZero then Exit(tuYear);
  if not D.FMonthsBig.IsZero then Exit(tuMonth);
  if not D.FWeeksBig.IsZero then Exit(tuWeek);
  if not D.FDaysBig.IsZero then Exit(tuDay);
  if not D.FHoursBig.IsZero then Exit(tuHour);
  if not D.FMinutesBig.IsZero then Exit(tuMinute);
  if not D.FSecondsBig.IsZero then Exit(tuSecond);
  if not D.FMillisecondsBig.IsZero then Exit(tuMillisecond);
  if not D.FMicrosecondsBig.IsZero then Exit(tuMicrosecond);
  Result := tuNanosecond;
end;

function IsDurationCalendarUnit(const AUnit: TTemporalUnit): Boolean; inline;
begin
  Result := AUnit in [tuYear, tuMonth, tuWeek];
end;

function DurationHasCalendarUnits(const D: TGocciaTemporalDurationValue): Boolean; inline;
begin
  Result := (not D.FYearsBig.IsZero) or
            (not D.FMonthsBig.IsZero) or
            (not D.FWeeksBig.IsZero);
end;

function DurationHasDateUnits(const D: TGocciaTemporalDurationValue): Boolean; inline;
begin
  Result := DurationHasCalendarUnits(D) or (not D.FDaysBig.IsZero);
end;

function DurationFieldsIdentical(const AOne, ATwo: TGocciaTemporalDurationValue): Boolean; inline;
begin
  Result := (AOne.FYearsBig.Compare(ATwo.FYearsBig) = 0) and
            (AOne.FMonthsBig.Compare(ATwo.FMonthsBig) = 0) and
            (AOne.FWeeksBig.Compare(ATwo.FWeeksBig) = 0) and
            (AOne.FDaysBig.Compare(ATwo.FDaysBig) = 0) and
            (AOne.FHoursBig.Compare(ATwo.FHoursBig) = 0) and
            (AOne.FMinutesBig.Compare(ATwo.FMinutesBig) = 0) and
            (AOne.FSecondsBig.Compare(ATwo.FSecondsBig) = 0) and
            (AOne.FMillisecondsBig.Compare(ATwo.FMillisecondsBig) = 0) and
            (AOne.FMicrosecondsBig.Compare(ATwo.FMicrosecondsBig) = 0) and
            (AOne.FNanosecondsBig.Compare(ATwo.FNanosecondsBig) = 0);
end;

function IsDurationDateUnit(const AUnit: TTemporalUnit): Boolean; inline;
begin
  Result := AUnit in [tuYear, tuMonth, tuWeek, tuDay];
end;

function DurationDateBefore(const AY, AM, AD, ABY, ABM, ABD: Integer): Boolean; forward;
function DurationDateInTemporalRange(const ADate: TTemporalDateRecord): Boolean; forward;
function DurationDateAtLowerZonedLimit(const ADate: TTemporalDateRecord): Boolean; forward;
function DurationDateAtUpperLimit(const ADate: TTemporalDateRecord): Boolean; forward;
function IsDurationZonedRelativeToValue(const AValue: TGocciaValue): Boolean; forward;

// TC39 Temporal §7.5.26 TemporalDurationFromInternal step 12: each balanced
// component is converted to a float64 Number before CreateTemporalDuration,
// so values that round up past a validation boundary must reject (test262
// Duration/prototype/add/result-out-of-range-3.js).
function RoundBigToFloat64(const AValue: TBigInteger): TBigInteger;
begin
  Result := TBigInteger.FromDouble(AValue.ToDouble);
end;

// TC39 Temporal §7.5.24 AddDurations: reject calendar units, sum the exact
// time durations, then rebalance to the larger of both default largest units.
// ASign is +1 for add and -1 for subtract (subtract adds the negation).
function AddDurationsCommon(const D, AOther: TGocciaTemporalDurationValue;
  const ASign: Integer): TGocciaTemporalDurationValue;
var
  LargestUnit, OtherLargest: TTemporalUnit;
  TotalNs, H, Mi, S, Ms, Us, Ns: TBigInteger;
  Days: Int64;
begin
  LargestUnit := DurationDefaultLargestUnit(D);
  OtherLargest := DurationDefaultLargestUnit(AOther);
  if Ord(OtherLargest) < Ord(LargestUnit) then
    LargestUnit := OtherLargest;
  if LargestUnit in [tuYear, tuMonth, tuWeek] then
    ThrowRangeError(SErrorDurationCalendarUnitsArith, SSuggestTemporalDurationArg);

  TotalNs := DurationTotalNanosecondsBig(D);
  if ASign >= 0 then
    TotalNs := TotalNs.Add(DurationTotalNanosecondsBig(AOther))
  else
    TotalNs := TotalNs.Subtract(DurationTotalNanosecondsBig(AOther));

  // §7.5.23 AddTimeDuration step 3: combined time beyond ±(2^53 s − 1 ns)
  if not IsValidTimeDuration(TotalNs) then
    ThrowRangeError(SErrorDurationTimeOutOfRange, SSuggestTemporalDurationRange);

  BalanceTimeDurationToFields(TotalNs, LargestUnit, H, Mi, S, Ms, Us, Ns, Days);
  Result := TGocciaTemporalDurationValue.CreateFromBigIntegers(
    TBigInteger.Zero, TBigInteger.Zero, TBigInteger.Zero,
    TBigInteger.FromInt64(Days),
    RoundBigToFloat64(H), RoundBigToFloat64(Mi), RoundBigToFloat64(S),
    RoundBigToFloat64(Ms), RoundBigToFloat64(Us), RoundBigToFloat64(Ns));
end;

function RoundingDivisorBig(const ASmallestUnit: TTemporalUnit; const AIncrement: Integer): TBigInteger;
begin
  if ASmallestUnit = tuWeek then
    Result := BigIntUnit(NANOSECONDS_PER_DAY).Multiply(BigIntUnit(7)).Multiply(BigIntUnit(AIncrement))
  else
    Result := UnitToNanosecondsBig(ASmallestUnit).Multiply(BigIntUnit(AIncrement));
end;

constructor TGocciaTemporalDurationValue.Create(const AYears, AMonths, AWeeks, ADays, AHours, AMinutes, ASeconds,
  AMilliseconds, AMicroseconds, ANanoseconds: Int64);
begin
  inherited Create(nil);
  SetDurationFields(
    TBigInteger.FromInt64(AYears),
    TBigInteger.FromInt64(AMonths),
    TBigInteger.FromInt64(AWeeks),
    TBigInteger.FromInt64(ADays),
    TBigInteger.FromInt64(AHours),
    TBigInteger.FromInt64(AMinutes),
    TBigInteger.FromInt64(ASeconds),
    TBigInteger.FromInt64(AMilliseconds),
    TBigInteger.FromInt64(AMicroseconds),
    TBigInteger.FromInt64(ANanoseconds));
end;

constructor TGocciaTemporalDurationValue.CreateFromBigIntegers(const AYears, AMonths, AWeeks, ADays,
  AHours, AMinutes, ASeconds, AMilliseconds, AMicroseconds, ANanoseconds: TBigInteger);
begin
  inherited Create(nil);
  SetDurationFields(AYears, AMonths, AWeeks, ADays, AHours, AMinutes, ASeconds,
    AMilliseconds, AMicroseconds, ANanoseconds);
end;

procedure TGocciaTemporalDurationValue.SetDurationFields(const AYears, AMonths, AWeeks, ADays,
  AHours, AMinutes, ASeconds, AMilliseconds, AMicroseconds, ANanoseconds: TBigInteger);
var
  HasPositive, HasNegative: Boolean;
  TimeDuration, NormalizedNanoseconds: TBigInteger;
begin
  FYearsBig := AYears;
  FMonthsBig := AMonths;
  FWeeksBig := AWeeks;
  FDaysBig := ADays;
  FHoursBig := AHours;
  FMinutesBig := AMinutes;
  FSecondsBig := ASeconds;
  FMillisecondsBig := AMilliseconds;
  FMicrosecondsBig := AMicroseconds;
  FNanosecondsBig := ANanoseconds;

  // Validate: sign of non-zero components must be uniform
  HasPositive := (BigIntSign(AYears) > 0) or (BigIntSign(AMonths) > 0) or
                 (BigIntSign(AWeeks) > 0) or (BigIntSign(ADays) > 0) or
                 (BigIntSign(AHours) > 0) or (BigIntSign(AMinutes) > 0) or
                 (BigIntSign(ASeconds) > 0) or (BigIntSign(AMilliseconds) > 0) or
                 (BigIntSign(AMicroseconds) > 0) or (BigIntSign(ANanoseconds) > 0);
  HasNegative := (BigIntSign(AYears) < 0) or (BigIntSign(AMonths) < 0) or
                 (BigIntSign(AWeeks) < 0) or (BigIntSign(ADays) < 0) or
                 (BigIntSign(AHours) < 0) or (BigIntSign(AMinutes) < 0) or
                 (BigIntSign(ASeconds) < 0) or (BigIntSign(AMilliseconds) < 0) or
                 (BigIntSign(AMicroseconds) < 0) or (BigIntSign(ANanoseconds) < 0);
  if HasPositive and HasNegative then
    ThrowRangeError(SErrorDurationMixedSigns, SSuggestTemporalDurationSigns);

  // TC39 Temporal §7.5.16 IsValidDuration step 3-5
  if (AYears.AbsValue.Compare(TBigInteger.FromInt64(UINT32_MODULUS)) >= 0) or
     (AMonths.AbsValue.Compare(TBigInteger.FromInt64(UINT32_MODULUS)) >= 0) or
     (AWeeks.AbsValue.Compare(TBigInteger.FromInt64(UINT32_MODULUS)) >= 0) then
    ThrowRangeError(SErrorDurationCalendarOutOfRange, SSuggestTemporalDurationRange);

  // TC39 Temporal §7.5.16 IsValidDuration step 6-8
  TimeDuration := TimeDurationFromComponents(AHours, AMinutes, ASeconds,
    AMilliseconds, AMicroseconds, ANanoseconds);
  NormalizedNanoseconds := TimeDuration.Add(ADays.Multiply(TBigInteger.FromInt64(NANOSECONDS_PER_DAY)));
  if not IsValidTimeDuration(NormalizedNanoseconds) then
    ThrowRangeError(SErrorDurationTimeOutOfRange, SSuggestTemporalDurationRange);

  InitializePrototype;
  if Assigned(GetTemporalDurationShared) then
    FPrototype := GetTemporalDurationShared.Prototype;
end;

function TGocciaTemporalDurationValue.GetYearsInt64: Int64;
begin
  Result := BigIntToCheckedInt64(FYearsBig, 'years');
end;

function TGocciaTemporalDurationValue.GetMonthsInt64: Int64;
begin
  Result := BigIntToCheckedInt64(FMonthsBig, 'months');
end;

function TGocciaTemporalDurationValue.GetWeeksInt64: Int64;
begin
  Result := BigIntToCheckedInt64(FWeeksBig, 'weeks');
end;

function TGocciaTemporalDurationValue.GetDaysInt64: Int64;
begin
  Result := BigIntToCheckedInt64(FDaysBig, 'days');
end;

function TGocciaTemporalDurationValue.GetHoursInt64: Int64;
begin
  Result := BigIntToCheckedInt64(FHoursBig, 'hours');
end;

function TGocciaTemporalDurationValue.GetMinutesInt64: Int64;
begin
  Result := BigIntToCheckedInt64(FMinutesBig, 'minutes');
end;

function TGocciaTemporalDurationValue.GetSecondsInt64: Int64;
begin
  Result := BigIntToCheckedInt64(FSecondsBig, 'seconds');
end;

function TGocciaTemporalDurationValue.GetMillisecondsInt64: Int64;
begin
  Result := BigIntToCheckedInt64(FMillisecondsBig, 'milliseconds');
end;

function TGocciaTemporalDurationValue.GetMicrosecondsInt64: Int64;
begin
  Result := BigIntToCheckedInt64(FMicrosecondsBig, 'microseconds');
end;

function TGocciaTemporalDurationValue.GetNanosecondsInt64: Int64;
begin
  Result := BigIntToCheckedInt64(FNanosecondsBig, 'nanoseconds');
end;

procedure TGocciaTemporalDurationValue.InitializePrototype;
var
  Members: TGocciaMemberCollection;
  Shared: TGocciaSharedPrototype;
begin
  if not Assigned(CurrentRealm) then Exit;
  if Assigned(GetTemporalDurationShared) then Exit;

  Shared := TGocciaSharedPrototype.Create(Self);
  CurrentRealm.SetOwnedSlot(GTemporalDurationSharedSlot, Shared);
  if Length(FPrototypeMembers) = 0 then
  begin
    Members := TGocciaMemberCollection.Create;
    try
      Members.AddAccessor('years', GetYears, nil, [pfConfigurable]);
      Members.AddAccessor('months', GetMonths, nil, [pfConfigurable]);
      Members.AddAccessor('weeks', GetWeeks, nil, [pfConfigurable]);
      Members.AddAccessor('days', GetDays, nil, [pfConfigurable]);
      Members.AddAccessor('hours', GetHours, nil, [pfConfigurable]);
      Members.AddAccessor('minutes', GetMinutes, nil, [pfConfigurable]);
      Members.AddAccessor('seconds', GetSeconds, nil, [pfConfigurable]);
      Members.AddAccessor('milliseconds', GetMilliseconds, nil, [pfConfigurable]);
      Members.AddAccessor('microseconds', GetMicroseconds, nil, [pfConfigurable]);
      Members.AddAccessor('nanoseconds', GetNanoseconds, nil, [pfConfigurable]);
      Members.AddAccessor('sign', GetSign, nil, [pfConfigurable]);
      Members.AddAccessor('blank', GetBlank, nil, [pfConfigurable]);
      Members.AddMethod(DurationNegated, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(DurationAbs, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(DurationAdd, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(DurationSubtract, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(DurationWith, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(DurationRound, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(DurationTotal, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(DurationToString, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(DurationToJSON, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(DurationValueOf, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(DurationToLocaleString, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddSymbolDataProperty(
        TGocciaSymbolValue.WellKnownToStringTag,
        TGocciaStringLiteralValue.Create('Temporal.Duration'),
        [pfConfigurable]);
      FPrototypeMembers := Members.ToDefinitions;
    finally
      Members.Free;
    end;
  end;
  RegisterMemberDefinitions(Shared.Prototype, FPrototypeMembers);
end;

class procedure TGocciaTemporalDurationValue.ExposePrototype(const AConstructor: TGocciaObjectValue);
var
  Shared: TGocciaSharedPrototype;
begin
  Shared := GetTemporalDurationShared;
  if not Assigned(Shared) then
  begin
    TGocciaTemporalDurationValue.Create(0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
    Shared := GetTemporalDurationShared;
  end;
  if Assigned(Shared) then
    ExposeSharedPrototypeOnConstructor(Shared, AConstructor);
end;

function TGocciaTemporalDurationValue.ComputeSign: Integer;
begin
  if (FYearsBig.IsPositive) or (FMonthsBig.IsPositive) or (FWeeksBig.IsPositive) or
     (FDaysBig.IsPositive) or (FHoursBig.IsPositive) or (FMinutesBig.IsPositive) or
     (FSecondsBig.IsPositive) or (FMillisecondsBig.IsPositive) or
     (FMicrosecondsBig.IsPositive) or (FNanosecondsBig.IsPositive) then
    Result := 1
  else if (FYearsBig.IsNegative) or (FMonthsBig.IsNegative) or (FWeeksBig.IsNegative) or
          (FDaysBig.IsNegative) or (FHoursBig.IsNegative) or (FMinutesBig.IsNegative) or
          (FSecondsBig.IsNegative) or (FMillisecondsBig.IsNegative) or
          (FMicrosecondsBig.IsNegative) or (FNanosecondsBig.IsNegative) then
    Result := -1
  else
    Result := 0;
end;

function TGocciaTemporalDurationValue.IsBlank: Boolean;
begin
  Result := ComputeSign = 0;
end;

function TGocciaTemporalDurationValue.ToISOString: string;
var
  DatePart, TimePart: string;
  ASign: Integer;
  AbsY, AbsMo, AbsW, AbsD, AbsH, AbsMi: TBigInteger;
  SecondsDuration, AbsSecondsDuration, SecondsPart, SubSecondsPart: TBigInteger;
  Fraction: string;
begin
  ASign := ComputeSign;
  if ASign = 0 then
  begin
    Result := 'PT0S';
    Exit;
  end;

  AbsY := FYearsBig.AbsValue;
  AbsMo := FMonthsBig.AbsValue;
  AbsW := FWeeksBig.AbsValue;
  AbsD := FDaysBig.AbsValue;
  AbsH := FHoursBig.AbsValue;
  AbsMi := FMinutesBig.AbsValue;

  DatePart := '';
  if not AbsY.IsZero then DatePart := DatePart + AbsY.ToString + 'Y';
  if not AbsMo.IsZero then DatePart := DatePart + AbsMo.ToString + 'M';
  if not AbsW.IsZero then DatePart := DatePart + AbsW.ToString + 'W';
  if not AbsD.IsZero then DatePart := DatePart + AbsD.ToString + 'D';

  TimePart := '';
  if not AbsH.IsZero then TimePart := TimePart + AbsH.ToString + 'H';
  if not AbsMi.IsZero then TimePart := TimePart + AbsMi.ToString + 'M';

  // TC39 Temporal §7.5.40 step 12: recombine second-and-smaller fields before formatting.
  SecondsDuration := TimeDurationFromComponents(TBigInteger.Zero, TBigInteger.Zero,
    FSecondsBig, FMillisecondsBig, FMicrosecondsBig, FNanosecondsBig);
  if (not SecondsDuration.IsZero) or ((DatePart = '') and (TimePart = '')) then
  begin
    AbsSecondsDuration := SecondsDuration.AbsValue;
    SecondsPart := AbsSecondsDuration.Divide(TBigInteger.FromInt64(NANOSECONDS_PER_SECOND));
    SubSecondsPart := AbsSecondsDuration.Modulo(TBigInteger.FromInt64(NANOSECONDS_PER_SECOND));
    TimePart := TimePart + SecondsPart.ToString;
    if not SubSecondsPart.IsZero then
    begin
      Fraction := Format('%.9d', [SubSecondsPart.ToInt64]);
      while (Length(Fraction) > 0) and (Fraction[Length(Fraction)] = '0') do
        Delete(Fraction, Length(Fraction), 1);
      if Length(Fraction) > 0 then
        TimePart := TimePart + '.' + Fraction;
    end;
    TimePart := TimePart + 'S';
  end;

  Result := '';
  if ASign < 0 then
    Result := '-';
  Result := Result + 'P' + DatePart;
  if Length(TimePart) > 0 then
    Result := Result + 'T' + TimePart;
end;

function TGocciaTemporalDurationValue.ToStringTag: string;
begin
  Result := 'Temporal.Duration';
end;

{ Getters }

function TGocciaTemporalDurationValue.GetYears(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := BigIntFieldToNumber(AsDuration(AThisValue, 'get Duration.years').FYearsBig);
end;

function TGocciaTemporalDurationValue.GetMonths(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := BigIntFieldToNumber(AsDuration(AThisValue, 'get Duration.months').FMonthsBig);
end;

function TGocciaTemporalDurationValue.GetWeeks(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := BigIntFieldToNumber(AsDuration(AThisValue, 'get Duration.weeks').FWeeksBig);
end;

function TGocciaTemporalDurationValue.GetDays(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := BigIntFieldToNumber(AsDuration(AThisValue, 'get Duration.days').FDaysBig);
end;

function TGocciaTemporalDurationValue.GetHours(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := BigIntFieldToNumber(AsDuration(AThisValue, 'get Duration.hours').FHoursBig);
end;

function TGocciaTemporalDurationValue.GetMinutes(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := BigIntFieldToNumber(AsDuration(AThisValue, 'get Duration.minutes').FMinutesBig);
end;

function TGocciaTemporalDurationValue.GetSeconds(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := BigIntFieldToNumber(AsDuration(AThisValue, 'get Duration.seconds').FSecondsBig);
end;

function TGocciaTemporalDurationValue.GetMilliseconds(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := BigIntFieldToNumber(AsDuration(AThisValue, 'get Duration.milliseconds').FMillisecondsBig);
end;

function TGocciaTemporalDurationValue.GetMicroseconds(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := BigIntFieldToNumber(AsDuration(AThisValue, 'get Duration.microseconds').FMicrosecondsBig);
end;

function TGocciaTemporalDurationValue.GetNanoseconds(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := BigIntFieldToNumber(AsDuration(AThisValue, 'get Duration.nanoseconds').FNanosecondsBig);
end;

function TGocciaTemporalDurationValue.GetSign(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaNumberLiteralValue.Create(AsDuration(AThisValue, 'get Duration.sign').ComputeSign);
end;

function TGocciaTemporalDurationValue.GetBlank(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  if AsDuration(AThisValue, 'get Duration.blank').IsBlank then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;

{ Methods }

function TGocciaTemporalDurationValue.DurationNegated(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalDurationValue;
begin
  D := AsDuration(AThisValue, 'Duration.prototype.negated');
  Result := TGocciaTemporalDurationValue.CreateFromBigIntegers(
    D.FYearsBig.Negate, D.FMonthsBig.Negate, D.FWeeksBig.Negate, D.FDaysBig.Negate,
    D.FHoursBig.Negate, D.FMinutesBig.Negate, D.FSecondsBig.Negate,
    D.FMillisecondsBig.Negate, D.FMicrosecondsBig.Negate, D.FNanosecondsBig.Negate);
end;

function TGocciaTemporalDurationValue.DurationAbs(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalDurationValue;
begin
  D := AsDuration(AThisValue, 'Duration.prototype.abs');
  Result := TGocciaTemporalDurationValue.CreateFromBigIntegers(
    D.FYearsBig.AbsValue, D.FMonthsBig.AbsValue, D.FWeeksBig.AbsValue, D.FDaysBig.AbsValue,
    D.FHoursBig.AbsValue, D.FMinutesBig.AbsValue, D.FSecondsBig.AbsValue,
    D.FMillisecondsBig.AbsValue, D.FMicrosecondsBig.AbsValue, D.FNanosecondsBig.AbsValue);
end;

function TGocciaTemporalDurationValue.DurationAdd(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D, Other: TGocciaTemporalDurationValue;
  Arg: TGocciaValue;
begin
  D := AsDuration(AThisValue, 'Duration.prototype.add');
  Arg := AArgs.GetElement(0);

  if Arg is TGocciaTemporalDurationValue then
    Other := TGocciaTemporalDurationValue(Arg)
  else if Arg is TGocciaStringLiteralValue then
    Other := DurationFromString(TGocciaStringLiteralValue(Arg).Value,
      SErrorInvalidDurationString)
  else if Arg is TGocciaObjectValue then
    Other := DurationFromObject(TGocciaObjectValue(Arg))
  else
  begin
    ThrowTypeError(SErrorInvalidDurationAddArg, SSuggestTemporalDurationArg);
    Other := nil;
  end;

  Result := AddDurationsCommon(D, Other, 1);
end;

function TGocciaTemporalDurationValue.DurationSubtract(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D, Other: TGocciaTemporalDurationValue;
  Arg: TGocciaValue;
begin
  D := AsDuration(AThisValue, 'Duration.prototype.subtract');
  Arg := AArgs.GetElement(0);

  if Arg is TGocciaTemporalDurationValue then
    Other := TGocciaTemporalDurationValue(Arg)
  else if Arg is TGocciaStringLiteralValue then
    Other := DurationFromString(TGocciaStringLiteralValue(Arg).Value,
      SErrorInvalidDurationString)
  else if Arg is TGocciaObjectValue then
    Other := DurationFromObject(TGocciaObjectValue(Arg))
  else
  begin
    ThrowTypeError(SErrorInvalidDurationSubtractArg, SSuggestTemporalDurationArg);
    Other := nil;
  end;

  Result := AddDurationsCommon(D, Other, -1);
end;

function TGocciaTemporalDurationValue.DurationWith(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalDurationValue;
  Obj: TGocciaObjectValue;
  V: TGocciaValue;
  Fields: TDurationFieldsBig;

begin
  D := AsDuration(AThisValue, 'Duration.prototype.with');
  V := AArgs.GetElement(0);
  if not (V is TGocciaObjectValue) then
    ThrowTypeError(Format(SErrorTemporalWithRequiresObject, ['Duration']), SSuggestTemporalWithObject);
  Obj := TGocciaObjectValue(V);

  Fields := ReadDurationFieldsFromObject(Obj, DurationFieldsFromValue(D));

  Result := TGocciaTemporalDurationValue.CreateFromBigIntegers(
    Fields.Years, Fields.Months, Fields.Weeks, Fields.Days, Fields.Hours,
    Fields.Minutes, Fields.Seconds, Fields.Milliseconds, Fields.Microseconds,
    Fields.Nanoseconds);
end;

// TC39 Temporal §7.3.21 Temporal.Duration.prototype.round(roundTo)
function TGocciaTemporalDurationValue.DurationRound(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalDurationValue;
  Arg, RelToArg: TGocciaValue;
  OptionsObj: TGocciaObjectValue;
  SmallestUnit, LargestUnit, ExistingLargestUnit, DefaultLargestUnit: TTemporalUnit;
  Mode: TTemporalRoundingMode;
  Increment: Integer;
  HasRelativeTo, HasZonedRelativeTo: Boolean;
  RelDate: TTemporalDateRecord;
  RelRecord: TDurationRelativeToRecord;
  TotalNsBig, DivisorBig, TimeNsBig, CarryDaysBig: TBigInteger;
  ResultHoursBig, ResultMinutesBig, ResultSecondsBig: TBigInteger;
  ResultMsBig, ResultUsBig, ResultNsBig: TBigInteger;
  RemNs, TimeNs: Int64;
  ResultYears, ResultMonths: Int64;
  ResultDays, ResultHours, ResultMinutes, ResultSeconds: Int64;
  ResultMs, ResultUs, ResultNs: Int64;
  NeedCalendar: Boolean;
  IntermDate, EndDate, WalkDate, NextDate: TTemporalDateRecord;
  StartEpoch, EndEpoch, WalkEpoch, NextEpoch: Int64;
  WholeUnits, PeriodNs, ScaledValue, RoundedValue: Int64;
  CarryDays: Int64;
  Sign: Integer;
  CalendarYears, CalendarMonths, CalendarWeeks, CalendarDays: Int64;

  function AddRelativeCalendarYears(const AYears: Int64): TTemporalDateRecord;
  begin
    if not TryAddCalendarDate(RelRecord.CalendarId, RelDate.Year, RelDate.Month,
      RelDate.Day, AYears, 0, 0, 0, True, Result) then
      ThrowRangeError(Format(SErrorTemporalInvalidRelativeTo,
        ['Duration.prototype.round']), SSuggestTemporalRelativeTo);
  end;

  function AddRelativeCalendarMonths(const AMonths: Int64): TTemporalDateRecord;
  begin
    if not TryAddCalendarDate(RelRecord.CalendarId, RelDate.Year, RelDate.Month,
      RelDate.Day, 0, AMonths, 0, 0, True, Result) then
      ThrowRangeError(Format(SErrorTemporalInvalidRelativeTo,
        ['Duration.prototype.round']), SSuggestTemporalRelativeTo);
  end;
begin
  try
  D := AsDuration(AThisValue, 'Duration.prototype.round');
  Arg := AArgs.GetElement(0);

  SmallestUnit := tuNone;
  LargestUnit := tuNone;
  Mode := rmHalfExpand;
  Increment := 1;
  HasRelativeTo := False;
  HasZonedRelativeTo := False;

  if Arg is TGocciaStringLiteralValue then
  begin
    if not GetTemporalUnitFromString(TGocciaStringLiteralValue(Arg).Value, SmallestUnit) then
      ThrowRangeError(Format(SErrorTemporalInvalidUnitFor, ['Duration.prototype.round', TGocciaStringLiteralValue(Arg).Value]), SSuggestTemporalValidUnits);
  end
  else if Arg is TGocciaObjectValue then
  begin
    OptionsObj := TGocciaObjectValue(Arg);
    LargestUnit := GetLargestUnit(OptionsObj, tuNone);
    RelToArg := OptionsObj.GetProperty('relativeTo');
    if (RelToArg <> nil) and not (RelToArg is TGocciaUndefinedLiteralValue) then
    begin
      RelRecord := ParseRelativeToRecord(RelToArg, 'Duration.prototype.round');
      HasZonedRelativeTo := RelRecord.IsZoned;
      RelDate := RelRecord.Date;
      HasRelativeTo := True;
    end;
    Increment := GetRoundingIncrement(OptionsObj, 1);
    Mode := GetRoundingMode(OptionsObj, rmHalfExpand);
    SmallestUnit := GetSmallestUnit(OptionsObj, tuNone);
  end
  else
    ThrowTypeError(Format(SErrorTemporalRoundRequiresStringOrOptions, ['Duration']), SSuggestTemporalRoundArg);

  // Validate units
  if (SmallestUnit = tuNone) and (LargestUnit = tuNone) then
    ThrowRangeError(SErrorDurationRoundRequiresUnit, SSuggestTemporalRoundArg);
  if SmallestUnit = tuNone then
    SmallestUnit := tuNanosecond;
  if SmallestUnit = tuAuto then
    ThrowRangeError(Format(SErrorTemporalInvalidUnitFor, ['Duration.prototype.round', 'smallestUnit']),
      SSuggestTemporalValidUnits);

  ExistingLargestUnit := DurationDefaultLargestUnit(D);
  DefaultLargestUnit := ExistingLargestUnit;
  if Ord(SmallestUnit) < Ord(DefaultLargestUnit) then
    DefaultLargestUnit := SmallestUnit;
  if (LargestUnit = tuNone) or (LargestUnit = tuAuto) then
  begin
    LargestUnit := DefaultLargestUnit;
  end;

  if Ord(LargestUnit) > Ord(SmallestUnit) then
    ThrowRangeError(SErrorDurationRoundLargestSmallerThanSmallest, SSuggestTemporalRoundArg);
  ValidateRoundingIncrement(Increment, SmallestUnit, LargestUnit);
  if (Increment > 1) and (LargestUnit <> SmallestUnit) and IsDurationDateUnit(SmallestUnit) then
    ThrowRangeError('roundingIncrement must be 1 when rounding a calendar unit into a larger unit',
      SSuggestTemporalRoundArg);

  // Determine if calendar-relative computation is needed
  NeedCalendar := IsDurationCalendarUnit(ExistingLargestUnit) or
    IsDurationCalendarUnit(SmallestUnit) or IsDurationCalendarUnit(LargestUnit);

  if NeedCalendar and not HasRelativeTo then
    ThrowRangeError(SErrorDurationRoundRequiresRelativeTo, SSuggestTemporalRelativeTo);

  if HasZonedRelativeTo then
  begin
    Sign := D.ComputeSign;
    if ((Sign > 0) and DurationDateAtUpperLimit(RelDate)) or
       ((Sign < 0) and DurationDateAtLowerZonedLimit(RelDate)) or
       ((Sign = 0) and DurationDateAtUpperLimit(RelDate) and
        ((LargestUnit = tuDay) or (SmallestUnit = tuDay))) then
      ThrowRangeError(Format(SErrorTemporalInvalidRelativeTo, ['Duration.prototype.round']),
        SSuggestTemporalRelativeTo);
    if D.FYearsBig.IsZero and D.FMonthsBig.IsZero and D.FWeeksBig.IsZero and
       D.FDaysBig.IsZero then
    begin
      TotalNsBig := DurationTotalNanosecondsBig(D);
      if TotalNsBig.AbsValue.Compare(
        TBigInteger.FromDecimalString('8640000000000000000000')) > 0 then
        ThrowRangeError(Format(SErrorTemporalInvalidRelativeTo, ['Duration.prototype.round']),
          SSuggestTemporalRelativeTo);
    end;
  end;
  if HasRelativeTo and not HasZonedRelativeTo then
  begin
    Sign := D.ComputeSign;
    if (Sign > 0) and DurationDateBefore(RelDate.Year, RelDate.Month,
      RelDate.Day, -271821, 4, 20) then
      ThrowRangeError(Format(SErrorTemporalInvalidRelativeTo, ['Duration.prototype.round']),
        SSuggestTemporalRelativeTo);
  end;

  if HasZonedRelativeTo then
  begin
    Result := RoundZonedDuration(D, RelRecord, LargestUnit, SmallestUnit,
      Mode, Increment);
    Exit;
  end;

  // --- Calendar-relative path ---
  if NeedCalendar then
  begin
    Sign := D.ComputeSign;
    if Sign = 0 then
    begin
      Result := TGocciaTemporalDurationValue.Create(0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
      Exit;
    end;

    // Resolve duration to a concrete end date with years and months as one
    // calendar duration, then add weeks and days.
    CalendarYears := BigIntToCheckedInt64(D.FYearsBig, 'years');
    CalendarMonths := BigIntToCheckedInt64(D.FMonthsBig, 'months');
    CalendarWeeks := BigIntToCheckedInt64(D.FWeeksBig, 'weeks');
    CalendarDays := BigIntToCheckedInt64(D.FDaysBig, 'days');
    TimeNsBig := DurationSubDayNanosecondsBig(D);
    CarryDaysBig := TimeNsBig.Divide(BigIntUnit(NANOSECONDS_PER_DAY));
    TimeNsBig := TimeNsBig.Modulo(BigIntUnit(NANOSECONDS_PER_DAY));
    CarryDays := BigIntToCheckedInt64(CarryDaysBig, 'carry days');
    TimeNs := BigIntToCheckedInt64(TimeNsBig, 'sub-day nanoseconds');

    if not TryAddCalendarDate(RelRecord.CalendarId, RelDate.Year, RelDate.Month,
      RelDate.Day, CalendarYears, CalendarMonths, CalendarWeeks,
      CalendarDays + CarryDays, True, EndDate) then
      ThrowRangeError(Format(SErrorTemporalInvalidRelativeTo, ['Duration.prototype.round']),
        SSuggestTemporalRelativeTo);
    if not DurationDateInTemporalRange(EndDate) then
      ThrowRangeError(Format(SErrorTemporalInvalidRelativeTo, ['Duration.prototype.round']),
        SSuggestTemporalRelativeTo);
    StartEpoch := DateToEpochDays(RelDate.Year, RelDate.Month, RelDate.Day);
    EndEpoch := DateToEpochDays(EndDate.Year, EndDate.Month, EndDate.Day);

    // --- Round to year or month ---
    if (SmallestUnit = tuYear) or (SmallestUnit = tuMonth) then
    begin
      if SmallestUnit = tuYear then
      begin
        // Walk years from relativeTo
        WholeUnits := 0;
        if Sign > 0 then
        begin
          repeat
            NextDate := AddRelativeCalendarYears(WholeUnits + 1);
            NextEpoch := DateToEpochDays(NextDate.Year, NextDate.Month, NextDate.Day);
            if (NextEpoch > EndEpoch) or ((NextEpoch = EndEpoch) and (TimeNs <= 0)) then
              Break;
            Inc(WholeUnits);
          until False;
        end
        else
        begin
          repeat
            NextDate := AddRelativeCalendarYears(WholeUnits - 1);
            NextEpoch := DateToEpochDays(NextDate.Year, NextDate.Month, NextDate.Day);
            if (NextEpoch < EndEpoch) or ((NextEpoch = EndEpoch) and (TimeNs >= 0)) then
              Break;
            Dec(WholeUnits);
          until False;
        end;

        // Align to increment-boundary bucket toward zero, then use the
        // sign-aware bucket span to decide whether rounding carries.
        if Sign > 0 then
          ScaledValue := (WholeUnits div Increment) * Increment
        else
          ScaledValue := -(((-WholeUnits) div Increment) * Increment);

        WalkDate := AddRelativeCalendarYears(ScaledValue);
        WalkEpoch := DateToEpochDays(WalkDate.Year, WalkDate.Month, WalkDate.Day);
        NextDate := AddRelativeCalendarYears(ScaledValue + Sign * Increment);
        NextEpoch := DateToEpochDays(NextDate.Year, NextDate.Month, NextDate.Day);
        PeriodNs := Abs(NextEpoch - WalkEpoch) * NANOSECONDS_PER_DAY;
        RoundedValue := RoundWithMode(
          (EndEpoch - WalkEpoch) * NANOSECONDS_PER_DAY + TimeNs, PeriodNs, Mode);
        if Abs(RoundedValue) >= PeriodNs then
          WholeUnits := ScaledValue + Sign * Increment
        else
          WholeUnits := ScaledValue;

        Result := TGocciaTemporalDurationValue.Create(WholeUnits, 0, 0, 0, 0, 0, 0, 0, 0, 0);
      end
      else // SmallestUnit = tuMonth
      begin
        // Walk months from relativeTo
        WholeUnits := 0;
        if Sign > 0 then
        begin
          repeat
            NextDate := AddRelativeCalendarMonths(WholeUnits + 1);
            NextEpoch := DateToEpochDays(NextDate.Year, NextDate.Month, NextDate.Day);
            if (NextEpoch > EndEpoch) or ((NextEpoch = EndEpoch) and (TimeNs <= 0)) then
              Break;
            Inc(WholeUnits);
          until False;
        end
        else
        begin
          repeat
            NextDate := AddRelativeCalendarMonths(WholeUnits - 1);
            NextEpoch := DateToEpochDays(NextDate.Year, NextDate.Month, NextDate.Day);
            if (NextEpoch < EndEpoch) or ((NextEpoch = EndEpoch) and (TimeNs >= 0)) then
              Break;
            Dec(WholeUnits);
          until False;
        end;

        if Sign > 0 then
          ScaledValue := (WholeUnits div Increment) * Increment
        else
          ScaledValue := -(((-WholeUnits) div Increment) * Increment);

        WalkDate := AddRelativeCalendarMonths(ScaledValue);
        WalkEpoch := DateToEpochDays(WalkDate.Year, WalkDate.Month, WalkDate.Day);
        NextDate := AddRelativeCalendarMonths(ScaledValue + Sign * Increment);
        NextEpoch := DateToEpochDays(NextDate.Year, NextDate.Month, NextDate.Day);
        PeriodNs := Abs(NextEpoch - WalkEpoch) * NANOSECONDS_PER_DAY;
        RoundedValue := RoundWithMode(
          (EndEpoch - WalkEpoch) * NANOSECONDS_PER_DAY + TimeNs, PeriodNs, Mode);
        if Abs(RoundedValue) >= PeriodNs then
          WholeUnits := ScaledValue + Sign * Increment
        else
          WholeUnits := ScaledValue;

        if Ord(LargestUnit) <= Ord(tuYear) then
        begin
          ResultYears := WholeUnits div 12;
          ResultMonths := WholeUnits mod 12;
        end
        else
        begin
          ResultYears := 0;
          ResultMonths := WholeUnits;
        end;

        Result := TGocciaTemporalDurationValue.Create(ResultYears, ResultMonths, 0, 0, 0, 0, 0, 0, 0, 0);
      end;
      Exit;
    end;

    // --- SmallestUnit is week, day, or time: convert to nanoseconds ---
    TotalNsBig := BigIntUnit(EndEpoch - StartEpoch)
      .Multiply(BigIntUnit(NANOSECONDS_PER_DAY))
      .Add(BigIntUnit(TimeNs));

    // --- Rebalance to year/month if needed ---
    if Ord(LargestUnit) <= Ord(tuMonth) then
    begin
      WholeUnits := 0;
      if Sign > 0 then
      begin
        repeat
          NextDate := AddRelativeCalendarMonths(WholeUnits + 1);
          NextEpoch := DateToEpochDays(NextDate.Year, NextDate.Month, NextDate.Day);
          if (NextEpoch > EndEpoch) or ((NextEpoch = EndEpoch) and (TimeNs < 0)) then Break;
          Inc(WholeUnits);
        until False;
      end
      else
      begin
        repeat
          NextDate := AddRelativeCalendarMonths(WholeUnits - 1);
          NextEpoch := DateToEpochDays(NextDate.Year, NextDate.Month, NextDate.Day);
          if (NextEpoch < EndEpoch) or ((NextEpoch = EndEpoch) and (TimeNs > 0)) then Break;
          Dec(WholeUnits);
        until False;
      end;

      WalkDate := AddRelativeCalendarMonths(WholeUnits);
      WalkEpoch := DateToEpochDays(WalkDate.Year, WalkDate.Month, WalkDate.Day);
      TotalNsBig := BigIntUnit(EndEpoch - WalkEpoch)
        .Multiply(BigIntUnit(NANOSECONDS_PER_DAY))
        .Add(BigIntUnit(TimeNs));

      if (SmallestUnit <> tuNanosecond) or (Increment <> 1) then
      begin
        DivisorBig := RoundingDivisorBig(SmallestUnit, Increment);
        TotalNsBig := RoundTimeDurationToIncrement(TotalNsBig, DivisorBig, Mode);
      end;

      RemNs := BigIntToCheckedInt64(TotalNsBig, 'calendar remainder nanoseconds');
      if Sign > 0 then
      begin
        repeat
          NextDate := AddRelativeCalendarMonths(WholeUnits + 1);
          NextEpoch := DateToEpochDays(NextDate.Year, NextDate.Month, NextDate.Day);
          PeriodNs := (NextEpoch - WalkEpoch) * NANOSECONDS_PER_DAY;
          if RemNs < PeriodNs then Break;
          Inc(WholeUnits);
          Dec(RemNs, PeriodNs);
          WalkDate := NextDate;
          WalkEpoch := NextEpoch;
        until False;
      end
      else
      begin
        repeat
          NextDate := AddRelativeCalendarMonths(WholeUnits - 1);
          NextEpoch := DateToEpochDays(NextDate.Year, NextDate.Month, NextDate.Day);
          PeriodNs := (WalkEpoch - NextEpoch) * NANOSECONDS_PER_DAY;
          if RemNs > -PeriodNs then Break;
          Dec(WholeUnits);
          Inc(RemNs, PeriodNs);
          WalkDate := NextDate;
          WalkEpoch := NextEpoch;
        until False;
      end;

      ResultDays := RemNs div NANOSECONDS_PER_DAY;
      RemNs := RemNs mod NANOSECONDS_PER_DAY;

      if Ord(LargestUnit) <= Ord(tuYear) then
      begin
        ResultYears := WholeUnits div 12;
        ResultMonths := WholeUnits mod 12;
      end
      else
      begin
        ResultYears := 0;
        ResultMonths := WholeUnits;
      end;

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

      if (LargestUnit = tuWeek) or (SmallestUnit = tuWeek) then
        Result := TGocciaTemporalDurationValue.Create(ResultYears, ResultMonths,
          ResultDays div 7, ResultDays mod 7,
          ResultHours, ResultMinutes, ResultSeconds, ResultMs, ResultUs, ResultNs)
      else
        Result := TGocciaTemporalDurationValue.Create(ResultYears, ResultMonths, 0, ResultDays,
          ResultHours, ResultMinutes, ResultSeconds, ResultMs, ResultUs, ResultNs);
      Exit;
    end;

    if (SmallestUnit <> tuNanosecond) or (Increment <> 1) then
    begin
      DivisorBig := RoundingDivisorBig(SmallestUnit, Increment);
      TotalNsBig := RoundTimeDurationToIncrement(TotalNsBig, DivisorBig, Mode);
    end;
    // Fall through to non-calendar breakdown below
  end
  else
  begin
    if HasZonedRelativeTo and D.FYearsBig.IsZero and D.FMonthsBig.IsZero and
       D.FWeeksBig.IsZero and not D.FDaysBig.IsZero and (LargestUnit = tuDay) and
       (SmallestUnit in [tuHour, tuMinute, tuSecond, tuMillisecond,
        tuMicrosecond, tuNanosecond]) then
    begin
      TimeNsBig := DurationSubDayNanosecondsBig(D);
      if (SmallestUnit <> tuNanosecond) or (Increment <> 1) then
      begin
        DivisorBig := RoundingDivisorBig(SmallestUnit, Increment);
        TimeNsBig := RoundTimeDurationToIncrement(TimeNsBig, DivisorBig, Mode);
      end;
      RemNs := BigIntToCheckedInt64(TimeNsBig, 'sub-day nanoseconds');
      ResultDays := BigIntToCheckedInt64(D.FDaysBig, 'days');
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
      Result := TGocciaTemporalDurationValue.Create(0, 0, 0, ResultDays,
        ResultHours, ResultMinutes, ResultSeconds, ResultMs, ResultUs, ResultNs);
      Exit;
    end;

    TotalNsBig := DurationTotalNanosecondsBig(D);

    if (SmallestUnit <> tuNanosecond) or (Increment <> 1) then
    begin
      DivisorBig := RoundingDivisorBig(SmallestUnit, Increment);
      TotalNsBig := RoundTimeDurationToIncrement(TotalNsBig, DivisorBig, Mode);
    end;
  end;

  BalanceTimeDurationToFields(TotalNsBig, LargestUnit, ResultHoursBig,
    ResultMinutesBig, ResultSecondsBig, ResultMsBig, ResultUsBig, ResultNsBig,
    ResultDays);

  if (LargestUnit = tuWeek) or (SmallestUnit = tuWeek) then
    Result := TGocciaTemporalDurationValue.CreateFromBigIntegers(TBigInteger.Zero, TBigInteger.Zero,
      BigIntUnit(ResultDays div 7), BigIntUnit(ResultDays mod 7),
      RoundBigToFloat64(ResultHoursBig), RoundBigToFloat64(ResultMinutesBig),
      RoundBigToFloat64(ResultSecondsBig), RoundBigToFloat64(ResultMsBig),
      RoundBigToFloat64(ResultUsBig), RoundBigToFloat64(ResultNsBig))
  else
    Result := TGocciaTemporalDurationValue.CreateFromBigIntegers(TBigInteger.Zero, TBigInteger.Zero,
      TBigInteger.Zero, BigIntUnit(ResultDays),
      RoundBigToFloat64(ResultHoursBig), RoundBigToFloat64(ResultMinutesBig),
      RoundBigToFloat64(ResultSecondsBig), RoundBigToFloat64(ResultMsBig),
      RoundBigToFloat64(ResultUsBig), RoundBigToFloat64(ResultNsBig));
  except
    on E: ETemporalDurationInt64Overflow do
      ThrowRangeError(E.Message, SSuggestTemporalDurationRange);
  end;
end;

function DurationIsUndefinedValue(const AValue: TGocciaValue): Boolean; inline;
begin
  Result := (AValue = nil) or (AValue is TGocciaUndefinedLiteralValue);
end;

function DurationASCIIEqualsIgnoreCase(const ALeft, ARight: string): Boolean;
var
  I: Integer;
  L, R: Char;

  function UpperASCII(const AChar: Char): Char; inline;
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
    L := UpperASCII(ALeft[I]);
    R := UpperASCII(ARight[I]);
    if L <> R then
      Exit(False);
  end;
  Result := True;
end;

function DurationDateBefore(const AY, AM, AD, ABY, ABM, ABD: Integer): Boolean;
begin
  Result := (AY < ABY) or ((AY = ABY) and
    ((AM < ABM) or ((AM = ABM) and (AD < ABD))));
end;

function DurationDateInTemporalRange(const ADate: TTemporalDateRecord): Boolean;
begin
  Result := not DurationDateBefore(ADate.Year, ADate.Month, ADate.Day, -271821, 4, 19) and
    not DurationDateBefore(275760, 9, 13, ADate.Year, ADate.Month, ADate.Day);
end;

function DurationDateAtLowerZonedLimit(const ADate: TTemporalDateRecord): Boolean;
begin
  Result := (ADate.Year = -271821) and (ADate.Month = 4) and (ADate.Day = 20);
end;

function DurationDateAtUpperLimit(const ADate: TTemporalDateRecord): Boolean;
begin
  Result := (ADate.Year = 275760) and (ADate.Month = 9) and (ADate.Day = 13);
end;

function IsDurationZonedRelativeToValue(const AValue: TGocciaValue): Boolean;
var
  Source, Annotation: string;
  BracketStart, BracketEnd: Integer;
begin
  if AValue is TGocciaTemporalZonedDateTimeValue then
    Exit(True);
  if AValue is TGocciaStringLiteralValue then
  begin
    Source := TGocciaStringLiteralValue(AValue).Value;
    while (Length(Source) > 0) and (Source[Length(Source)] = ']') do
    begin
      BracketEnd := Length(Source);
      BracketStart := BracketEnd - 1;
      while (BracketStart > 0) and (Source[BracketStart] <> '[') do
        Dec(BracketStart);
      if BracketStart = 0 then
        Break;
      Annotation := Copy(Source, BracketStart + 1,
        BracketEnd - BracketStart - 1);
      if (Length(Annotation) > 0) and (Annotation[1] = '!') then
        Annotation := Copy(Annotation, 2, Length(Annotation) - 1);
      if not ((Length(Annotation) > 5) and
              (Copy(Annotation, 1, 5) = 'u-ca=')) then
        Exit(True);
      Source := Copy(Source, 1, BracketStart - 1);
    end;
  end;
  Result := False;
end;

function DurationRelativeToHasInvalidPlainDateTimeOffset(const AValue: string): Boolean;
var
  TPos, I, OffsetSeconds: Integer;
  OffsetPart: string;
  HasUTCDesignator, HasSubMinuteSyntax: Boolean;
begin
  Result := False;
  TPos := System.Pos('T', AValue);
  if TPos = 0 then
    TPos := System.Pos('t', AValue);
  if TPos = 0 then
    TPos := System.Pos(' ', AValue);
  if TPos = 0 then
    Exit;

  for I := TPos + 1 to Length(AValue) do
  begin
    if AValue[I] = '[' then
      Exit;
    if (AValue[I] = '+') or (AValue[I] = '-') then
    begin
      OffsetPart := Copy(AValue, I, Length(AValue) - I + 1);
      Result := not TryParseTemporalOffsetString(OffsetPart, OffsetSeconds,
        HasUTCDesignator, HasSubMinuteSyntax) or HasUTCDesignator;
      Exit;
    end;
  end;
end;

function DurationRelativeToHasFractionalHourOrMinute(const AValue: string): Boolean;
var
  TPos, I, ColonCount, DigitCount: Integer;
  C: Char;
begin
  Result := False;
  TPos := System.Pos('T', AValue);
  if TPos = 0 then
    TPos := System.Pos('t', AValue);
  if TPos = 0 then
    TPos := System.Pos(' ', AValue);
  if TPos = 0 then
    Exit;

  ColonCount := 0;
  DigitCount := 0;
  for I := TPos + 1 to Length(AValue) do
  begin
    C := AValue[I];
    if (C = '[') or (C = 'Z') or (C = 'z') or (C = '+') or (C = '-') then
      Exit(False);
    if (C = '.') or (C = ',') then
    begin
      if ColonCount > 0 then
        Exit(ColonCount < 2);
      Exit(DigitCount < 6);
    end;
    if C = ':' then
      Inc(ColonCount)
    else if C in ['0'..'9'] then
      Inc(DigitCount);
  end;
end;

function DurationMonthCodeSyntaxValid(const AMonthCode: string): Boolean;
begin
  Result := ((Length(AMonthCode) = 3) or (Length(AMonthCode) = 4)) and
    (AMonthCode[1] = 'M') and
    (AMonthCode[2] in ['0'..'9']) and
    (AMonthCode[3] in ['0'..'9']) and
    ((Length(AMonthCode) = 3) or (AMonthCode[4] = 'L'));
end;

function DurationMonthFromMonthCode(const AMonthCode: string; out AMonth: Integer): Boolean;
var
  MonthPart: Integer;
begin
  Result := False;
  AMonth := 0;
  if not DurationMonthCodeSyntaxValid(AMonthCode) or
     not TryStrToInt(Copy(AMonthCode, 2, 2), MonthPart) then
    Exit;
  if (MonthPart < 1) or (MonthPart > 12) then
    Exit;
  AMonth := MonthPart;
  Result := True;
end;

function RelativeToCalendarIdFromValue(const AValue: TGocciaValue;
  const AMethod: string): string;
var
  Calendar: string;
begin
  if DurationIsUndefinedValue(AValue) then
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
    ThrowTypeError(AMethod + ' relativeTo.calendar must be a string',
      SSuggestTemporalRelativeTo);
  Calendar := TGocciaStringLiteralValue(AValue).Value;
  Result := CanonicalizeTemporalCalendarIdentifier(Calendar);
  if Result = '' then
    ThrowRangeError(AMethod + ' relativeTo calendar is not supported',
      SSuggestTemporalRelativeTo);
end;

function RequiredRelativeToIntegerField(const AValue: TGocciaValue;
  const AMethod, AName: string): Integer;
begin
  if DurationIsUndefinedValue(AValue) then
    ThrowTypeError(AMethod + ' relativeTo requires ' + AName + ' property',
      SSuggestTemporalRelativeTo);
  Result := ToIntegerWithTruncationValue(AValue);
end;

function OptionalRelativeToIntegerField(const AValue: TGocciaValue;
  const ADefault: Integer): Integer;
begin
  if DurationIsUndefinedValue(AValue) then
    Result := ADefault
  else
    Result := ToIntegerWithTruncationValue(AValue);
end;

function RelativeToOffsetString(const AValue: TGocciaValue; const AMethod: string): string;
begin
  if (AValue is TGocciaStringLiteralValue) or (AValue is TGocciaObjectValue) then
    Exit(AValue.ToStringLiteral.Value);
  ThrowTypeError(AMethod + ' relativeTo.offset must be a string',
    SSuggestTemporalRelativeTo);
end;

function RelativeToTimeZoneString(const AValue: TGocciaValue; const AMethod: string): string;
begin
  if AValue is TGocciaStringLiteralValue then
    Exit(TGocciaStringLiteralValue(AValue).Value);
  ThrowTypeError(AMethod + ' relativeTo.timeZone must be a string',
    SSuggestTemporalRelativeTo);
end;

function ZonedDateTimeToRelativeDate(const AZdt: TGocciaTemporalZonedDateTimeValue): TTemporalDateRecord;
const
  MS_PER_SECOND = 1000;
  MS_PER_DAY = Int64(86400000);
var
  OffsetSeconds: Integer;
  LocalMs, EpochDays, RemainingMs: Int64;
begin
  OffsetSeconds := GetUtcOffsetSeconds(AZdt.TimeZone,
    AZdt.EpochMilliseconds div MS_PER_SECOND);
  LocalMs := AZdt.EpochMilliseconds + Int64(OffsetSeconds) * MS_PER_SECOND;
  EpochDays := LocalMs div MS_PER_DAY;
  RemainingMs := LocalMs mod MS_PER_DAY;
  if RemainingMs < 0 then
    Dec(EpochDays);
  Result := EpochDaysToDate(EpochDays);
end;

function TryParseBasicISODateForRelativeTo(const AValue: string; out ADate: TTemporalDateRecord): Boolean;
var
  I, Year, Month, Day: Integer;
begin
  Result := False;
  if Length(AValue) <> 8 then
    Exit;
  for I := 1 to Length(AValue) do
    if not (AValue[I] in ['0'..'9']) then
      Exit;
  Year := StrToInt(Copy(AValue, 1, 4));
  Month := StrToInt(Copy(AValue, 5, 2));
  Day := StrToInt(Copy(AValue, 7, 2));
  if not IsValidDate(Year, Month, Day) then
    Exit;
  ADate.Year := Year;
  ADate.Month := Month;
  ADate.Day := Day;
  Result := True;
end;

function ParseRelativeToPropertyBag(const AObj: TGocciaObjectValue;
  const AMethod: string; out AIsZoned: Boolean; out ACalendarId,
  ATimeZone: string; out AHour, AMinute, ASecond, AMillisecond,
  AMicrosecond, ANanosecond: Integer; out AEpochMilliseconds: Int64;
  out ASubMillisecondNanoseconds: Integer): TTemporalDateRecord;
const
  MS_PER_SECOND = 1000;
  MS_PER_MINUTE = 60000;
  MS_PER_HOUR = 3600000;
  MS_PER_DAY = Int64(86400000);
var
  VCalendar, VDay, VHour, VMicrosecond, VMillisecond, VMinute, VMonth,
  VMonthCode, VNanosecond, VOffset, VSecond, VTimeZone, VYear, VEra,
  VEraYear: TGocciaValue;
  Year, Month, Day, Hour, Minute, Second, Millisecond, Microsecond, Nanosecond: Integer;
  CalendarId, MonthCode, OffsetString, TimeZoneString: string;
  HasMonth, HasMonthCode, HasOffset, HasTimeZone, IsLeapMonth: Boolean;
  MonthFromCode, OffsetSeconds: Integer;
  OffsetHasUTCDesignator, OffsetHasSubMinuteSyntax: Boolean;
  OffsetEpochMs: Int64;
  ActualOffsetSeconds: Integer;
  ISODate: TTemporalDateRecord;
begin
  AIsZoned := False;
  ACalendarId := 'iso8601';
  ATimeZone := '';
  AHour := 0;
  AMinute := 0;
  ASecond := 0;
  AMillisecond := 0;
  AMicrosecond := 0;
  ANanosecond := 0;
  AEpochMilliseconds := 0;
  ASubMillisecondNanoseconds := 0;

  VCalendar := AObj.GetProperty('calendar');
  CalendarId := RelativeToCalendarIdFromValue(VCalendar, AMethod);
  ACalendarId := CalendarId;

  VDay := AObj.GetProperty(PROP_DAY);
  Day := RequiredRelativeToIntegerField(VDay, AMethod, PROP_DAY);
  VHour := AObj.GetProperty(PROP_HOUR);
  Hour := OptionalRelativeToIntegerField(VHour, 0);
  VMicrosecond := AObj.GetProperty(PROP_MICROSECOND);
  Microsecond := OptionalRelativeToIntegerField(VMicrosecond, 0);
  VMillisecond := AObj.GetProperty(PROP_MILLISECOND);
  Millisecond := OptionalRelativeToIntegerField(VMillisecond, 0);
  VMinute := AObj.GetProperty(PROP_MINUTE);
  Minute := OptionalRelativeToIntegerField(VMinute, 0);
  VMonth := AObj.GetProperty(PROP_MONTH);
  HasMonth := not DurationIsUndefinedValue(VMonth);
  if HasMonth then
    Month := ToIntegerWithTruncationValue(VMonth)
  else
    Month := 0;
  VMonthCode := AObj.GetProperty(PROP_MONTH_CODE);
  HasMonthCode := not DurationIsUndefinedValue(VMonthCode);
  MonthCode := '';
  IsLeapMonth := False;
  if HasMonthCode then
  begin
    MonthCode := VMonthCode.ToStringLiteral.Value;
    if not DurationMonthCodeSyntaxValid(MonthCode) then
      ThrowRangeError(Format(SErrorInvalidMonthCodeFor, [AMethod]),
        SSuggestTemporalMonthCode);
  end;
  VNanosecond := AObj.GetProperty(PROP_NANOSECOND);
  Nanosecond := OptionalRelativeToIntegerField(VNanosecond, 0);
  VOffset := AObj.GetProperty('offset');
  HasOffset := not DurationIsUndefinedValue(VOffset);
  OffsetSeconds := 0;
  if HasOffset then
  begin
    OffsetString := RelativeToOffsetString(VOffset, AMethod);
    if (not TryParseTemporalOffsetString(OffsetString, OffsetSeconds,
      OffsetHasUTCDesignator, OffsetHasSubMinuteSyntax)) or OffsetHasUTCDesignator then
      ThrowRangeError(Format(SErrorTemporalInvalidRelativeTo, [AMethod]),
        SSuggestTemporalRelativeTo);
  end;
  VSecond := AObj.GetProperty(PROP_SECOND);
  Second := OptionalRelativeToIntegerField(VSecond, 0);
  if Second = 60 then
    Second := 59;
  VTimeZone := AObj.GetProperty('timeZone');
  HasTimeZone := not DurationIsUndefinedValue(VTimeZone);
  VYear := AObj.GetProperty(PROP_YEAR);
  if not DurationIsUndefinedValue(VYear) then
    Year := ToIntegerWithTruncationValue(VYear)
  else
  begin
    VEra := AObj.GetProperty('era');
    VEraYear := AObj.GetProperty('eraYear');
    if DurationIsUndefinedValue(VEra) or DurationIsUndefinedValue(VEraYear) or
       not TryCalendarYearFromEra(CalendarId, VEra.ToStringLiteral.Value,
         ToIntegerWithTruncationValue(VEraYear), Year) then
      ThrowTypeError(AMethod + ' relativeTo requires year property',
        SSuggestTemporalRelativeTo);
  end;

  if HasMonthCode then
  begin
    if not TryParseTemporalMonthCode(MonthCode, MonthFromCode, IsLeapMonth) then
      ThrowRangeError(Format(SErrorMonthCodeOutOfRangeIn, [AMethod]),
        SSuggestTemporalMonthCode);
    if HasMonth and (Month <> MonthFromCode) then
      ThrowRangeError(Format(SErrorMonthCodeMismatchIn, [AMethod]),
        SSuggestTemporalMonthCode);
    Month := MonthFromCode;
  end
  else if not HasMonth then
    ThrowTypeError(AMethod + ' relativeTo requires month property',
      SSuggestTemporalRelativeTo);

  if CalendarId = 'iso8601' then
  begin
    if not IsValidDate(Year, Month, Day) then
      ThrowRangeError(Format(SErrorTemporalInvalidRelativeTo, [AMethod]),
        SSuggestTemporalRelativeTo);
  end
  else
  begin
    if not TryResolveCalendarDateToISO(CalendarId, Year, Month, Day,
      HasMonthCode, IsLeapMonth, True, ISODate) then
      ThrowRangeError(Format(SErrorTemporalInvalidRelativeTo, [AMethod]),
        SSuggestTemporalRelativeTo);
    Year := ISODate.Year;
    Month := ISODate.Month;
    Day := ISODate.Day;
  end;
  if not IsValidTime(Hour, Minute, Second, Millisecond, Microsecond, Nanosecond) then
    ThrowRangeError(Format(SErrorTemporalInvalidRelativeTo, [AMethod]),
      SSuggestTemporalRelativeTo);

  if HasTimeZone then
  begin
    TimeZoneString := CanonicalizeTemporalTimeZoneIdentifier(
      RelativeToTimeZoneString(VTimeZone, AMethod));
    AIsZoned := True;
    ATimeZone := TimeZoneString;
    AHour := Hour;
    AMinute := Minute;
    ASecond := Second;
    AMillisecond := Millisecond;
    AMicrosecond := Microsecond;
    ANanosecond := Nanosecond;
    ASubMillisecondNanoseconds := Microsecond * 1000 + Nanosecond;

    if HasOffset then
    begin
      OffsetEpochMs := DateToEpochDays(Year, Month, Day) * MS_PER_DAY +
        Int64(Hour) * MS_PER_HOUR + Int64(Minute) * MS_PER_MINUTE +
        Int64(Second) * MS_PER_SECOND + Millisecond -
        Int64(OffsetSeconds) * MS_PER_SECOND;
      ActualOffsetSeconds := GetUtcOffsetSeconds(TimeZoneString,
        OffsetEpochMs div MS_PER_SECOND);
      if ActualOffsetSeconds <> OffsetSeconds then
        ThrowRangeError(Format(SErrorTemporalInvalidRelativeTo, [AMethod]),
          SSuggestTemporalRelativeTo);
      AEpochMilliseconds := OffsetEpochMs;
    end;
    if not HasOffset then
      AEpochMilliseconds := LocalDateTimeToEpochMillisecondsWithDisambiguation(
        Year, Month, Day, Hour, Minute, Second, Millisecond, TimeZoneString,
        ttzdCompatible);
    if not IsValidEpochNanoseconds(EpochNanosecondsFromParts(
      AEpochMilliseconds, ASubMillisecondNanoseconds)) then
      ThrowRangeError(Format(SErrorTemporalInvalidRelativeTo, [AMethod]),
        SSuggestTemporalRelativeTo);
  end;

  Result.Year := Year;
  Result.Month := Month;
  Result.Day := Day;
  if not DurationDateInTemporalRange(Result) then
    ThrowRangeError(Format(SErrorTemporalInvalidRelativeTo, [AMethod]),
      SSuggestTemporalRelativeTo);
end;

function ParseRelativeTo(const AValue: TGocciaValue; const AMethod: string): TTemporalDateRecord;
var
  DateRec: TTemporalDateRecord;
  PlainDate: TGocciaTemporalPlainDateValue;
  PlainDateTime: TGocciaTemporalPlainDateTimeValue;
  ZonedDateTime: TGocciaTemporalZonedDateTimeValue;
  Source: string;
  IgnoredIsZoned: Boolean;
  IgnoredCalendarId, IgnoredTimeZone: string;
  IgnoredHour, IgnoredMinute, IgnoredSecond, IgnoredMillisecond,
  IgnoredMicrosecond, IgnoredNanosecond: Integer;
  IgnoredEpochMilliseconds: Int64;
  IgnoredSubMillisecondNanoseconds: Integer;
begin
  if AValue is TGocciaTemporalPlainDateValue then
  begin
    PlainDate := TGocciaTemporalPlainDateValue(AValue);
    Result.Year := PlainDate.Year;
    Result.Month := PlainDate.Month;
    Result.Day := PlainDate.Day;
  end
  else if AValue is TGocciaTemporalPlainDateTimeValue then
  begin
    PlainDateTime := TGocciaTemporalPlainDateTimeValue(AValue);
    Result.Year := PlainDateTime.Year;
    Result.Month := PlainDateTime.Month;
    Result.Day := PlainDateTime.Day;
  end
  else if AValue is TGocciaTemporalZonedDateTimeValue then
  begin
    Result := ZonedDateTimeToRelativeDate(TGocciaTemporalZonedDateTimeValue(AValue));
  end
  else if AValue is TGocciaStringLiteralValue then
  begin
    Source := TGocciaStringLiteralValue(AValue).Value;
    if IsDurationZonedRelativeToValue(AValue) then
    begin
      ZonedDateTime := CoerceTemporalZonedDateTime(AValue, AMethod);
      Result := ZonedDateTimeToRelativeDate(ZonedDateTime);
      Exit;
    end;
    if DurationRelativeToHasInvalidPlainDateTimeOffset(Source) then
      ThrowRangeError(Format(SErrorTemporalInvalidRelativeTo, [AMethod]),
        SSuggestTemporalRelativeTo);
    if DurationRelativeToHasFractionalHourOrMinute(Source) then
      ThrowRangeError(Format(SErrorTemporalInvalidRelativeTo, [AMethod]),
        SSuggestTemporalRelativeTo);
    if ((System.Pos('T', Source) > 0) or (System.Pos('t', Source) > 0) or
        (System.Pos(' ', Source) > 0)) and
       ((Length(Source) > 0) and ((Source[Length(Source)] = 'Z') or
        (Source[Length(Source)] = 'z'))) then
      ThrowRangeError(Format(SErrorTemporalInvalidRelativeTo, [AMethod]),
        SSuggestTemporalRelativeTo);
    Source := StringReplace(Source, ':60', ':59', []);
    if not (CoerceToISODate(Source, DateRec) or
      TryParseBasicISODateForRelativeTo(Source, DateRec)) then
      ThrowRangeError(Format(SErrorTemporalInvalidRelativeTo, [AMethod]), SSuggestTemporalRelativeTo);
    if not DurationDateInTemporalRange(DateRec) then
      ThrowRangeError(Format(SErrorTemporalInvalidRelativeTo, [AMethod]), SSuggestTemporalRelativeTo);
    Result := DateRec;
  end
  else if AValue is TGocciaObjectValue then
    Result := ParseRelativeToPropertyBag(TGocciaObjectValue(AValue), AMethod,
      IgnoredIsZoned, IgnoredCalendarId, IgnoredTimeZone, IgnoredHour,
      IgnoredMinute, IgnoredSecond, IgnoredMillisecond, IgnoredMicrosecond,
      IgnoredNanosecond, IgnoredEpochMilliseconds,
      IgnoredSubMillisecondNanoseconds)
  else
    ThrowTypeError(Format(SErrorTemporalInvalidRelativeTo, [AMethod]), SSuggestTemporalRelativeTo);
end;

function CalendarIdForPlainRelativeTo(const AValue: TGocciaValue): string;
var
  CalendarValue: TGocciaValue;
  CalendarId: string;
begin
  if AValue is TGocciaTemporalPlainDateValue then
    Exit(TGocciaTemporalPlainDateValue(AValue).CalendarId);
  if AValue is TGocciaTemporalPlainDateTimeValue then
    Exit(TGocciaTemporalPlainDateTimeValue(AValue).CalendarId);
  if AValue is TGocciaTemporalPlainMonthDayValue then
    Exit(TGocciaTemporalPlainMonthDayValue(AValue).CalendarId);
  if AValue is TGocciaTemporalPlainYearMonthValue then
    Exit(TGocciaTemporalPlainYearMonthValue(AValue).CalendarId);
  if AValue is TGocciaStringLiteralValue then
  begin
    if TryExtractTemporalCalendarAnnotation(
      TGocciaStringLiteralValue(AValue).Value, CalendarId) then
      Exit(CalendarId);
    Exit('iso8601');
  end;
  if AValue is TGocciaObjectValue then
  begin
    CalendarValue := TGocciaObjectValue(AValue).GetProperty('calendar');
    Exit(RelativeToCalendarIdFromValue(CalendarValue, 'Duration relativeTo'));
  end;
  Result := 'iso8601';
end;

function ParseRelativeToRecord(const AValue: TGocciaValue;
  const AMethod: string): TDurationRelativeToRecord;
var
  ZonedDateTime: TGocciaTemporalZonedDateTimeValue;
  Year, Month, Day, Hour, Minute, Second, Millisecond, Microsecond,
  Nanosecond: Integer;
begin
  Result.HasRelativeTo := not DurationIsUndefinedValue(AValue);
  Result.IsZoned := False;
  Result.CalendarId := 'iso8601';
  Result.TimeZone := '';
  Result.Hour := 0;
  Result.Minute := 0;
  Result.Second := 0;
  Result.Millisecond := 0;
  Result.Microsecond := 0;
  Result.Nanosecond := 0;
  Result.EpochMilliseconds := 0;
  Result.SubMillisecondNanoseconds := 0;
  if not Result.HasRelativeTo then
    Exit;

  if (AValue is TGocciaObjectValue) and
     not (AValue is TGocciaTemporalPlainDateValue) and
     not (AValue is TGocciaTemporalPlainDateTimeValue) and
     not (AValue is TGocciaTemporalPlainMonthDayValue) and
     not (AValue is TGocciaTemporalPlainYearMonthValue) and
     not (AValue is TGocciaTemporalZonedDateTimeValue) then
  begin
    Result.Date := ParseRelativeToPropertyBag(TGocciaObjectValue(AValue),
      AMethod, Result.IsZoned, Result.CalendarId, Result.TimeZone,
      Result.Hour, Result.Minute, Result.Second, Result.Millisecond,
      Result.Microsecond, Result.Nanosecond, Result.EpochMilliseconds,
      Result.SubMillisecondNanoseconds);
    Exit;
  end;

  if IsDurationZonedRelativeToValue(AValue) then
  begin
    ZonedDateTime := CoerceTemporalZonedDateTime(AValue, AMethod);
    DurationLocalComponentsFromEpoch(ZonedDateTime.EpochMilliseconds,
      ZonedDateTime.SubMillisecondNanoseconds, ZonedDateTime.TimeZone, Year,
      Month, Day, Hour, Minute, Second, Millisecond, Microsecond, Nanosecond);
    Result.IsZoned := True;
    Result.Date.Year := Year;
    Result.Date.Month := Month;
    Result.Date.Day := Day;
    Result.Hour := Hour;
    Result.Minute := Minute;
    Result.Second := Second;
    Result.Millisecond := Millisecond;
    Result.Microsecond := Microsecond;
    Result.Nanosecond := Nanosecond;
    Result.EpochMilliseconds := ZonedDateTime.EpochMilliseconds;
    Result.SubMillisecondNanoseconds := ZonedDateTime.SubMillisecondNanoseconds;
    Result.TimeZone := ZonedDateTime.TimeZone;
    Result.CalendarId := ZonedDateTime.CalendarId;
    Exit;
  end;

  Result.Date := ParseRelativeTo(AValue, AMethod);
  Result.CalendarId := CalendarIdForPlainRelativeTo(AValue);
end;

function DurationExactTimeZonedEndOutOfRange(const D: TGocciaTemporalDurationValue;
  const ARelativeTo: TGocciaValue; const AMethod: string): Boolean;
var
  EndEpochNanoseconds: TBigInteger;
  StartEpochNanoseconds: TBigInteger;
  TotalNanoseconds: TBigInteger;
  ZonedDateTime: TGocciaTemporalZonedDateTimeValue;
begin
  Result := False;
  if not D.FYearsBig.IsZero or not D.FMonthsBig.IsZero or
     not D.FWeeksBig.IsZero or not D.FDaysBig.IsZero then
    Exit;

  TotalNanoseconds := DurationTotalNanosecondsBig(D);
  if TotalNanoseconds.IsZero then
    Exit;

  if ARelativeTo is TGocciaTemporalZonedDateTimeValue then
    ZonedDateTime := TGocciaTemporalZonedDateTimeValue(ARelativeTo)
  else if IsDurationZonedRelativeToValue(ARelativeTo) then
    ZonedDateTime := CoerceTemporalZonedDateTime(ARelativeTo, AMethod)
  else
    Exit;

  StartEpochNanoseconds := EpochNanosecondsFromParts(
    ZonedDateTime.EpochMilliseconds, ZonedDateTime.SubMillisecondNanoseconds);
  EndEpochNanoseconds := StartEpochNanoseconds.Add(TotalNanoseconds);
  Result := not IsValidEpochNanoseconds(EndEpochNanoseconds);
end;

// Add AMonths calendar months to a date, clamping the day if needed.
function AddMonthsToDate(const ADate: TTemporalDateRecord; const AMonths: Int64): TTemporalDateRecord;
var
  TotalMonths, Y: Int64;
  M, D: Integer;
begin
  TotalMonths := Int64(ADate.Year) * 12 + Int64(ADate.Month - 1) + AMonths;
  if TotalMonths >= 0 then
  begin
    Y := TotalMonths div 12;
    M := Integer(TotalMonths mod 12) + 1;
  end
  else
  begin
    Y := (TotalMonths - 11) div 12;
    M := Integer(TotalMonths - Y * 12) + 1;
  end;
  D := ADate.Day;
  if D > DaysInMonth(Integer(Y), M) then
    D := DaysInMonth(Integer(Y), M);
  Result.Year := Integer(Y);
  Result.Month := M;
  Result.Day := D;
end;

// TC39 Temporal §7.5.33 ComputeNudgeWindow(sign, duration, originEpochNs, isoDateTime, timeZone, calendar, increment, unit, additionalShift)
procedure ValidateZonedRelativeToTotalCalendarWindow(
  const ADuration: TGocciaTemporalDurationValue;
  const ARelDate: TTemporalDateRecord; const ATargetUnit: TTemporalUnit);
const
  MONTHS_PER_YEAR = 12;
var
  Sign: Integer;
  CalendarStepMonths: Int64;
  WindowDate: TTemporalDateRecord;
begin
  Sign := ADuration.ComputeSign;
  if Sign = 0 then
    Exit;

  case ATargetUnit of
    tuYear:
      CalendarStepMonths := MONTHS_PER_YEAR;
    tuMonth:
      CalendarStepMonths := 1;
  else
    Exit;
  end;

  WindowDate := AddMonthsToDate(ARelDate, CalendarStepMonths * Sign);
  if not DurationDateInTemporalRange(WindowDate) then
    ThrowRangeError(Format(SErrorTemporalInvalidRelativeTo, ['Duration.prototype.total']),
      SSuggestTemporalRelativeTo);
end;

// Compute the end date of a duration applied from a reference date using the
// relativeTo calendar (add years+months together, then weeks+days).
function DurationEndDate(const ACalendarId: string; const ADate: TTemporalDateRecord;
  const AYears, AMonths, AWeeks, ADays: Int64): TTemporalDateRecord;
begin
  if not TryAddCalendarDate(ACalendarId, ADate.Year, ADate.Month, ADate.Day,
    AYears, AMonths, AWeeks, ADays, True, Result) then
    ThrowRangeError(Format(SErrorTemporalInvalidRelativeTo,
      ['Duration relativeTo']), SSuggestTemporalRelativeTo);
end;

// Resolve calendar-relative duration components (years, months, weeks) to a
// concrete day count by walking forward from a reference date.
function ResolveRelativeDays(const ACalendarId: string; const ADate: TTemporalDateRecord;
  const AYears, AMonths, AWeeks, ADays: Int64): Int64;
var
  EndRec: TTemporalDateRecord;
begin
  EndRec := DurationEndDate(ACalendarId, ADate, AYears, AMonths, AWeeks, ADays);
  Result := DateToEpochDays(EndRec.Year, EndRec.Month, EndRec.Day) -
            DateToEpochDays(ADate.Year, ADate.Month, ADate.Day);
end;

// Express the duration as a fractional month count relative to a reference date.
// Walks forward from the reference date by the duration's calendar components,
// then measures the result in calendar months plus a fractional remainder.
// Shared calendar-walk helper for TotalInMonths and TotalInYears.
// AMonthsPerUnit is 1 for months, 12 for years.
function ComputeTotalInUnits(const D: TGocciaTemporalDurationValue;
  const ACalendarId: string; const ARelDate: TTemporalDateRecord;
  const AMonthsPerUnit: Int64): Double;
var
  EndRec, CheckRec, SpanRec: TTemporalDateRecord;
  WholeUnits, CarryDays: Int64;
  CalendarYears, CalendarMonths, CalendarWeeks, CalendarDays: Int64;
  TotalMonthsDiff: Int64;
  CheckEpoch, EndEpoch, SpanEpoch: Int64;
  RemainingDays, DaysInSpan: Int64;
  TimeNsBig, CarryDaysBig, RemainderNsBig, FractionNumerator,
  FractionDenominator, TotalNumerator: TBigInteger;
begin
  // Fold sub-day time overflow into whole days so the calendar walk is accurate
  TimeNsBig := DurationSubDayNanosecondsBig(D);
  CarryDaysBig := TimeNsBig.Divide(BigIntUnit(NANOSECONDS_PER_DAY));
  RemainderNsBig := TimeNsBig.Modulo(BigIntUnit(NANOSECONDS_PER_DAY));
  CarryDays := BigIntToCheckedInt64(CarryDaysBig, 'carry days');

  CalendarYears := BigIntToCheckedInt64(D.FYearsBig, 'years');
  CalendarMonths := BigIntToCheckedInt64(D.FMonthsBig, 'months');
  CalendarWeeks := BigIntToCheckedInt64(D.FWeeksBig, 'weeks');
  CalendarDays := BigIntToCheckedInt64(D.FDaysBig, 'days');
  EndRec := DurationEndDate(ACalendarId, ARelDate, CalendarYears, CalendarMonths,
    CalendarWeeks, CalendarDays + CarryDays);
  EndEpoch := DateToEpochDays(EndRec.Year, EndRec.Month, EndRec.Day);

  // Estimate whole units from the month difference
  TotalMonthsDiff := Int64(EndRec.Year - ARelDate.Year) * 12 +
                     Int64(EndRec.Month - ARelDate.Month);
  WholeUnits := TotalMonthsDiff div AMonthsPerUnit;

  // Verify estimate by walking from the reference date
  if not TryAddCalendarDate(ACalendarId, ARelDate.Year, ARelDate.Month,
    ARelDate.Day, 0, WholeUnits * AMonthsPerUnit, 0, 0, True, CheckRec) then
    ThrowRangeError(Format(SErrorTemporalInvalidRelativeTo,
      ['Duration.prototype.total']), SSuggestTemporalRelativeTo);
  CheckEpoch := DateToEpochDays(CheckRec.Year, CheckRec.Month, CheckRec.Day);

  // Adjust if overshot forward
  while (WholeUnits > 0) and (CheckEpoch > EndEpoch) do
  begin
    Dec(WholeUnits);
    if not TryAddCalendarDate(ACalendarId, ARelDate.Year, ARelDate.Month,
      ARelDate.Day, 0, WholeUnits * AMonthsPerUnit, 0, 0, True, CheckRec) then
      ThrowRangeError(Format(SErrorTemporalInvalidRelativeTo,
        ['Duration.prototype.total']), SSuggestTemporalRelativeTo);
    CheckEpoch := DateToEpochDays(CheckRec.Year, CheckRec.Month, CheckRec.Day);
  end;
  // Adjust if overshot backward
  while (WholeUnits < 0) and (CheckEpoch < EndEpoch) do
  begin
    Inc(WholeUnits);
    if not TryAddCalendarDate(ACalendarId, ARelDate.Year, ARelDate.Month,
      ARelDate.Day, 0, WholeUnits * AMonthsPerUnit, 0, 0, True, CheckRec) then
      ThrowRangeError(Format(SErrorTemporalInvalidRelativeTo,
        ['Duration.prototype.total']), SSuggestTemporalRelativeTo);
    CheckEpoch := DateToEpochDays(CheckRec.Year, CheckRec.Month, CheckRec.Day);
  end;

  // Fractional part: remaining days + sub-day remainder only
  RemainingDays := EndEpoch - CheckEpoch;

  // Determine the unit span for the fractional calculation
  FractionNumerator := BigIntUnit(RemainingDays)
    .Multiply(BigIntUnit(NANOSECONDS_PER_DAY)).Add(RemainderNsBig);
  if FractionNumerator.Compare(TBigInteger.Zero) >= 0 then
  begin
    if not TryAddCalendarDate(ACalendarId, ARelDate.Year, ARelDate.Month,
      ARelDate.Day, 0, (WholeUnits + 1) * AMonthsPerUnit, 0, 0, True,
      SpanRec) then
      ThrowRangeError(Format(SErrorTemporalInvalidRelativeTo,
        ['Duration.prototype.total']), SSuggestTemporalRelativeTo);
    SpanEpoch := DateToEpochDays(SpanRec.Year, SpanRec.Month, SpanRec.Day);
    DaysInSpan := SpanEpoch - CheckEpoch;
  end
  else
  begin
    if not TryAddCalendarDate(ACalendarId, ARelDate.Year, ARelDate.Month,
      ARelDate.Day, 0, (WholeUnits - 1) * AMonthsPerUnit, 0, 0, True,
      SpanRec) then
      ThrowRangeError(Format(SErrorTemporalInvalidRelativeTo,
        ['Duration.prototype.total']), SSuggestTemporalRelativeTo);
    SpanEpoch := DateToEpochDays(SpanRec.Year, SpanRec.Month, SpanRec.Day);
    DaysInSpan := CheckEpoch - SpanEpoch;
  end;

  if DaysInSpan > 0 then
  begin
    FractionDenominator := BigIntUnit(DaysInSpan)
      .Multiply(BigIntUnit(NANOSECONDS_PER_DAY));
    TotalNumerator := BigIntUnit(WholeUnits)
      .Multiply(FractionDenominator).Add(FractionNumerator);
    Result := BigRationalToEngineDouble(TotalNumerator, FractionDenominator);
  end
  else
    Result := WholeUnits;
end;

function TotalInMonths(const D: TGocciaTemporalDurationValue;
  const ACalendarId: string; const ARelDate: TTemporalDateRecord): TGocciaValue;
begin
  Result := TGocciaNumberLiteralValue.Create(ComputeTotalInUnits(D,
    ACalendarId, ARelDate, 1));
end;

// Express the duration as a fractional year count relative to a reference date.
function TotalInYears(const D: TGocciaTemporalDurationValue;
  const ACalendarId: string; const ARelDate: TTemporalDateRecord): TGocciaValue;
begin
  Result := TGocciaNumberLiteralValue.Create(ComputeTotalInUnits(D,
    ACalendarId, ARelDate, 12));
end;

function DurationNanosecondsRelativeToPlainDate(const D: TGocciaTemporalDurationValue;
  const ACalendarId: string; const ARelDate: TTemporalDateRecord): TBigInteger;
var
  ResolvedDays: Int64;
begin
  ResolvedDays := ResolveRelativeDays(ACalendarId, ARelDate,
    BigIntToCheckedInt64(D.FYearsBig, 'years'),
    BigIntToCheckedInt64(D.FMonthsBig, 'months'),
    BigIntToCheckedInt64(D.FWeeksBig, 'weeks'),
    BigIntToCheckedInt64(D.FDaysBig, 'days'));
  Result := DurationSubDayNanosecondsBig(D)
    .Add(BigIntUnit(ResolvedDays).Multiply(BigIntUnit(NANOSECONDS_PER_DAY)));
end;

procedure ValidateDurationTotalPlainDateRange(const D: TGocciaTemporalDurationValue;
  const ACalendarId: string; const ARelDate: TTemporalDateRecord;
  const AMethod: string);
var
  TimeNsBig, CarryDaysBig, RemainderNsBig, TotalNsBig: TBigInteger;
  CarryDays, CalendarYears, CalendarMonths, CalendarWeeks, CalendarDays: Int64;
  EndDate: TTemporalDateRecord;
begin
  TimeNsBig := DurationSubDayNanosecondsBig(D);
  CarryDaysBig := TimeNsBig.Divide(BigIntUnit(NANOSECONDS_PER_DAY));
  RemainderNsBig := TimeNsBig.Modulo(BigIntUnit(NANOSECONDS_PER_DAY));
  CarryDays := BigIntToCheckedInt64(CarryDaysBig, 'carry days');
  CalendarYears := BigIntToCheckedInt64(D.FYearsBig, 'years');
  CalendarMonths := BigIntToCheckedInt64(D.FMonthsBig, 'months');
  CalendarWeeks := BigIntToCheckedInt64(D.FWeeksBig, 'weeks');
  CalendarDays := BigIntToCheckedInt64(D.FDaysBig, 'days');
  EndDate := DurationEndDate(ACalendarId, ARelDate, CalendarYears, CalendarMonths,
    CalendarWeeks, CalendarDays + CarryDays);
  if (not DurationDateInTemporalRange(EndDate)) or
     (DurationDateAtUpperLimit(EndDate) and RemainderNsBig.IsPositive) or
     (DurationDateBefore(EndDate.Year, EndDate.Month, EndDate.Day,
       -271821, 4, 20) and RemainderNsBig.IsNegative) then
    ThrowRangeError(Format(SErrorTemporalInvalidRelativeTo, [AMethod]),
      SSuggestTemporalRelativeTo);

  TotalNsBig := DurationNanosecondsRelativeToPlainDate(D, ACalendarId, ARelDate);
  if not IsValidTimeDuration(TotalNsBig) then
    ThrowRangeError(Format(SErrorTemporalInvalidRelativeTo, [AMethod]),
      SSuggestTemporalRelativeTo);
end;

function DurationZonedDateTimeInTotalRange(const ADate: TTemporalDateRecord;
  const AHour, AMinute, ASecond, AMillisecond, AMicrosecond,
  ANanosecond: Integer): Boolean;
begin
  if DurationDateBefore(ADate.Year, ADate.Month, ADate.Day,
    -271821, 4, 20) then
    Exit(False);
  if DurationDateBefore(275760, 9, 12, ADate.Year, ADate.Month,
    ADate.Day) then
    Exit(False);
  if (ADate.Year = 275760) and (ADate.Month = 9) and
     (ADate.Day = 12) then
    Exit((AHour = 0) and (AMinute = 0) and (ASecond = 0) and
      (AMillisecond = 0) and (AMicrosecond = 0) and (ANanosecond = 0));
  Result := True;
end;

procedure ValidateDurationTotalZonedRange(const D: TGocciaTemporalDurationValue;
  const ARelativeTo: TDurationRelativeToRecord; const ATargetUnit: TTemporalUnit;
  const AMethod: string);
var
  EndNanoseconds: TBigInteger;
  EndMilliseconds: Int64;
  EndSubMillisecondNanoseconds: Integer;
  EndDate: TTemporalDateRecord;
  EndYear, EndMonth, EndDay, EndHour, EndMinute, EndSecond: Integer;
  EndMillisecond, EndMicrosecond, EndNanosecond: Integer;
begin
  if IsDurationDateUnit(ATargetUnit) and
     not DurationZonedDateTimeInTotalRange(ARelativeTo.Date,
       ARelativeTo.Hour, ARelativeTo.Minute, ARelativeTo.Second,
       ARelativeTo.Millisecond, ARelativeTo.Microsecond,
       ARelativeTo.Nanosecond) then
    ThrowRangeError(Format(SErrorTemporalInvalidRelativeTo, [AMethod]),
      SSuggestTemporalRelativeTo);

  if (D.ComputeSign = 0) and not IsDurationDateUnit(ATargetUnit) then
    Exit;

  EndNanoseconds := DurationAddZonedDateTime(D, ARelativeTo);
  SplitDurationEpochNanoseconds(EndNanoseconds, EndMilliseconds,
    EndSubMillisecondNanoseconds);
  DurationLocalComponentsFromEpoch(EndMilliseconds,
    EndSubMillisecondNanoseconds, ARelativeTo.TimeZone, EndYear, EndMonth,
    EndDay, EndHour, EndMinute, EndSecond, EndMillisecond, EndMicrosecond,
    EndNanosecond);
  EndDate.Year := EndYear;
  EndDate.Month := EndMonth;
  EndDate.Day := EndDay;
  if IsDurationDateUnit(ATargetUnit) and
     not DurationZonedDateTimeInTotalRange(EndDate, EndHour, EndMinute,
       EndSecond, EndMillisecond, EndMicrosecond, EndNanosecond) then
    ThrowRangeError(Format(SErrorTemporalInvalidRelativeTo, [AMethod]),
      SSuggestTemporalRelativeTo);
end;

function CompareTemporalDurations(const AOne, ATwo: TGocciaTemporalDurationValue;
  const AOptions: TGocciaValue): Integer;
var
  OptionsObj: TGocciaObjectValue;
  RelativeToArg: TGocciaValue;
  RelativeTo: TDurationRelativeToRecord;
  OneNanoseconds, TwoNanoseconds: TBigInteger;
begin
  RelativeToArg := nil;
  if DurationIsUndefinedValue(AOptions) then
    OptionsObj := nil
  else if AOptions is TGocciaObjectValue then
  begin
    OptionsObj := TGocciaObjectValue(AOptions);
    RelativeToArg := OptionsObj.GetProperty('relativeTo');
  end
  else
    ThrowTypeError('Temporal.Duration.compare options must be an object',
      SSuggestTemporalCompareArg);

  if not DurationIsUndefinedValue(RelativeToArg) then
  begin
    RelativeTo := ParseRelativeToRecord(RelativeToArg,
      'Temporal.Duration.compare');
    if RelativeTo.IsZoned then
    begin
      if DurationHasDateUnits(AOne) or DurationHasDateUnits(ATwo) then
      begin
        OneNanoseconds := DurationAddZonedDateTime(AOne, RelativeTo);
        TwoNanoseconds := DurationAddZonedDateTime(ATwo, RelativeTo);
      end
      else
      begin
        OneNanoseconds := DurationTotalNanosecondsBig(AOne);
        TwoNanoseconds := DurationTotalNanosecondsBig(ATwo);
      end;
    end
    else
    begin
      OneNanoseconds := DurationNanosecondsRelativeToPlainDate(AOne,
        RelativeTo.CalendarId, RelativeTo.Date);
      TwoNanoseconds := DurationNanosecondsRelativeToPlainDate(ATwo,
        RelativeTo.CalendarId, RelativeTo.Date);
      if (not IsValidTimeDuration(OneNanoseconds)) or
         (not IsValidTimeDuration(TwoNanoseconds)) then
        ThrowRangeError(Format(SErrorTemporalInvalidRelativeTo,
          ['Temporal.Duration.compare']), SSuggestTemporalRelativeTo);
    end;
  end
  else
  begin
    if DurationFieldsIdentical(AOne, ATwo) then
      Exit(0);
    if DurationHasCalendarUnits(AOne) or DurationHasCalendarUnits(ATwo) then
      ThrowRangeError(SErrorDurationTotalRequiresRelativeTo,
        SSuggestTemporalRelativeTo);
    OneNanoseconds := DurationTotalNanosecondsBig(AOne);
    TwoNanoseconds := DurationTotalNanosecondsBig(ATwo);
  end;

  Result := OneNanoseconds.Compare(TwoNanoseconds);
  if Result < 0 then
    Result := -1
  else if Result > 0 then
    Result := 1;
end;

function TGocciaTemporalDurationValue.DurationTotal(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalDurationValue;
  UnitStr: string;
  TargetUnit: TTemporalUnit;
  Arg, RelToArg: TGocciaValue;
  OptionsObj: TGocciaObjectValue;
  HasRelativeTo: Boolean;
  HasZonedRelativeTo: Boolean;
  RelDate: TTemporalDateRecord;
  RelRecord: TDurationRelativeToRecord;
  ResolvedDays: Int64;
  TotalNsBig: TBigInteger;
begin
  try
  D := AsDuration(AThisValue, 'Duration.prototype.total');
  Arg := AArgs.GetElement(0);
  HasRelativeTo := False;
  HasZonedRelativeTo := False;

  if Arg is TGocciaStringLiteralValue then
    UnitStr := TGocciaStringLiteralValue(Arg).Value
  else if Arg is TGocciaObjectValue then
  begin
    OptionsObj := TGocciaObjectValue(Arg);

    RelToArg := OptionsObj.GetProperty('relativeTo');
    HasRelativeTo := (RelToArg <> nil) and not (RelToArg is TGocciaUndefinedLiteralValue);
    if HasRelativeTo then
    begin
      RelRecord := ParseRelativeToRecord(RelToArg, 'Duration.prototype.total');
      HasZonedRelativeTo := RelRecord.IsZoned;
      RelDate := RelRecord.Date;
    end;

    Arg := OptionsObj.GetProperty('unit');
    if (Arg = nil) or (Arg is TGocciaUndefinedLiteralValue) then
      ThrowRangeError(SErrorDurationTotalRequiresUnit, SSuggestTemporalValidUnits);
    UnitStr := Arg.ToStringLiteral.Value;
  end
  else
  begin
    ThrowTypeError(SErrorDurationTotalRequiresStringOrOptions, SSuggestTemporalValidUnits);
    UnitStr := '';
  end;

  // Validate unit (accepts both singular and plural: 'hour'/'hours', 'day'/'days', etc.)
  if not GetTemporalUnitFromString(UnitStr, TargetUnit) or
     (TargetUnit = tuAuto) or (TargetUnit = tuNone) then
  begin
    ThrowRangeError(Format(SErrorTemporalInvalidUnitFor, ['Duration.prototype.total', UnitStr]), SSuggestTemporalValidUnits);
    TargetUnit := tuNanosecond;
  end;

  if HasRelativeTo and (not HasZonedRelativeTo) and
     (IsDurationDateUnit(TargetUnit) or DurationHasDateUnits(D)) then
    ValidateDurationTotalPlainDateRange(D, RelRecord.CalendarId, RelDate,
      'Duration.prototype.total');
  if HasRelativeTo and (not HasZonedRelativeTo) and
     (not IsDurationDateUnit(TargetUnit)) and (D.ComputeSign <> 0) and
     DurationDateBefore(RelDate.Year, RelDate.Month, RelDate.Day,
       -271821, 4, 20) then
    ThrowRangeError(Format(SErrorTemporalInvalidRelativeTo,
      ['Duration.prototype.total']), SSuggestTemporalRelativeTo);

  if HasZonedRelativeTo then
  begin
    ValidateDurationTotalZonedRange(D, RelRecord, TargetUnit,
      'Duration.prototype.total');
    Result := TGocciaNumberLiteralValue.Create(DurationTotalZoned(D,
      RelRecord, TargetUnit));
    Exit;
  end;

  // Calendar target units always require relativeTo.
  if (TargetUnit = tuMonth) or (TargetUnit = tuYear) then
  begin
    if not HasRelativeTo then
      ThrowRangeError(SErrorDurationTotalRequiresRelativeTo, SSuggestTemporalRelativeTo);
    if DurationExactTimeZonedEndOutOfRange(D, RelToArg,
      'Duration.prototype.total') then
      ThrowRangeError(Format(SErrorTemporalInvalidRelativeTo,
        ['Duration.prototype.total']), SSuggestTemporalRelativeTo);
    if TargetUnit = tuMonth then
      Result := TotalInMonths(D, RelRecord.CalendarId, RelDate)
    else
      Result := TotalInYears(D, RelRecord.CalendarId, RelDate);
    Exit;
  end;
  if (TargetUnit = tuWeek) and (not HasRelativeTo) then
    ThrowRangeError(SErrorDurationTotalRequiresRelativeTo, SSuggestTemporalRelativeTo);

  // Calendar source components (years/months/weeks) require relativeTo.
  if DurationHasCalendarUnits(D) then
  begin
    if not HasRelativeTo then
      ThrowRangeError(SErrorDurationTotalRequiresRelativeTo, SSuggestTemporalRelativeTo);

    ResolvedDays := ResolveRelativeDays(RelRecord.CalendarId, RelDate,
      BigIntToCheckedInt64(D.FYearsBig, 'years'),
      BigIntToCheckedInt64(D.FMonthsBig, 'months'),
      BigIntToCheckedInt64(D.FWeeksBig, 'weeks'),
      BigIntToCheckedInt64(D.FDaysBig, 'days'));

    TotalNsBig := DurationSubDayNanosecondsBig(D)
      .Add(BigIntUnit(ResolvedDays).Multiply(BigIntUnit(NANOSECONDS_PER_DAY)));
  end
  else
    TotalNsBig := DurationTotalNanosecondsBig(D);

  if TargetUnit = tuNanosecond then
    Result := TGocciaNumberLiteralValue.Create(TotalNsBig.ToDouble)
  else if TargetUnit = tuWeek then
    Result := TGocciaNumberLiteralValue.Create(
      TotalNanosecondsToUnitDouble(TotalNsBig, 7 * NANOSECONDS_PER_DAY))
  else
    Result := TGocciaNumberLiteralValue.Create(
      TotalNanosecondsToUnitDouble(TotalNsBig,
        UnitToNanoseconds(TargetUnit)));
  except
    on E: ETemporalDurationInt64Overflow do
      ThrowRangeError(E.Message, SSuggestTemporalDurationRange);
  end;
end;

function DurationToStringOptionsObject(const AValue: TGocciaValue;
  const AMethod: string): TGocciaObjectValue;
begin
  if DurationIsUndefinedValue(AValue) then
    Exit(nil);
  if AValue is TGocciaObjectValue then
    Exit(TGocciaObjectValue(AValue));
  ThrowTypeError(AMethod + ' options must be an object',
    SSuggestTemporalRoundArg);
  Result := nil;
end;

function DurationFractionalSecondDigitsOption(
  const AOptions: TGocciaObjectValue): Integer;
var
  V: TGocciaValue;
  NumberValue: Double;
  StringValue: string;
begin
  Result := -1;
  if AOptions = nil then
    Exit;

  V := AOptions.GetProperty('fractionalSecondDigits');
  if DurationIsUndefinedValue(V) then
    Exit;

  if V is TGocciaNumberLiteralValue then
  begin
    NumberValue := TGocciaNumberLiteralValue(V).Value;
    if IsNaN(NumberValue) or IsInfinite(NumberValue) then
      ThrowRangeError(SErrorFractionalDigitsRange, SSuggestTemporalRoundArg);
    NumberValue := Floor(NumberValue);
    if (NumberValue < 0) or (NumberValue > 9) then
      ThrowRangeError(SErrorFractionalDigitsRange, SSuggestTemporalRoundArg);
    Result := Trunc(NumberValue);
    Exit;
  end;

  StringValue := V.ToStringLiteral.Value;
  if StringValue = 'auto' then
    Result := -1
  else if (Length(StringValue) = 1) and (StringValue[1] in ['0'..'9']) then
    Result := Ord(StringValue[1]) - Ord('0')
  else
    ThrowRangeError(Format(SErrorInvalidFractionalDigits, [StringValue]),
      SSuggestTemporalRoundArg);
end;

function DurationRoundingModeOption(
  const AOptions: TGocciaObjectValue): TTemporalRoundingMode;
var
  V: TGocciaValue;
  StringValue: string;
begin
  Result := rmTrunc;
  if AOptions = nil then
    Exit;
  V := AOptions.GetProperty('roundingMode');
  if DurationIsUndefinedValue(V) then
    Exit;
  StringValue := V.ToStringLiteral.Value;
  if not GetRoundingModeFromString(StringValue, Result) then
    ThrowRangeError(Format(SErrorInvalidRoundingMode, [StringValue]),
      SSuggestTemporalRoundingMode);
end;

function DurationDigitsForSmallestUnit(const AOptions: TGocciaObjectValue;
  const AFractionalDigits: Integer): Integer;
var
  V: TGocciaValue;
  SmallestUnit: TTemporalUnit;
  SmallestUnitValue: string;
begin
  Result := AFractionalDigits;
  if AOptions = nil then
    Exit;
  V := AOptions.GetProperty('smallestUnit');
  if DurationIsUndefinedValue(V) then
    Exit;
  SmallestUnitValue := V.ToStringLiteral.Value;
  if (not GetTemporalUnitFromString(SmallestUnitValue, SmallestUnit)) or
     not (SmallestUnit in [tuSecond, tuMillisecond, tuMicrosecond, tuNanosecond]) then
    ThrowRangeError(Format(SErrorInvalidSmallestUnit, [SmallestUnitValue]),
      SSuggestTemporalValidUnits);
  case SmallestUnit of
    tuSecond: Result := 0;
    tuMillisecond: Result := 3;
    tuMicrosecond: Result := 6;
    tuNanosecond: Result := 9;
  else
    Result := AFractionalDigits;
  end;
end;

procedure ReadDurationToStringOptions(const AOptions: TGocciaObjectValue;
  out AFractionalDigits: Integer; out ARoundingMode: TTemporalRoundingMode);
begin
  AFractionalDigits := DurationFractionalSecondDigitsOption(AOptions);
  ARoundingMode := DurationRoundingModeOption(AOptions);
  AFractionalDigits := DurationDigitsForSmallestUnit(AOptions,
    AFractionalDigits);
end;

function FormatDurationFieldsISOString(const D: TGocciaTemporalDurationValue;
  const AFractionalDigits: Integer): string;
var
  DatePart, TimePart, Fraction: string;
  Sign: Integer;
  AbsY, AbsMo, AbsW, AbsD, AbsH, AbsMi: TBigInteger;
  SecondsDuration, AbsSecondsDuration, SecondsPart, SubSecondsPart: TBigInteger;
  IncludeSeconds: Boolean;
begin
  Sign := D.ComputeSign;
  AbsY := D.FYearsBig.AbsValue;
  AbsMo := D.FMonthsBig.AbsValue;
  AbsW := D.FWeeksBig.AbsValue;
  AbsD := D.FDaysBig.AbsValue;
  AbsH := D.FHoursBig.AbsValue;
  AbsMi := D.FMinutesBig.AbsValue;

  DatePart := '';
  if not AbsY.IsZero then DatePart := DatePart + AbsY.ToString + 'Y';
  if not AbsMo.IsZero then DatePart := DatePart + AbsMo.ToString + 'M';
  if not AbsW.IsZero then DatePart := DatePart + AbsW.ToString + 'W';
  if not AbsD.IsZero then DatePart := DatePart + AbsD.ToString + 'D';

  TimePart := '';
  if not AbsH.IsZero then TimePart := TimePart + AbsH.ToString + 'H';
  if not AbsMi.IsZero then TimePart := TimePart + AbsMi.ToString + 'M';

  SecondsDuration := TimeDurationFromComponents(TBigInteger.Zero,
    TBigInteger.Zero, D.FSecondsBig, D.FMillisecondsBig,
    D.FMicrosecondsBig, D.FNanosecondsBig);
  IncludeSeconds := (not SecondsDuration.IsZero) or
    ((DatePart = '') and (TimePart = '')) or (AFractionalDigits >= 0);
  if IncludeSeconds then
  begin
    AbsSecondsDuration := SecondsDuration.AbsValue;
    SecondsPart := AbsSecondsDuration.Divide(BigIntUnit(NANOSECONDS_PER_SECOND));
    SubSecondsPart := AbsSecondsDuration.Modulo(BigIntUnit(NANOSECONDS_PER_SECOND));
    TimePart := TimePart + SecondsPart.ToString;
    if AFractionalDigits < 0 then
    begin
      if not SubSecondsPart.IsZero then
      begin
        Fraction := Format('%.9d', [SubSecondsPart.ToInt64]);
        while (Length(Fraction) > 0) and (Fraction[Length(Fraction)] = '0') do
          Delete(Fraction, Length(Fraction), 1);
        if Length(Fraction) > 0 then
          TimePart := TimePart + '.' + Fraction;
      end;
    end
    else if AFractionalDigits > 0 then
    begin
      Fraction := Format('%.9d', [SubSecondsPart.ToInt64]);
      TimePart := TimePart + '.' + Copy(Fraction, 1, AFractionalDigits);
    end;
    TimePart := TimePart + 'S';
  end;

  Result := '';
  if Sign < 0 then
    Result := '-';
  Result := Result + 'P' + DatePart;
  if Length(TimePart) > 0 then
    Result := Result + 'T' + TimePart;
end;

function RoundDurationForToString(const D: TGocciaTemporalDurationValue;
  const AFractionalDigits: Integer;
  const ARoundingMode: TTemporalRoundingMode): TGocciaTemporalDurationValue;
var
  Fields: TDurationFieldsBig;
  TimeDuration, RoundedTime, Divisor: TBigInteger;
  Hours, Minutes, Seconds, Milliseconds, Microseconds, Nanoseconds: TBigInteger;
  ExtraDays: Int64;
begin
  if (AFractionalDigits < 0) or (AFractionalDigits = 9) then
    Exit(D);

  TimeDuration := DurationSubDayNanosecondsBig(D);
  Divisor := BigIntPowerOfTen(9 - AFractionalDigits);
  RoundedTime := RoundTimeDurationToIncrement(TimeDuration, Divisor,
    ARoundingMode);
  BalanceTimeDurationToFields(RoundedTime, DurationDefaultLargestUnit(D),
    Hours, Minutes, Seconds, Milliseconds, Microseconds, Nanoseconds,
    ExtraDays);

  Fields := DurationFieldsFromValue(D);
  Fields.Days := Fields.Days.Add(BigIntUnit(ExtraDays));
  Fields.Hours := Hours;
  Fields.Minutes := Minutes;
  Fields.Seconds := Seconds;
  Fields.Milliseconds := Milliseconds;
  Fields.Microseconds := Microseconds;
  Fields.Nanoseconds := Nanoseconds;

  Result := TGocciaTemporalDurationValue.CreateFromBigIntegers(
    Fields.Years, Fields.Months, Fields.Weeks, Fields.Days, Fields.Hours,
    Fields.Minutes, Fields.Seconds, Fields.Milliseconds, Fields.Microseconds,
    Fields.Nanoseconds);
end;

function DurationISOStringWithOptions(const D: TGocciaTemporalDurationValue;
  const AOptions: TGocciaObjectValue): string;
var
  FractionalDigits: Integer;
  RoundingMode: TTemporalRoundingMode;
  RoundedDuration: TGocciaTemporalDurationValue;
begin
  ReadDurationToStringOptions(AOptions, FractionalDigits, RoundingMode);
  RoundedDuration := RoundDurationForToString(D, FractionalDigits,
    RoundingMode);
  Result := FormatDurationFieldsISOString(RoundedDuration, FractionalDigits);
end;

function TGocciaTemporalDurationValue.DurationToString(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalDurationValue;
  OptionsObj: TGocciaObjectValue;
begin
  D := AsDuration(AThisValue, 'Duration.prototype.toString');
  OptionsObj := DurationToStringOptionsObject(AArgs.GetElement(0),
    'Duration.prototype.toString');
  Result := TGocciaStringLiteralValue.Create(
    DurationISOStringWithOptions(D, OptionsObj));
end;

function TGocciaTemporalDurationValue.DurationToJSON(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaStringLiteralValue.Create(
    AsDuration(AThisValue, 'Duration.prototype.toJSON').ToISOString);
end;

function TGocciaTemporalDurationValue.DurationValueOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  ThrowTypeError(Format(SErrorTemporalValueOf, ['Duration', 'toString or compare']), SSuggestTemporalNoValueOf);
  Result := nil;
end;

function TGocciaTemporalDurationValue.DurationToLocaleString(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalDurationValue;
begin
  D := AsDuration(AThisValue, 'Duration.prototype.toLocaleString');
  Result := FormatTemporalDurationFieldsToLocaleString(AArgs.GetElement(0),
    AArgs.GetElement(1), D.YearsBig.ToDouble, D.MonthsBig.ToDouble,
    D.WeeksBig.ToDouble, D.DaysBig.ToDouble, D.HoursBig.ToDouble,
    D.MinutesBig.ToDouble, D.SecondsBig.ToDouble,
    D.MillisecondsBig.ToDouble, D.MicrosecondsBig.ToDouble,
    D.NanosecondsBig.ToDouble);
end;

initialization
  RegisterThreadvarCleanup(@ClearThreadvarMembers);
  GTemporalDurationSharedSlot := RegisterRealmOwnedSlot('Temporal.Duration.shared');

end.
