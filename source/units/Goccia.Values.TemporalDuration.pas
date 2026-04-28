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

implementation

uses
  Math,

  Goccia.Constants.NumericLimits,
  Goccia.Constants.PropertyNames,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.Realm,
  Goccia.Temporal.DurationMath,
  Goccia.Temporal.Options,
  Goccia.Temporal.Utils,
  Goccia.Values.ErrorHelper,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.SymbolValue,
  Goccia.Values.TemporalPlainDate,
  Goccia.Values.TemporalZonedDateTime;

var
  GTemporalDurationSharedSlot: TGocciaRealmOwnedSlotId;

threadvar
  FPrototypeMembers: TArray<TGocciaMemberDefinition>;

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
function ValueToBigInteger(const AValue: TGocciaValue): TBigInteger; forward;

function DurationFromObject(const AObj: TGocciaObjectValue): TGocciaTemporalDurationValue;

  function GetFieldOr(const AObj: TGocciaObjectValue; const AName: string;
    const ADefault: TBigInteger): TBigInteger;
  var
    V: TGocciaValue;
  begin
    V := AObj.GetProperty(AName);
    if (V = nil) or (V is TGocciaUndefinedLiteralValue) then
      Result := ADefault
    else
      Result := ValueToBigInteger(V);
  end;

begin
  Result := TGocciaTemporalDurationValue.CreateFromBigIntegers(
    GetFieldOr(AObj, 'years', TBigInteger.Zero),
    GetFieldOr(AObj, 'months', TBigInteger.Zero),
    GetFieldOr(AObj, 'weeks', TBigInteger.Zero),
    GetFieldOr(AObj, 'days', TBigInteger.Zero),
    GetFieldOr(AObj, 'hours', TBigInteger.Zero),
    GetFieldOr(AObj, 'minutes', TBigInteger.Zero),
    GetFieldOr(AObj, 'seconds', TBigInteger.Zero),
    GetFieldOr(AObj, 'milliseconds', TBigInteger.Zero),
    GetFieldOr(AObj, 'microseconds', TBigInteger.Zero),
    GetFieldOr(AObj, 'nanoseconds', TBigInteger.Zero)
  );
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
var
  DecStr: string;
begin
  if IsNaN(AValue) or IsInfinite(AValue) then
    ThrowRangeError('Temporal.Duration field must be a finite integer',
      SSuggestTemporalDurationRange);
  if Frac(AValue) <> 0 then
    ThrowRangeError('Temporal.Duration field must be an integer',
      SSuggestTemporalDurationRange);
  Str(AValue:0:0, DecStr);
  Result := TBigInteger.FromDecimalString(DecStr);
end;

function ValueToBigInteger(const AValue: TGocciaValue): TBigInteger;
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
  DurRec: TTemporalDurationRecord;
begin
  D := AsDuration(AThisValue, 'Duration.prototype.add');
  Arg := AArgs.GetElement(0);

  if Arg is TGocciaTemporalDurationValue then
    Other := TGocciaTemporalDurationValue(Arg)
  else if Arg is TGocciaStringLiteralValue then
  begin
    Other := nil;
    // Parse ISO duration string
    if TryParseISODuration(TGocciaStringLiteralValue(Arg).Value, DurRec) then
      Other := TGocciaTemporalDurationValue.Create(
        DurRec.Years, DurRec.Months, DurRec.Weeks, DurRec.Days,
        DurRec.Hours, DurRec.Minutes, DurRec.Seconds,
        DurRec.Milliseconds, DurRec.Microseconds, DurRec.Nanoseconds)
    else
      ThrowRangeError(SErrorInvalidDurationString, SSuggestTemporalDurationArg);
  end
  else if Arg is TGocciaObjectValue then
    Other := DurationFromObject(TGocciaObjectValue(Arg))
  else
  begin
    ThrowTypeError(SErrorInvalidDurationAddArg, SSuggestTemporalDurationArg);
    Other := nil;
  end;

  Result := TGocciaTemporalDurationValue.CreateFromBigIntegers(
    D.FYearsBig.Add(Other.FYearsBig), D.FMonthsBig.Add(Other.FMonthsBig),
    D.FWeeksBig.Add(Other.FWeeksBig), D.FDaysBig.Add(Other.FDaysBig),
    D.FHoursBig.Add(Other.FHoursBig), D.FMinutesBig.Add(Other.FMinutesBig),
    D.FSecondsBig.Add(Other.FSecondsBig), D.FMillisecondsBig.Add(Other.FMillisecondsBig),
    D.FMicrosecondsBig.Add(Other.FMicrosecondsBig), D.FNanosecondsBig.Add(Other.FNanosecondsBig));
end;

function TGocciaTemporalDurationValue.DurationSubtract(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D, Other: TGocciaTemporalDurationValue;
  Arg: TGocciaValue;
  DurRec: TTemporalDurationRecord;
begin
  D := AsDuration(AThisValue, 'Duration.prototype.subtract');
  Arg := AArgs.GetElement(0);

  if Arg is TGocciaTemporalDurationValue then
    Other := TGocciaTemporalDurationValue(Arg)
  else if Arg is TGocciaStringLiteralValue then
  begin
    Other := nil;
    if TryParseISODuration(TGocciaStringLiteralValue(Arg).Value, DurRec) then
      Other := TGocciaTemporalDurationValue.Create(
        DurRec.Years, DurRec.Months, DurRec.Weeks, DurRec.Days,
        DurRec.Hours, DurRec.Minutes, DurRec.Seconds,
        DurRec.Milliseconds, DurRec.Microseconds, DurRec.Nanoseconds)
    else
      ThrowRangeError(SErrorInvalidDurationString, SSuggestTemporalDurationArg);
  end
  else if Arg is TGocciaObjectValue then
    Other := DurationFromObject(TGocciaObjectValue(Arg))
  else
  begin
    ThrowTypeError(SErrorInvalidDurationSubtractArg, SSuggestTemporalDurationArg);
    Other := nil;
  end;

  Result := TGocciaTemporalDurationValue.CreateFromBigIntegers(
    D.FYearsBig.Subtract(Other.FYearsBig), D.FMonthsBig.Subtract(Other.FMonthsBig),
    D.FWeeksBig.Subtract(Other.FWeeksBig), D.FDaysBig.Subtract(Other.FDaysBig),
    D.FHoursBig.Subtract(Other.FHoursBig), D.FMinutesBig.Subtract(Other.FMinutesBig),
    D.FSecondsBig.Subtract(Other.FSecondsBig), D.FMillisecondsBig.Subtract(Other.FMillisecondsBig),
    D.FMicrosecondsBig.Subtract(Other.FMicrosecondsBig), D.FNanosecondsBig.Subtract(Other.FNanosecondsBig));
end;

function TGocciaTemporalDurationValue.DurationWith(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalDurationValue;
  Obj: TGocciaObjectValue;
  V: TGocciaValue;
  NewYears, NewMonths, NewWeeks, NewDays, NewHours, NewMinutes, NewSeconds,
  NewMilliseconds, NewMicroseconds, NewNanoseconds: TBigInteger;

  function GetFieldOr(const AName: string; const ADefault: TBigInteger): TBigInteger;
  var
    Val: TGocciaValue;
  begin
    Val := Obj.GetProperty(AName);
    if (Val = nil) or (Val is TGocciaUndefinedLiteralValue) then
      Result := ADefault
    else
      Result := ValueToBigInteger(Val);
  end;

begin
  D := AsDuration(AThisValue, 'Duration.prototype.with');
  V := AArgs.GetElement(0);
  if not (V is TGocciaObjectValue) then
    ThrowTypeError(Format(SErrorTemporalWithRequiresObject, ['Duration']), SSuggestTemporalWithObject);
  Obj := TGocciaObjectValue(V);

  NewYears := GetFieldOr('years', D.FYearsBig);
  NewMonths := GetFieldOr('months', D.FMonthsBig);
  NewWeeks := GetFieldOr('weeks', D.FWeeksBig);
  NewDays := GetFieldOr('days', D.FDaysBig);
  NewHours := GetFieldOr('hours', D.FHoursBig);
  NewMinutes := GetFieldOr('minutes', D.FMinutesBig);
  NewSeconds := GetFieldOr('seconds', D.FSecondsBig);
  NewMilliseconds := GetFieldOr('milliseconds', D.FMillisecondsBig);
  NewMicroseconds := GetFieldOr('microseconds', D.FMicrosecondsBig);
  NewNanoseconds := GetFieldOr('nanoseconds', D.FNanosecondsBig);

  Result := TGocciaTemporalDurationValue.CreateFromBigIntegers(
    NewYears, NewMonths, NewWeeks, NewDays, NewHours, NewMinutes, NewSeconds,
    NewMilliseconds, NewMicroseconds, NewNanoseconds);
end;

// TC39 Temporal §7.3.21 Temporal.Duration.prototype.round(roundTo)
function TGocciaTemporalDurationValue.DurationRound(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalDurationValue;
  Arg, RelToArg: TGocciaValue;
  OptionsObj: TGocciaObjectValue;
  SmallestUnit, LargestUnit: TTemporalUnit;
  Mode: TTemporalRoundingMode;
  Increment: Integer;
  HasRelativeTo: Boolean;
  RelDate: TTemporalDateRecord;
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
begin
  try
  D := AsDuration(AThisValue, 'Duration.prototype.round');
  Arg := AArgs.GetElement(0);

  SmallestUnit := tuNone;
  LargestUnit := tuNone;
  Mode := rmHalfExpand;
  Increment := 1;
  HasRelativeTo := False;

  if Arg is TGocciaStringLiteralValue then
  begin
    if not GetTemporalUnitFromString(TGocciaStringLiteralValue(Arg).Value, SmallestUnit) then
      ThrowRangeError(Format(SErrorTemporalInvalidUnitFor, ['Duration.prototype.round', TGocciaStringLiteralValue(Arg).Value]), SSuggestTemporalValidUnits);
  end
  else if Arg is TGocciaObjectValue then
  begin
    OptionsObj := TGocciaObjectValue(Arg);
    SmallestUnit := GetSmallestUnit(OptionsObj, tuNone);
    LargestUnit := GetLargestUnit(OptionsObj, tuNone);
    Mode := GetRoundingMode(OptionsObj, rmHalfExpand);
    Increment := GetRoundingIncrement(OptionsObj, 1);

    RelToArg := OptionsObj.GetProperty('relativeTo');
    if (RelToArg <> nil) and not (RelToArg is TGocciaUndefinedLiteralValue) then
    begin
      RelDate := ParseRelativeTo(RelToArg, 'Duration.prototype.round');
      HasRelativeTo := True;
    end;
  end
  else
    ThrowTypeError(Format(SErrorTemporalRoundRequiresStringOrOptions, ['Duration']), SSuggestTemporalRoundArg);

  // Validate units
  if (SmallestUnit = tuNone) and (LargestUnit = tuNone) then
    ThrowRangeError(SErrorDurationRoundRequiresUnit, SSuggestTemporalRoundArg);
  if SmallestUnit = tuNone then
    SmallestUnit := tuNanosecond;
  if LargestUnit = tuNone then
  begin
    if not D.FYearsBig.IsZero then LargestUnit := tuYear
    else if not D.FMonthsBig.IsZero then LargestUnit := tuMonth
    else if not D.FWeeksBig.IsZero then LargestUnit := tuWeek
    else if not D.FDaysBig.IsZero then LargestUnit := tuDay
    else if not D.FHoursBig.IsZero then LargestUnit := tuHour
    else if not D.FMinutesBig.IsZero then LargestUnit := tuMinute
    else if not D.FSecondsBig.IsZero then LargestUnit := tuSecond
    else if not D.FMillisecondsBig.IsZero then LargestUnit := tuMillisecond
    else if not D.FMicrosecondsBig.IsZero then LargestUnit := tuMicrosecond
    else LargestUnit := SmallestUnit;
    if Ord(LargestUnit) > Ord(SmallestUnit) then
      LargestUnit := SmallestUnit;
  end;

  if Ord(LargestUnit) > Ord(SmallestUnit) then
    ThrowRangeError(SErrorDurationRoundLargestSmallerThanSmallest, SSuggestTemporalRoundArg);

  // Determine if calendar-relative computation is needed
  NeedCalendar := (not D.FYearsBig.IsZero) or (not D.FMonthsBig.IsZero) or
    (Ord(SmallestUnit) <= Ord(tuMonth)) or (Ord(LargestUnit) <= Ord(tuMonth));

  if NeedCalendar and not HasRelativeTo then
    ThrowRangeError(SErrorDurationRoundRequiresRelativeTo, SSuggestTemporalRelativeTo);

  // --- Calendar-relative path ---
  if NeedCalendar then
  begin
    Sign := D.ComputeSign;
    if Sign = 0 then
    begin
      Result := TGocciaTemporalDurationValue.Create(0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
      Exit;
    end;

    // Resolve duration to concrete end date. Apply years and months as
    // separate calendar steps so day-clamping is applied after each,
    // matching TC39 Temporal semantics (e.g. 2020-02-29 + P1Y1M clamps to
    // 2021-02-28 after +1Y, then advances to 2021-03-28).
    CalendarYears := BigIntToCheckedInt64(D.FYearsBig, 'years');
    CalendarMonths := BigIntToCheckedInt64(D.FMonthsBig, 'months');
    CalendarWeeks := BigIntToCheckedInt64(D.FWeeksBig, 'weeks');
    CalendarDays := BigIntToCheckedInt64(D.FDaysBig, 'days');
    IntermDate := RelDate;
    if not D.FYearsBig.IsZero then
      IntermDate := AddMonthsToDate(IntermDate.Year, IntermDate.Month, IntermDate.Day,
        CalendarYears * 12);
    if not D.FMonthsBig.IsZero then
      IntermDate := AddMonthsToDate(IntermDate.Year, IntermDate.Month, IntermDate.Day,
        CalendarMonths);
    TimeNsBig := DurationSubDayNanosecondsBig(D);
    CarryDaysBig := TimeNsBig.Divide(BigIntUnit(NANOSECONDS_PER_DAY));
    TimeNsBig := TimeNsBig.Modulo(BigIntUnit(NANOSECONDS_PER_DAY));
    CarryDays := BigIntToCheckedInt64(CarryDaysBig, 'carry days');
    TimeNs := BigIntToCheckedInt64(TimeNsBig, 'sub-day nanoseconds');

    EndDate := AddDaysToDate(IntermDate.Year, IntermDate.Month, IntermDate.Day,
      CalendarWeeks * 7 + CalendarDays + CarryDays);
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
            NextDate := AddMonthsToDate(RelDate.Year, RelDate.Month, RelDate.Day, (WholeUnits + 1) * 12);
            NextEpoch := DateToEpochDays(NextDate.Year, NextDate.Month, NextDate.Day);
            if (NextEpoch > EndEpoch) or ((NextEpoch = EndEpoch) and (TimeNs <= 0)) then
              Break;
            Inc(WholeUnits);
          until False;
        end
        else
        begin
          repeat
            NextDate := AddMonthsToDate(RelDate.Year, RelDate.Month, RelDate.Day, (WholeUnits - 1) * 12);
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

        WalkDate := AddMonthsToDate(RelDate.Year, RelDate.Month, RelDate.Day, ScaledValue * 12);
        WalkEpoch := DateToEpochDays(WalkDate.Year, WalkDate.Month, WalkDate.Day);
        NextDate := AddMonthsToDate(RelDate.Year, RelDate.Month, RelDate.Day,
          (ScaledValue + Sign * Increment) * 12);
        NextEpoch := DateToEpochDays(NextDate.Year, NextDate.Month, NextDate.Day);
        PeriodNs := Abs(NextEpoch - WalkEpoch) * NANOSECONDS_PER_DAY;
        RoundedValue := RoundWithMode(
          Abs((EndEpoch - WalkEpoch) * NANOSECONDS_PER_DAY + TimeNs), PeriodNs, Mode);
        if RoundedValue >= PeriodNs then
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
            NextDate := AddMonthsToDate(RelDate.Year, RelDate.Month, RelDate.Day, WholeUnits + 1);
            NextEpoch := DateToEpochDays(NextDate.Year, NextDate.Month, NextDate.Day);
            if (NextEpoch > EndEpoch) or ((NextEpoch = EndEpoch) and (TimeNs <= 0)) then
              Break;
            Inc(WholeUnits);
          until False;
        end
        else
        begin
          repeat
            NextDate := AddMonthsToDate(RelDate.Year, RelDate.Month, RelDate.Day, WholeUnits - 1);
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

        WalkDate := AddMonthsToDate(RelDate.Year, RelDate.Month, RelDate.Day, ScaledValue);
        WalkEpoch := DateToEpochDays(WalkDate.Year, WalkDate.Month, WalkDate.Day);
        NextDate := AddMonthsToDate(RelDate.Year, RelDate.Month, RelDate.Day,
          ScaledValue + Sign * Increment);
        NextEpoch := DateToEpochDays(NextDate.Year, NextDate.Month, NextDate.Day);
        PeriodNs := Abs(NextEpoch - WalkEpoch) * NANOSECONDS_PER_DAY;
        RoundedValue := RoundWithMode(
          Abs((EndEpoch - WalkEpoch) * NANOSECONDS_PER_DAY + TimeNs), PeriodNs, Mode);
        if RoundedValue >= PeriodNs then
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

    if SmallestUnit <> tuNanosecond then
    begin
      DivisorBig := RoundingDivisorBig(SmallestUnit, Increment);
      TotalNsBig := RoundTimeDurationToIncrement(TotalNsBig, DivisorBig, Mode);
    end;

    // --- Rebalance to year/month if needed ---
    if Ord(LargestUnit) <= Ord(tuMonth) then
    begin
      ResultDays := BigIntToCheckedInt64(
        TotalNsBig.Divide(BigIntUnit(NANOSECONDS_PER_DAY)), 'days');
      RemNs := BigIntToCheckedInt64(
        TotalNsBig.Modulo(BigIntUnit(NANOSECONDS_PER_DAY)), 'sub-day nanoseconds');

      EndEpoch := StartEpoch + ResultDays;
      WholeUnits := 0;
      if Sign > 0 then
      begin
        repeat
          NextDate := AddMonthsToDate(RelDate.Year, RelDate.Month, RelDate.Day, WholeUnits + 1);
          NextEpoch := DateToEpochDays(NextDate.Year, NextDate.Month, NextDate.Day);
          if NextEpoch > EndEpoch then Break;
          Inc(WholeUnits);
        until False;
      end
      else
      begin
        repeat
          NextDate := AddMonthsToDate(RelDate.Year, RelDate.Month, RelDate.Day, WholeUnits - 1);
          NextEpoch := DateToEpochDays(NextDate.Year, NextDate.Month, NextDate.Day);
          if NextEpoch < EndEpoch then Break;
          Dec(WholeUnits);
        until False;
      end;

      WalkDate := AddMonthsToDate(RelDate.Year, RelDate.Month, RelDate.Day, WholeUnits);
      WalkEpoch := DateToEpochDays(WalkDate.Year, WalkDate.Month, WalkDate.Day);
      ResultDays := EndEpoch - WalkEpoch;

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

      if Ord(LargestUnit) <= Ord(tuWeek) then
        Result := TGocciaTemporalDurationValue.Create(ResultYears, ResultMonths,
          ResultDays div 7, ResultDays mod 7,
          ResultHours, ResultMinutes, ResultSeconds, ResultMs, ResultUs, ResultNs)
      else
        Result := TGocciaTemporalDurationValue.Create(ResultYears, ResultMonths, 0, ResultDays,
          ResultHours, ResultMinutes, ResultSeconds, ResultMs, ResultUs, ResultNs);
      Exit;
    end;
    // Fall through to non-calendar breakdown below
  end
  else
  begin
    TotalNsBig := DurationTotalNanosecondsBig(D);

    if SmallestUnit <> tuNanosecond then
    begin
      DivisorBig := RoundingDivisorBig(SmallestUnit, Increment);
      TotalNsBig := RoundTimeDurationToIncrement(TotalNsBig, DivisorBig, Mode);
    end;
  end;

  BalanceTimeDurationToFields(TotalNsBig, LargestUnit, ResultHoursBig,
    ResultMinutesBig, ResultSecondsBig, ResultMsBig, ResultUsBig, ResultNsBig,
    ResultDays);

  if Ord(LargestUnit) <= Ord(tuWeek) then
    Result := TGocciaTemporalDurationValue.CreateFromBigIntegers(TBigInteger.Zero, TBigInteger.Zero,
      BigIntUnit(ResultDays div 7), BigIntUnit(ResultDays mod 7),
      ResultHoursBig, ResultMinutesBig, ResultSecondsBig, ResultMsBig, ResultUsBig, ResultNsBig)
  else
    Result := TGocciaTemporalDurationValue.CreateFromBigIntegers(TBigInteger.Zero, TBigInteger.Zero,
      TBigInteger.Zero, BigIntUnit(ResultDays),
      ResultHoursBig, ResultMinutesBig, ResultSecondsBig, ResultMsBig, ResultUsBig, ResultNsBig);
  except
    on E: ETemporalDurationInt64Overflow do
      ThrowRangeError(E.Message, SSuggestTemporalDurationRange);
  end;
end;

function TryParseDateFromPropertyBag(const AObj: TGocciaObjectValue;
  out ADate: TTemporalDateRecord): Boolean;
var
  V: TGocciaValue;
begin
  Result := False;
  V := AObj.GetProperty(PROP_YEAR);
  if (V = nil) or (V is TGocciaUndefinedLiteralValue) then
    Exit;
  ADate.Year := Trunc(V.ToNumberLiteral.Value);
  V := AObj.GetProperty(PROP_MONTH);
  if (V = nil) or (V is TGocciaUndefinedLiteralValue) then
    Exit;
  ADate.Month := Trunc(V.ToNumberLiteral.Value);
  V := AObj.GetProperty(PROP_DAY);
  if (V = nil) or (V is TGocciaUndefinedLiteralValue) then
    Exit;
  ADate.Day := Trunc(V.ToNumberLiteral.Value);
  if not IsValidDate(ADate.Year, ADate.Month, ADate.Day) then
    Exit;
  Result := True;
end;

function ParseRelativeTo(const AValue: TGocciaValue; const AMethod: string): TTemporalDateRecord;
var
  DateRec: TTemporalDateRecord;
  PlainDate: TGocciaTemporalPlainDateValue;
begin
  if AValue is TGocciaTemporalPlainDateValue then
  begin
    PlainDate := TGocciaTemporalPlainDateValue(AValue);
    Result.Year := PlainDate.Year;
    Result.Month := PlainDate.Month;
    Result.Day := PlainDate.Day;
  end
  else if AValue is TGocciaTemporalZonedDateTimeValue then
  begin
    // ZonedDateTime relativeTo needs timezone-aware DST handling. Reject it
    // until Duration round/total can route through that aware path.
    ThrowRangeError(Format(SErrorTemporalInvalidRelativeTo, [AMethod]), SSuggestTemporalRelativeTo);
  end
  else if AValue is TGocciaStringLiteralValue then
  begin
    if not CoerceToISODate(TGocciaStringLiteralValue(AValue).Value, DateRec) then
      ThrowRangeError(Format(SErrorTemporalInvalidRelativeTo, [AMethod]), SSuggestTemporalRelativeTo);
    Result := DateRec;
  end
  else if AValue is TGocciaObjectValue then
  begin
    if not TryParseDateFromPropertyBag(TGocciaObjectValue(AValue), DateRec) then
      ThrowRangeError(Format(SErrorTemporalInvalidRelativeTo, [AMethod]), SSuggestTemporalRelativeTo);
    Result := DateRec;
  end
  else
    ThrowRangeError(Format(SErrorTemporalInvalidRelativeTo, [AMethod]), SSuggestTemporalRelativeTo);
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

// Compute the end date of a duration applied from a reference date using
// ISO 8601 calendar arithmetic (add years+months, clamp day, add weeks+days).
function DurationEndDate(const ADate: TTemporalDateRecord;
  const AYears, AMonths, AWeeks, ADays: Int64): TTemporalDateRecord;
var
  Interm: TTemporalDateRecord;
begin
  // Apply years and months as separate steps so the day clamps after each,
  // matching TC39 Temporal calendar arithmetic (e.g. 2020-02-29 + P1Y1M
  // clamps to 2021-02-28 after +1Y, then advances to 2021-03-28).
  Interm := ADate;
  if AYears <> 0 then
    Interm := AddMonthsToDate(Interm, AYears * 12);
  if AMonths <> 0 then
    Interm := AddMonthsToDate(Interm, AMonths);
  Result := AddDaysToDate(Interm.Year, Interm.Month, Interm.Day, AWeeks * 7 + ADays);
end;

// Resolve calendar-relative duration components (years, months, weeks) to a
// concrete day count by walking forward from a reference date.
function ResolveRelativeDays(const ADate: TTemporalDateRecord;
  const AYears, AMonths, AWeeks, ADays: Int64): Int64;
var
  EndRec: TTemporalDateRecord;
begin
  EndRec := DurationEndDate(ADate, AYears, AMonths, AWeeks, ADays);
  Result := DateToEpochDays(EndRec.Year, EndRec.Month, EndRec.Day) -
            DateToEpochDays(ADate.Year, ADate.Month, ADate.Day);
end;

// Express the duration as a fractional month count relative to a reference date.
// Walks forward from the reference date by the duration's calendar components,
// then measures the result in calendar months plus a fractional remainder.
// Shared calendar-walk helper for TotalInMonths and TotalInYears.
// AMonthsPerUnit is 1 for months, 12 for years.
function ComputeTotalInUnits(const D: TGocciaTemporalDurationValue;
  const ARelDate: TTemporalDateRecord; const AMonthsPerUnit: Int64): Double;
var
  EndRec, CheckRec, SpanRec: TTemporalDateRecord;
  WholeUnits, CarryDays: Int64;
  CalendarYears, CalendarMonths, CalendarWeeks, CalendarDays: Int64;
  TotalMonthsDiff: Int64;
  CheckEpoch, EndEpoch, SpanEpoch: Int64;
  RemainingDays, DaysInSpan: Int64;
  RemainderNs, FracDays: Double;
  TimeNsBig, CarryDaysBig, RemainderNsBig: TBigInteger;
begin
  // Fold sub-day time overflow into whole days so the calendar walk is accurate
  TimeNsBig := DurationSubDayNanosecondsBig(D);
  CarryDaysBig := TimeNsBig.Divide(BigIntUnit(NANOSECONDS_PER_DAY));
  RemainderNsBig := TimeNsBig.Modulo(BigIntUnit(NANOSECONDS_PER_DAY));
  CarryDays := BigIntToCheckedInt64(CarryDaysBig, 'carry days');
  RemainderNs := RemainderNsBig.ToDouble;

  CalendarYears := BigIntToCheckedInt64(D.FYearsBig, 'years');
  CalendarMonths := BigIntToCheckedInt64(D.FMonthsBig, 'months');
  CalendarWeeks := BigIntToCheckedInt64(D.FWeeksBig, 'weeks');
  CalendarDays := BigIntToCheckedInt64(D.FDaysBig, 'days');
  EndRec := DurationEndDate(ARelDate, CalendarYears, CalendarMonths,
    CalendarWeeks, CalendarDays + CarryDays);
  EndEpoch := DateToEpochDays(EndRec.Year, EndRec.Month, EndRec.Day);

  // Estimate whole units from the month difference
  TotalMonthsDiff := Int64(EndRec.Year - ARelDate.Year) * 12 +
                     Int64(EndRec.Month - ARelDate.Month);
  WholeUnits := TotalMonthsDiff div AMonthsPerUnit;

  // Verify estimate by walking from the reference date
  CheckRec := AddMonthsToDate(ARelDate, WholeUnits * AMonthsPerUnit);
  CheckEpoch := DateToEpochDays(CheckRec.Year, CheckRec.Month, CheckRec.Day);

  // Adjust if overshot forward
  while (WholeUnits > 0) and (CheckEpoch > EndEpoch) do
  begin
    Dec(WholeUnits);
    CheckRec := AddMonthsToDate(ARelDate, WholeUnits * AMonthsPerUnit);
    CheckEpoch := DateToEpochDays(CheckRec.Year, CheckRec.Month, CheckRec.Day);
  end;
  // Adjust if overshot backward
  while (WholeUnits < 0) and (CheckEpoch < EndEpoch) do
  begin
    Inc(WholeUnits);
    CheckRec := AddMonthsToDate(ARelDate, WholeUnits * AMonthsPerUnit);
    CheckEpoch := DateToEpochDays(CheckRec.Year, CheckRec.Month, CheckRec.Day);
  end;

  // Fractional part: remaining days + sub-day remainder only
  RemainingDays := EndEpoch - CheckEpoch;
  FracDays := RemainingDays + RemainderNs / NANOSECONDS_PER_DAY;

  // Determine the unit span for the fractional calculation
  if FracDays >= 0 then
  begin
    SpanRec := AddMonthsToDate(ARelDate, (WholeUnits + 1) * AMonthsPerUnit);
    SpanEpoch := DateToEpochDays(SpanRec.Year, SpanRec.Month, SpanRec.Day);
    DaysInSpan := SpanEpoch - CheckEpoch;
  end
  else
  begin
    SpanRec := AddMonthsToDate(ARelDate, (WholeUnits - 1) * AMonthsPerUnit);
    SpanEpoch := DateToEpochDays(SpanRec.Year, SpanRec.Month, SpanRec.Day);
    DaysInSpan := CheckEpoch - SpanEpoch;
  end;

  if DaysInSpan > 0 then
    Result := WholeUnits + FracDays / DaysInSpan
  else
    Result := WholeUnits;
end;

function TotalInMonths(const D: TGocciaTemporalDurationValue;
  const ARelDate: TTemporalDateRecord): TGocciaValue;
begin
  Result := TGocciaNumberLiteralValue.Create(ComputeTotalInUnits(D, ARelDate, 1));
end;

// Express the duration as a fractional year count relative to a reference date.
function TotalInYears(const D: TGocciaTemporalDurationValue;
  const ARelDate: TTemporalDateRecord): TGocciaValue;
begin
  Result := TGocciaNumberLiteralValue.Create(ComputeTotalInUnits(D, ARelDate, 12));
end;

function TGocciaTemporalDurationValue.DurationTotal(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalDurationValue;
  UnitStr: string;
  TargetUnit: TTemporalUnit;
  Arg, RelToArg: TGocciaValue;
  OptionsObj: TGocciaObjectValue;
  HasRelativeTo: Boolean;
  RelDate: TTemporalDateRecord;
  ResolvedDays: Int64;
  TotalNsBig: TBigInteger;
begin
  try
  D := AsDuration(AThisValue, 'Duration.prototype.total');
  Arg := AArgs.GetElement(0);
  HasRelativeTo := False;

  if Arg is TGocciaStringLiteralValue then
    UnitStr := TGocciaStringLiteralValue(Arg).Value
  else if Arg is TGocciaObjectValue then
  begin
    OptionsObj := TGocciaObjectValue(Arg);

    Arg := OptionsObj.GetProperty('unit');
    if (Arg = nil) or (Arg is TGocciaUndefinedLiteralValue) then
      ThrowRangeError(SErrorDurationTotalRequiresUnit, SSuggestTemporalValidUnits);
    UnitStr := Arg.ToStringLiteral.Value;

    RelToArg := OptionsObj.GetProperty('relativeTo');
    HasRelativeTo := (RelToArg <> nil) and not (RelToArg is TGocciaUndefinedLiteralValue);
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

  // Calendar target units always require relativeTo
  if (TargetUnit = tuMonth) or (TargetUnit = tuYear) then
  begin
    if not HasRelativeTo then
      ThrowRangeError(SErrorDurationTotalRequiresRelativeTo, SSuggestTemporalRelativeTo);
    RelDate := ParseRelativeTo(RelToArg, 'Duration.prototype.total');
    if TargetUnit = tuMonth then
      Result := TotalInMonths(D, RelDate)
    else
      Result := TotalInYears(D, RelDate);
    Exit;
  end;

  // Calendar source components (years/months) require relativeTo
  if (not D.FYearsBig.IsZero) or (not D.FMonthsBig.IsZero) then
  begin
    if not HasRelativeTo then
      ThrowRangeError(SErrorDurationTotalRequiresRelativeTo, SSuggestTemporalRelativeTo);

    RelDate := ParseRelativeTo(RelToArg, 'Duration.prototype.total');
    ResolvedDays := ResolveRelativeDays(RelDate,
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
      TotalNsBig.ToDouble / (7 * NANOSECONDS_PER_DAY))
  else
    Result := TGocciaNumberLiteralValue.Create(
      TotalNsBig.ToDouble / UnitToNanoseconds(TargetUnit));
  except
    on E: ETemporalDurationInt64Overflow do
      ThrowRangeError(E.Message, SSuggestTemporalDurationRange);
  end;
end;

function TGocciaTemporalDurationValue.DurationToString(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaStringLiteralValue.Create(
    AsDuration(AThisValue, 'Duration.prototype.toString').ToISOString);
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
  EmptyArgs: TGocciaArgumentsCollection;
begin
  EmptyArgs := TGocciaArgumentsCollection.Create([]);
  try
    Result := DurationToString(EmptyArgs, AThisValue);
  finally
    EmptyArgs.Free;
  end;
end;

initialization
  GTemporalDurationSharedSlot := RegisterRealmOwnedSlot('Temporal.Duration.shared');

end.
