unit Goccia.Values.TemporalPlainDateTime;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.ObjectModel,
  Goccia.SharedPrototype,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaTemporalPlainDateTimeValue = class(TGocciaObjectValue)
  private
    FYear: Integer;
    FMonth: Integer;
    FDay: Integer;
    FHour: Integer;
    FMinute: Integer;
    FSecond: Integer;
    FMillisecond: Integer;
    FMicrosecond: Integer;
    FNanosecond: Integer;
    FCalendarId: string;
  published
    function GetCalendarId(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
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
    function GetHour(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetMinute(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetSecond(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetMillisecond(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetMicrosecond(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetNanosecond(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;

    function DateTimeWith(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DateTimeWithPlainTime(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DateTimeAdd(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DateTimeSubtract(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DateTimeUntil(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DateTimeSince(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DateTimeRound(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DateTimeEquals(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DateTimeToString(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DateTimeToJSON(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DateTimeValueOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DateTimeToPlainDate(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DateTimeToPlainTime(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DateTimeToPlainYearMonth(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DateTimeToPlainMonthDay(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DateTimeToZonedDateTime(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DateTimeToLocaleString(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DateTimeWithCalendar(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;

    procedure InitializePrototype;
  public
    constructor Create(const AYear, AMonth, ADay, AHour, AMinute, ASecond,
      AMillisecond, AMicrosecond, ANanosecond: Integer;
      const ACalendarId: string = 'iso8601'); overload;

    function ToStringTag: string; override;
    class procedure ExposePrototype(const AConstructor: TGocciaObjectValue);

    property Year: Integer read FYear;
    property Month: Integer read FMonth;
    property Day: Integer read FDay;
    property Hour: Integer read FHour;
    property Minute: Integer read FMinute;
    property Second: Integer read FSecond;
    property Millisecond: Integer read FMillisecond;
    property Microsecond: Integer read FMicrosecond;
    property Nanosecond: Integer read FNanosecond;
    property CalendarId: string read FCalendarId;
  end;

implementation

uses
  SysUtils,

  BigInteger,

  Goccia.Constants.PropertyNames,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.GarbageCollector,
  Goccia.Realm,
  Goccia.Temporal.Calendar,
  Goccia.Temporal.DurationMath,
  Goccia.Temporal.Options,
  Goccia.Temporal.TimeZone,
  Goccia.Temporal.Utils,
  Goccia.Utils,
  Goccia.Values.ErrorHelper,
  Goccia.Values.IntlDateTimeFormat,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.SymbolValue,
  Goccia.Values.TemporalDuration,
  Goccia.Values.TemporalPlainDate,
  Goccia.Values.TemporalPlainMonthDay,
  Goccia.Values.TemporalPlainTime,
  Goccia.Values.TemporalPlainYearMonth,
  Goccia.Values.TemporalZonedDateTime;

var
  GTemporalPlainDateTimeSharedSlot: TGocciaRealmOwnedSlotId;

threadvar
  FPrototypeMembers: TArray<TGocciaMemberDefinition>;

function GetTemporalPlainDateTimeShared: TGocciaSharedPrototype; inline;
begin
  if Assigned(CurrentRealm) then
    Result := TGocciaSharedPrototype(CurrentRealm.GetOwnedSlot(GTemporalPlainDateTimeSharedSlot))
  else
    Result := nil;
end;

function GetTimeZoneDisambiguationOption(const AOptions: TGocciaValue;
  const AMethod: string): TTemporalTimeZoneDisambiguation;
var
  OptionsObj: TGocciaObjectValue;
  Value: string;
begin
  if (AOptions = nil) or (AOptions is TGocciaUndefinedLiteralValue) then
    Exit(ttzdCompatible);
  if not (AOptions is TGocciaObjectValue) then
    ThrowTypeError(AMethod + ' options must be an object', SSuggestTemporalFromArg);

  OptionsObj := TGocciaObjectValue(AOptions);
  Value := GetOptionString(OptionsObj, 'disambiguation', 'compatible');
  if Value = 'compatible' then
    Result := ttzdCompatible
  else if Value = 'earlier' then
    Result := ttzdEarlier
  else if Value = 'later' then
    Result := ttzdLater
  else if Value = 'reject' then
    Result := ttzdReject
  else
    ThrowRangeError(AMethod + ' invalid disambiguation option: ' + Value,
      SSuggestTemporalRoundArg);
end;

function AsPlainDateTime(const AValue: TGocciaValue; const AMethod: string): TGocciaTemporalPlainDateTimeValue;
begin
  if not (AValue is TGocciaTemporalPlainDateTimeValue) then
    ThrowTypeError(AMethod + ' called on non-PlainDateTime', SSuggestTemporalThisType);
  Result := TGocciaTemporalPlainDateTimeValue(AValue);
end;

function ClampTemporalInteger(const AValue, AMin, AMax: Integer): Integer;
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
begin
  AHour := ClampTemporalInteger(AHour, 0, 23);
  AMinute := ClampTemporalInteger(AMinute, 0, 59);
  ASecond := ClampTemporalInteger(ASecond, 0, 59);
  AMillisecond := ClampTemporalInteger(AMillisecond, 0, 999);
  AMicrosecond := ClampTemporalInteger(AMicrosecond, 0, 999);
  ANanosecond := ClampTemporalInteger(ANanosecond, 0, 999);
end;

procedure ValidatePlainDateTimeRange(const AYear, AMonth, ADay, AHour,
  AMinute, ASecond, AMillisecond, AMicrosecond, ANanosecond: Integer;
  const AMethod: string);
begin
  if (AYear < -271821) or (AYear > 275760) or
     ((AYear = -271821) and
      ((AMonth < 4) or ((AMonth = 4) and (ADay < 19)))) or
     ((AYear = 275760) and
      ((AMonth > 9) or ((AMonth = 9) and (ADay > 13)))) or
     ((AYear = -271821) and (AMonth = 4) and (ADay = 19) and
      (AHour = 0) and (AMinute = 0) and (ASecond = 0) and
      (AMillisecond = 0) and (AMicrosecond = 0) and (ANanosecond = 0)) or
     ((AYear = 275760) and (AMonth = 9) and (ADay = 13) and
      ((AHour > 23) or ((AHour = 23) and
       ((AMinute > 59) or ((AMinute = 59) and
        ((ASecond > 59) or ((ASecond = 59) and
         ((AMillisecond > 999) or ((AMillisecond = 999) and
          ((AMicrosecond > 999) or ((AMicrosecond = 999) and
           (ANanosecond > 999)))))))))))) then
    ThrowRangeError(Format(SErrorTemporalInvalidISOStringFor, ['date-time',
      AMethod]), SSuggestTemporalDateRange);
end;

function PlainDateTimeStringWithConstrainedLeapSecond(const AValue: string): string;
var
  SeparatorPos, TimeStart, SecondPos: Integer;
begin
  Result := AValue;
  SeparatorPos := System.Pos('T', Result);
  if SeparatorPos = 0 then
    SeparatorPos := System.Pos('t', Result);
  if SeparatorPos = 0 then
    SeparatorPos := System.Pos(' ', Result);
  if SeparatorPos = 0 then
    Exit;

  TimeStart := SeparatorPos + 1;
  SecondPos := 0;
  if (TimeStart + 7 <= Length(Result)) and
     (Result[TimeStart + 2] = ':') and (Result[TimeStart + 5] = ':') then
    SecondPos := TimeStart + 6
  else if (TimeStart + 5 <= Length(Result)) and
          (Result[TimeStart] in ['0'..'9']) and
          (Result[TimeStart + 1] in ['0'..'9']) and
          (Result[TimeStart + 2] in ['0'..'9']) and
          (Result[TimeStart + 3] in ['0'..'9']) then
    SecondPos := TimeStart + 4;

  if (SecondPos > 0) and (SecondPos + 1 <= Length(Result)) and
     (Copy(Result, SecondPos, 2) = '60') then
    Result := Copy(Result, 1, SecondPos - 1) + '59' +
      Copy(Result, SecondPos + 2, Length(Result) - SecondPos - 1);
end;

procedure ConstrainPlainDateTimeLeapSecond(var ASecond: Integer); inline;
begin
  if ASecond = 60 then
    ASecond := 59;
end;

procedure ValidatePlainDateTimeRoundIncrement(const AIncrement: Integer;
  const ASmallestUnit: TTemporalUnit);
var
  MaxVal: Integer;
begin
  if AIncrement < 1 then
    ThrowRangeError(SErrorRoundingIncrementMin, SSuggestTemporalRoundArg);
  if AIncrement > 1000000000 then
    ThrowRangeError(SErrorRoundingIncrementMax, SSuggestTemporalRoundArg);

  case ASmallestUnit of
    tuDay:         MaxVal := 1;
    tuHour:        MaxVal := 24;
    tuMinute,
    tuSecond:      MaxVal := 60;
    tuMillisecond,
    tuMicrosecond,
    tuNanosecond:  MaxVal := 1000;
  else
    ThrowRangeError(Format(SErrorTemporalInvalidUnitFor,
      ['PlainDateTime.prototype.round', 'smallestUnit']), SSuggestTemporalValidUnits);
  end;

  if (AIncrement > MaxVal) or (MaxVal mod AIncrement <> 0) or
     ((ASmallestUnit <> tuDay) and (AIncrement = MaxVal)) then
    ThrowRangeError(Format(SErrorRoundingIncrementDivisor, [AIncrement, MaxVal]),
      SSuggestTemporalRoundArg);
end;

function TemporalUnitToOptionString(const AUnit: TTemporalUnit): string;
begin
  case AUnit of
    tuYear:        Result := 'year';
    tuMonth:       Result := 'month';
    tuWeek:        Result := 'week';
    tuDay:         Result := 'day';
    tuHour:        Result := 'hour';
    tuMinute:      Result := 'minute';
    tuSecond:      Result := 'second';
    tuMillisecond: Result := 'millisecond';
    tuMicrosecond: Result := 'microsecond';
    tuNanosecond:  Result := 'nanosecond';
    tuAuto:        Result := 'auto';
  else
    Result := '';
  end;
end;

function TemporalRoundingModeToOptionString(const AMode: TTemporalRoundingMode): string;
begin
  case AMode of
    rmCeil:       Result := 'ceil';
    rmFloor:      Result := 'floor';
    rmTrunc:      Result := 'trunc';
    rmHalfCeil:   Result := 'halfCeil';
    rmHalfFloor:  Result := 'halfFloor';
    rmHalfTrunc:  Result := 'halfTrunc';
    rmHalfEven:   Result := 'halfEven';
    rmExpand:     Result := 'expand';
  else
    Result := 'halfExpand';
  end;
end;

function NegateTemporalRoundingMode(const AMode: TTemporalRoundingMode): TTemporalRoundingMode;
begin
  case AMode of
    rmCeil:      Result := rmFloor;
    rmFloor:     Result := rmCeil;
    rmHalfCeil:  Result := rmHalfFloor;
    rmHalfFloor: Result := rmHalfCeil;
  else
    Result := AMode;
  end;
end;

function Float64RepresentableInteger(const AValue: Int64): Int64;
const
  MAX_SAFE_INTEGER = Int64(9007199254740991);
  FIRST_UNSAFE_INTEGER = Int64(9007199254740992);
var
  AbsValue, Threshold, Spacing, Quotient, Remainder, RoundedAbs: Int64;
begin
  if (AValue >= -MAX_SAFE_INTEGER) and (AValue <= MAX_SAFE_INTEGER) then
    Exit(AValue);

  AbsValue := Abs(AValue);
  Threshold := FIRST_UNSAFE_INTEGER;
  Spacing := 2;
  while (Threshold <= High(Int64) div 2) and (AbsValue >= Threshold * 2) do
  begin
    Threshold := Threshold * 2;
    Spacing := Spacing * 2;
  end;

  Quotient := AbsValue div Spacing;
  Remainder := AbsValue mod Spacing;
  if (Remainder * 2 > Spacing) or
     ((Remainder * 2 = Spacing) and Odd(Quotient)) then
    Inc(Quotient);
  RoundedAbs := Quotient * Spacing;

  if AValue < 0 then
    Result := -RoundedAbs
  else
    Result := RoundedAbs;
end;

procedure BalancePlainDateTimeTimeAddition(const AHour, AMinute, ASecond,
  AMillisecond, AMicrosecond, ANanosecond: Integer;
  const ADuration: TGocciaTemporalDurationValue; out ABalanced: TTemporalTimeRecord;
  out AExtraDays: Int64);
var
  TimeNs, DayNs, DaysBig, Remainder, H, Mi, S, Ms, Us, Ns: TBigInteger;
  IgnoredDays: Int64;
begin
  TimeNs := TimeDurationFromComponents(
    TBigInteger.FromInt64(AHour), TBigInteger.FromInt64(AMinute),
    TBigInteger.FromInt64(ASecond), TBigInteger.FromInt64(AMillisecond),
    TBigInteger.FromInt64(AMicrosecond), TBigInteger.FromInt64(ANanosecond))
    .Add(TimeDurationFromComponents(ADuration.HoursBig, ADuration.MinutesBig,
      ADuration.SecondsBig, ADuration.MillisecondsBig,
      ADuration.MicrosecondsBig, ADuration.NanosecondsBig));
  if not IsValidTimeDuration(TimeNs) then
    ThrowRangeError(SErrorDurationTimeOutOfRange, SSuggestTemporalDurationRange);
  DayNs := TBigInteger.FromInt64(NANOSECONDS_PER_DAY);
  if TimeNs.IsNegative then
    DaysBig := TimeNs.AbsValue.Add(DayNs).Subtract(TBigInteger.One)
      .Divide(DayNs).Negate
  else
    DaysBig := TimeNs.Divide(DayNs);
  Remainder := TimeNs.Subtract(DaysBig.Multiply(DayNs));
  AExtraDays := DaysBig.ToInt64;
  BalanceTimeDurationToFields(Remainder, tuHour, H, Mi, S, Ms, Us, Ns,
    IgnoredDays);
  ABalanced.Hour := Integer(H.ToInt64);
  ABalanced.Minute := Integer(Mi.ToInt64);
  ABalanced.Second := Integer(S.ToInt64);
  ABalanced.Millisecond := Integer(Ms.ToInt64);
  ABalanced.Microsecond := Integer(Us.ToInt64);
  ABalanced.Nanosecond := Integer(Ns.ToInt64);
end;

function CalendarFromProperty(const AValue: TGocciaValue; const AMethod: string): string;
var
  CalendarString: string;
begin
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
    ThrowTypeError(AMethod + ' requires a string or Temporal calendar-carrying object',
      SSuggestTemporalFromArg);

  CalendarString := TGocciaStringLiteralValue(AValue).Value;
  Result := ParseTemporalCalendarStringIdentifierStrict(
    PlainDateTimeStringWithConstrainedLeapSecond(CalendarString));
  if Result = '' then
    ThrowRangeError('Unknown calendar: ' + CalendarString, SSuggestTemporalFromArg);
end;

function CalendarInfoForPlainDateTime(const ADateTime: TGocciaTemporalPlainDateTimeValue;
  const AMethod: string): TTemporalCalendarDateInfo;
begin
  if not TryGetCalendarDateInfo(ADateTime.FCalendarId, ADateTime.FYear, ADateTime.FMonth,
    ADateTime.FDay, Result) then
    ThrowRangeError(Format(SErrorTemporalInvalidISOStringFor, ['date-time', AMethod]), SSuggestTemporalDateRange);
end;

function CoercePlainDateTime(const AValue: TGocciaValue; const AMethod: string): TGocciaTemporalPlainDateTimeValue;
var
  DateRec: TTemporalDateRecord;
  TimeRec: TTemporalTimeRecord;
  Obj: TGocciaObjectValue;
  EmptyArgs: TGocciaArgumentsCollection;
  Converted, V, VMonthValue, VMonthCode, VEra, VEraYear: TGocciaValue;
  VYear, VMonth, VDay, VHour, VMinute, VSecond, VMillisecond, VMicrosecond, VNanosecond: Integer;
  CalendarId, MonthCodeStr: string;
  HasYear, HasMonth, HasMonthCode, HasDay, IsLeapMonth: Boolean;
  MonthFieldValue: Integer;

  function GetRequiredField(const AName: string): Integer;
  begin
    V := Obj.GetProperty(AName);
    if (V = nil) or (V is TGocciaUndefinedLiteralValue) then
      ThrowTypeError(AMethod + ' requires ' + AName + ' property', SSuggestTemporalFromArg);
    Result := ToIntegerWithTruncationValue(V);
  end;

  function GetOptionalField(const AName: string; const ADefault: Integer): Integer;
  begin
    V := Obj.GetProperty(AName);
    if (V = nil) or (V is TGocciaUndefinedLiteralValue) then
      Result := ADefault
    else
      Result := ToIntegerWithTruncationValue(V);
  end;

begin
  CalendarId := 'iso8601';
  if AValue is TGocciaTemporalPlainDateTimeValue then
    Result := TGocciaTemporalPlainDateTimeValue(AValue)
  else if AValue is TGocciaTemporalPlainDateValue then
  begin
    Result := TGocciaTemporalPlainDateTimeValue.Create(
      TGocciaTemporalPlainDateValue(AValue).Year,
      TGocciaTemporalPlainDateValue(AValue).Month,
      TGocciaTemporalPlainDateValue(AValue).Day,
      0, 0, 0, 0, 0, 0,
      TGocciaTemporalPlainDateValue(AValue).CalendarId);
  end
  else if AValue is TGocciaTemporalZonedDateTimeValue then
  begin
    EmptyArgs := TGocciaArgumentsCollection.Create([]);
    try
      Converted := TGocciaTemporalZonedDateTimeValue(AValue).ZonedDateTimeToPlainDateTime(EmptyArgs, AValue);
      Result := TGocciaTemporalPlainDateTimeValue(Converted);
    finally
      EmptyArgs.Free;
    end;
  end
  else if AValue is TGocciaStringLiteralValue then
  begin
    if (not TryExtractTemporalCalendarAnnotation(
         TGocciaStringLiteralValue(AValue).Value, CalendarId)) or
       (not CoerceToISODateTime(
         PlainDateTimeStringWithConstrainedLeapSecond(
           TGocciaStringLiteralValue(AValue).Value), DateRec, TimeRec)) then
      ThrowRangeError(Format(SErrorTemporalInvalidISOStringFor, ['date-time', AMethod]), SSuggestTemporalISOFormat);
    ValidatePlainDateTimeRange(DateRec.Year, DateRec.Month, DateRec.Day,
      TimeRec.Hour, TimeRec.Minute, TimeRec.Second, TimeRec.Millisecond,
      TimeRec.Microsecond, TimeRec.Nanosecond, AMethod);
    Result := TGocciaTemporalPlainDateTimeValue.Create(
      DateRec.Year, DateRec.Month, DateRec.Day,
      TimeRec.Hour, TimeRec.Minute, TimeRec.Second,
      TimeRec.Millisecond, TimeRec.Microsecond, TimeRec.Nanosecond, CalendarId);
  end
  else if AValue is TGocciaObjectValue then
  begin
    Obj := TGocciaObjectValue(AValue);
    V := Obj.GetProperty('calendar');
    if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
      CalendarId := CalendarFromProperty(V, AMethod);
    V := Obj.GetProperty('day');
    HasDay := Assigned(V) and not (V is TGocciaUndefinedLiteralValue);
    if HasDay then
      VDay := ToIntegerWithTruncationValue(V);
    VHour := GetOptionalField('hour', 0);
    VMicrosecond := GetOptionalField('microsecond', 0);
    VMillisecond := GetOptionalField('millisecond', 0);
    VMinute := GetOptionalField('minute', 0);
    VMonthValue := Obj.GetProperty(PROP_MONTH);
    HasMonth := Assigned(VMonthValue) and not (VMonthValue is TGocciaUndefinedLiteralValue);
    if HasMonth then
    begin
      MonthFieldValue := ToIntegerWithTruncationValue(VMonthValue);
      VMonth := MonthFieldValue;
    end;
    VMonthCode := Obj.GetProperty(PROP_MONTH_CODE);
    HasMonthCode := Assigned(VMonthCode) and not (VMonthCode is TGocciaUndefinedLiteralValue);
    if HasMonthCode then
    begin
      MonthCodeStr := VMonthCode.ToStringLiteral.Value;
      if not TryParseTemporalMonthCode(MonthCodeStr, VMonth, IsLeapMonth) then
        ThrowTypeError(Format(SErrorInvalidMonthCodeFor, [AMethod]), SSuggestTemporalMonthCode);
      if HasMonth and (MonthFieldValue <> VMonth) then
        ThrowRangeError(Format(SErrorMonthCodeMismatchIn, [AMethod]), SSuggestTemporalMonthCode);
    end
    else if not HasMonth then
      VMonth := 0;
    VNanosecond := GetOptionalField('nanosecond', 0);
    VSecond := GetOptionalField('second', 0);
    V := Obj.GetProperty('year');
    HasYear := Assigned(V) and not (V is TGocciaUndefinedLiteralValue);
    if HasYear then
      VYear := ToIntegerWithTruncationValue(V);

    if not HasDay or (not HasMonth and not HasMonthCode) then
      ThrowTypeError(AMethod + ' requires year, month, day properties',
        SSuggestTemporalFromArg);
    if not HasYear then
    begin
      VEra := Obj.GetProperty('era');
      VEraYear := Obj.GetProperty('eraYear');
      if (VEra = nil) or (VEra is TGocciaUndefinedLiteralValue) or
         (VEraYear = nil) or (VEraYear is TGocciaUndefinedLiteralValue) or
         not TryCalendarYearFromEra(CalendarId, VEra.ToStringLiteral.Value,
           ToIntegerWithTruncationValue(VEraYear), VYear) then
        ThrowTypeError(AMethod + ' requires year property', SSuggestTemporalFromArg);
    end;
    ConstrainPlainDateTimeLeapSecond(VSecond);
    if CalendarId <> 'iso8601' then
    begin
      if HasMonthCode then
      begin
        if not TryCalendarMonthCodeDateToISO(CalendarId, VYear, VMonth, VDay,
          IsLeapMonth, DateRec) then
          ThrowRangeError(Format(SErrorTemporalInvalidISOStringFor, ['date-time', AMethod]), SSuggestTemporalDateRange);
      end
      else if not TryCalendarDateToISO(CalendarId, VYear, VMonth, VDay, DateRec) then
        ThrowRangeError(Format(SErrorTemporalInvalidISOStringFor, ['date-time', AMethod]), SSuggestTemporalDateRange);
      VYear := DateRec.Year;
      VMonth := DateRec.Month;
      VDay := DateRec.Day;
    end;
    ValidatePlainDateTimeRange(VYear, VMonth, VDay, VHour, VMinute, VSecond,
      VMillisecond, VMicrosecond, VNanosecond, AMethod);
    Result := TGocciaTemporalPlainDateTimeValue.Create(
      VYear, VMonth, VDay, VHour, VMinute, VSecond, VMillisecond, VMicrosecond,
      VNanosecond, CalendarId);
  end
  else
  begin
    ThrowTypeError(AMethod + ' requires a PlainDateTime, string, or object', SSuggestTemporalFromArg);
    Result := nil;
  end;
end;

{ TGocciaTemporalPlainDateTimeValue }

constructor TGocciaTemporalPlainDateTimeValue.Create(const AYear, AMonth, ADay, AHour, AMinute, ASecond,
  AMillisecond, AMicrosecond, ANanosecond: Integer; const ACalendarId: string = 'iso8601');
begin
  inherited Create(nil);
  if not IsValidDate(AYear, AMonth, ADay) then
    ThrowRangeError(SErrorInvalidDateInPlainDateTime, SSuggestTemporalDateRange);
  if not IsValidTime(AHour, AMinute, ASecond, AMillisecond, AMicrosecond, ANanosecond) then
    ThrowRangeError(SErrorInvalidTimeInPlainDateTime, SSuggestTemporalDateRange);
  ValidatePlainDateTimeRange(AYear, AMonth, ADay, AHour, AMinute, ASecond,
    AMillisecond, AMicrosecond, ANanosecond, 'PlainDateTime');
  FYear := AYear;
  FMonth := AMonth;
  FDay := ADay;
  FHour := AHour;
  FMinute := AMinute;
  FSecond := ASecond;
  FMillisecond := AMillisecond;
  FMicrosecond := AMicrosecond;
  FNanosecond := ANanosecond;
  FCalendarId := CanonicalizeTemporalCalendarIdentifier(ACalendarId);
  if FCalendarId = '' then
    ThrowRangeError('Unknown calendar: ' + ACalendarId, SSuggestTemporalDateRange);
  InitializePrototype;
  if Assigned(GetTemporalPlainDateTimeShared) then
    FPrototype := GetTemporalPlainDateTimeShared.Prototype;
end;

procedure TGocciaTemporalPlainDateTimeValue.InitializePrototype;
var
  Members: TGocciaMemberCollection;
  Shared: TGocciaSharedPrototype;
begin
  if not Assigned(CurrentRealm) then Exit;
  if Assigned(GetTemporalPlainDateTimeShared) then Exit;
  Shared := TGocciaSharedPrototype.Create(Self);
  CurrentRealm.SetOwnedSlot(GTemporalPlainDateTimeSharedSlot, Shared);
  if Length(FPrototypeMembers) = 0 then
  begin
    Members := TGocciaMemberCollection.Create;
    try
      Members.AddAccessor('calendarId', GetCalendarId, nil, [pfConfigurable]);
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
      Members.AddAccessor('hour', GetHour, nil, [pfConfigurable]);
      Members.AddAccessor('minute', GetMinute, nil, [pfConfigurable]);
      Members.AddAccessor('second', GetSecond, nil, [pfConfigurable]);
      Members.AddAccessor('millisecond', GetMillisecond, nil, [pfConfigurable]);
      Members.AddAccessor('microsecond', GetMicrosecond, nil, [pfConfigurable]);
      Members.AddAccessor('nanosecond', GetNanosecond, nil, [pfConfigurable]);
      Members.AddMethod(DateTimeWith, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(DateTimeWithPlainTime, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(DateTimeAdd, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(DateTimeSubtract, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(DateTimeUntil, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(DateTimeSince, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(DateTimeRound, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(DateTimeEquals, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(DateTimeToString, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(DateTimeToJSON, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(DateTimeValueOf, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(DateTimeToPlainDate, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(DateTimeToPlainTime, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(DateTimeToPlainYearMonth, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(DateTimeToPlainMonthDay, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(DateTimeToZonedDateTime, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(DateTimeToLocaleString, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(DateTimeWithCalendar, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddSymbolDataProperty(
        TGocciaSymbolValue.WellKnownToStringTag,
        TGocciaStringLiteralValue.Create('Temporal.PlainDateTime'),
        [pfConfigurable]);
      FPrototypeMembers := Members.ToDefinitions;
    finally
      Members.Free;
    end;
  end;
  RegisterMemberDefinitions(Shared.Prototype, FPrototypeMembers);
end;

class procedure TGocciaTemporalPlainDateTimeValue.ExposePrototype(const AConstructor: TGocciaObjectValue);
var
  Shared: TGocciaSharedPrototype;
begin
  Shared := GetTemporalPlainDateTimeShared;
  if not Assigned(Shared) then
  begin
    TGocciaTemporalPlainDateTimeValue.Create(1970, 1, 1, 0, 0, 0, 0, 0, 0);
    Shared := GetTemporalPlainDateTimeShared;
  end;
  if Assigned(Shared) then
    ExposeSharedPrototypeOnConstructor(Shared, AConstructor);
end;

function TGocciaTemporalPlainDateTimeValue.ToStringTag: string;
begin
  Result := 'Temporal.PlainDateTime';
end;

{ Getters }

function TGocciaTemporalPlainDateTimeValue.GetCalendarId(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalPlainDateTimeValue;
begin
  D := AsPlainDateTime(AThisValue, 'get PlainDateTime.calendarId');
  Result := TGocciaStringLiteralValue.Create(D.FCalendarId);
end;

function TGocciaTemporalPlainDateTimeValue.GetEra(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalPlainDateTimeValue;
  Info: TTemporalCalendarDateInfo;
begin
  D := AsPlainDateTime(AThisValue, 'get PlainDateTime.era');
  Info := CalendarInfoForPlainDateTime(D, 'get PlainDateTime.era');
  if not Info.HasEra then
    Exit(TGocciaUndefinedLiteralValue.UndefinedValue);
  Result := TGocciaStringLiteralValue.Create(Info.Era);
end;

function TGocciaTemporalPlainDateTimeValue.GetEraYear(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalPlainDateTimeValue;
  Info: TTemporalCalendarDateInfo;
begin
  D := AsPlainDateTime(AThisValue, 'get PlainDateTime.eraYear');
  Info := CalendarInfoForPlainDateTime(D, 'get PlainDateTime.eraYear');
  if not Info.HasEra then
    Exit(TGocciaUndefinedLiteralValue.UndefinedValue);
  Result := TGocciaNumberLiteralValue.Create(Info.EraYear);
end;

function TGocciaTemporalPlainDateTimeValue.GetYear(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalPlainDateTimeValue;
begin
  D := AsPlainDateTime(AThisValue, 'get PlainDateTime.year');
  Result := TGocciaNumberLiteralValue.Create(CalendarInfoForPlainDateTime(D, 'get PlainDateTime.year').Date.Year);
end;

function TGocciaTemporalPlainDateTimeValue.GetMonth(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalPlainDateTimeValue;
begin
  D := AsPlainDateTime(AThisValue, 'get PlainDateTime.month');
  Result := TGocciaNumberLiteralValue.Create(CalendarInfoForPlainDateTime(D, 'get PlainDateTime.month').Date.Month);
end;

function TGocciaTemporalPlainDateTimeValue.GetMonthCode(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalPlainDateTimeValue;
begin
  D := AsPlainDateTime(AThisValue, 'get PlainDateTime.monthCode');
  Result := TGocciaStringLiteralValue.Create(CalendarInfoForPlainDateTime(D, 'get PlainDateTime.monthCode').MonthCode);
end;

function TGocciaTemporalPlainDateTimeValue.GetDay(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalPlainDateTimeValue;
  Day: Integer;
begin
  D := AsPlainDateTime(AThisValue, 'get PlainDateTime.day');
  if not TryGetCalendarDateDay(D.FCalendarId, D.FYear, D.FMonth, D.FDay,
    Day) then
    ThrowRangeError(Format(SErrorTemporalInvalidISOStringFor, ['date-time',
      'get PlainDateTime.day']), SSuggestTemporalDateRange);
  Result := TGocciaNumberLiteralValue.Create(Day);
end;

function TGocciaTemporalPlainDateTimeValue.GetDayOfWeek(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalPlainDateTimeValue;
begin
  D := AsPlainDateTime(AThisValue, 'get PlainDateTime.dayOfWeek');
  Result := TGocciaNumberLiteralValue.Create(CalendarInfoForPlainDateTime(D, 'get PlainDateTime.dayOfWeek').DayOfWeek);
end;

function TGocciaTemporalPlainDateTimeValue.GetDayOfYear(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalPlainDateTimeValue;
begin
  D := AsPlainDateTime(AThisValue, 'get PlainDateTime.dayOfYear');
  Result := TGocciaNumberLiteralValue.Create(CalendarInfoForPlainDateTime(D, 'get PlainDateTime.dayOfYear').DayOfYear);
end;

function TGocciaTemporalPlainDateTimeValue.GetWeekOfYear(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalPlainDateTimeValue;
begin
  D := AsPlainDateTime(AThisValue, 'get PlainDateTime.weekOfYear');
  if D.FCalendarId <> 'iso8601' then
    Exit(TGocciaUndefinedLiteralValue.UndefinedValue);
  Result := TGocciaNumberLiteralValue.Create(Goccia.Temporal.Utils.WeekOfYear(D.FYear, D.FMonth, D.FDay));
end;

function TGocciaTemporalPlainDateTimeValue.GetYearOfWeek(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalPlainDateTimeValue;
begin
  D := AsPlainDateTime(AThisValue, 'get PlainDateTime.yearOfWeek');
  if D.FCalendarId <> 'iso8601' then
    Exit(TGocciaUndefinedLiteralValue.UndefinedValue);
  Result := TGocciaNumberLiteralValue.Create(Goccia.Temporal.Utils.YearOfWeek(D.FYear, D.FMonth, D.FDay));
end;

function TGocciaTemporalPlainDateTimeValue.GetDaysInWeek(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  AsPlainDateTime(AThisValue, 'get PlainDateTime.daysInWeek');
  Result := TGocciaNumberLiteralValue.Create(7);
end;

function TGocciaTemporalPlainDateTimeValue.GetDaysInMonth(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalPlainDateTimeValue;
  DaysInMonth: Integer;
begin
  D := AsPlainDateTime(AThisValue, 'get PlainDateTime.daysInMonth');
  if not TryGetCalendarDateDaysInMonth(D.FCalendarId, D.FYear, D.FMonth,
    D.FDay, DaysInMonth) then
    ThrowRangeError(Format(SErrorTemporalInvalidISOStringFor, ['date-time',
      'get PlainDateTime.daysInMonth']), SSuggestTemporalDateRange);
  Result := TGocciaNumberLiteralValue.Create(DaysInMonth);
end;

function TGocciaTemporalPlainDateTimeValue.GetDaysInYear(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalPlainDateTimeValue;
begin
  D := AsPlainDateTime(AThisValue, 'get PlainDateTime.daysInYear');
  Result := TGocciaNumberLiteralValue.Create(CalendarInfoForPlainDateTime(D, 'get PlainDateTime.daysInYear').DaysInYear);
end;

function TGocciaTemporalPlainDateTimeValue.GetMonthsInYear(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalPlainDateTimeValue;
begin
  D := AsPlainDateTime(AThisValue, 'get PlainDateTime.monthsInYear');
  Result := TGocciaNumberLiteralValue.Create(CalendarInfoForPlainDateTime(D, 'get PlainDateTime.monthsInYear').MonthsInYear);
end;

function TGocciaTemporalPlainDateTimeValue.GetInLeapYear(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalPlainDateTimeValue;
begin
  D := AsPlainDateTime(AThisValue, 'get PlainDateTime.inLeapYear');
  if CalendarInfoForPlainDateTime(D, 'get PlainDateTime.inLeapYear').InLeapYear then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;

function TGocciaTemporalPlainDateTimeValue.GetHour(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaNumberLiteralValue.Create(AsPlainDateTime(AThisValue, 'get PlainDateTime.hour').FHour);
end;

function TGocciaTemporalPlainDateTimeValue.GetMinute(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaNumberLiteralValue.Create(AsPlainDateTime(AThisValue, 'get PlainDateTime.minute').FMinute);
end;

function TGocciaTemporalPlainDateTimeValue.GetSecond(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaNumberLiteralValue.Create(AsPlainDateTime(AThisValue, 'get PlainDateTime.second').FSecond);
end;

function TGocciaTemporalPlainDateTimeValue.GetMillisecond(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaNumberLiteralValue.Create(AsPlainDateTime(AThisValue, 'get PlainDateTime.millisecond').FMillisecond);
end;

function TGocciaTemporalPlainDateTimeValue.GetMicrosecond(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaNumberLiteralValue.Create(AsPlainDateTime(AThisValue, 'get PlainDateTime.microsecond').FMicrosecond);
end;

function TGocciaTemporalPlainDateTimeValue.GetNanosecond(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaNumberLiteralValue.Create(AsPlainDateTime(AThisValue, 'get PlainDateTime.nanosecond').FNanosecond);
end;

{ Methods }

function TGocciaTemporalPlainDateTimeValue.DateTimeWith(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalPlainDateTimeValue;
  Obj: TGocciaObjectValue;
  V, VYear, VMonth, VMonthCode, VDay, VEra, VEraYear, VHour, VMicrosecond,
    VMillisecond, VMinute, VNanosecond, VSecond: TGocciaValue;
  DateRec: TTemporalDateRecord;
  Info: TTemporalCalendarDateInfo;
  NewYear, NewMonth, NewDay, NewHour, NewMinute, NewSecond,
    NewMillisecond, NewMicrosecond, NewNanosecond, MonthFieldValue: Integer;
  MonthCodeStr: string;
  HasYear, HasMonth, HasMonthCode, HasDay, HasEra, HasEraYear,
    IsLeapMonth, UseMonthCode, SawField: Boolean;
  Overflow: TTemporalOverflow;

  function IsPresent(const AValue: TGocciaValue): Boolean;
  begin
    Result := Assigned(AValue) and not (AValue is TGocciaUndefinedLiteralValue);
  end;

begin
  D := AsPlainDateTime(AThisValue, 'PlainDateTime.prototype.with');
  V := AArgs.GetElement(0);
  if not (V is TGocciaObjectValue) then
    ThrowTypeError(Format(SErrorTemporalWithRequiresObject, ['PlainDateTime']), SSuggestTemporalWithObject);
  Obj := TGocciaObjectValue(V);
  if Copy(Obj.ToStringTag, 1, 9) = 'Temporal.' then
    ThrowTypeError(Format(SErrorTemporalWithRequiresObject, ['PlainDateTime']), SSuggestTemporalWithObject);

  V := Obj.GetProperty('calendar');
  if IsPresent(V) then
    ThrowTypeError('PlainDateTime.prototype.with does not accept calendar',
      SSuggestTemporalWithObject);
  V := Obj.GetProperty('timeZone');
  if IsPresent(V) then
    ThrowTypeError('PlainDateTime.prototype.with does not accept timeZone',
      SSuggestTemporalWithObject);

  Info := CalendarInfoForPlainDateTime(D, 'PlainDateTime.prototype.with');
  NewYear := Info.Date.Year;
  NewMonth := Info.Date.Month;
  NewDay := Info.Date.Day;
  NewHour := D.FHour;
  NewMinute := D.FMinute;
  NewSecond := D.FSecond;
  NewMillisecond := D.FMillisecond;
  NewMicrosecond := D.FMicrosecond;
  NewNanosecond := D.FNanosecond;
  IsLeapMonth := False;
  UseMonthCode := False;
  SawField := False;

  VDay := Obj.GetProperty(PROP_DAY);
  HasDay := IsPresent(VDay);
  if HasDay then
  begin
    SawField := True;
    NewDay := ToIntegerWithTruncationValue(VDay);
  end;
  VHour := Obj.GetProperty('hour');
  if IsPresent(VHour) then
  begin
    SawField := True;
    NewHour := ToIntegerWithTruncationValue(VHour);
  end;
  VMicrosecond := Obj.GetProperty('microsecond');
  if IsPresent(VMicrosecond) then
  begin
    SawField := True;
    NewMicrosecond := ToIntegerWithTruncationValue(VMicrosecond);
  end;
  VMillisecond := Obj.GetProperty('millisecond');
  if IsPresent(VMillisecond) then
  begin
    SawField := True;
    NewMillisecond := ToIntegerWithTruncationValue(VMillisecond);
  end;
  VMinute := Obj.GetProperty('minute');
  if IsPresent(VMinute) then
  begin
    SawField := True;
    NewMinute := ToIntegerWithTruncationValue(VMinute);
  end;

  VMonth := Obj.GetProperty(PROP_MONTH);
  HasMonth := IsPresent(VMonth);
  if HasMonth then
  begin
    SawField := True;
    MonthFieldValue := ToIntegerWithTruncationValue(VMonth);
    NewMonth := MonthFieldValue;
  end;
  VMonthCode := Obj.GetProperty(PROP_MONTH_CODE);
  HasMonthCode := IsPresent(VMonthCode);
  if HasMonthCode then
  begin
    SawField := True;
    MonthCodeStr := VMonthCode.ToStringLiteral.Value;
    if not TryParseTemporalMonthCode(MonthCodeStr, NewMonth, IsLeapMonth) then
      ThrowTypeError(Format(SErrorInvalidMonthCodeFor, ['PlainDateTime.prototype.with']),
        SSuggestTemporalMonthCode);
    UseMonthCode := not HasMonth;
  end;
  VNanosecond := Obj.GetProperty('nanosecond');
  if IsPresent(VNanosecond) then
  begin
    SawField := True;
    NewNanosecond := ToIntegerWithTruncationValue(VNanosecond);
  end;
  VSecond := Obj.GetProperty('second');
  if IsPresent(VSecond) then
  begin
    SawField := True;
    NewSecond := ToIntegerWithTruncationValue(VSecond);
  end;
  VYear := Obj.GetProperty(PROP_YEAR);
  HasYear := IsPresent(VYear);
  if HasYear then
  begin
    SawField := True;
    NewYear := ToIntegerWithTruncationValue(VYear);
  end
  else if D.FCalendarId <> 'iso8601' then
  begin
    VEra := Obj.GetProperty('era');
    VEraYear := Obj.GetProperty('eraYear');
    HasEra := IsPresent(VEra);
    HasEraYear := IsPresent(VEraYear);
    if HasEra or HasEraYear then
    begin
      SawField := True;
      if (not HasEra) or (not HasEraYear) or
         not TryCalendarYearFromEra(D.FCalendarId, VEra.ToStringLiteral.Value,
           ToIntegerWithTruncationValue(VEraYear), NewYear) then
        ThrowTypeError('PlainDateTime.prototype.with requires era and eraYear together',
          SSuggestTemporalFromArg);
    end;
  end;

  if not SawField then
    ThrowTypeError(Format(SErrorTemporalWithRequiresObject, ['PlainDateTime']),
      SSuggestTemporalWithObject);

  if (HasMonth and (NewMonth < 1)) or (HasDay and (NewDay < 1)) or
     ((NewYear < -271821) or (NewYear > 275760)) then
    ThrowRangeError(Format(SErrorTemporalInvalidISOStringFor, ['date-time',
      'PlainDateTime.prototype.with']), SSuggestTemporalDateRange);

  if HasMonthCode and HasMonth and (NewMonth <> MonthFieldValue) then
    ThrowRangeError(Format(SErrorMonthCodeMismatchIn, ['PlainDateTime.prototype.with']),
      SSuggestTemporalMonthCode);

  if (not HasMonth) and (not HasMonthCode) and (D.FCalendarId <> 'iso8601') then
  begin
    MonthCodeStr := Info.MonthCode;
    if not TryParseTemporalMonthCode(MonthCodeStr, NewMonth, IsLeapMonth) then
      ThrowRangeError(Format(SErrorTemporalInvalidISOStringFor, ['date-time',
        'PlainDateTime.prototype.with']), SSuggestTemporalDateRange);
    UseMonthCode := True;
  end;

  Overflow := GetOverflowOptionFromValue(AArgs.GetElement(1), 'PlainDateTime.prototype.with');
  if Overflow = toConstrain then
    ConstrainTemporalTimeFields(NewHour, NewMinute, NewSecond, NewMillisecond,
      NewMicrosecond, NewNanosecond);

  if not TryResolveCalendarDateToISO(D.FCalendarId, NewYear, NewMonth, NewDay,
    UseMonthCode, IsLeapMonth, Overflow = toConstrain, DateRec) then
    ThrowRangeError(Format(SErrorTemporalInvalidISOStringFor, ['date-time',
      'PlainDateTime.prototype.with']), SSuggestTemporalDateRange);
  ValidatePlainDateTimeRange(DateRec.Year, DateRec.Month, DateRec.Day,
    NewHour, NewMinute, NewSecond, NewMillisecond, NewMicrosecond,
    NewNanosecond, 'PlainDateTime.prototype.with');

  Result := TGocciaTemporalPlainDateTimeValue.Create(
    DateRec.Year, DateRec.Month, DateRec.Day,
    NewHour, NewMinute, NewSecond, NewMillisecond, NewMicrosecond,
    NewNanosecond, D.FCalendarId);
end;

function TGocciaTemporalPlainDateTimeValue.DateTimeWithPlainTime(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalPlainDateTimeValue;
  Arg: TGocciaValue;
  H, Mi, S, Ms, Us, Ns: Integer;
  T: TGocciaTemporalPlainTimeValue;
begin
  D := AsPlainDateTime(AThisValue, 'PlainDateTime.prototype.withPlainTime');
  Arg := AArgs.GetElement(0);
  H := 0; Mi := 0; S := 0; Ms := 0; Us := 0; Ns := 0;

  if Assigned(Arg) and not (Arg is TGocciaUndefinedLiteralValue) then
  begin
    T := CoercePlainTime(Arg, 'PlainDateTime.prototype.withPlainTime');
    H := T.Hour; Mi := T.Minute; S := T.Second;
    Ms := T.Millisecond; Us := T.Microsecond; Ns := T.Nanosecond;
  end;

  ValidatePlainDateTimeRange(D.FYear, D.FMonth, D.FDay, H, Mi, S, Ms, Us, Ns,
    'PlainDateTime.prototype.withPlainTime');
  Result := TGocciaTemporalPlainDateTimeValue.Create(D.FYear, D.FMonth, D.FDay, H, Mi, S, Ms, Us, Ns,
    D.FCalendarId);
end;

function TGocciaTemporalPlainDateTimeValue.DateTimeAdd(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalPlainDateTimeValue;
  Dur: TGocciaTemporalDurationValue;
  Arg: TGocciaValue;
  Overflow: TTemporalOverflow;
  DateRec: TTemporalDateRecord;
  ExtraDays: Int64;
  Balanced: TTemporalTimeRecord;
begin
  try
  D := AsPlainDateTime(AThisValue, 'PlainDateTime.prototype.add');
  Arg := AArgs.GetElement(0);

  if Arg is TGocciaTemporalDurationValue then
    Dur := TGocciaTemporalDurationValue(Arg)
  else if Arg is TGocciaStringLiteralValue then
    Dur := DurationFromString(TGocciaStringLiteralValue(Arg).Value,
      SErrorInvalidDurationString)
  else if Arg is TGocciaObjectValue then
    Dur := DurationFromObject(TGocciaObjectValue(Arg))
  else
  begin
    ThrowTypeError(Format(SErrorTemporalAddRequiresDuration, ['PlainDateTime']), SSuggestTemporalDurationArg);
    Dur := nil;
  end;

  Overflow := GetOverflowOptionFromValue(AArgs.GetElement(1), 'PlainDateTime.prototype.add');
  BalancePlainDateTimeTimeAddition(D.FHour, D.FMinute, D.FSecond,
    D.FMillisecond, D.FMicrosecond, D.FNanosecond, Dur, Balanced, ExtraDays);
  if not TryAddCalendarDate(D.FCalendarId, D.FYear, D.FMonth, D.FDay,
    Dur.Years, Dur.Months, Dur.Weeks, Dur.Days + ExtraDays,
    Overflow = toConstrain, DateRec) then
    ThrowRangeError(Format(SErrorTemporalInvalidISOStringFor, ['date-time',
      'PlainDateTime.prototype.add']), SSuggestTemporalDateRange);
  ValidatePlainDateTimeRange(DateRec.Year, DateRec.Month, DateRec.Day,
    Balanced.Hour, Balanced.Minute, Balanced.Second, Balanced.Millisecond,
    Balanced.Microsecond, Balanced.Nanosecond, 'PlainDateTime.prototype.add');

  Result := TGocciaTemporalPlainDateTimeValue.Create(
    DateRec.Year, DateRec.Month, DateRec.Day,
    Balanced.Hour, Balanced.Minute, Balanced.Second,
    Balanced.Millisecond, Balanced.Microsecond, Balanced.Nanosecond, D.FCalendarId);
  except
    on E: ETemporalDurationInt64Overflow do
      ThrowRangeError(E.Message, SSuggestTemporalDurationRange);
  end;
end;

function TGocciaTemporalPlainDateTimeValue.DateTimeSubtract(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Dur: TGocciaTemporalDurationValue;
  NegDur: TGocciaTemporalDurationValue;
  Arg: TGocciaValue;
  NewArgs: TGocciaArgumentsCollection;
begin
  try
  Arg := AArgs.GetElement(0);

  if Arg is TGocciaTemporalDurationValue then
    Dur := TGocciaTemporalDurationValue(Arg)
  else if Arg is TGocciaStringLiteralValue then
    Dur := DurationFromString(TGocciaStringLiteralValue(Arg).Value,
      SErrorInvalidDurationString)
  else if Arg is TGocciaObjectValue then
    Dur := DurationFromObject(TGocciaObjectValue(Arg))
  else
  begin
    ThrowTypeError(Format(SErrorTemporalSubtractRequiresDuration, ['PlainDateTime']), SSuggestTemporalDurationArg);
    Dur := nil;
  end;

  NegDur := TGocciaTemporalDurationValue.CreateFromBigIntegers(
    Dur.YearsBig.Negate, Dur.MonthsBig.Negate, Dur.WeeksBig.Negate,
    Dur.DaysBig.Negate, Dur.HoursBig.Negate, Dur.MinutesBig.Negate,
    Dur.SecondsBig.Negate, Dur.MillisecondsBig.Negate,
    Dur.MicrosecondsBig.Negate, Dur.NanosecondsBig.Negate);

  NewArgs := TGocciaArgumentsCollection.Create([NegDur, AArgs.GetElement(1)]);
  try
    Result := DateTimeAdd(NewArgs, AThisValue);
  finally
    NewArgs.Free;
  end;
  except
    on E: ETemporalDurationInt64Overflow do
      ThrowRangeError(E.Message, SSuggestTemporalDurationRange);
  end;
end;

function TGocciaTemporalPlainDateTimeValue.DateTimeUntil(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D, Other: TGocciaTemporalPlainDateTimeValue;
  OptionsObj: TGocciaObjectValue;
  LargestUnit, SmallestUnit: TTemporalUnit;
  RMode: TTemporalRoundingMode;
  RIncrement: Integer;
  T1Ns, T2Ns, TimeDiffNs: Int64;
  DayDelta, Sgn: Int64;
  AdjY2, AdjM2, AdjD2: Integer;
  AdjDate: TTemporalDateRecord;
  Years, Months, Weeks, Days: Int64;
  AbsTimeNs, AbsDays, TotalInUnit, RemNs: Int64;
  RH, RM, RS, RMs, RUs, RNs: Int64;
begin
  D := AsPlainDateTime(AThisValue, 'PlainDateTime.prototype.until');
  Other := CoercePlainDateTime(AArgs.GetElement(0), 'PlainDateTime.prototype.until');

  OptionsObj := GetDiffOptions(AArgs, 1);
  LargestUnit := GetLargestUnit(OptionsObj, tuAuto);
  RIncrement := GetRoundingIncrement(OptionsObj, 1);
  RMode := GetRoundingMode(OptionsObj, rmTrunc);
  SmallestUnit := GetSmallestUnit(OptionsObj, tuNanosecond);
  if LargestUnit = tuAuto then
  begin
    if Ord(SmallestUnit) < Ord(tuDay) then
      LargestUnit := SmallestUnit
    else
      LargestUnit := tuDay;
  end;
  if Ord(LargestUnit) > Ord(SmallestUnit) then
    ThrowRangeError(SErrorDurationRoundLargestSmallerThanSmallest, SSuggestTemporalRoundArg);
  ValidateRoundingIncrement(RIncrement, SmallestUnit, LargestUnit);

  if D.FCalendarId <> Other.FCalendarId then
    ThrowRangeError('Temporal.PlainDateTime calendars must match',
      SSuggestTemporalDateRange);

  T1Ns := Int64(D.FNanosecond) + Int64(D.FMicrosecond) * 1000 + Int64(D.FMillisecond) * 1000000 +
           Int64(D.FSecond) * Int64(1000000000) + Int64(D.FMinute) * Int64(60000000000) +
           Int64(D.FHour) * Int64(3600000000000);
  T2Ns := Int64(Other.FNanosecond) + Int64(Other.FMicrosecond) * 1000 + Int64(Other.FMillisecond) * 1000000 +
           Int64(Other.FSecond) * Int64(1000000000) + Int64(Other.FMinute) * Int64(60000000000) +
           Int64(Other.FHour) * Int64(3600000000000);

  if LargestUnit in [tuYear, tuMonth, tuWeek, tuDay] then
  begin
    // Calendar-aware differencing — keep days and time-ns separate to avoid
    // Int64 overflow when multiplying day deltas by nanoseconds-per-day.
    DayDelta := DateToEpochDays(Other.FYear, Other.FMonth, Other.FDay) -
                DateToEpochDays(D.FYear, D.FMonth, D.FDay);
    TimeDiffNs := T2Ns - T1Ns;

    // Determine overall sign from the two components.
    // |TimeDiffNs| < 86400000000000 (always < 1 day), so DayDelta dominates.
    if DayDelta > 0 then Sgn := 1
    else if DayDelta < 0 then Sgn := -1
    else if TimeDiffNs > 0 then Sgn := 1
    else if TimeDiffNs < 0 then Sgn := -1
    else
    begin
      Result := TGocciaTemporalDurationValue.Create(0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
      Exit;
    end;

    // If time diff has opposite sign to overall diff, borrow/carry a day
    AdjY2 := Other.FYear;
    AdjM2 := Other.FMonth;
    AdjD2 := Other.FDay;
    if (Sgn > 0) and (TimeDiffNs < 0) then
    begin
      AdjDate := AddDaysToDate(AdjY2, AdjM2, AdjD2, -1);
      AdjY2 := AdjDate.Year; AdjM2 := AdjDate.Month; AdjD2 := AdjDate.Day;
      TimeDiffNs := TimeDiffNs + Int64(86400000000000);
    end
    else if (Sgn < 0) and (TimeDiffNs > 0) then
    begin
      AdjDate := AddDaysToDate(AdjY2, AdjM2, AdjD2, 1);
      AdjY2 := AdjDate.Year; AdjM2 := AdjDate.Month; AdjD2 := AdjDate.Day;
      TimeDiffNs := TimeDiffNs - Int64(86400000000000);
    end;

    CalendarDateUntil(D.FCalendarId, D.FYear, D.FMonth, D.FDay,
      AdjY2, AdjM2, AdjD2, LargestUnit,
      Years, Months, Weeks, Days);

    // Decompose time nanoseconds (same sign as overall result)
    AbsTimeNs := Abs(TimeDiffNs);
    RH := Sgn * (AbsTimeNs div Int64(3600000000000));
    RM := Sgn * ((AbsTimeNs div Int64(60000000000)) mod 60);
    RS := Sgn * ((AbsTimeNs div Int64(1000000000)) mod 60);
    RMs := Sgn * ((AbsTimeNs div 1000000) mod 1000);
    RUs := Sgn * ((AbsTimeNs div 1000) mod 1000);
    RNs := Sgn * (AbsTimeNs mod 1000);

    if (SmallestUnit <> tuNanosecond) or (RIncrement <> 1) then
      RoundDiffDuration(Years, Months, Weeks, Days,
        RH, RM, RS, RMs, RUs, RNs,
        D.FYear, D.FMonth, D.FDay,
        LargestUnit, SmallestUnit, RMode, RIncrement);

    Result := TGocciaTemporalDurationValue.Create(Years, Months, Weeks, Days,
      RH, RM, RS, RMs, RUs, RNs);
  end
  else
  begin
    // Sub-day largestUnit: keep days and time-ns separate to avoid overflow.
    DayDelta := DateToEpochDays(Other.FYear, Other.FMonth, Other.FDay) -
                DateToEpochDays(D.FYear, D.FMonth, D.FDay);
    TimeDiffNs := T2Ns - T1Ns;

    // Normalize: borrow/carry so DayDelta and TimeDiffNs share the same sign
    if (DayDelta > 0) and (TimeDiffNs < 0) then
    begin
      Dec(DayDelta);
      TimeDiffNs := TimeDiffNs + Int64(86400000000000);
    end
    else if (DayDelta < 0) and (TimeDiffNs > 0) then
    begin
      Inc(DayDelta);
      TimeDiffNs := TimeDiffNs - Int64(86400000000000);
    end;

    if DayDelta > 0 then Sgn := 1
    else if DayDelta < 0 then Sgn := -1
    else if TimeDiffNs > 0 then Sgn := 1
    else if TimeDiffNs < 0 then Sgn := -1
    else
    begin
      Result := TGocciaTemporalDurationValue.Create(0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
      Exit;
    end;

    AbsDays := Abs(DayDelta);
    AbsTimeNs := Abs(TimeDiffNs);

    // Decompose by converting day delta using safe integer multiplication,
    // then adding the within-day time remainder.
    Years := 0; Months := 0; Weeks := 0; Days := 0;
    case LargestUnit of
      tuHour:
        begin
          TotalInUnit := AbsDays * 24 + AbsTimeNs div Int64(3600000000000);
          RemNs := AbsTimeNs mod Int64(3600000000000);
          RH := Float64RepresentableInteger(Sgn * TotalInUnit);
          RM := Sgn * (RemNs div Int64(60000000000));
          RS := Sgn * ((RemNs div Int64(1000000000)) mod 60);
          RMs := Sgn * ((RemNs div 1000000) mod 1000);
          RUs := Sgn * ((RemNs div 1000) mod 1000);
          RNs := Sgn * (RemNs mod 1000);
        end;
      tuMinute:
        begin
          TotalInUnit := AbsDays * 1440 + AbsTimeNs div Int64(60000000000);
          RemNs := AbsTimeNs mod Int64(60000000000);
          RH := 0;
          RM := Float64RepresentableInteger(Sgn * TotalInUnit);
          RS := Sgn * (RemNs div Int64(1000000000));
          RMs := Sgn * ((RemNs div 1000000) mod 1000);
          RUs := Sgn * ((RemNs div 1000) mod 1000);
          RNs := Sgn * (RemNs mod 1000);
        end;
      tuSecond:
        begin
          TotalInUnit := AbsDays * 86400 + AbsTimeNs div Int64(1000000000);
          RemNs := AbsTimeNs mod Int64(1000000000);
          RH := 0; RM := 0;
          RS := Float64RepresentableInteger(Sgn * TotalInUnit);
          RMs := Sgn * (RemNs div 1000000);
          RUs := Sgn * ((RemNs div 1000) mod 1000);
          RNs := Sgn * (RemNs mod 1000);
        end;
      tuMillisecond:
        begin
          TotalInUnit := AbsDays * 86400000 + AbsTimeNs div 1000000;
          RemNs := AbsTimeNs mod 1000000;
          RH := 0; RM := 0; RS := 0;
          RMs := Float64RepresentableInteger(Sgn * TotalInUnit);
          RUs := Sgn * (RemNs div 1000);
          RNs := Sgn * (RemNs mod 1000);
        end;
      tuMicrosecond:
        begin
          TotalInUnit := AbsDays * Int64(86400000000) + AbsTimeNs div 1000;
          RH := 0; RM := 0; RS := 0; RMs := 0;
          RUs := Float64RepresentableInteger(Sgn * TotalInUnit);
          RNs := Sgn * (AbsTimeNs mod 1000);
        end;
    else // tuNanosecond
      begin
        TotalInUnit := AbsDays * Int64(86400000000000) + AbsTimeNs;
        RH := 0; RM := 0; RS := 0; RMs := 0; RUs := 0;
        RNs := Float64RepresentableInteger(Sgn * TotalInUnit);
      end;
    end;

    if (SmallestUnit <> tuNanosecond) or (RIncrement <> 1) then
      RoundDiffDuration(Years, Months, Weeks, Days,
        RH, RM, RS, RMs, RUs, RNs,
        D.FYear, D.FMonth, D.FDay,
        LargestUnit, SmallestUnit, RMode, RIncrement);

    Result := TGocciaTemporalDurationValue.Create(Years, Months, Weeks, Days,
      RH, RM, RS, RMs, RUs, RNs);
  end;
end;

function TGocciaTemporalPlainDateTimeValue.DateTimeSince(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D, Other: TGocciaTemporalPlainDateTimeValue;
  NewArgs: TGocciaArgumentsCollection;
  OptionsArg: TGocciaValue;
  OptionsObj, InternalOptions: TGocciaObjectValue;
  LargestUnit, SmallestUnit: TTemporalUnit;
  RMode: TTemporalRoundingMode;
  RIncrement: Integer;
  UntilDuration: TGocciaTemporalDurationValue;
begin
  D := AsPlainDateTime(AThisValue, 'PlainDateTime.prototype.since');
  Other := CoercePlainDateTime(AArgs.GetElement(0), 'PlainDateTime.prototype.since');

  OptionsArg := AArgs.GetElement(1);
  OptionsObj := GetDiffOptions(AArgs, 1);
  InternalOptions := nil;
  if OptionsObj <> nil then
  begin
    LargestUnit := GetLargestUnit(OptionsObj, tuAuto);
    RIncrement := GetRoundingIncrement(OptionsObj, 1);
    RMode := GetRoundingMode(OptionsObj, rmTrunc);
    SmallestUnit := GetSmallestUnit(OptionsObj, tuNanosecond);

    InternalOptions := TGocciaObjectValue.Create(nil, 4);
    InternalOptions.CreateDataPropertyOrThrow('largestUnit',
      TGocciaStringLiteralValue.Create(TemporalUnitToOptionString(LargestUnit)));
    InternalOptions.CreateDataPropertyOrThrow('roundingIncrement',
      TGocciaNumberLiteralValue.Create(RIncrement));
    InternalOptions.CreateDataPropertyOrThrow('roundingMode',
      TGocciaStringLiteralValue.Create(
        TemporalRoundingModeToOptionString(NegateTemporalRoundingMode(RMode))));
    InternalOptions.CreateDataPropertyOrThrow('smallestUnit',
      TGocciaStringLiteralValue.Create(TemporalUnitToOptionString(SmallestUnit)));
    TGarbageCollector.Instance.AddTempRoot(InternalOptions);
  end;
  try
    if InternalOptions <> nil then
      NewArgs := TGocciaArgumentsCollection.Create([Other, InternalOptions])
    else if (OptionsArg <> nil) and not (OptionsArg is TGocciaUndefinedLiteralValue) then
      NewArgs := TGocciaArgumentsCollection.Create([Other, OptionsArg])
    else
      NewArgs := TGocciaArgumentsCollection.Create([Other]);
    try
    UntilDuration := TGocciaTemporalDurationValue(DateTimeUntil(NewArgs, D));
    Result := TGocciaTemporalDurationValue.Create(
      -UntilDuration.Years, -UntilDuration.Months, -UntilDuration.Weeks,
      -UntilDuration.Days, -UntilDuration.Hours, -UntilDuration.Minutes,
      -UntilDuration.Seconds, -UntilDuration.Milliseconds,
      -UntilDuration.Microseconds, -UntilDuration.Nanoseconds);
    finally
      NewArgs.Free;
    end;
  finally
    if InternalOptions <> nil then
      TGarbageCollector.Instance.RemoveTempRoot(InternalOptions);
  end;
end;

function TGocciaTemporalPlainDateTimeValue.DateTimeRound(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalPlainDateTimeValue;
  UnitStr: string;
  Arg: TGocciaValue;
  OptionsObj: TGocciaObjectValue;
  TotalNs, Divisor, Rounded, ExtraDays: Int64;
  Balanced: TTemporalTimeRecord;
  DateRec: TTemporalDateRecord;
  SmallestUnit: TTemporalUnit;
  Mode: TTemporalRoundingMode;
  Increment: Integer;
begin
  D := AsPlainDateTime(AThisValue, 'PlainDateTime.prototype.round');
  Arg := AArgs.GetElement(0);

  Mode := rmHalfExpand;
  Increment := 1;

  if Arg is TGocciaStringLiteralValue then
  begin
    UnitStr := TGocciaStringLiteralValue(Arg).Value;
    if not GetTemporalUnitFromString(UnitStr, SmallestUnit) then
      ThrowRangeError(Format(SErrorTemporalInvalidUnitFor, ['PlainDateTime.prototype.round', UnitStr]), SSuggestTemporalValidUnits);
  end
  else if Arg is TGocciaObjectValue then
  begin
    OptionsObj := TGocciaObjectValue(Arg);
    Increment := GetRoundingIncrement(OptionsObj, 1);
    Mode := GetRoundingMode(OptionsObj, rmHalfExpand);
    SmallestUnit := GetSmallestUnit(OptionsObj, tuNone);
    if SmallestUnit = tuNone then
      ThrowRangeError(SErrorTemporalRoundRequiresSmallestUnit, SSuggestTemporalRoundArg);
  end
  else
  begin
    ThrowTypeError(Format(SErrorTemporalRoundRequiresStringOrOptions, ['PlainDateTime']), SSuggestTemporalRoundArg);
    SmallestUnit := tuNanosecond;
  end;
  ValidatePlainDateTimeRoundIncrement(Increment, SmallestUnit);

  TotalNs := Int64(D.FNanosecond) + Int64(D.FMicrosecond) * 1000 + Int64(D.FMillisecond) * 1000000 +
             Int64(D.FSecond) * Int64(1000000000) + Int64(D.FMinute) * Int64(60000000000) +
             Int64(D.FHour) * Int64(3600000000000);

  if SmallestUnit = tuDay then
  begin
    // Round to nearest day using the specified rounding mode and increment
    Divisor := NANOSECONDS_PER_DAY * Increment;
    Rounded := RoundWithMode(TotalNs, Divisor, Mode);
    ExtraDays := Rounded div NANOSECONDS_PER_DAY;
    DateRec := AddDaysToDate(D.FYear, D.FMonth, D.FDay, ExtraDays);
    ValidatePlainDateTimeRange(DateRec.Year, DateRec.Month, DateRec.Day,
      0, 0, 0, 0, 0, 0, 'PlainDateTime.prototype.round');
    Result := TGocciaTemporalPlainDateTimeValue.Create(DateRec.Year, DateRec.Month, DateRec.Day, 0, 0, 0, 0, 0, 0,
      D.FCalendarId);
    Exit;
  end;

  Divisor := UnitToNanoseconds(SmallestUnit) * Increment;
  Rounded := RoundWithMode(TotalNs, Divisor, Mode);
  Balanced := BalanceTime(0, 0, 0, 0, 0, Rounded, ExtraDays);
  DateRec := AddDaysToDate(D.FYear, D.FMonth, D.FDay, ExtraDays);
  ValidatePlainDateTimeRange(DateRec.Year, DateRec.Month, DateRec.Day,
    Balanced.Hour, Balanced.Minute, Balanced.Second, Balanced.Millisecond,
    Balanced.Microsecond, Balanced.Nanosecond, 'PlainDateTime.prototype.round');

  Result := TGocciaTemporalPlainDateTimeValue.Create(
    DateRec.Year, DateRec.Month, DateRec.Day,
    Balanced.Hour, Balanced.Minute, Balanced.Second,
    Balanced.Millisecond, Balanced.Microsecond, Balanced.Nanosecond, D.FCalendarId);
end;

function TGocciaTemporalPlainDateTimeValue.DateTimeEquals(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D, Other: TGocciaTemporalPlainDateTimeValue;
begin
  D := AsPlainDateTime(AThisValue, 'PlainDateTime.prototype.equals');
  Other := CoercePlainDateTime(AArgs.GetElement(0), 'PlainDateTime.prototype.equals');

  if (CompareDates(D.FYear, D.FMonth, D.FDay, Other.FYear, Other.FMonth, Other.FDay) = 0) and
     (CompareTimes(D.FHour, D.FMinute, D.FSecond, D.FMillisecond, D.FMicrosecond, D.FNanosecond,
                   Other.FHour, Other.FMinute, Other.FSecond, Other.FMillisecond, Other.FMicrosecond, Other.FNanosecond) = 0) and
     (D.FCalendarId = Other.FCalendarId) then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;

function TGocciaTemporalPlainDateTimeValue.DateTimeToString(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalPlainDateTimeValue;
  Arg: TGocciaValue;
  OptionsObj: TGocciaObjectValue;
  FracDigits, ExtraDays: Integer;
  Mode: TTemporalRoundingMode;
  CalDisp: TTemporalCalendarDisplay;
  H, Mi, S, Ms, Us, Ns: Integer;
  DateRec: TTemporalDateRecord;
  TimeStr: string;
begin
  D := AsPlainDateTime(AThisValue, 'PlainDateTime.prototype.toString');
  OptionsObj := nil;
  Arg := AArgs.GetElement(0);
  if Assigned(Arg) and not (Arg is TGocciaUndefinedLiteralValue) then
  begin
    if not (Arg is TGocciaObjectValue) then
      ThrowTypeError('options must be an object or undefined', SSuggestTemporalFromArg);
    OptionsObj := TGocciaObjectValue(Arg);
  end;
  CalDisp := GetCalendarDisplay(OptionsObj);
  ResolveTemporalToStringOptions(OptionsObj, FracDigits, Mode);
  H := D.FHour; Mi := D.FMinute; S := D.FSecond;
  Ms := D.FMillisecond; Us := D.FMicrosecond; Ns := D.FNanosecond;
  ExtraDays := 0;
  RoundTimeForToString(H, Mi, S, Ms, Us, Ns, ExtraDays, FracDigits, Mode);
  if ExtraDays <> 0 then
    DateRec := AddDaysToDate(D.FYear, D.FMonth, D.FDay, ExtraDays)
  else
  begin
    DateRec.Year := D.FYear;
    DateRec.Month := D.FMonth;
    DateRec.Day := D.FDay;
  end;
  ValidatePlainDateTimeRange(DateRec.Year, DateRec.Month, DateRec.Day,
    H, Mi, S, Ms, Us, Ns, 'PlainDateTime.prototype.toString');
  if FracDigits = -2 then // smallestUnit: minute
    TimeStr := PadTwo(H) + ':' + PadTwo(Mi)
  else
    TimeStr := FormatTimeWithPrecision(H, Mi, S, Ms, Us, Ns, FracDigits);
  Result := TGocciaStringLiteralValue.Create(
    FormatDateString(DateRec.Year, DateRec.Month, DateRec.Day) + 'T' +
    TimeStr + FormatCalendarAnnotation(CalDisp, D.FCalendarId));
end;

function TGocciaTemporalPlainDateTimeValue.DateTimeToJSON(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalPlainDateTimeValue;
begin
  D := AsPlainDateTime(AThisValue, 'PlainDateTime.prototype.toJSON');
  Result := TGocciaStringLiteralValue.Create(
    FormatDateString(D.FYear, D.FMonth, D.FDay) + 'T' +
    FormatTimeString(D.FHour, D.FMinute, D.FSecond, D.FMillisecond, D.FMicrosecond, D.FNanosecond) +
    FormatCalendarAnnotation(tcdAuto, D.FCalendarId));
end;

function TGocciaTemporalPlainDateTimeValue.DateTimeValueOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  ThrowTypeError(Format(SErrorTemporalValueOf, ['PlainDateTime', 'toString or compare']), SSuggestTemporalNoValueOf);
  Result := nil;
end;

function TGocciaTemporalPlainDateTimeValue.DateTimeToPlainDate(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalPlainDateTimeValue;
begin
  D := AsPlainDateTime(AThisValue, 'PlainDateTime.prototype.toPlainDate');
  Result := TGocciaTemporalPlainDateValue.Create(D.FYear, D.FMonth, D.FDay, D.FCalendarId);
end;

function TGocciaTemporalPlainDateTimeValue.DateTimeToPlainTime(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalPlainDateTimeValue;
begin
  D := AsPlainDateTime(AThisValue, 'PlainDateTime.prototype.toPlainTime');
  Result := TGocciaTemporalPlainTimeValue.Create(D.FHour, D.FMinute, D.FSecond,
    D.FMillisecond, D.FMicrosecond, D.FNanosecond);
end;

function TGocciaTemporalPlainDateTimeValue.DateTimeToPlainYearMonth(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalPlainDateTimeValue;
begin
  D := AsPlainDateTime(AThisValue, 'PlainDateTime.prototype.toPlainYearMonth');
  if D.FCalendarId = 'iso8601' then
    Result := TGocciaTemporalPlainYearMonthValue.Create(D.FYear, D.FMonth, 1,
      D.FCalendarId)
  else
    Result := TGocciaTemporalPlainYearMonthValue.Create(D.FYear, D.FMonth,
      D.FDay, D.FCalendarId);
end;

function TGocciaTemporalPlainDateTimeValue.DateTimeToPlainMonthDay(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalPlainDateTimeValue;
  Info: TTemporalCalendarDateInfo;
  DateRec: TTemporalDateRecord;
  MonthPart: Integer;
  IsLeapMonth: Boolean;
begin
  D := AsPlainDateTime(AThisValue, 'PlainDateTime.prototype.toPlainMonthDay');
  if D.FCalendarId = 'iso8601' then
    Result := TGocciaTemporalPlainMonthDayValue.Create(D.FMonth, D.FDay,
      1972, D.FCalendarId)
  else
  begin
    Info := CalendarInfoForPlainDateTime(D, 'PlainDateTime.prototype.toPlainMonthDay');
    if not TryParseTemporalMonthCode(Info.MonthCode, MonthPart, IsLeapMonth) then
      ThrowRangeError(Format(SErrorInvalidMonthDayStringFor,
        ['PlainDateTime.prototype.toPlainMonthDay']), SSuggestTemporalDateRange);
    if not TryResolvePlainMonthDayReferenceDate(D.FCalendarId, MonthPart,
      Info.Date.Day, True, IsLeapMonth, DateRec) then
      ThrowRangeError(Format(SErrorInvalidMonthDayStringFor,
        ['PlainDateTime.prototype.toPlainMonthDay']), SSuggestTemporalDateRange);
    Result := TGocciaTemporalPlainMonthDayValue.Create(DateRec.Month,
      DateRec.Day, DateRec.Year, D.FCalendarId);
  end;
end;

function TGocciaTemporalPlainDateTimeValue.DateTimeToZonedDateTime(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalPlainDateTimeValue;
  Arg: TGocciaValue;
  TimeZoneStr: string;
  EpochMs: Int64;
  Disambiguation: TTemporalTimeZoneDisambiguation;
begin
  D := AsPlainDateTime(AThisValue, 'PlainDateTime.prototype.toZonedDateTime');
  Arg := AArgs.GetElement(0);

  if Arg is TGocciaStringLiteralValue then
    TimeZoneStr := TGocciaStringLiteralValue(Arg).Value
  else if Arg is TGocciaObjectValue then
  begin
    Arg := TGocciaObjectValue(Arg).GetProperty('timeZone');
    if (Arg = nil) or (Arg is TGocciaUndefinedLiteralValue) then
      ThrowTypeError(SErrorPlainDateTimeToZonedRequiresTZ, SSuggestTemporalTimezone);
    TimeZoneStr := Arg.ToStringLiteral.Value;
  end
  else
  begin
    ThrowTypeError(SErrorPlainDateTimeToZonedRequiresStringOrOptions, SSuggestTemporalTimezone);
    TimeZoneStr := '';
  end;

  TimeZoneStr := CanonicalizeTemporalTimeZoneIdentifier(TimeZoneStr);
  Disambiguation := GetTimeZoneDisambiguationOption(AArgs.GetElement(1),
    'PlainDateTime.prototype.toZonedDateTime');

  EpochMs := LocalDateTimeToEpochMillisecondsWithDisambiguation(D.FYear,
    D.FMonth, D.FDay, D.FHour, D.FMinute, D.FSecond, D.FMillisecond,
    TimeZoneStr, Disambiguation);

  Result := TGocciaTemporalZonedDateTimeValue.Create(EpochMs,
    D.FMicrosecond * 1000 + D.FNanosecond, TimeZoneStr, D.FCalendarId);
end;

function TGocciaTemporalPlainDateTimeValue.DateTimeToLocaleString(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  AsPlainDateTime(AThisValue, 'PlainDateTime.prototype.toLocaleString');
  Result := FormatTemporalValueToLocaleString(AThisValue, AArgs.GetElement(0),
    AArgs.GetElement(1));
end;

function TGocciaTemporalPlainDateTimeValue.DateTimeWithCalendar(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalPlainDateTimeValue;
  Arg: TGocciaValue;
  CalId: string;
begin
  D := AsPlainDateTime(AThisValue, 'PlainDateTime.prototype.withCalendar');
  Arg := AArgs.GetElement(0);
  if (Arg = nil) or (Arg is TGocciaUndefinedLiteralValue) then
    ThrowTypeError('PlainDateTime.prototype.withCalendar requires a calendar argument', SSuggestTemporalFromArg);
  CalId := CalendarFromProperty(Arg, 'PlainDateTime.prototype.withCalendar');
  Result := TGocciaTemporalPlainDateTimeValue.Create(D.FYear, D.FMonth, D.FDay,
    D.FHour, D.FMinute, D.FSecond, D.FMillisecond, D.FMicrosecond, D.FNanosecond, CalId);
end;

initialization
  GTemporalPlainDateTimeSharedSlot := RegisterRealmOwnedSlot('Temporal.PlainDateTime.shared');

end.
