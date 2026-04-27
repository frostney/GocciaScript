unit Goccia.Values.TemporalZonedDateTime;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.ObjectModel,
  Goccia.SharedPrototype,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaTemporalZonedDateTimeValue = class(TGocciaObjectValue)
  private
    FEpochMilliseconds: Int64;
    FSubMillisecondNanoseconds: Integer;
    FTimeZone: string;

    procedure InitializePrototype;
  published
    function GetCalendarId(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetTimeZoneId(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
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
    function GetHoursInDay(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetHour(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetMinute(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetSecond(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetMillisecond(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetMicrosecond(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetNanosecond(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetOffset(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetOffsetNanoseconds(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetEpochMilliseconds(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetEpochNanoseconds(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;

    function ZonedDateTimeWith(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ZonedDateTimeWithPlainDate(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ZonedDateTimeWithPlainTime(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ZonedDateTimeWithTimeZone(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ZonedDateTimeAdd(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ZonedDateTimeSubtract(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ZonedDateTimeUntil(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ZonedDateTimeSince(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ZonedDateTimeRound(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ZonedDateTimeEquals(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ZonedDateTimeStartOfDay(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ZonedDateTimeToInstant(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ZonedDateTimeToPlainDate(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ZonedDateTimeToPlainTime(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ZonedDateTimeToPlainDateTime(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ZonedDateTimeToString(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ZonedDateTimeToJSON(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ZonedDateTimeValueOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ZonedDateTimeToLocaleString(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  public
    constructor Create(const AEpochMilliseconds: Int64; const ASubMillisecondNanoseconds: Integer;
      const ATimeZone: string); overload;

    function ToStringTag: string; override;
    class procedure ExposePrototype(const AConstructor: TGocciaObjectValue);

    property EpochMilliseconds: Int64 read FEpochMilliseconds;
    property SubMillisecondNanoseconds: Integer read FSubMillisecondNanoseconds;
    property TimeZone: string read FTimeZone;
  end;

implementation

uses
  SysUtils,

  BigInteger,

  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.Realm,
  Goccia.Temporal.DurationMath,
  Goccia.Temporal.Options,
  Goccia.Temporal.TimeZone,
  Goccia.Temporal.Utils,
  Goccia.Values.BigIntValue,
  Goccia.Values.ErrorHelper,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.SymbolValue,
  Goccia.Values.TemporalDuration,
  Goccia.Values.TemporalInstant,
  Goccia.Values.TemporalPlainDate,
  Goccia.Values.TemporalPlainDateTime,
  Goccia.Values.TemporalPlainTime;

var
  GTemporalZonedDateTimeSharedSlot: TGocciaRealmOwnedSlotId;

threadvar
  FPrototypeMembers: TArray<TGocciaMemberDefinition>;

function GetTemporalZonedDateTimeShared: TGocciaSharedPrototype; inline;
begin
  if Assigned(CurrentRealm) then
    Result := TGocciaSharedPrototype(CurrentRealm.GetOwnedSlot(GTemporalZonedDateTimeSharedSlot))
  else
    Result := nil;
end;

const
  MILLISECONDS_PER_SECOND = 1000;
  MILLISECONDS_PER_MINUTE = 60000;
  MILLISECONDS_PER_HOUR = 3600000;
  MILLISECONDS_PER_DAY = Int64(86400000);
  SUB_MS_NANOSECOND_LIMIT = 1000000;
  HOURS_PER_DAY = 24;
  MONTHS_PER_YEAR = 12;
  DAYS_PER_WEEK = 7;

function AsZonedDateTime(const AValue: TGocciaValue; const AMethod: string): TGocciaTemporalZonedDateTimeValue;
begin
  if not (AValue is TGocciaTemporalZonedDateTimeValue) then
    ThrowTypeError(AMethod + ' called on non-ZonedDateTime', SSuggestTemporalThisType);
  Result := TGocciaTemporalZonedDateTimeValue(AValue);
end;

function CoerceZonedDateTime(const AValue: TGocciaValue; const AMethod: string): TGocciaTemporalZonedDateTimeValue;
var
  DateRec: TTemporalDateRecord;
  TimeRec: TTemporalTimeRecord;
  OffsetSeconds: Integer;
  TimeZoneStr: string;
  EpochMs: Int64;
  SubMs: Integer;
begin
  if AValue is TGocciaTemporalZonedDateTimeValue then
    Result := TGocciaTemporalZonedDateTimeValue(AValue)
  else if AValue is TGocciaStringLiteralValue then
  begin
    // Parse ISO with timezone annotation: 2024-03-15T13:45:30+05:30[Asia/Kolkata]
    if not TryParseISODateTimeWithOffset(TGocciaStringLiteralValue(AValue).Value,
      DateRec, TimeRec, OffsetSeconds, TimeZoneStr) then
      ThrowRangeError(Format(SErrorTemporalInvalidISOStringFor, ['ZonedDateTime', AMethod]), SSuggestTemporalISOFormat);
    if TimeZoneStr = '' then
      ThrowRangeError(SErrorZonedDateTimeRequiresTZAnnotation, SSuggestTemporalTimezone);

    // Convert parsed local time to epoch ms using the parsed offset
    EpochMs := DateToEpochDays(DateRec.Year, DateRec.Month, DateRec.Day) * MILLISECONDS_PER_DAY +
               Int64(TimeRec.Hour) * MILLISECONDS_PER_HOUR +
               Int64(TimeRec.Minute) * MILLISECONDS_PER_MINUTE +
               Int64(TimeRec.Second) * MILLISECONDS_PER_SECOND +
               TimeRec.Millisecond;
    // Subtract offset to get UTC epoch ms
    EpochMs := EpochMs - Int64(OffsetSeconds) * MILLISECONDS_PER_SECOND;
    SubMs := TimeRec.Microsecond * 1000 + TimeRec.Nanosecond;
    Result := TGocciaTemporalZonedDateTimeValue.Create(EpochMs, SubMs, TimeZoneStr);
  end
  else
  begin
    ThrowTypeError(AMethod + ' requires a ZonedDateTime or string', SSuggestTemporalFromArg);
    Result := nil;
  end;
end;

procedure ComputeLocalComponents(const AZdt: TGocciaTemporalZonedDateTimeValue;
  out AYear, AMonth, ADay, AHour, AMinute, ASecond, AMs, AUs, ANs: Integer);
var
  OffsetSeconds: Integer;
  LocalEpochMs, EpochDays, RemainingMs: Int64;
  DateRec: TTemporalDateRecord;
begin
  OffsetSeconds := GetUtcOffsetSeconds(AZdt.FTimeZone, AZdt.FEpochMilliseconds div MILLISECONDS_PER_SECOND);
  LocalEpochMs := AZdt.FEpochMilliseconds + Int64(OffsetSeconds) * MILLISECONDS_PER_SECOND;

  EpochDays := LocalEpochMs div MILLISECONDS_PER_DAY;
  RemainingMs := LocalEpochMs mod MILLISECONDS_PER_DAY;
  if RemainingMs < 0 then
  begin
    Dec(EpochDays);
    Inc(RemainingMs, MILLISECONDS_PER_DAY);
  end;

  DateRec := EpochDaysToDate(EpochDays);
  AYear := DateRec.Year;
  AMonth := DateRec.Month;
  ADay := DateRec.Day;

  AHour := Integer(RemainingMs div MILLISECONDS_PER_HOUR);
  RemainingMs := RemainingMs mod MILLISECONDS_PER_HOUR;
  AMinute := Integer(RemainingMs div MILLISECONDS_PER_MINUTE);
  RemainingMs := RemainingMs mod MILLISECONDS_PER_MINUTE;
  ASecond := Integer(RemainingMs div MILLISECONDS_PER_SECOND);
  AMs := Integer(RemainingMs mod MILLISECONDS_PER_SECOND);

  AUs := AZdt.FSubMillisecondNanoseconds div 1000;
  ANs := AZdt.FSubMillisecondNanoseconds mod 1000;
end;

function LocalToEpochMs(const AYear, AMonth, ADay, AHour, AMinute, ASecond, AMs: Integer;
  const ATimeZone: string): Int64;
var
  UtcGuess: Int64;
  OffsetSeconds: Integer;
begin
  // Compute epoch ms as if the local time were UTC
  UtcGuess := DateToEpochDays(AYear, AMonth, ADay) * MILLISECONDS_PER_DAY +
              Int64(AHour) * MILLISECONDS_PER_HOUR +
              Int64(AMinute) * MILLISECONDS_PER_MINUTE +
              Int64(ASecond) * MILLISECONDS_PER_SECOND +
              AMs;

  // Get offset at that estimated UTC epoch
  OffsetSeconds := GetUtcOffsetSeconds(ATimeZone, UtcGuess div MILLISECONDS_PER_SECOND);

  // Subtract offset to get actual UTC epoch ms
  // (compatible disambiguation: use the first guess)
  Result := UtcGuess - Int64(OffsetSeconds) * MILLISECONDS_PER_SECOND;
end;

function FormatOffsetString(const AOffsetSeconds: Integer): string;
var
  AbsOffset, OffsetH, OffsetM: Integer;
  Sign: Char;
begin
  if AOffsetSeconds >= 0 then
    Sign := '+'
  else
    Sign := '-';
  AbsOffset := Abs(AOffsetSeconds);
  OffsetH := AbsOffset div 3600;
  OffsetM := (AbsOffset mod 3600) div 60;
  Result := Sign + Format('%.2d:%.2d', [OffsetH, OffsetM]);
end;

function CoerceDuration(const AArg: TGocciaValue; const AMethod: string): TGocciaTemporalDurationValue;
var
  DurRec: TTemporalDurationRecord;
  Obj: TGocciaObjectValue;
  VF: TGocciaValue;
  Y, Mo, W, Da, H, Mi, S, Ms, Us, Ns: Int64;
begin
  if AArg is TGocciaTemporalDurationValue then
    Result := TGocciaTemporalDurationValue(AArg)
  else if AArg is TGocciaStringLiteralValue then
  begin
    if not TryParseISODuration(TGocciaStringLiteralValue(AArg).Value, DurRec) then
      ThrowRangeError(SErrorInvalidDurationString, SSuggestTemporalDurationArg);
    Result := TGocciaTemporalDurationValue.Create(
      DurRec.Years, DurRec.Months, DurRec.Weeks, DurRec.Days,
      DurRec.Hours, DurRec.Minutes, DurRec.Seconds,
      DurRec.Milliseconds, DurRec.Microseconds, DurRec.Nanoseconds);
  end
  else if AArg is TGocciaObjectValue then
  begin
    Obj := TGocciaObjectValue(AArg);
    Y := 0; Mo := 0; W := 0; Da := 0; H := 0; Mi := 0; S := 0; Ms := 0; Us := 0; Ns := 0;
    VF := Obj.GetProperty('years'); if Assigned(VF) and not (VF is TGocciaUndefinedLiteralValue) then Y := Trunc(VF.ToNumberLiteral.Value);
    VF := Obj.GetProperty('months'); if Assigned(VF) and not (VF is TGocciaUndefinedLiteralValue) then Mo := Trunc(VF.ToNumberLiteral.Value);
    VF := Obj.GetProperty('weeks'); if Assigned(VF) and not (VF is TGocciaUndefinedLiteralValue) then W := Trunc(VF.ToNumberLiteral.Value);
    VF := Obj.GetProperty('days'); if Assigned(VF) and not (VF is TGocciaUndefinedLiteralValue) then Da := Trunc(VF.ToNumberLiteral.Value);
    VF := Obj.GetProperty('hours'); if Assigned(VF) and not (VF is TGocciaUndefinedLiteralValue) then H := Trunc(VF.ToNumberLiteral.Value);
    VF := Obj.GetProperty('minutes'); if Assigned(VF) and not (VF is TGocciaUndefinedLiteralValue) then Mi := Trunc(VF.ToNumberLiteral.Value);
    VF := Obj.GetProperty('seconds'); if Assigned(VF) and not (VF is TGocciaUndefinedLiteralValue) then S := Trunc(VF.ToNumberLiteral.Value);
    VF := Obj.GetProperty('milliseconds'); if Assigned(VF) and not (VF is TGocciaUndefinedLiteralValue) then Ms := Trunc(VF.ToNumberLiteral.Value);
    VF := Obj.GetProperty('microseconds'); if Assigned(VF) and not (VF is TGocciaUndefinedLiteralValue) then Us := Trunc(VF.ToNumberLiteral.Value);
    VF := Obj.GetProperty('nanoseconds'); if Assigned(VF) and not (VF is TGocciaUndefinedLiteralValue) then Ns := Trunc(VF.ToNumberLiteral.Value);
    Result := TGocciaTemporalDurationValue.Create(Y, Mo, W, Da, H, Mi, S, Ms, Us, Ns);
  end
  else
  begin
    ThrowTypeError(AMethod + ' requires a Duration, string, or object', SSuggestTemporalDurationArg);
    Result := nil;
  end;
end;

{ TGocciaTemporalZonedDateTimeValue }

constructor TGocciaTemporalZonedDateTimeValue.Create(const AEpochMilliseconds: Int64;
  const ASubMillisecondNanoseconds: Integer; const ATimeZone: string);
var
  Shared: TGocciaSharedPrototype;
begin
  inherited Create(nil);
  if ATimeZone = '' then
    ThrowRangeError(SErrorZonedDateTimeRequiresTZ, SSuggestTemporalTimezone);
  if not IsValidTimeZone(ATimeZone) then
    ThrowRangeError(Format(SErrorUnknownTimezone, [ATimeZone]), SSuggestTemporalTimezone);

  FEpochMilliseconds := AEpochMilliseconds;
  FSubMillisecondNanoseconds := ASubMillisecondNanoseconds;
  FTimeZone := ATimeZone;

  // Normalize sub-ms nanoseconds into 0..999999 range
  if (FSubMillisecondNanoseconds >= SUB_MS_NANOSECOND_LIMIT) or (FSubMillisecondNanoseconds < 0) then
  begin
    if FSubMillisecondNanoseconds >= 0 then
    begin
      Inc(FEpochMilliseconds, FSubMillisecondNanoseconds div SUB_MS_NANOSECOND_LIMIT);
      FSubMillisecondNanoseconds := FSubMillisecondNanoseconds mod SUB_MS_NANOSECOND_LIMIT;
    end
    else
    begin
      Inc(FEpochMilliseconds, (FSubMillisecondNanoseconds - (SUB_MS_NANOSECOND_LIMIT - 1)) div SUB_MS_NANOSECOND_LIMIT);
      FSubMillisecondNanoseconds := FSubMillisecondNanoseconds -
        ((FSubMillisecondNanoseconds - (SUB_MS_NANOSECOND_LIMIT - 1)) div SUB_MS_NANOSECOND_LIMIT) * SUB_MS_NANOSECOND_LIMIT;
    end;
  end;

  InitializePrototype;
  Shared := GetTemporalZonedDateTimeShared;
  if Assigned(Shared) then
    FPrototype := Shared.Prototype;
end;

procedure TGocciaTemporalZonedDateTimeValue.InitializePrototype;
var
  Members: TGocciaMemberCollection;
  Shared: TGocciaSharedPrototype;
begin
  if not Assigned(CurrentRealm) then Exit;
  if Assigned(GetTemporalZonedDateTimeShared) then Exit;
  Shared := TGocciaSharedPrototype.Create(Self);
  CurrentRealm.SetOwnedSlot(GTemporalZonedDateTimeSharedSlot, Shared);
  if Length(FPrototypeMembers) = 0 then
  begin
    Members := TGocciaMemberCollection.Create;
    try
      Members.AddAccessor('calendarId', GetCalendarId, nil, [pfConfigurable]);
      Members.AddAccessor('timeZoneId', GetTimeZoneId, nil, [pfConfigurable]);
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
      Members.AddAccessor('hoursInDay', GetHoursInDay, nil, [pfConfigurable]);
      Members.AddAccessor('hour', GetHour, nil, [pfConfigurable]);
      Members.AddAccessor('minute', GetMinute, nil, [pfConfigurable]);
      Members.AddAccessor('second', GetSecond, nil, [pfConfigurable]);
      Members.AddAccessor('millisecond', GetMillisecond, nil, [pfConfigurable]);
      Members.AddAccessor('microsecond', GetMicrosecond, nil, [pfConfigurable]);
      Members.AddAccessor('nanosecond', GetNanosecond, nil, [pfConfigurable]);
      Members.AddAccessor('offset', GetOffset, nil, [pfConfigurable]);
      Members.AddAccessor('offsetNanoseconds', GetOffsetNanoseconds, nil, [pfConfigurable]);
      Members.AddAccessor('epochMilliseconds', GetEpochMilliseconds, nil, [pfConfigurable]);
      Members.AddAccessor('epochNanoseconds', GetEpochNanoseconds, nil, [pfConfigurable]);
      Members.AddMethod(ZonedDateTimeWith, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(ZonedDateTimeWithPlainDate, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(ZonedDateTimeWithPlainTime, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(ZonedDateTimeWithTimeZone, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(ZonedDateTimeAdd, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(ZonedDateTimeSubtract, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(ZonedDateTimeUntil, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(ZonedDateTimeSince, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(ZonedDateTimeRound, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(ZonedDateTimeEquals, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(ZonedDateTimeStartOfDay, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(ZonedDateTimeToInstant, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(ZonedDateTimeToPlainDate, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(ZonedDateTimeToPlainTime, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(ZonedDateTimeToPlainDateTime, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(ZonedDateTimeToString, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(ZonedDateTimeToJSON, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(ZonedDateTimeValueOf, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(ZonedDateTimeToLocaleString, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddSymbolDataProperty(
        TGocciaSymbolValue.WellKnownToStringTag,
        TGocciaStringLiteralValue.Create('Temporal.ZonedDateTime'),
        [pfConfigurable]);
      FPrototypeMembers := Members.ToDefinitions;
    finally
      Members.Free;
    end;
  end;
  RegisterMemberDefinitions(Shared.Prototype, FPrototypeMembers);
end;

class procedure TGocciaTemporalZonedDateTimeValue.ExposePrototype(const AConstructor: TGocciaObjectValue);
var
  Shared: TGocciaSharedPrototype;
begin
  Shared := GetTemporalZonedDateTimeShared;
  if not Assigned(Shared) then
  begin
    TGocciaTemporalZonedDateTimeValue.Create(0, 0, 'UTC');
    Shared := GetTemporalZonedDateTimeShared;
  end;
  ExposeSharedPrototypeOnConstructor(Shared, AConstructor);
end;

function TGocciaTemporalZonedDateTimeValue.ToStringTag: string;
begin
  Result := 'Temporal.ZonedDateTime';
end;

{ Getters }

function TGocciaTemporalZonedDateTimeValue.GetCalendarId(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  AsZonedDateTime(AThisValue, 'get ZonedDateTime.calendarId');
  Result := TGocciaStringLiteralValue.Create('iso8601');
end;

function TGocciaTemporalZonedDateTimeValue.GetTimeZoneId(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaStringLiteralValue.Create(AsZonedDateTime(AThisValue, 'get ZonedDateTime.timeZoneId').FTimeZone);
end;

function TGocciaTemporalZonedDateTimeValue.GetYear(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Zdt: TGocciaTemporalZonedDateTimeValue;
  LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs: Integer;
begin
  Zdt := AsZonedDateTime(AThisValue, 'get ZonedDateTime.year');
  ComputeLocalComponents(Zdt, LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs);
  Result := TGocciaNumberLiteralValue.Create(LYear);
end;

function TGocciaTemporalZonedDateTimeValue.GetMonth(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Zdt: TGocciaTemporalZonedDateTimeValue;
  LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs: Integer;
begin
  Zdt := AsZonedDateTime(AThisValue, 'get ZonedDateTime.month');
  ComputeLocalComponents(Zdt, LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs);
  Result := TGocciaNumberLiteralValue.Create(LMonth);
end;

function TGocciaTemporalZonedDateTimeValue.GetMonthCode(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Zdt: TGocciaTemporalZonedDateTimeValue;
  LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs: Integer;
begin
  Zdt := AsZonedDateTime(AThisValue, 'get ZonedDateTime.monthCode');
  ComputeLocalComponents(Zdt, LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs);
  Result := TGocciaStringLiteralValue.Create('M' + PadTwo(LMonth));
end;

function TGocciaTemporalZonedDateTimeValue.GetDay(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Zdt: TGocciaTemporalZonedDateTimeValue;
  LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs: Integer;
begin
  Zdt := AsZonedDateTime(AThisValue, 'get ZonedDateTime.day');
  ComputeLocalComponents(Zdt, LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs);
  Result := TGocciaNumberLiteralValue.Create(LDay);
end;

function TGocciaTemporalZonedDateTimeValue.GetDayOfWeek(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Zdt: TGocciaTemporalZonedDateTimeValue;
  LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs: Integer;
begin
  Zdt := AsZonedDateTime(AThisValue, 'get ZonedDateTime.dayOfWeek');
  ComputeLocalComponents(Zdt, LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs);
  Result := TGocciaNumberLiteralValue.Create(Goccia.Temporal.Utils.DayOfWeek(LYear, LMonth, LDay));
end;

function TGocciaTemporalZonedDateTimeValue.GetDayOfYear(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Zdt: TGocciaTemporalZonedDateTimeValue;
  LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs: Integer;
begin
  Zdt := AsZonedDateTime(AThisValue, 'get ZonedDateTime.dayOfYear');
  ComputeLocalComponents(Zdt, LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs);
  Result := TGocciaNumberLiteralValue.Create(Goccia.Temporal.Utils.DayOfYear(LYear, LMonth, LDay));
end;

function TGocciaTemporalZonedDateTimeValue.GetWeekOfYear(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Zdt: TGocciaTemporalZonedDateTimeValue;
  LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs: Integer;
begin
  Zdt := AsZonedDateTime(AThisValue, 'get ZonedDateTime.weekOfYear');
  ComputeLocalComponents(Zdt, LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs);
  Result := TGocciaNumberLiteralValue.Create(Goccia.Temporal.Utils.WeekOfYear(LYear, LMonth, LDay));
end;

function TGocciaTemporalZonedDateTimeValue.GetYearOfWeek(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Zdt: TGocciaTemporalZonedDateTimeValue;
  LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs: Integer;
begin
  Zdt := AsZonedDateTime(AThisValue, 'get ZonedDateTime.yearOfWeek');
  ComputeLocalComponents(Zdt, LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs);
  Result := TGocciaNumberLiteralValue.Create(Goccia.Temporal.Utils.YearOfWeek(LYear, LMonth, LDay));
end;

function TGocciaTemporalZonedDateTimeValue.GetDaysInWeek(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  AsZonedDateTime(AThisValue, 'get ZonedDateTime.daysInWeek');
  Result := TGocciaNumberLiteralValue.Create(DAYS_PER_WEEK);
end;

function TGocciaTemporalZonedDateTimeValue.GetDaysInMonth(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Zdt: TGocciaTemporalZonedDateTimeValue;
  LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs: Integer;
begin
  Zdt := AsZonedDateTime(AThisValue, 'get ZonedDateTime.daysInMonth');
  ComputeLocalComponents(Zdt, LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs);
  Result := TGocciaNumberLiteralValue.Create(Goccia.Temporal.Utils.DaysInMonth(LYear, LMonth));
end;

function TGocciaTemporalZonedDateTimeValue.GetDaysInYear(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Zdt: TGocciaTemporalZonedDateTimeValue;
  LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs: Integer;
begin
  Zdt := AsZonedDateTime(AThisValue, 'get ZonedDateTime.daysInYear');
  ComputeLocalComponents(Zdt, LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs);
  Result := TGocciaNumberLiteralValue.Create(Goccia.Temporal.Utils.DaysInYear(LYear));
end;

function TGocciaTemporalZonedDateTimeValue.GetMonthsInYear(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  AsZonedDateTime(AThisValue, 'get ZonedDateTime.monthsInYear');
  Result := TGocciaNumberLiteralValue.Create(MONTHS_PER_YEAR);
end;

function TGocciaTemporalZonedDateTimeValue.GetInLeapYear(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Zdt: TGocciaTemporalZonedDateTimeValue;
  LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs: Integer;
begin
  Zdt := AsZonedDateTime(AThisValue, 'get ZonedDateTime.inLeapYear');
  ComputeLocalComponents(Zdt, LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs);
  if Goccia.Temporal.Utils.IsLeapYear(LYear) then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;

function TGocciaTemporalZonedDateTimeValue.GetHoursInDay(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Zdt: TGocciaTemporalZonedDateTimeValue;
  LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs: Integer;
  StartOfDayMs, StartOfNextDayMs: Int64;
  NextDayRec: TTemporalDateRecord;
begin
  Zdt := AsZonedDateTime(AThisValue, 'get ZonedDateTime.hoursInDay');
  ComputeLocalComponents(Zdt, LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs);
  StartOfDayMs := LocalToEpochMs(LYear, LMonth, LDay, 0, 0, 0, 0, Zdt.FTimeZone);
  NextDayRec := AddDaysToDate(LYear, LMonth, LDay, 1);
  StartOfNextDayMs := LocalToEpochMs(NextDayRec.Year, NextDayRec.Month, NextDayRec.Day, 0, 0, 0, 0, Zdt.FTimeZone);
  Result := TGocciaNumberLiteralValue.Create((StartOfNextDayMs - StartOfDayMs) / 3600000);
end;

function TGocciaTemporalZonedDateTimeValue.GetHour(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Zdt: TGocciaTemporalZonedDateTimeValue;
  LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs: Integer;
begin
  Zdt := AsZonedDateTime(AThisValue, 'get ZonedDateTime.hour');
  ComputeLocalComponents(Zdt, LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs);
  Result := TGocciaNumberLiteralValue.Create(LHour);
end;

function TGocciaTemporalZonedDateTimeValue.GetMinute(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Zdt: TGocciaTemporalZonedDateTimeValue;
  LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs: Integer;
begin
  Zdt := AsZonedDateTime(AThisValue, 'get ZonedDateTime.minute');
  ComputeLocalComponents(Zdt, LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs);
  Result := TGocciaNumberLiteralValue.Create(LMinute);
end;

function TGocciaTemporalZonedDateTimeValue.GetSecond(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Zdt: TGocciaTemporalZonedDateTimeValue;
  LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs: Integer;
begin
  Zdt := AsZonedDateTime(AThisValue, 'get ZonedDateTime.second');
  ComputeLocalComponents(Zdt, LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs);
  Result := TGocciaNumberLiteralValue.Create(LSecond);
end;

function TGocciaTemporalZonedDateTimeValue.GetMillisecond(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Zdt: TGocciaTemporalZonedDateTimeValue;
  LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs: Integer;
begin
  Zdt := AsZonedDateTime(AThisValue, 'get ZonedDateTime.millisecond');
  ComputeLocalComponents(Zdt, LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs);
  Result := TGocciaNumberLiteralValue.Create(LMs);
end;

function TGocciaTemporalZonedDateTimeValue.GetMicrosecond(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Zdt: TGocciaTemporalZonedDateTimeValue;
  LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs: Integer;
begin
  Zdt := AsZonedDateTime(AThisValue, 'get ZonedDateTime.microsecond');
  ComputeLocalComponents(Zdt, LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs);
  Result := TGocciaNumberLiteralValue.Create(LUs);
end;

function TGocciaTemporalZonedDateTimeValue.GetNanosecond(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Zdt: TGocciaTemporalZonedDateTimeValue;
  LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs: Integer;
begin
  Zdt := AsZonedDateTime(AThisValue, 'get ZonedDateTime.nanosecond');
  ComputeLocalComponents(Zdt, LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs);
  Result := TGocciaNumberLiteralValue.Create(LNs);
end;

function TGocciaTemporalZonedDateTimeValue.GetOffset(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Zdt: TGocciaTemporalZonedDateTimeValue;
  OffsetSeconds: Integer;
begin
  Zdt := AsZonedDateTime(AThisValue, 'get ZonedDateTime.offset');
  OffsetSeconds := GetUtcOffsetSeconds(Zdt.FTimeZone, Zdt.FEpochMilliseconds div MILLISECONDS_PER_SECOND);
  Result := TGocciaStringLiteralValue.Create(FormatOffsetString(OffsetSeconds));
end;

function TGocciaTemporalZonedDateTimeValue.GetOffsetNanoseconds(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Zdt: TGocciaTemporalZonedDateTimeValue;
  OffsetSeconds: Integer;
begin
  Zdt := AsZonedDateTime(AThisValue, 'get ZonedDateTime.offsetNanoseconds');
  OffsetSeconds := GetUtcOffsetSeconds(Zdt.FTimeZone, Zdt.FEpochMilliseconds div MILLISECONDS_PER_SECOND);
  Result := TGocciaNumberLiteralValue.Create(Int64(OffsetSeconds) * NANOSECONDS_PER_SECOND);
end;

function TGocciaTemporalZonedDateTimeValue.GetEpochMilliseconds(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaNumberLiteralValue.Create(AsZonedDateTime(AThisValue, 'get ZonedDateTime.epochMilliseconds').FEpochMilliseconds);
end;

function TGocciaTemporalZonedDateTimeValue.GetEpochNanoseconds(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Zdt: TGocciaTemporalZonedDateTimeValue;
  BigNs: TBigInteger;
begin
  Zdt := AsZonedDateTime(AThisValue, 'get ZonedDateTime.epochNanoseconds');
  BigNs := TBigInteger.FromInt64(Zdt.FEpochMilliseconds)
    .Multiply(TBigInteger.FromInt64(1000000))
    .Add(TBigInteger.FromInt64(Zdt.FSubMillisecondNanoseconds));
  Result := TGocciaBigIntValue.Create(BigNs);
end;

{ Methods }

// TC39 Temporal §6.3.22 Temporal.ZonedDateTime.prototype.with(temporalZonedDateTimeLike [, options])
function TGocciaTemporalZonedDateTimeValue.ZonedDateTimeWith(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Zdt: TGocciaTemporalZonedDateTimeValue;
  Obj: TGocciaObjectValue;
  V: TGocciaValue;
  LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs: Integer;
  NewYear, NewMonth, NewDay, NewHour, NewMinute, NewSecond, NewMs, NewUs, NewNs: Integer;
  NewEpochMs: Int64;

  function GetFieldOr(const AName: string; const ADefault: Integer): Integer;
  var
    Val: TGocciaValue;
  begin
    Val := Obj.GetProperty(AName);
    if (Val = nil) or (Val is TGocciaUndefinedLiteralValue) then
      Result := ADefault
    else
      Result := Trunc(Val.ToNumberLiteral.Value);
  end;

begin
  Zdt := AsZonedDateTime(AThisValue, 'ZonedDateTime.prototype.with');
  V := AArgs.GetElement(0);
  if not (V is TGocciaObjectValue) then
    ThrowTypeError(Format(SErrorTemporalWithRequiresObject, ['ZonedDateTime']), SSuggestTemporalWithObject);
  Obj := TGocciaObjectValue(V);

  ComputeLocalComponents(Zdt, LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs);

  NewYear := GetFieldOr('year', LYear);
  NewMonth := GetFieldOr('month', LMonth);
  NewDay := GetFieldOr('day', LDay);
  NewHour := GetFieldOr('hour', LHour);
  NewMinute := GetFieldOr('minute', LMinute);
  NewSecond := GetFieldOr('second', LSecond);
  NewMs := GetFieldOr('millisecond', LMs);
  NewUs := GetFieldOr('microsecond', LUs);
  NewNs := GetFieldOr('nanosecond', LNs);

  NewEpochMs := LocalToEpochMs(NewYear, NewMonth, NewDay, NewHour, NewMinute, NewSecond, NewMs, Zdt.FTimeZone);
  Result := TGocciaTemporalZonedDateTimeValue.Create(NewEpochMs, NewUs * 1000 + NewNs, Zdt.FTimeZone);
end;

// TC39 Temporal §6.3.23 Temporal.ZonedDateTime.prototype.withPlainDate(plainDateLike)
function TGocciaTemporalZonedDateTimeValue.ZonedDateTimeWithPlainDate(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Zdt: TGocciaTemporalZonedDateTimeValue;
  Arg: TGocciaValue;
  DateRec: TTemporalDateRecord;
  PlainDate: TGocciaTemporalPlainDateValue;
  LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs: Integer;
  NewEpochMs: Int64;
begin
  Zdt := AsZonedDateTime(AThisValue, 'ZonedDateTime.prototype.withPlainDate');
  Arg := AArgs.GetElement(0);

  if Arg is TGocciaTemporalPlainDateValue then
    PlainDate := TGocciaTemporalPlainDateValue(Arg)
  else if Arg is TGocciaStringLiteralValue then
  begin
    if not CoerceToISODate(TGocciaStringLiteralValue(Arg).Value, DateRec) then
      ThrowRangeError(SErrorInvalidDateStringForZDT, SSuggestTemporalISOFormat);
    PlainDate := TGocciaTemporalPlainDateValue.Create(DateRec.Year, DateRec.Month, DateRec.Day);
  end
  else
  begin
    ThrowTypeError(SErrorZDTWithPlainDateArg, SSuggestTemporalFromArg);
    PlainDate := nil;
  end;

  ComputeLocalComponents(Zdt, LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs);

  NewEpochMs := LocalToEpochMs(PlainDate.Year, PlainDate.Month, PlainDate.Day,
    LHour, LMinute, LSecond, LMs, Zdt.FTimeZone);
  Result := TGocciaTemporalZonedDateTimeValue.Create(NewEpochMs, LUs * 1000 + LNs, Zdt.FTimeZone);
end;

// TC39 Temporal §6.3.24 Temporal.ZonedDateTime.prototype.withPlainTime([plainTimeLike])
function TGocciaTemporalZonedDateTimeValue.ZonedDateTimeWithPlainTime(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Zdt: TGocciaTemporalZonedDateTimeValue;
  Arg: TGocciaValue;
  PlainTime: TGocciaTemporalPlainTimeValue;
  TimeRec: TTemporalTimeRecord;
  LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs: Integer;
  NewHour, NewMinute, NewSecond, NewMs, NewUs, NewNs: Integer;
  NewEpochMs: Int64;
begin
  Zdt := AsZonedDateTime(AThisValue, 'ZonedDateTime.prototype.withPlainTime');
  Arg := AArgs.GetElement(0);

  ComputeLocalComponents(Zdt, LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs);

  NewHour := 0; NewMinute := 0; NewSecond := 0; NewMs := 0; NewUs := 0; NewNs := 0;

  if Assigned(Arg) and not (Arg is TGocciaUndefinedLiteralValue) then
  begin
    if Arg is TGocciaTemporalPlainTimeValue then
    begin
      PlainTime := TGocciaTemporalPlainTimeValue(Arg);
      NewHour := PlainTime.Hour; NewMinute := PlainTime.Minute; NewSecond := PlainTime.Second;
      NewMs := PlainTime.Millisecond; NewUs := PlainTime.Microsecond; NewNs := PlainTime.Nanosecond;
    end
    else if Arg is TGocciaStringLiteralValue then
    begin
      if not CoerceToISOTime(TGocciaStringLiteralValue(Arg).Value, TimeRec) then
        ThrowRangeError(SErrorInvalidTimeStringForZDT, SSuggestTemporalISOFormat);
      NewHour := TimeRec.Hour; NewMinute := TimeRec.Minute; NewSecond := TimeRec.Second;
      NewMs := TimeRec.Millisecond; NewUs := TimeRec.Microsecond; NewNs := TimeRec.Nanosecond;
    end
    else
      ThrowTypeError(SErrorZDTWithPlainTimeArg, SSuggestTemporalFromArg);
  end;

  NewEpochMs := LocalToEpochMs(LYear, LMonth, LDay, NewHour, NewMinute, NewSecond, NewMs, Zdt.FTimeZone);
  Result := TGocciaTemporalZonedDateTimeValue.Create(NewEpochMs, NewUs * 1000 + NewNs, Zdt.FTimeZone);
end;

// TC39 Temporal §6.3.25 Temporal.ZonedDateTime.prototype.withTimeZone(timeZoneLike)
function TGocciaTemporalZonedDateTimeValue.ZonedDateTimeWithTimeZone(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Zdt: TGocciaTemporalZonedDateTimeValue;
  Arg: TGocciaValue;
  NewTimeZone: string;
begin
  Zdt := AsZonedDateTime(AThisValue, 'ZonedDateTime.prototype.withTimeZone');
  Arg := AArgs.GetElement(0);

  if (Arg = nil) or (Arg is TGocciaUndefinedLiteralValue) then
    ThrowTypeError(SErrorZDTWithTimeZoneArg, SSuggestTemporalTimezone);

  NewTimeZone := Arg.ToStringLiteral.Value;
  if NewTimeZone = '' then
    ThrowTypeError(SErrorZDTWithTimeZoneNonEmpty, SSuggestTemporalTimezone);

  // Keep the same epoch instant, just change the timezone
  Result := TGocciaTemporalZonedDateTimeValue.Create(
    Zdt.FEpochMilliseconds, Zdt.FSubMillisecondNanoseconds, NewTimeZone);
end;

// TC39 Temporal §6.3.26 Temporal.ZonedDateTime.prototype.add(temporalDurationLike [, options])
function TGocciaTemporalZonedDateTimeValue.ZonedDateTimeAdd(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Zdt: TGocciaTemporalZonedDateTimeValue;
  Dur: TGocciaTemporalDurationValue;
  LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs: Integer;
  NewYear, NewMonth, NewDay: Integer;
  DateRec: TTemporalDateRecord;
  ExtraDays: Int64;
  Balanced: TTemporalTimeRecord;
  NewEpochMs: Int64;
  NewSubMs: Integer;
begin
  Zdt := AsZonedDateTime(AThisValue, 'ZonedDateTime.prototype.add');
  Dur := CoerceDuration(AArgs.GetElement(0), 'ZonedDateTime.prototype.add');

  // Compute local wall-clock time
  ComputeLocalComponents(Zdt, LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs);

  // Balance time with added duration
  Balanced := BalanceTime(
    LHour + Dur.Hours, LMinute + Dur.Minutes, LSecond + Dur.Seconds,
    LMs + Dur.Milliseconds, LUs + Dur.Microseconds,
    LNs + Dur.Nanoseconds, ExtraDays);

  // Add date components
  NewYear := LYear + Integer(Dur.Years);
  NewMonth := LMonth + Integer(Dur.Months);
  while NewMonth > MONTHS_PER_YEAR do begin Inc(NewYear); Dec(NewMonth, MONTHS_PER_YEAR); end;
  while NewMonth < 1 do begin Dec(NewYear); Inc(NewMonth, MONTHS_PER_YEAR); end;
  NewDay := LDay;
  if NewDay > Goccia.Temporal.Utils.DaysInMonth(NewYear, NewMonth) then
    NewDay := Goccia.Temporal.Utils.DaysInMonth(NewYear, NewMonth);

  DateRec := AddDaysToDate(NewYear, NewMonth, NewDay, Dur.Weeks * DAYS_PER_WEEK + Dur.Days + ExtraDays);

  // Convert local wall-clock back to epoch
  NewEpochMs := LocalToEpochMs(DateRec.Year, DateRec.Month, DateRec.Day,
    Balanced.Hour, Balanced.Minute, Balanced.Second, Balanced.Millisecond, Zdt.FTimeZone);
  NewSubMs := Balanced.Microsecond * 1000 + Balanced.Nanosecond;

  Result := TGocciaTemporalZonedDateTimeValue.Create(NewEpochMs, NewSubMs, Zdt.FTimeZone);
end;

// TC39 Temporal §6.3.27 Temporal.ZonedDateTime.prototype.subtract(temporalDurationLike [, options])
function TGocciaTemporalZonedDateTimeValue.ZonedDateTimeSubtract(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Dur: TGocciaTemporalDurationValue;
  NegDur: TGocciaTemporalDurationValue;
  NewArgs: TGocciaArgumentsCollection;
begin
  Dur := CoerceDuration(AArgs.GetElement(0), 'ZonedDateTime.prototype.subtract');

  NegDur := TGocciaTemporalDurationValue.Create(
    -Dur.Years, -Dur.Months, -Dur.Weeks, -Dur.Days,
    -Dur.Hours, -Dur.Minutes, -Dur.Seconds,
    -Dur.Milliseconds, -Dur.Microseconds, -Dur.Nanoseconds);

  NewArgs := TGocciaArgumentsCollection.Create([NegDur]);
  try
    Result := ZonedDateTimeAdd(NewArgs, AThisValue);
  finally
    NewArgs.Free;
  end;
end;

function ZonedDiffToUnits(const ADiffNs: TBigInteger;
  const ALargestUnit: TTemporalUnit): TGocciaValue;
var
  Hours, Minutes, Seconds, Milliseconds, Microseconds, Nanoseconds: TBigInteger;
  Days: Int64;
begin
  BalanceTimeDurationToFields(ADiffNs, ALargestUnit,
    Hours, Minutes, Seconds, Milliseconds, Microseconds, Nanoseconds, Days);
  Result := TGocciaTemporalDurationValue.CreateFromBigIntegers(
    TBigInteger.Zero, TBigInteger.Zero, TBigInteger.Zero, TBigInteger.FromInt64(Days),
    Hours, Minutes, Seconds, Milliseconds, Microseconds, Nanoseconds);
end;

// TC39 Temporal §6.3.28 Temporal.ZonedDateTime.prototype.until(other [, options])
function TGocciaTemporalZonedDateTimeValue.ZonedDateTimeUntil(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Zdt, Other: TGocciaTemporalZonedDateTimeValue;
  OptionsObj: TGocciaObjectValue;
  LargestUnit, SmallestUnit: TTemporalUnit;
  RMode: TTemporalRoundingMode;
  RIncrement: Integer;
  StartNs, EndNs, DiffNs, IncrementNs: TBigInteger;
  Y1, M1, D1, H1, Mi1, S1, Ms1, Us1, Ns1: Integer;
  Y2, M2, D2, H2, Mi2, S2, Ms2, Us2, Ns2: Integer;
  T1Ns, T2Ns, TimeDiffNs, TotalNs, Sgn, AbsTimeNs: Int64;
  AdjDate: TTemporalDateRecord;
  AdjY2, AdjM2, AdjD2: Integer;
  Years, Months, Weeks, Days: Int64;
  RH, RM, RS, RMs, RUs, RNs: Int64;
  Dur: TGocciaTemporalDurationValue;
  RY, RMo, RW, RD: Int64;
begin
  Zdt := AsZonedDateTime(AThisValue, 'ZonedDateTime.prototype.until');
  Other := CoerceZonedDateTime(AArgs.GetElement(0), 'ZonedDateTime.prototype.until');

  OptionsObj := GetDiffOptions(AArgs, 1);
  LargestUnit := GetLargestUnit(OptionsObj, tuHour);
  if LargestUnit = tuAuto then LargestUnit := tuHour;

  SmallestUnit := GetSmallestUnit(OptionsObj, tuNanosecond);
  if Ord(LargestUnit) > Ord(SmallestUnit) then
    ThrowRangeError(SErrorDurationRoundLargestSmallerThanSmallest, SSuggestTemporalRoundArg);
  RMode := GetRoundingMode(OptionsObj, rmTrunc);
  RIncrement := GetRoundingIncrement(OptionsObj, 1);
  ValidateRoundingIncrement(RIncrement, SmallestUnit, LargestUnit);

  if LargestUnit in [tuYear, tuMonth, tuWeek, tuDay] then
  begin
    // Calendar-aware: use wall-clock components (DST-correct for day counting)
    ComputeLocalComponents(Zdt, Y1, M1, D1, H1, Mi1, S1, Ms1, Us1, Ns1);
    ComputeLocalComponents(Other, Y2, M2, D2, H2, Mi2, S2, Ms2, Us2, Ns2);

    T1Ns := Int64(Ns1) + Int64(Us1) * 1000 + Int64(Ms1) * 1000000 +
             Int64(S1) * Int64(1000000000) + Int64(Mi1) * Int64(60000000000) +
             Int64(H1) * Int64(3600000000000);
    T2Ns := Int64(Ns2) + Int64(Us2) * 1000 + Int64(Ms2) * 1000000 +
             Int64(S2) * Int64(1000000000) + Int64(Mi2) * Int64(60000000000) +
             Int64(H2) * Int64(3600000000000);
    TimeDiffNs := T2Ns - T1Ns;

    TotalNs := (DateToEpochDays(Y2, M2, D2) - DateToEpochDays(Y1, M1, D1)) *
               Int64(86400000000000) + TimeDiffNs;

    if TotalNs > 0 then
      Sgn := 1
    else if TotalNs < 0 then
      Sgn := -1
    else
    begin
      Result := TGocciaTemporalDurationValue.Create(0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
      Exit;
    end;

    AdjY2 := Y2; AdjM2 := M2; AdjD2 := D2;
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

    CalendarDateUntil(Y1, M1, D1, AdjY2, AdjM2, AdjD2, LargestUnit,
      Years, Months, Weeks, Days);

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
        Y1, M1, D1, LargestUnit, SmallestUnit, RMode, RIncrement);

    Result := TGocciaTemporalDurationValue.Create(Years, Months, Weeks, Days,
      RH, RM, RS, RMs, RUs, RNs);
  end
  else
  begin
    // Sub-day: use exact epoch nanosecond difference.
    StartNs := EpochNanosecondsFromParts(Zdt.FEpochMilliseconds, Zdt.FSubMillisecondNanoseconds);
    EndNs := EpochNanosecondsFromParts(Other.FEpochMilliseconds, Other.FSubMillisecondNanoseconds);
    DiffNs := TimeDurationFromEpochNanosecondsDifference(EndNs, StartNs);
    if (SmallestUnit <> tuNanosecond) or (RIncrement <> 1) then
    begin
      IncrementNs := TBigInteger.FromInt64(UnitToNanoseconds(SmallestUnit))
        .Multiply(TBigInteger.FromInt64(RIncrement));
      DiffNs := RoundTimeDurationToIncrement(DiffNs, IncrementNs, RMode);
    end;
    Result := ZonedDiffToUnits(DiffNs, LargestUnit);
  end;
end;

// TC39 Temporal §6.3.29 Temporal.ZonedDateTime.prototype.since(other [, options])
function TGocciaTemporalZonedDateTimeValue.ZonedDateTimeSince(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Zdt, Other: TGocciaTemporalZonedDateTimeValue;
  OptionsObj: TGocciaObjectValue;
  LargestUnit, SmallestUnit: TTemporalUnit;
  RMode: TTemporalRoundingMode;
  RIncrement: Integer;
  StartNs, EndNs, DiffNs, IncrementNs: TBigInteger;
  Y1, M1, D1, H1, Mi1, S1, Ms1, Us1, Ns1: Integer;
  Y2, M2, D2, H2, Mi2, S2, Ms2, Us2, Ns2: Integer;
  T1Ns, T2Ns, TimeDiffNs, TotalNs, Sgn, AbsTimeNs: Int64;
  AdjDate: TTemporalDateRecord;
  AdjY1, AdjM1, AdjD1: Integer;
  Years, Months, Weeks, Days: Int64;
  RH, RM, RS, RMs, RUs, RNs: Int64;
  Dur: TGocciaTemporalDurationValue;
  RY, RMo, RW, RD: Int64;
begin
  Zdt := AsZonedDateTime(AThisValue, 'ZonedDateTime.prototype.since');
  Other := CoerceZonedDateTime(AArgs.GetElement(0), 'ZonedDateTime.prototype.since');

  OptionsObj := GetDiffOptions(AArgs, 1);
  LargestUnit := GetLargestUnit(OptionsObj, tuHour);
  if LargestUnit = tuAuto then LargestUnit := tuHour;

  SmallestUnit := GetSmallestUnit(OptionsObj, tuNanosecond);
  if Ord(LargestUnit) > Ord(SmallestUnit) then
    ThrowRangeError(SErrorDurationRoundLargestSmallerThanSmallest, SSuggestTemporalRoundArg);
  RMode := GetRoundingMode(OptionsObj, rmTrunc);
  RIncrement := GetRoundingIncrement(OptionsObj, 1);
  ValidateRoundingIncrement(RIncrement, SmallestUnit, LargestUnit);

  if LargestUnit in [tuYear, tuMonth, tuWeek, tuDay] then
  begin
    // Calendar-aware: since = other.until(this) with same options
    ComputeLocalComponents(Other, Y1, M1, D1, H1, Mi1, S1, Ms1, Us1, Ns1);
    ComputeLocalComponents(Zdt, Y2, M2, D2, H2, Mi2, S2, Ms2, Us2, Ns2);

    T1Ns := Int64(Ns1) + Int64(Us1) * 1000 + Int64(Ms1) * 1000000 +
             Int64(S1) * Int64(1000000000) + Int64(Mi1) * Int64(60000000000) +
             Int64(H1) * Int64(3600000000000);
    T2Ns := Int64(Ns2) + Int64(Us2) * 1000 + Int64(Ms2) * 1000000 +
             Int64(S2) * Int64(1000000000) + Int64(Mi2) * Int64(60000000000) +
             Int64(H2) * Int64(3600000000000);
    TimeDiffNs := T2Ns - T1Ns;

    TotalNs := (DateToEpochDays(Y2, M2, D2) - DateToEpochDays(Y1, M1, D1)) *
               Int64(86400000000000) + TimeDiffNs;

    if TotalNs > 0 then
      Sgn := 1
    else if TotalNs < 0 then
      Sgn := -1
    else
    begin
      Result := TGocciaTemporalDurationValue.Create(0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
      Exit;
    end;

    AdjY1 := Y2; AdjM1 := M2; AdjD1 := D2;
    if (Sgn > 0) and (TimeDiffNs < 0) then
    begin
      AdjDate := AddDaysToDate(AdjY1, AdjM1, AdjD1, -1);
      AdjY1 := AdjDate.Year; AdjM1 := AdjDate.Month; AdjD1 := AdjDate.Day;
      TimeDiffNs := TimeDiffNs + Int64(86400000000000);
    end
    else if (Sgn < 0) and (TimeDiffNs > 0) then
    begin
      AdjDate := AddDaysToDate(AdjY1, AdjM1, AdjD1, 1);
      AdjY1 := AdjDate.Year; AdjM1 := AdjDate.Month; AdjD1 := AdjDate.Day;
      TimeDiffNs := TimeDiffNs - Int64(86400000000000);
    end;

    CalendarDateUntil(Y1, M1, D1, AdjY1, AdjM1, AdjD1, LargestUnit,
      Years, Months, Weeks, Days);

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
        Y1, M1, D1, LargestUnit, SmallestUnit, RMode, RIncrement);

    Result := TGocciaTemporalDurationValue.Create(Years, Months, Weeks, Days,
      RH, RM, RS, RMs, RUs, RNs);
  end
  else
  begin
    // Sub-day: use exact epoch nanosecond difference (this - other).
    StartNs := EpochNanosecondsFromParts(Other.FEpochMilliseconds, Other.FSubMillisecondNanoseconds);
    EndNs := EpochNanosecondsFromParts(Zdt.FEpochMilliseconds, Zdt.FSubMillisecondNanoseconds);
    DiffNs := TimeDurationFromEpochNanosecondsDifference(EndNs, StartNs);
    if (SmallestUnit <> tuNanosecond) or (RIncrement <> 1) then
    begin
      IncrementNs := TBigInteger.FromInt64(UnitToNanoseconds(SmallestUnit))
        .Multiply(TBigInteger.FromInt64(RIncrement));
      DiffNs := RoundTimeDurationToIncrement(DiffNs, IncrementNs, RMode);
    end;
    Result := ZonedDiffToUnits(DiffNs, LargestUnit);
  end;
end;

// TC39 Temporal §6.3.30 Temporal.ZonedDateTime.prototype.round(roundTo)
function TGocciaTemporalZonedDateTimeValue.ZonedDateTimeRound(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Zdt: TGocciaTemporalZonedDateTimeValue;
  UnitStr: string;
  Arg: TGocciaValue;
  SmallestUnit: TTemporalUnit;
  Mode: TTemporalRoundingMode;
  Increment: Integer;
  OptionsObj: TGocciaObjectValue;
  LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs: Integer;
  TotalNs, Divisor, Rounded, ExtraDays: Int64;
  Balanced: TTemporalTimeRecord;
  DateRec: TTemporalDateRecord;
  NewEpochMs: Int64;
  NewSubMs: Integer;
  StartEpochMs, NextEpochMs, DayNs: Int64;
begin
  Zdt := AsZonedDateTime(AThisValue, 'ZonedDateTime.prototype.round');
  Arg := AArgs.GetElement(0);

  Mode := rmHalfExpand;
  Increment := 1;

  if Arg is TGocciaStringLiteralValue then
  begin
    UnitStr := TGocciaStringLiteralValue(Arg).Value;
  end
  else if Arg is TGocciaObjectValue then
  begin
    OptionsObj := TGocciaObjectValue(Arg);
    SmallestUnit := GetSmallestUnit(OptionsObj, tuNone);
    if SmallestUnit = tuNone then
      ThrowRangeError(SErrorTemporalRoundRequiresSmallestUnit, SSuggestTemporalRoundArg);
    Mode := GetRoundingMode(OptionsObj, rmHalfExpand);
    Increment := GetRoundingIncrement(OptionsObj, 1);
    // Convert unit enum to string for the existing unit dispatch
    case SmallestUnit of
      tuDay: UnitStr := 'day';
      tuHour: UnitStr := 'hour';
      tuMinute: UnitStr := 'minute';
      tuSecond: UnitStr := 'second';
      tuMillisecond: UnitStr := 'millisecond';
      tuMicrosecond: UnitStr := 'microsecond';
      tuNanosecond: UnitStr := 'nanosecond';
    else
      ThrowRangeError(SErrorInvalidZDTRoundUnit, SSuggestTemporalValidUnits);
      UnitStr := '';
    end;
  end
  else
  begin
    ThrowTypeError(Format(SErrorTemporalRoundRequiresStringOrOptions, ['ZonedDateTime']), SSuggestTemporalRoundArg);
    UnitStr := '';
  end;

  ComputeLocalComponents(Zdt, LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs);

  if UnitStr = 'day' then
  begin
    // Compute actual day length in nanoseconds (DST-aware)
    StartEpochMs := LocalToEpochMs(LYear, LMonth, LDay, 0, 0, 0, 0, Zdt.FTimeZone);
    DateRec := AddDaysToDate(LYear, LMonth, LDay, 1);
    NextEpochMs := LocalToEpochMs(DateRec.Year, DateRec.Month, DateRec.Day, 0, 0, 0, 0, Zdt.FTimeZone);
    DayNs := (NextEpochMs - StartEpochMs) * NANOSECONDS_PER_MILLISECOND;
    if DayNs <= 0 then
      DayNs := NANOSECONDS_PER_DAY;  // Fallback for edge cases

    TotalNs := Int64(LHour) * NANOSECONDS_PER_HOUR +
               Int64(LMinute) * NANOSECONDS_PER_MINUTE +
               Int64(LSecond) * NANOSECONDS_PER_SECOND +
               Int64(LMs) * NANOSECONDS_PER_MILLISECOND +
               Int64(LUs) * NANOSECONDS_PER_MICROSECOND +
               LNs;
    Divisor := DayNs * Increment;
    Rounded := RoundWithMode(TotalNs, Divisor, Mode);
    ExtraDays := Rounded div DayNs;
    if ExtraDays > 0 then
      DateRec := AddDaysToDate(LYear, LMonth, LDay, ExtraDays)
    else
    begin
      DateRec.Year := LYear;
      DateRec.Month := LMonth;
      DateRec.Day := LDay;
    end;
    NewEpochMs := LocalToEpochMs(DateRec.Year, DateRec.Month, DateRec.Day, 0, 0, 0, 0, Zdt.FTimeZone);
    Result := TGocciaTemporalZonedDateTimeValue.Create(NewEpochMs, 0, Zdt.FTimeZone);
    Exit;
  end;

  TotalNs := Int64(LNs) + Int64(LUs) * NANOSECONDS_PER_MICROSECOND +
             Int64(LMs) * NANOSECONDS_PER_MILLISECOND +
             Int64(LSecond) * NANOSECONDS_PER_SECOND +
             Int64(LMinute) * NANOSECONDS_PER_MINUTE +
             Int64(LHour) * NANOSECONDS_PER_HOUR;

  if UnitStr = 'hour' then Divisor := NANOSECONDS_PER_HOUR
  else if UnitStr = 'minute' then Divisor := NANOSECONDS_PER_MINUTE
  else if UnitStr = 'second' then Divisor := NANOSECONDS_PER_SECOND
  else if UnitStr = 'millisecond' then Divisor := NANOSECONDS_PER_MILLISECOND
  else if UnitStr = 'microsecond' then Divisor := NANOSECONDS_PER_MICROSECOND
  else if UnitStr = 'nanosecond' then Divisor := 1
  else
  begin
    ThrowRangeError(Format(SErrorTemporalInvalidUnitFor, ['ZonedDateTime.prototype.round', UnitStr]), SSuggestTemporalValidUnits);
    Divisor := 1;
  end;

  Divisor := Divisor * Increment;
  Rounded := RoundWithMode(TotalNs, Divisor, Mode);
  Balanced := BalanceTime(0, 0, 0, 0, 0, Rounded, ExtraDays);
  DateRec := AddDaysToDate(LYear, LMonth, LDay, ExtraDays);

  NewEpochMs := LocalToEpochMs(DateRec.Year, DateRec.Month, DateRec.Day,
    Balanced.Hour, Balanced.Minute, Balanced.Second, Balanced.Millisecond, Zdt.FTimeZone);
  NewSubMs := Balanced.Microsecond * 1000 + Balanced.Nanosecond;

  Result := TGocciaTemporalZonedDateTimeValue.Create(NewEpochMs, NewSubMs, Zdt.FTimeZone);
end;

// TC39 Temporal §6.3.31 Temporal.ZonedDateTime.prototype.equals(other)
function TGocciaTemporalZonedDateTimeValue.ZonedDateTimeEquals(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Zdt, Other: TGocciaTemporalZonedDateTimeValue;
begin
  Zdt := AsZonedDateTime(AThisValue, 'ZonedDateTime.prototype.equals');
  Other := CoerceZonedDateTime(AArgs.GetElement(0), 'ZonedDateTime.prototype.equals');

  if (Zdt.FEpochMilliseconds = Other.FEpochMilliseconds) and
     (Zdt.FSubMillisecondNanoseconds = Other.FSubMillisecondNanoseconds) and
     (Zdt.FTimeZone = Other.FTimeZone) then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;

// TC39 Temporal §6.3.32 Temporal.ZonedDateTime.prototype.startOfDay()
function TGocciaTemporalZonedDateTimeValue.ZonedDateTimeStartOfDay(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Zdt: TGocciaTemporalZonedDateTimeValue;
  LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs: Integer;
  NewEpochMs: Int64;
begin
  Zdt := AsZonedDateTime(AThisValue, 'ZonedDateTime.prototype.startOfDay');
  ComputeLocalComponents(Zdt, LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs);

  // Midnight of the local date
  NewEpochMs := LocalToEpochMs(LYear, LMonth, LDay, 0, 0, 0, 0, Zdt.FTimeZone);
  Result := TGocciaTemporalZonedDateTimeValue.Create(NewEpochMs, 0, Zdt.FTimeZone);
end;

// TC39 Temporal §6.3.33 Temporal.ZonedDateTime.prototype.toInstant()
function TGocciaTemporalZonedDateTimeValue.ZonedDateTimeToInstant(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Zdt: TGocciaTemporalZonedDateTimeValue;
begin
  Zdt := AsZonedDateTime(AThisValue, 'ZonedDateTime.prototype.toInstant');
  Result := TGocciaTemporalInstantValue.Create(Zdt.FEpochMilliseconds, Zdt.FSubMillisecondNanoseconds);
end;

// TC39 Temporal §6.3.34 Temporal.ZonedDateTime.prototype.toPlainDate()
function TGocciaTemporalZonedDateTimeValue.ZonedDateTimeToPlainDate(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Zdt: TGocciaTemporalZonedDateTimeValue;
  LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs: Integer;
begin
  Zdt := AsZonedDateTime(AThisValue, 'ZonedDateTime.prototype.toPlainDate');
  ComputeLocalComponents(Zdt, LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs);
  Result := TGocciaTemporalPlainDateValue.Create(LYear, LMonth, LDay);
end;

// TC39 Temporal §6.3.35 Temporal.ZonedDateTime.prototype.toPlainTime()
function TGocciaTemporalZonedDateTimeValue.ZonedDateTimeToPlainTime(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Zdt: TGocciaTemporalZonedDateTimeValue;
  LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs: Integer;
begin
  Zdt := AsZonedDateTime(AThisValue, 'ZonedDateTime.prototype.toPlainTime');
  ComputeLocalComponents(Zdt, LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs);
  Result := TGocciaTemporalPlainTimeValue.Create(LHour, LMinute, LSecond, LMs, LUs, LNs);
end;

// TC39 Temporal §6.3.36 Temporal.ZonedDateTime.prototype.toPlainDateTime()
function TGocciaTemporalZonedDateTimeValue.ZonedDateTimeToPlainDateTime(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Zdt: TGocciaTemporalZonedDateTimeValue;
  LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs: Integer;
begin
  Zdt := AsZonedDateTime(AThisValue, 'ZonedDateTime.prototype.toPlainDateTime');
  ComputeLocalComponents(Zdt, LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs);
  Result := TGocciaTemporalPlainDateTimeValue.Create(LYear, LMonth, LDay,
    LHour, LMinute, LSecond, LMs, LUs, LNs);
end;

// TC39 Temporal §6.3.37 Temporal.ZonedDateTime.prototype.toString([options])
function TGocciaTemporalZonedDateTimeValue.ZonedDateTimeToString(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Zdt: TGocciaTemporalZonedDateTimeValue;
  LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs: Integer;
  SavedHour, SavedMinute, SavedSecond, SavedMs: Integer;
  OffsetSeconds: Integer;
  Arg: TGocciaValue;
  OptionsObj: TGocciaObjectValue;
  FracDigits, ExtraDays: Integer;
  Mode: TTemporalRoundingMode;
  CalDisp: TTemporalCalendarDisplay;
  DateRec: TTemporalDateRecord;
  RoundedEpochMs: Int64;
  TimeStr, S: string;
begin
  Zdt := AsZonedDateTime(AThisValue, 'ZonedDateTime.prototype.toString');
  ComputeLocalComponents(Zdt, LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs);

  OptionsObj := nil;
  Arg := AArgs.GetElement(0);
  if Assigned(Arg) and not (Arg is TGocciaUndefinedLiteralValue) then
  begin
    if not (Arg is TGocciaObjectValue) then
      ThrowTypeError('options must be an object or undefined', SSuggestTemporalFromArg);
    OptionsObj := TGocciaObjectValue(Arg);
  end;
  ResolveTemporalToStringOptions(OptionsObj, FracDigits, Mode);
  CalDisp := GetCalendarDisplay(OptionsObj);
  SavedHour := LHour;
  SavedMinute := LMinute;
  SavedSecond := LSecond;
  SavedMs := LMs;
  ExtraDays := 0;
  RoundTimeForToString(LHour, LMinute, LSecond, LMs, LUs, LNs, ExtraDays, FracDigits, Mode);
  if ExtraDays <> 0 then
    DateRec := AddDaysToDate(LYear, LMonth, LDay, ExtraDays)
  else
  begin
    DateRec.Year := LYear;
    DateRec.Month := LMonth;
    DateRec.Day := LDay;
  end;

  // Recompute UTC offset only when rounding actually changed the local time.
  // Unconditional recomputation via LocalToEpochMs would lose fold information
  // for ambiguous wall-clock times during DST fall-back (first-match disambiguation).
  if (ExtraDays <> 0) or (LHour <> SavedHour) or (LMinute <> SavedMinute) or
     (LSecond <> SavedSecond) or (LMs <> SavedMs) then
  begin
    RoundedEpochMs := LocalToEpochMs(DateRec.Year, DateRec.Month, DateRec.Day,
      LHour, LMinute, LSecond, LMs, Zdt.FTimeZone);
    OffsetSeconds := GetUtcOffsetSeconds(Zdt.FTimeZone, RoundedEpochMs div MILLISECONDS_PER_SECOND);
  end
  else
    OffsetSeconds := GetUtcOffsetSeconds(Zdt.FTimeZone, Zdt.FEpochMilliseconds div MILLISECONDS_PER_SECOND);

  if FracDigits = -2 then // smallestUnit: minute
    TimeStr := PadTwo(LHour) + ':' + PadTwo(LMinute)
  else
    TimeStr := FormatTimeWithPrecision(LHour, LMinute, LSecond, LMs, LUs, LNs, FracDigits);

  S := FormatDateString(DateRec.Year, DateRec.Month, DateRec.Day) + 'T' +
       TimeStr + FormatOffsetString(OffsetSeconds) +
       '[' + Zdt.FTimeZone + ']' + FormatCalendarAnnotation(CalDisp);

  Result := TGocciaStringLiteralValue.Create(S);
end;

// TC39 Temporal §6.3.38 Temporal.ZonedDateTime.prototype.toJSON()
function TGocciaTemporalZonedDateTimeValue.ZonedDateTimeToJSON(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Zdt: TGocciaTemporalZonedDateTimeValue;
  LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs: Integer;
  OffsetSeconds: Integer;
begin
  Zdt := AsZonedDateTime(AThisValue, 'ZonedDateTime.prototype.toJSON');
  ComputeLocalComponents(Zdt, LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs);
  OffsetSeconds := GetUtcOffsetSeconds(Zdt.FTimeZone, Zdt.FEpochMilliseconds div MILLISECONDS_PER_SECOND);
  Result := TGocciaStringLiteralValue.Create(
    FormatDateString(LYear, LMonth, LDay) + 'T' +
    FormatTimeString(LHour, LMinute, LSecond, LMs, LUs, LNs) +
    FormatOffsetString(OffsetSeconds) +
    '[' + Zdt.FTimeZone + ']');
end;

// TC39 Temporal §6.3.40 Temporal.ZonedDateTime.prototype.valueOf()
function TGocciaTemporalZonedDateTimeValue.ZonedDateTimeValueOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  ThrowTypeError(Format(SErrorTemporalValueOf, ['ZonedDateTime', 'epochMilliseconds, epochNanoseconds, or compare']), SSuggestTemporalNoValueOf);
  Result := nil;
end;

function TGocciaTemporalZonedDateTimeValue.ZonedDateTimeToLocaleString(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  EmptyArgs: TGocciaArgumentsCollection;
begin
  EmptyArgs := TGocciaArgumentsCollection.Create([]);
  try
    Result := ZonedDateTimeToString(EmptyArgs, AThisValue);
  finally
    EmptyArgs.Free;
  end;
end;

initialization
  GTemporalZonedDateTimeSharedSlot := RegisterRealmOwnedSlot('Temporal.ZonedDateTime.shared');

end.
