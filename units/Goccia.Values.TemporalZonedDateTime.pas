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

  Goccia.Temporal.Options,
  Goccia.Temporal.TimeZone,
  Goccia.Temporal.Utils,
  Goccia.Values.ErrorHelper,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.TemporalDuration,
  Goccia.Values.TemporalInstant,
  Goccia.Values.TemporalPlainDate,
  Goccia.Values.TemporalPlainDateTime,
  Goccia.Values.TemporalPlainTime;

threadvar
  FShared: TGocciaSharedPrototype;
  FPrototypeMembers: TArray<TGocciaMemberDefinition>;

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
    ThrowTypeError(AMethod + ' called on non-ZonedDateTime');
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
      ThrowTypeError('Invalid ISO ZonedDateTime string for ' + AMethod);
    if TimeZoneStr = '' then
      ThrowTypeError(AMethod + ' requires a timezone annotation in the string');

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
    ThrowTypeError(AMethod + ' requires a ZonedDateTime or string');
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
      ThrowTypeError('Invalid duration string for ' + AMethod);
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
    ThrowTypeError(AMethod + ' requires a Duration, string, or object');
    Result := nil;
  end;
end;

{ TGocciaTemporalZonedDateTimeValue }

constructor TGocciaTemporalZonedDateTimeValue.Create(const AEpochMilliseconds: Int64;
  const ASubMillisecondNanoseconds: Integer; const ATimeZone: string);
begin
  inherited Create(nil);
  if ATimeZone = '' then
    ThrowRangeError('ZonedDateTime requires a timezone');
  if not IsValidTimeZone(ATimeZone) then
    ThrowRangeError('Unknown timezone: ' + ATimeZone);

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
  if Assigned(FShared) then
    FPrototype := FShared.Prototype;
end;

procedure TGocciaTemporalZonedDateTimeValue.InitializePrototype;
var
  Members: TGocciaMemberCollection;
begin
  if Assigned(FShared) then Exit;
  FShared := TGocciaSharedPrototype.Create(Self);
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
      FPrototypeMembers := Members.ToDefinitions;
    finally
      Members.Free;
    end;
  end;
  RegisterMemberDefinitions(FShared.Prototype, FPrototypeMembers);
end;

class procedure TGocciaTemporalZonedDateTimeValue.ExposePrototype(const AConstructor: TGocciaObjectValue);
begin
  if not Assigned(FShared) then
    TGocciaTemporalZonedDateTimeValue.Create(0, 0, 'UTC');
  ExposeSharedPrototypeOnConstructor(FShared, AConstructor);
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
  MsFloat: Double;
begin
  Zdt := AsZonedDateTime(AThisValue, 'get ZonedDateTime.epochNanoseconds');
  // Safe Int64 → Double conversion (FPC 3.2.2 AArch64 bug: Int64 * Double gives wrong results)
  MsFloat := Zdt.FEpochMilliseconds;
  Result := TGocciaNumberLiteralValue.Create(MsFloat * 1000000.0 + Zdt.FSubMillisecondNanoseconds);
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
    ThrowTypeError('ZonedDateTime.prototype.with requires an object argument');
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
    if not TryParseISODate(TGocciaStringLiteralValue(Arg).Value, DateRec) then
      ThrowTypeError('Invalid date string for ZonedDateTime.prototype.withPlainDate');
    PlainDate := TGocciaTemporalPlainDateValue.Create(DateRec.Year, DateRec.Month, DateRec.Day);
  end
  else
  begin
    ThrowTypeError('ZonedDateTime.prototype.withPlainDate requires a PlainDate or string');
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
      if not TryParseISOTime(TGocciaStringLiteralValue(Arg).Value, TimeRec) then
        ThrowTypeError('Invalid time string for ZonedDateTime.prototype.withPlainTime');
      NewHour := TimeRec.Hour; NewMinute := TimeRec.Minute; NewSecond := TimeRec.Second;
      NewMs := TimeRec.Millisecond; NewUs := TimeRec.Microsecond; NewNs := TimeRec.Nanosecond;
    end
    else
      ThrowTypeError('ZonedDateTime.prototype.withPlainTime requires a PlainTime or string');
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
    ThrowTypeError('ZonedDateTime.prototype.withTimeZone requires a timezone argument');

  NewTimeZone := Arg.ToStringLiteral.Value;
  if NewTimeZone = '' then
    ThrowTypeError('ZonedDateTime.prototype.withTimeZone requires a non-empty timezone');

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

// TC39 Temporal §6.3.28 Temporal.ZonedDateTime.prototype.until(other [, options])
function TGocciaTemporalZonedDateTimeValue.ZonedDateTimeUntil(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Zdt, Other: TGocciaTemporalZonedDateTimeValue;
  DiffMs: Int64;
  DiffSubMs: Integer;
  DiffNs: Int64;
begin
  Zdt := AsZonedDateTime(AThisValue, 'ZonedDateTime.prototype.until');
  Other := CoerceZonedDateTime(AArgs.GetElement(0), 'ZonedDateTime.prototype.until');

  DiffMs := Other.FEpochMilliseconds - Zdt.FEpochMilliseconds;
  DiffSubMs := Other.FSubMillisecondNanoseconds - Zdt.FSubMillisecondNanoseconds;

  // Convert to total nanoseconds
  DiffNs := DiffMs * SUB_MS_NANOSECOND_LIMIT + DiffSubMs;

  Result := TGocciaTemporalDurationValue.Create(0, 0, 0, 0,
    DiffNs div NANOSECONDS_PER_HOUR,
    (DiffNs div NANOSECONDS_PER_MINUTE) mod 60,
    (DiffNs div NANOSECONDS_PER_SECOND) mod 60,
    (DiffNs div NANOSECONDS_PER_MILLISECOND) mod 1000,
    (DiffNs div NANOSECONDS_PER_MICROSECOND) mod 1000,
    DiffNs mod 1000);
end;

// TC39 Temporal §6.3.29 Temporal.ZonedDateTime.prototype.since(other [, options])
function TGocciaTemporalZonedDateTimeValue.ZonedDateTimeSince(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Zdt, Other: TGocciaTemporalZonedDateTimeValue;
  DiffMs: Int64;
  DiffSubMs: Integer;
  DiffNs: Int64;
begin
  Zdt := AsZonedDateTime(AThisValue, 'ZonedDateTime.prototype.since');
  Other := CoerceZonedDateTime(AArgs.GetElement(0), 'ZonedDateTime.prototype.since');

  DiffMs := Zdt.FEpochMilliseconds - Other.FEpochMilliseconds;
  DiffSubMs := Zdt.FSubMillisecondNanoseconds - Other.FSubMillisecondNanoseconds;

  DiffNs := DiffMs * SUB_MS_NANOSECOND_LIMIT + DiffSubMs;

  Result := TGocciaTemporalDurationValue.Create(0, 0, 0, 0,
    DiffNs div NANOSECONDS_PER_HOUR,
    (DiffNs div NANOSECONDS_PER_MINUTE) mod 60,
    (DiffNs div NANOSECONDS_PER_SECOND) mod 60,
    (DiffNs div NANOSECONDS_PER_MILLISECOND) mod 1000,
    (DiffNs div NANOSECONDS_PER_MICROSECOND) mod 1000,
    DiffNs mod 1000);
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
      ThrowRangeError('round() requires a smallestUnit option');
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
      ThrowRangeError('Invalid unit for ZonedDateTime.prototype.round');
      UnitStr := '';
    end;
  end
  else
  begin
    ThrowTypeError('ZonedDateTime.prototype.round requires a string or options object');
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
    ThrowRangeError('Invalid unit for ZonedDateTime.prototype.round: ' + UnitStr);
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
  OffsetSeconds: Integer;
  S: string;
begin
  Zdt := AsZonedDateTime(AThisValue, 'ZonedDateTime.prototype.toString');
  ComputeLocalComponents(Zdt, LYear, LMonth, LDay, LHour, LMinute, LSecond, LMs, LUs, LNs);
  OffsetSeconds := GetUtcOffsetSeconds(Zdt.FTimeZone, Zdt.FEpochMilliseconds div MILLISECONDS_PER_SECOND);

  S := FormatDateString(LYear, LMonth, LDay) + 'T' +
       FormatTimeString(LHour, LMinute, LSecond, LMs, LUs, LNs) +
       FormatOffsetString(OffsetSeconds) +
       '[' + Zdt.FTimeZone + ']';

  Result := TGocciaStringLiteralValue.Create(S);
end;

// TC39 Temporal §6.3.38 Temporal.ZonedDateTime.prototype.toJSON()
function TGocciaTemporalZonedDateTimeValue.ZonedDateTimeToJSON(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := ZonedDateTimeToString(AArgs, AThisValue);
end;

// TC39 Temporal §6.3.40 Temporal.ZonedDateTime.prototype.valueOf()
function TGocciaTemporalZonedDateTimeValue.ZonedDateTimeValueOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  ThrowTypeError('Temporal.ZonedDateTime.prototype.valueOf cannot be used; use epochMilliseconds, epochNanoseconds, or compare instead');
  Result := nil;
end;

end.
