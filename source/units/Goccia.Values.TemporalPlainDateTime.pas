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
  published
    function GetCalendarId(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
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
      AMillisecond, AMicrosecond, ANanosecond: Integer); overload;

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
  end;

implementation

uses
  SysUtils,

  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.Realm,
  Goccia.Temporal.Options,
  Goccia.Temporal.TimeZone,
  Goccia.Temporal.Utils,
  Goccia.Values.ErrorHelper,
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

function AsPlainDateTime(const AValue: TGocciaValue; const AMethod: string): TGocciaTemporalPlainDateTimeValue;
begin
  if not (AValue is TGocciaTemporalPlainDateTimeValue) then
    ThrowTypeError(AMethod + ' called on non-PlainDateTime', SSuggestTemporalThisType);
  Result := TGocciaTemporalPlainDateTimeValue(AValue);
end;

function CoercePlainDateTime(const AValue: TGocciaValue; const AMethod: string): TGocciaTemporalPlainDateTimeValue;
var
  DateRec: TTemporalDateRecord;
  TimeRec: TTemporalTimeRecord;
  Obj: TGocciaObjectValue;
  V: TGocciaValue;
  VYear, VMonth, VDay, VHour, VMinute, VSecond: Integer;

  function GetRequiredField(const AName: string): Integer;
  begin
    V := Obj.GetProperty(AName);
    if (V = nil) or (V is TGocciaUndefinedLiteralValue) then
      ThrowTypeError(AMethod + ' requires ' + AName + ' property', SSuggestTemporalFromArg);
    Result := Trunc(V.ToNumberLiteral.Value);
  end;

  function GetOptionalField(const AName: string; const ADefault: Integer): Integer;
  begin
    V := Obj.GetProperty(AName);
    if (V = nil) or (V is TGocciaUndefinedLiteralValue) then
      Result := ADefault
    else
      Result := Trunc(V.ToNumberLiteral.Value);
  end;

begin
  if AValue is TGocciaTemporalPlainDateTimeValue then
    Result := TGocciaTemporalPlainDateTimeValue(AValue)
  else if AValue is TGocciaStringLiteralValue then
  begin
    if not CoerceToISODateTime(TGocciaStringLiteralValue(AValue).Value, DateRec, TimeRec) then
      ThrowRangeError(Format(SErrorTemporalInvalidISOStringFor, ['date-time', AMethod]), SSuggestTemporalISOFormat);
    Result := TGocciaTemporalPlainDateTimeValue.Create(
      DateRec.Year, DateRec.Month, DateRec.Day,
      TimeRec.Hour, TimeRec.Minute, TimeRec.Second,
      TimeRec.Millisecond, TimeRec.Microsecond, TimeRec.Nanosecond);
  end
  else if AValue is TGocciaObjectValue then
  begin
    Obj := TGocciaObjectValue(AValue);
    VYear := GetRequiredField('year');
    VMonth := GetRequiredField('month');
    VDay := GetRequiredField('day');
    VHour := GetOptionalField('hour', 0);
    VMinute := GetOptionalField('minute', 0);
    VSecond := GetOptionalField('second', 0);
    Result := TGocciaTemporalPlainDateTimeValue.Create(
      VYear, VMonth, VDay, VHour, VMinute, VSecond, 0, 0, 0);
  end
  else
  begin
    ThrowTypeError(AMethod + ' requires a PlainDateTime, string, or object', SSuggestTemporalFromArg);
    Result := nil;
  end;
end;

{ TGocciaTemporalPlainDateTimeValue }

constructor TGocciaTemporalPlainDateTimeValue.Create(const AYear, AMonth, ADay, AHour, AMinute, ASecond,
  AMillisecond, AMicrosecond, ANanosecond: Integer);
begin
  inherited Create(nil);
  if not IsValidDate(AYear, AMonth, ADay) then
    ThrowRangeError(SErrorInvalidDateInPlainDateTime, SSuggestTemporalDateRange);
  if not IsValidTime(AHour, AMinute, ASecond, AMillisecond, AMicrosecond, ANanosecond) then
    ThrowRangeError(SErrorInvalidTimeInPlainDateTime, SSuggestTemporalDateRange);
  FYear := AYear;
  FMonth := AMonth;
  FDay := ADay;
  FHour := AHour;
  FMinute := AMinute;
  FSecond := ASecond;
  FMillisecond := AMillisecond;
  FMicrosecond := AMicrosecond;
  FNanosecond := ANanosecond;
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
begin
  AsPlainDateTime(AThisValue, 'get PlainDateTime.calendarId');
  Result := TGocciaStringLiteralValue.Create('iso8601');
end;

function TGocciaTemporalPlainDateTimeValue.GetYear(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaNumberLiteralValue.Create(AsPlainDateTime(AThisValue, 'get PlainDateTime.year').FYear);
end;

function TGocciaTemporalPlainDateTimeValue.GetMonth(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaNumberLiteralValue.Create(AsPlainDateTime(AThisValue, 'get PlainDateTime.month').FMonth);
end;

function TGocciaTemporalPlainDateTimeValue.GetMonthCode(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaStringLiteralValue.Create('M' + PadTwo(AsPlainDateTime(AThisValue, 'get PlainDateTime.monthCode').FMonth));
end;

function TGocciaTemporalPlainDateTimeValue.GetDay(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaNumberLiteralValue.Create(AsPlainDateTime(AThisValue, 'get PlainDateTime.day').FDay);
end;

function TGocciaTemporalPlainDateTimeValue.GetDayOfWeek(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalPlainDateTimeValue;
begin
  D := AsPlainDateTime(AThisValue, 'get PlainDateTime.dayOfWeek');
  Result := TGocciaNumberLiteralValue.Create(Goccia.Temporal.Utils.DayOfWeek(D.FYear, D.FMonth, D.FDay));
end;

function TGocciaTemporalPlainDateTimeValue.GetDayOfYear(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalPlainDateTimeValue;
begin
  D := AsPlainDateTime(AThisValue, 'get PlainDateTime.dayOfYear');
  Result := TGocciaNumberLiteralValue.Create(Goccia.Temporal.Utils.DayOfYear(D.FYear, D.FMonth, D.FDay));
end;

function TGocciaTemporalPlainDateTimeValue.GetWeekOfYear(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalPlainDateTimeValue;
begin
  D := AsPlainDateTime(AThisValue, 'get PlainDateTime.weekOfYear');
  Result := TGocciaNumberLiteralValue.Create(Goccia.Temporal.Utils.WeekOfYear(D.FYear, D.FMonth, D.FDay));
end;

function TGocciaTemporalPlainDateTimeValue.GetYearOfWeek(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalPlainDateTimeValue;
begin
  D := AsPlainDateTime(AThisValue, 'get PlainDateTime.yearOfWeek');
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
begin
  D := AsPlainDateTime(AThisValue, 'get PlainDateTime.daysInMonth');
  Result := TGocciaNumberLiteralValue.Create(Goccia.Temporal.Utils.DaysInMonth(D.FYear, D.FMonth));
end;

function TGocciaTemporalPlainDateTimeValue.GetDaysInYear(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalPlainDateTimeValue;
begin
  D := AsPlainDateTime(AThisValue, 'get PlainDateTime.daysInYear');
  Result := TGocciaNumberLiteralValue.Create(Goccia.Temporal.Utils.DaysInYear(D.FYear));
end;

function TGocciaTemporalPlainDateTimeValue.GetMonthsInYear(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  AsPlainDateTime(AThisValue, 'get PlainDateTime.monthsInYear');
  Result := TGocciaNumberLiteralValue.Create(12);
end;

function TGocciaTemporalPlainDateTimeValue.GetInLeapYear(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  if Goccia.Temporal.Utils.IsLeapYear(AsPlainDateTime(AThisValue, 'get PlainDateTime.inLeapYear').FYear) then
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
  V: TGocciaValue;

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
  D := AsPlainDateTime(AThisValue, 'PlainDateTime.prototype.with');
  V := AArgs.GetElement(0);
  if not (V is TGocciaObjectValue) then
    ThrowTypeError(Format(SErrorTemporalWithRequiresObject, ['PlainDateTime']), SSuggestTemporalWithObject);
  Obj := TGocciaObjectValue(V);

  Result := TGocciaTemporalPlainDateTimeValue.Create(
    GetFieldOr('year', D.FYear), GetFieldOr('month', D.FMonth), GetFieldOr('day', D.FDay),
    GetFieldOr('hour', D.FHour), GetFieldOr('minute', D.FMinute), GetFieldOr('second', D.FSecond),
    GetFieldOr('millisecond', D.FMillisecond), GetFieldOr('microsecond', D.FMicrosecond),
    GetFieldOr('nanosecond', D.FNanosecond));
end;

function TGocciaTemporalPlainDateTimeValue.DateTimeWithPlainTime(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalPlainDateTimeValue;
  Arg: TGocciaValue;
  H, Mi, S, Ms, Us, Ns: Integer;
  T: TGocciaTemporalPlainTimeValue;
  TimeRec: TTemporalTimeRecord;
begin
  D := AsPlainDateTime(AThisValue, 'PlainDateTime.prototype.withPlainTime');
  Arg := AArgs.GetElement(0);
  H := 0; Mi := 0; S := 0; Ms := 0; Us := 0; Ns := 0;

  if Assigned(Arg) and not (Arg is TGocciaUndefinedLiteralValue) then
  begin
    if Arg is TGocciaTemporalPlainTimeValue then
    begin
      T := TGocciaTemporalPlainTimeValue(Arg);
      H := T.Hour; Mi := T.Minute; S := T.Second;
      Ms := T.Millisecond; Us := T.Microsecond; Ns := T.Nanosecond;
    end
    else if Arg is TGocciaStringLiteralValue then
    begin
      if not CoerceToISOTime(TGocciaStringLiteralValue(Arg).Value, TimeRec) then
        ThrowRangeError(SErrorInvalidTimeString, SSuggestTemporalISOFormat);
      H := TimeRec.Hour; Mi := TimeRec.Minute; S := TimeRec.Second;
      Ms := TimeRec.Millisecond; Us := TimeRec.Microsecond; Ns := TimeRec.Nanosecond;
    end
    else
      ThrowTypeError(SErrorPlainDateTimeWithPlainTimeArg, SSuggestTemporalFromArg);
  end;

  Result := TGocciaTemporalPlainDateTimeValue.Create(D.FYear, D.FMonth, D.FDay, H, Mi, S, Ms, Us, Ns);
end;

function TGocciaTemporalPlainDateTimeValue.DateTimeAdd(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalPlainDateTimeValue;
  Dur: TGocciaTemporalDurationValue;
  Arg: TGocciaValue;
  DurRec: TTemporalDurationRecord;
  NewYear, NewMonth, NewDay: Integer;
  DateRec: TTemporalDateRecord;
  ExtraDays: Int64;
  Balanced: TTemporalTimeRecord;
  ObjArg: TGocciaObjectValue;
  VF: TGocciaValue;
  Y, Mo, W, Da, H, Mi, S, Ms, Us, Ns: Int64;
begin
  D := AsPlainDateTime(AThisValue, 'PlainDateTime.prototype.add');
  Arg := AArgs.GetElement(0);

  if Arg is TGocciaTemporalDurationValue then
    Dur := TGocciaTemporalDurationValue(Arg)
  else if Arg is TGocciaStringLiteralValue then
  begin
    if not TryParseISODuration(TGocciaStringLiteralValue(Arg).Value, DurRec) then
      ThrowRangeError(SErrorInvalidDurationString, SSuggestTemporalDurationArg);
    Dur := TGocciaTemporalDurationValue.Create(
      DurRec.Years, DurRec.Months, DurRec.Weeks, DurRec.Days,
      DurRec.Hours, DurRec.Minutes, DurRec.Seconds,
      DurRec.Milliseconds, DurRec.Microseconds, DurRec.Nanoseconds);
  end
  else if Arg is TGocciaObjectValue then
  begin
    ObjArg := TGocciaObjectValue(Arg);
    Y := 0; Mo := 0; W := 0; Da := 0; H := 0; Mi := 0; S := 0; Ms := 0; Us := 0; Ns := 0;
    VF := ObjArg.GetProperty('years'); if Assigned(VF) and not (VF is TGocciaUndefinedLiteralValue) then Y := Trunc(VF.ToNumberLiteral.Value);
    VF := ObjArg.GetProperty('months'); if Assigned(VF) and not (VF is TGocciaUndefinedLiteralValue) then Mo := Trunc(VF.ToNumberLiteral.Value);
    VF := ObjArg.GetProperty('weeks'); if Assigned(VF) and not (VF is TGocciaUndefinedLiteralValue) then W := Trunc(VF.ToNumberLiteral.Value);
    VF := ObjArg.GetProperty('days'); if Assigned(VF) and not (VF is TGocciaUndefinedLiteralValue) then Da := Trunc(VF.ToNumberLiteral.Value);
    VF := ObjArg.GetProperty('hours'); if Assigned(VF) and not (VF is TGocciaUndefinedLiteralValue) then H := Trunc(VF.ToNumberLiteral.Value);
    VF := ObjArg.GetProperty('minutes'); if Assigned(VF) and not (VF is TGocciaUndefinedLiteralValue) then Mi := Trunc(VF.ToNumberLiteral.Value);
    VF := ObjArg.GetProperty('seconds'); if Assigned(VF) and not (VF is TGocciaUndefinedLiteralValue) then S := Trunc(VF.ToNumberLiteral.Value);
    VF := ObjArg.GetProperty('milliseconds'); if Assigned(VF) and not (VF is TGocciaUndefinedLiteralValue) then Ms := Trunc(VF.ToNumberLiteral.Value);
    VF := ObjArg.GetProperty('microseconds'); if Assigned(VF) and not (VF is TGocciaUndefinedLiteralValue) then Us := Trunc(VF.ToNumberLiteral.Value);
    VF := ObjArg.GetProperty('nanoseconds'); if Assigned(VF) and not (VF is TGocciaUndefinedLiteralValue) then Ns := Trunc(VF.ToNumberLiteral.Value);
    Dur := TGocciaTemporalDurationValue.Create(Y, Mo, W, Da, H, Mi, S, Ms, Us, Ns);
  end
  else
  begin
    ThrowTypeError(Format(SErrorTemporalAddRequiresDuration, ['PlainDateTime']), SSuggestTemporalDurationArg);
    Dur := nil;
  end;

  // Balance time first
  Balanced := BalanceTime(
    D.FHour + Dur.Hours, D.FMinute + Dur.Minutes, D.FSecond + Dur.Seconds,
    D.FMillisecond + Dur.Milliseconds, D.FMicrosecond + Dur.Microseconds,
    D.FNanosecond + Dur.Nanoseconds, ExtraDays);

  // Add date components
  NewYear := D.FYear + Integer(Dur.Years);
  NewMonth := D.FMonth + Integer(Dur.Months);
  while NewMonth > 12 do begin Inc(NewYear); Dec(NewMonth, 12); end;
  while NewMonth < 1 do begin Dec(NewYear); Inc(NewMonth, 12); end;
  NewDay := D.FDay;
  if NewDay > Goccia.Temporal.Utils.DaysInMonth(NewYear, NewMonth) then
    NewDay := Goccia.Temporal.Utils.DaysInMonth(NewYear, NewMonth);

  DateRec := AddDaysToDate(NewYear, NewMonth, NewDay, Dur.Weeks * 7 + Dur.Days + ExtraDays);

  Result := TGocciaTemporalPlainDateTimeValue.Create(
    DateRec.Year, DateRec.Month, DateRec.Day,
    Balanced.Hour, Balanced.Minute, Balanced.Second,
    Balanced.Millisecond, Balanced.Microsecond, Balanced.Nanosecond);
end;

function TGocciaTemporalPlainDateTimeValue.DateTimeSubtract(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Dur: TGocciaTemporalDurationValue;
  NegDur: TGocciaTemporalDurationValue;
  Arg: TGocciaValue;
  DurRec: TTemporalDurationRecord;
  NewArgs: TGocciaArgumentsCollection;
  ObjArg: TGocciaObjectValue;
  VF: TGocciaValue;
  Y, Mo, W, Da, H, Mi, S, Ms, Us, Ns: Int64;
begin
  Arg := AArgs.GetElement(0);

  if Arg is TGocciaTemporalDurationValue then
    Dur := TGocciaTemporalDurationValue(Arg)
  else if Arg is TGocciaStringLiteralValue then
  begin
    if not TryParseISODuration(TGocciaStringLiteralValue(Arg).Value, DurRec) then
      ThrowRangeError(SErrorInvalidDurationString, SSuggestTemporalDurationArg);
    Dur := TGocciaTemporalDurationValue.Create(
      DurRec.Years, DurRec.Months, DurRec.Weeks, DurRec.Days,
      DurRec.Hours, DurRec.Minutes, DurRec.Seconds,
      DurRec.Milliseconds, DurRec.Microseconds, DurRec.Nanoseconds);
  end
  else if Arg is TGocciaObjectValue then
  begin
    ObjArg := TGocciaObjectValue(Arg);
    Y := 0; Mo := 0; W := 0; Da := 0; H := 0; Mi := 0; S := 0; Ms := 0; Us := 0; Ns := 0;
    VF := ObjArg.GetProperty('years'); if Assigned(VF) and not (VF is TGocciaUndefinedLiteralValue) then Y := Trunc(VF.ToNumberLiteral.Value);
    VF := ObjArg.GetProperty('months'); if Assigned(VF) and not (VF is TGocciaUndefinedLiteralValue) then Mo := Trunc(VF.ToNumberLiteral.Value);
    VF := ObjArg.GetProperty('weeks'); if Assigned(VF) and not (VF is TGocciaUndefinedLiteralValue) then W := Trunc(VF.ToNumberLiteral.Value);
    VF := ObjArg.GetProperty('days'); if Assigned(VF) and not (VF is TGocciaUndefinedLiteralValue) then Da := Trunc(VF.ToNumberLiteral.Value);
    VF := ObjArg.GetProperty('hours'); if Assigned(VF) and not (VF is TGocciaUndefinedLiteralValue) then H := Trunc(VF.ToNumberLiteral.Value);
    VF := ObjArg.GetProperty('minutes'); if Assigned(VF) and not (VF is TGocciaUndefinedLiteralValue) then Mi := Trunc(VF.ToNumberLiteral.Value);
    VF := ObjArg.GetProperty('seconds'); if Assigned(VF) and not (VF is TGocciaUndefinedLiteralValue) then S := Trunc(VF.ToNumberLiteral.Value);
    VF := ObjArg.GetProperty('milliseconds'); if Assigned(VF) and not (VF is TGocciaUndefinedLiteralValue) then Ms := Trunc(VF.ToNumberLiteral.Value);
    VF := ObjArg.GetProperty('microseconds'); if Assigned(VF) and not (VF is TGocciaUndefinedLiteralValue) then Us := Trunc(VF.ToNumberLiteral.Value);
    VF := ObjArg.GetProperty('nanoseconds'); if Assigned(VF) and not (VF is TGocciaUndefinedLiteralValue) then Ns := Trunc(VF.ToNumberLiteral.Value);
    Dur := TGocciaTemporalDurationValue.Create(Y, Mo, W, Da, H, Mi, S, Ms, Us, Ns);
  end
  else
  begin
    ThrowTypeError(Format(SErrorTemporalSubtractRequiresDuration, ['PlainDateTime']), SSuggestTemporalDurationArg);
    Dur := nil;
  end;

  NegDur := TGocciaTemporalDurationValue.Create(
    -Dur.Years, -Dur.Months, -Dur.Weeks, -Dur.Days,
    -Dur.Hours, -Dur.Minutes, -Dur.Seconds,
    -Dur.Milliseconds, -Dur.Microseconds, -Dur.Nanoseconds);

  NewArgs := TGocciaArgumentsCollection.Create([NegDur]);
  try
    Result := DateTimeAdd(NewArgs, AThisValue);
  finally
    NewArgs.Free;
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
  LargestUnit := GetLargestUnit(OptionsObj, tuDay);
  if LargestUnit = tuAuto then LargestUnit := tuDay;

  SmallestUnit := GetSmallestUnit(OptionsObj, tuNanosecond);
  if Ord(LargestUnit) > Ord(SmallestUnit) then
    ThrowRangeError(SErrorDurationRoundLargestSmallerThanSmallest, SSuggestTemporalRoundArg);
  RMode := GetRoundingMode(OptionsObj, rmTrunc);
  RIncrement := GetRoundingIncrement(OptionsObj, 1);

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

    CalendarDateUntil(D.FYear, D.FMonth, D.FDay,
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
          RH := Sgn * TotalInUnit;
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
          RM := Sgn * TotalInUnit;
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
          RS := Sgn * TotalInUnit;
          RMs := Sgn * (RemNs div 1000000);
          RUs := Sgn * ((RemNs div 1000) mod 1000);
          RNs := Sgn * (RemNs mod 1000);
        end;
      tuMillisecond:
        begin
          TotalInUnit := AbsDays * 86400000 + AbsTimeNs div 1000000;
          RemNs := AbsTimeNs mod 1000000;
          RH := 0; RM := 0; RS := 0;
          RMs := Sgn * TotalInUnit;
          RUs := Sgn * (RemNs div 1000);
          RNs := Sgn * (RemNs mod 1000);
        end;
      tuMicrosecond:
        begin
          TotalInUnit := AbsDays * Int64(86400000000) + AbsTimeNs div 1000;
          RH := 0; RM := 0; RS := 0; RMs := 0;
          RUs := Sgn * TotalInUnit;
          RNs := Sgn * (AbsTimeNs mod 1000);
        end;
    else // tuNanosecond
      begin
        TotalInUnit := AbsDays * Int64(86400000000000) + AbsTimeNs;
        RH := 0; RM := 0; RS := 0; RMs := 0; RUs := 0;
        RNs := Sgn * TotalInUnit;
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
begin
  D := AsPlainDateTime(AThisValue, 'PlainDateTime.prototype.since');
  Other := CoercePlainDateTime(AArgs.GetElement(0), 'PlainDateTime.prototype.since');

  // since(other, options) = other.until(this, options)
  OptionsArg := AArgs.GetElement(1);
  if (OptionsArg <> nil) and not (OptionsArg is TGocciaUndefinedLiteralValue) then
    NewArgs := TGocciaArgumentsCollection.Create([D, OptionsArg])
  else
    NewArgs := TGocciaArgumentsCollection.Create([D]);
  try
    Result := DateTimeUntil(NewArgs, Other);
  finally
    NewArgs.Free;
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
    SmallestUnit := GetSmallestUnit(OptionsObj, tuNone);
    if SmallestUnit = tuNone then
      ThrowRangeError(SErrorTemporalRoundRequiresSmallestUnit, SSuggestTemporalRoundArg);
    Mode := GetRoundingMode(OptionsObj, rmHalfExpand);
    Increment := GetRoundingIncrement(OptionsObj, 1);
  end
  else
  begin
    ThrowTypeError(Format(SErrorTemporalRoundRequiresStringOrOptions, ['PlainDateTime']), SSuggestTemporalRoundArg);
    SmallestUnit := tuNanosecond;
  end;

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
    Result := TGocciaTemporalPlainDateTimeValue.Create(DateRec.Year, DateRec.Month, DateRec.Day, 0, 0, 0, 0, 0, 0);
    Exit;
  end;

  Divisor := UnitToNanoseconds(SmallestUnit) * Increment;
  Rounded := RoundWithMode(TotalNs, Divisor, Mode);
  Balanced := BalanceTime(0, 0, 0, 0, 0, Rounded, ExtraDays);
  DateRec := AddDaysToDate(D.FYear, D.FMonth, D.FDay, ExtraDays);

  Result := TGocciaTemporalPlainDateTimeValue.Create(
    DateRec.Year, DateRec.Month, DateRec.Day,
    Balanced.Hour, Balanced.Minute, Balanced.Second,
    Balanced.Millisecond, Balanced.Microsecond, Balanced.Nanosecond);
end;

function TGocciaTemporalPlainDateTimeValue.DateTimeEquals(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D, Other: TGocciaTemporalPlainDateTimeValue;
begin
  D := AsPlainDateTime(AThisValue, 'PlainDateTime.prototype.equals');
  Other := CoercePlainDateTime(AArgs.GetElement(0), 'PlainDateTime.prototype.equals');

  if (CompareDates(D.FYear, D.FMonth, D.FDay, Other.FYear, Other.FMonth, Other.FDay) = 0) and
     (CompareTimes(D.FHour, D.FMinute, D.FSecond, D.FMillisecond, D.FMicrosecond, D.FNanosecond,
                   Other.FHour, Other.FMinute, Other.FSecond, Other.FMillisecond, Other.FMicrosecond, Other.FNanosecond) = 0) then
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
  ResolveTemporalToStringOptions(OptionsObj, FracDigits, Mode);
  CalDisp := GetCalendarDisplay(OptionsObj);
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
  if FracDigits = -2 then // smallestUnit: minute
    TimeStr := PadTwo(H) + ':' + PadTwo(Mi)
  else
    TimeStr := FormatTimeWithPrecision(H, Mi, S, Ms, Us, Ns, FracDigits);
  Result := TGocciaStringLiteralValue.Create(
    FormatDateString(DateRec.Year, DateRec.Month, DateRec.Day) + 'T' +
    TimeStr + FormatCalendarAnnotation(CalDisp));
end;

function TGocciaTemporalPlainDateTimeValue.DateTimeToJSON(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalPlainDateTimeValue;
begin
  D := AsPlainDateTime(AThisValue, 'PlainDateTime.prototype.toJSON');
  Result := TGocciaStringLiteralValue.Create(
    FormatDateString(D.FYear, D.FMonth, D.FDay) + 'T' +
    FormatTimeString(D.FHour, D.FMinute, D.FSecond, D.FMillisecond, D.FMicrosecond, D.FNanosecond));
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
  Result := TGocciaTemporalPlainDateValue.Create(D.FYear, D.FMonth, D.FDay);
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
  Result := TGocciaTemporalPlainYearMonthValue.Create(D.FYear, D.FMonth, D.FDay);
end;

function TGocciaTemporalPlainDateTimeValue.DateTimeToPlainMonthDay(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalPlainDateTimeValue;
begin
  D := AsPlainDateTime(AThisValue, 'PlainDateTime.prototype.toPlainMonthDay');
  Result := TGocciaTemporalPlainMonthDayValue.Create(D.FMonth, D.FDay, D.FYear);
end;

function TGocciaTemporalPlainDateTimeValue.DateTimeToZonedDateTime(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalPlainDateTimeValue;
  Arg: TGocciaValue;
  TimeZoneStr: string;
  EpochMs: Int64;
  OffsetSec: Integer;
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

  if not IsValidTimeZone(TimeZoneStr) then
    ThrowRangeError(Format(SErrorPlainDateTimeToZonedUnknownTZ, [TimeZoneStr]), SSuggestTemporalTimezone);

  // Compute epoch ms from local components
  EpochMs := DateToEpochDays(D.FYear, D.FMonth, D.FDay) * Int64(86400000) +
             Int64(D.FHour) * 3600000 + Int64(D.FMinute) * 60000 +
             Int64(D.FSecond) * 1000 + D.FMillisecond;
  // Adjust for timezone offset
  OffsetSec := GetUtcOffsetSeconds(TimeZoneStr, EpochMs div 1000);
  EpochMs := EpochMs - Int64(OffsetSec) * 1000;

  Result := TGocciaTemporalZonedDateTimeValue.Create(EpochMs,
    D.FMicrosecond * 1000 + D.FNanosecond, TimeZoneStr);
end;

function TGocciaTemporalPlainDateTimeValue.DateTimeToLocaleString(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  EmptyArgs: TGocciaArgumentsCollection;
begin
  EmptyArgs := TGocciaArgumentsCollection.Create([]);
  try
    Result := DateTimeToString(EmptyArgs, AThisValue);
  finally
    EmptyArgs.Free;
  end;
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
  CalId := Arg.ToStringLiteral.Value;
  if CalId <> 'iso8601' then
    ThrowRangeError('Unknown calendar: ' + CalId, SSuggestTemporalFromArg);
  Result := TGocciaTemporalPlainDateTimeValue.Create(D.FYear, D.FMonth, D.FDay,
    D.FHour, D.FMinute, D.FSecond, D.FMillisecond, D.FMicrosecond, D.FNanosecond);
end;

initialization
  GTemporalPlainDateTimeSharedSlot := RegisterRealmOwnedSlot('Temporal.PlainDateTime.shared');

end.
