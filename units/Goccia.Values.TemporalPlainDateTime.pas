unit Goccia.Values.TemporalPlainDateTime;

{$I Goccia.inc}

interface

uses
  SysUtils,

  Goccia.Arguments.Collection,
  Goccia.SharedPrototype,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaTemporalPlainDateTimeValue = class(TGocciaObjectValue)
  private
    class var FShared: TGocciaSharedPrototype;
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
  Goccia.GarbageCollector,
  Goccia.Temporal.Utils,
  Goccia.Values.ErrorHelper,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.TemporalDuration,
  Goccia.Values.TemporalPlainDate,
  Goccia.Values.TemporalPlainTime;

function AsPlainDateTime(const AValue: TGocciaValue; const AMethod: string): TGocciaTemporalPlainDateTimeValue;
begin
  if not (AValue is TGocciaTemporalPlainDateTimeValue) then
    ThrowTypeError(AMethod + ' called on non-PlainDateTime');
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
      ThrowTypeError(AMethod + ' requires ' + AName + ' property');
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
    if not TryParseISODateTime(TGocciaStringLiteralValue(AValue).Value, DateRec, TimeRec) then
      ThrowTypeError('Invalid ISO date-time string for ' + AMethod);
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
    ThrowTypeError(AMethod + ' requires a PlainDateTime, string, or object');
    Result := nil;
  end;
end;

{ TGocciaTemporalPlainDateTimeValue }

constructor TGocciaTemporalPlainDateTimeValue.Create(const AYear, AMonth, ADay, AHour, AMinute, ASecond,
  AMillisecond, AMicrosecond, ANanosecond: Integer);
begin
  inherited Create(nil);
  if not IsValidDate(AYear, AMonth, ADay) then
    ThrowRangeError('Invalid date in PlainDateTime');
  if not IsValidTime(AHour, AMinute, ASecond, AMillisecond, AMicrosecond, ANanosecond) then
    ThrowRangeError('Invalid time in PlainDateTime');
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
  if Assigned(FShared) then
    FPrototype := FShared.Prototype;
end;

procedure TGocciaTemporalPlainDateTimeValue.InitializePrototype;
begin
  if Assigned(FShared) then Exit;
  FShared := TGocciaSharedPrototype.Create(Self);

  // Date getters
  FShared.Prototype.DefineProperty('calendarId', TGocciaPropertyDescriptorAccessor.Create(
    TGocciaNativeFunctionValue.CreateWithoutPrototype(GetCalendarId, 'calendarId', 0), nil, [pfConfigurable]));
  FShared.Prototype.DefineProperty('year', TGocciaPropertyDescriptorAccessor.Create(
    TGocciaNativeFunctionValue.CreateWithoutPrototype(GetYear, 'year', 0), nil, [pfConfigurable]));
  FShared.Prototype.DefineProperty('month', TGocciaPropertyDescriptorAccessor.Create(
    TGocciaNativeFunctionValue.CreateWithoutPrototype(GetMonth, 'month', 0), nil, [pfConfigurable]));
  FShared.Prototype.DefineProperty('monthCode', TGocciaPropertyDescriptorAccessor.Create(
    TGocciaNativeFunctionValue.CreateWithoutPrototype(GetMonthCode, 'monthCode', 0), nil, [pfConfigurable]));
  FShared.Prototype.DefineProperty('day', TGocciaPropertyDescriptorAccessor.Create(
    TGocciaNativeFunctionValue.CreateWithoutPrototype(GetDay, 'day', 0), nil, [pfConfigurable]));
  FShared.Prototype.DefineProperty('dayOfWeek', TGocciaPropertyDescriptorAccessor.Create(
    TGocciaNativeFunctionValue.CreateWithoutPrototype(GetDayOfWeek, 'dayOfWeek', 0), nil, [pfConfigurable]));
  FShared.Prototype.DefineProperty('dayOfYear', TGocciaPropertyDescriptorAccessor.Create(
    TGocciaNativeFunctionValue.CreateWithoutPrototype(GetDayOfYear, 'dayOfYear', 0), nil, [pfConfigurable]));
  FShared.Prototype.DefineProperty('weekOfYear', TGocciaPropertyDescriptorAccessor.Create(
    TGocciaNativeFunctionValue.CreateWithoutPrototype(GetWeekOfYear, 'weekOfYear', 0), nil, [pfConfigurable]));
  FShared.Prototype.DefineProperty('yearOfWeek', TGocciaPropertyDescriptorAccessor.Create(
    TGocciaNativeFunctionValue.CreateWithoutPrototype(GetYearOfWeek, 'yearOfWeek', 0), nil, [pfConfigurable]));
  FShared.Prototype.DefineProperty('daysInWeek', TGocciaPropertyDescriptorAccessor.Create(
    TGocciaNativeFunctionValue.CreateWithoutPrototype(GetDaysInWeek, 'daysInWeek', 0), nil, [pfConfigurable]));
  FShared.Prototype.DefineProperty('daysInMonth', TGocciaPropertyDescriptorAccessor.Create(
    TGocciaNativeFunctionValue.CreateWithoutPrototype(GetDaysInMonth, 'daysInMonth', 0), nil, [pfConfigurable]));
  FShared.Prototype.DefineProperty('daysInYear', TGocciaPropertyDescriptorAccessor.Create(
    TGocciaNativeFunctionValue.CreateWithoutPrototype(GetDaysInYear, 'daysInYear', 0), nil, [pfConfigurable]));
  FShared.Prototype.DefineProperty('monthsInYear', TGocciaPropertyDescriptorAccessor.Create(
    TGocciaNativeFunctionValue.CreateWithoutPrototype(GetMonthsInYear, 'monthsInYear', 0), nil, [pfConfigurable]));
  FShared.Prototype.DefineProperty('inLeapYear', TGocciaPropertyDescriptorAccessor.Create(
    TGocciaNativeFunctionValue.CreateWithoutPrototype(GetInLeapYear, 'inLeapYear', 0), nil, [pfConfigurable]));
  // Time getters
  FShared.Prototype.DefineProperty('hour', TGocciaPropertyDescriptorAccessor.Create(
    TGocciaNativeFunctionValue.CreateWithoutPrototype(GetHour, 'hour', 0), nil, [pfConfigurable]));
  FShared.Prototype.DefineProperty('minute', TGocciaPropertyDescriptorAccessor.Create(
    TGocciaNativeFunctionValue.CreateWithoutPrototype(GetMinute, 'minute', 0), nil, [pfConfigurable]));
  FShared.Prototype.DefineProperty('second', TGocciaPropertyDescriptorAccessor.Create(
    TGocciaNativeFunctionValue.CreateWithoutPrototype(GetSecond, 'second', 0), nil, [pfConfigurable]));
  FShared.Prototype.DefineProperty('millisecond', TGocciaPropertyDescriptorAccessor.Create(
    TGocciaNativeFunctionValue.CreateWithoutPrototype(GetMillisecond, 'millisecond', 0), nil, [pfConfigurable]));
  FShared.Prototype.DefineProperty('microsecond', TGocciaPropertyDescriptorAccessor.Create(
    TGocciaNativeFunctionValue.CreateWithoutPrototype(GetMicrosecond, 'microsecond', 0), nil, [pfConfigurable]));
  FShared.Prototype.DefineProperty('nanosecond', TGocciaPropertyDescriptorAccessor.Create(
    TGocciaNativeFunctionValue.CreateWithoutPrototype(GetNanosecond, 'nanosecond', 0), nil, [pfConfigurable]));

  // Methods
  FShared.Prototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(DateTimeWith, 'with', 1));
  FShared.Prototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(DateTimeWithPlainTime, 'withPlainTime', 0));
  FShared.Prototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(DateTimeAdd, 'add', 1));
  FShared.Prototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(DateTimeSubtract, 'subtract', 1));
  FShared.Prototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(DateTimeUntil, 'until', 1));
  FShared.Prototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(DateTimeSince, 'since', 1));
  FShared.Prototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(DateTimeRound, 'round', 1));
  FShared.Prototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(DateTimeEquals, 'equals', 1));
  FShared.Prototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(DateTimeToString, 'toString', 0));
  FShared.Prototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(DateTimeToJSON, 'toJSON', 0));
  FShared.Prototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(DateTimeValueOf, 'valueOf', 0));
  FShared.Prototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(DateTimeToPlainDate, 'toPlainDate', 0));
  FShared.Prototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(DateTimeToPlainTime, 'toPlainTime', 0));
end;

class procedure TGocciaTemporalPlainDateTimeValue.ExposePrototype(const AConstructor: TGocciaObjectValue);
begin
  if not Assigned(FShared) then
    TGocciaTemporalPlainDateTimeValue.Create(1970, 1, 1, 0, 0, 0, 0, 0, 0);
  FShared.ExposeOnConstructor(AConstructor);
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
    ThrowTypeError('PlainDateTime.prototype.with requires an object argument');
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
      if not TryParseISOTime(TGocciaStringLiteralValue(Arg).Value, TimeRec) then
        ThrowTypeError('Invalid time string');
      H := TimeRec.Hour; Mi := TimeRec.Minute; S := TimeRec.Second;
      Ms := TimeRec.Millisecond; Us := TimeRec.Microsecond; Ns := TimeRec.Nanosecond;
    end;
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
      ThrowTypeError('Invalid duration string');
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
    ThrowTypeError('PlainDateTime.prototype.add requires a Duration or string');
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
      ThrowTypeError('Invalid duration string');
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
    ThrowTypeError('PlainDateTime.prototype.subtract requires a Duration or string');
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
  T1Ns, T2Ns, TotalNs, AbsNs, RemNs: Int64;
  DiffDays: Int64;
  Sgn: Int64;
begin
  D := AsPlainDateTime(AThisValue, 'PlainDateTime.prototype.until');
  Other := CoercePlainDateTime(AArgs.GetElement(0), 'PlainDateTime.prototype.until');

  T1Ns := Int64(D.FNanosecond) + Int64(D.FMicrosecond) * 1000 + Int64(D.FMillisecond) * 1000000 +
           Int64(D.FSecond) * Int64(1000000000) + Int64(D.FMinute) * Int64(60000000000) +
           Int64(D.FHour) * Int64(3600000000000);
  T2Ns := Int64(Other.FNanosecond) + Int64(Other.FMicrosecond) * 1000 + Int64(Other.FMillisecond) * 1000000 +
           Int64(Other.FSecond) * Int64(1000000000) + Int64(Other.FMinute) * Int64(60000000000) +
           Int64(Other.FHour) * Int64(3600000000000);

  // Compute total nanosecond difference to avoid mixed-sign components
  TotalNs := (DateToEpochDays(Other.FYear, Other.FMonth, Other.FDay) -
              DateToEpochDays(D.FYear, D.FMonth, D.FDay)) * Int64(86400000000000) +
             (T2Ns - T1Ns);

  if TotalNs > 0 then
    Sgn := 1
  else if TotalNs < 0 then
    Sgn := -1
  else
    Sgn := 0;

  AbsNs := Abs(TotalNs);
  DiffDays := AbsNs div Int64(86400000000000);
  RemNs := AbsNs mod Int64(86400000000000);

  Result := TGocciaTemporalDurationValue.Create(0, 0, 0, Sgn * DiffDays,
    Sgn * (RemNs div Int64(3600000000000)),
    Sgn * ((RemNs div Int64(60000000000)) mod 60),
    Sgn * ((RemNs div Int64(1000000000)) mod 60),
    Sgn * ((RemNs div 1000000) mod 1000),
    Sgn * ((RemNs div 1000) mod 1000),
    Sgn * (RemNs mod 1000));
end;

function TGocciaTemporalPlainDateTimeValue.DateTimeSince(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D, Other: TGocciaTemporalPlainDateTimeValue;
  NewArgs: TGocciaArgumentsCollection;
  UntilResult: TGocciaTemporalDurationValue;
begin
  D := AsPlainDateTime(AThisValue, 'PlainDateTime.prototype.since');
  Other := CoercePlainDateTime(AArgs.GetElement(0), 'PlainDateTime.prototype.since');

  // since(other) = other.until(this)
  NewArgs := TGocciaArgumentsCollection.Create([D]);
  try
    UntilResult := TGocciaTemporalDurationValue(DateTimeUntil(NewArgs, Other));
  finally
    NewArgs.Free;
  end;
  Result := UntilResult;
end;

function TGocciaTemporalPlainDateTimeValue.DateTimeRound(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalPlainDateTimeValue;
  UnitStr: string;
  Arg: TGocciaValue;
  TotalNs, Divisor, Rounded, ExtraDays: Int64;
  Balanced: TTemporalTimeRecord;
  DateRec: TTemporalDateRecord;
begin
  D := AsPlainDateTime(AThisValue, 'PlainDateTime.prototype.round');
  Arg := AArgs.GetElement(0);

  if Arg is TGocciaStringLiteralValue then
    UnitStr := TGocciaStringLiteralValue(Arg).Value
  else if Arg is TGocciaObjectValue then
  begin
    Arg := TGocciaObjectValue(Arg).GetProperty('smallestUnit');
    if (Arg = nil) or (Arg is TGocciaUndefinedLiteralValue) then
      ThrowRangeError('round() requires a smallestUnit option');
    UnitStr := Arg.ToStringLiteral.Value;
  end
  else
  begin
    ThrowTypeError('PlainDateTime.prototype.round requires a string or options object');
    UnitStr := '';
  end;

  TotalNs := Int64(D.FNanosecond) + Int64(D.FMicrosecond) * 1000 + Int64(D.FMillisecond) * 1000000 +
             Int64(D.FSecond) * Int64(1000000000) + Int64(D.FMinute) * Int64(60000000000) +
             Int64(D.FHour) * Int64(3600000000000);

  if UnitStr = 'day' then
  begin
    // Round to nearest day
    if TotalNs >= Int64(43200000000000) then
      DateRec := AddDaysToDate(D.FYear, D.FMonth, D.FDay, 1)
    else
    begin
      DateRec.Year := D.FYear;
      DateRec.Month := D.FMonth;
      DateRec.Day := D.FDay;
    end;
    Result := TGocciaTemporalPlainDateTimeValue.Create(DateRec.Year, DateRec.Month, DateRec.Day, 0, 0, 0, 0, 0, 0);
    Exit;
  end;

  if UnitStr = 'hour' then Divisor := Int64(3600000000000)
  else if UnitStr = 'minute' then Divisor := Int64(60000000000)
  else if UnitStr = 'second' then Divisor := Int64(1000000000)
  else if UnitStr = 'millisecond' then Divisor := 1000000
  else if UnitStr = 'microsecond' then Divisor := 1000
  else if UnitStr = 'nanosecond' then Divisor := 1
  else
  begin
    ThrowRangeError('Invalid unit for PlainDateTime.prototype.round: ' + UnitStr);
    Divisor := 1;
  end;

  Rounded := ((TotalNs + (Divisor div 2)) div Divisor) * Divisor;
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
begin
  D := AsPlainDateTime(AThisValue, 'PlainDateTime.prototype.toString');
  Result := TGocciaStringLiteralValue.Create(
    FormatDateString(D.FYear, D.FMonth, D.FDay) + 'T' +
    FormatTimeString(D.FHour, D.FMinute, D.FSecond, D.FMillisecond, D.FMicrosecond, D.FNanosecond));
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
  ThrowTypeError('Temporal.PlainDateTime.prototype.valueOf cannot be used; use toString or compare instead');
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

end.
