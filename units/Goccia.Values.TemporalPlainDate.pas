unit Goccia.Values.TemporalPlainDate;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Primitives, Goccia.Values.ObjectValue,
  Goccia.Values.NativeFunction, Goccia.Arguments.Collection,
  Goccia.SharedPrototype, SysUtils;

type
  TGocciaTemporalPlainDateValue = class(TGocciaObjectValue)
  private
    class var FShared: TGocciaSharedPrototype;
  private
    FYear: Integer;
    FMonth: Integer;
    FDay: Integer;

    function GetCalendarId(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function GetYear(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function GetMonth(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function GetMonthCode(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function GetDay(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function GetDayOfWeek(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function GetDayOfYear(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function GetWeekOfYear(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function GetYearOfWeek(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function GetDaysInWeek(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function GetDaysInMonth(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function GetDaysInYear(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function GetMonthsInYear(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function GetInLeapYear(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;

    function DateWith(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function DateAdd(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function DateSubtract(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function DateUntil(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function DateSince(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function DateEquals(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function DateToString(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function DateToJSON(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function DateValueOf(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function DateToPlainDateTime(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;

    procedure InitializePrototype;
  public
    constructor Create(AYear, AMonth, ADay: Integer); overload;

    function ToStringTag: string; override;
    class procedure ExposePrototype(AConstructor: TGocciaObjectValue);

    property Year: Integer read FYear;
    property Month: Integer read FMonth;
    property Day: Integer read FDay;
  end;

implementation

uses
  Goccia.Values.ErrorHelper, Goccia.GarbageCollector,
  Goccia.Values.ObjectPropertyDescriptor, Goccia.Temporal.Utils,
  Goccia.Values.TemporalDuration, Goccia.Values.TemporalPlainTime,
  Goccia.Values.TemporalPlainDateTime;

function AsPlainDate(AValue: TGocciaValue; const AMethod: string): TGocciaTemporalPlainDateValue;
begin
  if not (AValue is TGocciaTemporalPlainDateValue) then
    ThrowTypeError(AMethod + ' called on non-PlainDate');
  Result := TGocciaTemporalPlainDateValue(AValue);
end;

function CoercePlainDate(AValue: TGocciaValue; const AMethod: string): TGocciaTemporalPlainDateValue;
var
  DateRec: TTemporalDateRecord;
  TimeRec: TTemporalTimeRecord;
  Obj: TGocciaObjectValue;
  V: TGocciaValue;
begin
  if AValue is TGocciaTemporalPlainDateValue then
    Result := TGocciaTemporalPlainDateValue(AValue)
  else if AValue is TGocciaStringLiteralValue then
  begin
    if not TryParseISODate(TGocciaStringLiteralValue(AValue).Value, DateRec) then
    begin
      // Try parsing as datetime
      if TryParseISODateTime(TGocciaStringLiteralValue(AValue).Value, DateRec, TimeRec) then
        Result := TGocciaTemporalPlainDateValue.Create(DateRec.Year, DateRec.Month, DateRec.Day)
      else
        ThrowTypeError('Invalid ISO date string for ' + AMethod);
    end
    else
      Result := TGocciaTemporalPlainDateValue.Create(DateRec.Year, DateRec.Month, DateRec.Day);
  end
  else if AValue is TGocciaObjectValue then
  begin
    Obj := TGocciaObjectValue(AValue);
    V := Obj.GetProperty('year');
    if (V = nil) or (V is TGocciaUndefinedLiteralValue) then
      ThrowTypeError(AMethod + ' requires year, month, day properties');
    Result := TGocciaTemporalPlainDateValue.Create(
      Trunc(Obj.GetProperty('year').ToNumberLiteral.Value),
      Trunc(Obj.GetProperty('month').ToNumberLiteral.Value),
      Trunc(Obj.GetProperty('day').ToNumberLiteral.Value));
  end
  else
  begin
    ThrowTypeError(AMethod + ' requires a PlainDate, string, or object');
    Result := nil;
  end;
end;

{ TGocciaTemporalPlainDateValue }

constructor TGocciaTemporalPlainDateValue.Create(AYear, AMonth, ADay: Integer);
begin
  inherited Create(nil);
  if not IsValidDate(AYear, AMonth, ADay) then
    ThrowRangeError('Invalid date: ' + FormatDateString(AYear, AMonth, ADay));
  FYear := AYear;
  FMonth := AMonth;
  FDay := ADay;
  InitializePrototype;
  if Assigned(FShared) then
    FPrototype := FShared.Prototype;
end;

procedure TGocciaTemporalPlainDateValue.InitializePrototype;
begin
  if Assigned(FShared) then Exit;
  FShared := TGocciaSharedPrototype.Create(Self);

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

  FShared.Prototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(DateWith, 'with', 1));
  FShared.Prototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(DateAdd, 'add', 1));
  FShared.Prototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(DateSubtract, 'subtract', 1));
  FShared.Prototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(DateUntil, 'until', 1));
  FShared.Prototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(DateSince, 'since', 1));
  FShared.Prototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(DateEquals, 'equals', 1));
  FShared.Prototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(DateToString, 'toString', 0));
  FShared.Prototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(DateToJSON, 'toJSON', 0));
  FShared.Prototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(DateValueOf, 'valueOf', 0));
  FShared.Prototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(DateToPlainDateTime, 'toPlainDateTime', 0));
end;

class procedure TGocciaTemporalPlainDateValue.ExposePrototype(AConstructor: TGocciaObjectValue);
begin
  if not Assigned(FShared) then
    TGocciaTemporalPlainDateValue.Create(1970, 1, 1);
  FShared.ExposeOnConstructor(AConstructor);
end;

function TGocciaTemporalPlainDateValue.ToStringTag: string;
begin
  Result := 'Temporal.PlainDate';
end;

{ Getters }

function TGocciaTemporalPlainDateValue.GetCalendarId(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
begin
  AsPlainDate(ThisValue, 'get PlainDate.calendarId');
  Result := TGocciaStringLiteralValue.Create('iso8601');
end;

function TGocciaTemporalPlainDateValue.GetYear(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaNumberLiteralValue.Create(AsPlainDate(ThisValue, 'get PlainDate.year').FYear);
end;

function TGocciaTemporalPlainDateValue.GetMonth(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaNumberLiteralValue.Create(AsPlainDate(ThisValue, 'get PlainDate.month').FMonth);
end;

function TGocciaTemporalPlainDateValue.GetMonthCode(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaStringLiteralValue.Create('M' + PadTwo(AsPlainDate(ThisValue, 'get PlainDate.monthCode').FMonth));
end;

function TGocciaTemporalPlainDateValue.GetDay(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaNumberLiteralValue.Create(AsPlainDate(ThisValue, 'get PlainDate.day').FDay);
end;

function TGocciaTemporalPlainDateValue.GetDayOfWeek(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalPlainDateValue;
begin
  D := AsPlainDate(ThisValue, 'get PlainDate.dayOfWeek');
  Result := TGocciaNumberLiteralValue.Create(Goccia.Temporal.Utils.DayOfWeek(D.FYear, D.FMonth, D.FDay));
end;

function TGocciaTemporalPlainDateValue.GetDayOfYear(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalPlainDateValue;
begin
  D := AsPlainDate(ThisValue, 'get PlainDate.dayOfYear');
  Result := TGocciaNumberLiteralValue.Create(Goccia.Temporal.Utils.DayOfYear(D.FYear, D.FMonth, D.FDay));
end;

function TGocciaTemporalPlainDateValue.GetWeekOfYear(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalPlainDateValue;
begin
  D := AsPlainDate(ThisValue, 'get PlainDate.weekOfYear');
  Result := TGocciaNumberLiteralValue.Create(Goccia.Temporal.Utils.WeekOfYear(D.FYear, D.FMonth, D.FDay));
end;

function TGocciaTemporalPlainDateValue.GetYearOfWeek(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalPlainDateValue;
begin
  D := AsPlainDate(ThisValue, 'get PlainDate.yearOfWeek');
  Result := TGocciaNumberLiteralValue.Create(Goccia.Temporal.Utils.YearOfWeek(D.FYear, D.FMonth, D.FDay));
end;

function TGocciaTemporalPlainDateValue.GetDaysInWeek(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
begin
  AsPlainDate(ThisValue, 'get PlainDate.daysInWeek');
  Result := TGocciaNumberLiteralValue.Create(7);
end;

function TGocciaTemporalPlainDateValue.GetDaysInMonth(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalPlainDateValue;
begin
  D := AsPlainDate(ThisValue, 'get PlainDate.daysInMonth');
  Result := TGocciaNumberLiteralValue.Create(Goccia.Temporal.Utils.DaysInMonth(D.FYear, D.FMonth));
end;

function TGocciaTemporalPlainDateValue.GetDaysInYear(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalPlainDateValue;
begin
  D := AsPlainDate(ThisValue, 'get PlainDate.daysInYear');
  Result := TGocciaNumberLiteralValue.Create(Goccia.Temporal.Utils.DaysInYear(D.FYear));
end;

function TGocciaTemporalPlainDateValue.GetMonthsInYear(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
begin
  AsPlainDate(ThisValue, 'get PlainDate.monthsInYear');
  Result := TGocciaNumberLiteralValue.Create(12);
end;

function TGocciaTemporalPlainDateValue.GetInLeapYear(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
begin
  if Goccia.Temporal.Utils.IsLeapYear(AsPlainDate(ThisValue, 'get PlainDate.inLeapYear').FYear) then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;

{ Methods }

function TGocciaTemporalPlainDateValue.DateWith(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalPlainDateValue;
  Obj: TGocciaObjectValue;
  V: TGocciaValue;
  NewYear, NewMonth, NewDay: Integer;

  function GetFieldOr(const AName: string; ADefault: Integer): Integer;
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
  D := AsPlainDate(ThisValue, 'PlainDate.prototype.with');
  V := Args.GetElement(0);
  if not (V is TGocciaObjectValue) then
    ThrowTypeError('PlainDate.prototype.with requires an object argument');
  Obj := TGocciaObjectValue(V);

  NewYear := GetFieldOr('year', D.FYear);
  NewMonth := GetFieldOr('month', D.FMonth);
  NewDay := GetFieldOr('day', D.FDay);
  Result := TGocciaTemporalPlainDateValue.Create(NewYear, NewMonth, NewDay);
end;

function TGocciaTemporalPlainDateValue.DateAdd(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalPlainDateValue;
  Dur: TGocciaTemporalDurationValue;
  Arg: TGocciaValue;
  ObjArg: TGocciaObjectValue;
  V: TGocciaValue;
  DurRec: TTemporalDurationRecord;
  NewYear, NewMonth, NewDay: Integer;
  DateRec: TTemporalDateRecord;
begin
  D := AsPlainDate(ThisValue, 'PlainDate.prototype.add');
  Arg := Args.GetElement(0);

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
    Dur := TGocciaTemporalDurationValue.Create(0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
    // Try to extract duration fields from object
    ObjArg := TGocciaObjectValue(Arg);
    V := ObjArg.GetProperty('years');
    if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
      Dur := TGocciaTemporalDurationValue.Create(Trunc(V.ToNumberLiteral.Value), 0, 0, 0, 0, 0, 0, 0, 0, 0);
    // Simplified - create properly from all fields
    Dur := TGocciaTemporalDurationValue.Create(
      Trunc(ObjArg.GetProperty('years').ToNumberLiteral.Value),
      Trunc(ObjArg.GetProperty('months').ToNumberLiteral.Value),
      Trunc(ObjArg.GetProperty('weeks').ToNumberLiteral.Value),
      Trunc(ObjArg.GetProperty('days').ToNumberLiteral.Value),
      0, 0, 0, 0, 0, 0);
  end
  else
  begin
    ThrowTypeError('PlainDate.prototype.add requires a Duration or string');
    Dur := nil;
  end;

  // Add years then months then days
  NewYear := D.FYear + Integer(Dur.Years);
  NewMonth := D.FMonth + Integer(Dur.Months);

  // Normalize month overflow
  while NewMonth > 12 do
  begin
    Inc(NewYear);
    Dec(NewMonth, 12);
  end;
  while NewMonth < 1 do
  begin
    Dec(NewYear);
    Inc(NewMonth, 12);
  end;

  // Clamp day to valid range
  NewDay := D.FDay;
  if NewDay > Goccia.Temporal.Utils.DaysInMonth(NewYear, NewMonth) then
    NewDay := Goccia.Temporal.Utils.DaysInMonth(NewYear, NewMonth);

  // Add weeks and days
  DateRec := AddDaysToDate(NewYear, NewMonth, NewDay, Dur.Weeks * 7 + Dur.Days);

  Result := TGocciaTemporalPlainDateValue.Create(DateRec.Year, DateRec.Month, DateRec.Day);
end;

function TGocciaTemporalPlainDateValue.DateSubtract(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalPlainDateValue;
  Dur: TGocciaTemporalDurationValue;
  Arg: TGocciaValue;
  ObjArg: TGocciaObjectValue;
  DurRec: TTemporalDurationRecord;
  NegatedDur: TGocciaTemporalDurationValue;
  NewArgs: TGocciaArgumentsCollection;
begin
  D := AsPlainDate(ThisValue, 'PlainDate.prototype.subtract');
  Arg := Args.GetElement(0);

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
    Dur := TGocciaTemporalDurationValue.Create(
      Trunc(ObjArg.GetProperty('years').ToNumberLiteral.Value),
      Trunc(ObjArg.GetProperty('months').ToNumberLiteral.Value),
      Trunc(ObjArg.GetProperty('weeks').ToNumberLiteral.Value),
      Trunc(ObjArg.GetProperty('days').ToNumberLiteral.Value),
      0, 0, 0, 0, 0, 0);
  end
  else
  begin
    ThrowTypeError('PlainDate.prototype.subtract requires a Duration or string');
    Dur := nil;
  end;

  NegatedDur := TGocciaTemporalDurationValue.Create(
    -Dur.Years, -Dur.Months, -Dur.Weeks, -Dur.Days,
    -Dur.Hours, -Dur.Minutes, -Dur.Seconds,
    -Dur.Milliseconds, -Dur.Microseconds, -Dur.Nanoseconds);

  NewArgs := TGocciaArgumentsCollection.Create([NegatedDur]);
  try
    Result := DateAdd(NewArgs, ThisValue);
  finally
    NewArgs.Free;
  end;
end;

function TGocciaTemporalPlainDateValue.DateUntil(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  D, Other: TGocciaTemporalPlainDateValue;
  DiffDays: Int64;
begin
  D := AsPlainDate(ThisValue, 'PlainDate.prototype.until');
  Other := CoercePlainDate(Args.GetElement(0), 'PlainDate.prototype.until');

  DiffDays := DateToEpochDays(Other.FYear, Other.FMonth, Other.FDay) -
              DateToEpochDays(D.FYear, D.FMonth, D.FDay);

  Result := TGocciaTemporalDurationValue.Create(0, 0, 0, DiffDays, 0, 0, 0, 0, 0, 0);
end;

function TGocciaTemporalPlainDateValue.DateSince(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  D, Other: TGocciaTemporalPlainDateValue;
  DiffDays: Int64;
begin
  D := AsPlainDate(ThisValue, 'PlainDate.prototype.since');
  Other := CoercePlainDate(Args.GetElement(0), 'PlainDate.prototype.since');

  DiffDays := DateToEpochDays(D.FYear, D.FMonth, D.FDay) -
              DateToEpochDays(Other.FYear, Other.FMonth, Other.FDay);

  Result := TGocciaTemporalDurationValue.Create(0, 0, 0, DiffDays, 0, 0, 0, 0, 0, 0);
end;

function TGocciaTemporalPlainDateValue.DateEquals(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  D, Other: TGocciaTemporalPlainDateValue;
begin
  D := AsPlainDate(ThisValue, 'PlainDate.prototype.equals');
  Other := CoercePlainDate(Args.GetElement(0), 'PlainDate.prototype.equals');

  if (D.FYear = Other.FYear) and (D.FMonth = Other.FMonth) and (D.FDay = Other.FDay) then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;

function TGocciaTemporalPlainDateValue.DateToString(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalPlainDateValue;
begin
  D := AsPlainDate(ThisValue, 'PlainDate.prototype.toString');
  Result := TGocciaStringLiteralValue.Create(FormatDateString(D.FYear, D.FMonth, D.FDay));
end;

function TGocciaTemporalPlainDateValue.DateToJSON(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalPlainDateValue;
begin
  D := AsPlainDate(ThisValue, 'PlainDate.prototype.toJSON');
  Result := TGocciaStringLiteralValue.Create(FormatDateString(D.FYear, D.FMonth, D.FDay));
end;

function TGocciaTemporalPlainDateValue.DateValueOf(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
begin
  ThrowTypeError('Temporal.PlainDate.prototype.valueOf cannot be used; use toString or compare instead');
  Result := nil;
end;

function TGocciaTemporalPlainDateValue.DateToPlainDateTime(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalPlainDateValue;
  T: TGocciaTemporalPlainTimeValue;
  Arg: TGocciaValue;
  Obj: TGocciaObjectValue;
  V: TGocciaValue;
  Hour, Minute, Second, Ms, Us, Ns: Integer;
begin
  D := AsPlainDate(ThisValue, 'PlainDate.prototype.toPlainDateTime');
  Arg := Args.GetElement(0);

  Hour := 0; Minute := 0; Second := 0; Ms := 0; Us := 0; Ns := 0;

  if Assigned(Arg) and not (Arg is TGocciaUndefinedLiteralValue) then
  begin
    if Arg is TGocciaTemporalPlainTimeValue then
    begin
      T := TGocciaTemporalPlainTimeValue(Arg);
      Hour := T.Hour; Minute := T.Minute; Second := T.Second;
      Ms := T.Millisecond; Us := T.Microsecond; Ns := T.Nanosecond;
    end
    else if Arg is TGocciaObjectValue then
    begin
      Obj := TGocciaObjectValue(Arg);
      V := Obj.GetProperty('hour');
      if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
        Hour := Trunc(V.ToNumberLiteral.Value);
      V := Obj.GetProperty('minute');
      if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
        Minute := Trunc(V.ToNumberLiteral.Value);
      V := Obj.GetProperty('second');
      if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
        Second := Trunc(V.ToNumberLiteral.Value);
      V := Obj.GetProperty('millisecond');
      if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
        Ms := Trunc(V.ToNumberLiteral.Value);
      V := Obj.GetProperty('microsecond');
      if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
        Us := Trunc(V.ToNumberLiteral.Value);
      V := Obj.GetProperty('nanosecond');
      if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
        Ns := Trunc(V.ToNumberLiteral.Value);
    end;
  end;

  Result := TGocciaTemporalPlainDateTimeValue.Create(D.FYear, D.FMonth, D.FDay,
    Hour, Minute, Second, Ms, Us, Ns);
end;

end.
