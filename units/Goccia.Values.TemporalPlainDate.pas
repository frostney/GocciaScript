unit Goccia.Values.TemporalPlainDate;

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
  TGocciaTemporalPlainDateValue = class(TGocciaObjectValue)
  private
    class var FShared: TGocciaSharedPrototype;
  private
    FYear: Integer;
    FMonth: Integer;
    FDay: Integer;

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

    function DateWith(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DateAdd(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DateSubtract(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DateUntil(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DateSince(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DateEquals(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DateToString(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DateToJSON(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DateValueOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DateToPlainDateTime(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;

    procedure InitializePrototype;
  public
    constructor Create(const AYear, AMonth, ADay: Integer); overload;

    function ToStringTag: string; override;
    class procedure ExposePrototype(const AConstructor: TGocciaObjectValue);

    property Year: Integer read FYear;
    property Month: Integer read FMonth;
    property Day: Integer read FDay;
  end;

implementation

uses
  Goccia.GarbageCollector,
  Goccia.Temporal.Utils,
  Goccia.Values.ErrorHelper,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.TemporalDuration,
  Goccia.Values.TemporalPlainDateTime,
  Goccia.Values.TemporalPlainTime;

function GetDurFieldOr(const AObj: TGocciaObjectValue; const AName: string; const ADefault: Int64): Int64;
var
  V: TGocciaValue;
begin
  V := AObj.GetProperty(AName);
  if (V = nil) or (V is TGocciaUndefinedLiteralValue) then
    Result := ADefault
  else
    Result := Trunc(V.ToNumberLiteral.Value);
end;

function AsPlainDate(const AValue: TGocciaValue; const AMethod: string): TGocciaTemporalPlainDateValue;
begin
  if not (AValue is TGocciaTemporalPlainDateValue) then
    ThrowTypeError(AMethod + ' called on non-PlainDate');
  Result := TGocciaTemporalPlainDateValue(AValue);
end;

function CoercePlainDate(const AValue: TGocciaValue; const AMethod: string): TGocciaTemporalPlainDateValue;
var
  DateRec: TTemporalDateRecord;
  TimeRec: TTemporalTimeRecord;
  Obj: TGocciaObjectValue;
  V, VMonth, VDay: TGocciaValue;
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
    VMonth := Obj.GetProperty('month');
    if (VMonth = nil) or (VMonth is TGocciaUndefinedLiteralValue) then
      ThrowTypeError(AMethod + ' requires year, month, day properties');
    VDay := Obj.GetProperty('day');
    if (VDay = nil) or (VDay is TGocciaUndefinedLiteralValue) then
      ThrowTypeError(AMethod + ' requires year, month, day properties');
    Result := TGocciaTemporalPlainDateValue.Create(
      Trunc(V.ToNumberLiteral.Value),
      Trunc(VMonth.ToNumberLiteral.Value),
      Trunc(VDay.ToNumberLiteral.Value));
  end
  else
  begin
    ThrowTypeError(AMethod + ' requires a PlainDate, string, or object');
    Result := nil;
  end;
end;

{ TGocciaTemporalPlainDateValue }

constructor TGocciaTemporalPlainDateValue.Create(const AYear, AMonth, ADay: Integer);
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

class procedure TGocciaTemporalPlainDateValue.ExposePrototype(const AConstructor: TGocciaObjectValue);
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

function TGocciaTemporalPlainDateValue.GetCalendarId(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  AsPlainDate(AThisValue, 'get PlainDate.calendarId');
  Result := TGocciaStringLiteralValue.Create('iso8601');
end;

function TGocciaTemporalPlainDateValue.GetYear(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaNumberLiteralValue.Create(AsPlainDate(AThisValue, 'get PlainDate.year').FYear);
end;

function TGocciaTemporalPlainDateValue.GetMonth(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaNumberLiteralValue.Create(AsPlainDate(AThisValue, 'get PlainDate.month').FMonth);
end;

function TGocciaTemporalPlainDateValue.GetMonthCode(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaStringLiteralValue.Create('M' + PadTwo(AsPlainDate(AThisValue, 'get PlainDate.monthCode').FMonth));
end;

function TGocciaTemporalPlainDateValue.GetDay(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaNumberLiteralValue.Create(AsPlainDate(AThisValue, 'get PlainDate.day').FDay);
end;

function TGocciaTemporalPlainDateValue.GetDayOfWeek(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalPlainDateValue;
begin
  D := AsPlainDate(AThisValue, 'get PlainDate.dayOfWeek');
  Result := TGocciaNumberLiteralValue.Create(Goccia.Temporal.Utils.DayOfWeek(D.FYear, D.FMonth, D.FDay));
end;

function TGocciaTemporalPlainDateValue.GetDayOfYear(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalPlainDateValue;
begin
  D := AsPlainDate(AThisValue, 'get PlainDate.dayOfYear');
  Result := TGocciaNumberLiteralValue.Create(Goccia.Temporal.Utils.DayOfYear(D.FYear, D.FMonth, D.FDay));
end;

function TGocciaTemporalPlainDateValue.GetWeekOfYear(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalPlainDateValue;
begin
  D := AsPlainDate(AThisValue, 'get PlainDate.weekOfYear');
  Result := TGocciaNumberLiteralValue.Create(Goccia.Temporal.Utils.WeekOfYear(D.FYear, D.FMonth, D.FDay));
end;

function TGocciaTemporalPlainDateValue.GetYearOfWeek(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalPlainDateValue;
begin
  D := AsPlainDate(AThisValue, 'get PlainDate.yearOfWeek');
  Result := TGocciaNumberLiteralValue.Create(Goccia.Temporal.Utils.YearOfWeek(D.FYear, D.FMonth, D.FDay));
end;

function TGocciaTemporalPlainDateValue.GetDaysInWeek(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  AsPlainDate(AThisValue, 'get PlainDate.daysInWeek');
  Result := TGocciaNumberLiteralValue.Create(7);
end;

function TGocciaTemporalPlainDateValue.GetDaysInMonth(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalPlainDateValue;
begin
  D := AsPlainDate(AThisValue, 'get PlainDate.daysInMonth');
  Result := TGocciaNumberLiteralValue.Create(Goccia.Temporal.Utils.DaysInMonth(D.FYear, D.FMonth));
end;

function TGocciaTemporalPlainDateValue.GetDaysInYear(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalPlainDateValue;
begin
  D := AsPlainDate(AThisValue, 'get PlainDate.daysInYear');
  Result := TGocciaNumberLiteralValue.Create(Goccia.Temporal.Utils.DaysInYear(D.FYear));
end;

function TGocciaTemporalPlainDateValue.GetMonthsInYear(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  AsPlainDate(AThisValue, 'get PlainDate.monthsInYear');
  Result := TGocciaNumberLiteralValue.Create(12);
end;

function TGocciaTemporalPlainDateValue.GetInLeapYear(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  if Goccia.Temporal.Utils.IsLeapYear(AsPlainDate(AThisValue, 'get PlainDate.inLeapYear').FYear) then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;

{ Methods }

function TGocciaTemporalPlainDateValue.DateWith(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalPlainDateValue;
  Obj: TGocciaObjectValue;
  V: TGocciaValue;
  NewYear, NewMonth, NewDay: Integer;

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
  D := AsPlainDate(AThisValue, 'PlainDate.prototype.with');
  V := AArgs.GetElement(0);
  if not (V is TGocciaObjectValue) then
    ThrowTypeError('PlainDate.prototype.with requires an object argument');
  Obj := TGocciaObjectValue(V);

  NewYear := GetFieldOr('year', D.FYear);
  NewMonth := GetFieldOr('month', D.FMonth);
  NewDay := GetFieldOr('day', D.FDay);
  Result := TGocciaTemporalPlainDateValue.Create(NewYear, NewMonth, NewDay);
end;

function TGocciaTemporalPlainDateValue.DateAdd(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalPlainDateValue;
  Dur: TGocciaTemporalDurationValue;
  Arg: TGocciaValue;
  ObjArg: TGocciaObjectValue;
  DurRec: TTemporalDurationRecord;
  NewYear, NewMonth, NewDay: Integer;
  DateRec: TTemporalDateRecord;
begin
  D := AsPlainDate(AThisValue, 'PlainDate.prototype.add');
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
    Dur := TGocciaTemporalDurationValue.Create(
      GetDurFieldOr(ObjArg, 'years', 0),
      GetDurFieldOr(ObjArg, 'months', 0),
      GetDurFieldOr(ObjArg, 'weeks', 0),
      GetDurFieldOr(ObjArg, 'days', 0),
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

function TGocciaTemporalPlainDateValue.DateSubtract(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalPlainDateValue;
  Dur: TGocciaTemporalDurationValue;
  Arg: TGocciaValue;
  ObjArg: TGocciaObjectValue;
  DurRec: TTemporalDurationRecord;
  NegatedDur: TGocciaTemporalDurationValue;
  NewArgs: TGocciaArgumentsCollection;
begin
  D := AsPlainDate(AThisValue, 'PlainDate.prototype.subtract');
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
    Dur := TGocciaTemporalDurationValue.Create(
      GetDurFieldOr(ObjArg, 'years', 0),
      GetDurFieldOr(ObjArg, 'months', 0),
      GetDurFieldOr(ObjArg, 'weeks', 0),
      GetDurFieldOr(ObjArg, 'days', 0),
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
    Result := DateAdd(NewArgs, AThisValue);
  finally
    NewArgs.Free;
  end;
end;

function TGocciaTemporalPlainDateValue.DateUntil(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D, Other: TGocciaTemporalPlainDateValue;
  DiffDays: Int64;
begin
  D := AsPlainDate(AThisValue, 'PlainDate.prototype.until');
  Other := CoercePlainDate(AArgs.GetElement(0), 'PlainDate.prototype.until');

  DiffDays := DateToEpochDays(Other.FYear, Other.FMonth, Other.FDay) -
              DateToEpochDays(D.FYear, D.FMonth, D.FDay);

  Result := TGocciaTemporalDurationValue.Create(0, 0, 0, DiffDays, 0, 0, 0, 0, 0, 0);
end;

function TGocciaTemporalPlainDateValue.DateSince(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D, Other: TGocciaTemporalPlainDateValue;
  DiffDays: Int64;
begin
  D := AsPlainDate(AThisValue, 'PlainDate.prototype.since');
  Other := CoercePlainDate(AArgs.GetElement(0), 'PlainDate.prototype.since');

  DiffDays := DateToEpochDays(D.FYear, D.FMonth, D.FDay) -
              DateToEpochDays(Other.FYear, Other.FMonth, Other.FDay);

  Result := TGocciaTemporalDurationValue.Create(0, 0, 0, DiffDays, 0, 0, 0, 0, 0, 0);
end;

function TGocciaTemporalPlainDateValue.DateEquals(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D, Other: TGocciaTemporalPlainDateValue;
begin
  D := AsPlainDate(AThisValue, 'PlainDate.prototype.equals');
  Other := CoercePlainDate(AArgs.GetElement(0), 'PlainDate.prototype.equals');

  if (D.FYear = Other.FYear) and (D.FMonth = Other.FMonth) and (D.FDay = Other.FDay) then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;

function TGocciaTemporalPlainDateValue.DateToString(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalPlainDateValue;
begin
  D := AsPlainDate(AThisValue, 'PlainDate.prototype.toString');
  Result := TGocciaStringLiteralValue.Create(FormatDateString(D.FYear, D.FMonth, D.FDay));
end;

function TGocciaTemporalPlainDateValue.DateToJSON(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalPlainDateValue;
begin
  D := AsPlainDate(AThisValue, 'PlainDate.prototype.toJSON');
  Result := TGocciaStringLiteralValue.Create(FormatDateString(D.FYear, D.FMonth, D.FDay));
end;

function TGocciaTemporalPlainDateValue.DateValueOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  ThrowTypeError('Temporal.PlainDate.prototype.valueOf cannot be used; use toString or compare instead');
  Result := nil;
end;

function TGocciaTemporalPlainDateValue.DateToPlainDateTime(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalPlainDateValue;
  T: TGocciaTemporalPlainTimeValue;
  Arg: TGocciaValue;
  Obj: TGocciaObjectValue;
  V: TGocciaValue;
  Hour, Minute, Second, Ms, Us, Ns: Integer;
begin
  D := AsPlainDate(AThisValue, 'PlainDate.prototype.toPlainDateTime');
  Arg := AArgs.GetElement(0);

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
