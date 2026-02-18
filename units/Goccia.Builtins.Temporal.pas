unit Goccia.Builtins.Temporal;

{$I Goccia.inc}

interface

uses
  Goccia.Builtins.Base, Goccia.Scope, Goccia.Error.ThrowErrorCallback,
  Goccia.Values.Primitives, Goccia.Values.NativeFunction,
  Goccia.Values.ObjectValue, Goccia.Arguments.Collection, SysUtils;

type
  TGocciaTemporalBuiltin = class(TGocciaBuiltin)
  private
    FTemporalNamespace: TGocciaObjectValue;

    // Duration constructor + statics
    function DurationConstructorFn(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function DurationFrom(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function DurationCompare(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;

    // Instant constructor + statics
    function InstantConstructorFn(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function InstantFrom(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function InstantFromEpochMilliseconds(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function InstantFromEpochNanoseconds(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function InstantCompare(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;

    // PlainDate constructor + statics
    function PlainDateConstructorFn(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function PlainDateFrom(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function PlainDateCompare(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;

    // PlainTime constructor + statics
    function PlainTimeConstructorFn(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function PlainTimeFrom(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function PlainTimeCompare(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;

    // PlainDateTime constructor + statics
    function PlainDateTimeConstructorFn(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function PlainDateTimeFrom(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function PlainDateTimeCompare(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;

    // Temporal.Now
    function NowInstant(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function NowPlainDateISO(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function NowPlainTimeISO(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function NowPlainDateTimeISO(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;

    procedure RegisterDuration;
    procedure RegisterInstant;
    procedure RegisterPlainDate;
    procedure RegisterPlainTime;
    procedure RegisterPlainDateTime;
    procedure RegisterNow;
  public
    constructor Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
  end;

implementation

uses
  Goccia.Values.TemporalDuration, Goccia.Values.TemporalInstant,
  Goccia.Values.TemporalPlainDate, Goccia.Values.TemporalPlainTime,
  Goccia.Values.TemporalPlainDateTime, Goccia.Values.ErrorHelper,
  Goccia.Temporal.Utils, Goccia.Values.ObjectPropertyDescriptor,
  TimingUtils, DateUtils;

{ TGocciaTemporalBuiltin }

constructor TGocciaTemporalBuiltin.Create(const AName: string; const AScope: TGocciaScope;
  const AThrowError: TGocciaThrowErrorCallback);
begin
  inherited Create(AName, AScope, AThrowError);

  FTemporalNamespace := TGocciaObjectValue.Create;

  RegisterDuration;
  RegisterInstant;
  RegisterPlainDate;
  RegisterPlainTime;
  RegisterPlainDateTime;
  RegisterNow;

  AScope.DefineLexicalBinding(AName, FTemporalNamespace, dtLet);
end;

{ Duration }

procedure TGocciaTemporalBuiltin.RegisterDuration;
var
  Ctor: TGocciaNativeFunctionValue;
begin
  Ctor := TGocciaNativeFunctionValue.Create(DurationConstructorFn, 'Duration', 0);
  TGocciaTemporalDurationValue.ExposePrototype(Ctor);
  Ctor.AssignProperty('from',
    TGocciaNativeFunctionValue.CreateWithoutPrototype(DurationFrom, 'from', 1));
  Ctor.AssignProperty('compare',
    TGocciaNativeFunctionValue.CreateWithoutPrototype(DurationCompare, 'compare', 2));
  FTemporalNamespace.AssignProperty('Duration', Ctor);
end;

function TGocciaTemporalBuiltin.DurationConstructorFn(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  Y, Mo, W, D, H, Mi, S, Ms, Us, Ns: Int64;

  function GetArgOr(AIndex: Integer; ADefault: Int64): Int64;
  var
    V: TGocciaValue;
  begin
    if AIndex < Args.Length then
    begin
      V := Args.GetElement(AIndex);
      if (V is TGocciaUndefinedLiteralValue) then
        Result := ADefault
      else
        Result := Trunc(V.ToNumberLiteral.Value);
    end
    else
      Result := ADefault;
  end;

begin
  Y := GetArgOr(0, 0);
  Mo := GetArgOr(1, 0);
  W := GetArgOr(2, 0);
  D := GetArgOr(3, 0);
  H := GetArgOr(4, 0);
  Mi := GetArgOr(5, 0);
  S := GetArgOr(6, 0);
  Ms := GetArgOr(7, 0);
  Us := GetArgOr(8, 0);
  Ns := GetArgOr(9, 0);

  Result := TGocciaTemporalDurationValue.Create(Y, Mo, W, D, H, Mi, S, Ms, Us, Ns);
end;

function TGocciaTemporalBuiltin.DurationFrom(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  Arg: TGocciaValue;
  D: TGocciaTemporalDurationValue;
  DurRec: TTemporalDurationRecord;
  Obj: TGocciaObjectValue;

  function GetFieldOr(const AName: string; ADefault: Int64): Int64;
  var
    V: TGocciaValue;
  begin
    V := Obj.GetProperty(AName);
    if (V = nil) or (V is TGocciaUndefinedLiteralValue) then
      Result := ADefault
    else
      Result := Trunc(V.ToNumberLiteral.Value);
  end;

begin
  Arg := Args.GetElement(0);

  if Arg is TGocciaTemporalDurationValue then
  begin
    D := TGocciaTemporalDurationValue(Arg);
    Result := TGocciaTemporalDurationValue.Create(
      D.Years, D.Months, D.Weeks, D.Days, D.Hours, D.Minutes,
      D.Seconds, D.Milliseconds, D.Microseconds, D.Nanoseconds);
  end
  else if Arg is TGocciaStringLiteralValue then
  begin
    if not TryParseISODuration(TGocciaStringLiteralValue(Arg).Value, DurRec) then
      ThrowRangeError('Invalid ISO duration string');
    Result := TGocciaTemporalDurationValue.Create(
      DurRec.Years, DurRec.Months, DurRec.Weeks, DurRec.Days,
      DurRec.Hours, DurRec.Minutes, DurRec.Seconds,
      DurRec.Milliseconds, DurRec.Microseconds, DurRec.Nanoseconds);
  end
  else if Arg is TGocciaObjectValue then
  begin
    Obj := TGocciaObjectValue(Arg);
    Result := TGocciaTemporalDurationValue.Create(
      GetFieldOr('years', 0), GetFieldOr('months', 0), GetFieldOr('weeks', 0),
      GetFieldOr('days', 0), GetFieldOr('hours', 0), GetFieldOr('minutes', 0),
      GetFieldOr('seconds', 0), GetFieldOr('milliseconds', 0),
      GetFieldOr('microseconds', 0), GetFieldOr('nanoseconds', 0));
  end
  else
  begin
    ThrowTypeError('Temporal.Duration.from requires a string, Duration, or object');
    Result := nil;
  end;
end;

function TGocciaTemporalBuiltin.DurationCompare(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  D1, D2: TGocciaTemporalDurationValue;
  TotalNs1, TotalNs2: Double;

  function CoerceDuration(AArg: TGocciaValue): TGocciaTemporalDurationValue;
  var
    DurRec: TTemporalDurationRecord;
    Obj: TGocciaObjectValue;
  begin
    if AArg is TGocciaTemporalDurationValue then
      Result := TGocciaTemporalDurationValue(AArg)
    else if AArg is TGocciaStringLiteralValue then
    begin
      if not TryParseISODuration(TGocciaStringLiteralValue(AArg).Value, DurRec) then
        ThrowRangeError('Invalid ISO duration string');
      Result := TGocciaTemporalDurationValue.Create(
        DurRec.Years, DurRec.Months, DurRec.Weeks, DurRec.Days,
        DurRec.Hours, DurRec.Minutes, DurRec.Seconds,
        DurRec.Milliseconds, DurRec.Microseconds, DurRec.Nanoseconds);
    end
    else
    begin
      ThrowTypeError('Temporal.Duration.compare requires Duration arguments');
      Result := nil;
    end;
  end;

begin
  D1 := CoerceDuration(Args.GetElement(0));
  D2 := CoerceDuration(Args.GetElement(1));

  // Compare using total nanoseconds (ignoring calendar units for simplicity)
  TotalNs1 := D1.Nanoseconds + D1.Microseconds * 1000.0 + D1.Milliseconds * 1000000.0 +
              D1.Seconds * 1e9 + D1.Minutes * 6e10 + D1.Hours * 3.6e12 +
              D1.Days * 8.64e13 + D1.Weeks * 6.048e14;
  TotalNs2 := D2.Nanoseconds + D2.Microseconds * 1000.0 + D2.Milliseconds * 1000000.0 +
              D2.Seconds * 1e9 + D2.Minutes * 6e10 + D2.Hours * 3.6e12 +
              D2.Days * 8.64e13 + D2.Weeks * 6.048e14;

  if TotalNs1 < TotalNs2 then
    Result := TGocciaNumberLiteralValue.Create(-1)
  else if TotalNs1 > TotalNs2 then
    Result := TGocciaNumberLiteralValue.Create(1)
  else
    Result := TGocciaNumberLiteralValue.ZeroValue;
end;

{ Instant }

procedure TGocciaTemporalBuiltin.RegisterInstant;
var
  Ctor: TGocciaNativeFunctionValue;
begin
  Ctor := TGocciaNativeFunctionValue.Create(InstantConstructorFn, 'Instant', 1);
  TGocciaTemporalInstantValue.ExposePrototype(Ctor);
  Ctor.AssignProperty('from',
    TGocciaNativeFunctionValue.CreateWithoutPrototype(InstantFrom, 'from', 1));
  Ctor.AssignProperty('fromEpochMilliseconds',
    TGocciaNativeFunctionValue.CreateWithoutPrototype(InstantFromEpochMilliseconds, 'fromEpochMilliseconds', 1));
  Ctor.AssignProperty('fromEpochNanoseconds',
    TGocciaNativeFunctionValue.CreateWithoutPrototype(InstantFromEpochNanoseconds, 'fromEpochNanoseconds', 1));
  Ctor.AssignProperty('compare',
    TGocciaNativeFunctionValue.CreateWithoutPrototype(InstantCompare, 'compare', 2));
  FTemporalNamespace.AssignProperty('Instant', Ctor);
end;

function TGocciaTemporalBuiltin.InstantConstructorFn(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  EpochNs: Double;
  Ms: Int64;
  SubMs: Integer;
begin
  if Args.Length < 1 then
    ThrowTypeError('Temporal.Instant requires epochNanoseconds argument');

  EpochNs := Args.GetElement(0).ToNumberLiteral.Value;
  Ms := Trunc(EpochNs / 1000000);
  SubMs := Trunc(EpochNs - Ms * 1000000.0);

  Result := TGocciaTemporalInstantValue.Create(Ms, SubMs);
end;

function TGocciaTemporalBuiltin.InstantFrom(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  Arg: TGocciaValue;
  Inst: TGocciaTemporalInstantValue;
  DateRec: TTemporalDateRecord;
  TimeRec: TTemporalTimeRecord;
  EpochMs: Int64;
  SubMs: Integer;
begin
  Arg := Args.GetElement(0);

  if Arg is TGocciaTemporalInstantValue then
  begin
    Inst := TGocciaTemporalInstantValue(Arg);
    Result := TGocciaTemporalInstantValue.Create(Inst.EpochMilliseconds, Inst.SubMillisecondNanoseconds);
  end
  else if Arg is TGocciaStringLiteralValue then
  begin
    if not TryParseISODateTime(TGocciaStringLiteralValue(Arg).Value, DateRec, TimeRec) then
      ThrowTypeError('Invalid ISO instant string');
    EpochMs := DateToEpochDays(DateRec.Year, DateRec.Month, DateRec.Day) * Int64(86400000) +
               Int64(TimeRec.Hour) * 3600000 + Int64(TimeRec.Minute) * 60000 +
               Int64(TimeRec.Second) * 1000 + TimeRec.Millisecond;
    SubMs := TimeRec.Microsecond * 1000 + TimeRec.Nanosecond;
    Result := TGocciaTemporalInstantValue.Create(EpochMs, SubMs);
  end
  else
  begin
    ThrowTypeError('Temporal.Instant.from requires a string or Instant');
    Result := nil;
  end;
end;

function TGocciaTemporalBuiltin.InstantFromEpochMilliseconds(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
begin
  if Args.Length < 1 then
    ThrowTypeError('Temporal.Instant.fromEpochMilliseconds requires an argument');
  Result := TGocciaTemporalInstantValue.Create(
    Trunc(Args.GetElement(0).ToNumberLiteral.Value), 0);
end;

function TGocciaTemporalBuiltin.InstantFromEpochNanoseconds(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  EpochNs: Double;
  Ms: Int64;
  SubMs: Integer;
begin
  if Args.Length < 1 then
    ThrowTypeError('Temporal.Instant.fromEpochNanoseconds requires an argument');
  EpochNs := Args.GetElement(0).ToNumberLiteral.Value;
  Ms := Trunc(EpochNs / 1000000);
  SubMs := Trunc(EpochNs - Ms * 1000000.0);
  Result := TGocciaTemporalInstantValue.Create(Ms, SubMs);
end;

function TGocciaTemporalBuiltin.InstantCompare(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  I1, I2: TGocciaTemporalInstantValue;

  function CoerceInst(AArg: TGocciaValue): TGocciaTemporalInstantValue;
  var
    DateRec: TTemporalDateRecord;
    TimeRec: TTemporalTimeRecord;
    EpochMs: Int64;
    SubMs: Integer;
  begin
    if AArg is TGocciaTemporalInstantValue then
      Result := TGocciaTemporalInstantValue(AArg)
    else if AArg is TGocciaStringLiteralValue then
    begin
      if not TryParseISODateTime(TGocciaStringLiteralValue(AArg).Value, DateRec, TimeRec) then
        ThrowTypeError('Invalid ISO instant string');
      EpochMs := DateToEpochDays(DateRec.Year, DateRec.Month, DateRec.Day) * Int64(86400000) +
                 Int64(TimeRec.Hour) * 3600000 + Int64(TimeRec.Minute) * 60000 +
                 Int64(TimeRec.Second) * 1000 + TimeRec.Millisecond;
      SubMs := TimeRec.Microsecond * 1000 + TimeRec.Nanosecond;
      Result := TGocciaTemporalInstantValue.Create(EpochMs, SubMs);
    end
    else
    begin
      ThrowTypeError('Temporal.Instant.compare requires Instant arguments');
      Result := nil;
    end;
  end;

begin
  I1 := CoerceInst(Args.GetElement(0));
  I2 := CoerceInst(Args.GetElement(1));

  if I1.EpochMilliseconds < I2.EpochMilliseconds then
    Result := TGocciaNumberLiteralValue.Create(-1)
  else if I1.EpochMilliseconds > I2.EpochMilliseconds then
    Result := TGocciaNumberLiteralValue.Create(1)
  else if I1.SubMillisecondNanoseconds < I2.SubMillisecondNanoseconds then
    Result := TGocciaNumberLiteralValue.Create(-1)
  else if I1.SubMillisecondNanoseconds > I2.SubMillisecondNanoseconds then
    Result := TGocciaNumberLiteralValue.Create(1)
  else
    Result := TGocciaNumberLiteralValue.ZeroValue;
end;

{ PlainDate }

procedure TGocciaTemporalBuiltin.RegisterPlainDate;
var
  Ctor: TGocciaNativeFunctionValue;
begin
  Ctor := TGocciaNativeFunctionValue.Create(PlainDateConstructorFn, 'PlainDate', 3);
  TGocciaTemporalPlainDateValue.ExposePrototype(Ctor);
  Ctor.AssignProperty('from',
    TGocciaNativeFunctionValue.CreateWithoutPrototype(PlainDateFrom, 'from', 1));
  Ctor.AssignProperty('compare',
    TGocciaNativeFunctionValue.CreateWithoutPrototype(PlainDateCompare, 'compare', 2));
  FTemporalNamespace.AssignProperty('PlainDate', Ctor);
end;

function TGocciaTemporalBuiltin.PlainDateConstructorFn(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
begin
  if Args.Length < 3 then
    ThrowTypeError('Temporal.PlainDate requires year, month, day arguments');

  Result := TGocciaTemporalPlainDateValue.Create(
    Trunc(Args.GetElement(0).ToNumberLiteral.Value),
    Trunc(Args.GetElement(1).ToNumberLiteral.Value),
    Trunc(Args.GetElement(2).ToNumberLiteral.Value));
end;

function TGocciaTemporalBuiltin.PlainDateFrom(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  Arg: TGocciaValue;
  D: TGocciaTemporalPlainDateValue;
  DateRec: TTemporalDateRecord;
  TimeRec: TTemporalTimeRecord;
  Obj: TGocciaObjectValue;
begin
  Arg := Args.GetElement(0);

  if Arg is TGocciaTemporalPlainDateValue then
  begin
    D := TGocciaTemporalPlainDateValue(Arg);
    Result := TGocciaTemporalPlainDateValue.Create(D.Year, D.Month, D.Day);
  end
  else if Arg is TGocciaStringLiteralValue then
  begin
    if TryParseISODate(TGocciaStringLiteralValue(Arg).Value, DateRec) then
      Result := TGocciaTemporalPlainDateValue.Create(DateRec.Year, DateRec.Month, DateRec.Day)
    else if TryParseISODateTime(TGocciaStringLiteralValue(Arg).Value, DateRec, TimeRec) then
      Result := TGocciaTemporalPlainDateValue.Create(DateRec.Year, DateRec.Month, DateRec.Day)
    else
      ThrowTypeError('Invalid ISO date string');
  end
  else if Arg is TGocciaObjectValue then
  begin
    Obj := TGocciaObjectValue(Arg);
    Result := TGocciaTemporalPlainDateValue.Create(
      Trunc(Obj.GetProperty('year').ToNumberLiteral.Value),
      Trunc(Obj.GetProperty('month').ToNumberLiteral.Value),
      Trunc(Obj.GetProperty('day').ToNumberLiteral.Value));
  end
  else
  begin
    ThrowTypeError('Temporal.PlainDate.from requires a string, PlainDate, or object');
    Result := nil;
  end;
end;

function TGocciaTemporalBuiltin.PlainDateCompare(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  D1, D2: TGocciaTemporalPlainDateValue;

  function CoerceDate(AArg: TGocciaValue): TGocciaTemporalPlainDateValue;
  var
    DateRec: TTemporalDateRecord;
    TimeRec: TTemporalTimeRecord;
  begin
    if AArg is TGocciaTemporalPlainDateValue then
      Result := TGocciaTemporalPlainDateValue(AArg)
    else if AArg is TGocciaStringLiteralValue then
    begin
      if TryParseISODate(TGocciaStringLiteralValue(AArg).Value, DateRec) then
        Result := TGocciaTemporalPlainDateValue.Create(DateRec.Year, DateRec.Month, DateRec.Day)
      else if TryParseISODateTime(TGocciaStringLiteralValue(AArg).Value, DateRec, TimeRec) then
        Result := TGocciaTemporalPlainDateValue.Create(DateRec.Year, DateRec.Month, DateRec.Day)
      else
      begin
        ThrowTypeError('Invalid ISO date string');
        Result := nil;
      end;
    end
    else
    begin
      ThrowTypeError('Temporal.PlainDate.compare requires PlainDate arguments');
      Result := nil;
    end;
  end;

begin
  D1 := CoerceDate(Args.GetElement(0));
  D2 := CoerceDate(Args.GetElement(1));

  Result := TGocciaNumberLiteralValue.Create(
    CompareDates(D1.Year, D1.Month, D1.Day, D2.Year, D2.Month, D2.Day));
end;

{ PlainTime }

procedure TGocciaTemporalBuiltin.RegisterPlainTime;
var
  Ctor: TGocciaNativeFunctionValue;
begin
  Ctor := TGocciaNativeFunctionValue.Create(PlainTimeConstructorFn, 'PlainTime', 0);
  TGocciaTemporalPlainTimeValue.ExposePrototype(Ctor);
  Ctor.AssignProperty('from',
    TGocciaNativeFunctionValue.CreateWithoutPrototype(PlainTimeFrom, 'from', 1));
  Ctor.AssignProperty('compare',
    TGocciaNativeFunctionValue.CreateWithoutPrototype(PlainTimeCompare, 'compare', 2));
  FTemporalNamespace.AssignProperty('PlainTime', Ctor);
end;

function TGocciaTemporalBuiltin.PlainTimeConstructorFn(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;

  function GetArgOr(AIndex: Integer; ADefault: Integer): Integer;
  var
    V: TGocciaValue;
  begin
    if AIndex < Args.Length then
    begin
      V := Args.GetElement(AIndex);
      if V is TGocciaUndefinedLiteralValue then
        Result := ADefault
      else
        Result := Trunc(V.ToNumberLiteral.Value);
    end
    else
      Result := ADefault;
  end;

begin
  Result := TGocciaTemporalPlainTimeValue.Create(
    GetArgOr(0, 0), GetArgOr(1, 0), GetArgOr(2, 0),
    GetArgOr(3, 0), GetArgOr(4, 0), GetArgOr(5, 0));
end;

function TGocciaTemporalBuiltin.PlainTimeFrom(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  Arg: TGocciaValue;
  T: TGocciaTemporalPlainTimeValue;
  TimeRec: TTemporalTimeRecord;
  Obj: TGocciaObjectValue;
  V: TGocciaValue;
  H, Mi, S, Ms, Us, Ns: Integer;
begin
  Arg := Args.GetElement(0);

  if Arg is TGocciaTemporalPlainTimeValue then
  begin
    T := TGocciaTemporalPlainTimeValue(Arg);
    Result := TGocciaTemporalPlainTimeValue.Create(T.Hour, T.Minute, T.Second,
      T.Millisecond, T.Microsecond, T.Nanosecond);
  end
  else if Arg is TGocciaStringLiteralValue then
  begin
    if not TryParseISOTime(TGocciaStringLiteralValue(Arg).Value, TimeRec) then
      ThrowTypeError('Invalid ISO time string');
    Result := TGocciaTemporalPlainTimeValue.Create(
      TimeRec.Hour, TimeRec.Minute, TimeRec.Second,
      TimeRec.Millisecond, TimeRec.Microsecond, TimeRec.Nanosecond);
  end
  else if Arg is TGocciaObjectValue then
  begin
    Obj := TGocciaObjectValue(Arg);
    H := 0; Mi := 0; S := 0; Ms := 0; Us := 0; Ns := 0;
    V := Obj.GetProperty('hour');
    if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then H := Trunc(V.ToNumberLiteral.Value);
    V := Obj.GetProperty('minute');
    if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then Mi := Trunc(V.ToNumberLiteral.Value);
    V := Obj.GetProperty('second');
    if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then S := Trunc(V.ToNumberLiteral.Value);
    V := Obj.GetProperty('millisecond');
    if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then Ms := Trunc(V.ToNumberLiteral.Value);
    V := Obj.GetProperty('microsecond');
    if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then Us := Trunc(V.ToNumberLiteral.Value);
    V := Obj.GetProperty('nanosecond');
    if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then Ns := Trunc(V.ToNumberLiteral.Value);
    Result := TGocciaTemporalPlainTimeValue.Create(H, Mi, S, Ms, Us, Ns);
  end
  else
  begin
    ThrowTypeError('Temporal.PlainTime.from requires a string, PlainTime, or object');
    Result := nil;
  end;
end;

function TGocciaTemporalBuiltin.PlainTimeCompare(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  T1, T2: TGocciaTemporalPlainTimeValue;

  function CoerceTime(AArg: TGocciaValue): TGocciaTemporalPlainTimeValue;
  var
    TimeRec: TTemporalTimeRecord;
  begin
    if AArg is TGocciaTemporalPlainTimeValue then
      Result := TGocciaTemporalPlainTimeValue(AArg)
    else if AArg is TGocciaStringLiteralValue then
    begin
      if not TryParseISOTime(TGocciaStringLiteralValue(AArg).Value, TimeRec) then
      begin
        ThrowTypeError('Invalid ISO time string');
        Result := nil;
      end
      else
        Result := TGocciaTemporalPlainTimeValue.Create(
          TimeRec.Hour, TimeRec.Minute, TimeRec.Second,
          TimeRec.Millisecond, TimeRec.Microsecond, TimeRec.Nanosecond);
    end
    else
    begin
      ThrowTypeError('Temporal.PlainTime.compare requires PlainTime arguments');
      Result := nil;
    end;
  end;

begin
  T1 := CoerceTime(Args.GetElement(0));
  T2 := CoerceTime(Args.GetElement(1));

  Result := TGocciaNumberLiteralValue.Create(
    CompareTimes(T1.Hour, T1.Minute, T1.Second, T1.Millisecond, T1.Microsecond, T1.Nanosecond,
                 T2.Hour, T2.Minute, T2.Second, T2.Millisecond, T2.Microsecond, T2.Nanosecond));
end;

{ PlainDateTime }

procedure TGocciaTemporalBuiltin.RegisterPlainDateTime;
var
  Ctor: TGocciaNativeFunctionValue;
begin
  Ctor := TGocciaNativeFunctionValue.Create(PlainDateTimeConstructorFn, 'PlainDateTime', 3);
  TGocciaTemporalPlainDateTimeValue.ExposePrototype(Ctor);
  Ctor.AssignProperty('from',
    TGocciaNativeFunctionValue.CreateWithoutPrototype(PlainDateTimeFrom, 'from', 1));
  Ctor.AssignProperty('compare',
    TGocciaNativeFunctionValue.CreateWithoutPrototype(PlainDateTimeCompare, 'compare', 2));
  FTemporalNamespace.AssignProperty('PlainDateTime', Ctor);
end;

function TGocciaTemporalBuiltin.PlainDateTimeConstructorFn(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;

  function GetArgOr(AIndex: Integer; ADefault: Integer): Integer;
  var
    V: TGocciaValue;
  begin
    if AIndex < Args.Length then
    begin
      V := Args.GetElement(AIndex);
      if V is TGocciaUndefinedLiteralValue then
        Result := ADefault
      else
        Result := Trunc(V.ToNumberLiteral.Value);
    end
    else
      Result := ADefault;
  end;

begin
  if Args.Length < 3 then
    ThrowTypeError('Temporal.PlainDateTime requires at least year, month, day arguments');

  Result := TGocciaTemporalPlainDateTimeValue.Create(
    GetArgOr(0, 0), GetArgOr(1, 0), GetArgOr(2, 0),
    GetArgOr(3, 0), GetArgOr(4, 0), GetArgOr(5, 0),
    GetArgOr(6, 0), GetArgOr(7, 0), GetArgOr(8, 0));
end;

function TGocciaTemporalBuiltin.PlainDateTimeFrom(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  Arg: TGocciaValue;
  DT: TGocciaTemporalPlainDateTimeValue;
  DateRec: TTemporalDateRecord;
  TimeRec: TTemporalTimeRecord;
  Obj: TGocciaObjectValue;
  V: TGocciaValue;
  Y, Mo, D, H, Mi, S, Ms, Us, Ns: Integer;
begin
  Arg := Args.GetElement(0);

  if Arg is TGocciaTemporalPlainDateTimeValue then
  begin
    DT := TGocciaTemporalPlainDateTimeValue(Arg);
    Result := TGocciaTemporalPlainDateTimeValue.Create(DT.Year, DT.Month, DT.Day,
      DT.Hour, DT.Minute, DT.Second, DT.Millisecond, DT.Microsecond, DT.Nanosecond);
  end
  else if Arg is TGocciaStringLiteralValue then
  begin
    if not TryParseISODateTime(TGocciaStringLiteralValue(Arg).Value, DateRec, TimeRec) then
      ThrowTypeError('Invalid ISO date-time string');
    Result := TGocciaTemporalPlainDateTimeValue.Create(
      DateRec.Year, DateRec.Month, DateRec.Day,
      TimeRec.Hour, TimeRec.Minute, TimeRec.Second,
      TimeRec.Millisecond, TimeRec.Microsecond, TimeRec.Nanosecond);
  end
  else if Arg is TGocciaObjectValue then
  begin
    Obj := TGocciaObjectValue(Arg);
    Y := 0; Mo := 0; D := 0; H := 0; Mi := 0; S := 0; Ms := 0; Us := 0; Ns := 0;
    V := Obj.GetProperty('year'); if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then Y := Trunc(V.ToNumberLiteral.Value);
    V := Obj.GetProperty('month'); if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then Mo := Trunc(V.ToNumberLiteral.Value);
    V := Obj.GetProperty('day'); if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then D := Trunc(V.ToNumberLiteral.Value);
    V := Obj.GetProperty('hour'); if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then H := Trunc(V.ToNumberLiteral.Value);
    V := Obj.GetProperty('minute'); if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then Mi := Trunc(V.ToNumberLiteral.Value);
    V := Obj.GetProperty('second'); if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then S := Trunc(V.ToNumberLiteral.Value);
    V := Obj.GetProperty('millisecond'); if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then Ms := Trunc(V.ToNumberLiteral.Value);
    V := Obj.GetProperty('microsecond'); if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then Us := Trunc(V.ToNumberLiteral.Value);
    V := Obj.GetProperty('nanosecond'); if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then Ns := Trunc(V.ToNumberLiteral.Value);
    Result := TGocciaTemporalPlainDateTimeValue.Create(Y, Mo, D, H, Mi, S, Ms, Us, Ns);
  end
  else
  begin
    ThrowTypeError('Temporal.PlainDateTime.from requires a string, PlainDateTime, or object');
    Result := nil;
  end;
end;

function TGocciaTemporalBuiltin.PlainDateTimeCompare(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  DT1, DT2: TGocciaTemporalPlainDateTimeValue;
  Cmp: Integer;

  function CoerceDT(AArg: TGocciaValue): TGocciaTemporalPlainDateTimeValue;
  var
    DateRec: TTemporalDateRecord;
    TimeRec: TTemporalTimeRecord;
  begin
    if AArg is TGocciaTemporalPlainDateTimeValue then
      Result := TGocciaTemporalPlainDateTimeValue(AArg)
    else if AArg is TGocciaStringLiteralValue then
    begin
      if not TryParseISODateTime(TGocciaStringLiteralValue(AArg).Value, DateRec, TimeRec) then
      begin
        ThrowTypeError('Invalid ISO date-time string');
        Result := nil;
      end
      else
        Result := TGocciaTemporalPlainDateTimeValue.Create(
          DateRec.Year, DateRec.Month, DateRec.Day,
          TimeRec.Hour, TimeRec.Minute, TimeRec.Second,
          TimeRec.Millisecond, TimeRec.Microsecond, TimeRec.Nanosecond);
    end
    else
    begin
      ThrowTypeError('Temporal.PlainDateTime.compare requires PlainDateTime arguments');
      Result := nil;
    end;
  end;

begin
  DT1 := CoerceDT(Args.GetElement(0));
  DT2 := CoerceDT(Args.GetElement(1));

  Cmp := CompareDates(DT1.Year, DT1.Month, DT1.Day, DT2.Year, DT2.Month, DT2.Day);
  if Cmp = 0 then
    Cmp := CompareTimes(DT1.Hour, DT1.Minute, DT1.Second, DT1.Millisecond, DT1.Microsecond, DT1.Nanosecond,
                        DT2.Hour, DT2.Minute, DT2.Second, DT2.Millisecond, DT2.Microsecond, DT2.Nanosecond);

  Result := TGocciaNumberLiteralValue.Create(Cmp);
end;

{ Temporal.Now }

procedure TGocciaTemporalBuiltin.RegisterNow;
var
  NowObj: TGocciaObjectValue;
begin
  NowObj := TGocciaObjectValue.Create;
  NowObj.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(NowInstant, 'instant', 0));
  NowObj.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(NowPlainDateISO, 'plainDateISO', 0));
  NowObj.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(NowPlainTimeISO, 'plainTimeISO', 0));
  NowObj.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(NowPlainDateTimeISO, 'plainDateTimeISO', 0));
  FTemporalNamespace.AssignProperty('Now', NowObj);
end;

function TGocciaTemporalBuiltin.NowInstant(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  EpochNs: Int64;
begin
  EpochNs := GetEpochNanoseconds;
  Result := TGocciaTemporalInstantValue.Create(EpochNs div 1000000, Integer(EpochNs mod 1000000));
end;

function TGocciaTemporalBuiltin.NowPlainDateISO(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  Y, M, D: Word;
begin
  DecodeDate(Now, Y, M, D);
  Result := TGocciaTemporalPlainDateValue.Create(Y, M, D);
end;

function TGocciaTemporalBuiltin.NowPlainTimeISO(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  H, Mi, S, Ms: Word;
begin
  DecodeTime(Now, H, Mi, S, Ms);
  Result := TGocciaTemporalPlainTimeValue.Create(H, Mi, S, Ms, 0, 0);
end;

function TGocciaTemporalBuiltin.NowPlainDateTimeISO(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  Y, Mo, D, H, Mi, S, Ms: Word;
begin
  DecodeDate(Now, Y, Mo, D);
  DecodeTime(Now, H, Mi, S, Ms);
  Result := TGocciaTemporalPlainDateTimeValue.Create(Y, Mo, D, H, Mi, S, Ms, 0, 0);
end;

end.
