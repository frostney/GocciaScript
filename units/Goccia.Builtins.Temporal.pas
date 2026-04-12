unit Goccia.Builtins.Temporal;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.Builtins.Base,
  Goccia.Error.ThrowErrorCallback,
  Goccia.ObjectModel,
  Goccia.Scope,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaTemporalBuiltin = class(TGocciaBuiltin)
  private
    FTemporalNamespace: TGocciaObjectValue;

    // Duration constructor + statics
    function DurationConstructorFn(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DurationFrom(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DurationCompare(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;

    // Instant constructor + statics
    function InstantConstructorFn(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function InstantFrom(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function InstantFromEpochMilliseconds(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function InstantFromEpochNanoseconds(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function InstantCompare(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;

    // PlainDate constructor + statics
    function PlainDateConstructorFn(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function PlainDateFrom(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function PlainDateCompare(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;

    // PlainTime constructor + statics
    function PlainTimeConstructorFn(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function PlainTimeFrom(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function PlainTimeCompare(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;

    // PlainDateTime constructor + statics
    function PlainDateTimeConstructorFn(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function PlainDateTimeFrom(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function PlainDateTimeCompare(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;

    // PlainYearMonth constructor + statics
    function PlainYearMonthConstructorFn(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function PlainYearMonthFrom(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function PlainYearMonthCompare(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;

    // PlainMonthDay constructor + statics
    function PlainMonthDayConstructorFn(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function PlainMonthDayFrom(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function PlainMonthDayCompare(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;

    // ZonedDateTime constructor + statics
    function ZonedDateTimeConstructorFn(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ZonedDateTimeFrom(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ZonedDateTimeCompare(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;

    // Temporal.Now
    function NowInstant(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function NowPlainDateISO(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function NowPlainTimeISO(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function NowPlainDateTimeISO(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function NowTimeZoneId(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function NowZonedDateTimeISO(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;

    procedure RegisterDuration;
    procedure RegisterInstant;
    procedure RegisterPlainDate;
    procedure RegisterPlainTime;
    procedure RegisterPlainDateTime;
    procedure RegisterPlainYearMonth;
    procedure RegisterPlainMonthDay;
    procedure RegisterZonedDateTime;
    procedure RegisterNow;
  public
    constructor Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
  end;

implementation

uses
  DateUtils,
  SysUtils,

  TimingUtils,

  Goccia.Constants.PropertyNames,
  Goccia.GarbageCollector,
  Goccia.Temporal.Options,
  Goccia.Temporal.TimeZone,
  Goccia.Temporal.Utils,
  Goccia.Values.ErrorHelper,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.SymbolValue,
  Goccia.Values.TemporalDuration,
  Goccia.Values.TemporalInstant,
  Goccia.Values.TemporalPlainDate,
  Goccia.Values.TemporalPlainDateTime,
  Goccia.Values.TemporalPlainMonthDay,
  Goccia.Values.TemporalPlainTime,
  Goccia.Values.TemporalPlainYearMonth,
  Goccia.Values.TemporalZonedDateTime;

{ TGocciaTemporalBuiltin }

constructor TGocciaTemporalBuiltin.Create(const AName: string; const AScope: TGocciaScope;
  const AThrowError: TGocciaThrowErrorCallback);
var
  TemporalMembers: array[0..0] of TGocciaMemberDefinition;
begin
  inherited Create(AName, AScope, AThrowError);

  FTemporalNamespace := TGocciaObjectValue.Create(TGocciaObjectValue.SharedObjectPrototype);
  TGarbageCollector.Instance.AddTempRoot(FTemporalNamespace);
  try
    RegisterDuration;
    RegisterInstant;
    RegisterPlainDate;
    RegisterPlainTime;
    RegisterPlainDateTime;
    RegisterPlainYearMonth;
    RegisterPlainMonthDay;
    RegisterZonedDateTime;
    RegisterNow;

    TemporalMembers[0] := DefineSymbolDataProperty(
      TGocciaSymbolValue.WellKnownToStringTag,
      TGocciaStringLiteralValue.Create('Temporal'),
      [pfConfigurable]);
    RegisterMemberDefinitions(FTemporalNamespace, TemporalMembers);

    AScope.DefineLexicalBinding(AName, FTemporalNamespace, dtLet);
  finally
    TGarbageCollector.Instance.RemoveTempRoot(FTemporalNamespace);
  end;
end;

{ Duration }

procedure TGocciaTemporalBuiltin.RegisterDuration;
var
  ConstructorMethod: TGocciaNativeFunctionValue;
begin
  ConstructorMethod := TGocciaNativeFunctionValue.Create(DurationConstructorFn, 'Duration', 0);
  TGocciaTemporalDurationValue.ExposePrototype(ConstructorMethod);
  ConstructorMethod.AssignProperty('from',
    TGocciaNativeFunctionValue.CreateWithoutPrototype(DurationFrom, 'from', 1));
  ConstructorMethod.AssignProperty('compare',
    TGocciaNativeFunctionValue.CreateWithoutPrototype(DurationCompare, 'compare', 2));
  FTemporalNamespace.AssignProperty('Duration', ConstructorMethod);
end;

function TGocciaTemporalBuiltin.DurationConstructorFn(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Y, Mo, W, D, H, Mi, S, Ms, Us, Ns: Int64;

  function GetArgOr(const AIndex: Integer; const ADefault: Int64): Int64;
  var
    V: TGocciaValue;
  begin
    if AIndex < AArgs.Length then
    begin
      V := AArgs.GetElement(AIndex);
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

function TGocciaTemporalBuiltin.DurationFrom(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Arg: TGocciaValue;
  D: TGocciaTemporalDurationValue;
  DurRec: TTemporalDurationRecord;
  Obj: TGocciaObjectValue;

  function GetFieldOr(const AName: string; const ADefault: Int64): Int64;
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
  Arg := AArgs.GetElement(0);

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

function TGocciaTemporalBuiltin.DurationCompare(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D1, D2: TGocciaTemporalDurationValue;
  TotalNs1, TotalNs2: Double;

  function CoerceDuration(const AArg: TGocciaValue): TGocciaTemporalDurationValue;
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
  D1 := CoerceDuration(AArgs.GetElement(0));
  D2 := CoerceDuration(AArgs.GetElement(1));

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
  ConstructorMethod: TGocciaNativeFunctionValue;
begin
  ConstructorMethod := TGocciaNativeFunctionValue.Create(InstantConstructorFn, 'Instant', 1);
  TGocciaTemporalInstantValue.ExposePrototype(ConstructorMethod);
  ConstructorMethod.AssignProperty('from',
    TGocciaNativeFunctionValue.CreateWithoutPrototype(InstantFrom, 'from', 1));
  ConstructorMethod.AssignProperty('fromEpochMilliseconds',
    TGocciaNativeFunctionValue.CreateWithoutPrototype(InstantFromEpochMilliseconds, 'fromEpochMilliseconds', 1));
  ConstructorMethod.AssignProperty('fromEpochNanoseconds',
    TGocciaNativeFunctionValue.CreateWithoutPrototype(InstantFromEpochNanoseconds, 'fromEpochNanoseconds', 1));
  ConstructorMethod.AssignProperty('compare',
    TGocciaNativeFunctionValue.CreateWithoutPrototype(InstantCompare, 'compare', 2));
  FTemporalNamespace.AssignProperty('Instant', ConstructorMethod);
end;

function TGocciaTemporalBuiltin.InstantConstructorFn(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  EpochNs, MsFloat: Double;
  Ms: Int64;
  SubMs: Integer;
begin
  if AArgs.Length < 1 then
    ThrowTypeError('Temporal.Instant requires epochNanoseconds argument');

  EpochNs := AArgs.GetElement(0).ToNumberLiteral.Value;
  Ms := Trunc(EpochNs / 1000000);
  // Safe Int64 → Double conversion (FPC 3.2.2 AArch64 bug: Int64 * Double gives wrong results)
  MsFloat := Ms;
  SubMs := Trunc(EpochNs - MsFloat * 1000000.0);

  Result := TGocciaTemporalInstantValue.Create(Ms, SubMs);
end;

function TGocciaTemporalBuiltin.InstantFrom(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Arg: TGocciaValue;
  Inst: TGocciaTemporalInstantValue;
  DateRec: TTemporalDateRecord;
  TimeRec: TTemporalTimeRecord;
  EpochMs: Int64;
  SubMs: Integer;
begin
  Arg := AArgs.GetElement(0);

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

function TGocciaTemporalBuiltin.InstantFromEpochMilliseconds(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  if AArgs.Length < 1 then
    ThrowTypeError('Temporal.Instant.fromEpochMilliseconds requires an argument');
  Result := TGocciaTemporalInstantValue.Create(
    Trunc(AArgs.GetElement(0).ToNumberLiteral.Value), 0);
end;

function TGocciaTemporalBuiltin.InstantFromEpochNanoseconds(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  EpochNs, MsFloat: Double;
  Ms: Int64;
  SubMs: Integer;
begin
  if AArgs.Length < 1 then
    ThrowTypeError('Temporal.Instant.fromEpochNanoseconds requires an argument');
  EpochNs := AArgs.GetElement(0).ToNumberLiteral.Value;
  Ms := Trunc(EpochNs / 1000000);
  // Safe Int64 → Double conversion (FPC 3.2.2 AArch64 bug: Int64 * Double gives wrong results)
  MsFloat := Ms;
  SubMs := Trunc(EpochNs - MsFloat * 1000000.0);
  Result := TGocciaTemporalInstantValue.Create(Ms, SubMs);
end;

function TGocciaTemporalBuiltin.InstantCompare(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  I1, I2: TGocciaTemporalInstantValue;

  function CoerceInst(const AArg: TGocciaValue): TGocciaTemporalInstantValue;
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
  I1 := CoerceInst(AArgs.GetElement(0));
  I2 := CoerceInst(AArgs.GetElement(1));

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
  ConstructorMethod: TGocciaNativeFunctionValue;
begin
  ConstructorMethod := TGocciaNativeFunctionValue.Create(PlainDateConstructorFn, 'PlainDate', 3);
  TGocciaTemporalPlainDateValue.ExposePrototype(ConstructorMethod);
  ConstructorMethod.AssignProperty('from',
    TGocciaNativeFunctionValue.CreateWithoutPrototype(PlainDateFrom, 'from', 1));
  ConstructorMethod.AssignProperty('compare',
    TGocciaNativeFunctionValue.CreateWithoutPrototype(PlainDateCompare, 'compare', 2));
  FTemporalNamespace.AssignProperty('PlainDate', ConstructorMethod);
end;

function TGocciaTemporalBuiltin.PlainDateConstructorFn(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  if AArgs.Length < 3 then
    ThrowTypeError('Temporal.PlainDate requires year, month, day arguments');

  Result := TGocciaTemporalPlainDateValue.Create(
    Trunc(AArgs.GetElement(0).ToNumberLiteral.Value),
    Trunc(AArgs.GetElement(1).ToNumberLiteral.Value),
    Trunc(AArgs.GetElement(2).ToNumberLiteral.Value));
end;

function TGocciaTemporalBuiltin.PlainDateFrom(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Arg, OptionsArg, V: TGocciaValue;
  D: TGocciaTemporalPlainDateValue;
  DateRec: TTemporalDateRecord;
  TimeRec: TTemporalTimeRecord;
  Obj: TGocciaObjectValue;
  Y, Mo, Dy, MaxDay: Integer;
  Overflow: TTemporalOverflow;
begin
  Arg := AArgs.GetElement(0);

  // Parse overflow option from second argument
  Overflow := toConstrain;
  if AArgs.Length >= 2 then
  begin
    OptionsArg := AArgs.GetElement(1);
    if (OptionsArg is TGocciaObjectValue) then
      Overflow := GetOverflowOption(TGocciaObjectValue(OptionsArg));
  end;

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
    V := Obj.GetProperty(PROP_YEAR);
    if (V = nil) or (V is TGocciaUndefinedLiteralValue) then
      ThrowTypeError('Temporal.PlainDate.from requires year, month, day properties');
    Y := Trunc(V.ToNumberLiteral.Value);
    V := Obj.GetProperty(PROP_MONTH);
    if (V = nil) or (V is TGocciaUndefinedLiteralValue) then
      ThrowTypeError('Temporal.PlainDate.from requires year, month, day properties');
    Mo := Trunc(V.ToNumberLiteral.Value);
    if (Mo < 1) or (Mo > 12) then
    begin
      if Overflow = toReject then
        ThrowRangeError('Month ' + IntToStr(Mo) + ' out of range');
      if Mo < 1 then
        Mo := 1
      else
        Mo := 12;
    end;
    V := Obj.GetProperty(PROP_DAY);
    if (V = nil) or (V is TGocciaUndefinedLiteralValue) then
      ThrowTypeError('Temporal.PlainDate.from requires year, month, day properties');
    Dy := Trunc(V.ToNumberLiteral.Value);
    if Dy < 1 then
    begin
      if Overflow = toReject then
        ThrowRangeError('Day ' + IntToStr(Dy) + ' out of range');
      Dy := 1;
    end;

    MaxDay := DaysInMonth(Y, Mo);
    if Dy > MaxDay then
    begin
      if Overflow = toReject then
        ThrowRangeError('Day ' + IntToStr(Dy) + ' out of range for month ' + IntToStr(Mo));
      Dy := MaxDay;
    end;

    Result := TGocciaTemporalPlainDateValue.Create(Y, Mo, Dy);
  end
  else
  begin
    ThrowTypeError('Temporal.PlainDate.from requires a string, PlainDate, or object');
    Result := nil;
  end;
end;

function TGocciaTemporalBuiltin.PlainDateCompare(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D1, D2: TGocciaTemporalPlainDateValue;

  function CoerceDate(const AArg: TGocciaValue): TGocciaTemporalPlainDateValue;
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
  D1 := CoerceDate(AArgs.GetElement(0));
  D2 := CoerceDate(AArgs.GetElement(1));

  Result := TGocciaNumberLiteralValue.Create(
    CompareDates(D1.Year, D1.Month, D1.Day, D2.Year, D2.Month, D2.Day));
end;

{ PlainTime }

procedure TGocciaTemporalBuiltin.RegisterPlainTime;
var
  ConstructorMethod: TGocciaNativeFunctionValue;
begin
  ConstructorMethod := TGocciaNativeFunctionValue.Create(PlainTimeConstructorFn, 'PlainTime', 0);
  TGocciaTemporalPlainTimeValue.ExposePrototype(ConstructorMethod);
  ConstructorMethod.AssignProperty('from',
    TGocciaNativeFunctionValue.CreateWithoutPrototype(PlainTimeFrom, 'from', 1));
  ConstructorMethod.AssignProperty('compare',
    TGocciaNativeFunctionValue.CreateWithoutPrototype(PlainTimeCompare, 'compare', 2));
  FTemporalNamespace.AssignProperty('PlainTime', ConstructorMethod);
end;

function TGocciaTemporalBuiltin.PlainTimeConstructorFn(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;

  function GetArgOr(const AIndex: Integer; const ADefault: Integer): Integer;
  var
    V: TGocciaValue;
  begin
    if AIndex < AArgs.Length then
    begin
      V := AArgs.GetElement(AIndex);
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

function TGocciaTemporalBuiltin.PlainTimeFrom(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Arg: TGocciaValue;
  T: TGocciaTemporalPlainTimeValue;
  TimeRec: TTemporalTimeRecord;
  Obj: TGocciaObjectValue;
  V: TGocciaValue;
  H, Mi, S, Ms, Us, Ns: Integer;
begin
  Arg := AArgs.GetElement(0);

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
    V := Obj.GetProperty(PROP_HOUR);
    if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then H := Trunc(V.ToNumberLiteral.Value);
    V := Obj.GetProperty(PROP_MINUTE);
    if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then Mi := Trunc(V.ToNumberLiteral.Value);
    V := Obj.GetProperty(PROP_SECOND);
    if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then S := Trunc(V.ToNumberLiteral.Value);
    V := Obj.GetProperty(PROP_MILLISECOND);
    if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then Ms := Trunc(V.ToNumberLiteral.Value);
    V := Obj.GetProperty(PROP_MICROSECOND);
    if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then Us := Trunc(V.ToNumberLiteral.Value);
    V := Obj.GetProperty(PROP_NANOSECOND);
    if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then Ns := Trunc(V.ToNumberLiteral.Value);
    Result := TGocciaTemporalPlainTimeValue.Create(H, Mi, S, Ms, Us, Ns);
  end
  else
  begin
    ThrowTypeError('Temporal.PlainTime.from requires a string, PlainTime, or object');
    Result := nil;
  end;
end;

function TGocciaTemporalBuiltin.PlainTimeCompare(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  T1, T2: TGocciaTemporalPlainTimeValue;

  function CoerceTime(const AArg: TGocciaValue): TGocciaTemporalPlainTimeValue;
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
  T1 := CoerceTime(AArgs.GetElement(0));
  T2 := CoerceTime(AArgs.GetElement(1));

  Result := TGocciaNumberLiteralValue.Create(
    CompareTimes(T1.Hour, T1.Minute, T1.Second, T1.Millisecond, T1.Microsecond, T1.Nanosecond,
                 T2.Hour, T2.Minute, T2.Second, T2.Millisecond, T2.Microsecond, T2.Nanosecond));
end;

{ PlainDateTime }

procedure TGocciaTemporalBuiltin.RegisterPlainDateTime;
var
  ConstructorMethod: TGocciaNativeFunctionValue;
begin
  ConstructorMethod := TGocciaNativeFunctionValue.Create(PlainDateTimeConstructorFn, 'PlainDateTime', 3);
  TGocciaTemporalPlainDateTimeValue.ExposePrototype(ConstructorMethod);
  ConstructorMethod.AssignProperty('from',
    TGocciaNativeFunctionValue.CreateWithoutPrototype(PlainDateTimeFrom, 'from', 1));
  ConstructorMethod.AssignProperty('compare',
    TGocciaNativeFunctionValue.CreateWithoutPrototype(PlainDateTimeCompare, 'compare', 2));
  FTemporalNamespace.AssignProperty('PlainDateTime', ConstructorMethod);
end;

function TGocciaTemporalBuiltin.PlainDateTimeConstructorFn(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;

  function GetArgOr(const AIndex: Integer; const ADefault: Integer): Integer;
  var
    V: TGocciaValue;
  begin
    if AIndex < AArgs.Length then
    begin
      V := AArgs.GetElement(AIndex);
      if V is TGocciaUndefinedLiteralValue then
        Result := ADefault
      else
        Result := Trunc(V.ToNumberLiteral.Value);
    end
    else
      Result := ADefault;
  end;

begin
  if AArgs.Length < 3 then
    ThrowTypeError('Temporal.PlainDateTime requires at least year, month, day arguments');

  Result := TGocciaTemporalPlainDateTimeValue.Create(
    GetArgOr(0, 0), GetArgOr(1, 0), GetArgOr(2, 0),
    GetArgOr(3, 0), GetArgOr(4, 0), GetArgOr(5, 0),
    GetArgOr(6, 0), GetArgOr(7, 0), GetArgOr(8, 0));
end;

function TGocciaTemporalBuiltin.PlainDateTimeFrom(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Arg: TGocciaValue;
  DT: TGocciaTemporalPlainDateTimeValue;
  DateRec: TTemporalDateRecord;
  TimeRec: TTemporalTimeRecord;
  Obj: TGocciaObjectValue;
  V: TGocciaValue;
  Y, Mo, D, H, Mi, S, Ms, Us, Ns: Integer;
begin
  Arg := AArgs.GetElement(0);

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
    V := Obj.GetProperty(PROP_YEAR); if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then Y := Trunc(V.ToNumberLiteral.Value);
    V := Obj.GetProperty(PROP_MONTH); if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then Mo := Trunc(V.ToNumberLiteral.Value);
    V := Obj.GetProperty(PROP_DAY); if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then D := Trunc(V.ToNumberLiteral.Value);
    V := Obj.GetProperty(PROP_HOUR); if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then H := Trunc(V.ToNumberLiteral.Value);
    V := Obj.GetProperty(PROP_MINUTE); if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then Mi := Trunc(V.ToNumberLiteral.Value);
    V := Obj.GetProperty(PROP_SECOND); if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then S := Trunc(V.ToNumberLiteral.Value);
    V := Obj.GetProperty(PROP_MILLISECOND); if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then Ms := Trunc(V.ToNumberLiteral.Value);
    V := Obj.GetProperty(PROP_MICROSECOND); if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then Us := Trunc(V.ToNumberLiteral.Value);
    V := Obj.GetProperty(PROP_NANOSECOND); if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then Ns := Trunc(V.ToNumberLiteral.Value);
    Result := TGocciaTemporalPlainDateTimeValue.Create(Y, Mo, D, H, Mi, S, Ms, Us, Ns);
  end
  else
  begin
    ThrowTypeError('Temporal.PlainDateTime.from requires a string, PlainDateTime, or object');
    Result := nil;
  end;
end;

function TGocciaTemporalBuiltin.PlainDateTimeCompare(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  DT1, DT2: TGocciaTemporalPlainDateTimeValue;
  Cmp: Integer;

  function CoerceDT(const AArg: TGocciaValue): TGocciaTemporalPlainDateTimeValue;
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
  DT1 := CoerceDT(AArgs.GetElement(0));
  DT2 := CoerceDT(AArgs.GetElement(1));

  Cmp := CompareDates(DT1.Year, DT1.Month, DT1.Day, DT2.Year, DT2.Month, DT2.Day);
  if Cmp = 0 then
    Cmp := CompareTimes(DT1.Hour, DT1.Minute, DT1.Second, DT1.Millisecond, DT1.Microsecond, DT1.Nanosecond,
                        DT2.Hour, DT2.Minute, DT2.Second, DT2.Millisecond, DT2.Microsecond, DT2.Nanosecond);

  Result := TGocciaNumberLiteralValue.Create(Cmp);
end;

{ PlainYearMonth }

procedure TGocciaTemporalBuiltin.RegisterPlainYearMonth;
var
  ConstructorMethod: TGocciaNativeFunctionValue;
begin
  ConstructorMethod := TGocciaNativeFunctionValue.Create(PlainYearMonthConstructorFn, 'PlainYearMonth', 2);
  TGocciaTemporalPlainYearMonthValue.ExposePrototype(ConstructorMethod);
  ConstructorMethod.AssignProperty('from',
    TGocciaNativeFunctionValue.CreateWithoutPrototype(PlainYearMonthFrom, 'from', 1));
  ConstructorMethod.AssignProperty('compare',
    TGocciaNativeFunctionValue.CreateWithoutPrototype(PlainYearMonthCompare, 'compare', 2));
  FTemporalNamespace.AssignProperty('PlainYearMonth', ConstructorMethod);
end;

function TGocciaTemporalBuiltin.PlainYearMonthConstructorFn(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  RefDay: Integer;
begin
  if AArgs.Length < 2 then
    ThrowTypeError('Temporal.PlainYearMonth requires year, month arguments');

  RefDay := 1;
  if (AArgs.Length >= 3) and not (AArgs.GetElement(2) is TGocciaUndefinedLiteralValue) then
    RefDay := Trunc(AArgs.GetElement(2).ToNumberLiteral.Value);

  Result := TGocciaTemporalPlainYearMonthValue.Create(
    Trunc(AArgs.GetElement(0).ToNumberLiteral.Value),
    Trunc(AArgs.GetElement(1).ToNumberLiteral.Value),
    RefDay);
end;

function TGocciaTemporalBuiltin.PlainYearMonthFrom(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Arg, V, VMonthCode: TGocciaValue;
  YM: TGocciaTemporalPlainYearMonthValue;
  Y, M, MonthPart: Integer;
  Obj: TGocciaObjectValue;
  MonthCodeStr: string;
begin
  Arg := AArgs.GetElement(0);

  if Arg is TGocciaTemporalPlainYearMonthValue then
  begin
    YM := TGocciaTemporalPlainYearMonthValue(Arg);
    Result := TGocciaTemporalPlainYearMonthValue.Create(YM.Year, YM.Month, YM.ReferenceDay);
  end
  else if Arg is TGocciaStringLiteralValue then
  begin
    if not TryParseISOYearMonth(TGocciaStringLiteralValue(Arg).Value, Y, M) then
      ThrowRangeError('Invalid ISO year-month string');
    Result := TGocciaTemporalPlainYearMonthValue.Create(Y, M);
  end
  else if Arg is TGocciaObjectValue then
  begin
    Obj := TGocciaObjectValue(Arg);
    V := Obj.GetProperty(PROP_YEAR);
    if (V = nil) or (V is TGocciaUndefinedLiteralValue) then
      ThrowTypeError('Temporal.PlainYearMonth.from requires year property');
    Y := Trunc(V.ToNumberLiteral.Value);
    // Support both monthCode and month, consistent with PlainMonthDay.from
    VMonthCode := Obj.GetProperty(PROP_MONTH_CODE);
    if Assigned(VMonthCode) and not (VMonthCode is TGocciaUndefinedLiteralValue) then
    begin
      MonthCodeStr := VMonthCode.ToStringLiteral.Value;
      if (Length(MonthCodeStr) <> 3) or (MonthCodeStr[1] <> 'M') or
         not (MonthCodeStr[2] in ['0'..'9']) or not (MonthCodeStr[3] in ['0'..'9']) then
        ThrowTypeError('Invalid monthCode for Temporal.PlainYearMonth.from');
      if not TryStrToInt(Copy(MonthCodeStr, 2, 2), MonthPart) then
        ThrowTypeError('Invalid monthCode for Temporal.PlainYearMonth.from');
      if (MonthPart < 1) or (MonthPart > 12) then
        ThrowRangeError('monthCode month out of range in Temporal.PlainYearMonth.from');
      // Check consistency if both month and monthCode are provided
      V := Obj.GetProperty(PROP_MONTH);
      if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
        if Trunc(V.ToNumberLiteral.Value) <> MonthPart then
          ThrowRangeError('month and monthCode must match in Temporal.PlainYearMonth.from');
      M := MonthPart;
    end
    else
    begin
      V := Obj.GetProperty(PROP_MONTH);
      if (V = nil) or (V is TGocciaUndefinedLiteralValue) then
        ThrowTypeError('Temporal.PlainYearMonth.from requires monthCode or month property');
      M := Trunc(V.ToNumberLiteral.Value);
    end;
    Result := TGocciaTemporalPlainYearMonthValue.Create(Y, M);
  end
  else
  begin
    ThrowTypeError('Temporal.PlainYearMonth.from requires a string, PlainYearMonth, or object');
    Result := nil;
  end;
end;

function TGocciaTemporalBuiltin.PlainYearMonthCompare(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  YM1, YM2: TGocciaTemporalPlainYearMonthValue;

  function CoerceYearMonth(const AArg: TGocciaValue): TGocciaTemporalPlainYearMonthValue;
  var
    Y, M: Integer;
  begin
    if AArg is TGocciaTemporalPlainYearMonthValue then
      Result := TGocciaTemporalPlainYearMonthValue(AArg)
    else if AArg is TGocciaStringLiteralValue then
    begin
      if not TryParseISOYearMonth(TGocciaStringLiteralValue(AArg).Value, Y, M) then
      begin
        ThrowRangeError('Invalid ISO year-month string');
        Result := nil;
      end
      else
        Result := TGocciaTemporalPlainYearMonthValue.Create(Y, M);
    end
    else
    begin
      ThrowTypeError('Temporal.PlainYearMonth.compare requires PlainYearMonth arguments');
      Result := nil;
    end;
  end;

begin
  YM1 := CoerceYearMonth(AArgs.GetElement(0));
  YM2 := CoerceYearMonth(AArgs.GetElement(1));

  if YM1.Year < YM2.Year then
    Result := TGocciaNumberLiteralValue.Create(-1)
  else if YM1.Year > YM2.Year then
    Result := TGocciaNumberLiteralValue.Create(1)
  else if YM1.Month < YM2.Month then
    Result := TGocciaNumberLiteralValue.Create(-1)
  else if YM1.Month > YM2.Month then
    Result := TGocciaNumberLiteralValue.Create(1)
  else
    Result := TGocciaNumberLiteralValue.ZeroValue;
end;

{ PlainMonthDay }

procedure TGocciaTemporalBuiltin.RegisterPlainMonthDay;
var
  ConstructorMethod: TGocciaNativeFunctionValue;
begin
  ConstructorMethod := TGocciaNativeFunctionValue.Create(PlainMonthDayConstructorFn, 'PlainMonthDay', 2);
  TGocciaTemporalPlainMonthDayValue.ExposePrototype(ConstructorMethod);
  ConstructorMethod.AssignProperty('from',
    TGocciaNativeFunctionValue.CreateWithoutPrototype(PlainMonthDayFrom, 'from', 1));
  ConstructorMethod.AssignProperty('compare',
    TGocciaNativeFunctionValue.CreateWithoutPrototype(PlainMonthDayCompare, 'compare', 2));
  FTemporalNamespace.AssignProperty('PlainMonthDay', ConstructorMethod);
end;

function TGocciaTemporalBuiltin.PlainMonthDayConstructorFn(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if AArgs.Length < 2 then
    ThrowTypeError('Temporal.PlainMonthDay requires month, day arguments');

  Result := TGocciaTemporalPlainMonthDayValue.Create(
    Trunc(AArgs.GetElement(0).ToNumberLiteral.Value),
    Trunc(AArgs.GetElement(1).ToNumberLiteral.Value));
end;

function TGocciaTemporalBuiltin.PlainMonthDayFrom(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Arg, V: TGocciaValue;
  MD: TGocciaTemporalPlainMonthDayValue;
  Obj: TGocciaObjectValue;
  M, D: Integer;
  MonthCodeStr: string;
begin
  Arg := AArgs.GetElement(0);

  if Arg is TGocciaTemporalPlainMonthDayValue then
  begin
    MD := TGocciaTemporalPlainMonthDayValue(Arg);
    Result := TGocciaTemporalPlainMonthDayValue.Create(MD.Month, MD.Day, MD.ReferenceYear);
  end
  else if Arg is TGocciaStringLiteralValue then
  begin
    if not TryParseISOMonthDay(TGocciaStringLiteralValue(Arg).Value, M, D) then
      ThrowRangeError('Invalid ISO month-day string');
    Result := TGocciaTemporalPlainMonthDayValue.Create(M, D);
  end
  else if Arg is TGocciaObjectValue then
  begin
    Obj := TGocciaObjectValue(Arg);
    V := Obj.GetProperty(PROP_DAY);
    if (V = nil) or (V is TGocciaUndefinedLiteralValue) then
      ThrowTypeError('Temporal.PlainMonthDay.from requires day property');
    D := Trunc(V.ToNumberLiteral.Value);
    // Support both monthCode (e.g., "M07") and month (numeric)
    V := Obj.GetProperty(PROP_MONTH_CODE);
    if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
    begin
      // Parse monthCode — must be canonical format M01..M12
      MonthCodeStr := V.ToStringLiteral.Value;
      if (Length(MonthCodeStr) <> 3) or (MonthCodeStr[1] <> 'M') or
         not (MonthCodeStr[2] in ['0'..'9']) or not (MonthCodeStr[3] in ['0'..'9']) then
        ThrowTypeError('Invalid monthCode for Temporal.PlainMonthDay.from');
      if not TryStrToInt(Copy(MonthCodeStr, 2, 2), M) then
        ThrowTypeError('Invalid monthCode for Temporal.PlainMonthDay.from');
    end
    else
    begin
      V := Obj.GetProperty(PROP_MONTH);
      if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
        M := Trunc(V.ToNumberLiteral.Value)
      else
        M := 0;
    end;
    if (M < 1) or (M > 12) then
      ThrowTypeError('Temporal.PlainMonthDay.from requires a valid month property');
    Result := TGocciaTemporalPlainMonthDayValue.Create(M, D);
  end
  else
  begin
    ThrowTypeError('Temporal.PlainMonthDay.from requires a string, PlainMonthDay, or object');
    Result := nil;
  end;
end;

function TGocciaTemporalBuiltin.PlainMonthDayCompare(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  MD1, MD2: TGocciaTemporalPlainMonthDayValue;

  function CoerceMD(const AArg: TGocciaValue): TGocciaTemporalPlainMonthDayValue;
  var
    M, D: Integer;
  begin
    if AArg is TGocciaTemporalPlainMonthDayValue then
      Result := TGocciaTemporalPlainMonthDayValue(AArg)
    else if AArg is TGocciaStringLiteralValue then
    begin
      if not TryParseISOMonthDay(TGocciaStringLiteralValue(AArg).Value, M, D) then
      begin
        ThrowRangeError('Invalid ISO month-day string');
        Result := nil;
      end
      else
        Result := TGocciaTemporalPlainMonthDayValue.Create(M, D);
    end
    else
    begin
      ThrowTypeError('Temporal.PlainMonthDay.compare requires PlainMonthDay arguments');
      Result := nil;
    end;
  end;

begin
  MD1 := CoerceMD(AArgs.GetElement(0));
  MD2 := CoerceMD(AArgs.GetElement(1));

  if MD1.Month < MD2.Month then
    Result := TGocciaNumberLiteralValue.Create(-1)
  else if MD1.Month > MD2.Month then
    Result := TGocciaNumberLiteralValue.Create(1)
  else if MD1.Day < MD2.Day then
    Result := TGocciaNumberLiteralValue.Create(-1)
  else if MD1.Day > MD2.Day then
    Result := TGocciaNumberLiteralValue.Create(1)
  else
    Result := TGocciaNumberLiteralValue.ZeroValue;
end;

{ ZonedDateTime }

procedure TGocciaTemporalBuiltin.RegisterZonedDateTime;
var
  ConstructorMethod: TGocciaNativeFunctionValue;
begin
  ConstructorMethod := TGocciaNativeFunctionValue.Create(ZonedDateTimeConstructorFn, 'ZonedDateTime', 2);
  TGocciaTemporalZonedDateTimeValue.ExposePrototype(ConstructorMethod);
  ConstructorMethod.AssignProperty('from',
    TGocciaNativeFunctionValue.CreateWithoutPrototype(ZonedDateTimeFrom, 'from', 1));
  ConstructorMethod.AssignProperty('compare',
    TGocciaNativeFunctionValue.CreateWithoutPrototype(ZonedDateTimeCompare, 'compare', 2));
  FTemporalNamespace.AssignProperty('ZonedDateTime', ConstructorMethod);
end;

// TC39 Temporal §6.1.1 new Temporal.ZonedDateTime(epochNanoseconds, timeZone)
function TGocciaTemporalBuiltin.ZonedDateTimeConstructorFn(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  EpochNs, MsFloat: Double;
  Ms: Int64;
  SubMs: Integer;
  TZ: string;
begin
  if AArgs.Length < 2 then
    ThrowTypeError('Temporal.ZonedDateTime requires epochNanoseconds and timeZone arguments');

  EpochNs := AArgs.GetElement(0).ToNumberLiteral.Value;
  Ms := Trunc(EpochNs / 1000000);
  // Safe Int64 → Double conversion (FPC 3.2.2 AArch64 bug: Int64 * Double gives wrong results)
  MsFloat := Ms;
  SubMs := Trunc(EpochNs - MsFloat * 1000000.0);
  TZ := AArgs.GetElement(1).ToStringLiteral.Value;

  Result := TGocciaTemporalZonedDateTimeValue.Create(Ms, SubMs, TZ);
end;

function TGocciaTemporalBuiltin.ZonedDateTimeFrom(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Arg: TGocciaValue;
  ZDT: TGocciaTemporalZonedDateTimeValue;
  DateRec: TTemporalDateRecord;
  TimeRec: TTemporalTimeRecord;
  TZ: string;
  EpochMs: Int64;
  SubMs, OffsetSeconds: Integer;
begin
  Arg := AArgs.GetElement(0);

  if Arg is TGocciaTemporalZonedDateTimeValue then
  begin
    ZDT := TGocciaTemporalZonedDateTimeValue(Arg);
    Result := TGocciaTemporalZonedDateTimeValue.Create(
      ZDT.EpochMilliseconds, ZDT.SubMillisecondNanoseconds, ZDT.TimeZone);
  end
  else if Arg is TGocciaStringLiteralValue then
  begin
    if not TryParseISODateTimeWithOffset(TGocciaStringLiteralValue(Arg).Value,
        DateRec, TimeRec, OffsetSeconds, TZ) then
      ThrowRangeError('Invalid ISO zoned date-time string');

    // Compute UTC epoch from wall-clock time
    EpochMs := DateToEpochDays(DateRec.Year, DateRec.Month, DateRec.Day) * Int64(86400000) +
               Int64(TimeRec.Hour) * 3600000 + Int64(TimeRec.Minute) * 60000 +
               Int64(TimeRec.Second) * 1000 + TimeRec.Millisecond;
    SubMs := TimeRec.Microsecond * 1000 + TimeRec.Nanosecond;

    // Apply offset to convert wall-clock to UTC
    EpochMs := EpochMs - Int64(OffsetSeconds) * 1000;

    if TZ = '' then
      ThrowRangeError('Temporal.ZonedDateTime.from requires a timezone annotation (e.g., [UTC])');

    Result := TGocciaTemporalZonedDateTimeValue.Create(EpochMs, SubMs, TZ);
  end
  else
  begin
    ThrowTypeError('Temporal.ZonedDateTime.from requires a string or ZonedDateTime');
    Result := nil;
  end;
end;

function TGocciaTemporalBuiltin.ZonedDateTimeCompare(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Z1, Z2: TGocciaTemporalZonedDateTimeValue;

  function CoerceZDT(const AArg: TGocciaValue): TGocciaTemporalZonedDateTimeValue;
  var
    CoerceDateRec: TTemporalDateRecord;
    CoerceTimeRec: TTemporalTimeRecord;
    CoerceOffsetSeconds: Integer;
    CoerceTZ: string;
    CoerceEpochMs: Int64;
    CoerceSubMs: Integer;
  begin
    if AArg is TGocciaTemporalZonedDateTimeValue then
      Result := TGocciaTemporalZonedDateTimeValue(AArg)
    else if AArg is TGocciaStringLiteralValue then
    begin
      if not TryParseISODateTimeWithOffset(TGocciaStringLiteralValue(AArg).Value,
          CoerceDateRec, CoerceTimeRec, CoerceOffsetSeconds, CoerceTZ) then
        ThrowRangeError('Invalid ISO zoned date-time string');

      CoerceEpochMs := DateToEpochDays(CoerceDateRec.Year, CoerceDateRec.Month, CoerceDateRec.Day) * Int64(86400000) +
                       Int64(CoerceTimeRec.Hour) * 3600000 + Int64(CoerceTimeRec.Minute) * 60000 +
                       Int64(CoerceTimeRec.Second) * 1000 + CoerceTimeRec.Millisecond;
      CoerceSubMs := CoerceTimeRec.Microsecond * 1000 + CoerceTimeRec.Nanosecond;
      CoerceEpochMs := CoerceEpochMs - Int64(CoerceOffsetSeconds) * 1000;

      if CoerceTZ = '' then
        ThrowRangeError('Temporal.ZonedDateTime.compare requires a timezone annotation (e.g., [UTC])');

      Result := TGocciaTemporalZonedDateTimeValue.Create(CoerceEpochMs, CoerceSubMs, CoerceTZ);
    end
    else
    begin
      ThrowTypeError('Temporal.ZonedDateTime.compare requires ZonedDateTime or string arguments');
      Result := nil;
    end;
  end;

begin
  Z1 := CoerceZDT(AArgs.GetElement(0));
  Z2 := CoerceZDT(AArgs.GetElement(1));

  if Z1.EpochMilliseconds < Z2.EpochMilliseconds then
    Result := TGocciaNumberLiteralValue.Create(-1)
  else if Z1.EpochMilliseconds > Z2.EpochMilliseconds then
    Result := TGocciaNumberLiteralValue.Create(1)
  else if Z1.SubMillisecondNanoseconds < Z2.SubMillisecondNanoseconds then
    Result := TGocciaNumberLiteralValue.Create(-1)
  else if Z1.SubMillisecondNanoseconds > Z2.SubMillisecondNanoseconds then
    Result := TGocciaNumberLiteralValue.Create(1)
  else
    Result := TGocciaNumberLiteralValue.ZeroValue;
end;

{ Temporal.Now }

procedure TGocciaTemporalBuiltin.RegisterNow;
var
  NowObj: TGocciaObjectValue;
begin
  NowObj := TGocciaObjectValue.Create(TGocciaObjectValue.SharedObjectPrototype);
  NowObj.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(NowInstant, 'instant', 0));
  NowObj.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(NowPlainDateISO, 'plainDateISO', 0));
  NowObj.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(NowPlainTimeISO, 'plainTimeISO', 0));
  NowObj.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(NowPlainDateTimeISO, 'plainDateTimeISO', 0));
  NowObj.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(NowTimeZoneId, 'timeZoneId', 0));
  NowObj.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(NowZonedDateTimeISO, 'zonedDateTimeISO', 0));
  FTemporalNamespace.AssignProperty('Now', NowObj);
end;

function TGocciaTemporalBuiltin.NowInstant(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  EpochNs: Int64;
begin
  EpochNs := GetEpochNanoseconds;
  Result := TGocciaTemporalInstantValue.Create(EpochNs div 1000000, Integer(EpochNs mod 1000000));
end;

function TGocciaTemporalBuiltin.NowPlainDateISO(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Y, M, D: Word;
begin
  DecodeDate(Now, Y, M, D);
  Result := TGocciaTemporalPlainDateValue.Create(Y, M, D);
end;

function TGocciaTemporalBuiltin.NowPlainTimeISO(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  H, Mi, S, Ms: Word;
begin
  DecodeTime(Now, H, Mi, S, Ms);
  Result := TGocciaTemporalPlainTimeValue.Create(H, Mi, S, Ms, 0, 0);
end;

function TGocciaTemporalBuiltin.NowPlainDateTimeISO(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Y, Mo, D, H, Mi, S, Ms: Word;
begin
  DecodeDate(Now, Y, Mo, D);
  DecodeTime(Now, H, Mi, S, Ms);
  Result := TGocciaTemporalPlainDateTimeValue.Create(Y, Mo, D, H, Mi, S, Ms, 0, 0);
end;

// TC39 Temporal §2.2.1 Temporal.Now.timeZoneId()
function TGocciaTemporalBuiltin.NowTimeZoneId(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaStringLiteralValue.Create(GetSystemTimeZoneId);
end;

// TC39 Temporal §2.2.2 Temporal.Now.zonedDateTimeISO([timeZone])
function TGocciaTemporalBuiltin.NowZonedDateTimeISO(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  EpochNs: Int64;
  TZ: string;
  Arg: TGocciaValue;
begin
  EpochNs := GetEpochNanoseconds;

  if AArgs.Length >= 1 then
  begin
    Arg := AArgs.GetElement(0);
    if not (Arg is TGocciaUndefinedLiteralValue) then
      TZ := Arg.ToStringLiteral.Value
    else
      TZ := GetSystemTimeZoneId;
  end
  else
    TZ := GetSystemTimeZoneId;

  Result := TGocciaTemporalZonedDateTimeValue.Create(
    EpochNs div 1000000, Integer(EpochNs mod 1000000), TZ);
end;

end.
