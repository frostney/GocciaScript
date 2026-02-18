unit Goccia.Values.TemporalPlainTime;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Primitives, Goccia.Values.ObjectValue,
  Goccia.Values.NativeFunction, Goccia.Arguments.Collection,
  Goccia.SharedPrototype, SysUtils;

type
  TGocciaTemporalPlainTimeValue = class(TGocciaObjectValue)
  private
    class var FShared: TGocciaSharedPrototype;
  private
    FHour: Integer;
    FMinute: Integer;
    FSecond: Integer;
    FMillisecond: Integer;
    FMicrosecond: Integer;
    FNanosecond: Integer;

    function GetHour(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function GetMinute(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function GetSecond(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function GetMillisecond(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function GetMicrosecond(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function GetNanosecond(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;

    function TimeWith(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function TimeAdd(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function TimeSubtract(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function TimeUntil(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function TimeSince(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function TimeRound(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function TimeEquals(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function TimeToString(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function TimeToJSON(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function TimeValueOf(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;

    procedure InitializePrototype;
  public
    constructor Create(AHour, AMinute, ASecond, AMillisecond, AMicrosecond, ANanosecond: Integer); overload;

    function ToStringTag: string; override;
    class procedure ExposePrototype(AConstructor: TGocciaObjectValue);

    property Hour: Integer read FHour;
    property Minute: Integer read FMinute;
    property Second: Integer read FSecond;
    property Millisecond: Integer read FMillisecond;
    property Microsecond: Integer read FMicrosecond;
    property Nanosecond: Integer read FNanosecond;
  end;

implementation

uses
  Goccia.Values.ErrorHelper, Goccia.GarbageCollector,
  Goccia.Values.ObjectPropertyDescriptor, Goccia.Temporal.Utils,
  Goccia.Values.TemporalDuration;

function AsPlainTime(AValue: TGocciaValue; const AMethod: string): TGocciaTemporalPlainTimeValue;
begin
  if not (AValue is TGocciaTemporalPlainTimeValue) then
    ThrowTypeError(AMethod + ' called on non-PlainTime');
  Result := TGocciaTemporalPlainTimeValue(AValue);
end;

function CoercePlainTime(AValue: TGocciaValue; const AMethod: string): TGocciaTemporalPlainTimeValue;
var
  TimeRec: TTemporalTimeRecord;
  Obj: TGocciaObjectValue;
  V: TGocciaValue;
  H, Mi, S, Ms, Us, Ns: Integer;
begin
  if AValue is TGocciaTemporalPlainTimeValue then
    Result := TGocciaTemporalPlainTimeValue(AValue)
  else if AValue is TGocciaStringLiteralValue then
  begin
    if not TryParseISOTime(TGocciaStringLiteralValue(AValue).Value, TimeRec) then
      ThrowTypeError('Invalid ISO time string for ' + AMethod);
    Result := TGocciaTemporalPlainTimeValue.Create(
      TimeRec.Hour, TimeRec.Minute, TimeRec.Second,
      TimeRec.Millisecond, TimeRec.Microsecond, TimeRec.Nanosecond);
  end
  else if AValue is TGocciaObjectValue then
  begin
    Obj := TGocciaObjectValue(AValue);
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
    ThrowTypeError(AMethod + ' requires a PlainTime, string, or object');
    Result := nil;
  end;
end;

function TimeToTotalNanoseconds(T: TGocciaTemporalPlainTimeValue): Int64;
begin
  Result := Int64(T.FNanosecond) +
            Int64(T.FMicrosecond) * 1000 +
            Int64(T.FMillisecond) * 1000000 +
            Int64(T.FSecond) * Int64(1000000000) +
            Int64(T.FMinute) * Int64(60000000000) +
            Int64(T.FHour) * Int64(3600000000000);
end;

{ TGocciaTemporalPlainTimeValue }

constructor TGocciaTemporalPlainTimeValue.Create(AHour, AMinute, ASecond, AMillisecond, AMicrosecond, ANanosecond: Integer);
begin
  inherited Create(nil);
  if not IsValidTime(AHour, AMinute, ASecond, AMillisecond, AMicrosecond, ANanosecond) then
    ThrowRangeError('Invalid time');
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

procedure TGocciaTemporalPlainTimeValue.InitializePrototype;
begin
  if Assigned(FShared) then Exit;
  FShared := TGocciaSharedPrototype.Create(Self);

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

  FShared.Prototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(TimeWith, 'with', 1));
  FShared.Prototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(TimeAdd, 'add', 1));
  FShared.Prototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(TimeSubtract, 'subtract', 1));
  FShared.Prototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(TimeUntil, 'until', 1));
  FShared.Prototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(TimeSince, 'since', 1));
  FShared.Prototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(TimeRound, 'round', 1));
  FShared.Prototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(TimeEquals, 'equals', 1));
  FShared.Prototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(TimeToString, 'toString', 0));
  FShared.Prototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(TimeToJSON, 'toJSON', 0));
  FShared.Prototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(TimeValueOf, 'valueOf', 0));
end;

class procedure TGocciaTemporalPlainTimeValue.ExposePrototype(AConstructor: TGocciaObjectValue);
begin
  if not Assigned(FShared) then
    TGocciaTemporalPlainTimeValue.Create(0, 0, 0, 0, 0, 0);
  FShared.ExposeOnConstructor(AConstructor);
end;

function TGocciaTemporalPlainTimeValue.ToStringTag: string;
begin
  Result := 'Temporal.PlainTime';
end;

{ Getters }

function TGocciaTemporalPlainTimeValue.GetHour(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaNumberLiteralValue.Create(AsPlainTime(ThisValue, 'get PlainTime.hour').FHour);
end;

function TGocciaTemporalPlainTimeValue.GetMinute(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaNumberLiteralValue.Create(AsPlainTime(ThisValue, 'get PlainTime.minute').FMinute);
end;

function TGocciaTemporalPlainTimeValue.GetSecond(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaNumberLiteralValue.Create(AsPlainTime(ThisValue, 'get PlainTime.second').FSecond);
end;

function TGocciaTemporalPlainTimeValue.GetMillisecond(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaNumberLiteralValue.Create(AsPlainTime(ThisValue, 'get PlainTime.millisecond').FMillisecond);
end;

function TGocciaTemporalPlainTimeValue.GetMicrosecond(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaNumberLiteralValue.Create(AsPlainTime(ThisValue, 'get PlainTime.microsecond').FMicrosecond);
end;

function TGocciaTemporalPlainTimeValue.GetNanosecond(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaNumberLiteralValue.Create(AsPlainTime(ThisValue, 'get PlainTime.nanosecond').FNanosecond);
end;

{ Methods }

function TGocciaTemporalPlainTimeValue.TimeWith(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  T: TGocciaTemporalPlainTimeValue;
  Obj: TGocciaObjectValue;
  V: TGocciaValue;

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
  T := AsPlainTime(ThisValue, 'PlainTime.prototype.with');
  V := Args.GetElement(0);
  if not (V is TGocciaObjectValue) then
    ThrowTypeError('PlainTime.prototype.with requires an object argument');
  Obj := TGocciaObjectValue(V);

  Result := TGocciaTemporalPlainTimeValue.Create(
    GetFieldOr('hour', T.FHour),
    GetFieldOr('minute', T.FMinute),
    GetFieldOr('second', T.FSecond),
    GetFieldOr('millisecond', T.FMillisecond),
    GetFieldOr('microsecond', T.FMicrosecond),
    GetFieldOr('nanosecond', T.FNanosecond));
end;

function TGocciaTemporalPlainTimeValue.TimeAdd(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  T: TGocciaTemporalPlainTimeValue;
  Dur: TGocciaTemporalDurationValue;
  Arg: TGocciaValue;
  DurRec: TTemporalDurationRecord;
  ExtraDays: Int64;
  Balanced: TTemporalTimeRecord;
  Obj: TGocciaObjectValue;
  VH: TGocciaValue;
  H, Mi, S, Ms, Us, Ns: Int64;
begin
  T := AsPlainTime(ThisValue, 'PlainTime.prototype.add');
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
    Obj := TGocciaObjectValue(Arg);
    H := 0; Mi := 0; S := 0; Ms := 0; Us := 0; Ns := 0;
    VH := Obj.GetProperty('hours');
    if Assigned(VH) and not (VH is TGocciaUndefinedLiteralValue) then H := Trunc(VH.ToNumberLiteral.Value);
    VH := Obj.GetProperty('minutes');
    if Assigned(VH) and not (VH is TGocciaUndefinedLiteralValue) then Mi := Trunc(VH.ToNumberLiteral.Value);
    VH := Obj.GetProperty('seconds');
    if Assigned(VH) and not (VH is TGocciaUndefinedLiteralValue) then S := Trunc(VH.ToNumberLiteral.Value);
    VH := Obj.GetProperty('milliseconds');
    if Assigned(VH) and not (VH is TGocciaUndefinedLiteralValue) then Ms := Trunc(VH.ToNumberLiteral.Value);
    VH := Obj.GetProperty('microseconds');
    if Assigned(VH) and not (VH is TGocciaUndefinedLiteralValue) then Us := Trunc(VH.ToNumberLiteral.Value);
    VH := Obj.GetProperty('nanoseconds');
    if Assigned(VH) and not (VH is TGocciaUndefinedLiteralValue) then Ns := Trunc(VH.ToNumberLiteral.Value);
    Dur := TGocciaTemporalDurationValue.Create(0, 0, 0, 0, H, Mi, S, Ms, Us, Ns);
  end
  else
  begin
    ThrowTypeError('PlainTime.prototype.add requires a Duration or string');
    Dur := nil;
  end;

  Balanced := BalanceTime(
    T.FHour + Dur.Hours,
    T.FMinute + Dur.Minutes,
    T.FSecond + Dur.Seconds,
    T.FMillisecond + Dur.Milliseconds,
    T.FMicrosecond + Dur.Microseconds,
    T.FNanosecond + Dur.Nanoseconds,
    ExtraDays);

  Result := TGocciaTemporalPlainTimeValue.Create(
    Balanced.Hour, Balanced.Minute, Balanced.Second,
    Balanced.Millisecond, Balanced.Microsecond, Balanced.Nanosecond);
end;

function TGocciaTemporalPlainTimeValue.TimeSubtract(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  T: TGocciaTemporalPlainTimeValue;
  Dur: TGocciaTemporalDurationValue;
  Arg: TGocciaValue;
  DurRec: TTemporalDurationRecord;
  ExtraDays: Int64;
  Balanced: TTemporalTimeRecord;
  Obj: TGocciaObjectValue;
  VH: TGocciaValue;
  H, Mi, S, Ms, Us, Ns: Int64;
begin
  T := AsPlainTime(ThisValue, 'PlainTime.prototype.subtract');
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
    Obj := TGocciaObjectValue(Arg);
    H := 0; Mi := 0; S := 0; Ms := 0; Us := 0; Ns := 0;
    VH := Obj.GetProperty('hours');
    if Assigned(VH) and not (VH is TGocciaUndefinedLiteralValue) then H := Trunc(VH.ToNumberLiteral.Value);
    VH := Obj.GetProperty('minutes');
    if Assigned(VH) and not (VH is TGocciaUndefinedLiteralValue) then Mi := Trunc(VH.ToNumberLiteral.Value);
    VH := Obj.GetProperty('seconds');
    if Assigned(VH) and not (VH is TGocciaUndefinedLiteralValue) then S := Trunc(VH.ToNumberLiteral.Value);
    VH := Obj.GetProperty('milliseconds');
    if Assigned(VH) and not (VH is TGocciaUndefinedLiteralValue) then Ms := Trunc(VH.ToNumberLiteral.Value);
    VH := Obj.GetProperty('microseconds');
    if Assigned(VH) and not (VH is TGocciaUndefinedLiteralValue) then Us := Trunc(VH.ToNumberLiteral.Value);
    VH := Obj.GetProperty('nanoseconds');
    if Assigned(VH) and not (VH is TGocciaUndefinedLiteralValue) then Ns := Trunc(VH.ToNumberLiteral.Value);
    Dur := TGocciaTemporalDurationValue.Create(0, 0, 0, 0, H, Mi, S, Ms, Us, Ns);
  end
  else
  begin
    ThrowTypeError('PlainTime.prototype.subtract requires a Duration or string');
    Dur := nil;
  end;

  Balanced := BalanceTime(
    T.FHour - Dur.Hours,
    T.FMinute - Dur.Minutes,
    T.FSecond - Dur.Seconds,
    T.FMillisecond - Dur.Milliseconds,
    T.FMicrosecond - Dur.Microseconds,
    T.FNanosecond - Dur.Nanoseconds,
    ExtraDays);

  Result := TGocciaTemporalPlainTimeValue.Create(
    Balanced.Hour, Balanced.Minute, Balanced.Second,
    Balanced.Millisecond, Balanced.Microsecond, Balanced.Nanosecond);
end;

function TGocciaTemporalPlainTimeValue.TimeUntil(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  T, Other: TGocciaTemporalPlainTimeValue;
  DiffNs: Int64;
begin
  T := AsPlainTime(ThisValue, 'PlainTime.prototype.until');
  Other := CoercePlainTime(Args.GetElement(0), 'PlainTime.prototype.until');

  DiffNs := TimeToTotalNanoseconds(Other) - TimeToTotalNanoseconds(T);

  Result := TGocciaTemporalDurationValue.Create(0, 0, 0, 0,
    DiffNs div Int64(3600000000000),
    (DiffNs div Int64(60000000000)) mod 60,
    (DiffNs div Int64(1000000000)) mod 60,
    (DiffNs div 1000000) mod 1000,
    (DiffNs div 1000) mod 1000,
    DiffNs mod 1000);
end;

function TGocciaTemporalPlainTimeValue.TimeSince(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  T, Other: TGocciaTemporalPlainTimeValue;
  DiffNs: Int64;
begin
  T := AsPlainTime(ThisValue, 'PlainTime.prototype.since');
  Other := CoercePlainTime(Args.GetElement(0), 'PlainTime.prototype.since');

  DiffNs := TimeToTotalNanoseconds(T) - TimeToTotalNanoseconds(Other);

  Result := TGocciaTemporalDurationValue.Create(0, 0, 0, 0,
    DiffNs div Int64(3600000000000),
    (DiffNs div Int64(60000000000)) mod 60,
    (DiffNs div Int64(1000000000)) mod 60,
    (DiffNs div 1000000) mod 1000,
    (DiffNs div 1000) mod 1000,
    DiffNs mod 1000);
end;

function TGocciaTemporalPlainTimeValue.TimeRound(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  T: TGocciaTemporalPlainTimeValue;
  UnitStr: string;
  Arg: TGocciaValue;
  TotalNs, Divisor, Rounded: Int64;
  ExtraDays: Int64;
  Balanced: TTemporalTimeRecord;
begin
  T := AsPlainTime(ThisValue, 'PlainTime.prototype.round');
  Arg := Args.GetElement(0);

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
    ThrowTypeError('PlainTime.prototype.round requires a string or options object');
    UnitStr := '';
  end;

  TotalNs := TimeToTotalNanoseconds(T);

  if UnitStr = 'hour' then
    Divisor := Int64(3600000000000)
  else if UnitStr = 'minute' then
    Divisor := Int64(60000000000)
  else if UnitStr = 'second' then
    Divisor := Int64(1000000000)
  else if UnitStr = 'millisecond' then
    Divisor := 1000000
  else if UnitStr = 'microsecond' then
    Divisor := 1000
  else if UnitStr = 'nanosecond' then
    Divisor := 1
  else
  begin
    ThrowRangeError('Invalid unit for PlainTime.prototype.round: ' + UnitStr);
    Divisor := 1;
  end;

  // Half-expand rounding
  Rounded := ((TotalNs + (Divisor div 2)) div Divisor) * Divisor;

  Balanced := BalanceTime(0, 0, 0, 0, 0, Rounded, ExtraDays);

  Result := TGocciaTemporalPlainTimeValue.Create(
    Balanced.Hour, Balanced.Minute, Balanced.Second,
    Balanced.Millisecond, Balanced.Microsecond, Balanced.Nanosecond);
end;

function TGocciaTemporalPlainTimeValue.TimeEquals(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  T, Other: TGocciaTemporalPlainTimeValue;
begin
  T := AsPlainTime(ThisValue, 'PlainTime.prototype.equals');
  Other := CoercePlainTime(Args.GetElement(0), 'PlainTime.prototype.equals');

  if CompareTimes(T.FHour, T.FMinute, T.FSecond, T.FMillisecond, T.FMicrosecond, T.FNanosecond,
                  Other.FHour, Other.FMinute, Other.FSecond, Other.FMillisecond, Other.FMicrosecond, Other.FNanosecond) = 0 then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;

function TGocciaTemporalPlainTimeValue.TimeToString(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  T: TGocciaTemporalPlainTimeValue;
begin
  T := AsPlainTime(ThisValue, 'PlainTime.prototype.toString');
  Result := TGocciaStringLiteralValue.Create(
    FormatTimeString(T.FHour, T.FMinute, T.FSecond, T.FMillisecond, T.FMicrosecond, T.FNanosecond));
end;

function TGocciaTemporalPlainTimeValue.TimeToJSON(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  T: TGocciaTemporalPlainTimeValue;
begin
  T := AsPlainTime(ThisValue, 'PlainTime.prototype.toJSON');
  Result := TGocciaStringLiteralValue.Create(
    FormatTimeString(T.FHour, T.FMinute, T.FSecond, T.FMillisecond, T.FMicrosecond, T.FNanosecond));
end;

function TGocciaTemporalPlainTimeValue.TimeValueOf(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
begin
  ThrowTypeError('Temporal.PlainTime.prototype.valueOf cannot be used; use toString or compare instead');
  Result := nil;
end;

end.
