unit Goccia.Values.TemporalInstant;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Primitives, Goccia.Values.ObjectValue,
  Goccia.Values.NativeFunction, Goccia.Arguments.Collection,
  Goccia.SharedPrototype, SysUtils;

type
  TGocciaTemporalInstantValue = class(TGocciaObjectValue)
  private
    class var FShared: TGocciaSharedPrototype;
  private
    FEpochMilliseconds: Int64;
    FSubMillisecondNanoseconds: Integer;

    function GetEpochMilliseconds(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function GetEpochNanoseconds(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;

    function InstantAdd(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function InstantSubtract(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function InstantUntil(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function InstantSince(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function InstantRound(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function InstantEquals(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function InstantToString(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function InstantToJSON(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function InstantValueOf(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;

    procedure InitializePrototype;
    function TotalNanoseconds: Double;
  public
    constructor Create(AEpochMilliseconds: Int64; ASubMillisecondNanoseconds: Integer); overload;

    function ToStringTag: string; override;
    class procedure ExposePrototype(AConstructor: TGocciaObjectValue);

    property EpochMilliseconds: Int64 read FEpochMilliseconds;
    property SubMillisecondNanoseconds: Integer read FSubMillisecondNanoseconds;
  end;

implementation

uses
  Goccia.Values.ErrorHelper, Goccia.GarbageCollector,
  Goccia.Values.ObjectPropertyDescriptor, Goccia.Temporal.Utils,
  Goccia.Values.TemporalDuration;

function AsInstant(AValue: TGocciaValue; const AMethod: string): TGocciaTemporalInstantValue;
begin
  if not (AValue is TGocciaTemporalInstantValue) then
    ThrowTypeError(AMethod + ' called on non-Instant');
  Result := TGocciaTemporalInstantValue(AValue);
end;

function CoerceInstant(AValue: TGocciaValue; const AMethod: string): TGocciaTemporalInstantValue;
var
  DateRec: TTemporalDateRecord;
  TimeRec: TTemporalTimeRecord;
  EpochMs: Int64;
  SubMs: Integer;
begin
  if AValue is TGocciaTemporalInstantValue then
    Result := TGocciaTemporalInstantValue(AValue)
  else if AValue is TGocciaStringLiteralValue then
  begin
    // Parse as ISO date-time and convert to epoch
    if not TryParseISODateTime(TGocciaStringLiteralValue(AValue).Value, DateRec, TimeRec) then
      ThrowTypeError('Invalid ISO instant string for ' + AMethod);
    EpochMs := DateToEpochDays(DateRec.Year, DateRec.Month, DateRec.Day) * Int64(86400000) +
               Int64(TimeRec.Hour) * 3600000 + Int64(TimeRec.Minute) * 60000 +
               Int64(TimeRec.Second) * 1000 + TimeRec.Millisecond;
    SubMs := TimeRec.Microsecond * 1000 + TimeRec.Nanosecond;
    Result := TGocciaTemporalInstantValue.Create(EpochMs, SubMs);
  end
  else
  begin
    ThrowTypeError(AMethod + ' requires an Instant or string');
    Result := nil;
  end;
end;

{ TGocciaTemporalInstantValue }

constructor TGocciaTemporalInstantValue.Create(AEpochMilliseconds: Int64; ASubMillisecondNanoseconds: Integer);
begin
  inherited Create(nil);
  FEpochMilliseconds := AEpochMilliseconds;
  FSubMillisecondNanoseconds := ASubMillisecondNanoseconds;

  // Normalize sub-ms nanoseconds into 0..999999 range
  if (FSubMillisecondNanoseconds >= 1000000) or (FSubMillisecondNanoseconds < 0) then
  begin
    if FSubMillisecondNanoseconds >= 0 then
    begin
      Inc(FEpochMilliseconds, FSubMillisecondNanoseconds div 1000000);
      FSubMillisecondNanoseconds := FSubMillisecondNanoseconds mod 1000000;
    end
    else
    begin
      // For negative values, borrow enough whole milliseconds
      // (FSubMillisecondNanoseconds - 999999) div 1000000 gives the negative delta
      Inc(FEpochMilliseconds, (FSubMillisecondNanoseconds - 999999) div 1000000);
      FSubMillisecondNanoseconds := FSubMillisecondNanoseconds - ((FSubMillisecondNanoseconds - 999999) div 1000000) * 1000000;
    end;
  end;

  InitializePrototype;
  if Assigned(FShared) then
    FPrototype := FShared.Prototype;
end;

procedure TGocciaTemporalInstantValue.InitializePrototype;
begin
  if Assigned(FShared) then Exit;
  FShared := TGocciaSharedPrototype.Create(Self);

  FShared.Prototype.DefineProperty('epochMilliseconds', TGocciaPropertyDescriptorAccessor.Create(
    TGocciaNativeFunctionValue.CreateWithoutPrototype(GetEpochMilliseconds, 'epochMilliseconds', 0), nil, [pfConfigurable]));
  FShared.Prototype.DefineProperty('epochNanoseconds', TGocciaPropertyDescriptorAccessor.Create(
    TGocciaNativeFunctionValue.CreateWithoutPrototype(GetEpochNanoseconds, 'epochNanoseconds', 0), nil, [pfConfigurable]));

  FShared.Prototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(InstantAdd, 'add', 1));
  FShared.Prototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(InstantSubtract, 'subtract', 1));
  FShared.Prototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(InstantUntil, 'until', 1));
  FShared.Prototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(InstantSince, 'since', 1));
  FShared.Prototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(InstantRound, 'round', 1));
  FShared.Prototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(InstantEquals, 'equals', 1));
  FShared.Prototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(InstantToString, 'toString', 0));
  FShared.Prototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(InstantToJSON, 'toJSON', 0));
  FShared.Prototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(InstantValueOf, 'valueOf', 0));
end;

class procedure TGocciaTemporalInstantValue.ExposePrototype(AConstructor: TGocciaObjectValue);
begin
  if not Assigned(FShared) then
    TGocciaTemporalInstantValue.Create(0, 0);
  FShared.ExposeOnConstructor(AConstructor);
end;

function TGocciaTemporalInstantValue.ToStringTag: string;
begin
  Result := 'Temporal.Instant';
end;

function TGocciaTemporalInstantValue.TotalNanoseconds: Double;
begin
  Result := FEpochMilliseconds * 1000000.0 + FSubMillisecondNanoseconds;
end;

{ Getters }

function TGocciaTemporalInstantValue.GetEpochMilliseconds(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaNumberLiteralValue.Create(AsInstant(ThisValue, 'get Instant.epochMilliseconds').FEpochMilliseconds);
end;

function TGocciaTemporalInstantValue.GetEpochNanoseconds(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  Inst: TGocciaTemporalInstantValue;
begin
  Inst := AsInstant(ThisValue, 'get Instant.epochNanoseconds');
  Result := TGocciaNumberLiteralValue.Create(Inst.TotalNanoseconds);
end;

{ Methods }

function TGocciaTemporalInstantValue.InstantAdd(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  Inst: TGocciaTemporalInstantValue;
  Dur: TGocciaTemporalDurationValue;
  Arg: TGocciaValue;
  DurRec: TTemporalDurationRecord;
  NewMs: Int64;
  NewSubMs: Integer;
begin
  Inst := AsInstant(ThisValue, 'Instant.prototype.add');
  Arg := Args.GetElement(0);

  if Arg is TGocciaTemporalDurationValue then
    Dur := TGocciaTemporalDurationValue(Arg)
  else if Arg is TGocciaStringLiteralValue then
  begin
    if not TryParseISODuration(TGocciaStringLiteralValue(Arg).Value, DurRec) then
      ThrowTypeError('Invalid duration string');
    Dur := TGocciaTemporalDurationValue.Create(0, 0, 0, DurRec.Days,
      DurRec.Hours, DurRec.Minutes, DurRec.Seconds,
      DurRec.Milliseconds, DurRec.Microseconds, DurRec.Nanoseconds);
  end
  else
  begin
    ThrowTypeError('Instant.prototype.add requires a Duration or string');
    Dur := nil;
  end;

  // Instant only supports time-based duration
  if (Dur.Years <> 0) or (Dur.Months <> 0) or (Dur.Weeks <> 0) then
    ThrowRangeError('Instant.prototype.add does not support years, months, or weeks');

  NewMs := Inst.FEpochMilliseconds +
           Dur.Days * Int64(86400000) +
           Dur.Hours * 3600000 +
           Dur.Minutes * 60000 +
           Dur.Seconds * 1000 +
           Dur.Milliseconds;
  NewSubMs := Inst.FSubMillisecondNanoseconds +
              Integer(Dur.Microseconds * 1000 + Dur.Nanoseconds);

  Result := TGocciaTemporalInstantValue.Create(NewMs, NewSubMs);
end;

function TGocciaTemporalInstantValue.InstantSubtract(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  Inst: TGocciaTemporalInstantValue;
  Dur: TGocciaTemporalDurationValue;
  Arg: TGocciaValue;
  DurRec: TTemporalDurationRecord;
  NewMs: Int64;
  NewSubMs: Integer;
begin
  Inst := AsInstant(ThisValue, 'Instant.prototype.subtract');
  Arg := Args.GetElement(0);

  if Arg is TGocciaTemporalDurationValue then
    Dur := TGocciaTemporalDurationValue(Arg)
  else if Arg is TGocciaStringLiteralValue then
  begin
    if not TryParseISODuration(TGocciaStringLiteralValue(Arg).Value, DurRec) then
      ThrowTypeError('Invalid duration string');
    Dur := TGocciaTemporalDurationValue.Create(0, 0, 0, DurRec.Days,
      DurRec.Hours, DurRec.Minutes, DurRec.Seconds,
      DurRec.Milliseconds, DurRec.Microseconds, DurRec.Nanoseconds);
  end
  else
  begin
    ThrowTypeError('Instant.prototype.subtract requires a Duration or string');
    Dur := nil;
  end;

  if (Dur.Years <> 0) or (Dur.Months <> 0) or (Dur.Weeks <> 0) then
    ThrowRangeError('Instant.prototype.subtract does not support years, months, or weeks');

  NewMs := Inst.FEpochMilliseconds -
           Dur.Days * Int64(86400000) -
           Dur.Hours * 3600000 -
           Dur.Minutes * 60000 -
           Dur.Seconds * 1000 -
           Dur.Milliseconds;
  NewSubMs := Inst.FSubMillisecondNanoseconds -
              Integer(Dur.Microseconds * 1000 + Dur.Nanoseconds);

  Result := TGocciaTemporalInstantValue.Create(NewMs, NewSubMs);
end;

function TGocciaTemporalInstantValue.InstantUntil(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  Inst, Other: TGocciaTemporalInstantValue;
  DiffMs: Int64;
  DiffSubMs: Integer;
  DiffNs: Int64;
begin
  Inst := AsInstant(ThisValue, 'Instant.prototype.until');
  Other := CoerceInstant(Args.GetElement(0), 'Instant.prototype.until');

  DiffMs := Other.FEpochMilliseconds - Inst.FEpochMilliseconds;
  DiffSubMs := Other.FSubMillisecondNanoseconds - Inst.FSubMillisecondNanoseconds;

  // Convert to total nanoseconds
  DiffNs := DiffMs * 1000000 + DiffSubMs;

  Result := TGocciaTemporalDurationValue.Create(0, 0, 0, 0,
    DiffNs div Int64(3600000000000),
    (DiffNs div Int64(60000000000)) mod 60,
    (DiffNs div Int64(1000000000)) mod 60,
    (DiffNs div 1000000) mod 1000,
    (DiffNs div 1000) mod 1000,
    DiffNs mod 1000);
end;

function TGocciaTemporalInstantValue.InstantSince(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  Inst, Other: TGocciaTemporalInstantValue;
  DiffMs: Int64;
  DiffSubMs: Integer;
  DiffNs: Int64;
begin
  Inst := AsInstant(ThisValue, 'Instant.prototype.since');
  Other := CoerceInstant(Args.GetElement(0), 'Instant.prototype.since');

  DiffMs := Inst.FEpochMilliseconds - Other.FEpochMilliseconds;
  DiffSubMs := Inst.FSubMillisecondNanoseconds - Other.FSubMillisecondNanoseconds;

  DiffNs := DiffMs * 1000000 + DiffSubMs;

  Result := TGocciaTemporalDurationValue.Create(0, 0, 0, 0,
    DiffNs div Int64(3600000000000),
    (DiffNs div Int64(60000000000)) mod 60,
    (DiffNs div Int64(1000000000)) mod 60,
    (DiffNs div 1000000) mod 1000,
    (DiffNs div 1000) mod 1000,
    DiffNs mod 1000);
end;

function TGocciaTemporalInstantValue.InstantRound(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  Inst: TGocciaTemporalInstantValue;
  UnitStr: string;
  Arg: TGocciaValue;
  TotalNs, Divisor, Rounded, NewMs: Int64;
  NewSubMs: Integer;
begin
  Inst := AsInstant(ThisValue, 'Instant.prototype.round');
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
    ThrowTypeError('Instant.prototype.round requires a string or options object');
    UnitStr := '';
  end;

  TotalNs := Inst.FEpochMilliseconds * 1000000 + Inst.FSubMillisecondNanoseconds;

  if UnitStr = 'hour' then Divisor := Int64(3600000000000)
  else if UnitStr = 'minute' then Divisor := Int64(60000000000)
  else if UnitStr = 'second' then Divisor := Int64(1000000000)
  else if UnitStr = 'millisecond' then Divisor := 1000000
  else if UnitStr = 'microsecond' then Divisor := 1000
  else if UnitStr = 'nanosecond' then Divisor := 1
  else
  begin
    ThrowRangeError('Invalid unit for Instant.prototype.round: ' + UnitStr);
    Divisor := 1;
  end;

  // HalfExpand rounding: round half-values away from zero
  if TotalNs >= 0 then
    Rounded := ((TotalNs + (Divisor div 2)) div Divisor) * Divisor
  else
    Rounded := -((((-TotalNs) + (Divisor div 2)) div Divisor) * Divisor);
  NewMs := Rounded div 1000000;
  NewSubMs := Integer(Rounded mod 1000000);
  if NewSubMs < 0 then
  begin
    Dec(NewMs);
    Inc(NewSubMs, 1000000);
  end;

  Result := TGocciaTemporalInstantValue.Create(NewMs, NewSubMs);
end;

function TGocciaTemporalInstantValue.InstantEquals(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  Inst, Other: TGocciaTemporalInstantValue;
begin
  Inst := AsInstant(ThisValue, 'Instant.prototype.equals');
  Other := CoerceInstant(Args.GetElement(0), 'Instant.prototype.equals');

  if (Inst.FEpochMilliseconds = Other.FEpochMilliseconds) and
     (Inst.FSubMillisecondNanoseconds = Other.FSubMillisecondNanoseconds) then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;

function TGocciaTemporalInstantValue.InstantToString(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  Inst: TGocciaTemporalInstantValue;
  DateRec: TTemporalDateRecord;
  EpochDays, RemainingMs: Int64;
  Hour, Minute, Second, Ms: Integer;
  Us, Ns: Integer;
  S: string;
begin
  Inst := AsInstant(ThisValue, 'Instant.prototype.toString');

  EpochDays := Inst.FEpochMilliseconds div Int64(86400000);
  RemainingMs := Inst.FEpochMilliseconds mod Int64(86400000);
  if RemainingMs < 0 then
  begin
    Dec(EpochDays);
    Inc(RemainingMs, Int64(86400000));
  end;

  DateRec := EpochDaysToDate(EpochDays);
  Hour := Integer(RemainingMs div 3600000);
  RemainingMs := RemainingMs mod 3600000;
  Minute := Integer(RemainingMs div 60000);
  RemainingMs := RemainingMs mod 60000;
  Second := Integer(RemainingMs div 1000);
  Ms := Integer(RemainingMs mod 1000);

  Us := Inst.FSubMillisecondNanoseconds div 1000;
  Ns := Inst.FSubMillisecondNanoseconds mod 1000;

  S := FormatDateString(DateRec.Year, DateRec.Month, DateRec.Day) + 'T' +
       FormatTimeString(Hour, Minute, Second, Ms, Us, Ns) + 'Z';

  Result := TGocciaStringLiteralValue.Create(S);
end;

function TGocciaTemporalInstantValue.InstantToJSON(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
begin
  Result := InstantToString(Args, ThisValue);
end;

function TGocciaTemporalInstantValue.InstantValueOf(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
begin
  ThrowTypeError('Temporal.Instant.prototype.valueOf cannot be used; use epochMilliseconds, epochNanoseconds, or compare instead');
  Result := nil;
end;

end.
