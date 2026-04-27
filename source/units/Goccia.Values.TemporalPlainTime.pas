unit Goccia.Values.TemporalPlainTime;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.ObjectModel,
  Goccia.SharedPrototype,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaTemporalPlainTimeValue = class(TGocciaObjectValue)
  private
    FHour: Integer;
    FMinute: Integer;
    FSecond: Integer;
    FMillisecond: Integer;
    FMicrosecond: Integer;
    FNanosecond: Integer;
  published
    function GetHour(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetMinute(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetSecond(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetMillisecond(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetMicrosecond(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetNanosecond(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;

    function TimeWith(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function TimeAdd(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function TimeSubtract(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function TimeUntil(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function TimeSince(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function TimeRound(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function TimeEquals(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function TimeToString(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function TimeToJSON(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function TimeValueOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function PlainTimeToLocaleString(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;

    procedure InitializePrototype;
  public
    constructor Create(const AHour, AMinute, ASecond, AMillisecond, AMicrosecond, ANanosecond: Integer); overload;

    function ToStringTag: string; override;
    class procedure ExposePrototype(const AConstructor: TGocciaObjectValue);

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
  Goccia.Temporal.Utils,
  Goccia.Values.ErrorHelper,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.SymbolValue,
  Goccia.Values.TemporalDuration;

var
  GTemporalPlainTimeSharedSlot: TGocciaRealmOwnedSlotId;

threadvar
  FPrototypeMembers: TArray<TGocciaMemberDefinition>;

function GetTemporalPlainTimeShared: TGocciaSharedPrototype; inline;
begin
  if Assigned(CurrentRealm) then
    Result := TGocciaSharedPrototype(CurrentRealm.GetOwnedSlot(GTemporalPlainTimeSharedSlot))
  else
    Result := nil;
end;

function AsPlainTime(const AValue: TGocciaValue; const AMethod: string): TGocciaTemporalPlainTimeValue;
begin
  if not (AValue is TGocciaTemporalPlainTimeValue) then
    ThrowTypeError(AMethod + ' called on non-PlainTime', SSuggestTemporalThisType);
  Result := TGocciaTemporalPlainTimeValue(AValue);
end;

function CoercePlainTime(const AValue: TGocciaValue; const AMethod: string): TGocciaTemporalPlainTimeValue;
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
    if not CoerceToISOTime(TGocciaStringLiteralValue(AValue).Value, TimeRec) then
      ThrowRangeError(Format(SErrorTemporalInvalidISOStringFor, ['time', AMethod]), SSuggestTemporalISOFormat);
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
    ThrowTypeError(AMethod + ' requires a PlainTime, string, or object', SSuggestTemporalFromArg);
    Result := nil;
  end;
end;

function TimeToTotalNanoseconds(const T: TGocciaTemporalPlainTimeValue): Int64;
begin
  Result := Int64(T.FNanosecond) +
            Int64(T.FMicrosecond) * 1000 +
            Int64(T.FMillisecond) * 1000000 +
            Int64(T.FSecond) * Int64(1000000000) +
            Int64(T.FMinute) * Int64(60000000000) +
            Int64(T.FHour) * Int64(3600000000000);
end;

{ TGocciaTemporalPlainTimeValue }

constructor TGocciaTemporalPlainTimeValue.Create(const AHour, AMinute, ASecond, AMillisecond, AMicrosecond, ANanosecond: Integer);
var
  Shared: TGocciaSharedPrototype;
begin
  inherited Create(nil);
  if not IsValidTime(AHour, AMinute, ASecond, AMillisecond, AMicrosecond, ANanosecond) then
    ThrowRangeError(SErrorInvalidTime, SSuggestTemporalDateRange);
  FHour := AHour;
  FMinute := AMinute;
  FSecond := ASecond;
  FMillisecond := AMillisecond;
  FMicrosecond := AMicrosecond;
  FNanosecond := ANanosecond;
  InitializePrototype;
  Shared := GetTemporalPlainTimeShared;
  if Assigned(Shared) then
    FPrototype := Shared.Prototype;
end;

procedure TGocciaTemporalPlainTimeValue.InitializePrototype;
var
  Members: TGocciaMemberCollection;
  Shared: TGocciaSharedPrototype;
begin
  if not Assigned(CurrentRealm) then Exit;
  if Assigned(GetTemporalPlainTimeShared) then Exit;
  Shared := TGocciaSharedPrototype.Create(Self);
  CurrentRealm.SetOwnedSlot(GTemporalPlainTimeSharedSlot, Shared);
  if Length(FPrototypeMembers) = 0 then
  begin
    Members := TGocciaMemberCollection.Create;
    try
      Members.AddAccessor('hour', GetHour, nil, [pfConfigurable]);
      Members.AddAccessor('minute', GetMinute, nil, [pfConfigurable]);
      Members.AddAccessor('second', GetSecond, nil, [pfConfigurable]);
      Members.AddAccessor('millisecond', GetMillisecond, nil, [pfConfigurable]);
      Members.AddAccessor('microsecond', GetMicrosecond, nil, [pfConfigurable]);
      Members.AddAccessor('nanosecond', GetNanosecond, nil, [pfConfigurable]);
      Members.AddMethod(TimeWith, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(TimeAdd, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(TimeSubtract, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(TimeUntil, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(TimeSince, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(TimeRound, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(TimeEquals, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(TimeToString, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(TimeToJSON, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(TimeValueOf, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(PlainTimeToLocaleString, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddSymbolDataProperty(
        TGocciaSymbolValue.WellKnownToStringTag,
        TGocciaStringLiteralValue.Create('Temporal.PlainTime'),
        [pfConfigurable]);
      FPrototypeMembers := Members.ToDefinitions;
    finally
      Members.Free;
    end;
  end;
  RegisterMemberDefinitions(Shared.Prototype, FPrototypeMembers);
end;

class procedure TGocciaTemporalPlainTimeValue.ExposePrototype(const AConstructor: TGocciaObjectValue);
var
  Shared: TGocciaSharedPrototype;
begin
  Shared := GetTemporalPlainTimeShared;
  if not Assigned(Shared) then
  begin
    TGocciaTemporalPlainTimeValue.Create(0, 0, 0, 0, 0, 0);
    Shared := GetTemporalPlainTimeShared;
  end;
  // InitializePrototype exits early when CurrentRealm is nil, so the lazy
  // Create() can leave Shared still nil — guard before deref.
  if Assigned(Shared) then
    ExposeSharedPrototypeOnConstructor(Shared, AConstructor);
end;

function TGocciaTemporalPlainTimeValue.ToStringTag: string;
begin
  Result := 'Temporal.PlainTime';
end;

{ Getters }

function TGocciaTemporalPlainTimeValue.GetHour(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaNumberLiteralValue.Create(AsPlainTime(AThisValue, 'get PlainTime.hour').FHour);
end;

function TGocciaTemporalPlainTimeValue.GetMinute(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaNumberLiteralValue.Create(AsPlainTime(AThisValue, 'get PlainTime.minute').FMinute);
end;

function TGocciaTemporalPlainTimeValue.GetSecond(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaNumberLiteralValue.Create(AsPlainTime(AThisValue, 'get PlainTime.second').FSecond);
end;

function TGocciaTemporalPlainTimeValue.GetMillisecond(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaNumberLiteralValue.Create(AsPlainTime(AThisValue, 'get PlainTime.millisecond').FMillisecond);
end;

function TGocciaTemporalPlainTimeValue.GetMicrosecond(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaNumberLiteralValue.Create(AsPlainTime(AThisValue, 'get PlainTime.microsecond').FMicrosecond);
end;

function TGocciaTemporalPlainTimeValue.GetNanosecond(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaNumberLiteralValue.Create(AsPlainTime(AThisValue, 'get PlainTime.nanosecond').FNanosecond);
end;

{ Methods }

function TGocciaTemporalPlainTimeValue.TimeWith(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  T: TGocciaTemporalPlainTimeValue;
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
  T := AsPlainTime(AThisValue, 'PlainTime.prototype.with');
  V := AArgs.GetElement(0);
  if not (V is TGocciaObjectValue) then
    ThrowTypeError(Format(SErrorTemporalWithRequiresObject, ['PlainTime']), SSuggestTemporalWithObject);
  Obj := TGocciaObjectValue(V);

  Result := TGocciaTemporalPlainTimeValue.Create(
    GetFieldOr('hour', T.FHour),
    GetFieldOr('minute', T.FMinute),
    GetFieldOr('second', T.FSecond),
    GetFieldOr('millisecond', T.FMillisecond),
    GetFieldOr('microsecond', T.FMicrosecond),
    GetFieldOr('nanosecond', T.FNanosecond));
end;

function TGocciaTemporalPlainTimeValue.TimeAdd(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
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
  T := AsPlainTime(AThisValue, 'PlainTime.prototype.add');
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
    ThrowTypeError(Format(SErrorTemporalAddRequiresDuration, ['PlainTime']), SSuggestTemporalDurationArg);
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

function TGocciaTemporalPlainTimeValue.TimeSubtract(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
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
  T := AsPlainTime(AThisValue, 'PlainTime.prototype.subtract');
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
    ThrowTypeError(Format(SErrorTemporalSubtractRequiresDuration, ['PlainTime']), SSuggestTemporalDurationArg);
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

{ Shared logic for PlainTime until/since: parse options, decompose by
  largestUnit, round, and return the duration value.  ADiffNs is
  Other-This for until, This-Other for since. }
function ComputeTimeDiffDuration(const ADiffNs: Int64;
  const AArgs: TGocciaArgumentsCollection;
  const AMethodName: string): TGocciaValue;
var
  OptionsObj: TGocciaObjectValue;
  LargestUnit, SmallestUnit: TTemporalUnit;
  RMode: TTemporalRoundingMode;
  RIncrement: Integer;
  Y, Mo, W, D, H, Mi, S, Ms, Us, Ns: Int64;
begin
  OptionsObj := GetDiffOptions(AArgs, 1);
  LargestUnit := GetLargestUnit(OptionsObj, tuHour);
  if LargestUnit = tuAuto then LargestUnit := tuHour;
  if not (LargestUnit in [tuHour, tuMinute, tuSecond, tuMillisecond, tuMicrosecond, tuNanosecond]) then
    ThrowRangeError(Format(SErrorTemporalInvalidUnitFor, [AMethodName, 'largestUnit']), SSuggestTemporalValidUnits);

  SmallestUnit := GetSmallestUnit(OptionsObj, tuNanosecond);
  if not (SmallestUnit in [tuHour, tuMinute, tuSecond, tuMillisecond, tuMicrosecond, tuNanosecond]) then
    ThrowRangeError(Format(SErrorTemporalInvalidUnitFor, [AMethodName, 'smallestUnit']), SSuggestTemporalValidUnits);
  if Ord(LargestUnit) > Ord(SmallestUnit) then
    ThrowRangeError(SErrorDurationRoundLargestSmallerThanSmallest, SSuggestTemporalRoundArg);
  RMode := GetRoundingMode(OptionsObj, rmTrunc);
  RIncrement := GetRoundingIncrement(OptionsObj, 1);

  // Decompose ADiffNs based on largestUnit
  Y := 0; Mo := 0; W := 0; D := 0;
  H := 0; Mi := 0; S := 0; Ms := 0; Us := 0; Ns := 0;
  case LargestUnit of
    tuHour:
    begin
      H := ADiffNs div Int64(3600000000000);
      Mi := (ADiffNs mod Int64(3600000000000)) div Int64(60000000000);
      S := (ADiffNs mod Int64(60000000000)) div Int64(1000000000);
      Ms := (ADiffNs mod Int64(1000000000)) div 1000000;
      Us := (ADiffNs mod 1000000) div 1000;
      Ns := ADiffNs mod 1000;
    end;
    tuMinute:
    begin
      Mi := ADiffNs div Int64(60000000000);
      S := (ADiffNs mod Int64(60000000000)) div Int64(1000000000);
      Ms := (ADiffNs mod Int64(1000000000)) div 1000000;
      Us := (ADiffNs mod 1000000) div 1000;
      Ns := ADiffNs mod 1000;
    end;
    tuSecond:
    begin
      S := ADiffNs div Int64(1000000000);
      Ms := (ADiffNs mod Int64(1000000000)) div 1000000;
      Us := (ADiffNs mod 1000000) div 1000;
      Ns := ADiffNs mod 1000;
    end;
    tuMillisecond:
    begin
      Ms := ADiffNs div 1000000;
      Us := (ADiffNs mod 1000000) div 1000;
      Ns := ADiffNs mod 1000;
    end;
    tuMicrosecond:
    begin
      Us := ADiffNs div 1000;
      Ns := ADiffNs mod 1000;
    end;
  else // tuNanosecond
    Ns := ADiffNs;
  end;

  if (SmallestUnit <> tuNanosecond) or (RIncrement <> 1) then
    RoundDiffDuration(Y, Mo, W, D, H, Mi, S, Ms, Us, Ns,
      0, 0, 0, LargestUnit, SmallestUnit, RMode, RIncrement);

  Result := TGocciaTemporalDurationValue.Create(0, 0, 0, 0, H, Mi, S, Ms, Us, Ns);
end;

function TGocciaTemporalPlainTimeValue.TimeUntil(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  T, Other: TGocciaTemporalPlainTimeValue;
begin
  T := AsPlainTime(AThisValue, 'PlainTime.prototype.until');
  Other := CoercePlainTime(AArgs.GetElement(0), 'PlainTime.prototype.until');
  Result := ComputeTimeDiffDuration(
    TimeToTotalNanoseconds(Other) - TimeToTotalNanoseconds(T),
    AArgs, 'PlainTime.prototype.until');
end;

function TGocciaTemporalPlainTimeValue.TimeSince(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  T, Other: TGocciaTemporalPlainTimeValue;
begin
  T := AsPlainTime(AThisValue, 'PlainTime.prototype.since');
  Other := CoercePlainTime(AArgs.GetElement(0), 'PlainTime.prototype.since');
  Result := ComputeTimeDiffDuration(
    TimeToTotalNanoseconds(T) - TimeToTotalNanoseconds(Other),
    AArgs, 'PlainTime.prototype.since');
end;

function TGocciaTemporalPlainTimeValue.TimeRound(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  T: TGocciaTemporalPlainTimeValue;
  UnitStr: string;
  Arg: TGocciaValue;
  OptionsObj: TGocciaObjectValue;
  TotalNs, Divisor, Rounded: Int64;
  ExtraDays: Int64;
  Balanced: TTemporalTimeRecord;
  SmallestUnit: TTemporalUnit;
  Mode: TTemporalRoundingMode;
  Increment: Integer;
begin
  T := AsPlainTime(AThisValue, 'PlainTime.prototype.round');
  Arg := AArgs.GetElement(0);

  Mode := rmHalfExpand;
  Increment := 1;

  if Arg is TGocciaStringLiteralValue then
  begin
    UnitStr := TGocciaStringLiteralValue(Arg).Value;
    if not GetTemporalUnitFromString(UnitStr, SmallestUnit) then
      ThrowRangeError(Format(SErrorTemporalInvalidUnitFor, ['PlainTime.prototype.round', UnitStr]), SSuggestTemporalValidUnits);
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
    ThrowTypeError(Format(SErrorTemporalRoundRequiresStringOrOptions, ['PlainTime']), SSuggestTemporalRoundArg);
    SmallestUnit := tuNanosecond;
  end;

  TotalNs := TimeToTotalNanoseconds(T);
  Divisor := UnitToNanoseconds(SmallestUnit) * Increment;
  Rounded := RoundWithMode(TotalNs, Divisor, Mode);

  Balanced := BalanceTime(0, 0, 0, 0, 0, Rounded, ExtraDays);

  Result := TGocciaTemporalPlainTimeValue.Create(
    Balanced.Hour, Balanced.Minute, Balanced.Second,
    Balanced.Millisecond, Balanced.Microsecond, Balanced.Nanosecond);
end;

function TGocciaTemporalPlainTimeValue.TimeEquals(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  T, Other: TGocciaTemporalPlainTimeValue;
begin
  T := AsPlainTime(AThisValue, 'PlainTime.prototype.equals');
  Other := CoercePlainTime(AArgs.GetElement(0), 'PlainTime.prototype.equals');

  if CompareTimes(T.FHour, T.FMinute, T.FSecond, T.FMillisecond, T.FMicrosecond, T.FNanosecond,
                  Other.FHour, Other.FMinute, Other.FSecond, Other.FMillisecond, Other.FMicrosecond, Other.FNanosecond) = 0 then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;

function TGocciaTemporalPlainTimeValue.TimeToString(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  T: TGocciaTemporalPlainTimeValue;
  Arg: TGocciaValue;
  OptionsObj: TGocciaObjectValue;
  FracDigits, ExtraDays: Integer;
  Mode: TTemporalRoundingMode;
  H, Mi, S, Ms, Us, Ns: Integer;
begin
  T := AsPlainTime(AThisValue, 'PlainTime.prototype.toString');
  OptionsObj := nil;
  Arg := AArgs.GetElement(0);
  if Assigned(Arg) and not (Arg is TGocciaUndefinedLiteralValue) then
  begin
    if not (Arg is TGocciaObjectValue) then
      ThrowTypeError('options must be an object or undefined', SSuggestTemporalFromArg);
    OptionsObj := TGocciaObjectValue(Arg);
  end;
  ResolveTemporalToStringOptions(OptionsObj, FracDigits, Mode);
  H := T.FHour; Mi := T.FMinute; S := T.FSecond;
  Ms := T.FMillisecond; Us := T.FMicrosecond; Ns := T.FNanosecond;
  ExtraDays := 0;
  RoundTimeForToString(H, Mi, S, Ms, Us, Ns, ExtraDays, FracDigits, Mode);
  if FracDigits = -2 then // smallestUnit: minute
    Result := TGocciaStringLiteralValue.Create(PadTwo(H) + ':' + PadTwo(Mi))
  else
    Result := TGocciaStringLiteralValue.Create(
      FormatTimeWithPrecision(H, Mi, S, Ms, Us, Ns, FracDigits));
end;

function TGocciaTemporalPlainTimeValue.TimeToJSON(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  T: TGocciaTemporalPlainTimeValue;
begin
  T := AsPlainTime(AThisValue, 'PlainTime.prototype.toJSON');
  Result := TGocciaStringLiteralValue.Create(
    FormatTimeString(T.FHour, T.FMinute, T.FSecond, T.FMillisecond, T.FMicrosecond, T.FNanosecond));
end;

function TGocciaTemporalPlainTimeValue.TimeValueOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  ThrowTypeError(Format(SErrorTemporalValueOf, ['PlainTime', 'toString or compare']), SSuggestTemporalNoValueOf);
  Result := nil;
end;

function TGocciaTemporalPlainTimeValue.PlainTimeToLocaleString(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  EmptyArgs: TGocciaArgumentsCollection;
begin
  EmptyArgs := TGocciaArgumentsCollection.Create([]);
  try
    Result := TimeToString(EmptyArgs, AThisValue);
  finally
    EmptyArgs.Free;
  end;
end;

initialization
  GTemporalPlainTimeSharedSlot := RegisterRealmOwnedSlot('Temporal.PlainTime.shared');

end.
