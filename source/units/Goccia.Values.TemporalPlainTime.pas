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

function CoercePlainTime(const AValue: TGocciaValue; const AMethod: string): TGocciaTemporalPlainTimeValue;

implementation

uses
  SysUtils,

  BigInteger,

  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.Realm,
  Goccia.Temporal.DurationMath,
  Goccia.Temporal.Options,
  Goccia.Temporal.Utils,
  Goccia.Utils,
  Goccia.Values.ErrorHelper,
  Goccia.Values.IntlDateTimeFormat,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.SymbolValue,
  Goccia.Values.TemporalDuration,
  Goccia.Values.TemporalPlainDateTime,
  Goccia.Values.TemporalZonedDateTime;

var
  GTemporalPlainTimeSharedSlot: TGocciaRealmOwnedSlotId;

function GetTemporalPlainTimeShared: TGocciaSharedPrototype;
{$IFDEF FPC}inline;{$ENDIF}
begin
  if (CurrentRealm <> nil) then
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

procedure ConstrainTemporalTimeFields(var AHour, AMinute, ASecond,
  AMillisecond, AMicrosecond, ANanosecond: Integer); forward;

function CoercePlainTime(const AValue: TGocciaValue; const AMethod: string): TGocciaTemporalPlainTimeValue;
var
  TimeRec: TTemporalTimeRecord;
  Obj: TGocciaObjectValue;
  EmptyArgs: TGocciaArgumentsCollection;
  Converted, V: TGocciaValue;
  H, Mi, S, Ms, Us, Ns: Integer;
  SawTimeField: Boolean;
begin
  if AValue is TGocciaTemporalPlainTimeValue then
    Result := TGocciaTemporalPlainTimeValue(AValue)
  else if AValue is TGocciaTemporalPlainDateTimeValue then
  begin
    Result := TGocciaTemporalPlainTimeValue.Create(
      TGocciaTemporalPlainDateTimeValue(AValue).Hour,
      TGocciaTemporalPlainDateTimeValue(AValue).Minute,
      TGocciaTemporalPlainDateTimeValue(AValue).Second,
      TGocciaTemporalPlainDateTimeValue(AValue).Millisecond,
      TGocciaTemporalPlainDateTimeValue(AValue).Microsecond,
      TGocciaTemporalPlainDateTimeValue(AValue).Nanosecond);
  end
  else if AValue is TGocciaTemporalZonedDateTimeValue then
  begin
    EmptyArgs := TGocciaArgumentsCollection.Create([]);
    try
      Converted := TGocciaTemporalZonedDateTimeValue(AValue).ZonedDateTimeToPlainTime(EmptyArgs, AValue);
      Result := TGocciaTemporalPlainTimeValue(Converted);
    finally
      EmptyArgs.Free;
    end;
  end
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
    SawTimeField := False;
    V := Obj.GetProperty('hour');
    if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
    begin
      SawTimeField := True;
      H := ToIntegerWithTruncationValue(V);
    end;
    V := Obj.GetProperty('microsecond');
    if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
    begin
      SawTimeField := True;
      Us := ToIntegerWithTruncationValue(V);
    end;
    V := Obj.GetProperty('millisecond');
    if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
    begin
      SawTimeField := True;
      Ms := ToIntegerWithTruncationValue(V);
    end;
    V := Obj.GetProperty('minute');
    if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
    begin
      SawTimeField := True;
      Mi := ToIntegerWithTruncationValue(V);
    end;
    V := Obj.GetProperty('nanosecond');
    if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
    begin
      SawTimeField := True;
      Ns := ToIntegerWithTruncationValue(V);
    end;
    V := Obj.GetProperty('second');
    if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
    begin
      SawTimeField := True;
      S := ToIntegerWithTruncationValue(V);
    end;
    if not SawTimeField then
      ThrowTypeError(AMethod + ' requires at least one time property',
        SSuggestTemporalFromArg);
    ConstrainTemporalTimeFields(H, Mi, S, Ms, Us, Ns);
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

function ClampTemporalInteger(const AValue, AMin, AMax: Integer): Integer;
begin
  if AValue < AMin then
    Result := AMin
  else if AValue > AMax then
    Result := AMax
  else
    Result := AValue;
end;

procedure ConstrainTemporalTimeFields(var AHour, AMinute, ASecond,
  AMillisecond, AMicrosecond, ANanosecond: Integer);
begin
  AHour := ClampTemporalInteger(AHour, 0, 23);
  AMinute := ClampTemporalInteger(AMinute, 0, 59);
  ASecond := ClampTemporalInteger(ASecond, 0, 59);
  AMillisecond := ClampTemporalInteger(AMillisecond, 0, 999);
  AMicrosecond := ClampTemporalInteger(AMicrosecond, 0, 999);
  ANanosecond := ClampTemporalInteger(ANanosecond, 0, 999);
end;

procedure ValidatePlainTimeDurationTimeRange(const ADuration: TGocciaTemporalDurationValue);
var
  TimeNs: TBigInteger;
begin
  TimeNs := TimeDurationFromComponents(ADuration.HoursBig,
    ADuration.MinutesBig, ADuration.SecondsBig, ADuration.MillisecondsBig,
    ADuration.MicrosecondsBig, ADuration.NanosecondsBig);
  if not IsValidTimeDuration(TimeNs) then
    ThrowRangeError(SErrorInvalidDurationString, SSuggestTemporalDurationRange);
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
  PrototypeMembers: TArray<TGocciaMemberDefinition>;
begin
  if (CurrentRealm = nil) then Exit;
  if (GetTemporalPlainTimeShared <> nil) then Exit;
  Shared := TGocciaSharedPrototype.Create(Self);
  CurrentRealm.SetOwnedSlot(GTemporalPlainTimeSharedSlot, Shared);
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
    PrototypeMembers := Members.ToDefinitions;
  finally
    Members.Free;
  end;
  RegisterMemberDefinitions(Shared.Prototype, PrototypeMembers);
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
  Overflow: TTemporalOverflow;
  H, Mi, S, Ms, Us, Ns: Integer;
  SawTimeField: Boolean;

  procedure ReadField(const AName: string; var ATarget: Integer);
  var
    Val: TGocciaValue;
  begin
    Val := Obj.GetProperty(AName);
    if Assigned(Val) and not (Val is TGocciaUndefinedLiteralValue) then
    begin
      SawTimeField := True;
      ATarget := ToIntegerWithTruncationValue(Val);
    end;
  end;

begin
  T := AsPlainTime(AThisValue, 'PlainTime.prototype.with');
  V := AArgs.GetElement(0);
  if not (V is TGocciaObjectValue) then
    ThrowTypeError(Format(SErrorTemporalWithRequiresObject, ['PlainTime']), SSuggestTemporalWithObject);
  Obj := TGocciaObjectValue(V);
  if Copy(Obj.ToStringTag, 1, 9) = 'Temporal.' then
    ThrowTypeError(Format(SErrorTemporalWithRequiresObject, ['PlainTime']), SSuggestTemporalWithObject);

  V := Obj.GetProperty('calendar');
  if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
    ThrowTypeError('PlainTime.prototype.with does not accept calendar', SSuggestTemporalWithObject);
  V := Obj.GetProperty('timeZone');
  if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
    ThrowTypeError('PlainTime.prototype.with does not accept timeZone', SSuggestTemporalWithObject);

  H := T.FHour; Mi := T.FMinute; S := T.FSecond;
  Ms := T.FMillisecond; Us := T.FMicrosecond; Ns := T.FNanosecond;
  SawTimeField := False;
  ReadField('hour', H);
  ReadField('microsecond', Us);
  ReadField('millisecond', Ms);
  ReadField('minute', Mi);
  ReadField('nanosecond', Ns);
  ReadField('second', S);
  if not SawTimeField then
    ThrowTypeError(Format(SErrorTemporalWithRequiresObject, ['PlainTime']), SSuggestTemporalWithObject);

  Overflow := GetOverflowOptionFromValue(AArgs.GetElement(1), 'PlainTime.prototype.with');
  if Overflow = toConstrain then
    ConstrainTemporalTimeFields(H, Mi, S, Ms, Us, Ns);

  Result := TGocciaTemporalPlainTimeValue.Create(H, Mi, S, Ms, Us, Ns);
end;

function TGocciaTemporalPlainTimeValue.TimeAdd(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  T: TGocciaTemporalPlainTimeValue;
  Dur: TGocciaTemporalDurationValue;
  Arg: TGocciaValue;
  ExtraDays: Int64;
  Balanced: TTemporalTimeRecord;
  DurTimeNs: TBigInteger;
begin
  try
  T := AsPlainTime(AThisValue, 'PlainTime.prototype.add');
  Arg := AArgs.GetElement(0);

  if Arg is TGocciaTemporalDurationValue then
    Dur := TGocciaTemporalDurationValue(Arg)
  else if Arg is TGocciaStringLiteralValue then
  begin
    Dur := DurationFromString(TGocciaStringLiteralValue(Arg).Value,
      SErrorInvalidDurationString);
    ValidatePlainTimeDurationTimeRange(Dur);
  end
  else if Arg is TGocciaObjectValue then
    Dur := DurationFromObject(TGocciaObjectValue(Arg))
  else
  begin
    ThrowTypeError(Format(SErrorTemporalAddRequiresDuration, ['PlainTime']), SSuggestTemporalDurationArg);
    Dur := nil;
  end;

  // PlainTime arithmetic is modulo one day, so reduce the duration's exact
  // sub-day time (BigInteger — components may exceed Int64) before balancing;
  // summing per-component Int64 values would overflow for large durations.
  DurTimeNs := TimeDurationFromComponents(Dur.HoursBig, Dur.MinutesBig,
    Dur.SecondsBig, Dur.MillisecondsBig, Dur.MicrosecondsBig,
    Dur.NanosecondsBig).Modulo(TBigInteger.FromInt64(NANOSECONDS_PER_DAY));

  Balanced := BalanceTime(
    T.FHour, T.FMinute, T.FSecond, T.FMillisecond, T.FMicrosecond,
    T.FNanosecond + DurTimeNs.ToInt64,
    ExtraDays);

  Result := TGocciaTemporalPlainTimeValue.Create(
    Balanced.Hour, Balanced.Minute, Balanced.Second,
    Balanced.Millisecond, Balanced.Microsecond, Balanced.Nanosecond);
  except
    on E: ETemporalDurationInt64Overflow do
      ThrowRangeError(E.Message, SSuggestTemporalDurationRange);
  end;
end;

function TGocciaTemporalPlainTimeValue.TimeSubtract(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  T: TGocciaTemporalPlainTimeValue;
  Dur: TGocciaTemporalDurationValue;
  Arg: TGocciaValue;
  ExtraDays: Int64;
  Balanced: TTemporalTimeRecord;
  DurTimeNs: TBigInteger;
begin
  try
  T := AsPlainTime(AThisValue, 'PlainTime.prototype.subtract');
  Arg := AArgs.GetElement(0);

  if Arg is TGocciaTemporalDurationValue then
    Dur := TGocciaTemporalDurationValue(Arg)
  else if Arg is TGocciaStringLiteralValue then
  begin
    Dur := DurationFromString(TGocciaStringLiteralValue(Arg).Value,
      SErrorInvalidDurationString);
    ValidatePlainTimeDurationTimeRange(Dur);
  end
  else if Arg is TGocciaObjectValue then
    Dur := DurationFromObject(TGocciaObjectValue(Arg))
  else
  begin
    ThrowTypeError(Format(SErrorTemporalSubtractRequiresDuration, ['PlainTime']), SSuggestTemporalDurationArg);
    Dur := nil;
  end;

  // See TimeAdd: reduce the exact sub-day time before balancing.
  DurTimeNs := TimeDurationFromComponents(Dur.HoursBig, Dur.MinutesBig,
    Dur.SecondsBig, Dur.MillisecondsBig, Dur.MicrosecondsBig,
    Dur.NanosecondsBig).Modulo(TBigInteger.FromInt64(NANOSECONDS_PER_DAY));

  Balanced := BalanceTime(
    T.FHour, T.FMinute, T.FSecond, T.FMillisecond, T.FMicrosecond,
    T.FNanosecond - DurTimeNs.ToInt64,
    ExtraDays);

  Result := TGocciaTemporalPlainTimeValue.Create(
    Balanced.Hour, Balanced.Minute, Balanced.Second,
    Balanced.Millisecond, Balanced.Microsecond, Balanced.Nanosecond);
  except
    on E: ETemporalDurationInt64Overflow do
      ThrowRangeError(E.Message, SSuggestTemporalDurationRange);
  end;
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
  RIncrement := GetRoundingIncrement(OptionsObj, 1);
  RMode := GetRoundingMode(OptionsObj, rmTrunc);
  SmallestUnit := GetSmallestUnit(OptionsObj, tuNanosecond);

  if LargestUnit = tuAuto then LargestUnit := tuHour;
  if not (LargestUnit in [tuHour, tuMinute, tuSecond, tuMillisecond, tuMicrosecond, tuNanosecond]) then
    ThrowRangeError(Format(SErrorTemporalInvalidUnitFor, [AMethodName, 'largestUnit']), SSuggestTemporalValidUnits);

  if not (SmallestUnit in [tuHour, tuMinute, tuSecond, tuMillisecond, tuMicrosecond, tuNanosecond]) then
    ThrowRangeError(Format(SErrorTemporalInvalidUnitFor, [AMethodName, 'smallestUnit']), SSuggestTemporalValidUnits);
  if Ord(LargestUnit) > Ord(SmallestUnit) then
    ThrowRangeError(SErrorDurationRoundLargestSmallerThanSmallest, SSuggestTemporalRoundArg);
  ValidateRoundingIncrement(RIncrement, SmallestUnit);

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
  DurTimeNs: TBigInteger;
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
    if not (SmallestUnit in [tuHour, tuMinute, tuSecond, tuMillisecond, tuMicrosecond, tuNanosecond]) then
      ThrowRangeError(Format(SErrorTemporalInvalidUnitFor, ['PlainTime.prototype.round', 'smallestUnit']), SSuggestTemporalValidUnits);
  end
  else if Arg is TGocciaObjectValue then
  begin
    OptionsObj := TGocciaObjectValue(Arg);
    Increment := GetRoundingIncrement(OptionsObj, 1);
    Mode := GetRoundingMode(OptionsObj, rmHalfExpand);
    SmallestUnit := GetSmallestUnit(OptionsObj, tuNone);
    if SmallestUnit = tuNone then
      ThrowRangeError(SErrorTemporalRoundRequiresSmallestUnit, SSuggestTemporalRoundArg);
    if not (SmallestUnit in [tuHour, tuMinute, tuSecond, tuMillisecond, tuMicrosecond, tuNanosecond]) then
      ThrowRangeError(Format(SErrorTemporalInvalidUnitFor, ['PlainTime.prototype.round', UnitStr]), SSuggestTemporalValidUnits);
  end
  else
  begin
    ThrowTypeError(Format(SErrorTemporalRoundRequiresStringOrOptions, ['PlainTime']), SSuggestTemporalRoundArg);
    SmallestUnit := tuNanosecond;
  end;

  ValidateRoundingIncrement(Increment, SmallestUnit);

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
begin
  AsPlainTime(AThisValue, 'PlainTime.prototype.toLocaleString');
  Result := FormatTemporalValueToLocaleString(AThisValue, AArgs.GetElement(0),
    AArgs.GetElement(1));
end;

initialization
  GTemporalPlainTimeSharedSlot := RegisterRealmOwnedSlot('Temporal.PlainTime.shared');

end.
