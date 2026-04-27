unit Goccia.Values.TemporalInstant;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.ObjectModel,
  Goccia.SharedPrototype,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaTemporalInstantValue = class(TGocciaObjectValue)
  private
    FEpochMilliseconds: Int64;
    FSubMillisecondNanoseconds: Integer;

    procedure InitializePrototype;
  public
    constructor Create(const AEpochMilliseconds: Int64; const ASubMillisecondNanoseconds: Integer); overload;

    function ToStringTag: string; override;
    class procedure ExposePrototype(const AConstructor: TGocciaObjectValue);

    property EpochMilliseconds: Int64 read FEpochMilliseconds;
    property SubMillisecondNanoseconds: Integer read FSubMillisecondNanoseconds;
  published
    function GetEpochMilliseconds(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetEpochNanoseconds(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function InstantAdd(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function InstantSubtract(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function InstantUntil(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function InstantSince(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function InstantRound(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function InstantEquals(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function InstantToString(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function InstantToLocaleString(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function InstantToJSON(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function InstantValueOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function InstantToZonedDateTimeISO(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  end;

implementation

uses
  SysUtils,

  BigInteger,

  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.Realm,
  Goccia.Temporal.Options,
  Goccia.Temporal.Utils,
  Goccia.Values.BigIntValue,
  Goccia.Values.ErrorHelper,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.SymbolValue,
  Goccia.Values.TemporalDuration,
  Goccia.Values.TemporalZonedDateTime;

var
  GTemporalInstantSharedSlot: TGocciaRealmOwnedSlotId;

threadvar
  FPrototypeMembers: TArray<TGocciaMemberDefinition>;

function GetTemporalInstantShared: TGocciaSharedPrototype; inline;
begin
  if Assigned(CurrentRealm) then
    Result := TGocciaSharedPrototype(CurrentRealm.GetOwnedSlot(GTemporalInstantSharedSlot))
  else
    Result := nil;
end;

function AsInstant(const AValue: TGocciaValue; const AMethod: string): TGocciaTemporalInstantValue;
begin
  if not (AValue is TGocciaTemporalInstantValue) then
    ThrowTypeError(AMethod + ' called on non-Instant', SSuggestTemporalThisType);
  Result := TGocciaTemporalInstantValue(AValue);
end;

function CoerceInstant(const AValue: TGocciaValue; const AMethod: string): TGocciaTemporalInstantValue;
var
  DateRec: TTemporalDateRecord;
  TimeRec: TTemporalTimeRecord;
  OffsetSeconds: Integer;
  EpochMs: Int64;
  SubMs: Integer;
begin
  if AValue is TGocciaTemporalInstantValue then
    Result := TGocciaTemporalInstantValue(AValue)
  else if AValue is TGocciaStringLiteralValue then
  begin
    if not CoerceToISOInstant(TGocciaStringLiteralValue(AValue).Value, DateRec, TimeRec, OffsetSeconds) then
      ThrowRangeError(Format(SErrorTemporalInvalidISOStringFor, ['instant', AMethod]), SSuggestTemporalISOFormat);
    EpochMs := DateToEpochDays(DateRec.Year, DateRec.Month, DateRec.Day) * Int64(86400000) +
               Int64(TimeRec.Hour) * 3600000 + Int64(TimeRec.Minute) * 60000 +
               Int64(TimeRec.Second) * 1000 + TimeRec.Millisecond;
    // Adjust for UTC offset
    EpochMs := EpochMs - Int64(OffsetSeconds) * 1000;
    SubMs := TimeRec.Microsecond * 1000 + TimeRec.Nanosecond;
    Result := TGocciaTemporalInstantValue.Create(EpochMs, SubMs);
  end
  else
  begin
    ThrowTypeError(AMethod + ' requires an Instant or string', SSuggestTemporalFromArg);
    Result := nil;
  end;
end;

{ TGocciaTemporalInstantValue }

constructor TGocciaTemporalInstantValue.Create(const AEpochMilliseconds: Int64; const ASubMillisecondNanoseconds: Integer);
var
  Shared: TGocciaSharedPrototype;
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
  Shared := GetTemporalInstantShared;
  if Assigned(Shared) then
    FPrototype := Shared.Prototype;
end;

procedure TGocciaTemporalInstantValue.InitializePrototype;
var
  Members: TGocciaMemberCollection;
  Shared: TGocciaSharedPrototype;
begin
  if not Assigned(CurrentRealm) then Exit;
  if Assigned(GetTemporalInstantShared) then Exit;
  Shared := TGocciaSharedPrototype.Create(Self);
  CurrentRealm.SetOwnedSlot(GTemporalInstantSharedSlot, Shared);
  if Length(FPrototypeMembers) = 0 then
  begin
    Members := TGocciaMemberCollection.Create;
    try
      Members.AddAccessor('epochMilliseconds', GetEpochMilliseconds, nil, [pfConfigurable]);
      Members.AddAccessor('epochNanoseconds', GetEpochNanoseconds, nil, [pfConfigurable]);
      Members.AddMethod(InstantAdd, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(InstantSubtract, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(InstantUntil, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(InstantSince, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(InstantRound, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(InstantEquals, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(InstantToString, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(InstantToLocaleString, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(InstantToJSON, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(InstantValueOf, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(InstantToZonedDateTimeISO, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddSymbolDataProperty(
        TGocciaSymbolValue.WellKnownToStringTag,
        TGocciaStringLiteralValue.Create('Temporal.Instant'),
        [pfConfigurable]);
      FPrototypeMembers := Members.ToDefinitions;
    finally
      Members.Free;
    end;
  end;
  RegisterMemberDefinitions(Shared.Prototype, FPrototypeMembers);
end;

class procedure TGocciaTemporalInstantValue.ExposePrototype(const AConstructor: TGocciaObjectValue);
var
  Shared: TGocciaSharedPrototype;
begin
  Shared := GetTemporalInstantShared;
  if not Assigned(Shared) then
  begin
    TGocciaTemporalInstantValue.Create(0, 0);
    Shared := GetTemporalInstantShared;
  end;
  if Assigned(Shared) then
    ExposeSharedPrototypeOnConstructor(Shared, AConstructor);
end;

function TGocciaTemporalInstantValue.ToStringTag: string;
begin
  Result := 'Temporal.Instant';
end;

{ Getters }

function TGocciaTemporalInstantValue.GetEpochMilliseconds(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaNumberLiteralValue.Create(AsInstant(AThisValue, 'get Instant.epochMilliseconds').FEpochMilliseconds);
end;

function TGocciaTemporalInstantValue.GetEpochNanoseconds(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Inst: TGocciaTemporalInstantValue;
  BigNs: TBigInteger;
begin
  Inst := AsInstant(AThisValue, 'get Instant.epochNanoseconds');
  BigNs := TBigInteger.FromInt64(Inst.FEpochMilliseconds)
    .Multiply(TBigInteger.FromInt64(1000000))
    .Add(TBigInteger.FromInt64(Inst.FSubMillisecondNanoseconds));
  Result := TGocciaBigIntValue.Create(BigNs);
end;

{ Methods }

function TGocciaTemporalInstantValue.InstantAdd(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Inst: TGocciaTemporalInstantValue;
  Dur: TGocciaTemporalDurationValue;
  Arg: TGocciaValue;
  DurRec: TTemporalDurationRecord;
  NewMs: Int64;
  NewSubMs: Integer;
begin
  Inst := AsInstant(AThisValue, 'Instant.prototype.add');
  Arg := AArgs.GetElement(0);

  if Arg is TGocciaTemporalDurationValue then
    Dur := TGocciaTemporalDurationValue(Arg)
  else if Arg is TGocciaStringLiteralValue then
  begin
    if not TryParseISODuration(TGocciaStringLiteralValue(Arg).Value, DurRec) then
      ThrowRangeError(SErrorInvalidDurationString, SSuggestTemporalDurationArg);
    Dur := TGocciaTemporalDurationValue.Create(0, 0, 0, DurRec.Days,
      DurRec.Hours, DurRec.Minutes, DurRec.Seconds,
      DurRec.Milliseconds, DurRec.Microseconds, DurRec.Nanoseconds);
  end
  else
  begin
    ThrowTypeError(Format(SErrorTemporalAddRequiresDuration, ['Instant']), SSuggestTemporalDurationArg);
    Dur := nil;
  end;

  // Instant only supports time-based duration
  if (Dur.Years <> 0) or (Dur.Months <> 0) or (Dur.Weeks <> 0) then
    ThrowRangeError(SErrorInstantAddNoCalendar, SSuggestTemporalDurationArg);

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

function TGocciaTemporalInstantValue.InstantSubtract(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Inst: TGocciaTemporalInstantValue;
  Dur: TGocciaTemporalDurationValue;
  Arg: TGocciaValue;
  DurRec: TTemporalDurationRecord;
  NewMs: Int64;
  NewSubMs: Integer;
begin
  Inst := AsInstant(AThisValue, 'Instant.prototype.subtract');
  Arg := AArgs.GetElement(0);

  if Arg is TGocciaTemporalDurationValue then
    Dur := TGocciaTemporalDurationValue(Arg)
  else if Arg is TGocciaStringLiteralValue then
  begin
    if not TryParseISODuration(TGocciaStringLiteralValue(Arg).Value, DurRec) then
      ThrowRangeError(SErrorInvalidDurationString, SSuggestTemporalDurationArg);
    Dur := TGocciaTemporalDurationValue.Create(0, 0, 0, DurRec.Days,
      DurRec.Hours, DurRec.Minutes, DurRec.Seconds,
      DurRec.Milliseconds, DurRec.Microseconds, DurRec.Nanoseconds);
  end
  else
  begin
    ThrowTypeError(Format(SErrorTemporalSubtractRequiresDuration, ['Instant']), SSuggestTemporalDurationArg);
    Dur := nil;
  end;

  if (Dur.Years <> 0) or (Dur.Months <> 0) or (Dur.Weeks <> 0) then
    ThrowRangeError(SErrorInstantSubtractNoCalendar, SSuggestTemporalDurationArg);

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

function InstantDiffToUnits(const ADiffMs: Int64; const ADiffSubMs: Integer;
  const ALargestUnit: TTemporalUnit): TGocciaValue;
var
  Ms, SubMs: Int64;
  TotalSec, TotalMin: Int64;
begin
  Ms := ADiffMs;
  SubMs := ADiffSubMs;

  case ALargestUnit of
    tuHour:
      Result := TGocciaTemporalDurationValue.Create(0, 0, 0, 0,
        Ms div 3600000,
        (Ms mod 3600000) div 60000,
        ((Ms mod 3600000) mod 60000) div 1000,
        ((Ms mod 3600000) mod 60000) mod 1000,
        SubMs div 1000,
        SubMs mod 1000);
    tuMinute:
    begin
      TotalMin := Ms div 60000;
      Ms := Ms mod 60000;
      Result := TGocciaTemporalDurationValue.Create(0, 0, 0, 0, 0,
        TotalMin,
        Ms div 1000,
        Ms mod 1000,
        SubMs div 1000,
        SubMs mod 1000);
    end;
    tuSecond:
    begin
      TotalSec := Ms div 1000;
      Ms := Ms mod 1000;
      Result := TGocciaTemporalDurationValue.Create(0, 0, 0, 0, 0, 0,
        TotalSec,
        Ms,
        SubMs div 1000,
        SubMs mod 1000);
    end;
    tuMillisecond:
      Result := TGocciaTemporalDurationValue.Create(0, 0, 0, 0, 0, 0, 0,
        Ms,
        SubMs div 1000,
        SubMs mod 1000);
    tuMicrosecond:
      Result := TGocciaTemporalDurationValue.Create(0, 0, 0, 0, 0, 0, 0, 0,
        Ms * 1000 + SubMs div 1000,
        SubMs mod 1000);
  else // tuNanosecond
    Result := TGocciaTemporalDurationValue.Create(0, 0, 0, 0, 0, 0, 0, 0, 0,
      Ms * 1000000 + SubMs);
  end;
end;

function TGocciaTemporalInstantValue.InstantUntil(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Inst, Other: TGocciaTemporalInstantValue;
  OptionsObj: TGocciaObjectValue;
  LargestUnit: TTemporalUnit;
  DiffMs: Int64;
  DiffSubMs: Integer;
begin
  Inst := AsInstant(AThisValue, 'Instant.prototype.until');
  Other := CoerceInstant(AArgs.GetElement(0), 'Instant.prototype.until');

  OptionsObj := GetDiffOptions(AArgs, 1);
  LargestUnit := GetLargestUnit(OptionsObj, tuHour);
  if LargestUnit = tuAuto then LargestUnit := tuHour;
  if not (LargestUnit in [tuHour, tuMinute, tuSecond, tuMillisecond, tuMicrosecond, tuNanosecond]) then
    ThrowRangeError(Format(SErrorTemporalInvalidUnitFor, ['Instant.prototype.until', 'largestUnit']), SSuggestTemporalValidUnits);

  DiffMs := Other.FEpochMilliseconds - Inst.FEpochMilliseconds;
  DiffSubMs := Other.FSubMillisecondNanoseconds - Inst.FSubMillisecondNanoseconds;

  // Normalize so DiffMs and DiffSubMs share the same sign
  if (DiffMs > 0) and (DiffSubMs < 0) then
  begin
    Dec(DiffMs);
    Inc(DiffSubMs, 1000000);
  end
  else if (DiffMs < 0) and (DiffSubMs > 0) then
  begin
    Inc(DiffMs);
    Dec(DiffSubMs, 1000000);
  end;

  Result := InstantDiffToUnits(DiffMs, DiffSubMs, LargestUnit);
end;

function TGocciaTemporalInstantValue.InstantSince(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Inst, Other: TGocciaTemporalInstantValue;
  OptionsObj: TGocciaObjectValue;
  LargestUnit: TTemporalUnit;
  DiffMs: Int64;
  DiffSubMs: Integer;
begin
  Inst := AsInstant(AThisValue, 'Instant.prototype.since');
  Other := CoerceInstant(AArgs.GetElement(0), 'Instant.prototype.since');

  OptionsObj := GetDiffOptions(AArgs, 1);
  LargestUnit := GetLargestUnit(OptionsObj, tuHour);
  if LargestUnit = tuAuto then LargestUnit := tuHour;
  if not (LargestUnit in [tuHour, tuMinute, tuSecond, tuMillisecond, tuMicrosecond, tuNanosecond]) then
    ThrowRangeError(Format(SErrorTemporalInvalidUnitFor, ['Instant.prototype.since', 'largestUnit']), SSuggestTemporalValidUnits);

  DiffMs := Inst.FEpochMilliseconds - Other.FEpochMilliseconds;
  DiffSubMs := Inst.FSubMillisecondNanoseconds - Other.FSubMillisecondNanoseconds;

  // Normalize so DiffMs and DiffSubMs share the same sign
  if (DiffMs > 0) and (DiffSubMs < 0) then
  begin
    Dec(DiffMs);
    Inc(DiffSubMs, 1000000);
  end
  else if (DiffMs < 0) and (DiffSubMs > 0) then
  begin
    Inc(DiffMs);
    Dec(DiffSubMs, 1000000);
  end;

  Result := InstantDiffToUnits(DiffMs, DiffSubMs, LargestUnit);
end;

function TGocciaTemporalInstantValue.InstantRound(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Inst: TGocciaTemporalInstantValue;
  UnitStr: string;
  Arg: TGocciaValue;
  OptionsObj: TGocciaObjectValue;
  NewMs: Int64;
  NewSubMs: Integer;
  SmallestUnit: TTemporalUnit;
  Mode: TTemporalRoundingMode;
  Increment: Integer;
  BigTotal, BigDivisor, BigRounded, BigMillion: TBigInteger;
begin
  Inst := AsInstant(AThisValue, 'Instant.prototype.round');
  Arg := AArgs.GetElement(0);

  Mode := rmHalfExpand;
  Increment := 1;

  if Arg is TGocciaStringLiteralValue then
  begin
    UnitStr := TGocciaStringLiteralValue(Arg).Value;
    if not GetTemporalUnitFromString(UnitStr, SmallestUnit) then
      ThrowRangeError(Format(SErrorTemporalInvalidUnitFor, ['Instant.prototype.round', UnitStr]), SSuggestTemporalValidUnits);
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
    ThrowTypeError(Format(SErrorTemporalRoundRequiresStringOrOptions, ['Instant']), SSuggestTemporalRoundArg);
    SmallestUnit := tuNanosecond;
  end;

  // Use TBigInteger to avoid Int64 overflow on epochMs * 1000000
  BigMillion := TBigInteger.FromInt64(1000000);
  BigTotal := TBigInteger.FromInt64(Inst.FEpochMilliseconds)
    .Multiply(BigMillion)
    .Add(TBigInteger.FromInt64(Inst.FSubMillisecondNanoseconds));
  BigDivisor := TBigInteger.FromInt64(UnitToNanoseconds(SmallestUnit) * Increment);
  BigRounded := RoundBigIntWithMode(BigTotal, BigDivisor, Mode);

  NewMs := BigRounded.Divide(BigMillion).ToInt64;
  NewSubMs := Integer(BigRounded.Modulo(BigMillion).ToInt64);

  Result := TGocciaTemporalInstantValue.Create(NewMs, NewSubMs);
end;

function TGocciaTemporalInstantValue.InstantEquals(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Inst, Other: TGocciaTemporalInstantValue;
begin
  Inst := AsInstant(AThisValue, 'Instant.prototype.equals');
  Other := CoerceInstant(AArgs.GetElement(0), 'Instant.prototype.equals');

  if (Inst.FEpochMilliseconds = Other.FEpochMilliseconds) and
     (Inst.FSubMillisecondNanoseconds = Other.FSubMillisecondNanoseconds) then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;

function TGocciaTemporalInstantValue.InstantToString(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Inst: TGocciaTemporalInstantValue;
  DateRec: TTemporalDateRecord;
  EpochDays, RemainingMs: Int64;
  Hour, Minute, Second, Ms: Integer;
  Us, Ns: Integer;
  Arg: TGocciaValue;
  OptionsObj: TGocciaObjectValue;
  FracDigits, ExtraDays: Integer;
  Mode: TTemporalRoundingMode;
  S: string;
begin
  Inst := AsInstant(AThisValue, 'Instant.prototype.toString');

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

  OptionsObj := nil;
  Arg := AArgs.GetElement(0);
  if Assigned(Arg) and not (Arg is TGocciaUndefinedLiteralValue) then
  begin
    if not (Arg is TGocciaObjectValue) then
      ThrowTypeError('options must be an object or undefined', SSuggestTemporalFromArg);
    OptionsObj := TGocciaObjectValue(Arg);
  end;
  ResolveTemporalToStringOptions(OptionsObj, FracDigits, Mode);
  ExtraDays := 0;
  RoundTimeForToString(Hour, Minute, Second, Ms, Us, Ns, ExtraDays, FracDigits, Mode);
  if ExtraDays <> 0 then
    DateRec := AddDaysToDate(DateRec.Year, DateRec.Month, DateRec.Day, ExtraDays);

  if FracDigits = -2 then // smallestUnit: minute
    S := FormatDateString(DateRec.Year, DateRec.Month, DateRec.Day) + 'T' +
         PadTwo(Hour) + ':' + PadTwo(Minute) + 'Z'
  else
    S := FormatDateString(DateRec.Year, DateRec.Month, DateRec.Day) + 'T' +
         FormatTimeWithPrecision(Hour, Minute, Second, Ms, Us, Ns, FracDigits) + 'Z';

  Result := TGocciaStringLiteralValue.Create(S);
end;

function TGocciaTemporalInstantValue.InstantToJSON(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Inst: TGocciaTemporalInstantValue;
  DateRec: TTemporalDateRecord;
  EpochDays, RemainingMs: Int64;
  Hour, Minute, Second, Ms: Integer;
  Us, Ns: Integer;
begin
  Inst := AsInstant(AThisValue, 'Instant.prototype.toJSON');

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

  Result := TGocciaStringLiteralValue.Create(
    FormatDateString(DateRec.Year, DateRec.Month, DateRec.Day) + 'T' +
    FormatTimeString(Hour, Minute, Second, Ms, Us, Ns) + 'Z');
end;

function TGocciaTemporalInstantValue.InstantValueOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  ThrowTypeError(Format(SErrorTemporalValueOf, ['Instant', 'epochMilliseconds, epochNanoseconds, or compare']), SSuggestTemporalNoValueOf);
  Result := nil;
end;

function TGocciaTemporalInstantValue.InstantToLocaleString(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  EmptyArgs: TGocciaArgumentsCollection;
begin
  EmptyArgs := TGocciaArgumentsCollection.Create([]);
  try
    Result := InstantToString(EmptyArgs, AThisValue);
  finally
    EmptyArgs.Free;
  end;
end;

function TGocciaTemporalInstantValue.InstantToZonedDateTimeISO(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Inst: TGocciaTemporalInstantValue;
  Arg: TGocciaValue;
  TZ: string;
begin
  Inst := AsInstant(AThisValue, 'Instant.prototype.toZonedDateTimeISO');
  if AArgs.Length < 1 then
    ThrowTypeError('Instant.prototype.toZonedDateTimeISO requires a time zone argument', SSuggestTemporalTimezone);
  Arg := AArgs.GetElement(0);
  if Arg is TGocciaTemporalZonedDateTimeValue then
    TZ := TGocciaTemporalZonedDateTimeValue(Arg).TimeZone
  else if Arg is TGocciaStringLiteralValue then
    TZ := TGocciaStringLiteralValue(Arg).Value
  else
  begin
    ThrowTypeError('Instant.prototype.toZonedDateTimeISO requires a string time zone or ZonedDateTime', SSuggestTemporalTimezone);
    TZ := '';
  end;
  Result := TGocciaTemporalZonedDateTimeValue.Create(
    Inst.FEpochMilliseconds, Inst.FSubMillisecondNanoseconds, TZ);
end;

initialization
  GTemporalInstantSharedSlot := RegisterRealmOwnedSlot('Temporal.Instant.shared');

end.
