unit Goccia.Builtins.Temporal;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.Builtins.Base,
  Goccia.Error.ThrowErrorCallback,
  Goccia.HostEnvironment,
  Goccia.ObjectModel,
  Goccia.Scope,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaTemporalBuiltin = class(TGocciaBuiltin)
  private
    FTemporalNamespace: TGocciaObjectValue;
    FHostEnvironment: TGocciaHostEnvironment;

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
    constructor Create(const AName: string; const AScope: TGocciaScope;
      const AThrowError: TGocciaThrowErrorCallback;
      const AHostEnvironment: TGocciaHostEnvironment;
      const ADefineGlobalBinding: Boolean = True);

    property TemporalNamespace: TGocciaObjectValue read FTemporalNamespace;
  end;

implementation

uses
  DateUtils,
  SysUtils,

  BigInteger,

  Goccia.Constants.PropertyNames,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.GarbageCollector,
  Goccia.Temporal.AbstractOperations,
  Goccia.Temporal.Calendar,
  Goccia.Temporal.DurationMath,
  Goccia.Temporal.Options,
  Goccia.Temporal.TimeZone,
  Goccia.Temporal.Utils,
  Goccia.Utils,
  Goccia.Values.BigIntValue,
  Goccia.Values.ErrorHelper,
  Goccia.Values.HoleValue,
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
  Goccia.Values.TemporalZonedDateTime,
  Goccia.Values.ToPrimitive;

const
  PLAIN_MONTH_DAY_DEFAULT_REFERENCE_YEAR = 1972;

{ TGocciaTemporalBuiltin }

constructor TGocciaTemporalBuiltin.Create(const AName: string;
  const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback;
  const AHostEnvironment: TGocciaHostEnvironment;
  const ADefineGlobalBinding: Boolean = True);
var
  TemporalMembers: array[0..0] of TGocciaMemberDefinition;
begin
  inherited Create(AName, AScope, AThrowError);
  FHostEnvironment := AHostEnvironment;

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

    if ADefineGlobalBinding then
      AScope.DefineLexicalBinding(AName, FTemporalNamespace, dtLet, True);
  finally
    TGarbageCollector.Instance.RemoveTempRoot(FTemporalNamespace);
  end;
end;

function TemporalOverflowOptionFromArgs(const AArgs: TGocciaArgumentsCollection;
  const AIndex: Integer): TTemporalOverflow;
var
  OptionsArg: TGocciaValue;
begin
  Result := toConstrain;
  OptionsArg := AArgs.GetElement(AIndex);
  if (OptionsArg = nil) or (OptionsArg is TGocciaUndefinedLiteralValue) then
    Exit;
  if not (OptionsArg is TGocciaObjectValue) then
    ThrowTypeError('Options argument must be an object or undefined',
      SSuggestTemporalFromArg);
  Result := GetOverflowOption(TGocciaObjectValue(OptionsArg));
end;

function TemporalDateBefore(const AY, AM, AD, ABY, ABM, ABD: Integer): Boolean;
begin
  Result := (AY < ABY) or ((AY = ABY) and
    ((AM < ABM) or ((AM = ABM) and (AD < ABD))));
end;

function TemporalISOYearMonthInSupportedRange(const ADate: TTemporalDateRecord): Boolean;
begin
  Result := not TemporalDateBefore(ADate.Year, ADate.Month, 1,
    -271821, 4, 1) and
    not TemporalDateBefore(275760, 9, 1, ADate.Year, ADate.Month, 1);
end;

function TemporalISODateInSupportedRange(const AYear, AMonth, ADay: Integer): Boolean;
begin
  Result := not TemporalDateBefore(AYear, AMonth, ADay, -271821, 4, 19) and
    not TemporalDateBefore(275760, 9, 13, AYear, AMonth, ADay);
end;

function TemporalISODateTimeInSupportedRange(const AYear, AMonth, ADay,
  AHour, AMinute, ASecond, AMillisecond, AMicrosecond, ANanosecond: Integer): Boolean;
begin
  Result := False;
  if not TemporalISODateInSupportedRange(AYear, AMonth, ADay) then
    Exit;
  if (AYear = -271821) and (AMonth = 4) and (ADay = 19) and
     (AHour = 0) and (AMinute = 0) and (ASecond = 0) and
     (AMillisecond = 0) and (AMicrosecond = 0) and (ANanosecond = 0) then
    Exit;
  Result := True;
end;

function TemporalStringHasUTCDesignator(const AValue: string): Boolean;
var
  S: string;
begin
  S := StripAnnotations(AValue);
  Result := (Length(S) > 0) and ((S[Length(S)] = 'Z') or
    (S[Length(S)] = 'z'));
end;

function TemporalStringLooksLikeDateTime(const AValue: string): Boolean;
begin
  Result := (AValue <> '') and
    (((AValue[1] >= '0') and (AValue[1] <= '9')) or
     (AValue[1] = '+') or (AValue[1] = '-')) and
    ((System.Pos('T', AValue) > 0) or (System.Pos('t', AValue) > 0) or
     (System.Pos(' ', AValue) > 0));
end;

procedure RequireTemporalConstructorNew(const AThisValue: TGocciaValue;
  const AName: string);
begin
  if not (AThisValue is TGocciaHoleValue) then
    ThrowTypeError('Temporal.' + AName + ' cannot be called without new',
      SSuggestRequiresNew);
end;

function TemporalToBigInt(const AValue: TGocciaValue): TBigInteger;
var
  Prim: TGocciaValue;
begin
  Prim := AValue;
  if Prim is TGocciaObjectValue then
    Prim := ToPrimitive(Prim, tphNumber);

  if Prim is TGocciaBigIntValue then
    Exit(TGocciaBigIntValue(Prim).Value);

  if Prim is TGocciaBooleanLiteralValue then
  begin
    if TGocciaBooleanLiteralValue(Prim).Value then
      Exit(TBigInteger.One);
    Exit(TBigInteger.Zero);
  end;

  if Prim is TGocciaStringLiteralValue then
  begin
    if TryStringToBigInt(TGocciaStringLiteralValue(Prim).Value, Result) then
      Exit;
    ThrowSyntaxError(Format(SErrorBigIntInvalidConversion, [
      '''' + TGocciaStringLiteralValue(Prim).Value + '''']),
      SSuggestBigIntNoImplicitConversion);
    Exit(TBigInteger.Zero);
  end;

  ThrowTypeError(Format(SErrorBigIntInvalidConversion, [Prim.TypeName]),
    SSuggestBigIntNoImplicitConversion);
  Result := TBigInteger.Zero;
end;

function RequireTemporalMonthCodeString(const AValue: TGocciaValue;
  const AMethod: string): string;
var
  Prim: TGocciaValue;
begin
  if AValue is TGocciaStringLiteralValue then
    Exit(TGocciaStringLiteralValue(AValue).Value);

  if AValue is TGocciaObjectValue then
  begin
    Prim := ToPrimitive(AValue, tphString);
    if Prim is TGocciaStringLiteralValue then
      Exit(TGocciaStringLiteralValue(Prim).Value);
  end;

  ThrowTypeError(Format(SErrorInvalidMonthCodeFor, [AMethod]),
    SSuggestTemporalMonthCode);
  Result := '';
end;

function TemporalNowTimeZoneFromArgument(const AArg: TGocciaValue;
  const AMethod, ADefaultTimeZone: string): string;
begin
  if (AArg = nil) or (AArg is TGocciaUndefinedLiteralValue) then
    Exit(ADefaultTimeZone);

  if not (AArg is TGocciaStringLiteralValue) then
    ThrowTypeError(AMethod + ' timeZone must be a string',
      SSuggestTemporalTimezone);

  Result := CanonicalizeTemporalTimeZoneIdentifier(
    TGocciaStringLiteralValue(AArg).Value);
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

procedure ConstrainTemporalTime(var AHour, AMinute, ASecond, AMillisecond,
  AMicrosecond, ANanosecond: Integer);
begin
  AHour := ClampTemporalInteger(AHour, 0, 23);
  AMinute := ClampTemporalInteger(AMinute, 0, 59);
  ASecond := ClampTemporalInteger(ASecond, 0, 59);
  AMillisecond := ClampTemporalInteger(AMillisecond, 0, 999);
  AMicrosecond := ClampTemporalInteger(AMicrosecond, 0, 999);
  ANanosecond := ClampTemporalInteger(ANanosecond, 0, 999);
end;

procedure GetLocalFieldsFromEpoch(const AEpochMilliseconds: Int64;
  const ASubMillisecondNanoseconds: Integer; const ATimeZone: string;
  out AYear, AMonth, ADay, AHour, AMinute, ASecond, AMillisecond,
      AMicrosecond, ANanosecond: Integer);
var
  OffsetSeconds: Integer;
  LocalMilliseconds, EpochDays, RemainingMilliseconds: Int64;
  DateRec: TTemporalDateRecord;
begin
  OffsetSeconds := GetUtcOffsetSeconds(ATimeZone, AEpochMilliseconds div 1000);
  LocalMilliseconds := AEpochMilliseconds + Int64(OffsetSeconds) * 1000;

  EpochDays := LocalMilliseconds div 86400000;
  RemainingMilliseconds := LocalMilliseconds mod 86400000;
  if RemainingMilliseconds < 0 then
  begin
    Dec(EpochDays);
    Inc(RemainingMilliseconds, 86400000);
  end;

  DateRec := EpochDaysToDate(EpochDays);
  AYear := DateRec.Year;
  AMonth := DateRec.Month;
  ADay := DateRec.Day;
  AHour := Integer(RemainingMilliseconds div 3600000);
  RemainingMilliseconds := RemainingMilliseconds mod 3600000;
  AMinute := Integer(RemainingMilliseconds div 60000);
  RemainingMilliseconds := RemainingMilliseconds mod 60000;
  ASecond := Integer(RemainingMilliseconds div 1000);
  AMillisecond := Integer(RemainingMilliseconds mod 1000);
  AMicrosecond := ASubMillisecondNanoseconds div 1000;
  ANanosecond := ASubMillisecondNanoseconds mod 1000;
end;

procedure GetZonedDateTimeLocalFields(const AZdt: TGocciaTemporalZonedDateTimeValue;
  out AYear, AMonth, ADay, AHour, AMinute, ASecond, AMillisecond,
      AMicrosecond, ANanosecond: Integer);
begin
  GetLocalFieldsFromEpoch(AZdt.EpochMilliseconds,
    AZdt.SubMillisecondNanoseconds, AZdt.TimeZone, AYear, AMonth, ADay,
    AHour, AMinute, ASecond, AMillisecond, AMicrosecond, ANanosecond);
end;

{ Duration }

procedure TGocciaTemporalBuiltin.RegisterDuration;
var
  ConstructorMethod: TGocciaNativeFunctionValue;
begin
  ConstructorMethod := TGocciaNativeFunctionValue.Create(DurationConstructorFn, 'Duration', 0);
  TGocciaTemporalDurationValue.ExposePrototype(ConstructorMethod);
  ConstructorMethod.DefineProperty('from',
    TGocciaPropertyDescriptorData.Create(
      TGocciaNativeFunctionValue.CreateWithoutPrototype(DurationFrom, 'from', 1),
      [pfConfigurable, pfWritable]));
  ConstructorMethod.DefineProperty('compare',
    TGocciaPropertyDescriptorData.Create(
      TGocciaNativeFunctionValue.CreateWithoutPrototype(DurationCompare, 'compare', 2),
      [pfConfigurable, pfWritable]));
  FTemporalNamespace.DefineProperty('Duration',
    TGocciaPropertyDescriptorData.Create(ConstructorMethod, [pfConfigurable, pfWritable]));
end;

function TGocciaTemporalBuiltin.DurationConstructorFn(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Years, Months, Weeks, Days, Hours, Minutes, Seconds, Milliseconds,
    Microseconds, Nanoseconds: TBigInteger;

  // Components may exceed Int64 (the spec allows any float64 integer whose
  // normalized total stays under 2^53 seconds), so read into TBigInteger.
  function GetArgOr(const AIndex: Integer): TBigInteger;
  var
    V: TGocciaValue;
  begin
    if AIndex < AArgs.Length then
    begin
      V := AArgs.GetElement(AIndex);
      if (V is TGocciaUndefinedLiteralValue) then
        Result := TBigInteger.Zero
      else
        Result := DurationFieldToBigInteger(V);
    end
    else
      Result := TBigInteger.Zero;
  end;

begin
  RequireTemporalConstructorNew(AThisValue, 'Duration');
  Years := GetArgOr(0);
  Months := GetArgOr(1);
  Weeks := GetArgOr(2);
  Days := GetArgOr(3);
  Hours := GetArgOr(4);
  Minutes := GetArgOr(5);
  Seconds := GetArgOr(6);
  Milliseconds := GetArgOr(7);
  Microseconds := GetArgOr(8);
  Nanoseconds := GetArgOr(9);
  Result := TGocciaTemporalDurationValue.CreateFromBigIntegers(
    Years, Months, Weeks, Days, Hours, Minutes, Seconds, Milliseconds,
    Microseconds, Nanoseconds);
end;

function TGocciaTemporalBuiltin.DurationFrom(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Arg: TGocciaValue;
  D: TGocciaTemporalDurationValue;
begin
  Arg := AArgs.GetElement(0);

  if Arg is TGocciaTemporalDurationValue then
  begin
    D := TGocciaTemporalDurationValue(Arg);
    Result := TGocciaTemporalDurationValue.CreateFromBigIntegers(
      D.YearsBig, D.MonthsBig, D.WeeksBig, D.DaysBig, D.HoursBig, D.MinutesBig,
      D.SecondsBig, D.MillisecondsBig, D.MicrosecondsBig, D.NanosecondsBig);
  end
  else if Arg is TGocciaStringLiteralValue then
    Result := DurationFromString(TGocciaStringLiteralValue(Arg).Value,
      SErrorInvalidISODuration)
  else if Arg is TGocciaObjectValue then
    Result := DurationFromObject(TGocciaObjectValue(Arg))
  else
  begin
    ThrowTypeError(SErrorTemporalDurationFromArg, SSuggestTemporalFromArg);
    Result := nil;
  end;
end;

function TGocciaTemporalBuiltin.DurationCompare(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D1, D2: TGocciaTemporalDurationValue;
  Comparison: Integer;

  function CoerceDuration(const AArg: TGocciaValue): TGocciaTemporalDurationValue;
  begin
    if AArg is TGocciaTemporalDurationValue then
      Result := TGocciaTemporalDurationValue(AArg)
    else if AArg is TGocciaStringLiteralValue then
      Result := DurationFromString(TGocciaStringLiteralValue(AArg).Value,
        SErrorInvalidISODuration)
    else if AArg is TGocciaObjectValue then
      Result := DurationFromObject(TGocciaObjectValue(AArg))
    else
    begin
      ThrowTypeError(SErrorTemporalDurationCompareArg, SSuggestTemporalCompareArg);
      Result := nil;
    end;
  end;

begin
  D1 := CoerceDuration(AArgs.GetElement(0));
  D2 := CoerceDuration(AArgs.GetElement(1));

  Comparison := CompareTemporalDurations(D1, D2, AArgs.GetElement(2));
  if Comparison < 0 then
    Result := TGocciaNumberLiteralValue.Create(-1)
  else if Comparison > 0 then
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
  ConstructorMethod.DefineProperty('from',
    TGocciaPropertyDescriptorData.Create(
      TGocciaNativeFunctionValue.CreateWithoutPrototype(InstantFrom, 'from', 1),
      [pfConfigurable, pfWritable]));
  ConstructorMethod.DefineProperty('fromEpochMilliseconds',
    TGocciaPropertyDescriptorData.Create(
      TGocciaNativeFunctionValue.CreateWithoutPrototype(InstantFromEpochMilliseconds, 'fromEpochMilliseconds', 1),
      [pfConfigurable, pfWritable]));
  ConstructorMethod.DefineProperty('fromEpochNanoseconds',
    TGocciaPropertyDescriptorData.Create(
      TGocciaNativeFunctionValue.CreateWithoutPrototype(InstantFromEpochNanoseconds, 'fromEpochNanoseconds', 1),
      [pfConfigurable, pfWritable]));
  ConstructorMethod.DefineProperty('compare',
    TGocciaPropertyDescriptorData.Create(
      TGocciaNativeFunctionValue.CreateWithoutPrototype(InstantCompare, 'compare', 2),
      [pfConfigurable, pfWritable]));
  FTemporalNamespace.DefineProperty('Instant',
    TGocciaPropertyDescriptorData.Create(ConstructorMethod, [pfConfigurable, pfWritable]));
end;

function TGocciaTemporalBuiltin.InstantConstructorFn(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Arg: TGocciaValue;
  BigNs, BigMs, BigSubMs: TBigInteger;
  Ms: Int64;
  SubMs: Integer;
begin
  if not (AThisValue is TGocciaHoleValue) then
    ThrowTypeError('Temporal.Instant cannot be called without new',
      SSuggestTemporalThisType);
  if AArgs.Length < 1 then
    ThrowTypeError(SErrorTemporalInstantRequiresEpoch, SSuggestTemporalFromArg);

  Arg := AArgs.GetElement(0);
  BigNs := TemporalToBigInt(Arg);
  if not IsValidEpochNanoseconds(BigNs) then
    ThrowRangeError(SErrorTemporalInstantOutOfRange, SSuggestTemporalInstantRange);
  BigMs := BigNs.Divide(TBigInteger.FromInt64(1000000));
  BigSubMs := BigNs.Modulo(TBigInteger.FromInt64(1000000));
  Ms := BigMs.ToInt64;
  SubMs := Integer(BigSubMs.ToInt64);

  Result := TGocciaTemporalInstantValue.Create(Ms, SubMs);
end;

function TGocciaTemporalBuiltin.InstantFrom(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Inst: TGocciaTemporalInstantValue;
begin
  Inst := CoerceTemporalInstant(AArgs.GetElement(0), 'Temporal.Instant.from');
  Result := TGocciaTemporalInstantValue.Create(Inst.EpochMilliseconds,
    Inst.SubMillisecondNanoseconds);
end;

function TGocciaTemporalBuiltin.InstantFromEpochMilliseconds(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
const
  MAX_INSTANT_EPOCH_MILLISECONDS = 8640000000000000.0;
var
  Arg: TGocciaValue;
  Num: TGocciaNumberLiteralValue;
begin
  Arg := AArgs.GetElement(0);
  if (Arg = nil) or (Arg is TGocciaUndefinedLiteralValue) then
    ThrowRangeError(SErrorTemporalInstantFromEpochMillis, SSuggestTemporalFromArg);

  Num := Arg.ToNumberLiteral;
  if Num.IsNaN or Num.IsInfinity or (Frac(Num.Value) <> 0) then
    ThrowRangeError(SErrorTemporalInstantFromEpochMillis, SSuggestTemporalFromArg);
  if (Num.Value < -MAX_INSTANT_EPOCH_MILLISECONDS) or
     (Num.Value > MAX_INSTANT_EPOCH_MILLISECONDS) then
    ThrowRangeError(SErrorTemporalInstantOutOfRange, SSuggestTemporalInstantRange);
  Result := TGocciaTemporalInstantValue.Create(Trunc(Num.Value), 0);
end;

function TGocciaTemporalBuiltin.InstantFromEpochNanoseconds(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Arg: TGocciaValue;
  BigNs, BigMs, BigSubMs: TBigInteger;
  Ms: Int64;
  SubMs: Integer;
begin
  if AArgs.Length < 1 then
    ThrowTypeError(SErrorTemporalInstantFromEpochNanos, SSuggestTemporalFromArg);

  Arg := AArgs.GetElement(0);
  BigNs := TemporalToBigInt(Arg);
  if not IsValidEpochNanoseconds(BigNs) then
    ThrowRangeError(SErrorTemporalInstantOutOfRange, SSuggestTemporalInstantRange);
  BigMs := BigNs.Divide(TBigInteger.FromInt64(1000000));
  BigSubMs := BigNs.Modulo(TBigInteger.FromInt64(1000000));
  Ms := BigMs.ToInt64;
  SubMs := Integer(BigSubMs.ToInt64);

  Result := TGocciaTemporalInstantValue.Create(Ms, SubMs);
end;

function TGocciaTemporalBuiltin.InstantCompare(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  I1, I2: TGocciaTemporalInstantValue;
begin
  I1 := CoerceTemporalInstant(AArgs.GetElement(0), 'Temporal.Instant.compare');
  I2 := CoerceTemporalInstant(AArgs.GetElement(1), 'Temporal.Instant.compare');

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
  ConstructorMethod.DefineProperty('from',
    TGocciaPropertyDescriptorData.Create(
      TGocciaNativeFunctionValue.CreateWithoutPrototype(PlainDateFrom, 'from', 1),
      [pfConfigurable, pfWritable]));
  ConstructorMethod.DefineProperty('compare',
    TGocciaPropertyDescriptorData.Create(
      TGocciaNativeFunctionValue.CreateWithoutPrototype(PlainDateCompare, 'compare', 2),
      [pfConfigurable, pfWritable]));
  FTemporalNamespace.DefineProperty('PlainDate',
    TGocciaPropertyDescriptorData.Create(ConstructorMethod, [pfConfigurable, pfWritable]));
end;

function TGocciaTemporalBuiltin.PlainDateConstructorFn(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Year, Month, Day: Integer;
  CalendarId: string;
begin
  RequireTemporalConstructorNew(AThisValue, 'PlainDate');
  Year := ToIntegerWithTruncationValue(AArgs.GetElement(0));
  Month := ToIntegerWithTruncationValue(AArgs.GetElement(1));
  Day := ToIntegerWithTruncationValue(AArgs.GetElement(2));
  CalendarId := ToTemporalCalendarIdentifier(AArgs.GetElement(3), False);
  Result := TGocciaTemporalPlainDateValue.Create(
    Year, Month, Day, CalendarId);
end;

function TGocciaTemporalBuiltin.PlainDateFrom(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Arg, V, VDay, VMonth, VMonthCode, VYear, VEra, VEraYear: TGocciaValue;
  D: TGocciaTemporalPlainDateValue;
  DT: TGocciaTemporalPlainDateTimeValue;
  ZDT: TGocciaTemporalZonedDateTimeValue;
  DateRec: TTemporalDateRecord;
  Info: TTemporalCalendarDateInfo;
  Obj: TGocciaObjectValue;
  Y, Mo, Dy, MaxDay, SuppliedMonth, H, Mi, S, Ms, Us, Ns: Integer;
  Overflow: TTemporalOverflow;
  CalendarId, MonthCodeStr: string;
  CalendarHasEra, HasEra, HasEraYear, HasMonth, HasMonthCode, IsLeapMonth: Boolean;
  EraResolvedYear: Integer;
begin
  Arg := AArgs.GetElement(0);
  CalendarId := 'iso8601';
  IsLeapMonth := False;

  Overflow := toConstrain;

  if Arg is TGocciaTemporalPlainDateValue then
  begin
    D := TGocciaTemporalPlainDateValue(Arg);
    GetOverflowOptionFromValue(AArgs.GetElement(1), 'Temporal.PlainDate.from');
    Result := TGocciaTemporalPlainDateValue.Create(D.Year, D.Month, D.Day, D.CalendarId);
  end
  else if Arg is TGocciaTemporalPlainDateTimeValue then
  begin
    DT := TGocciaTemporalPlainDateTimeValue(Arg);
    GetOverflowOptionFromValue(AArgs.GetElement(1), 'Temporal.PlainDate.from');
    Result := TGocciaTemporalPlainDateValue.Create(DT.Year, DT.Month, DT.Day, DT.CalendarId);
  end
  else if Arg is TGocciaTemporalZonedDateTimeValue then
  begin
    ZDT := TGocciaTemporalZonedDateTimeValue(Arg);
    GetZonedDateTimeLocalFields(ZDT, Y, Mo, Dy, H, Mi, S, Ms, Us, Ns);
    GetOverflowOptionFromValue(AArgs.GetElement(1), 'Temporal.PlainDate.from');
    Result := TGocciaTemporalPlainDateValue.Create(Y, Mo, Dy, ZDT.CalendarId);
  end
  else if Arg is TGocciaStringLiteralValue then
  begin
    if TemporalStringHasUTCDesignator(TGocciaStringLiteralValue(Arg).Value) then
      ThrowRangeError(SErrorInvalidISODate, SSuggestTemporalISOFormat);
    if (not TryExtractTemporalCalendarAnnotation(TGocciaStringLiteralValue(Arg).Value, CalendarId)) or
       (not CoerceToISODate(TGocciaStringLiteralValue(Arg).Value, DateRec)) then
      ThrowRangeError(SErrorInvalidISODate, SSuggestTemporalISOFormat);
    if not TemporalISODateInSupportedRange(DateRec.Year, DateRec.Month, DateRec.Day) then
      ThrowRangeError(SErrorInvalidISODate, SSuggestTemporalDateRange);
    GetOverflowOptionFromValue(AArgs.GetElement(1), 'Temporal.PlainDate.from');
    Result := TGocciaTemporalPlainDateValue.Create(DateRec.Year, DateRec.Month, DateRec.Day, CalendarId);
  end
  else if Arg is TGocciaObjectValue then
  begin
    Obj := TGocciaObjectValue(Arg);
    CalendarId := GetTemporalCalendarIdentifierWithISODefault(Obj);
    CalendarHasEra := TemporalCalendarUsesEra(CalendarId);

    VDay := Obj.GetProperty(PROP_DAY);
    if (VDay = nil) or (VDay is TGocciaUndefinedLiteralValue) then
      ThrowTypeError(SErrorTemporalPlainDateFromProps, SSuggestTemporalFromArg);
    Dy := ToIntegerWithTruncationValue(VDay);

    VMonth := Obj.GetProperty(PROP_MONTH);
    HasMonth := Assigned(VMonth) and not (VMonth is TGocciaUndefinedLiteralValue);
    SuppliedMonth := 0;
    if HasMonth then
      SuppliedMonth := ToIntegerWithTruncationValue(VMonth);

    VMonthCode := Obj.GetProperty(PROP_MONTH_CODE);
    HasMonthCode := Assigned(VMonthCode) and not (VMonthCode is TGocciaUndefinedLiteralValue);
    if HasMonthCode then
    begin
      MonthCodeStr := RequireTemporalMonthCodeString(VMonthCode,
        'Temporal.PlainDate.from');
      if not TryParseTemporalMonthCode(MonthCodeStr, Mo, IsLeapMonth) then
        ThrowRangeError(Format(SErrorInvalidMonthCodeFor, ['Temporal.PlainDate.from']), SSuggestTemporalMonthCode);
    end
    else if HasMonth then
      Mo := SuppliedMonth
    else
      ThrowTypeError(SErrorTemporalPlainDateFromProps, SSuggestTemporalFromArg);

    VYear := Obj.GetProperty(PROP_YEAR);
    HasEra := False;
    HasEraYear := False;
    EraResolvedYear := 0;
    if CalendarHasEra then
    begin
      VEra := Obj.GetProperty('era');
      VEraYear := Obj.GetProperty('eraYear');
      HasEra := Assigned(VEra) and not (VEra is TGocciaUndefinedLiteralValue);
      HasEraYear := Assigned(VEraYear) and not (VEraYear is TGocciaUndefinedLiteralValue);
      if HasEra <> HasEraYear then
        ThrowTypeError(SErrorTemporalPlainDateFromProps, SSuggestTemporalFromArg);
      if HasEra and not TryCalendarYearFromEra(CalendarId,
        VEra.ToStringLiteral.Value, ToIntegerWithTruncationValue(VEraYear),
        EraResolvedYear) then
        ThrowRangeError(SErrorTemporalPlainDateFromProps, SSuggestTemporalDateRange);
    end;
    if Assigned(VYear) and not (VYear is TGocciaUndefinedLiteralValue) then
      Y := ToIntegerWithTruncationValue(VYear)
    else if CalendarHasEra and HasEra then
      Y := EraResolvedYear
    else
      ThrowTypeError(SErrorTemporalPlainDateFromProps, SSuggestTemporalFromArg);

    Overflow := GetOverflowOptionFromValue(AArgs.GetElement(1), 'Temporal.PlainDate.from');

    if (CalendarId = 'iso8601') and HasMonthCode and
       (IsLeapMonth or (Mo < 1) or (Mo > 12)) then
      ThrowRangeError(Format(SErrorInvalidMonthCodeFor, ['Temporal.PlainDate.from']), SSuggestTemporalMonthCode);

    if (CalendarId = 'iso8601') and HasMonth and HasMonthCode and
       (SuppliedMonth <> Mo) then
      ThrowRangeError(Format(SErrorMonthCodeMismatchIn, ['Temporal.PlainDate.from']), SSuggestTemporalMonthCode);

    if (CalendarId = 'iso8601') and ((Mo < 1) or (Mo > 12)) then
    begin
      if (Mo < 1) or (Overflow = toReject) then
        ThrowRangeError(Format(SErrorTemporalMonthOutOfRange, [Mo]), SSuggestTemporalDateRange);
      Mo := 12;
    end;
    if Dy < 1 then
      ThrowRangeError(Format(SErrorTemporalDayOutOfRange, [Dy]), SSuggestTemporalDateRange);

    if CalendarId = 'iso8601' then
    begin
      MaxDay := DaysInMonth(Y, Mo);
      if Dy > MaxDay then
      begin
        if Overflow = toReject then
          ThrowRangeError(Format(SErrorTemporalDayOutOfRangeForMonth, [Dy, Mo]), SSuggestTemporalDateRange);
        Dy := MaxDay;
      end;
    end
    else
    begin
      if not TryResolveCalendarDateToISO(CalendarId, Y, Mo, Dy,
        HasMonthCode, IsLeapMonth, Overflow = toConstrain, DateRec) then
        ThrowRangeError(SErrorInvalidISODate, SSuggestTemporalDateRange);
      if HasMonth and HasMonthCode and
         ((not TryGetCalendarDateInfo(CalendarId, DateRec.Year,
           DateRec.Month, DateRec.Day, Info)) or
          (Info.Date.Month <> SuppliedMonth)) then
        ThrowRangeError(Format(SErrorMonthCodeMismatchIn, ['Temporal.PlainDate.from']), SSuggestTemporalMonthCode);
      Y := DateRec.Year;
      Mo := DateRec.Month;
      Dy := DateRec.Day;
    end;

    if not TemporalISODateInSupportedRange(Y, Mo, Dy) then
      ThrowRangeError(SErrorInvalidISODate, SSuggestTemporalDateRange);

    Result := TGocciaTemporalPlainDateValue.Create(Y, Mo, Dy, CalendarId);
  end
  else
  begin
    ThrowTypeError(SErrorTemporalPlainDateFromArg, SSuggestTemporalFromArg);
    Result := nil;
  end;
end;

function TGocciaTemporalBuiltin.PlainDateCompare(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D1, D2: TGocciaTemporalPlainDateValue;
  CoerceArgs: TGocciaArgumentsCollection;

  function CoerceDateForCompare(const AArg: TGocciaValue): TGocciaTemporalPlainDateValue;
  var
    Converted: TGocciaValue;
  begin
    CoerceArgs := TGocciaArgumentsCollection.Create([AArg]);
    try
      Converted := PlainDateFrom(CoerceArgs, TGocciaUndefinedLiteralValue.UndefinedValue);
      Result := TGocciaTemporalPlainDateValue(Converted);
    finally
      CoerceArgs.Free;
    end;
  end;

begin
  D1 := CoerceDateForCompare(AArgs.GetElement(0));
  D2 := CoerceDateForCompare(AArgs.GetElement(1));

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
  ConstructorMethod.DefineProperty('from',
    TGocciaPropertyDescriptorData.Create(
      TGocciaNativeFunctionValue.CreateWithoutPrototype(PlainTimeFrom, 'from', 1),
      [pfConfigurable, pfWritable]));
  ConstructorMethod.DefineProperty('compare',
    TGocciaPropertyDescriptorData.Create(
      TGocciaNativeFunctionValue.CreateWithoutPrototype(PlainTimeCompare, 'compare', 2),
      [pfConfigurable, pfWritable]));
  FTemporalNamespace.DefineProperty('PlainTime',
    TGocciaPropertyDescriptorData.Create(ConstructorMethod, [pfConfigurable, pfWritable]));
end;

function TGocciaTemporalBuiltin.PlainTimeConstructorFn(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Hour, Minute, Second, Millisecond, Microsecond, Nanosecond: Integer;

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
        Result := ToIntegerWithTruncationValue(V);
    end
    else
      Result := ADefault;
  end;

begin
  RequireTemporalConstructorNew(AThisValue, 'PlainTime');
  Hour := GetArgOr(0, 0);
  Minute := GetArgOr(1, 0);
  Second := GetArgOr(2, 0);
  Millisecond := GetArgOr(3, 0);
  Microsecond := GetArgOr(4, 0);
  Nanosecond := GetArgOr(5, 0);
  Result := TGocciaTemporalPlainTimeValue.Create(
    Hour, Minute, Second, Millisecond, Microsecond, Nanosecond);
end;

function TGocciaTemporalBuiltin.PlainTimeFrom(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Arg: TGocciaValue;
  T: TGocciaTemporalPlainTimeValue;
  DT: TGocciaTemporalPlainDateTimeValue;
  ZDT: TGocciaTemporalZonedDateTimeValue;
  TimeRec: TTemporalTimeRecord;
  Obj: TGocciaObjectValue;
  V: TGocciaValue;
  Y, Mo, D, H, Mi, S, Ms, Us, Ns: Integer;
  Overflow: TTemporalOverflow;
  SawTimeField: Boolean;
begin
  Arg := AArgs.GetElement(0);

  if Arg is TGocciaTemporalPlainTimeValue then
  begin
    T := TGocciaTemporalPlainTimeValue(Arg);
    GetOverflowOptionFromValue(AArgs.GetElement(1), 'Temporal.PlainTime.from');
    Result := TGocciaTemporalPlainTimeValue.Create(T.Hour, T.Minute, T.Second,
      T.Millisecond, T.Microsecond, T.Nanosecond);
  end
  else if Arg is TGocciaTemporalPlainDateTimeValue then
  begin
    DT := TGocciaTemporalPlainDateTimeValue(Arg);
    GetOverflowOptionFromValue(AArgs.GetElement(1), 'Temporal.PlainTime.from');
    Result := TGocciaTemporalPlainTimeValue.Create(DT.Hour, DT.Minute, DT.Second,
      DT.Millisecond, DT.Microsecond, DT.Nanosecond);
  end
  else if Arg is TGocciaTemporalZonedDateTimeValue then
  begin
    ZDT := TGocciaTemporalZonedDateTimeValue(Arg);
    GetZonedDateTimeLocalFields(ZDT, Y, Mo, D, H, Mi, S, Ms, Us, Ns);
    GetOverflowOptionFromValue(AArgs.GetElement(1), 'Temporal.PlainTime.from');
    Result := TGocciaTemporalPlainTimeValue.Create(H, Mi, S, Ms, Us, Ns);
  end
  else if Arg is TGocciaStringLiteralValue then
  begin
    if not CoerceToISOTime(TGocciaStringLiteralValue(Arg).Value, TimeRec) then
      ThrowRangeError(SErrorInvalidISOTime, SSuggestTemporalISOFormat);
    GetOverflowOptionFromValue(AArgs.GetElement(1), 'Temporal.PlainTime.from');
    Result := TGocciaTemporalPlainTimeValue.Create(
      TimeRec.Hour, TimeRec.Minute, TimeRec.Second,
      TimeRec.Millisecond, TimeRec.Microsecond, TimeRec.Nanosecond);
  end
  else if Arg is TGocciaObjectValue then
  begin
    Obj := TGocciaObjectValue(Arg);
    H := 0; Mi := 0; S := 0; Ms := 0; Us := 0; Ns := 0;
    SawTimeField := False;
    V := Obj.GetProperty(PROP_HOUR);
    if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
    begin
      SawTimeField := True;
      H := ToIntegerWithTruncationValue(V);
    end;
    V := Obj.GetProperty(PROP_MICROSECOND);
    if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
    begin
      SawTimeField := True;
      Us := ToIntegerWithTruncationValue(V);
    end;
    V := Obj.GetProperty(PROP_MILLISECOND);
    if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
    begin
      SawTimeField := True;
      Ms := ToIntegerWithTruncationValue(V);
    end;
    V := Obj.GetProperty(PROP_MINUTE);
    if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
    begin
      SawTimeField := True;
      Mi := ToIntegerWithTruncationValue(V);
    end;
    V := Obj.GetProperty(PROP_NANOSECOND);
    if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
    begin
      SawTimeField := True;
      Ns := ToIntegerWithTruncationValue(V);
    end;
    V := Obj.GetProperty(PROP_SECOND);
    if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
    begin
      SawTimeField := True;
      S := ToIntegerWithTruncationValue(V);
    end;
    if not SawTimeField then
      ThrowTypeError(SErrorTemporalPlainTimeFromArg, SSuggestTemporalFromArg);

    Overflow := GetOverflowOptionFromValue(AArgs.GetElement(1), 'Temporal.PlainTime.from');
    if Overflow = toConstrain then
      ConstrainTemporalTime(H, Mi, S, Ms, Us, Ns);

    Result := TGocciaTemporalPlainTimeValue.Create(H, Mi, S, Ms, Us, Ns);
  end
  else
  begin
    ThrowTypeError(SErrorTemporalPlainTimeFromArg, SSuggestTemporalFromArg);
    Result := nil;
  end;
end;

function TGocciaTemporalBuiltin.PlainTimeCompare(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  T1, T2: TGocciaTemporalPlainTimeValue;
  CoerceArgs: TGocciaArgumentsCollection;

  function CoerceTime(const AArg: TGocciaValue): TGocciaTemporalPlainTimeValue;
  var
    Converted: TGocciaValue;
  begin
    CoerceArgs := TGocciaArgumentsCollection.Create([AArg]);
    try
      Converted := PlainTimeFrom(CoerceArgs, TGocciaUndefinedLiteralValue.UndefinedValue);
      Result := TGocciaTemporalPlainTimeValue(Converted);
    finally
      CoerceArgs.Free;
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
  ConstructorMethod.DefineProperty('from',
    TGocciaPropertyDescriptorData.Create(
      TGocciaNativeFunctionValue.CreateWithoutPrototype(PlainDateTimeFrom, 'from', 1),
      [pfConfigurable, pfWritable]));
  ConstructorMethod.DefineProperty('compare',
    TGocciaPropertyDescriptorData.Create(
      TGocciaNativeFunctionValue.CreateWithoutPrototype(PlainDateTimeCompare, 'compare', 2),
      [pfConfigurable, pfWritable]));
  FTemporalNamespace.DefineProperty('PlainDateTime',
    TGocciaPropertyDescriptorData.Create(ConstructorMethod, [pfConfigurable, pfWritable]));
end;

function TGocciaTemporalBuiltin.PlainDateTimeConstructorFn(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Year, Month, Day, Hour, Minute, Second, Millisecond, Microsecond,
    Nanosecond: Integer;
  CalendarId: string;

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
        Result := ToIntegerWithTruncationValue(V);
    end
    else
      Result := ADefault;
  end;

begin
  RequireTemporalConstructorNew(AThisValue, 'PlainDateTime');
  Year := ToIntegerWithTruncationValue(AArgs.GetElement(0));
  Month := ToIntegerWithTruncationValue(AArgs.GetElement(1));
  Day := ToIntegerWithTruncationValue(AArgs.GetElement(2));
  Hour := GetArgOr(3, 0);
  Minute := GetArgOr(4, 0);
  Second := GetArgOr(5, 0);
  Millisecond := GetArgOr(6, 0);
  Microsecond := GetArgOr(7, 0);
  Nanosecond := GetArgOr(8, 0);
  CalendarId := ToTemporalCalendarIdentifier(AArgs.GetElement(9), False);
  Result := TGocciaTemporalPlainDateTimeValue.Create(
    Year, Month, Day, Hour, Minute, Second, Millisecond, Microsecond,
    Nanosecond, CalendarId);
end;

function TGocciaTemporalBuiltin.PlainDateTimeFrom(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Arg: TGocciaValue;
  PD: TGocciaTemporalPlainDateValue;
  DT: TGocciaTemporalPlainDateTimeValue;
  ZDT: TGocciaTemporalZonedDateTimeValue;
  DateRec: TTemporalDateRecord;
  TimeRec: TTemporalTimeRecord;
  Info: TTemporalCalendarDateInfo;
  Obj: TGocciaObjectValue;
  V, VDay, VHour, VMicrosecond, VMillisecond, VMinute, VMonth, VMonthCode,
    VNanosecond, VSecond, VYear, VEra, VEraYear: TGocciaValue;
  Y, Mo, D, H, Mi, S, Ms, Us, Ns, SuppliedMonth, MaxDay: Integer;
  Overflow: TTemporalOverflow;
  CalendarId, MonthCodeStr: string;
  CalendarHasEra, HasEra, HasEraYear, HasMonth, HasMonthCode, IsLeapMonth: Boolean;
  EraResolvedYear: Integer;
begin
  Arg := AArgs.GetElement(0);
  CalendarId := 'iso8601';
  IsLeapMonth := False;
  Overflow := toConstrain;

  if Arg is TGocciaTemporalPlainDateTimeValue then
  begin
    DT := TGocciaTemporalPlainDateTimeValue(Arg);
    GetOverflowOptionFromValue(AArgs.GetElement(1), 'Temporal.PlainDateTime.from');
    Result := TGocciaTemporalPlainDateTimeValue.Create(DT.Year, DT.Month, DT.Day,
      DT.Hour, DT.Minute, DT.Second, DT.Millisecond, DT.Microsecond, DT.Nanosecond, DT.CalendarId);
  end
  else if Arg is TGocciaTemporalPlainDateValue then
  begin
    PD := TGocciaTemporalPlainDateValue(Arg);
    GetOverflowOptionFromValue(AArgs.GetElement(1), 'Temporal.PlainDateTime.from');
    Result := TGocciaTemporalPlainDateTimeValue.Create(PD.Year, PD.Month, PD.Day,
      0, 0, 0, 0, 0, 0, PD.CalendarId);
  end
  else if Arg is TGocciaTemporalZonedDateTimeValue then
  begin
    ZDT := TGocciaTemporalZonedDateTimeValue(Arg);
    GetZonedDateTimeLocalFields(ZDT, Y, Mo, D, H, Mi, S, Ms, Us, Ns);
    GetOverflowOptionFromValue(AArgs.GetElement(1), 'Temporal.PlainDateTime.from');
    Result := TGocciaTemporalPlainDateTimeValue.Create(Y, Mo, D, H, Mi, S, Ms,
      Us, Ns, ZDT.CalendarId);
  end
  else if Arg is TGocciaStringLiteralValue then
  begin
    if TemporalStringHasUTCDesignator(TGocciaStringLiteralValue(Arg).Value) then
      ThrowRangeError(SErrorInvalidISODateTime, SSuggestTemporalISOFormat);
    if (not TryExtractTemporalCalendarAnnotation(TGocciaStringLiteralValue(Arg).Value, CalendarId)) or
       (not CoerceToISODateTime(TGocciaStringLiteralValue(Arg).Value, DateRec, TimeRec)) then
      ThrowRangeError(SErrorInvalidISODateTime, SSuggestTemporalISOFormat);
    if not TemporalISODateTimeInSupportedRange(DateRec.Year, DateRec.Month,
      DateRec.Day, TimeRec.Hour, TimeRec.Minute, TimeRec.Second,
      TimeRec.Millisecond, TimeRec.Microsecond, TimeRec.Nanosecond) then
      ThrowRangeError(SErrorInvalidISODateTime, SSuggestTemporalDateRange);
    GetOverflowOptionFromValue(AArgs.GetElement(1), 'Temporal.PlainDateTime.from');
    Result := TGocciaTemporalPlainDateTimeValue.Create(
      DateRec.Year, DateRec.Month, DateRec.Day,
      TimeRec.Hour, TimeRec.Minute, TimeRec.Second,
      TimeRec.Millisecond, TimeRec.Microsecond, TimeRec.Nanosecond, CalendarId);
  end
  else if Arg is TGocciaObjectValue then
  begin
    Obj := TGocciaObjectValue(Arg);
    CalendarId := GetTemporalCalendarIdentifierWithISODefault(Obj);
    CalendarHasEra := TemporalCalendarUsesEra(CalendarId);
    Y := 0; Mo := 0; D := 0; H := 0; Mi := 0; S := 0; Ms := 0; Us := 0; Ns := 0;

    VDay := Obj.GetProperty(PROP_DAY);
    if Assigned(VDay) and not (VDay is TGocciaUndefinedLiteralValue) then
      D := ToIntegerWithTruncationValue(VDay)
    else
      ThrowTypeError(SErrorTemporalPlainDateTimeCompareArg, SSuggestTemporalFromArg);

    VHour := Obj.GetProperty(PROP_HOUR);
    if Assigned(VHour) and not (VHour is TGocciaUndefinedLiteralValue) then H := ToIntegerWithTruncationValue(VHour);
    VMicrosecond := Obj.GetProperty(PROP_MICROSECOND);
    if Assigned(VMicrosecond) and not (VMicrosecond is TGocciaUndefinedLiteralValue) then Us := ToIntegerWithTruncationValue(VMicrosecond);
    VMillisecond := Obj.GetProperty(PROP_MILLISECOND);
    if Assigned(VMillisecond) and not (VMillisecond is TGocciaUndefinedLiteralValue) then Ms := ToIntegerWithTruncationValue(VMillisecond);
    VMinute := Obj.GetProperty(PROP_MINUTE);
    if Assigned(VMinute) and not (VMinute is TGocciaUndefinedLiteralValue) then Mi := ToIntegerWithTruncationValue(VMinute);

    VMonth := Obj.GetProperty(PROP_MONTH);
    HasMonth := Assigned(VMonth) and not (VMonth is TGocciaUndefinedLiteralValue);
    SuppliedMonth := 0;
    if HasMonth then
      SuppliedMonth := ToIntegerWithTruncationValue(VMonth);

    VMonthCode := Obj.GetProperty(PROP_MONTH_CODE);
    HasMonthCode := Assigned(VMonthCode) and not (VMonthCode is TGocciaUndefinedLiteralValue);
    if HasMonthCode then
    begin
      MonthCodeStr := RequireTemporalMonthCodeString(VMonthCode,
        'Temporal.PlainDateTime.from');
      if not TryParseTemporalMonthCode(MonthCodeStr, Mo, IsLeapMonth) then
        ThrowRangeError(Format(SErrorInvalidMonthCodeFor, ['Temporal.PlainDateTime.from']), SSuggestTemporalMonthCode);
    end
    else if HasMonth then
      Mo := SuppliedMonth
    else
      ThrowTypeError(SErrorTemporalPlainDateTimeCompareArg, SSuggestTemporalFromArg);

    VNanosecond := Obj.GetProperty(PROP_NANOSECOND);
    if Assigned(VNanosecond) and not (VNanosecond is TGocciaUndefinedLiteralValue) then Ns := ToIntegerWithTruncationValue(VNanosecond);
    VSecond := Obj.GetProperty(PROP_SECOND);
    if Assigned(VSecond) and not (VSecond is TGocciaUndefinedLiteralValue) then S := ToIntegerWithTruncationValue(VSecond);

    VYear := Obj.GetProperty(PROP_YEAR);
    HasEra := False;
    HasEraYear := False;
    EraResolvedYear := 0;
    if CalendarHasEra then
    begin
      VEra := Obj.GetProperty('era');
      VEraYear := Obj.GetProperty('eraYear');
      HasEra := Assigned(VEra) and not (VEra is TGocciaUndefinedLiteralValue);
      HasEraYear := Assigned(VEraYear) and not (VEraYear is TGocciaUndefinedLiteralValue);
      if HasEra <> HasEraYear then
        ThrowTypeError(SErrorTemporalPlainDateTimeCompareArg, SSuggestTemporalFromArg);
      if HasEra and not TryCalendarYearFromEra(CalendarId,
        VEra.ToStringLiteral.Value, ToIntegerWithTruncationValue(VEraYear),
        EraResolvedYear) then
        ThrowRangeError(SErrorInvalidISODateTime, SSuggestTemporalDateRange);
    end;
    if Assigned(VYear) and not (VYear is TGocciaUndefinedLiteralValue) then
      Y := ToIntegerWithTruncationValue(VYear)
    else if CalendarHasEra and HasEra then
      Y := EraResolvedYear
    else
      ThrowTypeError(SErrorTemporalPlainDateTimeCompareArg, SSuggestTemporalFromArg);

    Overflow := GetOverflowOptionFromValue(AArgs.GetElement(1), 'Temporal.PlainDateTime.from');

    if (CalendarId = 'iso8601') and HasMonthCode and
       (IsLeapMonth or (Mo < 1) or (Mo > 12)) then
      ThrowRangeError(Format(SErrorInvalidMonthCodeFor, ['Temporal.PlainDateTime.from']), SSuggestTemporalMonthCode);

    if (CalendarId = 'iso8601') and HasMonth and HasMonthCode and
       (SuppliedMonth <> Mo) then
      ThrowRangeError(Format(SErrorMonthCodeMismatchIn, ['Temporal.PlainDateTime.from']), SSuggestTemporalMonthCode);
    if Overflow = toConstrain then
      ConstrainTemporalTime(H, Mi, S, Ms, Us, Ns);
    if (CalendarId = 'iso8601') and ((Mo < 1) or (Mo > 12)) then
    begin
      if (Mo < 1) or (Overflow = toReject) then
        ThrowRangeError(Format(SErrorTemporalMonthOutOfRange, [Mo]), SSuggestTemporalDateRange);
      Mo := 12;
    end;
    if D < 1 then
      ThrowRangeError(Format(SErrorTemporalDayOutOfRange, [D]), SSuggestTemporalDateRange);
    if CalendarId = 'iso8601' then
    begin
      MaxDay := DaysInMonth(Y, Mo);
      if D > MaxDay then
      begin
        if Overflow = toReject then
          ThrowRangeError(Format(SErrorTemporalDayOutOfRangeForMonth, [D, Mo]), SSuggestTemporalDateRange);
        D := MaxDay;
      end;
    end;
    if CalendarId <> 'iso8601' then
    begin
      if not TryResolveCalendarDateToISO(CalendarId, Y, Mo, D,
        HasMonthCode, IsLeapMonth, Overflow = toConstrain, DateRec) then
        ThrowRangeError(SErrorInvalidISODateTime, SSuggestTemporalDateRange);
      if HasMonth and HasMonthCode and
         ((not TryGetCalendarDateInfo(CalendarId, DateRec.Year,
           DateRec.Month, DateRec.Day, Info)) or
          (Info.Date.Month <> SuppliedMonth)) then
        ThrowRangeError(Format(SErrorMonthCodeMismatchIn, ['Temporal.PlainDateTime.from']), SSuggestTemporalMonthCode);
      Y := DateRec.Year;
      Mo := DateRec.Month;
      D := DateRec.Day;
    end;
    if not TemporalISODateTimeInSupportedRange(Y, Mo, D, H, Mi, S, Ms, Us, Ns) then
      ThrowRangeError(SErrorInvalidISODateTime, SSuggestTemporalDateRange);
    Result := TGocciaTemporalPlainDateTimeValue.Create(Y, Mo, D, H, Mi, S, Ms, Us, Ns, CalendarId);
  end
  else
  begin
    ThrowTypeError(SErrorTemporalPlainDateTimeFromArg, SSuggestTemporalFromArg);
    Result := nil;
  end;
end;

function TGocciaTemporalBuiltin.PlainDateTimeCompare(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  DT1, DT2: TGocciaTemporalPlainDateTimeValue;
  Cmp: Integer;
  CoerceArgs: TGocciaArgumentsCollection;

  function CoerceDateTimeForCompare(const AArg: TGocciaValue): TGocciaTemporalPlainDateTimeValue;
  var
    Converted: TGocciaValue;
  begin
    CoerceArgs := TGocciaArgumentsCollection.Create([AArg]);
    try
      Converted := PlainDateTimeFrom(CoerceArgs, TGocciaUndefinedLiteralValue.UndefinedValue);
      Result := TGocciaTemporalPlainDateTimeValue(Converted);
    finally
      CoerceArgs.Free;
    end;
  end;

begin
  DT1 := CoerceDateTimeForCompare(AArgs.GetElement(0));
  DT2 := CoerceDateTimeForCompare(AArgs.GetElement(1));

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
  ConstructorMethod.DefineProperty('from',
    TGocciaPropertyDescriptorData.Create(
      TGocciaNativeFunctionValue.CreateWithoutPrototype(PlainYearMonthFrom, 'from', 1),
      [pfConfigurable, pfWritable]));
  ConstructorMethod.DefineProperty('compare',
    TGocciaPropertyDescriptorData.Create(
      TGocciaNativeFunctionValue.CreateWithoutPrototype(PlainYearMonthCompare, 'compare', 2),
      [pfConfigurable, pfWritable]));
  FTemporalNamespace.DefineProperty('PlainYearMonth',
    TGocciaPropertyDescriptorData.Create(ConstructorMethod, [pfConfigurable, pfWritable]));
end;

function TGocciaTemporalBuiltin.PlainYearMonthConstructorFn(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  YearValue, MonthValue, RefDay: Integer;
  CalendarId: string;
begin
  if not (AThisValue is TGocciaHoleValue) then
    ThrowTypeError('Temporal.PlainYearMonth cannot be called without new',
      SSuggestTemporalThisType);
  if AArgs.Length = 0 then
    ThrowRangeError(SErrorTemporalPlainYearMonthArgs, SSuggestTemporalDateRange);

  // Temporal §9.1.1 PlainYearMonth(isoYear, isoMonth, calendarLike,
  // referenceISODay) — argument 2 is the calendar, the reference day is 3.
  YearValue := ToIntegerWithTruncationValue(AArgs.GetElement(0));
  if AArgs.Length < 2 then
    ThrowRangeError(SErrorTemporalPlainYearMonthArgs, SSuggestTemporalDateRange);
  MonthValue := ToIntegerWithTruncationValue(AArgs.GetElement(1));
  CalendarId := ToTemporalCalendarIdentifier(AArgs.GetElement(2), False);
  RefDay := 1;
  if (AArgs.Length >= 4) and not (AArgs.GetElement(3) is TGocciaUndefinedLiteralValue) then
    RefDay := ToIntegerWithTruncationValue(AArgs.GetElement(3));
  if (not IsValidDate(YearValue, MonthValue, RefDay)) or
     (not IsTemporalISOYearMonthInSupportedRange(YearValue, MonthValue)) then
    ThrowRangeError(SErrorInvalidISOYearMonth, SSuggestTemporalDateRange);

  Result := TGocciaTemporalPlainYearMonthValue.Create(
    YearValue,
    MonthValue,
    RefDay,
    CalendarId);
end;

function TGocciaTemporalBuiltin.PlainYearMonthFrom(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Arg, V, VMonth, VMonthCode, VYear, VEra, VEraYear: TGocciaValue;
  YM: TGocciaTemporalPlainYearMonthValue;
  DateRec: TTemporalDateRecord;
  Info: TTemporalCalendarDateInfo;
  Y, M, MonthPart, ReferenceDay, SuppliedMonth: Integer;
  Obj: TGocciaObjectValue;
  CalendarId, MonthCodeStr: string;
  CalendarHasEra, HasEra, HasEraYear, HasMonth, HasMonthCode, IsLeapMonth: Boolean;
  EraResolvedYear: Integer;
  Overflow: TTemporalOverflow;
begin
  Arg := AArgs.GetElement(0);
  CalendarId := 'iso8601';
  ReferenceDay := 1;
  IsLeapMonth := False;

  Overflow := toConstrain;

  if Arg is TGocciaTemporalPlainYearMonthValue then
  begin
    YM := TGocciaTemporalPlainYearMonthValue(Arg);
    GetOverflowOptionFromValue(AArgs.GetElement(1), 'Temporal.PlainYearMonth.from');
    Result := TGocciaTemporalPlainYearMonthValue.Create(YM.Year, YM.Month, YM.ReferenceDay, YM.CalendarId);
  end
  else if Arg is TGocciaStringLiteralValue then
  begin
    if not TryParseTemporalPlainYearMonthString(TGocciaStringLiteralValue(Arg).Value,
      Y, M, ReferenceDay, CalendarId) then
      ThrowRangeError(SErrorInvalidISOYearMonth, SSuggestTemporalISOFormat);
    if (CalendarId = 'iso8601') and not IsTemporalISOYearMonthInSupportedRange(Y, M) then
      ThrowRangeError(SErrorInvalidISOYearMonth, SSuggestTemporalDateRange);
    GetOverflowOptionFromValue(AArgs.GetElement(1), 'Temporal.PlainYearMonth.from');
    Result := TGocciaTemporalPlainYearMonthValue.Create(Y, M, ReferenceDay, CalendarId);
  end
  else if Arg is TGocciaObjectValue then
  begin
    Obj := TGocciaObjectValue(Arg);
    CalendarId := GetTemporalCalendarIdentifierWithISODefault(Obj);
    CalendarHasEra := TemporalCalendarUsesEra(CalendarId);

    VMonth := Obj.GetProperty(PROP_MONTH);
    HasMonth := Assigned(VMonth) and not (VMonth is TGocciaUndefinedLiteralValue);
    SuppliedMonth := 0;
    if HasMonth then
      SuppliedMonth := ToIntegerWithTruncationValue(VMonth);

    VMonthCode := Obj.GetProperty(PROP_MONTH_CODE);
    HasMonthCode := Assigned(VMonthCode) and not (VMonthCode is TGocciaUndefinedLiteralValue);
    if HasMonthCode then
    begin
      MonthCodeStr := RequireTemporalMonthCodeString(VMonthCode,
        'Temporal.PlainYearMonth.from');
      if not TryParseTemporalMonthCode(MonthCodeStr, MonthPart, IsLeapMonth) then
        ThrowRangeError(SErrorInvalidMonthCodeYearMonth, SSuggestTemporalMonthCode);
      M := MonthPart;
    end
    else if HasMonth then
      M := SuppliedMonth
    else
    begin
      ThrowTypeError(SErrorTemporalPlainYearMonthFromMonth, SSuggestTemporalFromArg);
    end;

    VYear := Obj.GetProperty(PROP_YEAR);
    HasEra := False;
    HasEraYear := False;
    EraResolvedYear := 0;
    if CalendarHasEra then
    begin
      VEra := Obj.GetProperty('era');
      VEraYear := Obj.GetProperty('eraYear');
      HasEra := Assigned(VEra) and not (VEra is TGocciaUndefinedLiteralValue);
      HasEraYear := Assigned(VEraYear) and not (VEraYear is TGocciaUndefinedLiteralValue);
      if HasEra <> HasEraYear then
        ThrowTypeError(SErrorTemporalPlainYearMonthFromYear, SSuggestTemporalFromArg);
      if HasEra and not TryCalendarYearFromEra(CalendarId,
        VEra.ToStringLiteral.Value, ToIntegerWithTruncationValue(VEraYear),
        EraResolvedYear) then
        ThrowRangeError(SErrorTemporalPlainYearMonthFromYear, SSuggestTemporalDateRange);
    end;
    if Assigned(VYear) and not (VYear is TGocciaUndefinedLiteralValue) then
      Y := ToIntegerWithTruncationValue(VYear)
    else if CalendarHasEra and HasEra then
      Y := EraResolvedYear
    else
      ThrowTypeError(SErrorTemporalPlainYearMonthFromYear, SSuggestTemporalFromArg);

    Overflow := GetOverflowOptionFromValue(AArgs.GetElement(1), 'Temporal.PlainYearMonth.from');

    if (CalendarId = 'iso8601') and HasMonthCode and
       (IsLeapMonth or (M < 1) or (M > 12)) then
      ThrowRangeError(SErrorInvalidMonthCodeYearMonth, SSuggestTemporalMonthCode);

    if (CalendarId = 'iso8601') and HasMonth and HasMonthCode and
       (SuppliedMonth <> M) then
      ThrowRangeError(SErrorMonthCodeMismatch, SSuggestTemporalMonthCode);
    if (CalendarId = 'iso8601') and ((M < 1) or (M > 12)) then
    begin
      if (M < 1) or (Overflow = toReject) then
        ThrowRangeError(Format(SErrorTemporalMonthOutOfRange, [M]), SSuggestTemporalDateRange);
      M := ClampTemporalInteger(M, 1, 12);
    end;
    if (CalendarId = 'iso8601') and
       not IsTemporalISOYearMonthInSupportedRange(Y, M) then
      ThrowRangeError(SErrorInvalidISOYearMonth, SSuggestTemporalDateRange);
    if CalendarId <> 'iso8601' then
    begin
      if not TryResolveCalendarDateToISO(CalendarId, Y, M, 1,
        HasMonthCode, IsLeapMonth, Overflow = toConstrain, DateRec) then
        ThrowRangeError(SErrorInvalidISOYearMonth, SSuggestTemporalDateRange);
      if not TemporalISOYearMonthInSupportedRange(DateRec) then
        ThrowRangeError(SErrorInvalidISOYearMonth, SSuggestTemporalDateRange);
      if HasMonth and HasMonthCode and
         ((not TryGetCalendarDateInfo(CalendarId, DateRec.Year,
           DateRec.Month, DateRec.Day, Info)) or
          (Info.Date.Month <> SuppliedMonth)) then
        ThrowRangeError(SErrorMonthCodeMismatch, SSuggestTemporalMonthCode);
      Y := DateRec.Year;
      M := DateRec.Month;
      ReferenceDay := DateRec.Day;
    end;
    Result := TGocciaTemporalPlainYearMonthValue.Create(Y, M, ReferenceDay, CalendarId);
  end
  else
  begin
    ThrowTypeError(SErrorTemporalPlainYearMonthFromArg, SSuggestTemporalFromArg);
    Result := nil;
  end;
end;

function TGocciaTemporalBuiltin.PlainYearMonthCompare(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  YM1, YM2: TGocciaTemporalPlainYearMonthValue;
  CoerceArgs: TGocciaArgumentsCollection;

  function CoerceYearMonthForCompare(const AArg: TGocciaValue): TGocciaTemporalPlainYearMonthValue;
  var
    Converted: TGocciaValue;
  begin
    CoerceArgs := TGocciaArgumentsCollection.Create([AArg]);
    try
      Converted := PlainYearMonthFrom(CoerceArgs, TGocciaUndefinedLiteralValue.UndefinedValue);
      Result := TGocciaTemporalPlainYearMonthValue(Converted);
    finally
      CoerceArgs.Free;
    end;
  end;

begin
  YM1 := CoerceYearMonthForCompare(AArgs.GetElement(0));
  YM2 := CoerceYearMonthForCompare(AArgs.GetElement(1));

  if YM1.Year < YM2.Year then
    Result := TGocciaNumberLiteralValue.Create(-1)
  else if YM1.Year > YM2.Year then
    Result := TGocciaNumberLiteralValue.Create(1)
  else if YM1.Month < YM2.Month then
    Result := TGocciaNumberLiteralValue.Create(-1)
  else if YM1.Month > YM2.Month then
    Result := TGocciaNumberLiteralValue.Create(1)
  else if YM1.ReferenceDay < YM2.ReferenceDay then
    Result := TGocciaNumberLiteralValue.Create(-1)
  else if YM1.ReferenceDay > YM2.ReferenceDay then
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
  ConstructorMethod.DefineProperty('from',
    TGocciaPropertyDescriptorData.Create(
      TGocciaNativeFunctionValue.CreateWithoutPrototype(PlainMonthDayFrom, 'from', 1),
      [pfConfigurable, pfWritable]));
  ConstructorMethod.DefineProperty('compare',
    TGocciaPropertyDescriptorData.Create(
      TGocciaNativeFunctionValue.CreateWithoutPrototype(PlainMonthDayCompare, 'compare', 2),
      [pfConfigurable, pfWritable]));
  FTemporalNamespace.DefineProperty('PlainMonthDay',
    TGocciaPropertyDescriptorData.Create(ConstructorMethod, [pfConfigurable, pfWritable]));
end;

function TGocciaTemporalBuiltin.PlainMonthDayConstructorFn(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  MonthValue, DayValue, ReferenceYear: Integer;
  CalendarId: string;
  V: TGocciaValue;
begin
  if not (AThisValue is TGocciaHoleValue) then
    ThrowTypeError('Temporal.PlainMonthDay cannot be called without new',
      SSuggestTemporalThisType);
  if AArgs.Length = 0 then
    ThrowRangeError(SErrorTemporalPlainMonthDayArgs, SSuggestTemporalDateRange);

  MonthValue := ToIntegerWithTruncationValue(AArgs.GetElement(0));
  if AArgs.Length < 2 then
    ThrowRangeError(SErrorTemporalPlainMonthDayArgs, SSuggestTemporalDateRange);
  DayValue := ToIntegerWithTruncationValue(AArgs.GetElement(1));
  CalendarId := ToTemporalCalendarIdentifier(AArgs.GetElement(2), False);
  ReferenceYear := PLAIN_MONTH_DAY_DEFAULT_REFERENCE_YEAR;
  V := AArgs.GetElement(3);
  if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
    ReferenceYear := ToIntegerWithTruncationValue(V);

  Result := TGocciaTemporalPlainMonthDayValue.Create(
    MonthValue,
    DayValue,
    ReferenceYear,
    CalendarId);
end;

function TGocciaTemporalBuiltin.PlainMonthDayFrom(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Arg, V, VMonth, VMonthCode, VDay, VYear, VEra, VEraYear: TGocciaValue;
  MD: TGocciaTemporalPlainMonthDayValue;
  DateRec, SourceDateRec: TTemporalDateRecord;
  Obj: TGocciaObjectValue;
  M, D, ReferenceYear, SuppliedYear, CandidateMonth: Integer;
  CalendarId, MonthCodeStr, EraName: string;
  IsLeapMonth, CalendarHasEra: Boolean;
  EraYear: Integer;
  HasMonth, HasMonthCode, HasReferenceYear, HasEra, HasEraYear, HasSourceDate: Boolean;
  Overflow: TTemporalOverflow;
  Info: TTemporalCalendarDateInfo;

  function IsPresent(const AValue: TGocciaValue): Boolean;
  begin
    Result := Assigned(AValue) and not (AValue is TGocciaUndefinedLiteralValue);
  end;

  function IsCalendarYearClearlyOutOfTemporalRange(const ACalendarId: string;
    const AYear: Integer): Boolean;
  var
    ISOYear: Int64;
  begin
    ISOYear := AYear;
    if ACalendarId = 'buddhist' then
      Dec(ISOYear, 543)
    else if ACalendarId = 'roc' then
      Inc(ISOYear, 1911);
    Result := (ISOYear < -271821) or (ISOYear > 275760);
  end;

  function TryReferenceInISOYear(const ACalendarId: string;
    const AISOReferenceYear, AMonthOrMonthCode, ADay: Integer;
    const AMatchMonthCode, AIsLeapMonth: Boolean;
    out AISODate: TTemporalDateRecord): Boolean;
  var
    StartDays, Days: Int64;
    Offset: Integer;
    Candidate: TTemporalDateRecord;
    CandidateInfo: TTemporalCalendarDateInfo;
    ExpectedMonthCode: string;
  begin
    Result := False;
    AISODate.Year := 0;
    AISODate.Month := 0;
    AISODate.Day := 0;
    if (ADay < 1) or (AMonthOrMonthCode < 1) or
       not IsValidDate(AISOReferenceYear, 1, 1) then
      Exit;

    if ACalendarId = 'iso8601' then
    begin
      if IsValidDate(AISOReferenceYear, AMonthOrMonthCode, ADay) then
      begin
        AISODate.Year := AISOReferenceYear;
        AISODate.Month := AMonthOrMonthCode;
        AISODate.Day := ADay;
        Exit(True);
      end;
      Exit;
    end;

    ExpectedMonthCode := FormatTemporalMonthCode(AMonthOrMonthCode, AIsLeapMonth);
    StartDays := DateToEpochDays(AISOReferenceYear, 1, 1);
    for Offset := DaysInYear(AISOReferenceYear) - 1 downto 0 do
    begin
      Days := StartDays + Offset;
      Candidate := EpochDaysToDate(Days);
      if not TryGetCalendarDateInfo(ACalendarId, Candidate.Year,
        Candidate.Month, Candidate.Day, CandidateInfo) then
        Exit(False);
      if CandidateInfo.Date.Day <> ADay then
        Continue;
      if AMatchMonthCode then
      begin
        if CandidateInfo.MonthCode = ExpectedMonthCode then
        begin
          AISODate := Candidate;
          Exit(True);
        end;
      end
      else if CandidateInfo.Date.Month = AMonthOrMonthCode then
      begin
        AISODate := Candidate;
        Exit(True);
      end;
    end;
  end;

  function TryReferenceMonthDay(const ACalendarId: string;
    const AMonthOrMonthCode, ADay: Integer;
    const AMatchMonthCode, AIsLeapMonth: Boolean;
    out AISODate: TTemporalDateRecord): Boolean;
  var
    Year: Integer;

    function MonthCodeSupported: Boolean;
    begin
      if (AMonthOrMonthCode < 1) or (ADay < 1) then
        Exit(False);
      if (ACalendarId = 'chinese') or (ACalendarId = 'dangi') then
        Exit(AMonthOrMonthCode <= 12);
      if ACalendarId = 'hebrew' then
      begin
        if AIsLeapMonth then
          Exit(AMonthOrMonthCode = 5);
        Exit(AMonthOrMonthCode <= 12);
      end;
      if (ACalendarId = 'coptic') or (ACalendarId = 'ethiopic') or
         (ACalendarId = 'ethioaa') then
        Exit((not AIsLeapMonth) and (AMonthOrMonthCode <= 13));
      Result := (not AIsLeapMonth) and (AMonthOrMonthCode <= 12);
    end;

    function TryReferenceYearHint(out AYear: Integer): Boolean;
    begin
      AYear := 1972;
      Result := MonthCodeSupported;
      if not Result then
        Exit;

      if (ACalendarId = 'chinese') or (ACalendarId = 'dangi') then
      begin
        if AIsLeapMonth then
        begin
          if ADay >= 30 then
          begin
            case AMonthOrMonthCode of
              3: AYear := 1955;
              4: AYear := 1944;
              5: AYear := 1952;
              6: AYear := 1941;
              7: AYear := 1938;
            else
              Exit(False);
            end;
          end
          else
          begin
            case AMonthOrMonthCode of
              2: AYear := 1947;
              3: AYear := 1966;
              4: AYear := 1963;
              5: AYear := 1971;
              6: AYear := 1960;
              7: AYear := 1968;
              8: AYear := 1957;
              9: AYear := 2014;
              10: AYear := 1984;
              11:
                begin
                  if ADay >= 29 then
                    AYear := 2034
                  else
                    AYear := 2033;
                end;
            else
              Exit(False);
            end;
          end;
        end
        else if ADay >= 30 then
        begin
          case AMonthOrMonthCode of
            1, 4, 11: AYear := 1970;
            2, 5, 7, 9, 10, 12: AYear := 1972;
            3:
              if ACalendarId = 'dangi' then
                AYear := 1968
              else
                AYear := 1966;
            6, 8: AYear := 1971;
          else
            Exit(False);
          end;
        end;
        Exit(True);
      end;

      if ACalendarId = 'hebrew' then
      begin
        if AIsLeapMonth then
        begin
          AYear := 1970;
          Exit(True);
        end;
        if ADay >= 30 then
        begin
          case AMonthOrMonthCode of
            1, 5, 7, 9, 11: AYear := 1972;
            2, 3: AYear := 1971;
          else
            Exit(False);
          end;
        end;
        Exit(True);
      end;

      if (ACalendarId = 'coptic') or (ACalendarId = 'ethiopic') or
         (ACalendarId = 'ethioaa') then
      begin
        if AMonthOrMonthCode = 13 then
        begin
          if ADay >= 6 then
            AYear := 1971
          else
            AYear := 1972;
        end;
        Exit(True);
      end;

      if (ACalendarId = 'islamic-civil') or
         (ACalendarId = 'islamic-tbla') then
      begin
        if (AMonthOrMonthCode = 12) and (ADay >= 30) then
          AYear := 1971
        else if (ADay >= 30) and not (AMonthOrMonthCode in [1, 3, 5, 7, 9, 11]) then
          Exit(False);
        Exit(True);
      end;

      if ACalendarId = 'islamic-umalqura' then
      begin
        if ADay >= 30 then
        begin
          case AMonthOrMonthCode of
            1, 4, 6, 8, 9, 11: AYear := 1972;
            2, 10: AYear := 1970;
            3, 5, 12: AYear := 1971;
            7: AYear := 1969;
          else
            Exit(False);
          end;
        end;
        Exit(True);
      end;
    end;
  begin
    Result := TryReferenceYearHint(Year) and
      TryReferenceInISOYear(ACalendarId, Year, AMonthOrMonthCode, ADay,
        AMatchMonthCode, AIsLeapMonth, AISODate);
  end;

  function TryResolveReferenceMonthDay(const ACalendarId: string;
    const AMonthOrMonthCode, ADay: Integer;
    const AMatchMonthCode, AIsLeapMonth: Boolean;
    const AOverflow: TTemporalOverflow;
    out AISODate: TTemporalDateRecord): Boolean;
  var
    WorkDay: Integer;
  begin
    if TryReferenceMonthDay(ACalendarId, AMonthOrMonthCode, ADay,
      AMatchMonthCode, AIsLeapMonth, AISODate) then
      Exit(True);
    if AOverflow = toReject then
      Exit(False);

    if AMatchMonthCode and AIsLeapMonth and
       ((ACalendarId = 'chinese') or (ACalendarId = 'dangi')) then
    begin
      if ADay >= 30 then
      begin
        if TryReferenceMonthDay(ACalendarId, AMonthOrMonthCode, 30,
          True, True, AISODate) then
          Exit(True);
        if TryReferenceMonthDay(ACalendarId, AMonthOrMonthCode, 30,
          True, False, AISODate) then
          Exit(True);
      end;
      if TryReferenceMonthDay(ACalendarId, AMonthOrMonthCode, ADay,
        True, False, AISODate) then
        Exit(True);
    end;

    WorkDay := ADay;
    while WorkDay > 1 do
    begin
      Dec(WorkDay);
      if TryReferenceMonthDay(ACalendarId, AMonthOrMonthCode, WorkDay,
        AMatchMonthCode, AIsLeapMonth, AISODate) then
        Exit(True);
    end;
    Result := False;
  end;
begin
  Arg := AArgs.GetElement(0);
  CalendarId := 'iso8601';
  ReferenceYear := PLAIN_MONTH_DAY_DEFAULT_REFERENCE_YEAR;
  SuppliedYear := ReferenceYear;
  HasReferenceYear := False;
  Overflow := toConstrain;

  if Arg is TGocciaTemporalPlainMonthDayValue then
  begin
    MD := TGocciaTemporalPlainMonthDayValue(Arg);
    GetOverflowOptionFromValue(AArgs.GetElement(1), 'Temporal.PlainMonthDay.from');
    Result := TGocciaTemporalPlainMonthDayValue.Create(MD.Month, MD.Day, MD.ReferenceYear, MD.CalendarId);
  end
  else if Arg is TGocciaStringLiteralValue then
  begin
    if not TryParseTemporalPlainMonthDayString(TGocciaStringLiteralValue(Arg).Value,
      M, D, CalendarId, SourceDateRec, HasSourceDate) then
      ThrowRangeError(SErrorInvalidISOMonthDay, SSuggestTemporalISOFormat);
    Overflow := GetOverflowOptionFromValue(AArgs.GetElement(1), 'Temporal.PlainMonthDay.from');
    if CalendarId <> 'iso8601' then
    begin
      if HasSourceDate then
      begin
        if not TryGetCalendarDateInfo(CalendarId, SourceDateRec.Year,
          SourceDateRec.Month, SourceDateRec.Day, Info) then
          ThrowRangeError(SErrorInvalidISOMonthDay, SSuggestTemporalDateRange);
      end
      else if not TryGetCalendarDateInfo(CalendarId, ReferenceYear, M, D, Info) then
        ThrowRangeError(SErrorInvalidISOMonthDay, SSuggestTemporalDateRange);
      if not TryParseTemporalMonthCode(Info.MonthCode, M, IsLeapMonth) then
        ThrowRangeError(SErrorInvalidISOMonthDay, SSuggestTemporalDateRange);
      D := Info.Date.Day;
      if not TryResolveReferenceMonthDay(CalendarId, M, D, True,
        IsLeapMonth, Overflow, DateRec) then
        ThrowRangeError(SErrorInvalidISOMonthDay, SSuggestTemporalDateRange);
      Result := TGocciaTemporalPlainMonthDayValue.Create(DateRec.Month,
        DateRec.Day, DateRec.Year, CalendarId);
    end
    else
      Result := TGocciaTemporalPlainMonthDayValue.Create(M, D, CalendarId);
  end
  else if Arg is TGocciaObjectValue then
  begin
    Obj := TGocciaObjectValue(Arg);
    CalendarId := GetTemporalCalendarIdentifierWithISODefault(Obj);

    VDay := Obj.GetProperty(PROP_DAY);
    if not IsPresent(VDay) then
      ThrowTypeError(SErrorTemporalPlainMonthDayFromDay, SSuggestTemporalFromArg);
    D := ToIntegerWithTruncationValue(VDay);

    VMonth := Obj.GetProperty(PROP_MONTH);
    HasMonth := IsPresent(VMonth);
    CandidateMonth := 0;
    if HasMonth then
      CandidateMonth := ToIntegerWithTruncationValue(VMonth);

    VMonthCode := Obj.GetProperty(PROP_MONTH_CODE);
    HasMonthCode := IsPresent(VMonthCode);
    if HasMonthCode then
    begin
      MonthCodeStr := RequireTemporalMonthCodeString(VMonthCode,
        'Temporal.PlainMonthDay.from');
      if not TryParseTemporalMonthCode(MonthCodeStr, M, IsLeapMonth) then
        ThrowRangeError(SErrorInvalidMonthCodeMonthDay, SSuggestTemporalMonthCode);
    end
    else if HasMonth then
      M := CandidateMonth
    else
      ThrowTypeError(SErrorTemporalPlainMonthDayFromMonth, SSuggestTemporalFromArg);

    VYear := Obj.GetProperty(PROP_YEAR);
    HasReferenceYear := IsPresent(VYear);
    if HasReferenceYear then
    begin
      SuppliedYear := ToIntegerWithTruncationValue(VYear);
      ReferenceYear := SuppliedYear;
    end;

    CalendarHasEra := TemporalCalendarUsesEra(CalendarId);
    HasEra := False;
    HasEraYear := False;
    if CalendarHasEra then
    begin
      VEra := Obj.GetProperty('era');
      VEraYear := Obj.GetProperty('eraYear');
      HasEra := IsPresent(VEra);
      HasEraYear := IsPresent(VEraYear);
      if HasEra <> HasEraYear then
        ThrowTypeError(SErrorTemporalPlainMonthDayFromArg, SSuggestTemporalFromArg);
    end;

    Overflow := GetOverflowOptionFromValue(AArgs.GetElement(1), 'Temporal.PlainMonthDay.from');

    if (CalendarId = 'islamic') or (CalendarId = 'islamic-rgsa') then
      ThrowRangeError(SErrorTemporalPlainMonthDayFromArg, SSuggestTemporalDateRange);

    if HasReferenceYear and (CalendarId <> 'iso8601') and
       IsCalendarYearClearlyOutOfTemporalRange(CalendarId, SuppliedYear) then
      ThrowRangeError(SErrorTemporalPlainMonthDayFromArg, SSuggestTemporalDateRange);

    if HasEra and HasEraYear then
    begin
      EraName := '';
      EraYear := 0;
      if TryCalendarEraFromYear(CalendarId, ReferenceYear, EraName, EraYear,
        CalendarHasEra) and CalendarHasEra then
      begin
        if not TryCalendarYearFromEra(CalendarId, VEra.ToStringLiteral.Value,
          ToIntegerWithTruncationValue(VEraYear), SuppliedYear) then
          ThrowRangeError(SErrorTemporalPlainMonthDayFromArg, SSuggestTemporalFromArg);
        if IsCalendarYearClearlyOutOfTemporalRange(CalendarId, SuppliedYear) then
          ThrowRangeError(SErrorTemporalPlainMonthDayFromArg, SSuggestTemporalDateRange);
        if HasReferenceYear and (ReferenceYear <> SuppliedYear) then
          ThrowRangeError(SErrorTemporalPlainMonthDayFromArg, SSuggestTemporalFromArg);
        ReferenceYear := SuppliedYear;
        HasReferenceYear := True;
      end;
    end;

    if (CalendarId <> 'iso8601') and HasMonth and not HasReferenceYear then
      ThrowTypeError(SErrorTemporalPlainMonthDayFromArg, SSuggestTemporalFromArg);

    if HasMonth and (CalendarId = 'iso8601') then
    begin
      if HasMonthCode and (CandidateMonth <> M) then
        ThrowRangeError(SErrorInvalidMonthCodeMonthDay, SSuggestTemporalMonthCode);
      M := CandidateMonth;
    end;
    if (CalendarId = 'iso8601') and HasMonthCode and
       (IsLeapMonth or (M < 1) or (M > 12)) then
      ThrowRangeError(SErrorInvalidMonthCodeMonthDay, SSuggestTemporalMonthCode);
    if (CalendarId = 'iso8601') and ((M < 1) or (M > 12)) then
    begin
      if (M < 1) or (Overflow = toReject) then
        ThrowRangeError(SErrorTemporalPlainMonthDayFromMonth, SSuggestTemporalDateRange);
      M := ClampTemporalInteger(M, 1, 12);
    end;
    if (CalendarId = 'iso8601') and (D < 1) then
    begin
      ThrowRangeError(SErrorInvalidISOMonthDay, SSuggestTemporalDateRange);
    end;
    if CalendarId <> 'iso8601' then
    begin
      if not HasReferenceYear then
      begin
        if not TryResolveReferenceMonthDay(CalendarId, M, D,
          HasMonthCode, IsLeapMonth, Overflow, DateRec) then
          ThrowRangeError(SErrorInvalidISOMonthDay, SSuggestTemporalDateRange);
      end
      else
      begin
        if HasMonth then
        begin
          CandidateMonth := ToIntegerWithTruncationValue(VMonth);
          if not TryResolveCalendarDateToISO(CalendarId, SuppliedYear,
            CandidateMonth, D, False, False, Overflow = toConstrain, DateRec) then
            ThrowRangeError(SErrorInvalidISOMonthDay, SSuggestTemporalDateRange);
          if not TryGetCalendarDateInfo(CalendarId, DateRec.Year, DateRec.Month,
            DateRec.Day, Info) then
            ThrowRangeError(SErrorInvalidISOMonthDay, SSuggestTemporalDateRange);
          if HasMonthCode and (Info.MonthCode <> MonthCodeStr) then
            ThrowRangeError(SErrorInvalidMonthCodeMonthDay, SSuggestTemporalMonthCode);
        end
        else if not TryResolveCalendarDateToISO(CalendarId, SuppliedYear, M,
          D, True, IsLeapMonth, Overflow = toConstrain, DateRec) then
          ThrowRangeError(SErrorInvalidISOMonthDay, SSuggestTemporalDateRange);
        if not TryGetCalendarDateInfo(CalendarId, DateRec.Year, DateRec.Month,
          DateRec.Day, Info) then
          ThrowRangeError(SErrorInvalidISOMonthDay, SSuggestTemporalDateRange);
        if not TryParseTemporalMonthCode(Info.MonthCode, M, IsLeapMonth) then
          ThrowRangeError(SErrorInvalidISOMonthDay, SSuggestTemporalDateRange);
        D := Info.Date.Day;
        if not TryResolveReferenceMonthDay(CalendarId, M, D, True,
          IsLeapMonth, Overflow, DateRec) then
          ThrowRangeError(SErrorInvalidISOMonthDay, SSuggestTemporalDateRange);
      end;
      M := DateRec.Month;
      D := DateRec.Day;
      ReferenceYear := DateRec.Year;
    end
    else
    begin
      if HasReferenceYear and (D > DaysInMonth(SuppliedYear, M)) then
      begin
        if Overflow = toReject then
          ThrowRangeError(SErrorInvalidISOMonthDay, SSuggestTemporalDateRange);
        D := DaysInMonth(SuppliedYear, M);
      end;
      if not TryResolveReferenceMonthDay(CalendarId, M, D, True, False,
        Overflow, DateRec) then
        ThrowRangeError(SErrorInvalidISOMonthDay, SSuggestTemporalDateRange);
      M := DateRec.Month;
      D := DateRec.Day;
      ReferenceYear := DateRec.Year;
    end;
    Result := TGocciaTemporalPlainMonthDayValue.Create(M, D, ReferenceYear, CalendarId);
  end
  else
  begin
    ThrowTypeError(SErrorTemporalPlainMonthDayFromArg, SSuggestTemporalFromArg);
    Result := nil;
  end;
end;

function TGocciaTemporalBuiltin.PlainMonthDayCompare(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  MD1, MD2: TGocciaTemporalPlainMonthDayValue;
  CoerceArgs: TGocciaArgumentsCollection;

  function CoerceMD(const AArg: TGocciaValue): TGocciaTemporalPlainMonthDayValue;
  var
    Converted: TGocciaValue;
  begin
    CoerceArgs := TGocciaArgumentsCollection.Create([AArg]);
    try
      Converted := PlainMonthDayFrom(CoerceArgs, TGocciaUndefinedLiteralValue.UndefinedValue);
      Result := TGocciaTemporalPlainMonthDayValue(Converted);
    finally
      CoerceArgs.Free;
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
  ConstructorMethod.DefineProperty('from',
    TGocciaPropertyDescriptorData.Create(
      TGocciaNativeFunctionValue.CreateWithoutPrototype(ZonedDateTimeFrom, 'from', 1),
      [pfConfigurable, pfWritable]));
  ConstructorMethod.DefineProperty('compare',
    TGocciaPropertyDescriptorData.Create(
      TGocciaNativeFunctionValue.CreateWithoutPrototype(ZonedDateTimeCompare, 'compare', 2),
      [pfConfigurable, pfWritable]));
  FTemporalNamespace.DefineProperty('ZonedDateTime',
    TGocciaPropertyDescriptorData.Create(ConstructorMethod, [pfConfigurable, pfWritable]));
end;

// TC39 Temporal §6.1.1 new Temporal.ZonedDateTime(epochNanoseconds, timeZone, calendar)
function TGocciaTemporalBuiltin.ZonedDateTimeConstructorFn(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Arg, TimeZoneArg: TGocciaValue;
  BigNs, BigMs, BigSubMs: TBigInteger;
  Ms: Int64;
  SubMs: Integer;
  TZ, TimeZoneString: string;
begin
  RequireTemporalConstructorNew(AThisValue, 'ZonedDateTime');
  if AArgs.Length < 2 then
    ThrowTypeError(SErrorTemporalZonedDateTimeArgs, SSuggestTemporalTimezone);

  Arg := AArgs.GetElement(0);
  BigNs := TemporalToBigInt(Arg);
  if not IsValidEpochNanoseconds(BigNs) then
    ThrowRangeError(SErrorTemporalInstantOutOfRange, SSuggestTemporalInstantRange);
  BigMs := BigNs.Divide(TBigInteger.FromInt64(1000000));
  BigSubMs := BigNs.Modulo(TBigInteger.FromInt64(1000000));
  Ms := BigMs.ToInt64;
  SubMs := Integer(BigSubMs.ToInt64);
  TimeZoneArg := AArgs.GetElement(1);
  if not (TimeZoneArg is TGocciaStringLiteralValue) then
    ThrowTypeError('Temporal.ZonedDateTime timeZone must be a string',
      SSuggestTemporalTimezone);
  TimeZoneString := TGocciaStringLiteralValue(TimeZoneArg).Value;
  if TemporalStringLooksLikeDateTime(TimeZoneString) then
    ThrowRangeError(Format(SErrorUnknownTimezone, [TimeZoneString]),
      SSuggestTemporalTimezone);
  TZ := CanonicalizeTemporalTimeZoneIdentifier(TimeZoneString);

  Result := TGocciaTemporalZonedDateTimeValue.Create(Ms, SubMs, TZ,
    ToTemporalCalendarIdentifier(AArgs.GetElement(2), False));
end;

function TGocciaTemporalBuiltin.ZonedDateTimeFrom(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Arg: TGocciaValue;
  ZDT: TGocciaTemporalZonedDateTimeValue;
begin
  Arg := AArgs.GetElement(0);
  ZDT := CoerceTemporalZonedDateTime(Arg, 'Temporal.ZonedDateTime.from', AArgs.GetElement(1));
  Result := TGocciaTemporalZonedDateTimeValue.Create(
    ZDT.EpochMilliseconds, ZDT.SubMillisecondNanoseconds, ZDT.TimeZone, ZDT.CalendarId);
end;

function TGocciaTemporalBuiltin.ZonedDateTimeCompare(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Z1, Z2: TGocciaTemporalZonedDateTimeValue;
begin
  Z1 := CoerceTemporalZonedDateTime(AArgs.GetElement(0), 'Temporal.ZonedDateTime.compare');
  Z2 := CoerceTemporalZonedDateTime(AArgs.GetElement(1), 'Temporal.ZonedDateTime.compare');

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
  NowMembers: array[0..0] of TGocciaMemberDefinition;
begin
  NowObj := TGocciaObjectValue.Create(TGocciaObjectValue.SharedObjectPrototype);
  NowObj.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(NowInstant, 'instant', 0));
  NowObj.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(NowPlainDateISO, 'plainDateISO', 0));
  NowObj.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(NowPlainTimeISO, 'plainTimeISO', 0));
  NowObj.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(NowPlainDateTimeISO, 'plainDateTimeISO', 0));
  NowObj.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(NowTimeZoneId, 'timeZoneId', 0));
  NowObj.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(NowZonedDateTimeISO, 'zonedDateTimeISO', 0));
  NowMembers[0] := DefineSymbolDataProperty(
    TGocciaSymbolValue.WellKnownToStringTag,
    TGocciaStringLiteralValue.Create('Temporal.Now'),
    [pfConfigurable]);
  RegisterMemberDefinitions(NowObj, NowMembers);
  FTemporalNamespace.DefineProperty('Now',
    TGocciaPropertyDescriptorData.Create(NowObj, [pfConfigurable, pfWritable]));
end;

function TGocciaTemporalBuiltin.NowInstant(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  EpochNs: Int64;
begin
  EpochNs := FHostEnvironment.EpochNanoseconds;
  Result := TGocciaTemporalInstantValue.Create(EpochNs div 1000000, Integer(EpochNs mod 1000000));
end;

function TGocciaTemporalBuiltin.NowPlainDateISO(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  EpochNs: Int64;
  TZ: string;
  Y, M, D, H, Mi, S, Ms, Us, Ns: Integer;
begin
  EpochNs := FHostEnvironment.EpochNanoseconds;
  TZ := TemporalNowTimeZoneFromArgument(AArgs.GetElement(0),
    'Temporal.Now.plainDateISO', FHostEnvironment.TimeZoneIdentifier);
  GetLocalFieldsFromEpoch(EpochNs div 1000000, Integer(EpochNs mod 1000000),
    TZ, Y, M, D, H, Mi, S, Ms, Us, Ns);
  Result := TGocciaTemporalPlainDateValue.Create(Y, M, D);
end;

function TGocciaTemporalBuiltin.NowPlainTimeISO(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  EpochNs: Int64;
  TZ: string;
  Y, M, D, H, Mi, S, Ms, Us, Ns: Integer;
begin
  EpochNs := FHostEnvironment.EpochNanoseconds;
  TZ := TemporalNowTimeZoneFromArgument(AArgs.GetElement(0),
    'Temporal.Now.plainTimeISO', FHostEnvironment.TimeZoneIdentifier);
  GetLocalFieldsFromEpoch(EpochNs div 1000000, Integer(EpochNs mod 1000000),
    TZ, Y, M, D, H, Mi, S, Ms, Us, Ns);
  Result := TGocciaTemporalPlainTimeValue.Create(H, Mi, S, Ms, Us, Ns);
end;

function TGocciaTemporalBuiltin.NowPlainDateTimeISO(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  EpochNs: Int64;
  TZ: string;
  Y, Mo, D, H, Mi, S, Ms, Us, Ns: Integer;
begin
  EpochNs := FHostEnvironment.EpochNanoseconds;
  TZ := TemporalNowTimeZoneFromArgument(AArgs.GetElement(0),
    'Temporal.Now.plainDateTimeISO', FHostEnvironment.TimeZoneIdentifier);
  GetLocalFieldsFromEpoch(EpochNs div 1000000, Integer(EpochNs mod 1000000),
    TZ, Y, Mo, D, H, Mi, S, Ms, Us, Ns);
  Result := TGocciaTemporalPlainDateTimeValue.Create(Y, Mo, D, H, Mi, S, Ms,
    Us, Ns);
end;

// TC39 Temporal §2.2.1 Temporal.Now.timeZoneId()
function TGocciaTemporalBuiltin.NowTimeZoneId(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaStringLiteralValue.Create(
    FHostEnvironment.TimeZoneIdentifier);
end;

// TC39 Temporal §2.2.2 Temporal.Now.zonedDateTimeISO([timeZone])
function TGocciaTemporalBuiltin.NowZonedDateTimeISO(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  EpochNs: Int64;
  TZ: string;
begin
  EpochNs := FHostEnvironment.EpochNanoseconds;
  TZ := TemporalNowTimeZoneFromArgument(AArgs.GetElement(0),
    'Temporal.Now.zonedDateTimeISO', FHostEnvironment.TimeZoneIdentifier);

  Result := TGocciaTemporalZonedDateTimeValue.Create(
    EpochNs div 1000000, Integer(EpochNs mod 1000000), TZ);
end;

end.
