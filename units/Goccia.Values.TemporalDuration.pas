unit Goccia.Values.TemporalDuration;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.ObjectModel,
  Goccia.SharedPrototype,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaTemporalDurationValue = class(TGocciaObjectValue)
  private
    FYears: Int64;
    FMonths: Int64;
    FWeeks: Int64;
    FDays: Int64;
    FHours: Int64;
    FMinutes: Int64;
    FSeconds: Int64;
    FMilliseconds: Int64;
    FMicroseconds: Int64;
    FNanoseconds: Int64;

    procedure InitializePrototype;

    function ComputeSign: Integer;
    function IsBlank: Boolean;
    function ToISOString: string;
  public
    constructor Create(const AYears, AMonths, AWeeks, ADays, AHours, AMinutes, ASeconds,
      AMilliseconds, AMicroseconds, ANanoseconds: Int64); overload;

    function ToStringTag: string; override;
    class procedure ExposePrototype(const AConstructor: TGocciaObjectValue);

    property Years: Int64 read FYears;
    property Months: Int64 read FMonths;
    property Weeks: Int64 read FWeeks;
    property Days: Int64 read FDays;
    property Hours: Int64 read FHours;
    property Minutes: Int64 read FMinutes;
    property Seconds: Int64 read FSeconds;
    property Milliseconds: Int64 read FMilliseconds;
    property Microseconds: Int64 read FMicroseconds;
    property Nanoseconds: Int64 read FNanoseconds;
  published
    function GetYears(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetMonths(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetWeeks(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetDays(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetHours(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetMinutes(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetSeconds(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetMilliseconds(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetMicroseconds(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetNanoseconds(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetSign(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetBlank(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DurationNegated(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DurationAbs(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DurationAdd(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DurationSubtract(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DurationWith(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DurationRound(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DurationToString(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DurationToJSON(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DurationValueOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DurationTotal(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  end;

implementation

uses
  SysUtils,

  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.Temporal.Options,
  Goccia.Temporal.Utils,
  Goccia.Values.ErrorHelper,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.TemporalPlainDate;

threadvar
  FShared: TGocciaSharedPrototype;
  FPrototypeMembers: TArray<TGocciaMemberDefinition>;

function AsDuration(const AValue: TGocciaValue; const AMethod: string): TGocciaTemporalDurationValue;
begin
  if not (AValue is TGocciaTemporalDurationValue) then
    ThrowTypeError(AMethod + ' called on non-Duration', SSuggestTemporalThisType);
  Result := TGocciaTemporalDurationValue(AValue);
end;

function DurationFromObject(const AObj: TGocciaObjectValue): TGocciaTemporalDurationValue;

  function GetFieldOr(const AObj: TGocciaObjectValue; const AName: string; const ADefault: Int64): Int64;
  var
    V: TGocciaValue;
  begin
    V := AObj.GetProperty(AName);
    if (V = nil) or (V is TGocciaUndefinedLiteralValue) then
      Result := ADefault
    else
      Result := Trunc(V.ToNumberLiteral.Value);
  end;

begin
  Result := TGocciaTemporalDurationValue.Create(
    GetFieldOr(AObj, 'years', 0),
    GetFieldOr(AObj, 'months', 0),
    GetFieldOr(AObj, 'weeks', 0),
    GetFieldOr(AObj, 'days', 0),
    GetFieldOr(AObj, 'hours', 0),
    GetFieldOr(AObj, 'minutes', 0),
    GetFieldOr(AObj, 'seconds', 0),
    GetFieldOr(AObj, 'milliseconds', 0),
    GetFieldOr(AObj, 'microseconds', 0),
    GetFieldOr(AObj, 'nanoseconds', 0)
  );
end;

{ TGocciaTemporalDurationValue }

constructor TGocciaTemporalDurationValue.Create(const AYears, AMonths, AWeeks, ADays, AHours, AMinutes, ASeconds,
  AMilliseconds, AMicroseconds, ANanoseconds: Int64);
var
  HasPositive, HasNegative: Boolean;
begin
  inherited Create(nil);
  FYears := AYears;
  FMonths := AMonths;
  FWeeks := AWeeks;
  FDays := ADays;
  FHours := AHours;
  FMinutes := AMinutes;
  FSeconds := ASeconds;
  FMilliseconds := AMilliseconds;
  FMicroseconds := AMicroseconds;
  FNanoseconds := ANanoseconds;

  // Validate: sign of non-zero components must be uniform
  HasPositive := (AYears > 0) or (AMonths > 0) or (AWeeks > 0) or (ADays > 0) or
                 (AHours > 0) or (AMinutes > 0) or (ASeconds > 0) or
                 (AMilliseconds > 0) or (AMicroseconds > 0) or (ANanoseconds > 0);
  HasNegative := (AYears < 0) or (AMonths < 0) or (AWeeks < 0) or (ADays < 0) or
                 (AHours < 0) or (AMinutes < 0) or (ASeconds < 0) or
                 (AMilliseconds < 0) or (AMicroseconds < 0) or (ANanoseconds < 0);
  if HasPositive and HasNegative then
    ThrowRangeError(SErrorDurationMixedSigns, SSuggestTemporalDurationSigns);

  InitializePrototype;
  if Assigned(FShared) then
    FPrototype := FShared.Prototype;
end;

procedure TGocciaTemporalDurationValue.InitializePrototype;
var
  Members: TGocciaMemberCollection;
begin
  if Assigned(FShared) then Exit;

  FShared := TGocciaSharedPrototype.Create(Self);
  if Length(FPrototypeMembers) = 0 then
  begin
    Members := TGocciaMemberCollection.Create;
    try
      Members.AddAccessor('years', GetYears, nil, [pfConfigurable]);
      Members.AddAccessor('months', GetMonths, nil, [pfConfigurable]);
      Members.AddAccessor('weeks', GetWeeks, nil, [pfConfigurable]);
      Members.AddAccessor('days', GetDays, nil, [pfConfigurable]);
      Members.AddAccessor('hours', GetHours, nil, [pfConfigurable]);
      Members.AddAccessor('minutes', GetMinutes, nil, [pfConfigurable]);
      Members.AddAccessor('seconds', GetSeconds, nil, [pfConfigurable]);
      Members.AddAccessor('milliseconds', GetMilliseconds, nil, [pfConfigurable]);
      Members.AddAccessor('microseconds', GetMicroseconds, nil, [pfConfigurable]);
      Members.AddAccessor('nanoseconds', GetNanoseconds, nil, [pfConfigurable]);
      Members.AddAccessor('sign', GetSign, nil, [pfConfigurable]);
      Members.AddAccessor('blank', GetBlank, nil, [pfConfigurable]);
      Members.AddMethod(DurationNegated, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(DurationAbs, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(DurationAdd, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(DurationSubtract, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(DurationWith, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(DurationRound, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(DurationTotal, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(DurationToString, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(DurationToJSON, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(DurationValueOf, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      FPrototypeMembers := Members.ToDefinitions;
    finally
      Members.Free;
    end;
  end;
  RegisterMemberDefinitions(FShared.Prototype, FPrototypeMembers);
end;

class procedure TGocciaTemporalDurationValue.ExposePrototype(const AConstructor: TGocciaObjectValue);
begin
  if not Assigned(FShared) then
    TGocciaTemporalDurationValue.Create(0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
  ExposeSharedPrototypeOnConstructor(FShared, AConstructor);
end;

function TGocciaTemporalDurationValue.ComputeSign: Integer;
begin
  if (FYears > 0) or (FMonths > 0) or (FWeeks > 0) or (FDays > 0) or
     (FHours > 0) or (FMinutes > 0) or (FSeconds > 0) or
     (FMilliseconds > 0) or (FMicroseconds > 0) or (FNanoseconds > 0) then
    Result := 1
  else if (FYears < 0) or (FMonths < 0) or (FWeeks < 0) or (FDays < 0) or
          (FHours < 0) or (FMinutes < 0) or (FSeconds < 0) or
          (FMilliseconds < 0) or (FMicroseconds < 0) or (FNanoseconds < 0) then
    Result := -1
  else
    Result := 0;
end;

function TGocciaTemporalDurationValue.IsBlank: Boolean;
begin
  Result := ComputeSign = 0;
end;

function TGocciaTemporalDurationValue.ToISOString: string;
var
  DatePart, TimePart: string;
  ASign: Integer;
  AbsY, AbsMo, AbsW, AbsD, AbsH, AbsMi, AbsS, AbsMs, AbsUs, AbsNs: Int64;
begin
  ASign := ComputeSign;
  if ASign = 0 then
  begin
    Result := 'PT0S';
    Exit;
  end;

  AbsY := Abs(FYears);
  AbsMo := Abs(FMonths);
  AbsW := Abs(FWeeks);
  AbsD := Abs(FDays);
  AbsH := Abs(FHours);
  AbsMi := Abs(FMinutes);
  AbsS := Abs(FSeconds);
  AbsMs := Abs(FMilliseconds);
  AbsUs := Abs(FMicroseconds);
  AbsNs := Abs(FNanoseconds);

  DatePart := '';
  if AbsY > 0 then DatePart := DatePart + IntToStr(AbsY) + 'Y';
  if AbsMo > 0 then DatePart := DatePart + IntToStr(AbsMo) + 'M';
  if AbsW > 0 then DatePart := DatePart + IntToStr(AbsW) + 'W';
  if AbsD > 0 then DatePart := DatePart + IntToStr(AbsD) + 'D';

  TimePart := '';
  if AbsH > 0 then TimePart := TimePart + IntToStr(AbsH) + 'H';
  if AbsMi > 0 then TimePart := TimePart + IntToStr(AbsMi) + 'M';

  if (AbsS > 0) or (AbsMs > 0) or (AbsUs > 0) or (AbsNs > 0) then
  begin
    TimePart := TimePart + IntToStr(AbsS);
    if (AbsMs > 0) or (AbsUs > 0) or (AbsNs > 0) then
    begin
      TimePart := TimePart + '.';
      if AbsNs > 0 then
        TimePart := TimePart + Format('%.3d%.3d%.3d', [AbsMs, AbsUs, AbsNs])
      else if AbsUs > 0 then
        TimePart := TimePart + Format('%.3d%.3d', [AbsMs, AbsUs])
      else
        TimePart := TimePart + Format('%.3d', [AbsMs]);
    end;
    TimePart := TimePart + 'S';
  end;

  Result := '';
  if ASign < 0 then
    Result := '-';
  Result := Result + 'P' + DatePart;
  if Length(TimePart) > 0 then
    Result := Result + 'T' + TimePart;
end;

function TGocciaTemporalDurationValue.ToStringTag: string;
begin
  Result := 'Temporal.Duration';
end;

{ Getters }

function TGocciaTemporalDurationValue.GetYears(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaNumberLiteralValue.Create(AsDuration(AThisValue, 'get Duration.years').FYears);
end;

function TGocciaTemporalDurationValue.GetMonths(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaNumberLiteralValue.Create(AsDuration(AThisValue, 'get Duration.months').FMonths);
end;

function TGocciaTemporalDurationValue.GetWeeks(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaNumberLiteralValue.Create(AsDuration(AThisValue, 'get Duration.weeks').FWeeks);
end;

function TGocciaTemporalDurationValue.GetDays(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaNumberLiteralValue.Create(AsDuration(AThisValue, 'get Duration.days').FDays);
end;

function TGocciaTemporalDurationValue.GetHours(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaNumberLiteralValue.Create(AsDuration(AThisValue, 'get Duration.hours').FHours);
end;

function TGocciaTemporalDurationValue.GetMinutes(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaNumberLiteralValue.Create(AsDuration(AThisValue, 'get Duration.minutes').FMinutes);
end;

function TGocciaTemporalDurationValue.GetSeconds(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaNumberLiteralValue.Create(AsDuration(AThisValue, 'get Duration.seconds').FSeconds);
end;

function TGocciaTemporalDurationValue.GetMilliseconds(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaNumberLiteralValue.Create(AsDuration(AThisValue, 'get Duration.milliseconds').FMilliseconds);
end;

function TGocciaTemporalDurationValue.GetMicroseconds(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaNumberLiteralValue.Create(AsDuration(AThisValue, 'get Duration.microseconds').FMicroseconds);
end;

function TGocciaTemporalDurationValue.GetNanoseconds(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaNumberLiteralValue.Create(AsDuration(AThisValue, 'get Duration.nanoseconds').FNanoseconds);
end;

function TGocciaTemporalDurationValue.GetSign(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaNumberLiteralValue.Create(AsDuration(AThisValue, 'get Duration.sign').ComputeSign);
end;

function TGocciaTemporalDurationValue.GetBlank(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  if AsDuration(AThisValue, 'get Duration.blank').IsBlank then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;

{ Methods }

function TGocciaTemporalDurationValue.DurationNegated(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalDurationValue;
begin
  D := AsDuration(AThisValue, 'Duration.prototype.negated');
  Result := TGocciaTemporalDurationValue.Create(
    -D.FYears, -D.FMonths, -D.FWeeks, -D.FDays,
    -D.FHours, -D.FMinutes, -D.FSeconds,
    -D.FMilliseconds, -D.FMicroseconds, -D.FNanoseconds);
end;

function TGocciaTemporalDurationValue.DurationAbs(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalDurationValue;
begin
  D := AsDuration(AThisValue, 'Duration.prototype.abs');
  Result := TGocciaTemporalDurationValue.Create(
    Abs(D.FYears), Abs(D.FMonths), Abs(D.FWeeks), Abs(D.FDays),
    Abs(D.FHours), Abs(D.FMinutes), Abs(D.FSeconds),
    Abs(D.FMilliseconds), Abs(D.FMicroseconds), Abs(D.FNanoseconds));
end;

function TGocciaTemporalDurationValue.DurationAdd(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D, Other: TGocciaTemporalDurationValue;
  Arg: TGocciaValue;
  DurRec: TTemporalDurationRecord;
begin
  D := AsDuration(AThisValue, 'Duration.prototype.add');
  Arg := AArgs.GetElement(0);

  if Arg is TGocciaTemporalDurationValue then
    Other := TGocciaTemporalDurationValue(Arg)
  else if Arg is TGocciaStringLiteralValue then
  begin
    Other := nil;
    // Parse ISO duration string
    if TryParseISODuration(TGocciaStringLiteralValue(Arg).Value, DurRec) then
      Other := TGocciaTemporalDurationValue.Create(
        DurRec.Years, DurRec.Months, DurRec.Weeks, DurRec.Days,
        DurRec.Hours, DurRec.Minutes, DurRec.Seconds,
        DurRec.Milliseconds, DurRec.Microseconds, DurRec.Nanoseconds)
    else
      ThrowRangeError(SErrorInvalidDurationString, SSuggestTemporalDurationArg);
  end
  else if Arg is TGocciaObjectValue then
    Other := DurationFromObject(TGocciaObjectValue(Arg))
  else
  begin
    ThrowTypeError(SErrorInvalidDurationAddArg, SSuggestTemporalDurationArg);
    Other := nil;
  end;

  Result := TGocciaTemporalDurationValue.Create(
    D.FYears + Other.FYears, D.FMonths + Other.FMonths,
    D.FWeeks + Other.FWeeks, D.FDays + Other.FDays,
    D.FHours + Other.FHours, D.FMinutes + Other.FMinutes,
    D.FSeconds + Other.FSeconds, D.FMilliseconds + Other.FMilliseconds,
    D.FMicroseconds + Other.FMicroseconds, D.FNanoseconds + Other.FNanoseconds);
end;

function TGocciaTemporalDurationValue.DurationSubtract(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D, Other: TGocciaTemporalDurationValue;
  Arg: TGocciaValue;
  DurRec: TTemporalDurationRecord;
begin
  D := AsDuration(AThisValue, 'Duration.prototype.subtract');
  Arg := AArgs.GetElement(0);

  if Arg is TGocciaTemporalDurationValue then
    Other := TGocciaTemporalDurationValue(Arg)
  else if Arg is TGocciaStringLiteralValue then
  begin
    Other := nil;
    if TryParseISODuration(TGocciaStringLiteralValue(Arg).Value, DurRec) then
      Other := TGocciaTemporalDurationValue.Create(
        DurRec.Years, DurRec.Months, DurRec.Weeks, DurRec.Days,
        DurRec.Hours, DurRec.Minutes, DurRec.Seconds,
        DurRec.Milliseconds, DurRec.Microseconds, DurRec.Nanoseconds)
    else
      ThrowRangeError(SErrorInvalidDurationString, SSuggestTemporalDurationArg);
  end
  else if Arg is TGocciaObjectValue then
    Other := DurationFromObject(TGocciaObjectValue(Arg))
  else
  begin
    ThrowTypeError(SErrorInvalidDurationSubtractArg, SSuggestTemporalDurationArg);
    Other := nil;
  end;

  Result := TGocciaTemporalDurationValue.Create(
    D.FYears - Other.FYears, D.FMonths - Other.FMonths,
    D.FWeeks - Other.FWeeks, D.FDays - Other.FDays,
    D.FHours - Other.FHours, D.FMinutes - Other.FMinutes,
    D.FSeconds - Other.FSeconds, D.FMilliseconds - Other.FMilliseconds,
    D.FMicroseconds - Other.FMicroseconds, D.FNanoseconds - Other.FNanoseconds);
end;

function TGocciaTemporalDurationValue.DurationWith(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalDurationValue;
  Obj: TGocciaObjectValue;
  V: TGocciaValue;
  NewYears, NewMonths, NewWeeks, NewDays, NewHours, NewMinutes, NewSeconds,
  NewMilliseconds, NewMicroseconds, NewNanoseconds: Int64;

  function GetFieldOr(const AName: string; const ADefault: Int64): Int64;
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
  D := AsDuration(AThisValue, 'Duration.prototype.with');
  V := AArgs.GetElement(0);
  if not (V is TGocciaObjectValue) then
    ThrowTypeError(Format(SErrorTemporalWithRequiresObject, ['Duration']), SSuggestTemporalWithObject);
  Obj := TGocciaObjectValue(V);

  Result := TGocciaTemporalDurationValue.Create(
    GetFieldOr('years', D.FYears),
    GetFieldOr('months', D.FMonths),
    GetFieldOr('weeks', D.FWeeks),
    GetFieldOr('days', D.FDays),
    GetFieldOr('hours', D.FHours),
    GetFieldOr('minutes', D.FMinutes),
    GetFieldOr('seconds', D.FSeconds),
    GetFieldOr('milliseconds', D.FMilliseconds),
    GetFieldOr('microseconds', D.FMicroseconds),
    GetFieldOr('nanoseconds', D.FNanoseconds));
end;

// TC39 Temporal §7.3.21 Temporal.Duration.prototype.round(roundTo)
function TGocciaTemporalDurationValue.DurationRound(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalDurationValue;
  Arg, RelToArg: TGocciaValue;
  OptionsObj: TGocciaObjectValue;
  SmallestUnit, LargestUnit: TTemporalUnit;
  Mode: TTemporalRoundingMode;
  Increment: Integer;
  HasRelativeTo: Boolean;
  RelDate: TTemporalDateRecord;
  RelTimeRec: TTemporalTimeRecord;
  TotalNs, Divisor: Int64;
  RemNs, TimeNs: Int64;
  ResultYears, ResultMonths: Int64;
  ResultDays, ResultHours, ResultMinutes, ResultSeconds: Int64;
  ResultMs, ResultUs, ResultNs: Int64;
  NeedCalendar: Boolean;
  IntermDate, EndDate, WalkDate, NextDate: TTemporalDateRecord;
  StartEpoch, EndEpoch, WalkEpoch, NextEpoch: Int64;
  WholeUnits, PeriodNs, ScaledValue, RoundedValue: Int64;
  Sign: Integer;
begin
  D := AsDuration(AThisValue, 'Duration.prototype.round');
  Arg := AArgs.GetElement(0);

  SmallestUnit := tuNone;
  LargestUnit := tuNone;
  Mode := rmHalfExpand;
  Increment := 1;
  HasRelativeTo := False;

  if Arg is TGocciaStringLiteralValue then
  begin
    if not GetTemporalUnitFromString(TGocciaStringLiteralValue(Arg).Value, SmallestUnit) then
      ThrowRangeError(Format(SErrorTemporalInvalidUnitFor, ['Duration.prototype.round', TGocciaStringLiteralValue(Arg).Value]), SSuggestTemporalValidUnits);
  end
  else if Arg is TGocciaObjectValue then
  begin
    OptionsObj := TGocciaObjectValue(Arg);
    SmallestUnit := GetSmallestUnit(OptionsObj, tuNone);
    LargestUnit := GetLargestUnit(OptionsObj, tuNone);
    Mode := GetRoundingMode(OptionsObj, rmHalfExpand);
    Increment := GetRoundingIncrement(OptionsObj, 1);

    RelToArg := OptionsObj.GetProperty('relativeTo');
    if (RelToArg <> nil) and not (RelToArg is TGocciaUndefinedLiteralValue) then
    begin
      if RelToArg is TGocciaTemporalPlainDateValue then
      begin
        RelDate.Year := TGocciaTemporalPlainDateValue(RelToArg).Year;
        RelDate.Month := TGocciaTemporalPlainDateValue(RelToArg).Month;
        RelDate.Day := TGocciaTemporalPlainDateValue(RelToArg).Day;
        HasRelativeTo := True;
      end
      else if RelToArg is TGocciaStringLiteralValue then
      begin
        if TryParseISODate(TGocciaStringLiteralValue(RelToArg).Value, RelDate) then
          HasRelativeTo := True
        else if TryParseISODateTime(TGocciaStringLiteralValue(RelToArg).Value, RelDate, RelTimeRec) then
          HasRelativeTo := True
        else
          ThrowRangeError(Format(SErrorTemporalInvalidISOStringFor, ['relativeTo', 'Duration.prototype.round']), SSuggestTemporalISOFormat);
      end
      else
        ThrowTypeError('relativeTo must be a Temporal.PlainDate or ISO 8601 string', SSuggestTemporalRelativeTo);
    end;
  end
  else
    ThrowTypeError(Format(SErrorTemporalRoundRequiresStringOrOptions, ['Duration']), SSuggestTemporalRoundArg);

  // Validate units
  if (SmallestUnit = tuNone) and (LargestUnit = tuNone) then
    ThrowRangeError(SErrorDurationRoundRequiresUnit, SSuggestTemporalRoundArg);
  if SmallestUnit = tuNone then
    SmallestUnit := tuNanosecond;
  if LargestUnit = tuNone then
  begin
    if D.FYears <> 0 then LargestUnit := tuYear
    else if D.FMonths <> 0 then LargestUnit := tuMonth
    else if D.FWeeks <> 0 then LargestUnit := tuWeek
    else if D.FDays <> 0 then LargestUnit := tuDay
    else if D.FHours <> 0 then LargestUnit := tuHour
    else if D.FMinutes <> 0 then LargestUnit := tuMinute
    else if D.FSeconds <> 0 then LargestUnit := tuSecond
    else if D.FMilliseconds <> 0 then LargestUnit := tuMillisecond
    else if D.FMicroseconds <> 0 then LargestUnit := tuMicrosecond
    else LargestUnit := SmallestUnit;
    if Ord(LargestUnit) > Ord(SmallestUnit) then
      LargestUnit := SmallestUnit;
  end;

  // Determine if calendar-relative computation is needed
  NeedCalendar := (D.FYears <> 0) or (D.FMonths <> 0) or
    (Ord(SmallestUnit) <= Ord(tuMonth)) or (Ord(LargestUnit) <= Ord(tuMonth));

  if NeedCalendar and not HasRelativeTo then
    ThrowRangeError(SErrorDurationRoundRequiresRelativeTo, SSuggestTemporalRelativeTo);

  // --- Calendar-relative path ---
  if NeedCalendar then
  begin
    Sign := D.ComputeSign;
    if Sign = 0 then
    begin
      Result := TGocciaTemporalDurationValue.Create(0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
      Exit;
    end;

    // Resolve duration to concrete end date. Apply years and months as
    // separate calendar steps so day-clamping is applied after each,
    // matching TC39 Temporal semantics (e.g. 2020-02-29 + P1Y1M clamps to
    // 2021-02-28 after +1Y, then advances to 2021-03-28).
    IntermDate := RelDate;
    if D.FYears <> 0 then
      IntermDate := AddMonthsToDate(IntermDate.Year, IntermDate.Month, IntermDate.Day,
        D.FYears * 12);
    if D.FMonths <> 0 then
      IntermDate := AddMonthsToDate(IntermDate.Year, IntermDate.Month, IntermDate.Day,
        D.FMonths);
    EndDate := AddDaysToDate(IntermDate.Year, IntermDate.Month, IntermDate.Day,
      D.FWeeks * 7 + D.FDays);
    StartEpoch := DateToEpochDays(RelDate.Year, RelDate.Month, RelDate.Day);
    EndEpoch := DateToEpochDays(EndDate.Year, EndDate.Month, EndDate.Day);

    TimeNs := D.FNanoseconds +
              D.FMicroseconds * NANOSECONDS_PER_MICROSECOND +
              D.FMilliseconds * NANOSECONDS_PER_MILLISECOND +
              D.FSeconds * NANOSECONDS_PER_SECOND +
              D.FMinutes * NANOSECONDS_PER_MINUTE +
              D.FHours * NANOSECONDS_PER_HOUR;

    // --- Round to year or month ---
    if (SmallestUnit = tuYear) or (SmallestUnit = tuMonth) then
    begin
      if SmallestUnit = tuYear then
      begin
        // Walk years from relativeTo
        WholeUnits := 0;
        if Sign > 0 then
        begin
          repeat
            NextDate := AddMonthsToDate(RelDate.Year, RelDate.Month, RelDate.Day, (WholeUnits + 1) * 12);
            NextEpoch := DateToEpochDays(NextDate.Year, NextDate.Month, NextDate.Day);
            if (NextEpoch > EndEpoch) or ((NextEpoch = EndEpoch) and (TimeNs <= 0)) then
              Break;
            Inc(WholeUnits);
          until False;
        end
        else
        begin
          repeat
            NextDate := AddMonthsToDate(RelDate.Year, RelDate.Month, RelDate.Day, (WholeUnits - 1) * 12);
            NextEpoch := DateToEpochDays(NextDate.Year, NextDate.Month, NextDate.Day);
            if (NextEpoch < EndEpoch) or ((NextEpoch = EndEpoch) and (TimeNs >= 0)) then
              Break;
            Dec(WholeUnits);
          until False;
        end;

        WalkDate := AddMonthsToDate(RelDate.Year, RelDate.Month, RelDate.Day, WholeUnits * 12);
        WalkEpoch := DateToEpochDays(WalkDate.Year, WalkDate.Month, WalkDate.Day);
        NextDate := AddMonthsToDate(RelDate.Year, RelDate.Month, RelDate.Day, (WholeUnits + Sign) * 12);
        NextEpoch := DateToEpochDays(NextDate.Year, NextDate.Month, NextDate.Day);
        PeriodNs := Abs(NextEpoch - WalkEpoch) * NANOSECONDS_PER_DAY;

        ScaledValue := WholeUnits * PeriodNs + (EndEpoch - WalkEpoch) * NANOSECONDS_PER_DAY + TimeNs;
        Divisor := PeriodNs * Increment;
        RoundedValue := RoundWithMode(ScaledValue, Divisor, Mode);
        WholeUnits := RoundedValue div PeriodNs;

        Result := TGocciaTemporalDurationValue.Create(WholeUnits, 0, 0, 0, 0, 0, 0, 0, 0, 0);
      end
      else // SmallestUnit = tuMonth
      begin
        // Walk months from relativeTo
        WholeUnits := 0;
        if Sign > 0 then
        begin
          repeat
            NextDate := AddMonthsToDate(RelDate.Year, RelDate.Month, RelDate.Day, WholeUnits + 1);
            NextEpoch := DateToEpochDays(NextDate.Year, NextDate.Month, NextDate.Day);
            if (NextEpoch > EndEpoch) or ((NextEpoch = EndEpoch) and (TimeNs <= 0)) then
              Break;
            Inc(WholeUnits);
          until False;
        end
        else
        begin
          repeat
            NextDate := AddMonthsToDate(RelDate.Year, RelDate.Month, RelDate.Day, WholeUnits - 1);
            NextEpoch := DateToEpochDays(NextDate.Year, NextDate.Month, NextDate.Day);
            if (NextEpoch < EndEpoch) or ((NextEpoch = EndEpoch) and (TimeNs >= 0)) then
              Break;
            Dec(WholeUnits);
          until False;
        end;

        WalkDate := AddMonthsToDate(RelDate.Year, RelDate.Month, RelDate.Day, WholeUnits);
        WalkEpoch := DateToEpochDays(WalkDate.Year, WalkDate.Month, WalkDate.Day);
        NextDate := AddMonthsToDate(RelDate.Year, RelDate.Month, RelDate.Day, WholeUnits + Sign);
        NextEpoch := DateToEpochDays(NextDate.Year, NextDate.Month, NextDate.Day);
        PeriodNs := Abs(NextEpoch - WalkEpoch) * NANOSECONDS_PER_DAY;

        ScaledValue := WholeUnits * PeriodNs + (EndEpoch - WalkEpoch) * NANOSECONDS_PER_DAY + TimeNs;
        Divisor := PeriodNs * Increment;
        RoundedValue := RoundWithMode(ScaledValue, Divisor, Mode);
        WholeUnits := RoundedValue div PeriodNs;

        if Ord(LargestUnit) <= Ord(tuYear) then
        begin
          ResultYears := WholeUnits div 12;
          ResultMonths := WholeUnits mod 12;
        end
        else
        begin
          ResultYears := 0;
          ResultMonths := WholeUnits;
        end;

        Result := TGocciaTemporalDurationValue.Create(ResultYears, ResultMonths, 0, 0, 0, 0, 0, 0, 0, 0);
      end;
      Exit;
    end;

    // --- SmallestUnit is week, day, or time: convert to nanoseconds ---
    TotalNs := (EndEpoch - StartEpoch) * NANOSECONDS_PER_DAY + TimeNs;

    if SmallestUnit <> tuNanosecond then
    begin
      if SmallestUnit = tuWeek then
        Divisor := NANOSECONDS_PER_DAY * 7 * Increment
      else
        Divisor := UnitToNanoseconds(SmallestUnit) * Increment;
      TotalNs := RoundWithMode(TotalNs, Divisor, Mode);
    end;

    // --- Rebalance to year/month if needed ---
    if Ord(LargestUnit) <= Ord(tuMonth) then
    begin
      ResultDays := TotalNs div NANOSECONDS_PER_DAY;
      RemNs := TotalNs mod NANOSECONDS_PER_DAY;

      EndEpoch := StartEpoch + ResultDays;
      WholeUnits := 0;
      if Sign > 0 then
      begin
        repeat
          NextDate := AddMonthsToDate(RelDate.Year, RelDate.Month, RelDate.Day, WholeUnits + 1);
          NextEpoch := DateToEpochDays(NextDate.Year, NextDate.Month, NextDate.Day);
          if NextEpoch > EndEpoch then Break;
          Inc(WholeUnits);
        until False;
      end
      else
      begin
        repeat
          NextDate := AddMonthsToDate(RelDate.Year, RelDate.Month, RelDate.Day, WholeUnits - 1);
          NextEpoch := DateToEpochDays(NextDate.Year, NextDate.Month, NextDate.Day);
          if NextEpoch < EndEpoch then Break;
          Dec(WholeUnits);
        until False;
      end;

      WalkDate := AddMonthsToDate(RelDate.Year, RelDate.Month, RelDate.Day, WholeUnits);
      WalkEpoch := DateToEpochDays(WalkDate.Year, WalkDate.Month, WalkDate.Day);
      ResultDays := EndEpoch - WalkEpoch;

      if Ord(LargestUnit) <= Ord(tuYear) then
      begin
        ResultYears := WholeUnits div 12;
        ResultMonths := WholeUnits mod 12;
      end
      else
      begin
        ResultYears := 0;
        ResultMonths := WholeUnits;
      end;

      ResultHours := RemNs div NANOSECONDS_PER_HOUR;
      RemNs := RemNs mod NANOSECONDS_PER_HOUR;
      ResultMinutes := RemNs div NANOSECONDS_PER_MINUTE;
      RemNs := RemNs mod NANOSECONDS_PER_MINUTE;
      ResultSeconds := RemNs div NANOSECONDS_PER_SECOND;
      RemNs := RemNs mod NANOSECONDS_PER_SECOND;
      ResultMs := RemNs div NANOSECONDS_PER_MILLISECOND;
      RemNs := RemNs mod NANOSECONDS_PER_MILLISECOND;
      ResultUs := RemNs div NANOSECONDS_PER_MICROSECOND;
      ResultNs := RemNs mod NANOSECONDS_PER_MICROSECOND;

      if Ord(LargestUnit) <= Ord(tuWeek) then
        Result := TGocciaTemporalDurationValue.Create(ResultYears, ResultMonths,
          ResultDays div 7, ResultDays mod 7,
          ResultHours, ResultMinutes, ResultSeconds, ResultMs, ResultUs, ResultNs)
      else
        Result := TGocciaTemporalDurationValue.Create(ResultYears, ResultMonths, 0, ResultDays,
          ResultHours, ResultMinutes, ResultSeconds, ResultMs, ResultUs, ResultNs);
      Exit;
    end;
    // Fall through to non-calendar breakdown below
  end
  else
  begin
    // --- Non-calendar path (existing behavior) ---
    TotalNs := D.FNanoseconds +
               D.FMicroseconds * NANOSECONDS_PER_MICROSECOND +
               D.FMilliseconds * NANOSECONDS_PER_MILLISECOND +
               D.FSeconds * NANOSECONDS_PER_SECOND +
               D.FMinutes * NANOSECONDS_PER_MINUTE +
               D.FHours * NANOSECONDS_PER_HOUR +
               D.FDays * NANOSECONDS_PER_DAY +
               D.FWeeks * 7 * NANOSECONDS_PER_DAY;

    if SmallestUnit <> tuNanosecond then
    begin
      if SmallestUnit = tuWeek then
        Divisor := NANOSECONDS_PER_DAY * 7 * Increment
      else
        Divisor := UnitToNanoseconds(SmallestUnit) * Increment;
      TotalNs := RoundWithMode(TotalNs, Divisor, Mode);
    end;
  end;

  // Break down from largestUnit (non-calendar output)
  ResultDays := 0;
  ResultHours := 0;
  ResultMinutes := 0;
  ResultSeconds := 0;
  ResultMs := 0;
  ResultUs := 0;
  ResultNs := 0;
  RemNs := TotalNs;

  if Ord(LargestUnit) <= Ord(tuDay) then
  begin
    ResultDays := RemNs div NANOSECONDS_PER_DAY;
    RemNs := RemNs mod NANOSECONDS_PER_DAY;
  end;
  if Ord(LargestUnit) <= Ord(tuHour) then
  begin
    ResultHours := RemNs div NANOSECONDS_PER_HOUR;
    RemNs := RemNs mod NANOSECONDS_PER_HOUR;
  end;
  if Ord(LargestUnit) <= Ord(tuMinute) then
  begin
    ResultMinutes := RemNs div NANOSECONDS_PER_MINUTE;
    RemNs := RemNs mod NANOSECONDS_PER_MINUTE;
  end;
  if Ord(LargestUnit) <= Ord(tuSecond) then
  begin
    ResultSeconds := RemNs div NANOSECONDS_PER_SECOND;
    RemNs := RemNs mod NANOSECONDS_PER_SECOND;
  end;
  if Ord(LargestUnit) <= Ord(tuMillisecond) then
  begin
    ResultMs := RemNs div NANOSECONDS_PER_MILLISECOND;
    RemNs := RemNs mod NANOSECONDS_PER_MILLISECOND;
  end;
  if Ord(LargestUnit) <= Ord(tuMicrosecond) then
  begin
    ResultUs := RemNs div NANOSECONDS_PER_MICROSECOND;
    RemNs := RemNs mod NANOSECONDS_PER_MICROSECOND;
  end;
  ResultNs := RemNs;

  if Ord(LargestUnit) <= Ord(tuWeek) then
    Result := TGocciaTemporalDurationValue.Create(0, 0, ResultDays div 7, ResultDays mod 7,
      ResultHours, ResultMinutes, ResultSeconds, ResultMs, ResultUs, ResultNs)
  else
    Result := TGocciaTemporalDurationValue.Create(0, 0, 0, ResultDays,
      ResultHours, ResultMinutes, ResultSeconds, ResultMs, ResultUs, ResultNs);
end;

function ParseRelativeTo(const AValue: TGocciaValue; const AMethod: string): TTemporalDateRecord;
var
  DateRec: TTemporalDateRecord;
  TimeRec: TTemporalTimeRecord;
  PlainDate: TGocciaTemporalPlainDateValue;
begin
  if AValue is TGocciaTemporalPlainDateValue then
  begin
    PlainDate := TGocciaTemporalPlainDateValue(AValue);
    Result.Year := PlainDate.Year;
    Result.Month := PlainDate.Month;
    Result.Day := PlainDate.Day;
  end
  else if AValue is TGocciaStringLiteralValue then
  begin
    if not TryParseISODate(TGocciaStringLiteralValue(AValue).Value, DateRec) then
    begin
      if TryParseISODateTime(TGocciaStringLiteralValue(AValue).Value, DateRec, TimeRec) then
        Result := DateRec
      else
        ThrowRangeError(Format(SErrorTemporalInvalidRelativeTo, [AMethod]), SSuggestTemporalRelativeTo);
    end
    else
      Result := DateRec;
  end
  else
    ThrowRangeError(Format(SErrorTemporalInvalidRelativeTo, [AMethod]), SSuggestTemporalRelativeTo);
end;

// Add AMonths calendar months to a date, clamping the day if needed.
function AddMonthsToDate(const ADate: TTemporalDateRecord; const AMonths: Int64): TTemporalDateRecord;
var
  TotalMonths, Y: Int64;
  M, D: Integer;
begin
  TotalMonths := Int64(ADate.Year) * 12 + Int64(ADate.Month - 1) + AMonths;
  if TotalMonths >= 0 then
  begin
    Y := TotalMonths div 12;
    M := Integer(TotalMonths mod 12) + 1;
  end
  else
  begin
    Y := (TotalMonths - 11) div 12;
    M := Integer(TotalMonths - Y * 12) + 1;
  end;
  D := ADate.Day;
  if D > DaysInMonth(Integer(Y), M) then
    D := DaysInMonth(Integer(Y), M);
  Result.Year := Integer(Y);
  Result.Month := M;
  Result.Day := D;
end;

// Compute the end date of a duration applied from a reference date using
// ISO 8601 calendar arithmetic (add years+months, clamp day, add weeks+days).
function DurationEndDate(const ADate: TTemporalDateRecord;
  const AYears, AMonths, AWeeks, ADays: Int64): TTemporalDateRecord;
var
  Interm: TTemporalDateRecord;
begin
  // Apply years and months as separate steps so the day clamps after each,
  // matching TC39 Temporal calendar arithmetic (e.g. 2020-02-29 + P1Y1M
  // clamps to 2021-02-28 after +1Y, then advances to 2021-03-28).
  Interm := ADate;
  if AYears <> 0 then
    Interm := AddMonthsToDate(Interm, AYears * 12);
  if AMonths <> 0 then
    Interm := AddMonthsToDate(Interm, AMonths);
  Result := AddDaysToDate(Interm.Year, Interm.Month, Interm.Day, AWeeks * 7 + ADays);
end;

// Resolve calendar-relative duration components (years, months, weeks) to a
// concrete day count by walking forward from a reference date.
function ResolveRelativeDays(const ADate: TTemporalDateRecord;
  const AYears, AMonths, AWeeks, ADays: Int64): Int64;
var
  EndRec: TTemporalDateRecord;
begin
  EndRec := DurationEndDate(ADate, AYears, AMonths, AWeeks, ADays);
  Result := DateToEpochDays(EndRec.Year, EndRec.Month, EndRec.Day) -
            DateToEpochDays(ADate.Year, ADate.Month, ADate.Day);
end;

function SubDayNanoseconds(const D: TGocciaTemporalDurationValue): Double;
var
  Ns, V: Double;
begin
  // Accumulate via implicit Int64->Double assignment to avoid both FPC 3.2.2
  // bugs: Bug A (Double(Int64) bit reinterpretation) and Bug B (Int64 * 1.0
  // wrong results near +/-2^31 on AArch64). See docs/contributing/tooling.md.
  Ns := D.FNanoseconds;
  V := D.FMicroseconds; Ns := Ns + V * NANOSECONDS_PER_MICROSECOND;
  V := D.FMilliseconds; Ns := Ns + V * NANOSECONDS_PER_MILLISECOND;
  V := D.FSeconds;      Ns := Ns + V * NANOSECONDS_PER_SECOND;
  V := D.FMinutes;      Ns := Ns + V * NANOSECONDS_PER_MINUTE;
  V := D.FHours;        Ns := Ns + V * NANOSECONDS_PER_HOUR;
  Result := Ns;
end;

// Express the duration as a fractional month count relative to a reference date.
// Walks forward from the reference date by the duration's calendar components,
// then measures the result in calendar months plus a fractional remainder.
// Shared calendar-walk helper for TotalInMonths and TotalInYears.
// AMonthsPerUnit is 1 for months, 12 for years.
function ComputeTotalInUnits(const D: TGocciaTemporalDurationValue;
  const ARelDate: TTemporalDateRecord; const AMonthsPerUnit: Int64): Double;
var
  EndRec, CheckRec, SpanRec: TTemporalDateRecord;
  WholeUnits, CarryDays: Int64;
  TotalMonthsDiff: Int64;
  CheckEpoch, EndEpoch, SpanEpoch: Int64;
  RemainingDays, DaysInSpan: Int64;
  TimeNs, RemainderNs, FracDays: Double;
begin
  // Fold sub-day time overflow into whole days so the calendar walk is accurate
  TimeNs := SubDayNanoseconds(D);
  CarryDays := Trunc(TimeNs / NANOSECONDS_PER_DAY);
  RemainderNs := TimeNs - CarryDays * NANOSECONDS_PER_DAY;

  EndRec := DurationEndDate(ARelDate, D.FYears, D.FMonths, D.FWeeks, D.FDays + CarryDays);
  EndEpoch := DateToEpochDays(EndRec.Year, EndRec.Month, EndRec.Day);

  // Estimate whole units from the month difference
  TotalMonthsDiff := Int64(EndRec.Year - ARelDate.Year) * 12 +
                     Int64(EndRec.Month - ARelDate.Month);
  WholeUnits := TotalMonthsDiff div AMonthsPerUnit;

  // Verify estimate by walking from the reference date
  CheckRec := AddMonthsToDate(ARelDate, WholeUnits * AMonthsPerUnit);
  CheckEpoch := DateToEpochDays(CheckRec.Year, CheckRec.Month, CheckRec.Day);

  // Adjust if overshot forward
  while (WholeUnits > 0) and (CheckEpoch > EndEpoch) do
  begin
    Dec(WholeUnits);
    CheckRec := AddMonthsToDate(ARelDate, WholeUnits * AMonthsPerUnit);
    CheckEpoch := DateToEpochDays(CheckRec.Year, CheckRec.Month, CheckRec.Day);
  end;
  // Adjust if overshot backward
  while (WholeUnits < 0) and (CheckEpoch < EndEpoch) do
  begin
    Inc(WholeUnits);
    CheckRec := AddMonthsToDate(ARelDate, WholeUnits * AMonthsPerUnit);
    CheckEpoch := DateToEpochDays(CheckRec.Year, CheckRec.Month, CheckRec.Day);
  end;

  // Fractional part: remaining days + sub-day remainder only
  RemainingDays := EndEpoch - CheckEpoch;
  FracDays := RemainingDays + RemainderNs / NANOSECONDS_PER_DAY;

  // Determine the unit span for the fractional calculation
  if FracDays >= 0 then
  begin
    SpanRec := AddMonthsToDate(ARelDate, (WholeUnits + 1) * AMonthsPerUnit);
    SpanEpoch := DateToEpochDays(SpanRec.Year, SpanRec.Month, SpanRec.Day);
    DaysInSpan := SpanEpoch - CheckEpoch;
  end
  else
  begin
    SpanRec := AddMonthsToDate(ARelDate, (WholeUnits - 1) * AMonthsPerUnit);
    SpanEpoch := DateToEpochDays(SpanRec.Year, SpanRec.Month, SpanRec.Day);
    DaysInSpan := CheckEpoch - SpanEpoch;
  end;

  if DaysInSpan > 0 then
    Result := WholeUnits + FracDays / DaysInSpan
  else
    Result := WholeUnits;
end;

function TotalInMonths(const D: TGocciaTemporalDurationValue;
  const ARelDate: TTemporalDateRecord): TGocciaValue;
begin
  Result := TGocciaNumberLiteralValue.Create(ComputeTotalInUnits(D, ARelDate, 1));
end;

// Express the duration as a fractional year count relative to a reference date.
function TotalInYears(const D: TGocciaTemporalDurationValue;
  const ARelDate: TTemporalDateRecord): TGocciaValue;
begin
  Result := TGocciaNumberLiteralValue.Create(ComputeTotalInUnits(D, ARelDate, 12));
end;

function TGocciaTemporalDurationValue.DurationTotal(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalDurationValue;
  UnitStr: string;
  TargetUnit: TTemporalUnit;
  Arg, RelToArg: TGocciaValue;
  OptionsObj: TGocciaObjectValue;
  HasRelativeTo: Boolean;
  RelDate: TTemporalDateRecord;
  ResolvedDays: Int64;
  TotalNs, V: Double;
begin
  D := AsDuration(AThisValue, 'Duration.prototype.total');
  Arg := AArgs.GetElement(0);
  HasRelativeTo := False;

  if Arg is TGocciaStringLiteralValue then
    UnitStr := TGocciaStringLiteralValue(Arg).Value
  else if Arg is TGocciaObjectValue then
  begin
    OptionsObj := TGocciaObjectValue(Arg);

    Arg := OptionsObj.GetProperty('unit');
    if (Arg = nil) or (Arg is TGocciaUndefinedLiteralValue) then
      ThrowRangeError(SErrorDurationTotalRequiresUnit, SSuggestTemporalValidUnits);
    UnitStr := Arg.ToStringLiteral.Value;

    RelToArg := OptionsObj.GetProperty('relativeTo');
    HasRelativeTo := (RelToArg <> nil) and not (RelToArg is TGocciaUndefinedLiteralValue);
  end
  else
  begin
    ThrowTypeError(SErrorDurationTotalRequiresStringOrOptions, SSuggestTemporalValidUnits);
    UnitStr := '';
  end;

  // Validate unit (accepts both singular and plural: 'hour'/'hours', 'day'/'days', etc.)
  if not GetTemporalUnitFromString(UnitStr, TargetUnit) or
     (TargetUnit = tuAuto) or (TargetUnit = tuNone) then
  begin
    ThrowRangeError(Format(SErrorTemporalInvalidUnitFor, ['Duration.prototype.total', UnitStr]), SSuggestTemporalValidUnits);
    TargetUnit := tuNanosecond;
  end;

  // Calendar target units always require relativeTo
  if (TargetUnit = tuMonth) or (TargetUnit = tuYear) then
  begin
    if not HasRelativeTo then
      ThrowRangeError(SErrorDurationTotalRequiresRelativeTo, SSuggestTemporalRelativeTo);
    RelDate := ParseRelativeTo(RelToArg, 'Duration.prototype.total');
    if TargetUnit = tuMonth then
      Result := TotalInMonths(D, RelDate)
    else
      Result := TotalInYears(D, RelDate);
    Exit;
  end;

  // Calendar source components (years/months) require relativeTo
  if (D.FYears <> 0) or (D.FMonths <> 0) then
  begin
    if not HasRelativeTo then
      ThrowRangeError(SErrorDurationTotalRequiresRelativeTo, SSuggestTemporalRelativeTo);

    RelDate := ParseRelativeTo(RelToArg, 'Duration.prototype.total');
    ResolvedDays := ResolveRelativeDays(RelDate, D.FYears, D.FMonths, D.FWeeks, D.FDays);

    V := ResolvedDays;
    TotalNs := SubDayNanoseconds(D) + V * NANOSECONDS_PER_DAY;
  end
  else
  begin
    TotalNs := SubDayNanoseconds(D);
    V := D.FDays;  TotalNs := TotalNs + V * NANOSECONDS_PER_DAY;
    V := D.FWeeks; TotalNs := TotalNs + V * 7 * NANOSECONDS_PER_DAY;
  end;

  if TargetUnit = tuNanosecond then
    Result := TGocciaNumberLiteralValue.Create(TotalNs)
  else if TargetUnit = tuWeek then
    Result := TGocciaNumberLiteralValue.Create(TotalNs / (7 * NANOSECONDS_PER_DAY))
  else
    Result := TGocciaNumberLiteralValue.Create(TotalNs / UnitToNanoseconds(TargetUnit));
end;

function TGocciaTemporalDurationValue.DurationToString(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaStringLiteralValue.Create(
    AsDuration(AThisValue, 'Duration.prototype.toString').ToISOString);
end;

function TGocciaTemporalDurationValue.DurationToJSON(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaStringLiteralValue.Create(
    AsDuration(AThisValue, 'Duration.prototype.toJSON').ToISOString);
end;

function TGocciaTemporalDurationValue.DurationValueOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  ThrowTypeError(Format(SErrorTemporalValueOf, ['Duration', 'toString or compare']), SSuggestTemporalNoValueOf);
  Result := nil;
end;

end.
