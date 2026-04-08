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
    class var FShared: TGocciaSharedPrototype;
    class var FPrototypeMembers: array of TGocciaMemberDefinition;
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
    function DurationToString(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DurationToJSON(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DurationValueOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DurationTotal(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  end;

implementation

uses
  SysUtils,

  Goccia.Temporal.Utils,
  Goccia.Values.ErrorHelper,
  Goccia.Values.ObjectPropertyDescriptor;

function AsDuration(const AValue: TGocciaValue; const AMethod: string): TGocciaTemporalDurationValue;
begin
  if not (AValue is TGocciaTemporalDurationValue) then
    ThrowTypeError(AMethod + ' called on non-Duration');
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
    ThrowRangeError('Duration fields must not have mixed signs');

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
      ThrowTypeError('Invalid duration string');
  end
  else if Arg is TGocciaObjectValue then
    Other := DurationFromObject(TGocciaObjectValue(Arg))
  else
  begin
    ThrowTypeError('Invalid argument to Duration.prototype.add');
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
      ThrowTypeError('Invalid duration string');
  end
  else if Arg is TGocciaObjectValue then
    Other := DurationFromObject(TGocciaObjectValue(Arg))
  else
  begin
    ThrowTypeError('Invalid argument to Duration.prototype.subtract');
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
    ThrowTypeError('Duration.prototype.with requires an object argument');
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

function TGocciaTemporalDurationValue.DurationTotal(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalDurationValue;
  UnitStr: string;
  Arg, RelToArg: TGocciaValue;
  OptionsObj: TGocciaObjectValue;
  HasRelativeTo: Boolean;
  TotalNs: Double;
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
      ThrowRangeError('total() requires a unit option');
    UnitStr := Arg.ToStringLiteral.Value;

    RelToArg := OptionsObj.GetProperty('relativeTo');
    HasRelativeTo := (RelToArg <> nil) and not (RelToArg is TGocciaUndefinedLiteralValue);
  end
  else
  begin
    ThrowTypeError('Duration.prototype.total requires a string or options object');
    UnitStr := '';
  end;

  if (D.FYears <> 0) or (D.FMonths <> 0) then
  begin
    if HasRelativeTo then
      ThrowRangeError('relativeTo for Duration.prototype.total is not yet supported')
    else
      ThrowRangeError('Duration with years or months requires relativeTo for total()');
  end;

  TotalNs := D.FNanoseconds +
             D.FMicroseconds * 1000.0 +
             D.FMilliseconds * 1000000.0 +
             D.FSeconds * 1e9 +
             D.FMinutes * 6e10 +
             D.FHours * 3.6e12 +
             D.FDays * 8.64e13 +
             D.FWeeks * 6.048e14;

  if UnitStr = 'nanoseconds' then
    Result := TGocciaNumberLiteralValue.Create(TotalNs)
  else if UnitStr = 'microseconds' then
    Result := TGocciaNumberLiteralValue.Create(TotalNs / 1000)
  else if UnitStr = 'milliseconds' then
    Result := TGocciaNumberLiteralValue.Create(TotalNs / 1000000)
  else if UnitStr = 'seconds' then
    Result := TGocciaNumberLiteralValue.Create(TotalNs / 1e9)
  else if UnitStr = 'minutes' then
    Result := TGocciaNumberLiteralValue.Create(TotalNs / 6e10)
  else if UnitStr = 'hours' then
    Result := TGocciaNumberLiteralValue.Create(TotalNs / 3.6e12)
  else if UnitStr = 'days' then
    Result := TGocciaNumberLiteralValue.Create(TotalNs / 8.64e13)
  else if UnitStr = 'weeks' then
    Result := TGocciaNumberLiteralValue.Create(TotalNs / 6.048e14)
  else
  begin
    ThrowRangeError('Invalid unit for Duration.prototype.total: ' + UnitStr);
    Result := nil;
  end;
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
  ThrowTypeError('Temporal.Duration.prototype.valueOf cannot be used; use toString or compare instead');
  Result := nil;
end;

end.
