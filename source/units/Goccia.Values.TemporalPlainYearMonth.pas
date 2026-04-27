unit Goccia.Values.TemporalPlainYearMonth;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.ObjectModel,
  Goccia.SharedPrototype,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaTemporalPlainYearMonthValue = class(TGocciaObjectValue)
  private
    FYear: Integer;
    FMonth: Integer;
    FReferenceDay: Integer;
  published
    function GetCalendarId(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetYear(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetMonth(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetMonthCode(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetDaysInMonth(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetDaysInYear(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetMonthsInYear(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetInLeapYear(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;

    function YearMonthWith(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function YearMonthAdd(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function YearMonthSubtract(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function YearMonthUntil(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function YearMonthSince(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function YearMonthEquals(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function YearMonthToString(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function YearMonthToJSON(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function YearMonthValueOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function YearMonthToPlainDate(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function YearMonthToLocaleString(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;

    procedure InitializePrototype;
  public
    constructor Create(const AYear, AMonth: Integer; const AReferenceDay: Integer = 1); overload;

    function ToStringTag: string; override;
    class procedure ExposePrototype(const AConstructor: TGocciaObjectValue);

    property Year: Integer read FYear;
    property Month: Integer read FMonth;
    property ReferenceDay: Integer read FReferenceDay;
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
  Goccia.Values.TemporalDuration,
  Goccia.Values.TemporalPlainDate;

var
  GTemporalPlainYearMonthSharedSlot: TGocciaRealmOwnedSlotId;

threadvar
  FPrototypeMembers: TArray<TGocciaMemberDefinition>;

function GetTemporalPlainYearMonthShared: TGocciaSharedPrototype; inline;
begin
  if Assigned(CurrentRealm) then
    Result := TGocciaSharedPrototype(CurrentRealm.GetOwnedSlot(GTemporalPlainYearMonthSharedSlot))
  else
    Result := nil;
end;

function FormatYearMonthString(const AYear, AMonth: Integer): string;
begin
  Result := PadISOYear(AYear) + '-' + PadTwo(AMonth);
end;

function AsPlainYearMonth(const AValue: TGocciaValue; const AMethod: string): TGocciaTemporalPlainYearMonthValue;
begin
  if not (AValue is TGocciaTemporalPlainYearMonthValue) then
    ThrowTypeError(AMethod + ' called on non-PlainYearMonth', SSuggestTemporalThisType);
  Result := TGocciaTemporalPlainYearMonthValue(AValue);
end;

function CoercePlainYearMonth(const AValue: TGocciaValue; const AMethod: string): TGocciaTemporalPlainYearMonthValue;
var
  Obj: TGocciaObjectValue;
  V, VMonth: TGocciaValue;
  Year, Month: Integer;
begin
  if AValue is TGocciaTemporalPlainYearMonthValue then
    Result := TGocciaTemporalPlainYearMonthValue(AValue)
  else if AValue is TGocciaStringLiteralValue then
  begin
    if not CoerceToISOYearMonth(TGocciaStringLiteralValue(AValue).Value, Year, Month) then
      ThrowRangeError(Format(SErrorInvalidYearMonthStringFor, [AMethod]), SSuggestTemporalISOFormat);
    Result := TGocciaTemporalPlainYearMonthValue.Create(Year, Month);
  end
  else if AValue is TGocciaObjectValue then
  begin
    Obj := TGocciaObjectValue(AValue);
    V := Obj.GetProperty('year');
    if (V = nil) or (V is TGocciaUndefinedLiteralValue) then
      ThrowTypeError(AMethod + ' requires year and month properties', SSuggestTemporalFromArg);
    VMonth := Obj.GetProperty('month');
    if (VMonth = nil) or (VMonth is TGocciaUndefinedLiteralValue) then
      ThrowTypeError(AMethod + ' requires year and month properties', SSuggestTemporalFromArg);
    Result := TGocciaTemporalPlainYearMonthValue.Create(
      Trunc(V.ToNumberLiteral.Value),
      Trunc(VMonth.ToNumberLiteral.Value));
  end
  else
  begin
    ThrowTypeError(AMethod + ' requires a PlainYearMonth, string, or object', SSuggestTemporalFromArg);
    Result := nil;
  end;
end;

{ TGocciaTemporalPlainYearMonthValue }

constructor TGocciaTemporalPlainYearMonthValue.Create(const AYear, AMonth: Integer; const AReferenceDay: Integer = 1);
var
  MaxDay: Integer;
  Shared: TGocciaSharedPrototype;
begin
  inherited Create(nil);
  if (AMonth < 1) or (AMonth > 12) then
    ThrowRangeError(Format(SErrorInvalidMonth, [IntToStr(AMonth)]), SSuggestTemporalDateRange);
  FYear := AYear;
  FMonth := AMonth;
  MaxDay := Goccia.Temporal.Utils.DaysInMonth(AYear, AMonth);
  if AReferenceDay < 1 then
    FReferenceDay := 1
  else if AReferenceDay > MaxDay then
    FReferenceDay := MaxDay
  else
    FReferenceDay := AReferenceDay;
  InitializePrototype;
  Shared := GetTemporalPlainYearMonthShared;
  if Assigned(Shared) then
    FPrototype := Shared.Prototype;
end;

procedure TGocciaTemporalPlainYearMonthValue.InitializePrototype;
var
  Members: TGocciaMemberCollection;
  Shared: TGocciaSharedPrototype;
begin
  if not Assigned(CurrentRealm) then Exit;
  if Assigned(GetTemporalPlainYearMonthShared) then Exit;
  Shared := TGocciaSharedPrototype.Create(Self);
  CurrentRealm.SetOwnedSlot(GTemporalPlainYearMonthSharedSlot, Shared);
  if Length(FPrototypeMembers) = 0 then
  begin
    Members := TGocciaMemberCollection.Create;
    try
      Members.AddAccessor('calendarId', GetCalendarId, nil, [pfConfigurable]);
      Members.AddAccessor('year', GetYear, nil, [pfConfigurable]);
      Members.AddAccessor('month', GetMonth, nil, [pfConfigurable]);
      Members.AddAccessor('monthCode', GetMonthCode, nil, [pfConfigurable]);
      Members.AddAccessor('daysInMonth', GetDaysInMonth, nil, [pfConfigurable]);
      Members.AddAccessor('daysInYear', GetDaysInYear, nil, [pfConfigurable]);
      Members.AddAccessor('monthsInYear', GetMonthsInYear, nil, [pfConfigurable]);
      Members.AddAccessor('inLeapYear', GetInLeapYear, nil, [pfConfigurable]);
      Members.AddMethod(YearMonthWith, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(YearMonthAdd, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(YearMonthSubtract, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(YearMonthUntil, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(YearMonthSince, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(YearMonthEquals, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(YearMonthToString, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(YearMonthToJSON, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(YearMonthValueOf, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(YearMonthToPlainDate, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(YearMonthToLocaleString, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddSymbolDataProperty(
        TGocciaSymbolValue.WellKnownToStringTag,
        TGocciaStringLiteralValue.Create('Temporal.PlainYearMonth'),
        [pfConfigurable]);
      FPrototypeMembers := Members.ToDefinitions;
    finally
      Members.Free;
    end;
  end;
  RegisterMemberDefinitions(Shared.Prototype, FPrototypeMembers);
end;

class procedure TGocciaTemporalPlainYearMonthValue.ExposePrototype(const AConstructor: TGocciaObjectValue);
var
  Shared: TGocciaSharedPrototype;
begin
  Shared := GetTemporalPlainYearMonthShared;
  if not Assigned(Shared) then
  begin
    TGocciaTemporalPlainYearMonthValue.Create(1970, 1);
    Shared := GetTemporalPlainYearMonthShared;
  end;
  // InitializePrototype exits early when CurrentRealm is nil, so the lazy
  // Create() can leave Shared still nil — guard before deref.
  if Assigned(Shared) then
    ExposeSharedPrototypeOnConstructor(Shared, AConstructor);
end;

function TGocciaTemporalPlainYearMonthValue.ToStringTag: string;
begin
  Result := 'Temporal.PlainYearMonth';
end;

{ Getters }

// TC39 Temporal §10.3.3 get Temporal.PlainYearMonth.prototype.calendarId
function TGocciaTemporalPlainYearMonthValue.GetCalendarId(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  AsPlainYearMonth(AThisValue, 'get PlainYearMonth.calendarId');
  Result := TGocciaStringLiteralValue.Create('iso8601');
end;

// TC39 Temporal §10.3.4 get Temporal.PlainYearMonth.prototype.year
function TGocciaTemporalPlainYearMonthValue.GetYear(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaNumberLiteralValue.Create(AsPlainYearMonth(AThisValue, 'get PlainYearMonth.year').FYear);
end;

// TC39 Temporal §10.3.5 get Temporal.PlainYearMonth.prototype.month
function TGocciaTemporalPlainYearMonthValue.GetMonth(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaNumberLiteralValue.Create(AsPlainYearMonth(AThisValue, 'get PlainYearMonth.month').FMonth);
end;

// TC39 Temporal §10.3.6 get Temporal.PlainYearMonth.prototype.monthCode
function TGocciaTemporalPlainYearMonthValue.GetMonthCode(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaStringLiteralValue.Create('M' + PadTwo(AsPlainYearMonth(AThisValue, 'get PlainYearMonth.monthCode').FMonth));
end;

// TC39 Temporal §10.3.7 get Temporal.PlainYearMonth.prototype.daysInMonth
function TGocciaTemporalPlainYearMonthValue.GetDaysInMonth(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  YM: TGocciaTemporalPlainYearMonthValue;
begin
  YM := AsPlainYearMonth(AThisValue, 'get PlainYearMonth.daysInMonth');
  Result := TGocciaNumberLiteralValue.Create(Goccia.Temporal.Utils.DaysInMonth(YM.FYear, YM.FMonth));
end;

// TC39 Temporal §10.3.8 get Temporal.PlainYearMonth.prototype.daysInYear
function TGocciaTemporalPlainYearMonthValue.GetDaysInYear(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  YM: TGocciaTemporalPlainYearMonthValue;
begin
  YM := AsPlainYearMonth(AThisValue, 'get PlainYearMonth.daysInYear');
  Result := TGocciaNumberLiteralValue.Create(Goccia.Temporal.Utils.DaysInYear(YM.FYear));
end;

// TC39 Temporal §10.3.9 get Temporal.PlainYearMonth.prototype.monthsInYear
function TGocciaTemporalPlainYearMonthValue.GetMonthsInYear(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  AsPlainYearMonth(AThisValue, 'get PlainYearMonth.monthsInYear');
  Result := TGocciaNumberLiteralValue.Create(12);
end;

// TC39 Temporal §10.3.10 get Temporal.PlainYearMonth.prototype.inLeapYear
function TGocciaTemporalPlainYearMonthValue.GetInLeapYear(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  if Goccia.Temporal.Utils.IsLeapYear(AsPlainYearMonth(AThisValue, 'get PlainYearMonth.inLeapYear').FYear) then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;

{ Methods }

// TC39 Temporal §10.3.11 Temporal.PlainYearMonth.prototype.with(temporalYearMonthLike)
function TGocciaTemporalPlainYearMonthValue.YearMonthWith(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  YM: TGocciaTemporalPlainYearMonthValue;
  Obj: TGocciaObjectValue;
  V: TGocciaValue;
  NewYear, NewMonth: Integer;

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
  YM := AsPlainYearMonth(AThisValue, 'PlainYearMonth.prototype.with');
  V := AArgs.GetElement(0);
  if not (V is TGocciaObjectValue) then
    ThrowTypeError(Format(SErrorTemporalWithRequiresObject, ['PlainYearMonth']), SSuggestTemporalWithObject);
  Obj := TGocciaObjectValue(V);

  NewYear := GetFieldOr('year', YM.FYear);
  NewMonth := GetFieldOr('month', YM.FMonth);
  Result := TGocciaTemporalPlainYearMonthValue.Create(NewYear, NewMonth, YM.FReferenceDay);
end;

// TC39 Temporal §10.3.12 Temporal.PlainYearMonth.prototype.add(temporalDurationLike)
function TGocciaTemporalPlainYearMonthValue.YearMonthAdd(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  YM: TGocciaTemporalPlainYearMonthValue;
  Dur: TGocciaTemporalDurationValue;
  Arg: TGocciaValue;
  ObjArg: TGocciaObjectValue;
  DurRec: TTemporalDurationRecord;
  NewYear, NewMonth: Integer;

  function GetDurFieldOr(const AObj: TGocciaObjectValue; const AName: string; const ADefault: Int64): Int64;
  var
    Val: TGocciaValue;
  begin
    Val := AObj.GetProperty(AName);
    if (Val = nil) or (Val is TGocciaUndefinedLiteralValue) then
      Result := ADefault
    else
      Result := Trunc(Val.ToNumberLiteral.Value);
  end;

begin
  YM := AsPlainYearMonth(AThisValue, 'PlainYearMonth.prototype.add');
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
    ObjArg := TGocciaObjectValue(Arg);
    Dur := TGocciaTemporalDurationValue.Create(
      GetDurFieldOr(ObjArg, 'years', 0),
      GetDurFieldOr(ObjArg, 'months', 0),
      0, 0, 0, 0, 0, 0, 0, 0);
  end
  else
  begin
    ThrowTypeError(Format(SErrorTemporalAddRequiresDuration, ['PlainYearMonth']), SSuggestTemporalDurationArg);
    Dur := nil;
  end;

  // Add years then months (days/time components are ignored for PlainYearMonth)
  NewYear := YM.FYear + Integer(Dur.Years);
  NewMonth := YM.FMonth + Integer(Dur.Months);

  // Normalize month overflow
  while NewMonth > 12 do
  begin
    Inc(NewYear);
    Dec(NewMonth, 12);
  end;
  while NewMonth < 1 do
  begin
    Dec(NewYear);
    Inc(NewMonth, 12);
  end;

  Result := TGocciaTemporalPlainYearMonthValue.Create(NewYear, NewMonth, YM.FReferenceDay);
end;

// TC39 Temporal §10.3.13 Temporal.PlainYearMonth.prototype.subtract(temporalDurationLike)
function TGocciaTemporalPlainYearMonthValue.YearMonthSubtract(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Dur: TGocciaTemporalDurationValue;
  Arg: TGocciaValue;
  ObjArg: TGocciaObjectValue;
  DurRec: TTemporalDurationRecord;
  NegatedDur: TGocciaTemporalDurationValue;
  NewArgs: TGocciaArgumentsCollection;

  function GetDurFieldOr(const AObj: TGocciaObjectValue; const AName: string; const ADefault: Int64): Int64;
  var
    Val: TGocciaValue;
  begin
    Val := AObj.GetProperty(AName);
    if (Val = nil) or (Val is TGocciaUndefinedLiteralValue) then
      Result := ADefault
    else
      Result := Trunc(Val.ToNumberLiteral.Value);
  end;

begin
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
    ObjArg := TGocciaObjectValue(Arg);
    Dur := TGocciaTemporalDurationValue.Create(
      GetDurFieldOr(ObjArg, 'years', 0),
      GetDurFieldOr(ObjArg, 'months', 0),
      0, 0, 0, 0, 0, 0, 0, 0);
  end
  else
  begin
    ThrowTypeError(Format(SErrorTemporalSubtractRequiresDuration, ['PlainYearMonth']), SSuggestTemporalDurationArg);
    Dur := nil;
  end;

  NegatedDur := TGocciaTemporalDurationValue.Create(
    -Dur.Years, -Dur.Months, -Dur.Weeks, -Dur.Days,
    -Dur.Hours, -Dur.Minutes, -Dur.Seconds,
    -Dur.Milliseconds, -Dur.Microseconds, -Dur.Nanoseconds);

  NewArgs := TGocciaArgumentsCollection.Create([NegatedDur]);
  try
    Result := YearMonthAdd(NewArgs, AThisValue);
  finally
    NewArgs.Free;
  end;
end;

// TC39 Temporal §10.3.14 Temporal.PlainYearMonth.prototype.until(other [, options])
function TGocciaTemporalPlainYearMonthValue.YearMonthUntil(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  YM, Other: TGocciaTemporalPlainYearMonthValue;
  OptionsObj: TGocciaObjectValue;
  LargestUnit, SmallestUnit: TTemporalUnit;
  RMode: TTemporalRoundingMode;
  RIncrement: Integer;
  TotalMonths1, TotalMonths2, DiffMonths: Int64;
  DiffYears, RemMonths: Int64;
  W, D, H, Mi, S, Ms, Us, Ns: Int64;
begin
  YM := AsPlainYearMonth(AThisValue, 'PlainYearMonth.prototype.until');
  Other := CoercePlainYearMonth(AArgs.GetElement(0), 'PlainYearMonth.prototype.until');

  OptionsObj := GetDiffOptions(AArgs, 1);
  LargestUnit := GetLargestUnit(OptionsObj, tuYear);
  if LargestUnit = tuAuto then LargestUnit := tuYear;
  if not (LargestUnit in [tuYear, tuMonth]) then
    ThrowRangeError(Format(SErrorTemporalInvalidUnitFor, ['PlainYearMonth.prototype.until', 'largestUnit']), SSuggestTemporalValidUnits);

  SmallestUnit := GetSmallestUnit(OptionsObj, tuMonth);
  if not (SmallestUnit in [tuYear, tuMonth]) then
    ThrowRangeError(Format(SErrorTemporalInvalidUnitFor, ['PlainYearMonth.prototype.until', 'smallestUnit']), SSuggestTemporalValidUnits);
  if Ord(LargestUnit) > Ord(SmallestUnit) then
    ThrowRangeError(SErrorDurationRoundLargestSmallerThanSmallest, SSuggestTemporalRoundArg);
  RMode := GetRoundingMode(OptionsObj, rmTrunc);
  RIncrement := GetRoundingIncrement(OptionsObj, 1);

  TotalMonths1 := Int64(YM.FYear) * 12 + Int64(YM.FMonth);
  TotalMonths2 := Int64(Other.FYear) * 12 + Int64(Other.FMonth);
  DiffMonths := TotalMonths2 - TotalMonths1;

  if LargestUnit = tuYear then
  begin
    DiffYears := DiffMonths div 12;
    RemMonths := DiffMonths mod 12;
  end
  else
  begin
    DiffYears := 0;
    RemMonths := DiffMonths;
  end;

  if (SmallestUnit <> tuMonth) or (RIncrement <> 1) then
  begin
    W := 0; D := 0; H := 0; Mi := 0; S := 0; Ms := 0; Us := 0; Ns := 0;
    RoundDiffDuration(DiffYears, RemMonths, W, D, H, Mi, S, Ms, Us, Ns,
      YM.FYear, YM.FMonth, 1,
      LargestUnit, SmallestUnit, RMode, RIncrement);
  end;

  Result := TGocciaTemporalDurationValue.Create(DiffYears, RemMonths, 0, 0, 0, 0, 0, 0, 0, 0);
end;

// TC39 Temporal §10.3.15 Temporal.PlainYearMonth.prototype.since(other [, options])
function TGocciaTemporalPlainYearMonthValue.YearMonthSince(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  YM, Other: TGocciaTemporalPlainYearMonthValue;
  OptionsObj: TGocciaObjectValue;
  LargestUnit, SmallestUnit: TTemporalUnit;
  RMode: TTemporalRoundingMode;
  RIncrement: Integer;
  TotalMonths1, TotalMonths2, DiffMonths: Int64;
  DiffYears, RemMonths: Int64;
  W, D, H, Mi, S, Ms, Us, Ns: Int64;
begin
  YM := AsPlainYearMonth(AThisValue, 'PlainYearMonth.prototype.since');
  Other := CoercePlainYearMonth(AArgs.GetElement(0), 'PlainYearMonth.prototype.since');

  OptionsObj := GetDiffOptions(AArgs, 1);
  LargestUnit := GetLargestUnit(OptionsObj, tuYear);
  if LargestUnit = tuAuto then LargestUnit := tuYear;
  if not (LargestUnit in [tuYear, tuMonth]) then
    ThrowRangeError(Format(SErrorTemporalInvalidUnitFor, ['PlainYearMonth.prototype.since', 'largestUnit']), SSuggestTemporalValidUnits);

  SmallestUnit := GetSmallestUnit(OptionsObj, tuMonth);
  if not (SmallestUnit in [tuYear, tuMonth]) then
    ThrowRangeError(Format(SErrorTemporalInvalidUnitFor, ['PlainYearMonth.prototype.since', 'smallestUnit']), SSuggestTemporalValidUnits);
  if Ord(LargestUnit) > Ord(SmallestUnit) then
    ThrowRangeError(SErrorDurationRoundLargestSmallerThanSmallest, SSuggestTemporalRoundArg);
  RMode := GetRoundingMode(OptionsObj, rmTrunc);
  RIncrement := GetRoundingIncrement(OptionsObj, 1);

  TotalMonths1 := Int64(YM.FYear) * 12 + Int64(YM.FMonth);
  TotalMonths2 := Int64(Other.FYear) * 12 + Int64(Other.FMonth);
  DiffMonths := TotalMonths1 - TotalMonths2;

  if LargestUnit = tuYear then
  begin
    DiffYears := DiffMonths div 12;
    RemMonths := DiffMonths mod 12;
  end
  else
  begin
    DiffYears := 0;
    RemMonths := DiffMonths;
  end;

  if (SmallestUnit <> tuMonth) or (RIncrement <> 1) then
  begin
    W := 0; D := 0; H := 0; Mi := 0; S := 0; Ms := 0; Us := 0; Ns := 0;
    RoundDiffDuration(DiffYears, RemMonths, W, D, H, Mi, S, Ms, Us, Ns,
      Other.FYear, Other.FMonth, 1,
      LargestUnit, SmallestUnit, RMode, RIncrement);
  end;

  Result := TGocciaTemporalDurationValue.Create(DiffYears, RemMonths, 0, 0, 0, 0, 0, 0, 0, 0);
end;

// TC39 Temporal §10.3.16 Temporal.PlainYearMonth.prototype.equals(other)
function TGocciaTemporalPlainYearMonthValue.YearMonthEquals(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  YM, Other: TGocciaTemporalPlainYearMonthValue;
begin
  YM := AsPlainYearMonth(AThisValue, 'PlainYearMonth.prototype.equals');
  Other := CoercePlainYearMonth(AArgs.GetElement(0), 'PlainYearMonth.prototype.equals');

  if (YM.FYear = Other.FYear) and (YM.FMonth = Other.FMonth) then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;

// TC39 Temporal §10.3.17 Temporal.PlainYearMonth.prototype.toString()
function TGocciaTemporalPlainYearMonthValue.YearMonthToString(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  YM: TGocciaTemporalPlainYearMonthValue;
  Arg: TGocciaValue;
  OptionsObj: TGocciaObjectValue;
  CalDisp: TTemporalCalendarDisplay;
begin
  YM := AsPlainYearMonth(AThisValue, 'PlainYearMonth.prototype.toString');
  OptionsObj := nil;
  Arg := AArgs.GetElement(0);
  if Assigned(Arg) and not (Arg is TGocciaUndefinedLiteralValue) then
  begin
    if not (Arg is TGocciaObjectValue) then
      ThrowTypeError('options must be an object or undefined', SSuggestTemporalFromArg);
    OptionsObj := TGocciaObjectValue(Arg);
  end;
  CalDisp := GetCalendarDisplay(OptionsObj);
  // When calendar annotation is present, include reference day for round-tripping
  if (CalDisp = tcdAlways) or (CalDisp = tcdCritical) then
    Result := TGocciaStringLiteralValue.Create(
      FormatDateString(YM.FYear, YM.FMonth, YM.FReferenceDay) + FormatCalendarAnnotation(CalDisp))
  else
    Result := TGocciaStringLiteralValue.Create(
      FormatYearMonthString(YM.FYear, YM.FMonth));
end;

// TC39 Temporal §10.3.18 Temporal.PlainYearMonth.prototype.toJSON()
function TGocciaTemporalPlainYearMonthValue.YearMonthToJSON(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  YM: TGocciaTemporalPlainYearMonthValue;
begin
  YM := AsPlainYearMonth(AThisValue, 'PlainYearMonth.prototype.toJSON');
  Result := TGocciaStringLiteralValue.Create(FormatYearMonthString(YM.FYear, YM.FMonth));
end;

// TC39 Temporal §10.3.19 Temporal.PlainYearMonth.prototype.valueOf()
function TGocciaTemporalPlainYearMonthValue.YearMonthValueOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  ThrowTypeError(Format(SErrorTemporalValueOf, ['PlainYearMonth', 'toString or compare']), SSuggestTemporalNoValueOf);
  Result := nil;
end;

// TC39 Temporal §10.3.20 Temporal.PlainYearMonth.prototype.toPlainDate(item)
function TGocciaTemporalPlainYearMonthValue.YearMonthToPlainDate(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  YM: TGocciaTemporalPlainYearMonthValue;
  Arg, V: TGocciaValue;
  Obj: TGocciaObjectValue;
  Day: Integer;
begin
  YM := AsPlainYearMonth(AThisValue, 'PlainYearMonth.prototype.toPlainDate');
  Arg := AArgs.GetElement(0);

  if not (Arg is TGocciaObjectValue) then
    ThrowTypeError(SErrorPlainYearMonthToPlainDateRequiresDay, SSuggestTemporalFromArg);
  Obj := TGocciaObjectValue(Arg);

  V := Obj.GetProperty('day');
  if (V = nil) or (V is TGocciaUndefinedLiteralValue) then
    ThrowTypeError(SErrorPlainYearMonthToPlainDateRequiresDay, SSuggestTemporalFromArg);

  Day := Trunc(V.ToNumberLiteral.Value);
  Result := TGocciaTemporalPlainDateValue.Create(YM.FYear, YM.FMonth, Day);
end;

function TGocciaTemporalPlainYearMonthValue.YearMonthToLocaleString(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  EmptyArgs: TGocciaArgumentsCollection;
begin
  EmptyArgs := TGocciaArgumentsCollection.Create([]);
  try
    Result := YearMonthToString(EmptyArgs, AThisValue);
  finally
    EmptyArgs.Free;
  end;
end;

initialization
  GTemporalPlainYearMonthSharedSlot := RegisterRealmOwnedSlot('Temporal.PlainYearMonth.shared');

end.
