unit Goccia.Values.TemporalPlainMonthDay;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.ObjectModel,
  Goccia.SharedPrototype,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaTemporalPlainMonthDayValue = class(TGocciaObjectValue)
  private
    class var FShared: TGocciaSharedPrototype;
    class var FPrototypeMembers: array of TGocciaMemberDefinition;
  private
    FMonth: Integer;
    FDay: Integer;
    FReferenceYear: Integer;
  published
    function GetCalendarId(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetMonthCode(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetDay(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;

    function MonthDayWith(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function MonthDayEquals(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function MonthDayToString(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function MonthDayToJSON(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function MonthDayValueOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function MonthDayToPlainDate(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;

    procedure InitializePrototype;
  public
    constructor Create(const AMonth, ADay: Integer); overload;
    constructor Create(const AMonth, ADay, AReferenceYear: Integer); overload;

    function ToStringTag: string; override;
    class procedure ExposePrototype(const AConstructor: TGocciaObjectValue);

    property Month: Integer read FMonth;
    property Day: Integer read FDay;
    property ReferenceYear: Integer read FReferenceYear;
  end;

implementation

uses
  SysUtils,

  Goccia.Constants.PropertyNames,
  Goccia.Temporal.Utils,
  Goccia.Values.ErrorHelper,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.TemporalPlainDate;

const
  DEFAULT_REFERENCE_YEAR = 1972;

function AsPlainMonthDay(const AValue: TGocciaValue; const AMethod: string): TGocciaTemporalPlainMonthDayValue;
begin
  if not (AValue is TGocciaTemporalPlainMonthDayValue) then
    ThrowTypeError(AMethod + ' called on non-PlainMonthDay');
  Result := TGocciaTemporalPlainMonthDayValue(AValue);
end;

function CoercePlainMonthDay(const AValue: TGocciaValue; const AMethod: string): TGocciaTemporalPlainMonthDayValue;
var
  S: string;
  DashPos: Integer;
  MonthPart, DayPart: Integer;
  Obj: TGocciaObjectValue;
  V, VMonthCode, VDay: TGocciaValue;
  MonthCodeStr: string;
begin
  if AValue is TGocciaTemporalPlainMonthDayValue then
    Result := TGocciaTemporalPlainMonthDayValue.Create(
      TGocciaTemporalPlainMonthDayValue(AValue).FMonth,
      TGocciaTemporalPlainMonthDayValue(AValue).FDay,
      TGocciaTemporalPlainMonthDayValue(AValue).FReferenceYear)
  else if AValue is TGocciaStringLiteralValue then
  begin
    S := TGocciaStringLiteralValue(AValue).Value;
    DashPos := Pos('-', S);
    if (DashPos < 2) or (DashPos >= Length(S)) then
      ThrowTypeError('Invalid month-day string for ' + AMethod);
    if not TryStrToInt(Copy(S, 1, DashPos - 1), MonthPart) then
      ThrowTypeError('Invalid month-day string for ' + AMethod);
    if not TryStrToInt(Copy(S, DashPos + 1, Length(S) - DashPos), DayPart) then
      ThrowTypeError('Invalid month-day string for ' + AMethod);
    Result := TGocciaTemporalPlainMonthDayValue.Create(MonthPart, DayPart);
  end
  else if AValue is TGocciaObjectValue then
  begin
    Obj := TGocciaObjectValue(AValue);
    VDay := Obj.GetProperty(PROP_DAY);
    if (VDay = nil) or (VDay is TGocciaUndefinedLiteralValue) then
      ThrowTypeError(AMethod + ' requires day property');
    VMonthCode := Obj.GetProperty(PROP_MONTH_CODE);
    if Assigned(VMonthCode) and not (VMonthCode is TGocciaUndefinedLiteralValue) then
    begin
      MonthCodeStr := VMonthCode.ToStringLiteral.Value;
      if (Length(MonthCodeStr) <> 3) or (MonthCodeStr[1] <> 'M') or
         not (MonthCodeStr[2] in ['0'..'9']) or not (MonthCodeStr[3] in ['0'..'9']) then
        ThrowTypeError('Invalid monthCode for ' + AMethod);
      if not TryStrToInt(Copy(MonthCodeStr, 2, 2), MonthPart) then
        ThrowTypeError('Invalid monthCode for ' + AMethod);
      if (MonthPart < 1) or (MonthPart > 12) then
        ThrowRangeError('monthCode month out of range in ' + AMethod);
      // Validate month/monthCode consistency when both are provided
      V := Obj.GetProperty(PROP_MONTH);
      if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
        if Trunc(V.ToNumberLiteral.Value) <> MonthPart then
          ThrowRangeError('month and monthCode must match in ' + AMethod);
    end
    else
    begin
      V := Obj.GetProperty(PROP_MONTH);
      if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
        MonthPart := Trunc(V.ToNumberLiteral.Value)
      else
        ThrowTypeError(AMethod + ' requires monthCode or month property');
    end;
    Result := TGocciaTemporalPlainMonthDayValue.Create(
      MonthPart, Trunc(VDay.ToNumberLiteral.Value));
  end
  else
  begin
    ThrowTypeError(AMethod + ' requires a PlainMonthDay, string, or object');
    Result := nil;
  end;
end;

{ TGocciaTemporalPlainMonthDayValue }

constructor TGocciaTemporalPlainMonthDayValue.Create(const AMonth, ADay: Integer);
begin
  Create(AMonth, ADay, DEFAULT_REFERENCE_YEAR);
end;

constructor TGocciaTemporalPlainMonthDayValue.Create(const AMonth, ADay, AReferenceYear: Integer);
begin
  inherited Create(nil);
  if not IsValidDate(AReferenceYear, AMonth, ADay) then
    ThrowRangeError('Invalid month-day: ' + PadTwo(AMonth) + '-' + PadTwo(ADay));
  FMonth := AMonth;
  FDay := ADay;
  FReferenceYear := AReferenceYear;
  InitializePrototype;
  if Assigned(FShared) then
    FPrototype := FShared.Prototype;
end;

procedure TGocciaTemporalPlainMonthDayValue.InitializePrototype;
var
  Members: TGocciaMemberCollection;
begin
  if Assigned(FShared) then Exit;
  FShared := TGocciaSharedPrototype.Create(Self);
  if Length(FPrototypeMembers) = 0 then
  begin
    Members := TGocciaMemberCollection.Create;
    try
      Members.AddAccessor(PROP_CALENDAR_ID, GetCalendarId, nil, [pfConfigurable]);
      Members.AddAccessor(PROP_MONTH_CODE, GetMonthCode, nil, [pfConfigurable]);
      Members.AddAccessor(PROP_DAY, GetDay, nil, [pfConfigurable]);
      Members.AddMethod(MonthDayWith, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(MonthDayEquals, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(MonthDayToString, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(MonthDayToJSON, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(MonthDayValueOf, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(MonthDayToPlainDate, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      FPrototypeMembers := Members.ToDefinitions;
    finally
      Members.Free;
    end;
  end;
  RegisterMemberDefinitions(FShared.Prototype, FPrototypeMembers);
end;

class procedure TGocciaTemporalPlainMonthDayValue.ExposePrototype(const AConstructor: TGocciaObjectValue);
begin
  if not Assigned(FShared) then
    TGocciaTemporalPlainMonthDayValue.Create(1, 1);
  ExposeSharedPrototypeOnConstructor(FShared, AConstructor);
end;

function TGocciaTemporalPlainMonthDayValue.ToStringTag: string;
begin
  Result := 'Temporal.PlainMonthDay';
end;

{ Getters }

// TC39 Temporal §11.3.3 get Temporal.PlainMonthDay.prototype.calendarId
function TGocciaTemporalPlainMonthDayValue.GetCalendarId(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  AsPlainMonthDay(AThisValue, 'get PlainMonthDay.calendarId');
  Result := TGocciaStringLiteralValue.Create('iso8601');
end;

// TC39 Temporal §11.3.4 get Temporal.PlainMonthDay.prototype.monthCode
function TGocciaTemporalPlainMonthDayValue.GetMonthCode(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaStringLiteralValue.Create('M' + PadTwo(AsPlainMonthDay(AThisValue, 'get PlainMonthDay.monthCode').FMonth));
end;

// TC39 Temporal §11.3.5 get Temporal.PlainMonthDay.prototype.day
function TGocciaTemporalPlainMonthDayValue.GetDay(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaNumberLiteralValue.Create(AsPlainMonthDay(AThisValue, 'get PlainMonthDay.day').FDay);
end;

{ Methods }

// TC39 Temporal §11.3.6 Temporal.PlainMonthDay.prototype.with(temporalMonthDayLike)
function TGocciaTemporalPlainMonthDayValue.MonthDayWith(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  MD: TGocciaTemporalPlainMonthDayValue;
  Obj: TGocciaObjectValue;
  V: TGocciaValue;
  NewMonth, NewDay: Integer;
  MonthCodeStr: string;

  function GetDayFieldOr(const ADefault: Integer): Integer;
  var
    Val: TGocciaValue;
  begin
    Val := Obj.GetProperty(PROP_DAY);
    if (Val = nil) or (Val is TGocciaUndefinedLiteralValue) then
      Result := ADefault
    else
      Result := Trunc(Val.ToNumberLiteral.Value);
  end;

begin
  MD := AsPlainMonthDay(AThisValue, 'PlainMonthDay.prototype.with');
  V := AArgs.GetElement(0);
  if not (V is TGocciaObjectValue) then
    ThrowTypeError('PlainMonthDay.prototype.with requires an object argument');
  Obj := TGocciaObjectValue(V);

  NewMonth := MD.FMonth;
  V := Obj.GetProperty(PROP_MONTH_CODE);
  if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
  begin
    MonthCodeStr := V.ToStringLiteral.Value;
    if (Length(MonthCodeStr) <> 3) or (MonthCodeStr[1] <> 'M') or
       not (MonthCodeStr[2] in ['0'..'9']) or not (MonthCodeStr[3] in ['0'..'9']) then
      ThrowTypeError('Invalid monthCode in PlainMonthDay.prototype.with');
    if not TryStrToInt(Copy(MonthCodeStr, 2, 2), NewMonth) then
      ThrowTypeError('Invalid monthCode in PlainMonthDay.prototype.with');
    if (NewMonth < 1) or (NewMonth > 12) then
      ThrowRangeError('monthCode month out of range in PlainMonthDay.prototype.with');
    // Validate month/monthCode consistency when both are provided
    V := Obj.GetProperty(PROP_MONTH);
    if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
      if Trunc(V.ToNumberLiteral.Value) <> NewMonth then
        ThrowRangeError('month and monthCode must match in PlainMonthDay.prototype.with');
  end
  else
  begin
    V := Obj.GetProperty(PROP_MONTH);
    if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
      NewMonth := Trunc(V.ToNumberLiteral.Value);
  end;

  NewDay := GetDayFieldOr(MD.FDay);
  Result := TGocciaTemporalPlainMonthDayValue.Create(NewMonth, NewDay, MD.FReferenceYear);
end;

// TC39 Temporal §11.3.7 Temporal.PlainMonthDay.prototype.equals(other)
function TGocciaTemporalPlainMonthDayValue.MonthDayEquals(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  MD, Other: TGocciaTemporalPlainMonthDayValue;
begin
  MD := AsPlainMonthDay(AThisValue, 'PlainMonthDay.prototype.equals');
  Other := CoercePlainMonthDay(AArgs.GetElement(0), 'PlainMonthDay.prototype.equals');

  if (MD.FMonth = Other.FMonth) and (MD.FDay = Other.FDay) then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;

// TC39 Temporal §11.3.8 Temporal.PlainMonthDay.prototype.toString([options])
function TGocciaTemporalPlainMonthDayValue.MonthDayToString(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  MD: TGocciaTemporalPlainMonthDayValue;
begin
  MD := AsPlainMonthDay(AThisValue, 'PlainMonthDay.prototype.toString');
  Result := TGocciaStringLiteralValue.Create(PadTwo(MD.FMonth) + '-' + PadTwo(MD.FDay));
end;

// TC39 Temporal §11.3.9 Temporal.PlainMonthDay.prototype.toJSON()
function TGocciaTemporalPlainMonthDayValue.MonthDayToJSON(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  MD: TGocciaTemporalPlainMonthDayValue;
begin
  MD := AsPlainMonthDay(AThisValue, 'PlainMonthDay.prototype.toJSON');
  Result := TGocciaStringLiteralValue.Create(PadTwo(MD.FMonth) + '-' + PadTwo(MD.FDay));
end;

// TC39 Temporal §11.3.10 Temporal.PlainMonthDay.prototype.valueOf()
function TGocciaTemporalPlainMonthDayValue.MonthDayValueOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  ThrowTypeError('Temporal.PlainMonthDay.prototype.valueOf cannot be used; use toString or compare instead');
  Result := nil;
end;

// TC39 Temporal §11.3.11 Temporal.PlainMonthDay.prototype.toPlainDate(item)
function TGocciaTemporalPlainMonthDayValue.MonthDayToPlainDate(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  MD: TGocciaTemporalPlainMonthDayValue;
  Arg, V: TGocciaValue;
  Obj: TGocciaObjectValue;
  YearValue: Integer;
begin
  MD := AsPlainMonthDay(AThisValue, 'PlainMonthDay.prototype.toPlainDate');
  Arg := AArgs.GetElement(0);

  if not (Arg is TGocciaObjectValue) then
    ThrowTypeError('PlainMonthDay.prototype.toPlainDate requires an object with a year property');
  Obj := TGocciaObjectValue(Arg);
  V := Obj.GetProperty(PROP_YEAR);
  if (V = nil) or (V is TGocciaUndefinedLiteralValue) then
    ThrowTypeError('PlainMonthDay.prototype.toPlainDate requires an object with a year property');

  YearValue := Trunc(V.ToNumberLiteral.Value);
  Result := TGocciaTemporalPlainDateValue.Create(YearValue, MD.FMonth, MD.FDay);
end;

end.
