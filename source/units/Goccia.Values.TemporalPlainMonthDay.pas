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
    function MonthDayToLocaleString(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;

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
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.Realm,
  Goccia.Temporal.Options,
  Goccia.Temporal.Utils,
  Goccia.Values.ErrorHelper,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.SymbolValue,
  Goccia.Values.TemporalPlainDate;

var
  GTemporalPlainMonthDaySharedSlot: TGocciaRealmOwnedSlotId;

threadvar
  FPrototypeMembers: TArray<TGocciaMemberDefinition>;

function GetTemporalPlainMonthDayShared: TGocciaSharedPrototype; inline;
begin
  if Assigned(CurrentRealm) then
    Result := TGocciaSharedPrototype(CurrentRealm.GetOwnedSlot(GTemporalPlainMonthDaySharedSlot))
  else
    Result := nil;
end;

const
  DEFAULT_REFERENCE_YEAR = 1972;

function AsPlainMonthDay(const AValue: TGocciaValue; const AMethod: string): TGocciaTemporalPlainMonthDayValue;
begin
  if not (AValue is TGocciaTemporalPlainMonthDayValue) then
    ThrowTypeError(AMethod + ' called on non-PlainMonthDay', SSuggestTemporalThisType);
  Result := TGocciaTemporalPlainMonthDayValue(AValue);
end;

function CoercePlainMonthDay(const AValue: TGocciaValue; const AMethod: string): TGocciaTemporalPlainMonthDayValue;
var
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
    if not CoerceToISOMonthDay(TGocciaStringLiteralValue(AValue).Value, MonthPart, DayPart) then
      ThrowRangeError(Format(SErrorInvalidMonthDayStringFor, [AMethod]), SSuggestTemporalISOFormat);
    Result := TGocciaTemporalPlainMonthDayValue.Create(MonthPart, DayPart);
  end
  else if AValue is TGocciaObjectValue then
  begin
    Obj := TGocciaObjectValue(AValue);
    VDay := Obj.GetProperty(PROP_DAY);
    if (VDay = nil) or (VDay is TGocciaUndefinedLiteralValue) then
      ThrowTypeError(AMethod + ' requires day property', SSuggestTemporalFromArg);
    VMonthCode := Obj.GetProperty(PROP_MONTH_CODE);
    if Assigned(VMonthCode) and not (VMonthCode is TGocciaUndefinedLiteralValue) then
    begin
      MonthCodeStr := VMonthCode.ToStringLiteral.Value;
      if (Length(MonthCodeStr) <> 3) or (MonthCodeStr[1] <> 'M') or
         not (MonthCodeStr[2] in ['0'..'9']) or not (MonthCodeStr[3] in ['0'..'9']) then
        ThrowTypeError(Format(SErrorInvalidMonthCodeFor, [AMethod]), SSuggestTemporalMonthCode);
      if not TryStrToInt(Copy(MonthCodeStr, 2, 2), MonthPart) then
        ThrowTypeError(Format(SErrorInvalidMonthCodeFor, [AMethod]), SSuggestTemporalMonthCode);
      if (MonthPart < 1) or (MonthPart > 12) then
        ThrowRangeError(Format(SErrorMonthCodeOutOfRangeIn, [AMethod]), SSuggestTemporalMonthCode);
      // Validate month/monthCode consistency when both are provided
      V := Obj.GetProperty(PROP_MONTH);
      if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
        if Trunc(V.ToNumberLiteral.Value) <> MonthPart then
          ThrowRangeError(Format(SErrorMonthCodeMismatchIn, [AMethod]), SSuggestTemporalMonthCode);
    end
    else
    begin
      V := Obj.GetProperty(PROP_MONTH);
      if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
        MonthPart := Trunc(V.ToNumberLiteral.Value)
      else
        ThrowTypeError(AMethod + ' requires monthCode or month property', SSuggestTemporalFromArg);
    end;
    Result := TGocciaTemporalPlainMonthDayValue.Create(
      MonthPart, Trunc(VDay.ToNumberLiteral.Value));
  end
  else
  begin
    ThrowTypeError(AMethod + ' requires a PlainMonthDay, string, or object', SSuggestTemporalFromArg);
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
    ThrowRangeError(Format(SErrorInvalidMonthDay, [PadTwo(AMonth) + '-' + PadTwo(ADay)]), SSuggestTemporalDateRange);
  FMonth := AMonth;
  FDay := ADay;
  FReferenceYear := AReferenceYear;
  InitializePrototype;
  if Assigned(GetTemporalPlainMonthDayShared) then
    FPrototype := GetTemporalPlainMonthDayShared.Prototype;
end;

procedure TGocciaTemporalPlainMonthDayValue.InitializePrototype;
var
  Members: TGocciaMemberCollection;
  Shared: TGocciaSharedPrototype;
begin
  if not Assigned(CurrentRealm) then Exit;
  if Assigned(GetTemporalPlainMonthDayShared) then Exit;
  Shared := TGocciaSharedPrototype.Create(Self);
  CurrentRealm.SetOwnedSlot(GTemporalPlainMonthDaySharedSlot, Shared);
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
      Members.AddMethod(MonthDayToLocaleString, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddSymbolDataProperty(
        TGocciaSymbolValue.WellKnownToStringTag,
        TGocciaStringLiteralValue.Create('Temporal.PlainMonthDay'),
        [pfConfigurable]);
      FPrototypeMembers := Members.ToDefinitions;
    finally
      Members.Free;
    end;
  end;
  RegisterMemberDefinitions(Shared.Prototype, FPrototypeMembers);
end;

class procedure TGocciaTemporalPlainMonthDayValue.ExposePrototype(const AConstructor: TGocciaObjectValue);
var
  Shared: TGocciaSharedPrototype;
begin
  Shared := GetTemporalPlainMonthDayShared;
  if not Assigned(Shared) then
  begin
    TGocciaTemporalPlainMonthDayValue.Create(1, 1);
    Shared := GetTemporalPlainMonthDayShared;
  end;
  if Assigned(Shared) then
    ExposeSharedPrototypeOnConstructor(Shared, AConstructor);
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
    ThrowTypeError(Format(SErrorTemporalWithRequiresObject, ['PlainMonthDay']), SSuggestTemporalWithObject);
  Obj := TGocciaObjectValue(V);

  NewMonth := MD.FMonth;
  V := Obj.GetProperty(PROP_MONTH_CODE);
  if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
  begin
    MonthCodeStr := V.ToStringLiteral.Value;
    if (Length(MonthCodeStr) <> 3) or (MonthCodeStr[1] <> 'M') or
       not (MonthCodeStr[2] in ['0'..'9']) or not (MonthCodeStr[3] in ['0'..'9']) then
      ThrowTypeError(Format(SErrorInvalidMonthCodeFor, ['PlainMonthDay.prototype.with']), SSuggestTemporalMonthCode);
    if not TryStrToInt(Copy(MonthCodeStr, 2, 2), NewMonth) then
      ThrowTypeError(Format(SErrorInvalidMonthCodeFor, ['PlainMonthDay.prototype.with']), SSuggestTemporalMonthCode);
    if (NewMonth < 1) or (NewMonth > 12) then
      ThrowRangeError(Format(SErrorMonthCodeOutOfRangeIn, ['PlainMonthDay.prototype.with']), SSuggestTemporalMonthCode);
    // Validate month/monthCode consistency when both are provided
    V := Obj.GetProperty(PROP_MONTH);
    if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
      if Trunc(V.ToNumberLiteral.Value) <> NewMonth then
        ThrowRangeError(Format(SErrorMonthCodeMismatchIn, ['PlainMonthDay.prototype.with']), SSuggestTemporalMonthCode);
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
  Arg: TGocciaValue;
  OptionsObj: TGocciaObjectValue;
  CalDisp: TTemporalCalendarDisplay;
begin
  MD := AsPlainMonthDay(AThisValue, 'PlainMonthDay.prototype.toString');
  OptionsObj := nil;
  Arg := AArgs.GetElement(0);
  if Assigned(Arg) and not (Arg is TGocciaUndefinedLiteralValue) then
  begin
    if not (Arg is TGocciaObjectValue) then
      ThrowTypeError('options must be an object or undefined', SSuggestTemporalFromArg);
    OptionsObj := TGocciaObjectValue(Arg);
  end;
  CalDisp := GetCalendarDisplay(OptionsObj);
  // When calendar annotation is present, include reference year for round-tripping
  if (CalDisp = tcdAlways) or (CalDisp = tcdCritical) then
    Result := TGocciaStringLiteralValue.Create(
      FormatDateString(MD.FReferenceYear, MD.FMonth, MD.FDay) + FormatCalendarAnnotation(CalDisp))
  else
    Result := TGocciaStringLiteralValue.Create(
      PadTwo(MD.FMonth) + '-' + PadTwo(MD.FDay));
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
  ThrowTypeError(Format(SErrorTemporalValueOf, ['PlainMonthDay', 'toString or compare']), SSuggestTemporalNoValueOf);
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
    ThrowTypeError(SErrorPlainMonthDayToPlainDateRequiresYear, SSuggestTemporalToPlainDateYear);
  Obj := TGocciaObjectValue(Arg);
  V := Obj.GetProperty(PROP_YEAR);
  if (V = nil) or (V is TGocciaUndefinedLiteralValue) then
    ThrowTypeError(SErrorPlainMonthDayToPlainDateRequiresYear, SSuggestTemporalToPlainDateYear);

  YearValue := Trunc(V.ToNumberLiteral.Value);
  Result := TGocciaTemporalPlainDateValue.Create(YearValue, MD.FMonth, MD.FDay);
end;

function TGocciaTemporalPlainMonthDayValue.MonthDayToLocaleString(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  EmptyArgs: TGocciaArgumentsCollection;
begin
  EmptyArgs := TGocciaArgumentsCollection.Create([]);
  try
    Result := MonthDayToString(EmptyArgs, AThisValue);
  finally
    EmptyArgs.Free;
  end;
end;

initialization
  GTemporalPlainMonthDaySharedSlot := RegisterRealmOwnedSlot('Temporal.PlainMonthDay.shared');

end.
