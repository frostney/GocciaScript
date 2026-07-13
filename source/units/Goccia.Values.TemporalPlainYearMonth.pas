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
    FCalendarId: string;
  published
    function GetCalendarId(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetEra(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetEraYear(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
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
    constructor Create(const AYear, AMonth: Integer; const AReferenceDay: Integer = 1;
      const ACalendarId: string = 'iso8601'); overload;

    function ToStringTag: string; override;
    class procedure ExposePrototype(const AConstructor: TGocciaObjectValue);

    property Year: Integer read FYear;
    property Month: Integer read FMonth;
    property ReferenceDay: Integer read FReferenceDay;
    property CalendarId: string read FCalendarId;
  end;

implementation

uses
  SysUtils,

  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.Realm,
  Goccia.Temporal.Calendar,
  Goccia.Temporal.Options,
  Goccia.Temporal.Utils,
  Goccia.Utils,
  Goccia.Values.ErrorHelper,
  Goccia.Values.IntlDateTimeFormat,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.SymbolValue,
  Goccia.Values.TemporalDuration,
  Goccia.Values.TemporalPlainDate,
  Goccia.Values.TemporalPlainDateTime,
  Goccia.Values.TemporalPlainMonthDay,
  Goccia.Values.TemporalZonedDateTime,
  Goccia.Values.ToPrimitive;

var
  GTemporalPlainYearMonthSharedSlot: TGocciaRealmOwnedSlotId;

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

function CalendarInfoForPlainYearMonth(const AYearMonth: TGocciaTemporalPlainYearMonthValue;
  const AMethod: string): TTemporalCalendarDateInfo;
begin
  if not TryGetCalendarDateInfo(AYearMonth.FCalendarId, AYearMonth.FYear, AYearMonth.FMonth,
    AYearMonth.FReferenceDay, Result) then
    ThrowRangeError(Format(SErrorInvalidYearMonthStringFor, [AMethod]), SSuggestTemporalDateRange);
end;

function CalendarIdFromTemporalCalendarLike(const AValue: TGocciaValue;
  const AMethod: string): string;
var
  CalendarString: string;
begin
  if (AValue = nil) or (AValue is TGocciaUndefinedLiteralValue) then
    Exit('iso8601');
  if AValue is TGocciaTemporalPlainDateValue then
    Exit(TGocciaTemporalPlainDateValue(AValue).CalendarId);
  if AValue is TGocciaTemporalPlainDateTimeValue then
    Exit(TGocciaTemporalPlainDateTimeValue(AValue).CalendarId);
  if AValue is TGocciaTemporalPlainMonthDayValue then
    Exit(TGocciaTemporalPlainMonthDayValue(AValue).CalendarId);
  if AValue is TGocciaTemporalPlainYearMonthValue then
    Exit(TGocciaTemporalPlainYearMonthValue(AValue).CalendarId);
  if AValue is TGocciaTemporalZonedDateTimeValue then
    Exit(TGocciaTemporalZonedDateTimeValue(AValue).CalendarId);
  if not (AValue is TGocciaStringLiteralValue) then
    ThrowTypeError(AMethod + ' calendar must be a string or Temporal object',
      SSuggestTemporalFromArg);

  CalendarString := TGocciaStringLiteralValue(AValue).Value;
  Result := ParseTemporalCalendarStringIdentifierStrict(CalendarString);
  if Result = '' then
    ThrowRangeError('Unknown calendar: ' + CalendarString, SSuggestTemporalFromArg);
end;

function YearMonthMonthCodeStringFromValue(const AValue: TGocciaValue;
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

function CoercePlainYearMonth(const AValue: TGocciaValue; const AMethod: string): TGocciaTemporalPlainYearMonthValue;
var
  Obj: TGocciaObjectValue;
  V, VMonth, VMonthCode, VEra, VEraYear: TGocciaValue;
  DateRec: TTemporalDateRecord;
  Year, Month, ReferenceDay, CandidateMonth: Integer;
  CalendarId, MonthCodeStr: string;
  HasMonth, HasMonthCode, IsLeapMonth: Boolean;
begin
  CalendarId := 'iso8601';
  ReferenceDay := 1;
  if AValue is TGocciaTemporalPlainYearMonthValue then
    Result := TGocciaTemporalPlainYearMonthValue(AValue)
  else if AValue is TGocciaStringLiteralValue then
  begin
    if not TryParseTemporalPlainYearMonthString(TGocciaStringLiteralValue(AValue).Value,
      Year, Month, ReferenceDay, CalendarId) then
      ThrowRangeError(Format(SErrorInvalidYearMonthStringFor, [AMethod]), SSuggestTemporalISOFormat);
    Result := TGocciaTemporalPlainYearMonthValue.Create(Year, Month, ReferenceDay, CalendarId);
  end
  else if AValue is TGocciaObjectValue then
  begin
    Obj := TGocciaObjectValue(AValue);
    V := Obj.GetProperty('calendar');
    CalendarId := CalendarIdFromTemporalCalendarLike(V, AMethod);
    IsLeapMonth := False;

    VMonth := Obj.GetProperty('month');
    HasMonth := Assigned(VMonth) and not (VMonth is TGocciaUndefinedLiteralValue);
    CandidateMonth := 0;
    if HasMonth then
    begin
      CandidateMonth := ToIntegerWithTruncationValue(VMonth);
      Month := CandidateMonth;
    end;

    VMonthCode := Obj.GetProperty('monthCode');
    HasMonthCode := Assigned(VMonthCode) and not (VMonthCode is TGocciaUndefinedLiteralValue);
    if HasMonthCode then
    begin
      MonthCodeStr := YearMonthMonthCodeStringFromValue(VMonthCode, AMethod);
      if not TryParseTemporalMonthCode(MonthCodeStr, Month, IsLeapMonth) then
        ThrowRangeError(SErrorInvalidMonthCodeYearMonth, SSuggestTemporalMonthCode);
      if HasMonth and (CandidateMonth <> Month) then
        ThrowRangeError(SErrorMonthCodeMismatch, SSuggestTemporalMonthCode);
    end
    else if not HasMonth then
    begin
      ThrowTypeError(AMethod + ' requires year and month properties',
        SSuggestTemporalFromArg);
    end;

    V := Obj.GetProperty('year');
    if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
      Year := ToIntegerWithTruncationValue(V)
    else
    begin
      VEra := Obj.GetProperty('era');
      VEraYear := Obj.GetProperty('eraYear');
      if (VEra = nil) or (VEra is TGocciaUndefinedLiteralValue) or
         (VEraYear = nil) or (VEraYear is TGocciaUndefinedLiteralValue) or
         not TryCalendarYearFromEra(CalendarId, VEra.ToStringLiteral.Value,
           ToIntegerWithTruncationValue(VEraYear), Year) then
        ThrowTypeError(AMethod + ' requires year and month properties', SSuggestTemporalFromArg);
    end;
    if CalendarId <> 'iso8601' then
    begin
      if Assigned(VMonthCode) and not (VMonthCode is TGocciaUndefinedLiteralValue) then
      begin
        if not TryCalendarMonthCodeDateToISO(CalendarId, Year, Month, 1,
          IsLeapMonth, DateRec) then
          ThrowRangeError(Format(SErrorInvalidYearMonthStringFor, [AMethod]), SSuggestTemporalDateRange);
      end
      else if not TryCalendarDateToISO(CalendarId, Year, Month, 1, DateRec) then
        ThrowRangeError(Format(SErrorInvalidYearMonthStringFor, [AMethod]), SSuggestTemporalDateRange);
      Year := DateRec.Year;
      Month := DateRec.Month;
      ReferenceDay := DateRec.Day;
    end;
    Result := TGocciaTemporalPlainYearMonthValue.Create(Year, Month, ReferenceDay, CalendarId);
  end
  else
  begin
    ThrowTypeError(AMethod + ' requires a PlainYearMonth, string, or object', SSuggestTemporalFromArg);
    Result := nil;
  end;
end;

{ TGocciaTemporalPlainYearMonthValue }

constructor TGocciaTemporalPlainYearMonthValue.Create(const AYear, AMonth: Integer; const AReferenceDay: Integer = 1;
  const ACalendarId: string = 'iso8601');
var
  MaxDay: Integer;
  Shared: TGocciaSharedPrototype;
begin
  inherited Create(nil);
  if (AMonth < 1) or (AMonth > 12) then
    ThrowRangeError(Format(SErrorInvalidMonth, [IntToStr(AMonth)]), SSuggestTemporalDateRange);
  if not IsTemporalISOYearMonthInSupportedRange(AYear, AMonth) then
    ThrowRangeError(Format(SErrorInvalidYearMonthStringFor, ['PlainYearMonth']),
      SSuggestTemporalDateRange);
  FYear := AYear;
  FMonth := AMonth;
  MaxDay := Goccia.Temporal.Utils.DaysInMonth(AYear, AMonth);
  if AReferenceDay < 1 then
    FReferenceDay := 1
  else if AReferenceDay > MaxDay then
    FReferenceDay := MaxDay
  else
    FReferenceDay := AReferenceDay;
  FCalendarId := CanonicalizeTemporalCalendarIdentifier(ACalendarId);
  if FCalendarId = '' then
    ThrowRangeError('Unknown calendar: ' + ACalendarId, SSuggestTemporalDateRange);
  InitializePrototype;
  Shared := GetTemporalPlainYearMonthShared;
  if Assigned(Shared) then
    FPrototype := Shared.Prototype;
end;

procedure TGocciaTemporalPlainYearMonthValue.InitializePrototype;
var
  Members: TGocciaMemberCollection;
  Shared: TGocciaSharedPrototype;
  PrototypeMembers: TArray<TGocciaMemberDefinition>;
begin
  if not Assigned(CurrentRealm) then Exit;
  if Assigned(GetTemporalPlainYearMonthShared) then Exit;
  Shared := TGocciaSharedPrototype.Create(Self);
  CurrentRealm.SetOwnedSlot(GTemporalPlainYearMonthSharedSlot, Shared);
  Members := TGocciaMemberCollection.Create;
  try
    Members.AddAccessor('calendarId', GetCalendarId, nil, [pfConfigurable]);
    Members.AddAccessor('era', GetEra, nil, [pfConfigurable]);
    Members.AddAccessor('eraYear', GetEraYear, nil, [pfConfigurable]);
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
    PrototypeMembers := Members.ToDefinitions;
  finally
    Members.Free;
  end;
  RegisterMemberDefinitions(Shared.Prototype, PrototypeMembers);
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
var
  YM: TGocciaTemporalPlainYearMonthValue;
begin
  YM := AsPlainYearMonth(AThisValue, 'get PlainYearMonth.calendarId');
  Result := TGocciaStringLiteralValue.Create(YM.FCalendarId);
end;

function TGocciaTemporalPlainYearMonthValue.GetEra(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  YM: TGocciaTemporalPlainYearMonthValue;
  Info: TTemporalCalendarDateInfo;
begin
  YM := AsPlainYearMonth(AThisValue, 'get PlainYearMonth.era');
  Info := CalendarInfoForPlainYearMonth(YM, 'get PlainYearMonth.era');
  if not Info.HasEra then
    Exit(TGocciaUndefinedLiteralValue.UndefinedValue);
  Result := TGocciaStringLiteralValue.Create(Info.Era);
end;

function TGocciaTemporalPlainYearMonthValue.GetEraYear(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  YM: TGocciaTemporalPlainYearMonthValue;
  Info: TTemporalCalendarDateInfo;
begin
  YM := AsPlainYearMonth(AThisValue, 'get PlainYearMonth.eraYear');
  Info := CalendarInfoForPlainYearMonth(YM, 'get PlainYearMonth.eraYear');
  if not Info.HasEra then
    Exit(TGocciaUndefinedLiteralValue.UndefinedValue);
  Result := TGocciaNumberLiteralValue.Create(Info.EraYear);
end;

// TC39 Temporal §10.3.4 get Temporal.PlainYearMonth.prototype.year
function TGocciaTemporalPlainYearMonthValue.GetYear(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  YM: TGocciaTemporalPlainYearMonthValue;
begin
  YM := AsPlainYearMonth(AThisValue, 'get PlainYearMonth.year');
  Result := TGocciaNumberLiteralValue.Create(CalendarInfoForPlainYearMonth(YM, 'get PlainYearMonth.year').Date.Year);
end;

// TC39 Temporal §10.3.5 get Temporal.PlainYearMonth.prototype.month
function TGocciaTemporalPlainYearMonthValue.GetMonth(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  YM: TGocciaTemporalPlainYearMonthValue;
begin
  YM := AsPlainYearMonth(AThisValue, 'get PlainYearMonth.month');
  Result := TGocciaNumberLiteralValue.Create(CalendarInfoForPlainYearMonth(YM, 'get PlainYearMonth.month').Date.Month);
end;

// TC39 Temporal §10.3.6 get Temporal.PlainYearMonth.prototype.monthCode
function TGocciaTemporalPlainYearMonthValue.GetMonthCode(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  YM: TGocciaTemporalPlainYearMonthValue;
begin
  YM := AsPlainYearMonth(AThisValue, 'get PlainYearMonth.monthCode');
  Result := TGocciaStringLiteralValue.Create(CalendarInfoForPlainYearMonth(YM, 'get PlainYearMonth.monthCode').MonthCode);
end;

// TC39 Temporal §10.3.7 get Temporal.PlainYearMonth.prototype.daysInMonth
function TGocciaTemporalPlainYearMonthValue.GetDaysInMonth(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  YM: TGocciaTemporalPlainYearMonthValue;
begin
  YM := AsPlainYearMonth(AThisValue, 'get PlainYearMonth.daysInMonth');
  Result := TGocciaNumberLiteralValue.Create(CalendarInfoForPlainYearMonth(YM, 'get PlainYearMonth.daysInMonth').DaysInMonth);
end;

// TC39 Temporal §10.3.8 get Temporal.PlainYearMonth.prototype.daysInYear
function TGocciaTemporalPlainYearMonthValue.GetDaysInYear(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  YM: TGocciaTemporalPlainYearMonthValue;
begin
  YM := AsPlainYearMonth(AThisValue, 'get PlainYearMonth.daysInYear');
  Result := TGocciaNumberLiteralValue.Create(CalendarInfoForPlainYearMonth(YM, 'get PlainYearMonth.daysInYear').DaysInYear);
end;

// TC39 Temporal §10.3.9 get Temporal.PlainYearMonth.prototype.monthsInYear
function TGocciaTemporalPlainYearMonthValue.GetMonthsInYear(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  YM: TGocciaTemporalPlainYearMonthValue;
begin
  YM := AsPlainYearMonth(AThisValue, 'get PlainYearMonth.monthsInYear');
  Result := TGocciaNumberLiteralValue.Create(CalendarInfoForPlainYearMonth(YM, 'get PlainYearMonth.monthsInYear').MonthsInYear);
end;

// TC39 Temporal §10.3.10 get Temporal.PlainYearMonth.prototype.inLeapYear
function TGocciaTemporalPlainYearMonthValue.GetInLeapYear(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  YM: TGocciaTemporalPlainYearMonthValue;
begin
  YM := AsPlainYearMonth(AThisValue, 'get PlainYearMonth.inLeapYear');
  if CalendarInfoForPlainYearMonth(YM, 'get PlainYearMonth.inLeapYear').InLeapYear then
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
  V, VCalendar, VTimeZone, VYear, VMonth, VMonthCode, VEra,
  VEraYear: TGocciaValue;
  DateRec: TTemporalDateRecord;
  Info: TTemporalCalendarDateInfo;
  NewYear, NewMonth, NewDay, CandidateMonth: Integer;
  MonthCodeStr: string;
  HasYear, HasMonth, HasMonthCode, HasEra, HasEraYear, IsLeapMonth,
    UseMonthCode: Boolean;
  Overflow: TTemporalOverflow;

  function IsPresent(const AValue: TGocciaValue): Boolean;
  begin
    Result := Assigned(AValue) and not (AValue is TGocciaUndefinedLiteralValue);
  end;

begin
  YM := AsPlainYearMonth(AThisValue, 'PlainYearMonth.prototype.with');
  V := AArgs.GetElement(0);
  if (not (V is TGocciaObjectValue)) or
     (V is TGocciaTemporalPlainDateValue) or
     (V is TGocciaTemporalPlainDateTimeValue) or
     (V is TGocciaTemporalPlainMonthDayValue) or
     (V is TGocciaTemporalPlainYearMonthValue) or
     (V is TGocciaTemporalZonedDateTimeValue) then
    ThrowTypeError(Format(SErrorTemporalWithRequiresObject, ['PlainYearMonth']), SSuggestTemporalWithObject);
  Obj := TGocciaObjectValue(V);

  Info := CalendarInfoForPlainYearMonth(YM, 'PlainYearMonth.prototype.with');
  VCalendar := Obj.GetProperty('calendar');
  if IsPresent(VCalendar) then
    ThrowTypeError('PlainYearMonth.prototype.with does not accept calendar',
      SSuggestTemporalFromArg);
  VTimeZone := Obj.GetProperty('timeZone');
  if IsPresent(VTimeZone) then
    ThrowTypeError('PlainYearMonth.prototype.with does not accept timeZone',
      SSuggestTemporalFromArg);

  VMonth := Obj.GetProperty('month');
  HasMonth := IsPresent(VMonth);
  CandidateMonth := 0;
  if HasMonth then
  begin
    CandidateMonth := ToIntegerWithTruncationValue(VMonth);
    NewMonth := CandidateMonth;
  end
  else
    NewMonth := Info.Date.Month;

  VMonthCode := Obj.GetProperty('monthCode');
  HasMonthCode := IsPresent(VMonthCode);
  IsLeapMonth := False;
  UseMonthCode := False;
  if HasMonthCode then
  begin
    MonthCodeStr := YearMonthMonthCodeStringFromValue(VMonthCode,
      'PlainYearMonth.prototype.with');
    if not TryParseTemporalMonthCode(MonthCodeStr, NewMonth, IsLeapMonth) then
      ThrowRangeError(SErrorInvalidMonthCodeYearMonth, SSuggestTemporalMonthCode);
    UseMonthCode := not HasMonth;
  end;

  VYear := Obj.GetProperty('year');
  HasYear := IsPresent(VYear);
  HasEra := False;
  HasEraYear := False;
  if HasYear then
    NewYear := ToIntegerWithTruncationValue(VYear)
  else if YM.FCalendarId <> 'iso8601' then
  begin
    VEra := Obj.GetProperty('era');
    VEraYear := Obj.GetProperty('eraYear');
    HasEra := IsPresent(VEra);
    HasEraYear := IsPresent(VEraYear);
    if HasEra or HasEraYear then
    begin
      if (not HasEra) or (not HasEraYear) or
         not TryCalendarYearFromEra(YM.FCalendarId, VEra.ToStringLiteral.Value,
           ToIntegerWithTruncationValue(VEraYear), NewYear) then
        ThrowTypeError('PlainYearMonth.prototype.with requires era and eraYear together',
          SSuggestTemporalFromArg);
    end
    else
      NewYear := Info.Date.Year;
  end
  else
    NewYear := Info.Date.Year;

  if (not HasYear) and (not HasMonth) and (not HasMonthCode) and
     (not HasEra) and (not HasEraYear) then
    ThrowTypeError(Format(SErrorTemporalWithRequiresObject, ['PlainYearMonth']),
      SSuggestTemporalWithObject);

  if (YM.FCalendarId = 'iso8601') and HasMonth and (CandidateMonth < 1) then
    ThrowRangeError(Format(SErrorInvalidYearMonthStringFor,
      ['PlainYearMonth.prototype.with']), SSuggestTemporalDateRange);

  Overflow := GetOverflowOptionFromValue(AArgs.GetElement(1),
    'PlainYearMonth.prototype.with');

  if HasMonthCode then
  begin
    if HasMonth and (CandidateMonth <> NewMonth) then
      ThrowRangeError(SErrorMonthCodeMismatch, SSuggestTemporalMonthCode);
  end
  else if not HasMonth then
  begin
    if YM.FCalendarId <> 'iso8601' then
    begin
      MonthCodeStr := Info.MonthCode;
      if not TryParseTemporalMonthCode(MonthCodeStr, NewMonth, IsLeapMonth) then
        ThrowRangeError(Format(SErrorInvalidYearMonthStringFor,
          ['PlainYearMonth.prototype.with']), SSuggestTemporalDateRange);
      UseMonthCode := True;
    end
    else
      NewMonth := Info.Date.Month;
  end;

  if (YM.FCalendarId = 'iso8601') and HasMonthCode and
     (IsLeapMonth or (NewMonth < 1) or (NewMonth > 12)) then
    ThrowRangeError(SErrorInvalidMonthCodeYearMonth, SSuggestTemporalMonthCode);

  NewDay := Info.Date.Day;
  if not TryResolveCalendarDateToISO(YM.FCalendarId, NewYear, NewMonth, NewDay,
    UseMonthCode, IsLeapMonth, Overflow = toConstrain, DateRec) then
    ThrowRangeError(Format(SErrorInvalidYearMonthStringFor,
      ['PlainYearMonth.prototype.with']), SSuggestTemporalDateRange);
  if not IsTemporalISOYearMonthInSupportedRange(DateRec.Year, DateRec.Month) then
    ThrowRangeError(Format(SErrorInvalidYearMonthStringFor,
      ['PlainYearMonth.prototype.with']), SSuggestTemporalDateRange);

  Result := TGocciaTemporalPlainYearMonthValue.Create(DateRec.Year, DateRec.Month,
    DateRec.Day, YM.FCalendarId);
end;

// TC39 Temporal §10.3.12 Temporal.PlainYearMonth.prototype.add(temporalDurationLike)
function TGocciaTemporalPlainYearMonthValue.YearMonthAdd(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  YM: TGocciaTemporalPlainYearMonthValue;
  Dur: TGocciaTemporalDurationValue;
  Arg: TGocciaValue;
  ObjArg: TGocciaObjectValue;
  DurRec: TTemporalDurationRecord;
  DateRec: TTemporalDateRecord;
  Overflow: TTemporalOverflow;

  function GetDurFieldOr(const AObj: TGocciaObjectValue; const AName: string; const ADefault: Int64): Int64;
  var
    Val: TGocciaValue;
  begin
    Val := AObj.GetProperty(AName);
    if (Val = nil) or (Val is TGocciaUndefinedLiteralValue) then
      Result := ADefault
    else
      Result := ToIntegerWithTruncation64Value(Val);
  end;

begin
  try
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
    Dur := DurationFromObject(ObjArg);
  end
  else
  begin
    ThrowTypeError(Format(SErrorTemporalAddRequiresDuration, ['PlainYearMonth']), SSuggestTemporalDurationArg);
    Dur := nil;
  end;

  Overflow := GetOverflowOptionFromValue(AArgs.GetElement(1),
    'PlainYearMonth.prototype.add');
  if (Dur.Weeks <> 0) or (Dur.Days <> 0) or (Dur.Hours <> 0) or
     (Dur.Minutes <> 0) or (Dur.Seconds <> 0) or
     (Dur.Milliseconds <> 0) or (Dur.Microseconds <> 0) or
     (Dur.Nanoseconds <> 0) then
    ThrowRangeError('PlainYearMonth.prototype.add does not accept units lower than months',
      SSuggestTemporalDurationArg);
  if not TryAddCalendarDate(YM.FCalendarId, YM.FYear, YM.FMonth,
    YM.FReferenceDay, Dur.Years, Dur.Months, 0, 0, Overflow = toConstrain,
    DateRec) then
    ThrowRangeError(Format(SErrorInvalidYearMonthStringFor,
      ['PlainYearMonth.prototype.add']), SSuggestTemporalDateRange);
  if (YM.FCalendarId = 'iso8601') and
     ((not IsTemporalISODateInSupportedRange(YM.FYear, YM.FMonth, 1)) or
      (not IsTemporalISOYearMonthInSupportedRange(DateRec.Year,
        DateRec.Month))) then
    ThrowRangeError(Format(SErrorInvalidYearMonthStringFor,
      ['PlainYearMonth.prototype.add']), SSuggestTemporalDateRange);

  Result := TGocciaTemporalPlainYearMonthValue.Create(DateRec.Year, DateRec.Month,
    DateRec.Day, YM.FCalendarId);
  except
    on E: ETemporalDurationInt64Overflow do
      ThrowRangeError(E.Message, SSuggestTemporalDurationRange);
  end;
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
      Result := ToIntegerWithTruncation64Value(Val);
  end;

begin
  try
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
    Dur := DurationFromObject(ObjArg);
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

  NewArgs := TGocciaArgumentsCollection.Create([NegatedDur, AArgs.GetElement(1)]);
  try
    Result := YearMonthAdd(NewArgs, AThisValue);
  finally
    NewArgs.Free;
  end;
  except
    on E: ETemporalDurationInt64Overflow do
      ThrowRangeError(E.Message, SSuggestTemporalDurationRange);
  end;
end;

function PlainYearMonthMonthIndex(const AYear, AMonth: Integer): Int64;
begin
  Result := Int64(AYear) * 12 + (AMonth - 1);
end;

function RoundSignedToIncrement(const AValue, AIncrement: Int64;
  const AMode: TTemporalRoundingMode): Int64;
var
  Sign, AbsValue, Quotient, Remainder, TwiceRemainder: Int64;
  RoundAway: Boolean;
begin
  if (AValue = 0) or (AIncrement <= 1) then
    Exit(AValue);

  Sign := 1;
  AbsValue := AValue;
  if AbsValue < 0 then
  begin
    Sign := -1;
    AbsValue := -AbsValue;
  end;

  Quotient := AbsValue div AIncrement;
  Remainder := AbsValue mod AIncrement;
  RoundAway := False;
  if Remainder <> 0 then
  begin
    case AMode of
      rmCeil:
        RoundAway := Sign > 0;
      rmFloor:
        RoundAway := Sign < 0;
      rmExpand:
        RoundAway := True;
      rmHalfExpand, rmHalfCeil, rmHalfFloor, rmHalfTrunc, rmHalfEven:
        begin
          TwiceRemainder := Remainder * 2;
          if TwiceRemainder > AIncrement then
            RoundAway := True
          else if TwiceRemainder = AIncrement then
          begin
            case AMode of
              rmHalfExpand:
                RoundAway := True;
              rmHalfCeil:
                RoundAway := Sign > 0;
              rmHalfFloor:
                RoundAway := Sign < 0;
              rmHalfEven:
                RoundAway := Odd(Quotient);
            else
              RoundAway := False;
            end;
          end;
        end;
    else
      RoundAway := False;
    end;
  end;

  if RoundAway then
    Inc(Quotient);
  Result := Sign * Quotient * AIncrement;
end;

procedure ReadPlainYearMonthDifferenceSettings(const AArgs: TGocciaArgumentsCollection;
  const AMethod: string; out ALargestUnit, ASmallestUnit: TTemporalUnit;
  out ARoundingMode: TTemporalRoundingMode; out ARoundingIncrement: Integer);
var
  OptionsObj: TGocciaObjectValue;
  LargestUnitString, SmallestUnitString: string;
begin
  OptionsObj := GetDiffOptions(AArgs, 1);
  LargestUnitString := GetOptionString(OptionsObj, 'largestUnit', '');
  ARoundingIncrement := GetRoundingIncrement(OptionsObj, 1);
  ARoundingMode := GetRoundingMode(OptionsObj, rmTrunc);
  SmallestUnitString := GetOptionString(OptionsObj, 'smallestUnit', '');

  if LargestUnitString = '' then
    ALargestUnit := tuYear
  else if not GetTemporalUnitFromString(LargestUnitString, ALargestUnit) then
    ThrowRangeError(Format(SErrorInvalidLargestUnit, [LargestUnitString]),
      SSuggestTemporalValidUnits);
  if ALargestUnit = tuAuto then
    ALargestUnit := tuYear;

  if SmallestUnitString = '' then
    ASmallestUnit := tuMonth
  else if not GetTemporalUnitFromString(SmallestUnitString, ASmallestUnit) then
    ThrowRangeError(Format(SErrorInvalidSmallestUnit, [SmallestUnitString]),
      SSuggestTemporalValidUnits);

  if not (ALargestUnit in [tuYear, tuMonth]) then
    ThrowRangeError(Format(SErrorTemporalInvalidUnitFor, [AMethod,
      'largestUnit']), SSuggestTemporalValidUnits);
  if not (ASmallestUnit in [tuYear, tuMonth]) then
    ThrowRangeError(Format(SErrorTemporalInvalidUnitFor, [AMethod,
      'smallestUnit']), SSuggestTemporalValidUnits);
  if Ord(ALargestUnit) > Ord(ASmallestUnit) then
    ThrowRangeError(SErrorDurationRoundLargestSmallerThanSmallest,
      SSuggestTemporalRoundArg);
  ValidateRoundingIncrement(ARoundingIncrement, ASmallestUnit);
end;

procedure CheckPlainYearMonthRoundingEndpoint(const AStart: TGocciaTemporalPlainYearMonthValue;
  const ATotalMonths: Int64; const ASmallestUnit: TTemporalUnit;
  const ARoundingIncrement: Integer);
var
  IncrementMonths, StartIndex, MinIndex, MaxIndex: Int64;
begin
  if (ATotalMonths = 0) or (ARoundingIncrement <= 1) then
    Exit;
  if ASmallestUnit = tuYear then
    IncrementMonths := Int64(ARoundingIncrement) * 12
  else
    IncrementMonths := ARoundingIncrement;

  StartIndex := PlainYearMonthMonthIndex(AStart.FYear, AStart.FMonth);
  MinIndex := PlainYearMonthMonthIndex(-271821, 5);
  MaxIndex := PlainYearMonthMonthIndex(275760, 9);
  if ((ATotalMonths > 0) and (StartIndex + IncrementMonths > MaxIndex)) or
     ((ATotalMonths < 0) and (StartIndex - IncrementMonths < MinIndex)) then
    ThrowRangeError(Format(SErrorInvalidYearMonthStringFor,
      ['PlainYearMonth.prototype.until/since']), SSuggestTemporalDateRange);
end;

function FixedCalendarMonthsPerYear(const ACalendarId: string): Integer;
begin
  if (ACalendarId = 'coptic') or (ACalendarId = 'ethioaa') or
     (ACalendarId = 'ethiopic') then
    Exit(13);
  if (ACalendarId <> 'chinese') and (ACalendarId <> 'dangi') and
     (ACalendarId <> 'hebrew') then
    Exit(12);
  Result := 0;
end;

function CalendarMonthsBeforeYear(const ACalendarId: string;
  const AYear: Integer; const AMethod: string): Int64;
var
  FixedMonths, MonthsInYear, YearCursor: Integer;
begin
  FixedMonths := FixedCalendarMonthsPerYear(ACalendarId);
  if FixedMonths > 0 then
    Exit(Int64(AYear) * FixedMonths);

  Result := 0;
  if AYear >= 0 then
  begin
    for YearCursor := 0 to AYear - 1 do
    begin
      if not TryCalendarMonthsInYear(ACalendarId, YearCursor, MonthsInYear) then
        ThrowRangeError(Format(SErrorInvalidYearMonthStringFor, [AMethod]),
          SSuggestTemporalDateRange);
      Inc(Result, MonthsInYear);
    end;
  end
  else
  begin
    for YearCursor := -1 downto AYear do
    begin
      if not TryCalendarMonthsInYear(ACalendarId, YearCursor, MonthsInYear) then
        ThrowRangeError(Format(SErrorInvalidYearMonthStringFor, [AMethod]),
          SSuggestTemporalDateRange);
      Dec(Result, MonthsInYear);
    end;
  end;
end;

function CalendarMonthsBetweenYears(const ACalendarId: string;
  const AStartYear, AEndYear: Integer; const AMethod: string): Int64;
var
  FixedMonths, MonthsInYear, YearCursor: Integer;
begin
  FixedMonths := FixedCalendarMonthsPerYear(ACalendarId);
  if FixedMonths > 0 then
    Exit(Int64(AEndYear - AStartYear) * FixedMonths);

  Result := 0;
  if AEndYear >= AStartYear then
  begin
    for YearCursor := AStartYear to AEndYear - 1 do
    begin
      if not TryCalendarMonthsInYear(ACalendarId, YearCursor, MonthsInYear) then
        ThrowRangeError(Format(SErrorInvalidYearMonthStringFor, [AMethod]),
          SSuggestTemporalDateRange);
      Inc(Result, MonthsInYear);
    end;
  end
  else
  begin
    for YearCursor := AEndYear to AStartYear - 1 do
    begin
      if not TryCalendarMonthsInYear(ACalendarId, YearCursor, MonthsInYear) then
        ThrowRangeError(Format(SErrorInvalidYearMonthStringFor, [AMethod]),
          SSuggestTemporalDateRange);
      Dec(Result, MonthsInYear);
    end;
  end;
end;

function PlainYearMonthCalendarMonthIndex(
  const AYearMonth: TGocciaTemporalPlainYearMonthValue;
  const AMethod: string): Int64;
var
  Info: TTemporalCalendarDateInfo;
begin
  if AYearMonth.FCalendarId = 'iso8601' then
    Exit(PlainYearMonthMonthIndex(AYearMonth.FYear, AYearMonth.FMonth));

  Info := CalendarInfoForPlainYearMonth(AYearMonth, AMethod);
  Result := CalendarMonthsBeforeYear(AYearMonth.FCalendarId, Info.Date.Year,
    AMethod) + Info.Date.Month - 1;
end;

function PlainYearMonthCalendarMonthsBetween(
  const AStart, AEnd: TGocciaTemporalPlainYearMonthValue;
  const AMethod: string): Int64;
var
  StartInfo, EndInfo: TTemporalCalendarDateInfo;
begin
  if AStart.FCalendarId = 'iso8601' then
    Exit(PlainYearMonthMonthIndex(AEnd.FYear, AEnd.FMonth) -
      PlainYearMonthMonthIndex(AStart.FYear, AStart.FMonth));

  StartInfo := CalendarInfoForPlainYearMonth(AStart, AMethod);
  EndInfo := CalendarInfoForPlainYearMonth(AEnd, AMethod);
  Result := CalendarMonthsBetweenYears(AStart.FCalendarId, StartInfo.Date.Year,
    EndInfo.Date.Year, AMethod) + EndInfo.Date.Month - StartInfo.Date.Month;
end;

function CalendarMonthCodeSortKey(const AMonthCode: string): Integer;
var
  Month: Integer;
  IsLeapMonth: Boolean;
begin
  if not TryParseTemporalMonthCode(AMonthCode, Month, IsLeapMonth) then
    Exit(0);
  Result := Month * 2;
  if IsLeapMonth then
    Inc(Result);
end;

function CalendarMonthOrdinalForMonthCode(const ACalendarId: string;
  const AYear: Integer; const AMonthCode: string; const AMethod: string): Integer;
var
  Month: Integer;
  IsLeapMonth: Boolean;
  DateRec: TTemporalDateRecord;
  Info: TTemporalCalendarDateInfo;
begin
  if not TryParseTemporalMonthCode(AMonthCode, Month, IsLeapMonth) or
     not TryResolveCalendarDateToISO(ACalendarId, AYear, Month, 1, True,
       IsLeapMonth, True, DateRec) or
     not TryGetCalendarDateInfo(ACalendarId, DateRec.Year, DateRec.Month,
       DateRec.Day, Info) then
    ThrowRangeError(Format(SErrorInvalidYearMonthStringFor, [AMethod]),
      SSuggestTemporalDateRange);
  Result := Info.Date.Month;
end;

procedure BuildPlainYearMonthDifference(const ATotalMonths: Int64;
  const ALargestUnit, ASmallestUnit: TTemporalUnit;
  const ARoundingMode: TTemporalRoundingMode; const ARoundingIncrement: Integer;
  out AYears, AMonths: Int64);
var
  RoundedMonths: Int64;
begin
  if ASmallestUnit = tuYear then
  begin
    RoundedMonths := RoundSignedToIncrement(ATotalMonths,
      Int64(ARoundingIncrement) * 12, ARoundingMode);
    AYears := RoundedMonths div 12;
    AMonths := 0;
    Exit;
  end;

  if ALargestUnit = tuMonth then
  begin
    AYears := 0;
    AMonths := RoundSignedToIncrement(ATotalMonths, ARoundingIncrement,
      ARoundingMode);
    Exit;
  end;

  AYears := ATotalMonths div 12;
  AMonths := ATotalMonths mod 12;
  AMonths := RoundSignedToIncrement(AMonths, ARoundingIncrement,
    ARoundingMode);
  if (AMonths >= 12) or (AMonths <= -12) then
  begin
    AYears := AYears + (AMonths div 12);
    AMonths := AMonths mod 12;
  end;
end;

procedure BuildPlainYearMonthDifferenceBetween(
  const AStart, AEnd: TGocciaTemporalPlainYearMonthValue;
  const ATotalMonths: Int64; const ALargestUnit, ASmallestUnit: TTemporalUnit;
  const ARoundingMode: TTemporalRoundingMode;
  const ARoundingIncrement: Integer; out AYears, AMonths: Int64);
var
  StartInfo, EndInfo: TTemporalCalendarDateInfo;
  YearMonths: Int64;
  TargetYear, TargetMonth: Integer;
begin
  if (AStart.FCalendarId = 'iso8601') or (ALargestUnit = tuMonth) or
     (ASmallestUnit = tuYear) then
  begin
    BuildPlainYearMonthDifference(ATotalMonths, ALargestUnit, ASmallestUnit,
      ARoundingMode, ARoundingIncrement, AYears, AMonths);
    Exit;
  end;

  StartInfo := CalendarInfoForPlainYearMonth(AStart,
    'PlainYearMonth.prototype.until/since');
  EndInfo := CalendarInfoForPlainYearMonth(AEnd,
    'PlainYearMonth.prototype.until/since');

  AYears := EndInfo.Date.Year - StartInfo.Date.Year;
  if (ATotalMonths > 0) and
     (CalendarMonthCodeSortKey(EndInfo.MonthCode) <
      CalendarMonthCodeSortKey(StartInfo.MonthCode)) then
    Dec(AYears)
  else if (ATotalMonths < 0) and
          (CalendarMonthCodeSortKey(EndInfo.MonthCode) >
           CalendarMonthCodeSortKey(StartInfo.MonthCode)) then
    Inc(AYears);

  TargetYear := StartInfo.Date.Year + Integer(AYears);
  TargetMonth := CalendarMonthOrdinalForMonthCode(AStart.FCalendarId,
    TargetYear, StartInfo.MonthCode, 'PlainYearMonth.prototype.until/since');
  YearMonths := CalendarMonthsBetweenYears(AStart.FCalendarId,
    StartInfo.Date.Year, TargetYear,
    'PlainYearMonth.prototype.until/since');
  Inc(YearMonths, TargetMonth - StartInfo.Date.Month);
  AMonths := RoundSignedToIncrement(ATotalMonths - YearMonths,
    ARoundingIncrement, ARoundingMode);
end;

function InvertPlainYearMonthSinceRoundingMode(
  const AMode: TTemporalRoundingMode): TTemporalRoundingMode;
begin
  case AMode of
    rmCeil:      Result := rmFloor;
    rmFloor:     Result := rmCeil;
    rmHalfCeil:  Result := rmHalfFloor;
    rmHalfFloor: Result := rmHalfCeil;
  else
    Result := AMode;
  end;
end;

// TC39 Temporal §10.3.14 Temporal.PlainYearMonth.prototype.until(other [, options])
function TGocciaTemporalPlainYearMonthValue.YearMonthUntil(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  YM, Other: TGocciaTemporalPlainYearMonthValue;
  LargestUnit, SmallestUnit: TTemporalUnit;
  RMode: TTemporalRoundingMode;
  RIncrement: Integer;
  DiffYears, RemMonths: Int64;
  TotalMonths: Int64;
begin
  YM := AsPlainYearMonth(AThisValue, 'PlainYearMonth.prototype.until');
  Other := CoercePlainYearMonth(AArgs.GetElement(0), 'PlainYearMonth.prototype.until');

  ReadPlainYearMonthDifferenceSettings(AArgs,
    'PlainYearMonth.prototype.until', LargestUnit, SmallestUnit, RMode,
    RIncrement);

  if YM.FCalendarId <> Other.FCalendarId then
    ThrowRangeError('Temporal.PlainYearMonth calendars must match',
      SSuggestTemporalDateRange);

  TotalMonths := PlainYearMonthCalendarMonthsBetween(YM, Other,
    'PlainYearMonth.prototype.until');
  if TotalMonths <> 0 then
  begin
    if (not IsTemporalISODateInSupportedRange(YM.FYear, YM.FMonth, 1)) or
       (not IsTemporalISODateInSupportedRange(Other.FYear, Other.FMonth, 1)) then
      ThrowRangeError(Format(SErrorInvalidYearMonthStringFor,
        ['PlainYearMonth.prototype.until']), SSuggestTemporalDateRange);
    CheckPlainYearMonthRoundingEndpoint(YM, TotalMonths, SmallestUnit,
      RIncrement);
  end;
  BuildPlainYearMonthDifferenceBetween(YM, Other, TotalMonths, LargestUnit,
    SmallestUnit, RMode, RIncrement, DiffYears, RemMonths);

  Result := TGocciaTemporalDurationValue.Create(DiffYears, RemMonths, 0, 0, 0, 0, 0, 0, 0, 0);
end;

// TC39 Temporal §10.3.15 Temporal.PlainYearMonth.prototype.since(other [, options])
function TGocciaTemporalPlainYearMonthValue.YearMonthSince(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  YM, Other: TGocciaTemporalPlainYearMonthValue;
  LargestUnit, SmallestUnit: TTemporalUnit;
  RMode: TTemporalRoundingMode;
  RIncrement: Integer;
  DiffYears, RemMonths: Int64;
  TotalMonths: Int64;
begin
  YM := AsPlainYearMonth(AThisValue, 'PlainYearMonth.prototype.since');
  Other := CoercePlainYearMonth(AArgs.GetElement(0), 'PlainYearMonth.prototype.since');

  ReadPlainYearMonthDifferenceSettings(AArgs,
    'PlainYearMonth.prototype.since', LargestUnit, SmallestUnit, RMode,
    RIncrement);
  RMode := InvertPlainYearMonthSinceRoundingMode(RMode);

  if YM.FCalendarId <> Other.FCalendarId then
    ThrowRangeError('Temporal.PlainYearMonth calendars must match',
      SSuggestTemporalDateRange);

  TotalMonths := PlainYearMonthCalendarMonthsBetween(YM, Other,
    'PlainYearMonth.prototype.since');
  if TotalMonths <> 0 then
  begin
    if (not IsTemporalISODateInSupportedRange(YM.FYear, YM.FMonth, 1)) or
       (not IsTemporalISODateInSupportedRange(Other.FYear, Other.FMonth, 1)) then
      ThrowRangeError(Format(SErrorInvalidYearMonthStringFor,
        ['PlainYearMonth.prototype.since']), SSuggestTemporalDateRange);
    CheckPlainYearMonthRoundingEndpoint(YM, -TotalMonths, SmallestUnit,
      RIncrement);
  end;
  BuildPlainYearMonthDifferenceBetween(YM, Other, TotalMonths, LargestUnit,
    SmallestUnit, RMode, RIncrement, DiffYears, RemMonths);

  Result := TGocciaTemporalDurationValue.Create(-DiffYears, -RemMonths, 0, 0, 0, 0, 0, 0, 0, 0);
end;

// TC39 Temporal §10.3.16 Temporal.PlainYearMonth.prototype.equals(other)
function TGocciaTemporalPlainYearMonthValue.YearMonthEquals(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  YM, Other: TGocciaTemporalPlainYearMonthValue;
begin
  YM := AsPlainYearMonth(AThisValue, 'PlainYearMonth.prototype.equals');
  Other := CoercePlainYearMonth(AArgs.GetElement(0), 'PlainYearMonth.prototype.equals');

  if (YM.FYear = Other.FYear) and (YM.FMonth = Other.FMonth) and
     (YM.FReferenceDay = Other.FReferenceDay) and
     (YM.FCalendarId = Other.FCalendarId) then
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
  if (CalDisp = tcdAlways) or (CalDisp = tcdCritical) or
     ((CalDisp = tcdAuto) and (YM.FCalendarId <> 'iso8601')) then
    Result := TGocciaStringLiteralValue.Create(
      FormatDateString(YM.FYear, YM.FMonth, YM.FReferenceDay) + FormatCalendarAnnotation(CalDisp, YM.FCalendarId))
  else if YM.FCalendarId <> 'iso8601' then
    Result := TGocciaStringLiteralValue.Create(
      FormatDateString(YM.FYear, YM.FMonth, YM.FReferenceDay))
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
  Result := TGocciaStringLiteralValue.Create(
    FormatYearMonthString(YM.FYear, YM.FMonth) + FormatCalendarAnnotation(tcdAuto, YM.FCalendarId));
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
  Info: TTemporalCalendarDateInfo;
  DateRec: TTemporalDateRecord;
  Year, Month, Day: Integer;
  MonthCodeStr: string;
  IsLeapMonth, UseMonthCode: Boolean;
begin
  YM := AsPlainYearMonth(AThisValue, 'PlainYearMonth.prototype.toPlainDate');
  Arg := AArgs.GetElement(0);

  if not (Arg is TGocciaObjectValue) then
    ThrowTypeError(SErrorPlainYearMonthToPlainDateRequiresDay, SSuggestTemporalFromArg);
  Obj := TGocciaObjectValue(Arg);

  V := Obj.GetProperty('day');
  if (V = nil) or (V is TGocciaUndefinedLiteralValue) then
    ThrowTypeError(SErrorPlainYearMonthToPlainDateRequiresDay, SSuggestTemporalFromArg);

  Day := ToIntegerWithTruncationValue(V);
  Info := CalendarInfoForPlainYearMonth(YM, 'PlainYearMonth.prototype.toPlainDate');
  Year := Info.Date.Year;
  UseMonthCode := YM.FCalendarId <> 'iso8601';
  IsLeapMonth := False;
  if UseMonthCode then
  begin
    MonthCodeStr := Info.MonthCode;
    if not TryParseTemporalMonthCode(MonthCodeStr, Month, IsLeapMonth) then
      ThrowRangeError(Format(SErrorTemporalInvalidISOStringFor, ['date',
        'PlainYearMonth.prototype.toPlainDate']), SSuggestTemporalDateRange);
  end
  else
    Month := Info.Date.Month;

  if not TryResolveCalendarDateToISO(YM.FCalendarId, Year, Month, Day,
    UseMonthCode, IsLeapMonth, True, DateRec) then
    ThrowRangeError(Format(SErrorTemporalInvalidISOStringFor, ['date',
      'PlainYearMonth.prototype.toPlainDate']), SSuggestTemporalDateRange);
  if not IsTemporalISODateInSupportedRange(DateRec.Year, DateRec.Month,
    DateRec.Day) then
    ThrowRangeError(Format(SErrorTemporalInvalidISOStringFor, ['date',
      'PlainYearMonth.prototype.toPlainDate']), SSuggestTemporalDateRange);

  Result := TGocciaTemporalPlainDateValue.Create(DateRec.Year, DateRec.Month, DateRec.Day,
    YM.FCalendarId);
end;

function TGocciaTemporalPlainYearMonthValue.YearMonthToLocaleString(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  AsPlainYearMonth(AThisValue, 'PlainYearMonth.prototype.toLocaleString');
  Result := FormatTemporalValueToLocaleString(AThisValue, AArgs.GetElement(0),
    AArgs.GetElement(1));
end;

initialization
  GTemporalPlainYearMonthSharedSlot := RegisterRealmOwnedSlot('Temporal.PlainYearMonth.shared');

end.
