unit Goccia.Values.TemporalPlainDate;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.ObjectModel,
  Goccia.SharedPrototype,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaTemporalPlainDateValue = class(TGocciaObjectValue)
  private
    FYear: Integer;
    FMonth: Integer;
    FDay: Integer;
    FCalendarId: string;
  published
    function GetCalendarId(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetEra(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetEraYear(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetYear(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetMonth(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetMonthCode(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetDay(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetDayOfWeek(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetDayOfYear(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetWeekOfYear(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetYearOfWeek(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetDaysInWeek(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetDaysInMonth(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetDaysInYear(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetMonthsInYear(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetInLeapYear(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;

    function DateWith(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DateAdd(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DateSubtract(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DateUntil(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DateSince(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DateEquals(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DateToString(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DateToJSON(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DateValueOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DateToPlainDateTime(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DateToPlainYearMonth(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DateToPlainMonthDay(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DateToZonedDateTime(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DateToLocaleString(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DateWithCalendar(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;

    procedure InitializePrototype;
  public
    constructor Create(const AYear, AMonth, ADay: Integer;
      const ACalendarId: string = 'iso8601'); overload;

    function ToStringTag: string; override;
    class procedure ExposePrototype(const AConstructor: TGocciaObjectValue);

    property Year: Integer read FYear;
    property Month: Integer read FMonth;
    property Day: Integer read FDay;
    property CalendarId: string read FCalendarId;
  end;

implementation

uses
  SysUtils,

  BigInteger,

  Goccia.Constants.PropertyNames,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.GarbageCollector,
  Goccia.Realm,
  Goccia.Temporal.Calendar,
  Goccia.Temporal.DurationMath,
  Goccia.Temporal.Options,
  Goccia.Temporal.TimeZone,
  Goccia.Temporal.Utils,
  Goccia.ThreadCleanupRegistry,
  Goccia.Utils,
  Goccia.Values.ErrorHelper,
  Goccia.Values.IntlDateTimeFormat,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.SymbolValue,
  Goccia.Values.TemporalDuration,
  Goccia.Values.TemporalPlainDateTime,
  Goccia.Values.TemporalPlainMonthDay,
  Goccia.Values.TemporalPlainTime,
  Goccia.Values.TemporalPlainYearMonth,
  Goccia.Values.TemporalZonedDateTime;

var
  GTemporalPlainDateSharedSlot: TGocciaRealmOwnedSlotId;

threadvar
  FPrototypeMembers: TArray<TGocciaMemberDefinition>;

procedure ClearThreadvarMembers;
begin
  SetLength(FPrototypeMembers, 0);
end;

function GetTemporalPlainDateShared: TGocciaSharedPrototype;
{$IFDEF FPC}inline;{$ENDIF}
begin
  if (CurrentRealm <> nil) then
    Result := TGocciaSharedPrototype(CurrentRealm.GetOwnedSlot(GTemporalPlainDateSharedSlot))
  else
    Result := nil;
end;

function GetDurFieldOr(const AObj: TGocciaObjectValue; const AName: string; const ADefault: Int64): Int64;
var
  V: TGocciaValue;
begin
  V := AObj.GetProperty(AName);
  if (V = nil) or (V is TGocciaUndefinedLiteralValue) then
    Result := ADefault
  else
    Result := ToIntegerWithTruncation64Value(V);
end;

function DurationTimeWholeDays(const ADuration: TGocciaTemporalDurationValue): Int64;
var
  TimeDuration, H, Mi, S, Ms, Us, Ns: TBigInteger;
begin
  TimeDuration := TimeDurationFromComponents(
    ADuration.HoursBig, ADuration.MinutesBig, ADuration.SecondsBig,
    ADuration.MillisecondsBig, ADuration.MicrosecondsBig,
    ADuration.NanosecondsBig);
  BalanceTimeDurationToFields(TimeDuration, tuDay, H, Mi, S, Ms, Us, Ns, Result);
end;

procedure ValidatePlainDateTimeRange(const AYear, AMonth, ADay, AHour,
  AMinute, ASecond, AMillisecond, AMicrosecond, ANanosecond: Integer;
  const AMethod: string);
begin
  if (AYear < -271821) or (AYear > 275760) or
     ((AYear = -271821) and
      ((AMonth < 4) or ((AMonth = 4) and (ADay < 19)))) or
     ((AYear = 275760) and
      ((AMonth > 9) or ((AMonth = 9) and (ADay > 13)))) or
     ((AYear = -271821) and (AMonth = 4) and (ADay = 19) and
      (AHour = 0) and (AMinute = 0) and (ASecond = 0) and
      (AMillisecond = 0) and (AMicrosecond = 0) and (ANanosecond = 0)) then
    ThrowRangeError(Format(SErrorTemporalInvalidISOStringFor, ['date-time',
      AMethod]), SSuggestTemporalDateRange);
end;

function NegateTemporalRoundingMode(const AMode: TTemporalRoundingMode): TTemporalRoundingMode;
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

function AsPlainDate(const AValue: TGocciaValue; const AMethod: string): TGocciaTemporalPlainDateValue;
begin
  if not (AValue is TGocciaTemporalPlainDateValue) then
    ThrowTypeError(AMethod + ' called on non-PlainDate', SSuggestTemporalThisType);
  Result := TGocciaTemporalPlainDateValue(AValue);
end;

function CalendarFromProperty(const AValue: TGocciaValue; const AMethod: string): string;
var
  CalendarString: string;
begin
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
    ThrowTypeError(AMethod + ' requires a string or Temporal calendar-carrying object',
      SSuggestTemporalFromArg);

  CalendarString := TGocciaStringLiteralValue(AValue).Value;
  Result := ParseTemporalCalendarStringIdentifierStrict(CalendarString);
  if Result = '' then
    ThrowRangeError('Unknown calendar: ' + CalendarString, SSuggestTemporalFromArg);
end;

function CalendarInfoForPlainDate(const ADate: TGocciaTemporalPlainDateValue;
  const AMethod: string): TTemporalCalendarDateInfo;
begin
  if not TryGetCalendarDateInfo(ADate.FCalendarId, ADate.FYear, ADate.FMonth, ADate.FDay, Result) then
    ThrowRangeError(Format(SErrorTemporalInvalidISOStringFor, ['date', AMethod]), SSuggestTemporalDateRange);
end;

function CoercePlainDate(const AValue: TGocciaValue; const AMethod: string): TGocciaTemporalPlainDateValue;
var
  DateRec: TTemporalDateRecord;
  Obj: TGocciaObjectValue;
  EmptyArgs: TGocciaArgumentsCollection;
  Converted, V, VMonth, VMonthCode, VDay, VYear, VEra, VEraYear: TGocciaValue;
  CalendarId, MonthCodeStr: string;
  Year, Month, Day, MonthFieldValue: Integer;
  HasYear, HasMonth, HasMonthCode, HasDay, IsLeapMonth: Boolean;
  RequiredFieldsMsg: string;
begin
  CalendarId := 'iso8601';
  if AValue is TGocciaTemporalPlainDateValue then
    Result := TGocciaTemporalPlainDateValue(AValue)
  else if AValue is TGocciaTemporalPlainDateTimeValue then
  begin
    Result := TGocciaTemporalPlainDateValue.Create(
      TGocciaTemporalPlainDateTimeValue(AValue).Year,
      TGocciaTemporalPlainDateTimeValue(AValue).Month,
      TGocciaTemporalPlainDateTimeValue(AValue).Day,
      TGocciaTemporalPlainDateTimeValue(AValue).CalendarId);
  end
  else if AValue is TGocciaTemporalZonedDateTimeValue then
  begin
    EmptyArgs := TGocciaArgumentsCollection.Create([]);
    try
      Converted := TGocciaTemporalZonedDateTimeValue(AValue).ZonedDateTimeToPlainDate(EmptyArgs, AValue);
      Result := TGocciaTemporalPlainDateValue(Converted);
    finally
      EmptyArgs.Free;
    end;
  end
  else if AValue is TGocciaStringLiteralValue then
  begin
    if (not TryExtractTemporalCalendarAnnotation(TGocciaStringLiteralValue(AValue).Value, CalendarId)) or
       (not CoerceToISODate(TGocciaStringLiteralValue(AValue).Value, DateRec)) then
      ThrowRangeError(Format(SErrorTemporalInvalidISOStringFor, ['date', AMethod]), SSuggestTemporalISOFormat);
    if not IsTemporalISODateInSupportedRange(DateRec.Year, DateRec.Month, DateRec.Day) then
      ThrowRangeError(Format(SErrorTemporalInvalidISOStringFor, ['date', AMethod]), SSuggestTemporalDateRange);
    Result := TGocciaTemporalPlainDateValue.Create(DateRec.Year, DateRec.Month, DateRec.Day, CalendarId);
  end
  else if AValue is TGocciaObjectValue then
  begin
    Obj := TGocciaObjectValue(AValue);
    V := Obj.GetProperty('calendar');
    if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
      CalendarId := CalendarFromProperty(V, AMethod);
    RequiredFieldsMsg := Format('%s requires %s, %s, %s properties', [AMethod, PROP_YEAR, PROP_MONTH, PROP_DAY]);
    VDay := Obj.GetProperty(PROP_DAY);
    HasDay := Assigned(VDay) and not (VDay is TGocciaUndefinedLiteralValue);
    if HasDay then
      Day := ToIntegerWithTruncationValue(VDay);
    VMonth := Obj.GetProperty(PROP_MONTH);
    HasMonth := Assigned(VMonth) and not (VMonth is TGocciaUndefinedLiteralValue);
    if HasMonth then
    begin
      MonthFieldValue := ToIntegerWithTruncationValue(VMonth);
      Month := MonthFieldValue;
    end;
    VMonthCode := Obj.GetProperty(PROP_MONTH_CODE);
    HasMonthCode := Assigned(VMonthCode) and not (VMonthCode is TGocciaUndefinedLiteralValue);
    if HasMonthCode then
    begin
      MonthCodeStr := VMonthCode.ToStringLiteral.Value;
      if (not TryParseTemporalMonthCode(MonthCodeStr, Month, IsLeapMonth)) or
         IsLeapMonth then
        ThrowTypeError(Format(SErrorInvalidMonthCodeFor, [AMethod]), SSuggestTemporalMonthCode);
      if HasMonth and (MonthFieldValue <> Month) then
        ThrowRangeError(Format(SErrorMonthCodeMismatchIn, [AMethod]), SSuggestTemporalMonthCode);
    end
    else if not HasMonth then
    begin
      Month := 0;
    end;
    VYear := Obj.GetProperty(PROP_YEAR);
    HasYear := Assigned(VYear) and not (VYear is TGocciaUndefinedLiteralValue);
    if HasYear then
      Year := ToIntegerWithTruncationValue(VYear);

    if not HasDay or (not HasMonth and not HasMonthCode) then
      ThrowTypeError(RequiredFieldsMsg, SSuggestTemporalFromArg);
    if not HasYear then
    begin
      VEra := Obj.GetProperty('era');
      VEraYear := Obj.GetProperty('eraYear');
      if (VEra = nil) or (VEra is TGocciaUndefinedLiteralValue) or
         (VEraYear = nil) or (VEraYear is TGocciaUndefinedLiteralValue) or
         not TryCalendarYearFromEra(CalendarId, VEra.ToStringLiteral.Value,
           ToIntegerWithTruncationValue(VEraYear), Year) then
        ThrowTypeError(RequiredFieldsMsg, SSuggestTemporalFromArg);
    end;
    if CalendarId <> 'iso8601' then
    begin
      if HasMonthCode then
      begin
        if not TryCalendarMonthCodeDateToISO(CalendarId, Year, Month, Day,
          IsLeapMonth, DateRec) then
          ThrowRangeError(Format(SErrorTemporalInvalidISOStringFor, ['date', AMethod]), SSuggestTemporalDateRange);
      end
      else if not TryCalendarDateToISO(CalendarId, Year, Month, Day, DateRec) then
        ThrowRangeError(Format(SErrorTemporalInvalidISOStringFor, ['date', AMethod]), SSuggestTemporalDateRange);
      Year := DateRec.Year;
      Month := DateRec.Month;
      Day := DateRec.Day;
    end;
    if not IsTemporalISODateInSupportedRange(Year, Month, Day) then
      ThrowRangeError(Format(SErrorTemporalInvalidISOStringFor, ['date', AMethod]), SSuggestTemporalDateRange);
    Result := TGocciaTemporalPlainDateValue.Create(Year, Month, Day, CalendarId);
  end
  else
  begin
    ThrowTypeError(AMethod + ' requires a PlainDate, string, or object', SSuggestTemporalFromArg);
    Result := nil;
  end;
end;

{ TGocciaTemporalPlainDateValue }

constructor TGocciaTemporalPlainDateValue.Create(const AYear, AMonth, ADay: Integer;
  const ACalendarId: string = 'iso8601');
begin
  inherited Create(nil);
  if not IsValidDate(AYear, AMonth, ADay) then
    ThrowRangeError(Format(SErrorInvalidDate, [FormatDateString(AYear, AMonth, ADay)]), SSuggestTemporalDateRange);
  if not IsTemporalISODateInSupportedRange(AYear, AMonth, ADay) then
    ThrowRangeError(Format(SErrorTemporalInvalidISOStringFor, ['date',
      'Temporal.PlainDate']), SSuggestTemporalDateRange);
  FYear := AYear;
  FMonth := AMonth;
  FDay := ADay;
  FCalendarId := CanonicalizeTemporalCalendarIdentifier(ACalendarId);
  if FCalendarId = '' then
    ThrowRangeError('Unknown calendar: ' + ACalendarId, SSuggestTemporalDateRange);
  InitializePrototype;
  if (GetTemporalPlainDateShared <> nil) then
    FPrototype := GetTemporalPlainDateShared.Prototype;
end;

procedure TGocciaTemporalPlainDateValue.InitializePrototype;
var
  Members: TGocciaMemberCollection;
  Shared: TGocciaSharedPrototype;
begin
  if (CurrentRealm = nil) then Exit;
  if (GetTemporalPlainDateShared <> nil) then Exit;

  // Rebuild member definitions per realm: callbacks bind to Self (the
  // bootstrap instance pinned by Shared), and TGocciaSharedPrototype.Destroy
  // unpins Self on realm tear-down.  Caching across realms would leave stale
  // method pointers referencing a freed instance.
  //
  // Defer SetOwnedSlot until after RegisterMemberDefinitions completes — if
  // member-collection construction or registration raises, the realm slot
  // stays nil and Shared is freed by the except handler, avoiding a half-
  // initialized prototype lingering in the slot.
  Shared := TGocciaSharedPrototype.Create(Self);
  try
    Members := TGocciaMemberCollection.Create;
    try
      Members.AddAccessor('calendarId', GetCalendarId, nil, [pfConfigurable]);
      Members.AddAccessor('era', GetEra, nil, [pfConfigurable]);
      Members.AddAccessor('eraYear', GetEraYear, nil, [pfConfigurable]);
      Members.AddAccessor('year', GetYear, nil, [pfConfigurable]);
      Members.AddAccessor('month', GetMonth, nil, [pfConfigurable]);
      Members.AddAccessor('monthCode', GetMonthCode, nil, [pfConfigurable]);
      Members.AddAccessor('day', GetDay, nil, [pfConfigurable]);
      Members.AddAccessor('dayOfWeek', GetDayOfWeek, nil, [pfConfigurable]);
      Members.AddAccessor('dayOfYear', GetDayOfYear, nil, [pfConfigurable]);
      Members.AddAccessor('weekOfYear', GetWeekOfYear, nil, [pfConfigurable]);
      Members.AddAccessor('yearOfWeek', GetYearOfWeek, nil, [pfConfigurable]);
      Members.AddAccessor('daysInWeek', GetDaysInWeek, nil, [pfConfigurable]);
      Members.AddAccessor('daysInMonth', GetDaysInMonth, nil, [pfConfigurable]);
      Members.AddAccessor('daysInYear', GetDaysInYear, nil, [pfConfigurable]);
      Members.AddAccessor('monthsInYear', GetMonthsInYear, nil, [pfConfigurable]);
      Members.AddAccessor('inLeapYear', GetInLeapYear, nil, [pfConfigurable]);
      Members.AddMethod(DateWith, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(DateAdd, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(DateSubtract, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(DateUntil, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(DateSince, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(DateEquals, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(DateToString, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(DateToJSON, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(DateValueOf, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(DateToPlainDateTime, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(DateToPlainYearMonth, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(DateToPlainMonthDay, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(DateToZonedDateTime, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(DateToLocaleString, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(DateWithCalendar, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddSymbolDataProperty(
        TGocciaSymbolValue.WellKnownToStringTag,
        TGocciaStringLiteralValue.Create('Temporal.PlainDate'),
        [pfConfigurable]);
      FPrototypeMembers := Members.ToDefinitions;
    finally
      Members.Free;
    end;
    RegisterMemberDefinitions(Shared.Prototype, FPrototypeMembers);
    CurrentRealm.SetOwnedSlot(GTemporalPlainDateSharedSlot, Shared);
  except
    Shared.Free;
    raise;
  end;
end;

class procedure TGocciaTemporalPlainDateValue.ExposePrototype(const AConstructor: TGocciaObjectValue);
var
  Shared: TGocciaSharedPrototype;
begin
  Shared := GetTemporalPlainDateShared;
  if not Assigned(Shared) then
  begin
    TGocciaTemporalPlainDateValue.Create(1970, 1, 1);
    Shared := GetTemporalPlainDateShared;
  end;
  if Assigned(Shared) then
    ExposeSharedPrototypeOnConstructor(Shared, AConstructor);
end;

function TGocciaTemporalPlainDateValue.ToStringTag: string;
begin
  Result := 'Temporal.PlainDate';
end;

{ Getters }

function TGocciaTemporalPlainDateValue.GetCalendarId(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalPlainDateValue;
begin
  D := AsPlainDate(AThisValue, 'get PlainDate.calendarId');
  Result := TGocciaStringLiteralValue.Create(D.FCalendarId);
end;

function TGocciaTemporalPlainDateValue.GetEra(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalPlainDateValue;
  Info: TTemporalCalendarDateInfo;
begin
  D := AsPlainDate(AThisValue, 'get PlainDate.era');
  Info := CalendarInfoForPlainDate(D, 'get PlainDate.era');
  if not Info.HasEra then
    Exit(TGocciaUndefinedLiteralValue.UndefinedValue);
  Result := TGocciaStringLiteralValue.Create(Info.Era);
end;

function TGocciaTemporalPlainDateValue.GetEraYear(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalPlainDateValue;
  Info: TTemporalCalendarDateInfo;
begin
  D := AsPlainDate(AThisValue, 'get PlainDate.eraYear');
  Info := CalendarInfoForPlainDate(D, 'get PlainDate.eraYear');
  if not Info.HasEra then
    Exit(TGocciaUndefinedLiteralValue.UndefinedValue);
  Result := TGocciaNumberLiteralValue.Create(Info.EraYear);
end;

function TGocciaTemporalPlainDateValue.GetYear(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalPlainDateValue;
begin
  D := AsPlainDate(AThisValue, 'get PlainDate.year');
  Result := TGocciaNumberLiteralValue.Create(CalendarInfoForPlainDate(D, 'get PlainDate.year').Date.Year);
end;

function TGocciaTemporalPlainDateValue.GetMonth(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalPlainDateValue;
begin
  D := AsPlainDate(AThisValue, 'get PlainDate.month');
  Result := TGocciaNumberLiteralValue.Create(CalendarInfoForPlainDate(D, 'get PlainDate.month').Date.Month);
end;

function TGocciaTemporalPlainDateValue.GetMonthCode(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalPlainDateValue;
begin
  D := AsPlainDate(AThisValue, 'get PlainDate.monthCode');
  Result := TGocciaStringLiteralValue.Create(CalendarInfoForPlainDate(D, 'get PlainDate.monthCode').MonthCode);
end;

function TGocciaTemporalPlainDateValue.GetDay(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalPlainDateValue;
  Day: Integer;
begin
  D := AsPlainDate(AThisValue, 'get PlainDate.day');
  if not TryGetCalendarDateDay(D.FCalendarId, D.FYear, D.FMonth, D.FDay,
    Day) then
    ThrowRangeError(Format(SErrorTemporalInvalidISOStringFor, ['date',
      'get PlainDate.day']), SSuggestTemporalDateRange);
  Result := TGocciaNumberLiteralValue.Create(Day);
end;

function TGocciaTemporalPlainDateValue.GetDayOfWeek(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalPlainDateValue;
begin
  D := AsPlainDate(AThisValue, 'get PlainDate.dayOfWeek');
  Result := TGocciaNumberLiteralValue.Create(CalendarInfoForPlainDate(D, 'get PlainDate.dayOfWeek').DayOfWeek);
end;

function TGocciaTemporalPlainDateValue.GetDayOfYear(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalPlainDateValue;
begin
  D := AsPlainDate(AThisValue, 'get PlainDate.dayOfYear');
  Result := TGocciaNumberLiteralValue.Create(CalendarInfoForPlainDate(D, 'get PlainDate.dayOfYear').DayOfYear);
end;

function TGocciaTemporalPlainDateValue.GetWeekOfYear(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalPlainDateValue;
begin
  D := AsPlainDate(AThisValue, 'get PlainDate.weekOfYear');
  if D.FCalendarId <> 'iso8601' then
    Exit(TGocciaUndefinedLiteralValue.UndefinedValue);
  Result := TGocciaNumberLiteralValue.Create(Goccia.Temporal.Utils.WeekOfYear(D.FYear, D.FMonth, D.FDay));
end;

function TGocciaTemporalPlainDateValue.GetYearOfWeek(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalPlainDateValue;
begin
  D := AsPlainDate(AThisValue, 'get PlainDate.yearOfWeek');
  if D.FCalendarId <> 'iso8601' then
    Exit(TGocciaUndefinedLiteralValue.UndefinedValue);
  Result := TGocciaNumberLiteralValue.Create(Goccia.Temporal.Utils.YearOfWeek(D.FYear, D.FMonth, D.FDay));
end;

function TGocciaTemporalPlainDateValue.GetDaysInWeek(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  AsPlainDate(AThisValue, 'get PlainDate.daysInWeek');
  Result := TGocciaNumberLiteralValue.Create(7);
end;

function TGocciaTemporalPlainDateValue.GetDaysInMonth(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalPlainDateValue;
  DaysInMonth: Integer;
begin
  D := AsPlainDate(AThisValue, 'get PlainDate.daysInMonth');
  if not TryGetCalendarDateDaysInMonth(D.FCalendarId, D.FYear, D.FMonth,
    D.FDay, DaysInMonth) then
    ThrowRangeError(Format(SErrorTemporalInvalidISOStringFor, ['date',
      'get PlainDate.daysInMonth']), SSuggestTemporalDateRange);
  Result := TGocciaNumberLiteralValue.Create(DaysInMonth);
end;

function TGocciaTemporalPlainDateValue.GetDaysInYear(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalPlainDateValue;
begin
  D := AsPlainDate(AThisValue, 'get PlainDate.daysInYear');
  Result := TGocciaNumberLiteralValue.Create(CalendarInfoForPlainDate(D, 'get PlainDate.daysInYear').DaysInYear);
end;

function TGocciaTemporalPlainDateValue.GetMonthsInYear(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalPlainDateValue;
begin
  D := AsPlainDate(AThisValue, 'get PlainDate.monthsInYear');
  Result := TGocciaNumberLiteralValue.Create(CalendarInfoForPlainDate(D, 'get PlainDate.monthsInYear').MonthsInYear);
end;

function TGocciaTemporalPlainDateValue.GetInLeapYear(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalPlainDateValue;
begin
  D := AsPlainDate(AThisValue, 'get PlainDate.inLeapYear');
  if CalendarInfoForPlainDate(D, 'get PlainDate.inLeapYear').InLeapYear then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;

{ Methods }

function TGocciaTemporalPlainDateValue.DateWith(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalPlainDateValue;
  Obj: TGocciaObjectValue;
  V, VYear, VMonth, VMonthCode, VDay, VEra, VEraYear: TGocciaValue;
  DateRec: TTemporalDateRecord;
  Info: TTemporalCalendarDateInfo;
  NewYear, NewMonth, NewDay, MonthFieldValue: Integer;
  MonthCodeStr: string;
  HasYear, HasMonth, HasMonthCode, HasDay, HasEra, HasEraYear,
    IsLeapMonth, IsValidMonthCode, UseMonthCode, SawDateField: Boolean;
  Overflow: TTemporalOverflow;

  function IsPresent(const AValue: TGocciaValue): Boolean;
  begin
    Result := Assigned(AValue) and not (AValue is TGocciaUndefinedLiteralValue);
  end;

begin
  D := AsPlainDate(AThisValue, 'PlainDate.prototype.with');
  V := AArgs.GetElement(0);
  if not (V is TGocciaObjectValue) then
    ThrowTypeError(Format(SErrorTemporalWithRequiresObject, ['PlainDate']), SSuggestTemporalWithObject);
  Obj := TGocciaObjectValue(V);
  if Copy(Obj.ToStringTag, 1, 9) = 'Temporal.' then
    ThrowTypeError(Format(SErrorTemporalWithRequiresObject, ['PlainDate']), SSuggestTemporalWithObject);

  V := Obj.GetProperty('calendar');
  if IsPresent(V) then
    ThrowTypeError('PlainDate.prototype.with does not accept calendar',
      SSuggestTemporalWithObject);
  V := Obj.GetProperty('timeZone');
  if IsPresent(V) then
    ThrowTypeError('PlainDate.prototype.with does not accept timeZone',
      SSuggestTemporalWithObject);

  Info := CalendarInfoForPlainDate(D, 'PlainDate.prototype.with');
  NewYear := Info.Date.Year;
  NewMonth := Info.Date.Month;
  NewDay := Info.Date.Day;
  IsLeapMonth := False;
  IsValidMonthCode := True;
  UseMonthCode := False;
  SawDateField := False;

  VDay := Obj.GetProperty(PROP_DAY);
  HasDay := IsPresent(VDay);
  if HasDay then
  begin
    SawDateField := True;
    NewDay := ToIntegerWithTruncationValue(VDay);
  end;
  VMonth := Obj.GetProperty(PROP_MONTH);
  HasMonth := IsPresent(VMonth);
  if HasMonth then
  begin
    SawDateField := True;
    MonthFieldValue := ToIntegerWithTruncationValue(VMonth);
    NewMonth := MonthFieldValue;
  end;
  VMonthCode := Obj.GetProperty(PROP_MONTH_CODE);
  HasMonthCode := IsPresent(VMonthCode);
  if HasMonthCode then
  begin
    SawDateField := True;
    MonthCodeStr := VMonthCode.ToStringLiteral.Value;
    IsValidMonthCode := TryParseTemporalMonthCode(MonthCodeStr, NewMonth,
      IsLeapMonth);
    UseMonthCode := not HasMonth;
  end;
  VYear := Obj.GetProperty(PROP_YEAR);
  HasYear := IsPresent(VYear);
  if HasYear then
  begin
    SawDateField := True;
    NewYear := ToIntegerWithTruncationValue(VYear);
  end
  else if D.FCalendarId <> 'iso8601' then
  begin
    VEra := Obj.GetProperty('era');
    VEraYear := Obj.GetProperty('eraYear');
    HasEra := IsPresent(VEra);
    HasEraYear := IsPresent(VEraYear);
    if HasEra or HasEraYear then
    begin
      SawDateField := True;
      if (not HasEra) or (not HasEraYear) or
         not TryCalendarYearFromEra(D.FCalendarId, VEra.ToStringLiteral.Value,
           ToIntegerWithTruncationValue(VEraYear), NewYear) then
        ThrowTypeError('PlainDate.prototype.with requires era and eraYear together',
          SSuggestTemporalFromArg);
    end;
  end;

  if not SawDateField then
    ThrowTypeError(Format(SErrorTemporalWithRequiresObject, ['PlainDate']),
      SSuggestTemporalWithObject);

  if (HasMonth and (NewMonth < 1)) or (HasDay and (NewDay < 1)) or
     ((NewYear < -271821) or (NewYear > 275760)) then
    ThrowRangeError(Format(SErrorTemporalInvalidISOStringFor, ['date',
      'PlainDate.prototype.with']), SSuggestTemporalDateRange);

  if HasMonthCode and HasMonth and (NewMonth <> MonthFieldValue) then
    ThrowRangeError(Format(SErrorMonthCodeMismatchIn, ['PlainDate.prototype.with']),
      SSuggestTemporalMonthCode);

  if (not HasMonth) and (not HasMonthCode) and (D.FCalendarId <> 'iso8601') then
  begin
    MonthCodeStr := Info.MonthCode;
    if not TryParseTemporalMonthCode(MonthCodeStr, NewMonth, IsLeapMonth) then
      ThrowRangeError(Format(SErrorTemporalInvalidISOStringFor, ['date',
        'PlainDate.prototype.with']), SSuggestTemporalDateRange);
    UseMonthCode := True;
  end;

  Overflow := GetOverflowOptionFromValue(AArgs.GetElement(1), 'PlainDate.prototype.with');

  if (not IsValidMonthCode) or
     (HasMonthCode and (D.FCalendarId = 'iso8601') and IsLeapMonth) then
    ThrowRangeError(Format(SErrorInvalidMonthCodeFor,
      ['PlainDate.prototype.with']), SSuggestTemporalMonthCode);

  if not TryResolveCalendarDateToISO(D.FCalendarId, NewYear, NewMonth, NewDay,
    UseMonthCode, IsLeapMonth, Overflow = toConstrain, DateRec) then
    ThrowRangeError(Format(SErrorTemporalInvalidISOStringFor, ['date',
      'PlainDate.prototype.with']), SSuggestTemporalDateRange);

  Result := TGocciaTemporalPlainDateValue.Create(DateRec.Year, DateRec.Month, DateRec.Day, D.FCalendarId);
end;

function TGocciaTemporalPlainDateValue.DateAdd(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalPlainDateValue;
  Dur: TGocciaTemporalDurationValue;
  Arg: TGocciaValue;
  Overflow: TTemporalOverflow;
  DateRec: TTemporalDateRecord;
  ExtraDays: Int64;
begin
  try
  D := AsPlainDate(AThisValue, 'PlainDate.prototype.add');
  Arg := AArgs.GetElement(0);

  if Arg is TGocciaTemporalDurationValue then
    Dur := TGocciaTemporalDurationValue(Arg)
  else if Arg is TGocciaStringLiteralValue then
    Dur := DurationFromString(TGocciaStringLiteralValue(Arg).Value,
      SErrorInvalidDurationString)
  else if Arg is TGocciaObjectValue then
    Dur := DurationFromObject(TGocciaObjectValue(Arg))
  else
  begin
    ThrowTypeError(Format(SErrorTemporalAddRequiresDuration, ['PlainDate']), SSuggestTemporalDurationArg);
    Dur := nil;
  end;

  Overflow := GetOverflowOptionFromValue(AArgs.GetElement(1), 'PlainDate.prototype.add');
  ExtraDays := DurationTimeWholeDays(Dur);
  if not TryAddCalendarDate(D.FCalendarId, D.FYear, D.FMonth, D.FDay,
    Dur.Years, Dur.Months, Dur.Weeks, Dur.Days + ExtraDays,
    Overflow = toConstrain, DateRec) then
    ThrowRangeError(Format(SErrorTemporalInvalidISOStringFor, ['date',
      'PlainDate.prototype.add']), SSuggestTemporalDateRange);
  if not IsTemporalISODateInSupportedRange(DateRec.Year, DateRec.Month,
    DateRec.Day) then
    ThrowRangeError(Format(SErrorTemporalInvalidISOStringFor, ['date',
      'PlainDate.prototype.add']), SSuggestTemporalDateRange);

  Result := TGocciaTemporalPlainDateValue.Create(DateRec.Year, DateRec.Month, DateRec.Day, D.FCalendarId);
  except
    on E: ETemporalDurationInt64Overflow do
      ThrowRangeError(E.Message, SSuggestTemporalDurationRange);
  end;
end;

function TGocciaTemporalPlainDateValue.DateSubtract(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalPlainDateValue;
  Dur: TGocciaTemporalDurationValue;
  Arg: TGocciaValue;
  NegatedDur: TGocciaTemporalDurationValue;
  NewArgs: TGocciaArgumentsCollection;
begin
  try
  D := AsPlainDate(AThisValue, 'PlainDate.prototype.subtract');
  Arg := AArgs.GetElement(0);

  if Arg is TGocciaTemporalDurationValue then
    Dur := TGocciaTemporalDurationValue(Arg)
  else if Arg is TGocciaStringLiteralValue then
    Dur := DurationFromString(TGocciaStringLiteralValue(Arg).Value,
      SErrorInvalidDurationString)
  else if Arg is TGocciaObjectValue then
    Dur := DurationFromObject(TGocciaObjectValue(Arg))
  else
  begin
    ThrowTypeError(Format(SErrorTemporalSubtractRequiresDuration, ['PlainDate']), SSuggestTemporalDurationArg);
    Dur := nil;
  end;

  NegatedDur := TGocciaTemporalDurationValue.CreateFromBigIntegers(
    Dur.YearsBig.Negate, Dur.MonthsBig.Negate, Dur.WeeksBig.Negate,
    Dur.DaysBig.Negate, Dur.HoursBig.Negate, Dur.MinutesBig.Negate,
    Dur.SecondsBig.Negate, Dur.MillisecondsBig.Negate,
    Dur.MicrosecondsBig.Negate, Dur.NanosecondsBig.Negate);

  TGarbageCollector.Instance.AddTempRoot(NegatedDur);
  try
    NewArgs := TGocciaArgumentsCollection.Create([NegatedDur, AArgs.GetElement(1)]);
    try
      Result := DateAdd(NewArgs, AThisValue);
    finally
      NewArgs.Free;
    end;
  finally
    TGarbageCollector.Instance.RemoveTempRoot(NegatedDur);
  end;
  except
    on E: ETemporalDurationInt64Overflow do
      ThrowRangeError(E.Message, SSuggestTemporalDurationRange);
  end;
end;

function TGocciaTemporalPlainDateValue.DateUntil(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D, Other: TGocciaTemporalPlainDateValue;
  OptionsObj: TGocciaObjectValue;
  LargestUnit, SmallestUnit: TTemporalUnit;
  Mode: TTemporalRoundingMode;
  Increment: Integer;
  Years, Months, Weeks, Days: Int64;
  ZeroH, ZeroM, ZeroS, ZeroMs, ZeroUs, ZeroNs: Int64;
begin
  D := AsPlainDate(AThisValue, 'PlainDate.prototype.until');
  Other := CoercePlainDate(AArgs.GetElement(0), 'PlainDate.prototype.until');

  OptionsObj := GetDiffOptions(AArgs, 1);
  LargestUnit := GetLargestUnit(OptionsObj, tuAuto);
  Increment := GetRoundingIncrement(OptionsObj, 1);
  Mode := GetRoundingMode(OptionsObj, rmTrunc);
  SmallestUnit := GetSmallestUnit(OptionsObj, tuDay);
  if LargestUnit = tuAuto then
  begin
    if Ord(SmallestUnit) < Ord(tuDay) then
      LargestUnit := SmallestUnit
    else
      LargestUnit := tuDay;
  end;
  if not (SmallestUnit in [tuYear, tuMonth, tuWeek, tuDay]) then
    ThrowRangeError(Format(SErrorTemporalInvalidUnitFor, ['PlainDate.prototype.until', 'smallestUnit']), SSuggestTemporalValidUnits);
  if not (LargestUnit in [tuYear, tuMonth, tuWeek, tuDay]) then
    ThrowRangeError(Format(SErrorTemporalInvalidUnitFor, ['PlainDate.prototype.until', 'largestUnit']), SSuggestTemporalValidUnits);
  if Ord(LargestUnit) > Ord(SmallestUnit) then
    ThrowRangeError(SErrorDurationRoundLargestSmallerThanSmallest, SSuggestTemporalRoundArg);
  ValidateRoundingIncrement(Increment, SmallestUnit);

  if D.FCalendarId <> Other.FCalendarId then
    ThrowRangeError('Temporal.PlainDate calendars must match',
      SSuggestTemporalDateRange);

  CalendarDateUntil(D.FCalendarId, D.FYear, D.FMonth, D.FDay,
    Other.FYear, Other.FMonth, Other.FDay, LargestUnit,
    Years, Months, Weeks, Days);

  if (SmallestUnit <> tuDay) or (Increment <> 1) then
  begin
    ZeroH := 0; ZeroM := 0; ZeroS := 0; ZeroMs := 0; ZeroUs := 0; ZeroNs := 0;
    RoundDiffDuration(Years, Months, Weeks, Days,
      ZeroH, ZeroM, ZeroS, ZeroMs, ZeroUs, ZeroNs,
      D.FYear, D.FMonth, D.FDay,
      LargestUnit, SmallestUnit, Mode, Increment);
  end;

  Result := TGocciaTemporalDurationValue.Create(Years, Months, Weeks, Days, 0, 0, 0, 0, 0, 0);
end;

function TGocciaTemporalPlainDateValue.DateSince(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D, Other: TGocciaTemporalPlainDateValue;
  OptionsObj: TGocciaObjectValue;
  LargestUnit, SmallestUnit: TTemporalUnit;
  Mode: TTemporalRoundingMode;
  Increment: Integer;
  Years, Months, Weeks, Days: Int64;
  ZeroH, ZeroM, ZeroS, ZeroMs, ZeroUs, ZeroNs: Int64;
begin
  D := AsPlainDate(AThisValue, 'PlainDate.prototype.since');
  Other := CoercePlainDate(AArgs.GetElement(0), 'PlainDate.prototype.since');

  OptionsObj := GetDiffOptions(AArgs, 1);
  LargestUnit := GetLargestUnit(OptionsObj, tuAuto);
  Increment := GetRoundingIncrement(OptionsObj, 1);
  Mode := GetRoundingMode(OptionsObj, rmTrunc);
  SmallestUnit := GetSmallestUnit(OptionsObj, tuDay);
  if LargestUnit = tuAuto then
  begin
    if Ord(SmallestUnit) < Ord(tuDay) then
      LargestUnit := SmallestUnit
    else
      LargestUnit := tuDay;
  end;
  if not (SmallestUnit in [tuYear, tuMonth, tuWeek, tuDay]) then
    ThrowRangeError(Format(SErrorTemporalInvalidUnitFor, ['PlainDate.prototype.since', 'smallestUnit']), SSuggestTemporalValidUnits);
  if not (LargestUnit in [tuYear, tuMonth, tuWeek, tuDay]) then
    ThrowRangeError(Format(SErrorTemporalInvalidUnitFor, ['PlainDate.prototype.since', 'largestUnit']), SSuggestTemporalValidUnits);
  if Ord(LargestUnit) > Ord(SmallestUnit) then
    ThrowRangeError(SErrorDurationRoundLargestSmallerThanSmallest, SSuggestTemporalRoundArg);
  ValidateRoundingIncrement(Increment, SmallestUnit);
  Mode := NegateTemporalRoundingMode(Mode);

  if D.FCalendarId <> Other.FCalendarId then
    ThrowRangeError('Temporal.PlainDate calendars must match',
      SSuggestTemporalDateRange);

  CalendarDateUntil(D.FCalendarId, D.FYear, D.FMonth, D.FDay,
    Other.FYear, Other.FMonth, Other.FDay, LargestUnit,
    Years, Months, Weeks, Days);

  if (SmallestUnit <> tuDay) or (Increment <> 1) then
  begin
    ZeroH := 0; ZeroM := 0; ZeroS := 0; ZeroMs := 0; ZeroUs := 0; ZeroNs := 0;
    RoundDiffDuration(Years, Months, Weeks, Days,
      ZeroH, ZeroM, ZeroS, ZeroMs, ZeroUs, ZeroNs,
      D.FYear, D.FMonth, D.FDay,
      LargestUnit, SmallestUnit, Mode, Increment);
  end;

  Years := -Years;
  Months := -Months;
  Weeks := -Weeks;
  Days := -Days;

  Result := TGocciaTemporalDurationValue.Create(Years, Months, Weeks, Days, 0, 0, 0, 0, 0, 0);
end;

function TGocciaTemporalPlainDateValue.DateEquals(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D, Other: TGocciaTemporalPlainDateValue;
begin
  D := AsPlainDate(AThisValue, 'PlainDate.prototype.equals');
  Other := CoercePlainDate(AArgs.GetElement(0), 'PlainDate.prototype.equals');

  if (D.FYear = Other.FYear) and (D.FMonth = Other.FMonth) and
     (D.FDay = Other.FDay) and (D.FCalendarId = Other.FCalendarId) then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;

function TGocciaTemporalPlainDateValue.DateToString(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalPlainDateValue;
  Arg: TGocciaValue;
  OptionsObj: TGocciaObjectValue;
  CalDisp: TTemporalCalendarDisplay;
begin
  D := AsPlainDate(AThisValue, 'PlainDate.prototype.toString');
  OptionsObj := nil;
  Arg := AArgs.GetElement(0);
  if Assigned(Arg) and not (Arg is TGocciaUndefinedLiteralValue) then
  begin
    if not (Arg is TGocciaObjectValue) then
      ThrowTypeError('options must be an object or undefined', SSuggestTemporalFromArg);
    OptionsObj := TGocciaObjectValue(Arg);
  end;
  CalDisp := GetCalendarDisplay(OptionsObj);
  Result := TGocciaStringLiteralValue.Create(
    FormatDateString(D.FYear, D.FMonth, D.FDay) + FormatCalendarAnnotation(CalDisp, D.FCalendarId));
end;

function TGocciaTemporalPlainDateValue.DateToJSON(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalPlainDateValue;
begin
  D := AsPlainDate(AThisValue, 'PlainDate.prototype.toJSON');
  Result := TGocciaStringLiteralValue.Create(
    FormatDateString(D.FYear, D.FMonth, D.FDay) + FormatCalendarAnnotation(tcdAuto, D.FCalendarId));
end;

function TGocciaTemporalPlainDateValue.DateValueOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  ThrowTypeError(Format(SErrorTemporalValueOf, ['PlainDate', 'toString or compare']), SSuggestTemporalNoValueOf);
  Result := nil;
end;

function TGocciaTemporalPlainDateValue.DateToPlainDateTime(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalPlainDateValue;
  T: TGocciaTemporalPlainTimeValue;
  Arg: TGocciaValue;
  Hour, Minute, Second, Ms, Us, Ns: Integer;
begin
  D := AsPlainDate(AThisValue, 'PlainDate.prototype.toPlainDateTime');
  Arg := AArgs.GetElement(0);

  Hour := 0; Minute := 0; Second := 0; Ms := 0; Us := 0; Ns := 0;

  if Assigned(Arg) and not (Arg is TGocciaUndefinedLiteralValue) then
  begin
    T := CoercePlainTime(Arg, 'PlainDate.prototype.toPlainDateTime');
    Hour := T.Hour; Minute := T.Minute; Second := T.Second;
    Ms := T.Millisecond; Us := T.Microsecond; Ns := T.Nanosecond;
  end;

  ValidatePlainDateTimeRange(D.FYear, D.FMonth, D.FDay, Hour, Minute, Second,
    Ms, Us, Ns, 'PlainDate.prototype.toPlainDateTime');
  Result := TGocciaTemporalPlainDateTimeValue.Create(D.FYear, D.FMonth, D.FDay,
    Hour, Minute, Second, Ms, Us, Ns, D.FCalendarId);
end;

function TGocciaTemporalPlainDateValue.DateToPlainYearMonth(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalPlainDateValue;
begin
  D := AsPlainDate(AThisValue, 'PlainDate.prototype.toPlainYearMonth');
  if D.FCalendarId = 'iso8601' then
    Result := TGocciaTemporalPlainYearMonthValue.Create(D.FYear, D.FMonth, 1,
      D.FCalendarId)
  else
    Result := TGocciaTemporalPlainYearMonthValue.Create(D.FYear, D.FMonth,
      D.FDay, D.FCalendarId);
end;

function TGocciaTemporalPlainDateValue.DateToPlainMonthDay(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalPlainDateValue;
  Info: TTemporalCalendarDateInfo;
  DateRec: TTemporalDateRecord;
  MonthPart: Integer;
  IsLeapMonth: Boolean;
begin
  D := AsPlainDate(AThisValue, 'PlainDate.prototype.toPlainMonthDay');
  if D.FCalendarId = 'iso8601' then
    Result := TGocciaTemporalPlainMonthDayValue.Create(D.FMonth, D.FDay,
      1972, D.FCalendarId)
  else
  begin
    Info := CalendarInfoForPlainDate(D, 'PlainDate.prototype.toPlainMonthDay');
    if not TryParseTemporalMonthCode(Info.MonthCode, MonthPart, IsLeapMonth) then
      ThrowRangeError(Format(SErrorInvalidMonthDayStringFor,
        ['PlainDate.prototype.toPlainMonthDay']), SSuggestTemporalDateRange);
    if not TryResolvePlainMonthDayReferenceDate(D.FCalendarId, MonthPart,
      Info.Date.Day, True, IsLeapMonth, DateRec) then
      ThrowRangeError(Format(SErrorInvalidMonthDayStringFor,
        ['PlainDate.prototype.toPlainMonthDay']), SSuggestTemporalDateRange);
    Result := TGocciaTemporalPlainMonthDayValue.Create(DateRec.Month,
      DateRec.Day, DateRec.Year, D.FCalendarId);
  end;
end;

function TGocciaTemporalPlainDateValue.DateToZonedDateTime(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalPlainDateValue;
  Arg, PlainTimeArg: TGocciaValue;
  PlainTime: TGocciaTemporalPlainTimeValue;
  OptionsObj: TGocciaObjectValue;
  TimeZoneStr: string;
  EpochMs: Int64;
  SubMs: Integer;
begin
  D := AsPlainDate(AThisValue, 'PlainDate.prototype.toZonedDateTime');
  Arg := AArgs.GetElement(0);
  PlainTimeArg := nil;

  if Arg is TGocciaStringLiteralValue then
    TimeZoneStr := TGocciaStringLiteralValue(Arg).Value
  else if Arg is TGocciaObjectValue then
  begin
    OptionsObj := TGocciaObjectValue(Arg);
    Arg := OptionsObj.GetProperty('timeZone');
    if (Arg = nil) or (Arg is TGocciaUndefinedLiteralValue) then
      ThrowTypeError(SErrorPlainDateToZonedRequiresTZ, SSuggestTemporalTimezone);
    if not (Arg is TGocciaStringLiteralValue) then
      ThrowTypeError(SErrorPlainDateToZonedRequiresTZ, SSuggestTemporalTimezone);
    TimeZoneStr := TGocciaStringLiteralValue(Arg).Value;
    PlainTimeArg := OptionsObj.GetProperty('plainTime');
  end
  else
  begin
    ThrowTypeError(SErrorPlainDateToZonedRequiresStringOrOptions, SSuggestTemporalTimezone);
    TimeZoneStr := '';
  end;

  TimeZoneStr := CanonicalizeTemporalTimeZoneIdentifier(TimeZoneStr);

  SubMs := 0;
  if Assigned(PlainTimeArg) and not (PlainTimeArg is TGocciaUndefinedLiteralValue) then
  begin
    PlainTime := CoercePlainTime(PlainTimeArg, 'PlainDate.prototype.toZonedDateTime');
    EpochMs := LocalDateTimeToEpochMillisecondsWithDisambiguation(D.FYear,
      D.FMonth, D.FDay, PlainTime.Hour, PlainTime.Minute, PlainTime.Second,
      PlainTime.Millisecond, TimeZoneStr, ttzdCompatible);
    SubMs := PlainTime.Microsecond * 1000 + PlainTime.Nanosecond;
  end
  else
    EpochMs := StartOfDayToEpochMilliseconds(D.FYear, D.FMonth, D.FDay,
      TimeZoneStr);

  Result := TGocciaTemporalZonedDateTimeValue.Create(EpochMs, SubMs,
    TimeZoneStr, D.FCalendarId);
end;

function TGocciaTemporalPlainDateValue.DateToLocaleString(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  AsPlainDate(AThisValue, 'PlainDate.prototype.toLocaleString');
  Result := FormatTemporalValueToLocaleString(AThisValue, AArgs.GetElement(0),
    AArgs.GetElement(1));
end;

function TGocciaTemporalPlainDateValue.DateWithCalendar(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  D: TGocciaTemporalPlainDateValue;
  Arg: TGocciaValue;
  CalId: string;
begin
  D := AsPlainDate(AThisValue, 'PlainDate.prototype.withCalendar');
  Arg := AArgs.GetElement(0);
  if (Arg = nil) or (Arg is TGocciaUndefinedLiteralValue) then
    ThrowTypeError('PlainDate.prototype.withCalendar requires a calendar argument', SSuggestTemporalFromArg);
  CalId := CalendarFromProperty(Arg, 'PlainDate.prototype.withCalendar');
  Result := TGocciaTemporalPlainDateValue.Create(D.FYear, D.FMonth, D.FDay, CalId);
end;

initialization
  RegisterThreadvarCleanup(@ClearThreadvarMembers);
  GTemporalPlainDateSharedSlot := RegisterRealmOwnedSlot('Temporal.PlainDate.shared');

end.
