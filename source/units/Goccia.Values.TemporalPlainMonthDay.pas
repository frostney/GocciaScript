unit Goccia.Values.TemporalPlainMonthDay;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.ObjectModel,
  Goccia.SharedPrototype,
  Goccia.Temporal.Utils,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

const
  DEFAULT_REFERENCE_YEAR = 1972;

type
  TGocciaTemporalPlainMonthDayValue = class(TGocciaObjectValue)
  private
    FMonth: Integer;
    FDay: Integer;
    FReferenceYear: Integer;
    FCalendarId: string;
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
    constructor Create(const AMonth, ADay: Integer; const ACalendarId: string = 'iso8601'); overload;
    constructor Create(const AMonth, ADay, AReferenceYear: Integer;
      const ACalendarId: string = 'iso8601'); overload;

    function ToStringTag: string; override;
    class procedure ExposePrototype(const AConstructor: TGocciaObjectValue);

    property Month: Integer read FMonth;
    property Day: Integer read FDay;
    property ReferenceYear: Integer read FReferenceYear;
    property CalendarId: string read FCalendarId;
  end;

function TryResolvePlainMonthDayReferenceDate(const ACalendarId: string;
  const AMonthOrMonthCode, ADay: Integer; const AMatchMonthCode,
  AIsLeapMonth: Boolean; out AISODate: TTemporalDateRecord): Boolean;

implementation

uses
  SysUtils,

  Goccia.Constants.PropertyNames,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.Realm,
  Goccia.Temporal.Calendar,
  Goccia.Temporal.Options,
  Goccia.Utils,
  Goccia.Values.ErrorHelper,
  Goccia.Values.IntlDateTimeFormat,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.SymbolValue,
  Goccia.Values.TemporalPlainDate,
  Goccia.Values.TemporalPlainDateTime,
  Goccia.Values.TemporalPlainYearMonth,
  Goccia.Values.TemporalZonedDateTime;

var
  GTemporalPlainMonthDaySharedSlot: TGocciaRealmOwnedSlotId;

function GetTemporalPlainMonthDayShared: TGocciaSharedPrototype;
{$IFDEF FPC}inline;{$ENDIF}
begin
  if (CurrentRealm <> nil) then
    Result := TGocciaSharedPrototype(CurrentRealm.GetOwnedSlot(GTemporalPlainMonthDaySharedSlot))
  else
    Result := nil;
end;

function TryResolvePlainMonthDayReferenceDate(const ACalendarId: string;
  const AMonthOrMonthCode, ADay: Integer; const AMatchMonthCode,
  AIsLeapMonth: Boolean; out AISODate: TTemporalDateRecord): Boolean;

  function TryReferenceInISOYear(const AISOReferenceYear, ATargetMonth,
    ATargetDay: Integer; const AMatchCode, ALeapMonth: Boolean;
    out ADate: TTemporalDateRecord): Boolean;
  var
    StartDays, Days: Int64;
    Offset: Integer;
    Candidate: TTemporalDateRecord;
    CandidateInfo: TTemporalCalendarDateInfo;
    ExpectedMonthCode: string;
  begin
    Result := False;
    ADate.Year := 0;
    ADate.Month := 0;
    ADate.Day := 0;
    if (ATargetDay < 1) or (ATargetMonth < 1) or
       not IsValidDate(AISOReferenceYear, 1, 1) then
      Exit;

    if ACalendarId = 'iso8601' then
    begin
      if IsValidDate(AISOReferenceYear, ATargetMonth, ATargetDay) then
      begin
        ADate.Year := AISOReferenceYear;
        ADate.Month := ATargetMonth;
        ADate.Day := ATargetDay;
        Exit(True);
      end;
      Exit;
    end;

    ExpectedMonthCode := FormatTemporalMonthCode(ATargetMonth, ALeapMonth);
    StartDays := DateToEpochDays(AISOReferenceYear, 1, 1);
    for Offset := DaysInYear(AISOReferenceYear) - 1 downto 0 do
    begin
      Days := StartDays + Offset;
      Candidate := EpochDaysToDate(Days);
      if not TryGetCalendarDateInfo(ACalendarId, Candidate.Year,
        Candidate.Month, Candidate.Day, CandidateInfo) then
        Exit(False);
      if CandidateInfo.Date.Day <> ATargetDay then
        Continue;
      if AMatchCode then
      begin
        if CandidateInfo.MonthCode = ExpectedMonthCode then
        begin
          ADate := Candidate;
          Exit(True);
        end;
      end
      else if CandidateInfo.Date.Month = ATargetMonth then
      begin
        ADate := Candidate;
        Exit(True);
      end;
    end;
  end;

  function MonthCodeSupported(const AMonthValue, ADayValue: Integer;
    const ALeapMonth: Boolean): Boolean;
  begin
    if (AMonthValue < 1) or (ADayValue < 1) then
      Exit(False);
    if (ACalendarId = 'chinese') or (ACalendarId = 'dangi') then
      Exit(AMonthValue <= 12);
    if ACalendarId = 'hebrew' then
    begin
      if ALeapMonth then
        Exit(AMonthValue = 5);
      Exit(AMonthValue <= 12);
    end;
    if (ACalendarId = 'coptic') or (ACalendarId = 'ethiopic') or
       (ACalendarId = 'ethioaa') then
      Exit((not ALeapMonth) and (AMonthValue <= 13));
    Result := (not ALeapMonth) and (AMonthValue <= 12);
  end;

  function TryReferenceYearHint(const AMonthValue, ADayForHint: Integer;
    const ALeapMonth: Boolean; out AYear: Integer): Boolean;
  begin
    AYear := DEFAULT_REFERENCE_YEAR;
    Result := MonthCodeSupported(AMonthValue, ADayForHint, ALeapMonth);
    if not Result then
      Exit;

    if (ACalendarId = 'chinese') or (ACalendarId = 'dangi') then
    begin
      if ALeapMonth then
      begin
        if ADayForHint >= 30 then
        begin
          case AMonthValue of
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
          case AMonthValue of
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
                if ADayForHint >= 29 then
                  AYear := 2034
                else
                  AYear := 2033;
              end;
          else
            Exit(False);
          end;
        end;
      end
      else if ADayForHint >= 30 then
      begin
        case AMonthValue of
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
      if ALeapMonth then
      begin
        AYear := 1970;
        Exit(True);
      end;
      if ADayForHint >= 30 then
      begin
        case AMonthValue of
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
      if AMonthValue = 13 then
      begin
        if ADayForHint >= 6 then
          AYear := 1971
        else
          AYear := 1972;
      end;
      Exit(True);
    end;

    if (ACalendarId = 'islamic-civil') or
       (ACalendarId = 'islamic-tbla') then
    begin
      if (AMonthValue = 12) and (ADayForHint >= 30) then
        AYear := 1971
      else if (ADayForHint >= 30) and not (AMonthValue in [1, 3, 5, 7, 9, 11]) then
        Exit(False);
      Exit(True);
    end;

    if ACalendarId = 'islamic-umalqura' then
    begin
      if ADayForHint >= 30 then
      begin
        case AMonthValue of
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

  function TryReferenceMonthDay(const AMonthValue, ADayValue: Integer;
    const AMatchCode, ALeapMonth: Boolean; out ADate: TTemporalDateRecord): Boolean;
  var
    Year: Integer;
  begin
    Result := TryReferenceYearHint(AMonthValue, ADayValue, ALeapMonth, Year) and
      TryReferenceInISOYear(Year, AMonthValue, ADayValue, AMatchCode,
        ALeapMonth, ADate);
  end;

var
  WorkDay: Integer;
begin
  if TryReferenceMonthDay(AMonthOrMonthCode, ADay, AMatchMonthCode,
    AIsLeapMonth, AISODate) then
    Exit(True);

  if AMatchMonthCode and AIsLeapMonth and
     ((ACalendarId = 'chinese') or (ACalendarId = 'dangi')) then
  begin
    if ADay >= 30 then
    begin
      if TryReferenceMonthDay(AMonthOrMonthCode, 30, True, True, AISODate) then
        Exit(True);
      if TryReferenceMonthDay(AMonthOrMonthCode, 30, True, False, AISODate) then
        Exit(True);
    end;
    if TryReferenceMonthDay(AMonthOrMonthCode, ADay, True, False, AISODate) then
      Exit(True);
  end;

  WorkDay := ADay;
  while WorkDay > 1 do
  begin
    Dec(WorkDay);
    if TryReferenceMonthDay(AMonthOrMonthCode, WorkDay, AMatchMonthCode,
      AIsLeapMonth, AISODate) then
      Exit(True);
  end;
  Result := False;
end;

function AsPlainMonthDay(const AValue: TGocciaValue; const AMethod: string): TGocciaTemporalPlainMonthDayValue;
begin
  if not (AValue is TGocciaTemporalPlainMonthDayValue) then
    ThrowTypeError(AMethod + ' called on non-PlainMonthDay', SSuggestTemporalThisType);
  Result := TGocciaTemporalPlainMonthDayValue(AValue);
end;

function CalendarInfoForPlainMonthDay(const AMonthDay: TGocciaTemporalPlainMonthDayValue;
  const AMethod: string): TTemporalCalendarDateInfo;
begin
  if not TryGetCalendarDateInfo(AMonthDay.FCalendarId, AMonthDay.FReferenceYear,
    AMonthDay.FMonth, AMonthDay.FDay, Result) then
    ThrowRangeError(Format(SErrorInvalidMonthDayStringFor, [AMethod]), SSuggestTemporalDateRange);
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

function MonthCodeStringFromValue(const AValue: TGocciaValue;
  const AMethod: string): string;
begin
  if AValue is TGocciaStringLiteralValue then
    Exit(TGocciaStringLiteralValue(AValue).Value);
  if AValue is TGocciaObjectValue then
    Exit(AValue.ToStringLiteral.Value);
  ThrowTypeError(Format(SErrorInvalidMonthCodeFor, [AMethod]),
    SSuggestTemporalMonthCode);
  Result := '';
end;

function CoercePlainMonthDay(const AValue: TGocciaValue; const AMethod: string): TGocciaTemporalPlainMonthDayValue;
var
  MonthPart, DayPart: Integer;
  DateRec, SourceDateRec: TTemporalDateRecord;
  Obj: TGocciaObjectValue;
  V, VMonthCode, VDay, VEra, VEraYear: TGocciaValue;
  MonthCodeStr: string;
  CalendarId: string;
  ReferenceYear: Integer;
  IsLeapMonth: Boolean;
  HasReferenceYear, HasSourceDate: Boolean;
begin
  CalendarId := 'iso8601';
  ReferenceYear := DEFAULT_REFERENCE_YEAR;
  HasReferenceYear := False;
  if AValue is TGocciaTemporalPlainMonthDayValue then
    Result := TGocciaTemporalPlainMonthDayValue.Create(
      TGocciaTemporalPlainMonthDayValue(AValue).FMonth,
      TGocciaTemporalPlainMonthDayValue(AValue).FDay,
      TGocciaTemporalPlainMonthDayValue(AValue).FReferenceYear,
      TGocciaTemporalPlainMonthDayValue(AValue).FCalendarId)
  else if AValue is TGocciaStringLiteralValue then
  begin
    if not TryParseTemporalPlainMonthDayString(TGocciaStringLiteralValue(AValue).Value,
      MonthPart, DayPart, CalendarId, SourceDateRec, HasSourceDate) then
      ThrowRangeError(Format(SErrorInvalidMonthDayStringFor, [AMethod]), SSuggestTemporalISOFormat);
    Result := TGocciaTemporalPlainMonthDayValue.Create(MonthPart, DayPart, CalendarId);
  end
  else if AValue is TGocciaObjectValue then
  begin
    Obj := TGocciaObjectValue(AValue);
    V := Obj.GetProperty('calendar');
    CalendarId := CalendarIdFromTemporalCalendarLike(V, AMethod);
    V := Obj.GetProperty(PROP_YEAR);
    if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
    begin
      ReferenceYear := ToIntegerWithTruncationValue(V);
      HasReferenceYear := True;
    end;
    if not HasReferenceYear then
    begin
      VEra := Obj.GetProperty('era');
      VEraYear := Obj.GetProperty('eraYear');
      if Assigned(VEra) and not (VEra is TGocciaUndefinedLiteralValue) then
      begin
        if (VEraYear = nil) or (VEraYear is TGocciaUndefinedLiteralValue) or
           not TryCalendarYearFromEra(CalendarId, VEra.ToStringLiteral.Value,
             ToIntegerWithTruncationValue(VEraYear), ReferenceYear) then
          ThrowRangeError(Format(SErrorInvalidMonthDayStringFor, [AMethod]), SSuggestTemporalDateRange);
        HasReferenceYear := True;
      end
      else if Assigned(VEraYear) and not (VEraYear is TGocciaUndefinedLiteralValue) then
        ThrowRangeError(Format(SErrorInvalidMonthDayStringFor, [AMethod]), SSuggestTemporalDateRange);
    end;
    VDay := Obj.GetProperty(PROP_DAY);
    if (VDay = nil) or (VDay is TGocciaUndefinedLiteralValue) then
      ThrowTypeError(AMethod + ' requires day property', SSuggestTemporalFromArg);
    VMonthCode := Obj.GetProperty(PROP_MONTH_CODE);
    if Assigned(VMonthCode) and not (VMonthCode is TGocciaUndefinedLiteralValue) then
    begin
      MonthCodeStr := VMonthCode.ToStringLiteral.Value;
      if not TryParseTemporalMonthCode(MonthCodeStr, MonthPart, IsLeapMonth) then
        ThrowTypeError(Format(SErrorInvalidMonthCodeFor, [AMethod]), SSuggestTemporalMonthCode);
      // Validate month/monthCode consistency when both are provided
      V := Obj.GetProperty(PROP_MONTH);
      if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
        if ToIntegerWithTruncationValue(V) <> MonthPart then
          ThrowRangeError(Format(SErrorMonthCodeMismatchIn, [AMethod]), SSuggestTemporalMonthCode);
    end
    else
    begin
      V := Obj.GetProperty(PROP_MONTH);
      if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
        MonthPart := ToIntegerWithTruncationValue(V)
      else
        ThrowTypeError(AMethod + ' requires monthCode or month property', SSuggestTemporalFromArg);
    end;
    DayPart := ToIntegerWithTruncationValue(VDay);
    if CalendarId <> 'iso8601' then
    begin
      if not HasReferenceYear then
      begin
        if not TryCalendarMonthDayInISOYear(CalendarId, ReferenceYear,
          MonthPart, DayPart, Assigned(VMonthCode) and
          not (VMonthCode is TGocciaUndefinedLiteralValue), IsLeapMonth,
          DateRec) then
          ThrowRangeError(Format(SErrorInvalidMonthDayStringFor, [AMethod]), SSuggestTemporalDateRange);
      end
      else if Assigned(VMonthCode) and not (VMonthCode is TGocciaUndefinedLiteralValue) then
      begin
        if not TryCalendarMonthCodeDateToISO(CalendarId, ReferenceYear,
          MonthPart, DayPart, IsLeapMonth, DateRec) then
          ThrowRangeError(Format(SErrorInvalidMonthDayStringFor, [AMethod]), SSuggestTemporalDateRange);
      end
      else if not TryCalendarDateToISO(CalendarId, ReferenceYear, MonthPart, DayPart, DateRec) then
        ThrowRangeError(Format(SErrorInvalidMonthDayStringFor, [AMethod]), SSuggestTemporalDateRange);
      ReferenceYear := DateRec.Year;
      MonthPart := DateRec.Month;
      DayPart := DateRec.Day;
    end;
    if (CalendarId = 'iso8601') and HasReferenceYear then
    begin
      if not IsTemporalISODateInSupportedRange(ReferenceYear, MonthPart,
        DayPart) then
        ThrowRangeError(Format(SErrorInvalidMonthDayStringFor, [AMethod]),
          SSuggestTemporalDateRange);
      ReferenceYear := DEFAULT_REFERENCE_YEAR;
    end;
    Result := TGocciaTemporalPlainMonthDayValue.Create(MonthPart, DayPart, ReferenceYear, CalendarId);
  end
  else
  begin
    ThrowTypeError(AMethod + ' requires a PlainMonthDay, string, or object', SSuggestTemporalFromArg);
    Result := nil;
  end;
end;

{ TGocciaTemporalPlainMonthDayValue }

constructor TGocciaTemporalPlainMonthDayValue.Create(const AMonth, ADay: Integer;
  const ACalendarId: string = 'iso8601');
begin
  Create(AMonth, ADay, DEFAULT_REFERENCE_YEAR, ACalendarId);
end;

constructor TGocciaTemporalPlainMonthDayValue.Create(const AMonth, ADay, AReferenceYear: Integer;
  const ACalendarId: string = 'iso8601');
var
  Shared: TGocciaSharedPrototype;
begin
  inherited Create(nil);
  if not IsValidDate(AReferenceYear, AMonth, ADay) then
    ThrowRangeError(Format(SErrorInvalidMonthDay, [PadTwo(AMonth) + '-' + PadTwo(ADay)]), SSuggestTemporalDateRange);
  if not IsTemporalISODateInSupportedRange(AReferenceYear, AMonth, ADay) then
    ThrowRangeError(Format(SErrorInvalidMonthDay, [PadTwo(AMonth) + '-' + PadTwo(ADay)]),
      SSuggestTemporalDateRange);
  FMonth := AMonth;
  FDay := ADay;
  FReferenceYear := AReferenceYear;
  FCalendarId := CanonicalizeTemporalCalendarIdentifier(ACalendarId);
  if FCalendarId = '' then
    ThrowRangeError('Unknown calendar: ' + ACalendarId, SSuggestTemporalDateRange);
  InitializePrototype;
  Shared := GetTemporalPlainMonthDayShared;
  if Assigned(Shared) then
    FPrototype := Shared.Prototype;
end;

procedure TGocciaTemporalPlainMonthDayValue.InitializePrototype;
var
  Members: TGocciaMemberCollection;
  Shared: TGocciaSharedPrototype;
  PrototypeMembers: TArray<TGocciaMemberDefinition>;
begin
  if (CurrentRealm = nil) then Exit;
  if (GetTemporalPlainMonthDayShared <> nil) then Exit;
  Shared := TGocciaSharedPrototype.Create(Self);
  CurrentRealm.SetOwnedSlot(GTemporalPlainMonthDaySharedSlot, Shared);
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
    PrototypeMembers := Members.ToDefinitions;
  finally
    Members.Free;
  end;
  RegisterMemberDefinitions(Shared.Prototype, PrototypeMembers);
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
var
  MD: TGocciaTemporalPlainMonthDayValue;
begin
  MD := AsPlainMonthDay(AThisValue, 'get PlainMonthDay.calendarId');
  Result := TGocciaStringLiteralValue.Create(MD.FCalendarId);
end;

// TC39 Temporal §11.3.4 get Temporal.PlainMonthDay.prototype.monthCode
function TGocciaTemporalPlainMonthDayValue.GetMonthCode(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  MD: TGocciaTemporalPlainMonthDayValue;
begin
  MD := AsPlainMonthDay(AThisValue, 'get PlainMonthDay.monthCode');
  Result := TGocciaStringLiteralValue.Create(CalendarInfoForPlainMonthDay(MD, 'get PlainMonthDay.monthCode').MonthCode);
end;

// TC39 Temporal §11.3.5 get Temporal.PlainMonthDay.prototype.day
function TGocciaTemporalPlainMonthDayValue.GetDay(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  MD: TGocciaTemporalPlainMonthDayValue;
begin
  MD := AsPlainMonthDay(AThisValue, 'get PlainMonthDay.day');
  Result := TGocciaNumberLiteralValue.Create(CalendarInfoForPlainMonthDay(MD, 'get PlainMonthDay.day').Date.Day);
end;

{ Methods }

// TC39 Temporal §11.3.6 Temporal.PlainMonthDay.prototype.with(temporalMonthDayLike)
function TGocciaTemporalPlainMonthDayValue.MonthDayWith(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  MD: TGocciaTemporalPlainMonthDayValue;
  Obj: TGocciaObjectValue;
  V, VCalendar, VTimeZone, VMonth, VMonthCode, VDay, VYear: TGocciaValue;
  DateRec: TTemporalDateRecord;
  Info: TTemporalCalendarDateInfo;
  NewMonth, NewDay, ReferenceYear, CandidateMonth: Integer;
  MonthCodeStr: string;
  HasDay, HasMonth, HasMonthCode, HasYear, IsLeapMonth, UseMonthCode: Boolean;
  Overflow: TTemporalOverflow;

  function IsPresent(const AValue: TGocciaValue): Boolean;
  begin
    Result := Assigned(AValue) and not (AValue is TGocciaUndefinedLiteralValue);
  end;

begin
  MD := AsPlainMonthDay(AThisValue, 'PlainMonthDay.prototype.with');
  V := AArgs.GetElement(0);
  if (not (V is TGocciaObjectValue)) or
     (V is TGocciaTemporalPlainDateValue) or
     (V is TGocciaTemporalPlainDateTimeValue) or
     (V is TGocciaTemporalPlainMonthDayValue) or
     (V is TGocciaTemporalPlainYearMonthValue) or
     (V is TGocciaTemporalZonedDateTimeValue) then
    ThrowTypeError(Format(SErrorTemporalWithRequiresObject, ['PlainMonthDay']), SSuggestTemporalWithObject);
  Obj := TGocciaObjectValue(V);

  Info := CalendarInfoForPlainMonthDay(MD, 'PlainMonthDay.prototype.with');
  VCalendar := Obj.GetProperty('calendar');
  if IsPresent(VCalendar) then
    ThrowTypeError('PlainMonthDay.prototype.with does not accept calendar',
      SSuggestTemporalFromArg);
  VTimeZone := Obj.GetProperty('timeZone');
  if IsPresent(VTimeZone) then
    ThrowTypeError('PlainMonthDay.prototype.with does not accept timeZone',
      SSuggestTemporalFromArg);
  VDay := Obj.GetProperty(PROP_DAY);
  HasDay := IsPresent(VDay);
  if HasDay then
    NewDay := ToIntegerWithTruncationValue(VDay)
  else
    NewDay := Info.Date.Day;
  VMonth := Obj.GetProperty(PROP_MONTH);
  HasMonth := IsPresent(VMonth);
  CandidateMonth := 0;
  if HasMonth then
  begin
    CandidateMonth := ToIntegerWithTruncationValue(VMonth);
    NewMonth := CandidateMonth;
  end
  else
    NewMonth := Info.Date.Month;
  VMonthCode := Obj.GetProperty(PROP_MONTH_CODE);
  HasMonthCode := IsPresent(VMonthCode);
  IsLeapMonth := False;
  UseMonthCode := False;

  if HasMonthCode then
  begin
    MonthCodeStr := MonthCodeStringFromValue(VMonthCode,
      'PlainMonthDay.prototype.with');
    if not TryParseTemporalMonthCode(MonthCodeStr, NewMonth, IsLeapMonth) then
      ThrowRangeError(Format(SErrorInvalidMonthCodeFor, ['PlainMonthDay.prototype.with']), SSuggestTemporalMonthCode);
    if HasMonth and (CandidateMonth <> NewMonth) then
      ThrowRangeError(Format(SErrorMonthCodeMismatchIn, ['PlainMonthDay.prototype.with']), SSuggestTemporalMonthCode);
    UseMonthCode := not HasMonth;
  end;
  VYear := Obj.GetProperty(PROP_YEAR);
  HasYear := IsPresent(VYear);
  ReferenceYear := MD.FReferenceYear;
  if HasYear then
    ReferenceYear := ToIntegerWithTruncationValue(VYear);
  if (not HasDay) and (not HasMonth) and (not HasMonthCode) and (not HasYear) then
    ThrowTypeError(Format(SErrorTemporalWithRequiresObject, ['PlainMonthDay']),
      SSuggestTemporalWithObject);

  if NewDay < 1 then
    ThrowRangeError(Format(SErrorInvalidMonthDayStringFor,
      ['PlainMonthDay.prototype.with']), SSuggestTemporalDateRange);
  if (MD.FCalendarId = 'iso8601') and (NewMonth < 1) then
    ThrowRangeError(Format(SErrorInvalidMonthDayStringFor,
      ['PlainMonthDay.prototype.with']), SSuggestTemporalDateRange);

  Overflow := GetOverflowOptionFromValue(AArgs.GetElement(1),
    'PlainMonthDay.prototype.with');

  if HasMonth and (not HasMonthCode) and (MD.FCalendarId <> 'iso8601') then
    ThrowTypeError('PlainMonthDay.prototype.with requires monthCode for non-ISO calendars',
      SSuggestTemporalMonthCode);
  if (MD.FCalendarId = 'iso8601') and HasMonthCode and
     (IsLeapMonth or (NewMonth < 1) or (NewMonth > 12)) then
    ThrowRangeError(Format(SErrorInvalidMonthCodeFor,
      ['PlainMonthDay.prototype.with']), SSuggestTemporalMonthCode);
  if (MD.FCalendarId = 'iso8601') and (NewMonth > 12) then
  begin
    if Overflow = toReject then
      ThrowRangeError(Format(SErrorInvalidMonthDayStringFor,
        ['PlainMonthDay.prototype.with']), SSuggestTemporalDateRange);
    NewMonth := 12;
  end;

  if MD.FCalendarId <> 'iso8601' then
  begin
    if not HasMonthCode then
    begin
      MonthCodeStr := Info.MonthCode;
      if not TryParseTemporalMonthCode(MonthCodeStr, NewMonth, IsLeapMonth) then
        ThrowRangeError(Format(SErrorInvalidMonthDayStringFor,
          ['PlainMonthDay.prototype.with']), SSuggestTemporalDateRange);
      UseMonthCode := True;
    end;
  end
  else if HasYear and (NewDay > DaysInMonth(ReferenceYear, NewMonth)) then
  begin
    if Overflow = toReject then
      ThrowRangeError(Format(SErrorInvalidMonthDayStringFor,
        ['PlainMonthDay.prototype.with']), SSuggestTemporalDateRange);
    NewDay := DaysInMonth(ReferenceYear, NewMonth);
  end;

  if (Overflow = toReject) and
     (not TryCalendarMonthDayInISOYear(MD.FCalendarId, ReferenceYear,
       NewMonth, NewDay, UseMonthCode, IsLeapMonth, DateRec)) then
    ThrowRangeError(Format(SErrorInvalidMonthDayStringFor,
      ['PlainMonthDay.prototype.with']), SSuggestTemporalDateRange);
  if not TryResolvePlainMonthDayReferenceDate(MD.FCalendarId,
    NewMonth, NewDay, UseMonthCode, IsLeapMonth, DateRec) then
    ThrowRangeError(Format(SErrorInvalidMonthDayStringFor,
      ['PlainMonthDay.prototype.with']), SSuggestTemporalDateRange);

  Result := TGocciaTemporalPlainMonthDayValue.Create(DateRec.Month, DateRec.Day,
    DateRec.Year, MD.FCalendarId);
end;

// TC39 Temporal §11.3.7 Temporal.PlainMonthDay.prototype.equals(other)
function TGocciaTemporalPlainMonthDayValue.MonthDayEquals(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  MD, Other: TGocciaTemporalPlainMonthDayValue;
begin
  MD := AsPlainMonthDay(AThisValue, 'PlainMonthDay.prototype.equals');
  Other := CoercePlainMonthDay(AArgs.GetElement(0), 'PlainMonthDay.prototype.equals');

  if (MD.FMonth = Other.FMonth) and (MD.FDay = Other.FDay) and
     (MD.FReferenceYear = Other.FReferenceYear) and
     (MD.FCalendarId = Other.FCalendarId) then
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
  if (CalDisp = tcdAlways) or (CalDisp = tcdCritical) or
     ((CalDisp = tcdAuto) and (MD.FCalendarId <> 'iso8601')) then
    Result := TGocciaStringLiteralValue.Create(
      FormatDateString(MD.FReferenceYear, MD.FMonth, MD.FDay) + FormatCalendarAnnotation(CalDisp, MD.FCalendarId))
  else if MD.FCalendarId <> 'iso8601' then
    Result := TGocciaStringLiteralValue.Create(
      FormatDateString(MD.FReferenceYear, MD.FMonth, MD.FDay))
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
  if MD.FCalendarId <> 'iso8601' then
    Result := TGocciaStringLiteralValue.Create(
      FormatDateString(MD.FReferenceYear, MD.FMonth, MD.FDay) +
      FormatCalendarAnnotation(tcdAuto, MD.FCalendarId))
  else
    Result := TGocciaStringLiteralValue.Create(
      PadTwo(MD.FMonth) + '-' + PadTwo(MD.FDay));
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
  Arg, V, VEra, VEraYear: TGocciaValue;
  Obj: TGocciaObjectValue;
  DateRec: TTemporalDateRecord;
  Info: TTemporalCalendarDateInfo;
  YearValue, MonthValue, DayValue: Integer;
  MonthCodeStr: string;
  IsLeapMonth: Boolean;
begin
  MD := AsPlainMonthDay(AThisValue, 'PlainMonthDay.prototype.toPlainDate');
  Arg := AArgs.GetElement(0);

  if not (Arg is TGocciaObjectValue) then
    ThrowTypeError(SErrorPlainMonthDayToPlainDateRequiresYear, SSuggestTemporalToPlainDateYear);
  Obj := TGocciaObjectValue(Arg);
  V := Obj.GetProperty(PROP_YEAR);
  if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
    YearValue := ToIntegerWithTruncationValue(V)
  else
  begin
    VEra := Obj.GetProperty('era');
    VEraYear := Obj.GetProperty('eraYear');
    if (VEra = nil) or (VEra is TGocciaUndefinedLiteralValue) or
       (VEraYear = nil) or (VEraYear is TGocciaUndefinedLiteralValue) then
      ThrowTypeError(SErrorPlainMonthDayToPlainDateRequiresYear, SSuggestTemporalToPlainDateYear);
    if not TryCalendarYearFromEra(MD.FCalendarId, VEra.ToStringLiteral.Value,
      ToIntegerWithTruncationValue(VEraYear), YearValue) then
      ThrowRangeError(SErrorPlainMonthDayToPlainDateRequiresYear, SSuggestTemporalDateRange);
  end;
  Info := CalendarInfoForPlainMonthDay(MD, 'PlainMonthDay.prototype.toPlainDate');
  if MD.FCalendarId = 'iso8601' then
  begin
    MonthValue := MD.FMonth;
    DayValue := MD.FDay;
    if DayValue > DaysInMonth(YearValue, MonthValue) then
      DayValue := DaysInMonth(YearValue, MonthValue);
    if not IsTemporalISODateInSupportedRange(YearValue, MonthValue,
      DayValue) then
      ThrowRangeError(SErrorPlainMonthDayToPlainDateRequiresYear,
        SSuggestTemporalDateRange);
    Result := TGocciaTemporalPlainDateValue.Create(YearValue, MonthValue,
      DayValue, MD.FCalendarId);
  end
  else
  begin
    MonthCodeStr := Info.MonthCode;
    if not TryParseTemporalMonthCode(MonthCodeStr, MonthValue, IsLeapMonth) then
      ThrowRangeError(SErrorPlainMonthDayToPlainDateRequiresYear,
        SSuggestTemporalDateRange);
    if not TryResolveCalendarDateToISO(MD.FCalendarId, YearValue, MonthValue,
      Info.Date.Day, True, IsLeapMonth, True, DateRec) then
      ThrowRangeError(SErrorPlainMonthDayToPlainDateRequiresYear,
        SSuggestTemporalDateRange);
    if not IsTemporalISODateInSupportedRange(DateRec.Year, DateRec.Month,
      DateRec.Day) then
      ThrowRangeError(SErrorPlainMonthDayToPlainDateRequiresYear,
        SSuggestTemporalDateRange);
    Result := TGocciaTemporalPlainDateValue.Create(DateRec.Year, DateRec.Month,
      DateRec.Day, MD.FCalendarId);
  end;
end;

function TGocciaTemporalPlainMonthDayValue.MonthDayToLocaleString(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  AsPlainMonthDay(AThisValue, 'PlainMonthDay.prototype.toLocaleString');
  Result := FormatTemporalValueToLocaleString(AThisValue, AArgs.GetElement(0),
    AArgs.GetElement(1));
end;

initialization
  GTemporalPlainMonthDaySharedSlot := RegisterRealmOwnedSlot('Temporal.PlainMonthDay.shared');

end.
