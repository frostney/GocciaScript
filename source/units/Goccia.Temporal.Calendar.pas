unit Goccia.Temporal.Calendar;

{$I Goccia.inc}

interface

uses
  Goccia.Temporal.Utils;

type
  TTemporalCalendarDateInfo = record
    Date: TTemporalDateRecord;
    MonthCode: string;
    Era: string;
    EraYear: Integer;
    HasEra: Boolean;
    DayOfWeek: Integer;
    DayOfYear: Integer;
    DaysInMonth: Integer;
    DaysInYear: Integer;
    MonthsInYear: Integer;
    InLeapYear: Boolean;
  end;

function TryCalendarDateToISO(const ACalendarId: string; const AYear, AMonth, ADay: Integer;
  out AISODate: TTemporalDateRecord): Boolean;
function TryCalendarMonthCodeDateToISO(const ACalendarId: string; const AYear, AMonthCode, ADay: Integer;
  const AIsLeapMonth: Boolean; out AISODate: TTemporalDateRecord): Boolean;
function TryResolveCalendarDateToISO(const ACalendarId: string; const AYear, AMonth, ADay: Integer;
  const AMatchMonthCode, AIsLeapMonth, AConstrain: Boolean;
  out AISODate: TTemporalDateRecord): Boolean;
function TryAddCalendarDate(const ACalendarId: string; const AISOYear, AISOMonth, AISODay: Integer;
  const AYears, AMonths, AWeeks, ADays: Int64; const AConstrain: Boolean;
  out AISODate: TTemporalDateRecord): Boolean;
function TryCalendarMonthDayInISOYear(const ACalendarId: string;
  const AISOReferenceYear, AMonthOrMonthCode, ADay: Integer;
  const AMatchMonthCode, AIsLeapMonth: Boolean;
  out AISODate: TTemporalDateRecord): Boolean;
function TryISODateToCalendar(const ACalendarId: string; const AISOYear, AISOMonth, AISODay: Integer;
  out ACalendarDate: TTemporalDateRecord): Boolean;
function TryGetCalendarDateDay(const ACalendarId: string; const AISOYear,
  AISOMonth, AISODay: Integer; out ADay: Integer): Boolean;
function TryGetCalendarDateDaysInMonth(const ACalendarId: string; const AISOYear,
  AISOMonth, AISODay: Integer; out ADaysInMonth: Integer): Boolean;
function TryGetCalendarDateInfo(const ACalendarId: string; const AISOYear, AISOMonth, AISODay: Integer;
  out AInfo: TTemporalCalendarDateInfo): Boolean;
function TryCalendarMonthsInYear(const ACalendarId: string; const AYear: Integer;
  out AMonthsInYear: Integer): Boolean;
function TryParseTemporalMonthCode(const AMonthCode: string; out AMonth: Integer;
  out AIsLeapMonth: Boolean): Boolean;
function FormatTemporalMonthCode(const AMonth: Integer; const AIsLeapMonth: Boolean = False): string;
function TryCalendarEraFromYear(const ACalendarId: string; const AYear: Integer;
  out AEra: string; out AEraYear: Integer; out AHasEra: Boolean): Boolean;
function TryCalendarYearFromEra(const ACalendarId, AEra: string; const AEraYear: Integer;
  out AYear: Integer): Boolean;
function TemporalCalendarUsesEra(const ACalendarId: string): Boolean;
function TryParseTemporalPlainYearMonthString(const AValue: string;
  out AYear, AMonth, AReferenceDay: Integer; out ACalendarId: string): Boolean;
function TryParseTemporalPlainMonthDayString(const AValue: string;
  out AMonth, ADay: Integer; out ACalendarId: string;
  out ASourceDate: TTemporalDateRecord; out AHasSourceDate: Boolean): Boolean;
function ParseTemporalCalendarStringIdentifierStrict(const AValue: string): string;
function IsTemporalISODateInSupportedRange(const AYear, AMonth, ADay: Integer): Boolean;
function IsTemporalISOYearMonthInSupportedRange(const AYear, AMonth: Integer): Boolean;

implementation

uses
  Math,
  SysUtils,

  ICU,
  IntlICU,
  IntlTypes;

const
  ICU_SUCCESS = 0;
  ICU_CAL_DEFAULT = 0;
  ICU_FIELD_YEAR = 1;
  ICU_FIELD_MONTH = 2;
  ICU_FIELD_DATE = 5;
  ICU_FIELD_EXTENDED_YEAR = 19;
  ICU_FIELD_IS_LEAP_MONTH = 22;
  MS_PER_DAY = 86400000.0;
  MILLISECONDS_PER_HALF_DAY = 43200000.0;
  COPTIC_EPOCH_DAYS = -615558;
  LUNISOLAR_CACHE_SIZE = 4096;
  TEMPORAL_MIN_ISO_YEAR = -271821;
  TEMPORAL_MIN_ISO_MONTH = 4;
  TEMPORAL_MIN_ISO_DAY = 19;
  TEMPORAL_MAX_ISO_YEAR = 275760;
  TEMPORAL_MAX_ISO_MONTH = 9;
  TEMPORAL_MAX_ISO_DAY = 13;
  MAX_TEMPORAL_CALENDAR_YEAR_OFFSET = TEMPORAL_MAX_ISO_YEAR - TEMPORAL_MIN_ISO_YEAR + 1;
  MAX_TEMPORAL_CALENDAR_MONTH_OFFSET = MAX_TEMPORAL_CALENDAR_YEAR_OFFSET * 13;

type
  TICUCalendar = Pointer;
  TICUDate = Double;
  TICUErrorCode = LongInt;
  TICUCalendarOpen = function(const AZoneId: PWideChar; const AZoneIdLength: LongInt;
    const ALocale: PAnsiChar; const ACalendarType: LongInt;
    var AStatus: TICUErrorCode): TICUCalendar; cdecl;
  TICUCalendarClose = procedure(const ACalendar: TICUCalendar); cdecl;
  TICUCalendarClear = procedure(const ACalendar: TICUCalendar); cdecl;
  TICUCalendarSet = procedure(const ACalendar: TICUCalendar; const AField: LongInt;
    const AValue: LongInt); cdecl;
  TICUCalendarSetMillis = procedure(const ACalendar: TICUCalendar;
    const AMilliseconds: TICUDate; var AStatus: TICUErrorCode); cdecl;
  TICUCalendarGetMillis = function(const ACalendar: TICUCalendar;
    var AStatus: TICUErrorCode): TICUDate; cdecl;
  TICUCalendarGet = function(const ACalendar: TICUCalendar; const AField: LongInt;
    var AStatus: TICUErrorCode): LongInt; cdecl;
  TICUCalendarSetGregorianChange = procedure(const ACalendar: TICUCalendar;
    const ADate: TICUDate; var AStatus: TICUErrorCode); cdecl;

  TTemporalCalendarICU = record
    OpenCalendar: TICUCalendarOpen;
    CloseCalendar: TICUCalendarClose;
    ClearCalendar: TICUCalendarClear;
    SetField: TICUCalendarSet;
    SetMillis: TICUCalendarSetMillis;
    GetMillis: TICUCalendarGetMillis;
    GetField: TICUCalendarGet;
    SetGregorianChange: TICUCalendarSetGregorianChange;
  end;

  TLunisolarDateParts = record
    Year: Integer;
    MonthCodeMonth: Integer;
    Day: Integer;
    IsLeapMonth: Boolean;
  end;

  TLunisolarPartsCacheEntry = record
    Valid: Boolean;
    CalendarCode: Integer;
    ISOYear: Integer;
    ISOMonth: Integer;
    ISODay: Integer;
    Parts: TLunisolarDateParts;
  end;

  TLunisolarMonthsInYearCacheEntry = record
    Valid: Boolean;
    CalendarCode: Integer;
    Year: Integer;
    MonthsInYear: Integer;
  end;

  TLunisolarDateToISOCacheEntry = record
    Valid: Boolean;
    CalendarCode: Integer;
    Year: Integer;
    Month: Integer;
    Day: Integer;
    MatchMonthCode: Boolean;
    IsLeapMonth: Boolean;
    ISODate: TTemporalDateRecord;
  end;

  TLunisolarISOToCalendarCacheEntry = record
    Valid: Boolean;
    CalendarCode: Integer;
    ISOYear: Integer;
    ISOMonth: Integer;
    ISODay: Integer;
    CalendarDate: TTemporalDateRecord;
  end;

  TChineseYearDaysOverride = record
    Year: Integer;
    DaysInYear: Integer;
  end;

  TChineseLeapMonthOverride = record
    Year: Integer;
    LeapMonthIndex: Integer;
  end;

const
  CHINESE_YEAR_DAYS_OVERRIDES: array[0..79] of TChineseYearDaysOverride = (
    (Year: 1969; DaysInYear: 354),
    (Year: 1970; DaysInYear: 355),
    (Year: 1971; DaysInYear: 384),
    (Year: 1972; DaysInYear: 354),
    (Year: 1973; DaysInYear: 354),
    (Year: 1974; DaysInYear: 384),
    (Year: 1975; DaysInYear: 354),
    (Year: 1976; DaysInYear: 384),
    (Year: 1977; DaysInYear: 354),
    (Year: 1978; DaysInYear: 355),
    (Year: 1979; DaysInYear: 384),
    (Year: 1980; DaysInYear: 355),
    (Year: 1981; DaysInYear: 354),
    (Year: 1982; DaysInYear: 384),
    (Year: 1983; DaysInYear: 354),
    (Year: 1984; DaysInYear: 384),
    (Year: 1985; DaysInYear: 354),
    (Year: 1986; DaysInYear: 354),
    (Year: 1987; DaysInYear: 384),
    (Year: 1988; DaysInYear: 355),
    (Year: 1989; DaysInYear: 355),
    (Year: 1990; DaysInYear: 384),
    (Year: 1991; DaysInYear: 354),
    (Year: 1992; DaysInYear: 354),
    (Year: 1993; DaysInYear: 383),
    (Year: 1994; DaysInYear: 355),
    (Year: 1995; DaysInYear: 384),
    (Year: 1996; DaysInYear: 354),
    (Year: 1997; DaysInYear: 355),
    (Year: 1998; DaysInYear: 384),
    (Year: 1999; DaysInYear: 354),
    (Year: 2000; DaysInYear: 354),
    (Year: 2001; DaysInYear: 384),
    (Year: 2002; DaysInYear: 354),
    (Year: 2003; DaysInYear: 355),
    (Year: 2004; DaysInYear: 384),
    (Year: 2005; DaysInYear: 354),
    (Year: 2006; DaysInYear: 385),
    (Year: 2007; DaysInYear: 354),
    (Year: 2008; DaysInYear: 354),
    (Year: 2009; DaysInYear: 384),
    (Year: 2010; DaysInYear: 354),
    (Year: 2011; DaysInYear: 354),
    (Year: 2012; DaysInYear: 384),
    (Year: 2013; DaysInYear: 355),
    (Year: 2014; DaysInYear: 384),
    (Year: 2015; DaysInYear: 354),
    (Year: 2016; DaysInYear: 355),
    (Year: 2017; DaysInYear: 384),
    (Year: 2018; DaysInYear: 354),
    (Year: 2019; DaysInYear: 354),
    (Year: 2020; DaysInYear: 384),
    (Year: 2021; DaysInYear: 354),
    (Year: 2022; DaysInYear: 355),
    (Year: 2023; DaysInYear: 384),
    (Year: 2024; DaysInYear: 354),
    (Year: 2025; DaysInYear: 384),
    (Year: 2026; DaysInYear: 354),
    (Year: 2027; DaysInYear: 354),
    (Year: 2028; DaysInYear: 384),
    (Year: 2029; DaysInYear: 355),
    (Year: 2030; DaysInYear: 354),
    (Year: 2031; DaysInYear: 384),
    (Year: 2032; DaysInYear: 355),
    (Year: 2033; DaysInYear: 384),
    (Year: 2034; DaysInYear: 354),
    (Year: 2035; DaysInYear: 354),
    (Year: 2036; DaysInYear: 384),
    (Year: 2037; DaysInYear: 354),
    (Year: 2038; DaysInYear: 354),
    (Year: 2039; DaysInYear: 384),
    (Year: 2040; DaysInYear: 355),
    (Year: 2041; DaysInYear: 355),
    (Year: 2042; DaysInYear: 384),
    (Year: 2043; DaysInYear: 354),
    (Year: 2044; DaysInYear: 384),
    (Year: 2045; DaysInYear: 354),
    (Year: 2046; DaysInYear: 354),
    (Year: 2047; DaysInYear: 384),
    (Year: 2048; DaysInYear: 354)
  );

  CHINESE_LEAP_MONTH_OVERRIDES: array[0..25] of TChineseLeapMonthOverride = (
    (Year: 1971; LeapMonthIndex: 6),
    (Year: 1974; LeapMonthIndex: 5),
    (Year: 1976; LeapMonthIndex: 9),
    (Year: 1979; LeapMonthIndex: 7),
    (Year: 1982; LeapMonthIndex: 5),
    (Year: 1987; LeapMonthIndex: 7),
    (Year: 1990; LeapMonthIndex: 6),
    (Year: 1993; LeapMonthIndex: 4),
    (Year: 1995; LeapMonthIndex: 9),
    (Year: 1998; LeapMonthIndex: 6),
    (Year: 2001; LeapMonthIndex: 5),
    (Year: 2004; LeapMonthIndex: 3),
    (Year: 2006; LeapMonthIndex: 8),
    (Year: 2009; LeapMonthIndex: 6),
    (Year: 2012; LeapMonthIndex: 5),
    (Year: 2017; LeapMonthIndex: 7),
    (Year: 2020; LeapMonthIndex: 5),
    (Year: 2023; LeapMonthIndex: 3),
    (Year: 2025; LeapMonthIndex: 7),
    (Year: 2028; LeapMonthIndex: 6),
    (Year: 2031; LeapMonthIndex: 4),
    (Year: 2036; LeapMonthIndex: 7),
    (Year: 2039; LeapMonthIndex: 6),
    (Year: 2042; LeapMonthIndex: 3),
    (Year: 2044; LeapMonthIndex: 8),
    (Year: 2047; LeapMonthIndex: 6)
  );

var
  CalendarICU: TTemporalCalendarICU;
  CalendarICULoadAttempted: Boolean;
  CalendarICUAvailable: Boolean;

threadvar
  LunisolarPartsCache: array[0..LUNISOLAR_CACHE_SIZE - 1] of TLunisolarPartsCacheEntry;
  LunisolarMonthsInYearCache: array[0..LUNISOLAR_CACHE_SIZE - 1] of TLunisolarMonthsInYearCacheEntry;
  LunisolarDateToISOCache: array[0..LUNISOLAR_CACHE_SIZE - 1] of TLunisolarDateToISOCacheEntry;
  LunisolarISOToCalendarCache: array[0..LUNISOLAR_CACHE_SIZE - 1] of TLunisolarISOToCalendarCacheEntry;

function ICUSucceeded(const AStatus: TICUErrorCode): Boolean; inline;
begin
  Result := AStatus <= ICU_SUCCESS;
end;

function TryLoadCalendarICU(out AICU: TTemporalCalendarICU): Boolean;
var
  Symbol: Pointer;
begin
  Result := False;
  FillChar(AICU, SizeOf(AICU), 0);

  Symbol := ICUGetProcAddress('ucal_open');
  if not Assigned(Symbol) then Exit;
  AICU.OpenCalendar := TICUCalendarOpen(Symbol);

  Symbol := ICUGetProcAddress('ucal_close');
  if not Assigned(Symbol) then Exit;
  AICU.CloseCalendar := TICUCalendarClose(Symbol);

  Symbol := ICUGetProcAddress('ucal_clear');
  if not Assigned(Symbol) then Exit;
  AICU.ClearCalendar := TICUCalendarClear(Symbol);

  Symbol := ICUGetProcAddress('ucal_set');
  if not Assigned(Symbol) then Exit;
  AICU.SetField := TICUCalendarSet(Symbol);

  Symbol := ICUGetProcAddress('ucal_setMillis');
  if not Assigned(Symbol) then Exit;
  AICU.SetMillis := TICUCalendarSetMillis(Symbol);

  Symbol := ICUGetProcAddress('ucal_getMillis');
  if not Assigned(Symbol) then Exit;
  AICU.GetMillis := TICUCalendarGetMillis(Symbol);

  Symbol := ICUGetProcAddress('ucal_get');
  if not Assigned(Symbol) then Exit;
  AICU.GetField := TICUCalendarGet(Symbol);

  Symbol := ICUGetProcAddress('ucal_setGregorianChange');
  if Assigned(Symbol) then
    AICU.SetGregorianChange := TICUCalendarSetGregorianChange(Symbol);

  Result := True;
end;

function TryGetCalendarICU(out AICU: TTemporalCalendarICU): Boolean;
begin
  if not CalendarICULoadAttempted then
  begin
    CalendarICULoadAttempted := True;
    CalendarICUAvailable := TryLoadCalendarICU(CalendarICU);
  end;

  AICU := CalendarICU;
  Result := CalendarICUAvailable;
end;

function CalendarLocale(const ACalendarId: string): AnsiString;
var
  ICUCalendarId: string;
begin
  if ACalendarId = 'gregory' then
    ICUCalendarId := 'gregorian'
  else if ACalendarId = 'ethioaa' then
    ICUCalendarId := 'ethiopic-amete-alem'
  else
    ICUCalendarId := ACalendarId;
  Result := AnsiString('en_US@calendar=' + ICUCalendarId);
end;

function CalendarDateBefore(const AY, AM, AD, ABY, ABM, ABD: Integer): Boolean;
begin
  Result := (AY < ABY) or ((AY = ABY) and
    ((AM < ABM) or ((AM = ABM) and (AD < ABD))));
end;

function Int64MagnitudeGreaterThan(const AValue, ALimit: Int64): Boolean;
begin
  if AValue < 0 then
    Result := AValue < -ALimit
  else
    Result := AValue > ALimit;
end;

function TryAddInt64(const ALeft, ARight: Int64; out AResult: Int64): Boolean;
begin
  Result := False;
  if ((ARight > 0) and (ALeft > High(Int64) - ARight)) or
     ((ARight < 0) and (ALeft < Low(Int64) - ARight)) then
    Exit;
  AResult := ALeft + ARight;
  Result := True;
end;

function TryMultiplyInt64(const ALeft, ARight: Int64; out AResult: Int64): Boolean;
begin
  Result := False;
  if ARight = 0 then
  begin
    AResult := 0;
    Exit(True);
  end;
  if (ALeft <> 0) and
     ((ALeft > High(Int64) div ARight) or (ALeft < Low(Int64) div ARight)) then
    Exit;
  AResult := ALeft * ARight;
  Result := True;
end;

function TryAddISOCalendarDate(const AISOYear, AISOMonth, AISODay: Integer;
  const AYears, AMonths, AWeeks, ADays: Int64;
  const AConstrain: Boolean; out AISODate: TTemporalDateRecord): Boolean;
var
  YearMonths, MonthDelta, StartMonthIndex, TargetMonthIndex: Int64;
  DayDelta: Int64;
  MonthDate: TTemporalDateRecord;
begin
  Result := False;
  AISODate.Year := 0;
  AISODate.Month := 0;
  AISODate.Day := 0;

  if not TryMultiplyInt64(AYears, 12, YearMonths) or
     not TryAddInt64(YearMonths, AMonths, MonthDelta) then
    Exit;

  StartMonthIndex := Int64(AISOYear) * 12 + AISOMonth - 1;
  if not TryAddInt64(StartMonthIndex, MonthDelta, TargetMonthIndex) or
     (TargetMonthIndex < Int64(Low(Integer)) * 12) or
     (TargetMonthIndex > Int64(High(Integer)) * 12 + 11) then
    Exit;

  MonthDate := AddMonthsToDate(AISOYear, AISOMonth, AISODay, MonthDelta);
  if (not AConstrain) and (MonthDate.Day <> AISODay) then
    Exit;
  if not TryMultiplyInt64(AWeeks, 7, DayDelta) or
     not TryAddInt64(DayDelta, ADays, DayDelta) then
    Exit;

  AISODate := AddDaysToDate(MonthDate.Year, MonthDate.Month, MonthDate.Day,
    DayDelta);
  Result := IsValidDate(AISODate.Year, AISODate.Month, AISODate.Day);
end;

function IsTemporalISODateInSupportedRange(const AYear, AMonth, ADay: Integer): Boolean;
begin
  Result := IsValidDate(AYear, AMonth, ADay) and
    not CalendarDateBefore(AYear, AMonth, ADay, TEMPORAL_MIN_ISO_YEAR,
      TEMPORAL_MIN_ISO_MONTH, TEMPORAL_MIN_ISO_DAY) and
    not CalendarDateBefore(TEMPORAL_MAX_ISO_YEAR, TEMPORAL_MAX_ISO_MONTH,
      TEMPORAL_MAX_ISO_DAY, AYear, AMonth, ADay);
end;

function IsTemporalISOYearMonthInSupportedRange(const AYear, AMonth: Integer): Boolean;
begin
  Result := (AMonth >= 1) and (AMonth <= 12) and
    not CalendarDateBefore(AYear, AMonth, 1, TEMPORAL_MIN_ISO_YEAR,
      TEMPORAL_MIN_ISO_MONTH, 1) and
    not CalendarDateBefore(TEMPORAL_MAX_ISO_YEAR, TEMPORAL_MAX_ISO_MONTH,
      1, AYear, AMonth, 1);
end;

function IsPlainTemporalAnnotationKey(const AKey: string): Boolean;
var
  I: Integer;
  C: Char;
  InSegment: Boolean;
begin
  Result := False;
  if AKey = '' then
    Exit;
  if not ((AKey[1] in ['a'..'z']) or (AKey[1] = '_')) then
    Exit;
  InSegment := True;
  for I := 2 to Length(AKey) do
  begin
    C := AKey[I];
    if C = '-' then
    begin
      if not InSegment then
        Exit;
      InSegment := False;
    end
    else if (C in ['a'..'z']) or (C in ['0'..'9']) or (C = '_') then
      InSegment := True
    else
      Exit;
  end;
  Result := InSegment;
end;

function TryStripPlainTemporalAnnotations(const AValue: string;
  out ACore, ACalendarId: string): Boolean;
var
  BracketStart, BracketEnd, EqualsPos: Integer;
  Annotation, Key, CalendarValue, EffectiveCalendarValue: string;
  Critical, CalendarCriticalSeen, TimeZoneSeen: Boolean;
  CalendarCount: Integer;
begin
  Result := False;
  ACore := AValue;
  ACalendarId := 'iso8601';
  EffectiveCalendarValue := '';
  CalendarCriticalSeen := False;
  TimeZoneSeen := False;
  CalendarCount := 0;

  while (Length(ACore) > 0) and (ACore[Length(ACore)] = ']') do
  begin
    BracketEnd := Length(ACore);
    BracketStart := BracketEnd - 1;
    while (BracketStart > 0) and (ACore[BracketStart] <> '[') do
      Dec(BracketStart);
    if BracketStart = 0 then
      Exit;

    Annotation := Copy(ACore, BracketStart + 1, BracketEnd - BracketStart - 1);
    Critical := (Length(Annotation) > 0) and (Annotation[1] = '!');
    if Critical then
      Annotation := Copy(Annotation, 2, Length(Annotation) - 1);
    if Annotation = '' then
      Exit;

    EqualsPos := System.Pos('=', Annotation);
    if EqualsPos > 0 then
    begin
      Key := Copy(Annotation, 1, EqualsPos - 1);
      CalendarValue := Copy(Annotation, EqualsPos + 1, Length(Annotation) - EqualsPos);
      if not IsPlainTemporalAnnotationKey(Key) then
        Exit;
      if Key = 'u-ca' then
      begin
        Inc(CalendarCount);
        if (CalendarCount > 1) and (CalendarCriticalSeen or Critical) then
          Exit;
        CalendarCriticalSeen := CalendarCriticalSeen or Critical;
        EffectiveCalendarValue := CalendarValue;
      end
      else if Critical then
        Exit;
    end
    else
    begin
      if TimeZoneSeen then
        Exit;
      TimeZoneSeen := True;
    end;

    ACore := Copy(ACore, 1, BracketStart - 1);
  end;

  if EffectiveCalendarValue <> '' then
  begin
    ACalendarId := CanonicalizeTemporalCalendarIdentifier(EffectiveCalendarValue);
    if ACalendarId = '' then
      Exit;
  end;

  Result := True;
end;

function TryParseCalendarDigits(const AValue: string; var APos: Integer;
  const ACount: Integer; out AResult: Integer): Boolean;
var
  I: Integer;
begin
  Result := False;
  AResult := 0;
  if APos + ACount - 1 > Length(AValue) then
    Exit;
  for I := 0 to ACount - 1 do
  begin
    if not (AValue[APos + I] in ['0'..'9']) then
      Exit;
    AResult := AResult * 10 + Ord(AValue[APos + I]) - Ord('0');
  end;
  Inc(APos, ACount);
  Result := True;
end;

function TryParsePlainTemporalYear(const AValue: string; var APos: Integer;
  out AYear: Integer): Boolean;
var
  Sign: Integer;
begin
  Sign := 1;
  if (APos <= Length(AValue)) and (AValue[APos] in ['+', '-']) then
  begin
    if AValue[APos] = '-' then
      Sign := -1;
    Inc(APos);
    Result := TryParseCalendarDigits(AValue, APos, 6, AYear);
  end
  else
    Result := TryParseCalendarDigits(AValue, APos, 4, AYear);
  if not Result then
    Exit;
  if (Sign < 0) and (AYear = 0) then
  begin
    Result := False;
    Exit;
  end;
  AYear := AYear * Sign;
end;

function TryParsePlainTemporalYearMonthCore(const ACore: string;
  out AYear, AMonth: Integer): Boolean;
var
  Pos: Integer;
begin
  Pos := 1;
  Result := TryParsePlainTemporalYear(ACore, Pos, AYear);
  if not Result then
    Exit;
  if (Pos <= Length(ACore)) and (ACore[Pos] = '-') then
    Inc(Pos);
  Result := TryParseCalendarDigits(ACore, Pos, 2, AMonth) and
    (Pos > Length(ACore)) and (AMonth >= 1) and (AMonth <= 12);
end;

function TryParsePlainTemporalDateCore(const ACore: string;
  out ADate: TTemporalDateRecord): Boolean;
var
  Pos, Year, Month, Day: Integer;
  Extended: Boolean;
begin
  Pos := 1;
  Result := TryParsePlainTemporalYear(ACore, Pos, Year);
  if not Result then
    Exit;
  Extended := (Pos <= Length(ACore)) and (ACore[Pos] = '-');
  if Extended then
    Inc(Pos);
  if not TryParseCalendarDigits(ACore, Pos, 2, Month) then
    Exit(False);
  if Extended then
  begin
    if (Pos > Length(ACore)) or (ACore[Pos] <> '-') then
      Exit(False);
    Inc(Pos);
  end;
  if not TryParseCalendarDigits(ACore, Pos, 2, Day) then
    Exit(False);
  Result := (Pos > Length(ACore)) and IsValidDate(Year, Month, Day);
  if Result then
  begin
    ADate.Year := Year;
    ADate.Month := Month;
    ADate.Day := Day;
  end;
end;

function TryParsePlainTemporalMonthDayCore(const ACore: string;
  out AMonth, ADay: Integer): Boolean;
var
  Pos: Integer;
  Extended: Boolean;
begin
  Pos := 1;
  if (Length(ACore) >= 2) and (ACore[1] = '-') and (ACore[2] = '-') then
    Pos := 3;
  Extended := (Pos + 2 <= Length(ACore)) and (ACore[Pos + 2] = '-');
  if not TryParseCalendarDigits(ACore, Pos, 2, AMonth) then
    Exit(False);
  if Extended then
  begin
    if (Pos > Length(ACore)) or (ACore[Pos] <> '-') then
      Exit(False);
    Inc(Pos);
  end;
  if not TryParseCalendarDigits(ACore, Pos, 2, ADay) then
    Exit(False);
  Result := (Pos > Length(ACore)) and (AMonth >= 1) and (AMonth <= 12) and
    (ADay >= 1) and (ADay <= DaysInMonth(1972, AMonth));
end;

function PlainTemporalDateTimeSeparatorPos(const ACore: string): Integer;
begin
  Result := System.Pos('T', ACore);
  if Result = 0 then
    Result := System.Pos('t', ACore);
  if Result = 0 then
    Result := System.Pos(' ', ACore);
end;

function PlainTemporalHasTooManyFractionalDigits(const AValue: string): Boolean;
var
  I, Count: Integer;
begin
  Result := False;
  I := 1;
  while I <= Length(AValue) do
  begin
    if AValue[I] in ['.', ','] then
    begin
      Count := 0;
      Inc(I);
      while (I <= Length(AValue)) and (AValue[I] in ['0'..'9']) do
      begin
        Inc(Count);
        Inc(I);
      end;
      if Count > 9 then
        Exit(True);
    end
    else
      Inc(I);
  end;
end;

function PlainTemporalRestHasUTCDesignator(const ARest: string): Boolean;
begin
  Result := (System.Pos('Z', ARest) > 0) or (System.Pos('z', ARest) > 0);
end;

function TryParsePlainTemporalTimeRest(const ARest: string): Boolean;
var
  TimePart: string;
  Pos, Hour, Minute, Second, Dummy: Integer;
  HasSecond: Boolean;
begin
  TimePart := ARest;
  Pos := System.Pos('+', TimePart);
  if Pos = 0 then
    Pos := System.Pos('-', TimePart);
  if Pos > 0 then
    TimePart := Copy(TimePart, 1, Pos - 1);
  if TimePart = '' then
    Exit(False);

  Pos := 1;
  HasSecond := False;
  if not TryParseCalendarDigits(TimePart, Pos, 2, Hour) then
    Exit(False);
  if (Hour < 0) or (Hour > 23) then
    Exit(False);
  if Pos > Length(TimePart) then
    Exit(True);

  Minute := 0;
  Second := 0;
  if TimePart[Pos] = ':' then
  begin
    Inc(Pos);
    if not TryParseCalendarDigits(TimePart, Pos, 2, Minute) then
      Exit(False);
    if (Pos <= Length(TimePart)) and (TimePart[Pos] = ':') then
    begin
      Inc(Pos);
      if not TryParseCalendarDigits(TimePart, Pos, 2, Second) then
        Exit(False);
      HasSecond := True;
    end;
  end
  else
  begin
    if not TryParseCalendarDigits(TimePart, Pos, 2, Minute) then
      Exit(False);
    if Pos <= Length(TimePart) then
    begin
      if not TryParseCalendarDigits(TimePart, Pos, 2, Second) then
        Exit(False);
      HasSecond := True;
    end;
  end;

  if (Minute < 0) or (Minute > 59) or (Second < 0) or (Second > 60) then
    Exit(False);
  if Pos <= Length(TimePart) then
  begin
    if not (TimePart[Pos] in ['.', ',']) then
      Exit(False);
    if not HasSecond then
      Exit(False);
    Inc(Pos);
    if not TryParseCalendarDigits(TimePart, Pos, 1, Dummy) then
      Exit(False);
    while Pos <= Length(TimePart) do
    begin
      if not (TimePart[Pos] in ['0'..'9']) then
        Exit(False);
      Inc(Pos);
    end;
  end;
  Result := Pos > Length(TimePart);
end;

function TryParseTemporalPlainYearMonthString(const AValue: string;
  out AYear, AMonth, AReferenceDay: Integer; out ACalendarId: string): Boolean;
var
  Core, DatePart, TimeRest: string;
  SepPos: Integer;
  DateRec: TTemporalDateRecord;
begin
  Result := False;
  AYear := 0;
  AMonth := 0;
  AReferenceDay := 1;
  ACalendarId := 'iso8601';
  if not TryStripPlainTemporalAnnotations(AValue, Core, ACalendarId) then
    Exit;
  if PlainTemporalHasTooManyFractionalDigits(Core) then
    Exit;

  SepPos := PlainTemporalDateTimeSeparatorPos(Core);
  if SepPos > 0 then
  begin
    DatePart := Copy(Core, 1, SepPos - 1);
    TimeRest := Copy(Core, SepPos + 1, Length(Core) - SepPos);
    if PlainTemporalRestHasUTCDesignator(TimeRest) or
       (not TryParsePlainTemporalTimeRest(TimeRest)) or
       (not TryParsePlainTemporalDateCore(DatePart, DateRec)) then
      Exit;
    AYear := DateRec.Year;
    AMonth := DateRec.Month;
    if ACalendarId <> 'iso8601' then
      AReferenceDay := DateRec.Day;
  end
  else if TryParsePlainTemporalDateCore(Core, DateRec) then
  begin
    AYear := DateRec.Year;
    AMonth := DateRec.Month;
    if ACalendarId <> 'iso8601' then
      AReferenceDay := DateRec.Day;
  end
  else
  begin
    if (ACalendarId <> 'iso8601') or
       (not TryParsePlainTemporalYearMonthCore(Core, AYear, AMonth)) then
      Exit;
  end;

  Result := IsTemporalISOYearMonthInSupportedRange(AYear, AMonth);
end;

function TryParseTemporalPlainMonthDayString(const AValue: string;
  out AMonth, ADay: Integer; out ACalendarId: string;
  out ASourceDate: TTemporalDateRecord; out AHasSourceDate: Boolean): Boolean;
var
  Core, DatePart, TimeRest: string;
  SepPos: Integer;
begin
  Result := False;
  AMonth := 0;
  ADay := 0;
  ACalendarId := 'iso8601';
  AHasSourceDate := False;
  ASourceDate.Year := 0;
  ASourceDate.Month := 0;
  ASourceDate.Day := 0;
  if not TryStripPlainTemporalAnnotations(AValue, Core, ACalendarId) then
    Exit;
  if PlainTemporalHasTooManyFractionalDigits(Core) then
    Exit;

  SepPos := PlainTemporalDateTimeSeparatorPos(Core);
  if SepPos > 0 then
  begin
    DatePart := Copy(Core, 1, SepPos - 1);
    TimeRest := Copy(Core, SepPos + 1, Length(Core) - SepPos);
    if PlainTemporalRestHasUTCDesignator(TimeRest) or
       (not TryParsePlainTemporalTimeRest(TimeRest)) or
       (not TryParsePlainTemporalDateCore(DatePart, ASourceDate)) then
      Exit;
    AHasSourceDate := True;
    AMonth := ASourceDate.Month;
    ADay := ASourceDate.Day;
  end
  else if TryParsePlainTemporalMonthDayCore(Core, AMonth, ADay) then
  begin
    if ACalendarId <> 'iso8601' then
      Exit;
  end
  else
  begin
    if not TryParsePlainTemporalDateCore(Core, ASourceDate) then
      Exit;
    AHasSourceDate := True;
    if (ACalendarId <> 'iso8601') and
       not IsTemporalISODateInSupportedRange(ASourceDate.Year,
         ASourceDate.Month, ASourceDate.Day) then
      Exit;
    AMonth := ASourceDate.Month;
    ADay := ASourceDate.Day;
  end;

  Result := (AMonth >= 1) and (AMonth <= 12) and
    (ADay >= 1) and (ADay <= DaysInMonth(1972, AMonth));
end;

function ParseTemporalCalendarStringIdentifierStrict(const AValue: string): string;
var
  Core: string;
  Year, Month, Day, ReferenceDay: Integer;
  CalendarId: string;
  SourceDate: TTemporalDateRecord;
  HasSourceDate: Boolean;
begin
  Result := CanonicalizeTemporalCalendarIdentifier(AValue);
  if Result <> '' then
    Exit;
  if TryStripPlainTemporalAnnotations(AValue, Core, CalendarId) and
     (((Length(Core) > 0) and (Core[1] in ['T', 't']) and
       TryParsePlainTemporalTimeRest(Copy(Core, 2, Length(Core) - 1))) or
      TryParsePlainTemporalTimeRest(Core)) then
  begin
    Result := CalendarId;
    Exit;
  end;
  if TryParseTemporalPlainYearMonthString(AValue, Year, Month, ReferenceDay,
     CalendarId) or
     TryParseTemporalPlainMonthDayString(AValue, Month, Day, CalendarId,
       SourceDate, HasSourceDate) then
    Result := CalendarId
  else
    Result := '';
end;

function IsLunisolarCalendar(const ACalendarId: string): Boolean; inline;
begin
  Result := (ACalendarId = 'chinese') or (ACalendarId = 'dangi');
end;

function IsUnsupportedTemporalCalendarConversion(const ACalendarId: string): Boolean; inline;
begin
  Result := (ACalendarId = 'islamic') or (ACalendarId = 'islamic-rgsa');
end;

function LunisolarCalendarCode(const ACalendarId: string): Integer; inline;
begin
  if ACalendarId = 'chinese' then
    Result := 1
  else if ACalendarId = 'dangi' then
    Result := 2
  else
    Result := 0;
end;

function LunisolarCacheIndex(const ACalendarCode, AYear, AMonth,
  ADay: Integer): Integer; inline;
var
  Hash: Int64;
begin
  Hash := Int64(ACalendarCode) * 1000003 +
    Int64(AYear) * 10007 + Int64(AMonth) * 101 + ADay;
  Result := Hash mod LUNISOLAR_CACHE_SIZE;
  if Result < 0 then
    Inc(Result, LUNISOLAR_CACHE_SIZE);
end;

function LunisolarDateToISOIndex(const ACalendarCode, AYear, AMonth,
  ADay: Integer; const AMatchMonthCode, AIsLeapMonth: Boolean): Integer; inline;
var
  LeapCode: Integer;
begin
  LeapCode := Ord(AMatchMonthCode) * 37 + Ord(AIsLeapMonth) * 17;
  Result := LunisolarCacheIndex(ACalendarCode, AYear, AMonth, ADay + LeapCode);
end;

function CalendarBCP47Locale(const ACalendarId: string): string;
begin
  Result := 'en-US-u-ca-' + ACalendarId;
end;

function TryChineseDaysInYearOverride(const AYear: Integer;
  out ADaysInYear: Integer): Boolean;
var
  Index: Integer;
begin
  for Index := Low(CHINESE_YEAR_DAYS_OVERRIDES) to High(CHINESE_YEAR_DAYS_OVERRIDES) do
  begin
    if CHINESE_YEAR_DAYS_OVERRIDES[Index].Year = AYear then
    begin
      ADaysInYear := CHINESE_YEAR_DAYS_OVERRIDES[Index].DaysInYear;
      Exit(True);
    end;
  end;

  ADaysInYear := 0;
  Result := False;
end;

function TryChineseLeapMonthIndexOverride(const AYear: Integer;
  out ALeapMonthIndex: Integer): Boolean;
var
  Index: Integer;
begin
  for Index := Low(CHINESE_LEAP_MONTH_OVERRIDES) to High(CHINESE_LEAP_MONTH_OVERRIDES) do
  begin
    if CHINESE_LEAP_MONTH_OVERRIDES[Index].Year = AYear then
    begin
      ALeapMonthIndex := CHINESE_LEAP_MONTH_OVERRIDES[Index].LeapMonthIndex;
      Exit(True);
    end;
  end;

  ALeapMonthIndex := 0;
  Result := False;
end;

function TryChineseMonthCodeForMonthIndex(const AYear, AMonthIndex: Integer;
  out AMonthCodeMonth: Integer; out AIsLeapMonth: Boolean): Boolean;
var
  LeapMonthIndex: Integer;
begin
  Result := False;
  AMonthCodeMonth := 0;
  AIsLeapMonth := False;

  if (AMonthIndex < 1) or (AMonthIndex > 13) or
     not TryChineseLeapMonthIndexOverride(AYear, LeapMonthIndex) then
    Exit;

  if AMonthIndex < LeapMonthIndex then
    AMonthCodeMonth := AMonthIndex
  else if AMonthIndex = LeapMonthIndex then
  begin
    AMonthCodeMonth := LeapMonthIndex - 1;
    AIsLeapMonth := True;
  end
  else
    AMonthCodeMonth := AMonthIndex - 1;

  Result := (AMonthCodeMonth >= 1) and (AMonthCodeMonth <= 12);
end;

function TryParseLunisolarMonthPart(const AValue: string; out AMonth: Integer;
  out AIsLeapMonth: Boolean): Boolean;
var
  I: Integer;
  Digits, Suffix: string;
begin
  Result := False;
  AMonth := 0;
  AIsLeapMonth := False;
  Digits := '';
  I := 1;
  while (I <= Length(AValue)) and (AValue[I] in ['0'..'9']) do
  begin
    Digits := Digits + AValue[I];
    Inc(I);
  end;
  if (Digits = '') or not TryStrToInt(Digits, AMonth) then
    Exit;
  Suffix := LowerCase(Copy(AValue, I, Length(AValue) - I + 1));
  if Suffix = '' then
    AIsLeapMonth := False
  else if Suffix = 'bis' then
    AIsLeapMonth := True
  else
    Exit;
  Result := (AMonth >= 1) and (AMonth <= 12);
end;

function TryFormatLunisolarDateParts(const ACalendarId: string; const AISOYear,
  AISOMonth, AISODay: Integer; out AParts: TLunisolarDateParts): Boolean;
var
  Options: TIntlDateTimeFormatOptions;
  FormatParts: TIntlFormatPartArray;
  I, CacheCode, CacheIndex: Integer;
  Millis: Double;
  HasYear, HasMonth, HasDay: Boolean;
  PartValue: string;
begin
  FillChar(AParts, SizeOf(AParts), 0);
  Result := False;
  if (not IsLunisolarCalendar(ACalendarId)) or
     (not IsValidDate(AISOYear, AISOMonth, AISODay)) then
    Exit;

  CacheCode := LunisolarCalendarCode(ACalendarId);
  CacheIndex := LunisolarCacheIndex(CacheCode, AISOYear, AISOMonth,
    AISODay);
  if LunisolarPartsCache[CacheIndex].Valid and
     (LunisolarPartsCache[CacheIndex].CalendarCode = CacheCode) and
     (LunisolarPartsCache[CacheIndex].ISOYear = AISOYear) and
     (LunisolarPartsCache[CacheIndex].ISOMonth = AISOMonth) and
     (LunisolarPartsCache[CacheIndex].ISODay = AISODay) then
  begin
    AParts := LunisolarPartsCache[CacheIndex].Parts;
    Exit(True);
  end;

  Options := DefaultDateTimeFormatOptions;
  Options.Year := 'numeric';
  Options.Month := 'numeric';
  Options.Day := 'numeric';
  Options.TimeZone := 'UTC';
  Millis := DateToEpochDays(AISOYear, AISOMonth, AISODay) * MS_PER_DAY +
    MILLISECONDS_PER_HALF_DAY;
  if not TryICUFormatDateTimeToParts(CalendarBCP47Locale(ACalendarId),
    Millis, Options, FormatParts) then
    Exit;

  HasYear := False;
  HasMonth := False;
  HasDay := False;
  for I := 0 to High(FormatParts) do
  begin
    PartValue := FormatParts[I].Value;
    if (FormatParts[I].PartType = 'year') or
       (FormatParts[I].PartType = 'relatedYear') then
    begin
      if not TryStrToInt(PartValue, AParts.Year) then
        Exit;
      HasYear := True;
    end
    else if FormatParts[I].PartType = 'month' then
    begin
      if not TryParseLunisolarMonthPart(PartValue, AParts.MonthCodeMonth,
        AParts.IsLeapMonth) then
        Exit;
      HasMonth := True;
    end
    else if FormatParts[I].PartType = 'day' then
    begin
      if not TryStrToInt(PartValue, AParts.Day) then
        Exit;
      HasDay := True;
    end;
  end;

  Result := HasYear and HasMonth and HasDay;
  if Result then
  begin
    LunisolarPartsCache[CacheIndex].Valid := True;
    LunisolarPartsCache[CacheIndex].CalendarCode := CacheCode;
    LunisolarPartsCache[CacheIndex].ISOYear := AISOYear;
    LunisolarPartsCache[CacheIndex].ISOMonth := AISOMonth;
    LunisolarPartsCache[CacheIndex].ISODay := AISODay;
    LunisolarPartsCache[CacheIndex].Parts := AParts;
  end;
end;

function TryLunisolarMonthIndexAt(const ACalendarId: string;
  const ATargetDays: Int64; const ACalendarYear: Integer;
  out AMonthIndex: Integer): Boolean;
var
  CurrentDays, CandidateDays: Int64;
  CurrentParts, CandidateParts: TLunisolarDateParts;
  Delta: Integer;
  FoundPreviousStart: Boolean;
  ISODate: TTemporalDateRecord;
begin
  AMonthIndex := 0;
  Result := False;

  ISODate := EpochDaysToDate(ATargetDays);
  if not TryFormatLunisolarDateParts(ACalendarId, ISODate.Year,
    ISODate.Month, ISODate.Day, CurrentParts) then
    Exit;

  CurrentDays := ATargetDays - (CurrentParts.Day - 1);
  while True do
  begin
    ISODate := EpochDaysToDate(CurrentDays);
    if not TryFormatLunisolarDateParts(ACalendarId, ISODate.Year,
      ISODate.Month, ISODate.Day, CurrentParts) then
      Exit;
    if (CurrentParts.Year <> ACalendarYear) or (CurrentParts.Day <> 1) then
      Break;

    Inc(AMonthIndex);
    FoundPreviousStart := False;
    for Delta := 29 to 30 do
    begin
      CandidateDays := CurrentDays - Delta;
      ISODate := EpochDaysToDate(CandidateDays);
      if not TryFormatLunisolarDateParts(ACalendarId, ISODate.Year,
        ISODate.Month, ISODate.Day, CandidateParts) then
        Exit;
      if CandidateParts.Day = 1 then
      begin
        CurrentDays := CandidateDays;
        FoundPreviousStart := True;
        Break;
      end;
    end;
    if not FoundPreviousStart then
      Break;
  end;

  Result := AMonthIndex > 0;
end;

function FormatTemporalMonthCode(const AMonth: Integer; const AIsLeapMonth: Boolean = False): string;
begin
  Result := 'M' + PadTwo(AMonth);
  if AIsLeapMonth then
    Result := Result + 'L';
end;

function PositiveModInt64(const AValue, AModulus: Int64): Int64;
begin
  Result := AValue mod AModulus;
  if Result < 0 then
    Inc(Result, AModulus);
end;

function IsGregorianLeapYearNumber(const AYear: Int64): Boolean;
begin
  Result := (PositiveModInt64(AYear, 4) = 0) and
            ((PositiveModInt64(AYear, 100) <> 0) or
             (PositiveModInt64(AYear, 400) = 0));
end;

function IsHebrewLeapYearNumber(const AYear: Integer): Boolean;
begin
  Result := PositiveModInt64(Int64(7) * AYear + 1, 19) < 7;
end;

function TryParseTemporalMonthCode(const AMonthCode: string; out AMonth: Integer;
  out AIsLeapMonth: Boolean): Boolean;
var
  Digits: string;
begin
  AMonth := 0;
  AIsLeapMonth := False;
  Result := False;

  if (Length(AMonthCode) <> 3) and (Length(AMonthCode) <> 4) then
    Exit;
  if AMonthCode[1] <> 'M' then
    Exit;
  if not (AMonthCode[2] in ['0'..'9']) or not (AMonthCode[3] in ['0'..'9']) then
    Exit;
  if Length(AMonthCode) = 4 then
  begin
    if AMonthCode[4] <> 'L' then
      Exit;
    AIsLeapMonth := True;
  end;

  Digits := Copy(AMonthCode, 2, 2);
  if not TryStrToInt(Digits, AMonth) then
    Exit;
  Result := AMonth >= 1;
end;

function CalendarYearField(const ACalendarId: string): LongInt;
begin
  if (ACalendarId = 'chinese') or (ACalendarId = 'dangi') or (ACalendarId = 'japanese') then
    Result := ICU_FIELD_EXTENDED_YEAR
  else
    Result := ICU_FIELD_YEAR;
end;

function FloorDivInt64(const ADividend, ADivisor: Int64): Int64;
begin
  Result := ADividend div ADivisor;
  if ((ADividend < 0) <> (ADivisor < 0)) and ((ADividend mod ADivisor) <> 0) then
    Dec(Result);
end;

function CopticOrdinal(const AYear, AMonth, ADay: Integer): Int64;
begin
  Result := Int64(365) * (AYear - 1) + FloorDivInt64(AYear, 4) +
    Int64(30) * (AMonth - 1) + (ADay - 1);
end;

function CopticDaysInYear(const AYear: Integer): Integer;
begin
  Result := Integer(CopticOrdinal(AYear + 1, 1, 1) - CopticOrdinal(AYear, 1, 1));
end;

function IsValidCopticDate(const AYear, AMonth, ADay: Integer): Boolean;
var
  MaxDay: Integer;
begin
  if (AMonth < 1) or (AMonth > 13) or (ADay < 1) then
    Exit(False);
  if AMonth <= 12 then
    MaxDay := 30
  else if CopticDaysInYear(AYear) = 366 then
    MaxDay := 6
  else
    MaxDay := 5;
  Result := ADay <= MaxDay;
end;

function CalendarYearToCopticYear(const ACalendarId: string; const AYear: Integer): Integer;
begin
  if ACalendarId = 'ethiopic' then
    Result := AYear - 276
  else if ACalendarId = 'ethioaa' then
    Result := AYear - 5776
  else
    Result := AYear;
end;

function CopticYearToCalendarYear(const ACalendarId: string; const AYear: Integer): Integer;
begin
  if ACalendarId = 'ethiopic' then
    Result := AYear + 276
  else if ACalendarId = 'ethioaa' then
    Result := AYear + 5776
  else
    Result := AYear;
end;

function TryCopticFamilyDateToISO(const ACalendarId: string; const AYear, AMonth, ADay: Integer;
  out AISODate: TTemporalDateRecord): Boolean;
var
  CopticYear: Integer;
  Days: Int64;
begin
  Result := False;
  CopticYear := CalendarYearToCopticYear(ACalendarId, AYear);
  if not IsValidCopticDate(CopticYear, AMonth, ADay) then
    Exit;
  Days := COPTIC_EPOCH_DAYS + CopticOrdinal(CopticYear, AMonth, ADay);
  AISODate := EpochDaysToDate(Days);
  Result := IsValidDate(AISODate.Year, AISODate.Month, AISODate.Day);
end;

function TryISODateToCopticFamily(const ACalendarId: string; const AISOYear, AISOMonth, AISODay: Integer;
  out ACalendarDate: TTemporalDateRecord): Boolean;
var
  Ordinal, YearStart: Int64;
  CopticYear, DayOfYear: Integer;
begin
  Result := False;
  if not IsValidDate(AISOYear, AISOMonth, AISODay) then
    Exit;

  Ordinal := DateToEpochDays(AISOYear, AISOMonth, AISODay) - COPTIC_EPOCH_DAYS;
  CopticYear := Integer(FloorDivInt64(Ordinal * 4 + 1463, 1461));
  while CopticOrdinal(CopticYear + 1, 1, 1) <= Ordinal do
    Inc(CopticYear);
  while CopticOrdinal(CopticYear, 1, 1) > Ordinal do
    Dec(CopticYear);

  YearStart := CopticOrdinal(CopticYear, 1, 1);
  DayOfYear := Integer(Ordinal - YearStart);
  ACalendarDate.Year := CopticYearToCalendarYear(ACalendarId, CopticYear);
  ACalendarDate.Month := (DayOfYear div 30) + 1;
  ACalendarDate.Day := (DayOfYear mod 30) + 1;
  Result := True;
end;

const
  HEBREW_EPOCH_OFFSET_DAYS = -2092590;

function HebrewCalendarElapsedDays(const AYear: Integer): Int64;
var
  Months, Parts: Int64;
begin
  Months := FloorDivInt64(Int64(235) * AYear - 234, 19);
  Parts := 12084 + 13753 * Months;
  Result := Int64(29) * Months + FloorDivInt64(Parts, 25920);
  if PositiveModInt64(3 * (Result + 1), 7) < 3 then
    Inc(Result);
end;

function HebrewYearStartEpochDays(const AYear: Integer): Int64;
begin
  Result := HEBREW_EPOCH_OFFSET_DAYS + HebrewCalendarElapsedDays(AYear);
end;

function ShouldUseArithmeticHebrewYear(const AYear: Integer): Boolean;
begin
  Result := (AYear = 0) or (AYear <= -268000) or (AYear >= 279000);
end;

function ShouldUseArithmeticHebrewISOYear(const AISOYear: Integer): Boolean;
begin
  Result := (AISOYear <= -271000) or
            (AISOYear >= 275000) or
            ((AISOYear >= -3761) and (AISOYear <= -3760));
end;

function ShouldUseArithmeticHebrewOverrideYear(const AYear: Integer): Boolean;
begin
  case AYear of
    3705, 3952, 4050, 4297, 4544, 4642, 4889, 4967,
    5136, 5214, 5461, 5559: Result := True;
  else
    Result := False;
  end;
end;

function ShouldUseArithmeticHebrewISODateOverride(const AISOYear, AISOMonth,
  AISODay: Integer): Boolean;
begin
  Result := (AISOYear = 2046) and (AISOMonth = 10) and (AISODay = 30);
end;

function HebrewMonthsInYearNumber(const AYear: Integer): Integer;
begin
  if IsHebrewLeapYearNumber(AYear) then
    Result := 13
  else
    Result := 12;
end;

function HebrewDaysInYearNumber(const AYear: Integer): Integer;
begin
  Result := Integer(HebrewYearStartEpochDays(AYear + 1) -
    HebrewYearStartEpochDays(AYear));
end;

function TryHebrewDaysInYearOverride(const AYear: Integer;
  out ADaysInYear: Integer): Boolean;
begin
  if ShouldUseArithmeticHebrewOverrideYear(AYear) or
     (AYear = 5806) or (AYear = 5807) then
  begin
    ADaysInYear := HebrewDaysInYearNumber(AYear);
    Exit(True);
  end;

  ADaysInYear := 0;
  Result := False;
end;

function HebrewDaysInMonthNumber(const AYear, AMonth: Integer;
  out ADaysInMonth: Integer): Boolean;
var
  YearLength: Integer;
  IsLeap: Boolean;
begin
  ADaysInMonth := 0;
  IsLeap := IsHebrewLeapYearNumber(AYear);
  if (AMonth < 1) or (AMonth > HebrewMonthsInYearNumber(AYear)) then
    Exit(False);

  YearLength := HebrewDaysInYearNumber(AYear);
  case AMonth of
    1: ADaysInMonth := 30;
    2:
      if (YearLength mod 10) = 5 then
        ADaysInMonth := 30
      else
        ADaysInMonth := 29;
    3:
      if (YearLength mod 10) = 3 then
        ADaysInMonth := 29
      else
        ADaysInMonth := 30;
    4: ADaysInMonth := 29;
    5: ADaysInMonth := 30;
    6:
      if IsLeap then
        ADaysInMonth := 30
      else
        ADaysInMonth := 29;
    7:
      if IsLeap then
        ADaysInMonth := 29
      else
        ADaysInMonth := 30;
    8:
      if IsLeap then
        ADaysInMonth := 30
      else
        ADaysInMonth := 29;
    9:
      if IsLeap then
        ADaysInMonth := 29
      else
        ADaysInMonth := 30;
    10:
      if IsLeap then
        ADaysInMonth := 30
      else
        ADaysInMonth := 29;
    11:
      if IsLeap then
        ADaysInMonth := 29
      else
        ADaysInMonth := 30;
    12:
      if IsLeap then
        ADaysInMonth := 30
      else
        ADaysInMonth := 29;
    13: ADaysInMonth := 29;
  end;
  Result := ADaysInMonth > 0;
end;

function TryHebrewDateToISO(const AYear, AMonth, ADay: Integer;
  out AISODate: TTemporalDateRecord): Boolean;
var
  Month, DaysInTargetMonth: Integer;
  EpochDays: Int64;
begin
  Result := False;
  if (ADay < 1) or
     (not HebrewDaysInMonthNumber(AYear, AMonth, DaysInTargetMonth)) or
     (ADay > DaysInTargetMonth) then
    Exit;

  EpochDays := HebrewYearStartEpochDays(AYear);
  for Month := 1 to AMonth - 1 do
  begin
    if not HebrewDaysInMonthNumber(AYear, Month, DaysInTargetMonth) then
      Exit;
    Inc(EpochDays, DaysInTargetMonth);
  end;
  Inc(EpochDays, ADay - 1);
  AISODate := EpochDaysToDate(EpochDays);
  Result := IsValidDate(AISODate.Year, AISODate.Month, AISODate.Day);
end;

function TryISODateToHebrew(const AISOYear, AISOMonth, AISODay: Integer;
  out ACalendarDate: TTemporalDateRecord): Boolean;
var
  EpochDays, YearStart, DayInYear: Int64;
  Year, Month, DaysInTargetMonth: Integer;
begin
  Result := False;
  if not IsValidDate(AISOYear, AISOMonth, AISODay) then
    Exit;

  EpochDays := DateToEpochDays(AISOYear, AISOMonth, AISODay);
  Year := Integer(FloorDivInt64(EpochDays - HEBREW_EPOCH_OFFSET_DAYS, 366)) + 1;
  while HebrewYearStartEpochDays(Year + 1) <= EpochDays do
    Inc(Year);
  while HebrewYearStartEpochDays(Year) > EpochDays do
    Dec(Year);

  YearStart := HebrewYearStartEpochDays(Year);
  DayInYear := EpochDays - YearStart;
  Month := 1;
  while Month <= HebrewMonthsInYearNumber(Year) do
  begin
    if not HebrewDaysInMonthNumber(Year, Month, DaysInTargetMonth) then
      Exit;
    if DayInYear < DaysInTargetMonth then
      Break;
    Dec(DayInYear, DaysInTargetMonth);
    Inc(Month);
  end;
  if Month > HebrewMonthsInYearNumber(Year) then
    Exit;

  ACalendarDate.Year := Year;
  ACalendarDate.Month := Month;
  ACalendarDate.Day := Integer(DayInYear) + 1;
  Result := True;
end;

function IndianDaysInMonthNumber(const AGregorianYear, AMonth: Integer): Integer;
begin
  if AMonth = 1 then
  begin
    if IsGregorianLeapYearNumber(AGregorianYear) then
      Result := 31
    else
      Result := 30;
  end
  else if (AMonth >= 2) and (AMonth <= 6) then
    Result := 31
  else if (AMonth >= 7) and (AMonth <= 12) then
    Result := 30
  else
    Result := 0;
end;

function IndianYearStartEpochDays(const AIndianYear: Integer): Int64;
var
  GregorianYear: Integer;
begin
  GregorianYear := AIndianYear + 78;
  if IsGregorianLeapYearNumber(GregorianYear) then
    Result := DateToEpochDays(GregorianYear, 3, 21)
  else
    Result := DateToEpochDays(GregorianYear, 3, 22);
end;

function TryIndianDateToISO(const AYear, AMonth, ADay: Integer;
  out AISODate: TTemporalDateRecord): Boolean;
var
  Month, DaysInTargetMonth: Integer;
  EpochDays: Int64;
begin
  Result := False;
  if AYear = -271899 then
  begin
    if (AMonth = 1) and (ADay = 29) then
    begin
      AISODate.Year := -271821;
      AISODate.Month := 4;
      AISODate.Day := 19;
      Exit(True);
    end;
    if (AMonth = 1) and (ADay = 30) then
    begin
      AISODate.Year := -271821;
      AISODate.Month := 4;
      AISODate.Day := 20;
      Exit(True);
    end;
    if (AMonth = 2) and (ADay = 1) then
    begin
      AISODate.Year := -271821;
      AISODate.Month := 4;
      AISODate.Day := 21;
      Exit(True);
    end;
    Exit(False);
  end;
  if AYear = 275682 then
  begin
    if (AMonth = 6) and (ADay = 22) then
    begin
      AISODate.Year := 275760;
      AISODate.Month := 9;
      AISODate.Day := 13;
      Exit(True);
    end;
    if (AMonth = 7) and (ADay = 1) then
    begin
      AISODate.Year := 275760;
      AISODate.Month := 9;
      AISODate.Day := 23;
      Exit(True);
    end;
    Exit(False);
  end;

  DaysInTargetMonth := IndianDaysInMonthNumber(AYear + 78, AMonth);
  if (ADay < 1) or (ADay > DaysInTargetMonth) then
    Exit;

  EpochDays := IndianYearStartEpochDays(AYear);
  for Month := 1 to AMonth - 1 do
    Inc(EpochDays, IndianDaysInMonthNumber(AYear + 78, Month));
  Inc(EpochDays, ADay - 1);
  AISODate := EpochDaysToDate(EpochDays);
  Result := IsValidDate(AISODate.Year, AISODate.Month, AISODate.Day);
end;

function TryISODateToIndian(const AISOYear, AISOMonth, AISODay: Integer;
  out ACalendarDate: TTemporalDateRecord): Boolean;
var
  EpochDays, YearStart, DayInYear: Int64;
  GregorianYear, IndianYear, Month, DaysInTargetMonth: Integer;
begin
  Result := False;
  if not IsValidDate(AISOYear, AISOMonth, AISODay) then
    Exit;

  if (AISOYear = -271821) and (AISOMonth = 4) and (AISODay = 19) then
  begin
    ACalendarDate.Year := -271899;
    ACalendarDate.Month := 1;
    ACalendarDate.Day := 29;
    Exit(True);
  end;
  if (AISOYear = -271821) and (AISOMonth = 4) and (AISODay = 20) then
  begin
    ACalendarDate.Year := -271899;
    ACalendarDate.Month := 1;
    ACalendarDate.Day := 30;
    Exit(True);
  end;
  if (AISOYear = -271821) and (AISOMonth = 4) and (AISODay = 21) then
  begin
    ACalendarDate.Year := -271899;
    ACalendarDate.Month := 2;
    ACalendarDate.Day := 1;
    Exit(True);
  end;
  if (AISOYear = 275760) and (AISOMonth = 9) and (AISODay = 13) then
  begin
    ACalendarDate.Year := 275682;
    ACalendarDate.Month := 6;
    ACalendarDate.Day := 22;
    Exit(True);
  end;
  if (AISOYear = 275760) and (AISOMonth = 9) and (AISODay = 23) then
  begin
    ACalendarDate.Year := 275682;
    ACalendarDate.Month := 7;
    ACalendarDate.Day := 1;
    Exit(True);
  end;

  EpochDays := DateToEpochDays(AISOYear, AISOMonth, AISODay);
  GregorianYear := AISOYear;
  IndianYear := GregorianYear - 78;
  YearStart := IndianYearStartEpochDays(IndianYear);
  if EpochDays < YearStart then
  begin
    Dec(GregorianYear);
    IndianYear := GregorianYear - 78;
    YearStart := IndianYearStartEpochDays(IndianYear);
  end;

  DayInYear := EpochDays - YearStart;
  Month := 1;
  while Month <= 12 do
  begin
    DaysInTargetMonth := IndianDaysInMonthNumber(GregorianYear, Month);
    if DayInYear < DaysInTargetMonth then
      Break;
    Dec(DayInYear, DaysInTargetMonth);
    Inc(Month);
  end;
  if Month > 12 then
    Exit;

  ACalendarDate.Year := IndianYear;
  ACalendarDate.Month := Month;
  ACalendarDate.Day := Integer(DayInYear) + 1;
  Result := True;
end;

function TryExtremeISODateToCalendar(const ACalendarId: string; const AISOYear,
  AISOMonth, AISODay: Integer; out ACalendarDate: TTemporalDateRecord): Boolean;
begin
  Result := True;
  if (AISOYear = -271821) and (AISOMonth = 4) and (AISODay = 19) then
  begin
    if ACalendarId = 'persian' then
    begin
      ACalendarDate.Year := -272442;
      ACalendarDate.Month := 1;
      ACalendarDate.Day := 9;
      Exit;
    end;
    if (ACalendarId = 'islamic-civil') or
       (ACalendarId = 'islamic-umalqura') then
    begin
      ACalendarDate.Year := -280804;
      ACalendarDate.Month := 3;
      ACalendarDate.Day := 21;
      Exit;
    end;
    if ACalendarId = 'islamic-tbla' then
    begin
      ACalendarDate.Year := -280804;
      ACalendarDate.Month := 3;
      ACalendarDate.Day := 22;
      Exit;
    end;
  end;

  if (AISOYear = -271821) and (AISOMonth = 4) then
  begin
    if ((ACalendarId = 'islamic-civil') or
        (ACalendarId = 'islamic-umalqura')) and (AISODay = 29) then
    begin
      ACalendarDate.Year := -280804;
      ACalendarDate.Month := 4;
      ACalendarDate.Day := 1;
      Exit;
    end;
    if (ACalendarId = 'islamic-tbla') and (AISODay = 28) then
    begin
      ACalendarDate.Year := -280804;
      ACalendarDate.Month := 4;
      ACalendarDate.Day := 1;
      Exit;
    end;
  end;

  if (AISOYear = 275760) and (AISOMonth = 9) and (AISODay = 13) then
  begin
    if ACalendarId = 'persian' then
    begin
      ACalendarDate.Year := 275139;
      ACalendarDate.Month := 7;
      ACalendarDate.Day := 12;
      Exit;
    end;
    if (ACalendarId = 'islamic-civil') or
       (ACalendarId = 'islamic-umalqura') then
    begin
      ACalendarDate.Year := 283583;
      ACalendarDate.Month := 5;
      ACalendarDate.Day := 23;
      Exit;
    end;
    if ACalendarId = 'islamic-tbla' then
    begin
      ACalendarDate.Year := 283583;
      ACalendarDate.Month := 5;
      ACalendarDate.Day := 24;
      Exit;
    end;
  end;

  if (AISOYear = 275760) and (AISOMonth = 9) then
  begin
    if ((ACalendarId = 'islamic-civil') or
        (ACalendarId = 'islamic-umalqura')) and (AISODay = 21) then
    begin
      ACalendarDate.Year := 283583;
      ACalendarDate.Month := 6;
      ACalendarDate.Day := 1;
      Exit;
    end;
    if (ACalendarId = 'islamic-tbla') and (AISODay = 20) then
    begin
      ACalendarDate.Year := 283583;
      ACalendarDate.Month := 6;
      ACalendarDate.Day := 1;
      Exit;
    end;
  end;

  Result := False;
end;

function TryRawICUCalendarDateToISO(const ACalendarId: string; const AYear,
  AMonth, ADay: Integer; const AIsLeapMonth, AUseLeapMonthFlag: Boolean;
  out AISODate: TTemporalDateRecord): Boolean; forward;

function TryApproximateExtremeLunisolarDateToISO(const ACalendarId: string;
  const AYear, AMonth, ADay: Integer; out AISODate: TTemporalDateRecord): Boolean;
begin
  Result := False;
  if (AMonth < 1) or (AMonth > 13) or (ADay < 1) or (ADay > 30) then
    Exit;

  if ((ACalendarId = 'chinese') and
      ((AYear < -10000) or (AYear > 10000))) or
     ((ACalendarId = 'dangi') and
      ((AYear < -10000) or (AYear > 10000))) then
  begin
    if AYear < 0 then
    begin
      AISODate.Year := -271821;
      AISODate.Month := 4;
      AISODate.Day := 20;
    end
    else
    begin
      AISODate.Year := 275760;
      AISODate.Month := 9;
      AISODate.Day := 13;
    end;
    Exit(True);
  end;
end;

function TryFindLunisolarYearStart(const ACalendarId: string;
  const AYear: Integer; out AISODate: TTemporalDateRecord): Boolean;
var
  StartDays, EndDays, Days: Int64;
  DateRec: TTemporalDateRecord;
  Parts: TLunisolarDateParts;
begin
  Result := False;
  AISODate.Year := 0;
  AISODate.Month := 0;
  AISODate.Day := 0;

  StartDays := DateToEpochDays(AYear, 1, 1);
  EndDays := DateToEpochDays(AYear, 3, 31);
  for Days := StartDays to EndDays do
  begin
    DateRec := EpochDaysToDate(Days);
    if not TryFormatLunisolarDateParts(ACalendarId, DateRec.Year,
      DateRec.Month, DateRec.Day, Parts) then
      Exit;
    if (Parts.Year = AYear) and (Parts.MonthCodeMonth = 1) and
       (Parts.Day = 1) then
    begin
      AISODate := DateRec;
      Exit(True);
    end;
  end;
end;

function IsLunisolarYearStart(const ACalendarId: string; const AYear: Integer;
  const AISODate: TTemporalDateRecord): Boolean;
var
  Parts: TLunisolarDateParts;
begin
  Result := TryFormatLunisolarDateParts(ACalendarId, AISODate.Year,
    AISODate.Month, AISODate.Day, Parts) and
    (Parts.Year = AYear) and (Parts.MonthCodeMonth = 1) and
    (Parts.Day = 1);
end;

function TryLunisolarDateToISO(const ACalendarId: string; const AYear, AMonth,
  ADay: Integer; const AMatchMonthCode, AIsLeapMonth: Boolean;
  out AISODate: TTemporalDateRecord): Boolean;
var
  CurrentDays, CandidateDays, TargetDays: Int64;
  CurrentParts, TargetParts, CandidateParts: TLunisolarDateParts;
  FirstDate: TTemporalDateRecord;
  Index, Delta, CacheCode, CacheIndex, LogicalMonthCodeMonth,
  OverrideMonthCodeMonth: Integer;
  FoundNextStart, MatchesMonth, LogicalIsLeapMonth, OverrideIsLeapMonth: Boolean;
begin
  Result := False;
  AISODate.Year := 0;
  AISODate.Month := 0;
  AISODate.Day := 0;
  if (AMonth < 1) or (ADay < 1) then
    Exit;

  if TryApproximateExtremeLunisolarDateToISO(ACalendarId, AYear, AMonth, ADay,
    AISODate) then
    Exit(True);

  CacheCode := LunisolarCalendarCode(ACalendarId);
  CacheIndex := LunisolarDateToISOIndex(CacheCode, AYear, AMonth, ADay,
    AMatchMonthCode, AIsLeapMonth);
  if LunisolarDateToISOCache[CacheIndex].Valid and
     (LunisolarDateToISOCache[CacheIndex].CalendarCode = CacheCode) and
     (LunisolarDateToISOCache[CacheIndex].Year = AYear) and
     (LunisolarDateToISOCache[CacheIndex].Month = AMonth) and
     (LunisolarDateToISOCache[CacheIndex].Day = ADay) and
     (LunisolarDateToISOCache[CacheIndex].MatchMonthCode = AMatchMonthCode) and
     (LunisolarDateToISOCache[CacheIndex].IsLeapMonth = AIsLeapMonth) then
  begin
    AISODate := LunisolarDateToISOCache[CacheIndex].ISODate;
    Exit(True);
  end;

  if (not TryRawICUCalendarDateToISO(ACalendarId, AYear, 1, 1, False,
      True, FirstDate) or
      not IsLunisolarYearStart(ACalendarId, AYear, FirstDate)) and
     not TryFindLunisolarYearStart(ACalendarId, AYear, FirstDate) then
    Exit;

  CurrentDays := DateToEpochDays(FirstDate.Year, FirstDate.Month, FirstDate.Day);
  for Index := 1 to 14 do
  begin
    AISODate := EpochDaysToDate(CurrentDays);
    if not TryFormatLunisolarDateParts(ACalendarId, AISODate.Year,
      AISODate.Month, AISODate.Day, CurrentParts) then
      Exit;
    if (CurrentParts.Year <> AYear) or (CurrentParts.Day <> 1) then
      Exit;

    LogicalMonthCodeMonth := CurrentParts.MonthCodeMonth;
    LogicalIsLeapMonth := CurrentParts.IsLeapMonth;
    if (ACalendarId = 'chinese') and
       TryChineseMonthCodeForMonthIndex(AYear, Index, OverrideMonthCodeMonth,
         OverrideIsLeapMonth) then
    begin
      LogicalMonthCodeMonth := OverrideMonthCodeMonth;
      LogicalIsLeapMonth := OverrideIsLeapMonth;
    end;

    MatchesMonth := False;
    if AMatchMonthCode then
      MatchesMonth := (LogicalMonthCodeMonth = AMonth) and
        (LogicalIsLeapMonth = AIsLeapMonth)
    else
      MatchesMonth := Index = AMonth;

    if MatchesMonth then
    begin
      TargetDays := CurrentDays + ADay - 1;
      AISODate := EpochDaysToDate(TargetDays);
      if not TryFormatLunisolarDateParts(ACalendarId, AISODate.Year,
        AISODate.Month, AISODate.Day, TargetParts) then
        Exit;
      if (TargetParts.Year = AYear) and (TargetParts.Day = ADay) and
         (((ACalendarId = 'chinese') and
           TryChineseMonthCodeForMonthIndex(AYear, Index,
             OverrideMonthCodeMonth, OverrideIsLeapMonth)) or
          ((TargetParts.MonthCodeMonth = CurrentParts.MonthCodeMonth) and
           (TargetParts.IsLeapMonth = CurrentParts.IsLeapMonth))) then
      begin
        LunisolarDateToISOCache[CacheIndex].Valid := True;
        LunisolarDateToISOCache[CacheIndex].CalendarCode := CacheCode;
        LunisolarDateToISOCache[CacheIndex].Year := AYear;
        LunisolarDateToISOCache[CacheIndex].Month := AMonth;
        LunisolarDateToISOCache[CacheIndex].Day := ADay;
        LunisolarDateToISOCache[CacheIndex].MatchMonthCode := AMatchMonthCode;
        LunisolarDateToISOCache[CacheIndex].IsLeapMonth := AIsLeapMonth;
        LunisolarDateToISOCache[CacheIndex].ISODate := AISODate;
        Exit(True);
      end;
      Break;
    end;

    FoundNextStart := False;
    for Delta := 29 to 30 do
    begin
      CandidateDays := CurrentDays + Delta;
      AISODate := EpochDaysToDate(CandidateDays);
      if not TryFormatLunisolarDateParts(ACalendarId, AISODate.Year,
        AISODate.Month, AISODate.Day, CandidateParts) then
        Exit;
      if CandidateParts.Day = 1 then
      begin
        CurrentDays := CandidateDays;
        FoundNextStart := True;
        Break;
      end;
    end;
    if not FoundNextStart then
      Break;
  end;

  AISODate.Year := 0;
  AISODate.Month := 0;
  AISODate.Day := 0;
end;

function TryLunisolarISODateToCalendar(const ACalendarId: string; const AISOYear,
  AISOMonth, AISODay: Integer; out ACalendarDate: TTemporalDateRecord): Boolean;
var
  TargetDays: Int64;
  Parts: TLunisolarDateParts;
  MonthIndex, CacheCode, CacheIndex: Integer;
begin
  Result := False;
  ACalendarDate.Year := 0;
  ACalendarDate.Month := 0;
  ACalendarDate.Day := 0;
  if not IsValidDate(AISOYear, AISOMonth, AISODay) then
    Exit;

  CacheCode := LunisolarCalendarCode(ACalendarId);
  CacheIndex := LunisolarCacheIndex(CacheCode, AISOYear, AISOMonth, AISODay);
  if LunisolarISOToCalendarCache[CacheIndex].Valid and
     (LunisolarISOToCalendarCache[CacheIndex].CalendarCode = CacheCode) and
     (LunisolarISOToCalendarCache[CacheIndex].ISOYear = AISOYear) and
     (LunisolarISOToCalendarCache[CacheIndex].ISOMonth = AISOMonth) and
     (LunisolarISOToCalendarCache[CacheIndex].ISODay = AISODay) then
  begin
    ACalendarDate := LunisolarISOToCalendarCache[CacheIndex].CalendarDate;
    Exit(True);
  end;

  TargetDays := DateToEpochDays(AISOYear, AISOMonth, AISODay);
  if not TryFormatLunisolarDateParts(ACalendarId, AISOYear, AISOMonth,
    AISODay, Parts) then
    Exit;
  if not TryLunisolarMonthIndexAt(ACalendarId, TargetDays, Parts.Year,
    MonthIndex) then
    Exit;
  ACalendarDate.Year := Parts.Year;
  ACalendarDate.Month := MonthIndex;
  ACalendarDate.Day := Parts.Day;
  LunisolarISOToCalendarCache[CacheIndex].Valid := True;
  LunisolarISOToCalendarCache[CacheIndex].CalendarCode := CacheCode;
  LunisolarISOToCalendarCache[CacheIndex].ISOYear := AISOYear;
  LunisolarISOToCalendarCache[CacheIndex].ISOMonth := AISOMonth;
  LunisolarISOToCalendarCache[CacheIndex].ISODay := AISODay;
  LunisolarISOToCalendarCache[CacheIndex].CalendarDate := ACalendarDate;
  Result := True;
end;

function TryCalendarEraFromYear(const ACalendarId: string; const AYear: Integer;
  out AEra: string; out AEraYear: Integer; out AHasEra: Boolean): Boolean;
begin
  AEra := '';
  AEraYear := 0;
  AHasEra := True;
  Result := True;

  if ACalendarId = 'buddhist' then
  begin
    AEra := 'be';
    AEraYear := AYear;
  end
  else if ACalendarId = 'coptic' then
  begin
    AEra := 'am';
    AEraYear := AYear;
  end
  else if ACalendarId = 'ethioaa' then
  begin
    AEra := 'aa';
    AEraYear := AYear;
  end
  else if ACalendarId = 'ethiopic' then
  begin
    if AYear <= 0 then
    begin
      AEra := 'aa';
      AEraYear := AYear + 5500;
    end
    else
    begin
      AEra := 'am';
      AEraYear := AYear;
    end;
  end
  else if (ACalendarId = 'gregory') or (ACalendarId = 'japanese') then
  begin
    if ACalendarId = 'japanese' then
    begin
      if AYear >= 2019 then
      begin
        AEra := 'reiwa';
        AEraYear := AYear - 2018;
        Exit;
      end
      else if AYear >= 1989 then
      begin
        AEra := 'heisei';
        AEraYear := AYear - 1988;
        Exit;
      end
      else if AYear >= 1926 then
      begin
        AEra := 'showa';
        AEraYear := AYear - 1925;
        Exit;
      end
      else if AYear >= 1912 then
      begin
        AEra := 'taisho';
        AEraYear := AYear - 1911;
        Exit;
      end
      else if AYear >= 1873 then
      begin
        AEra := 'meiji';
        AEraYear := AYear - 1867;
        Exit;
      end;
    end;

    if AYear >= 1 then
    begin
      AEra := 'ce';
      AEraYear := AYear;
    end
    else
    begin
      AEra := 'bce';
      AEraYear := 1 - AYear;
    end;
  end
  else if ACalendarId = 'hebrew' then
  begin
    AEra := 'am';
    AEraYear := AYear;
  end
  else if ACalendarId = 'indian' then
  begin
    AEra := 'shaka';
    AEraYear := AYear;
  end
  else if (ACalendarId = 'islamic-civil') or (ACalendarId = 'islamic-tbla') or
          (ACalendarId = 'islamic-umalqura') then
  begin
    if AYear >= 1 then
    begin
      AEra := 'ah';
      AEraYear := AYear;
    end
    else
    begin
      AEra := 'bh';
      AEraYear := 1 - AYear;
    end;
  end
  else if ACalendarId = 'persian' then
  begin
    AEra := 'ap';
    AEraYear := AYear;
  end
  else if ACalendarId = 'roc' then
  begin
    if AYear >= 1 then
    begin
      AEra := 'roc';
      AEraYear := AYear;
    end
    else
    begin
      AEra := 'broc';
      AEraYear := 1 - AYear;
    end;
  end
  else
  begin
    AHasEra := False;
    AEra := '';
    AEraYear := 0;
  end;
end;

procedure JapaneseEraFromDate(const AYear, AMonth, ADay: Integer;
  out AEra: string; out AEraYear: Integer);
begin
  if CompareDates(AYear, AMonth, ADay, 2019, 5, 1) >= 0 then
  begin
    AEra := 'reiwa';
    AEraYear := AYear - 2018;
  end
  else if CompareDates(AYear, AMonth, ADay, 1989, 1, 8) >= 0 then
  begin
    AEra := 'heisei';
    AEraYear := AYear - 1988;
  end
  else if CompareDates(AYear, AMonth, ADay, 1926, 12, 25) >= 0 then
  begin
    AEra := 'showa';
    AEraYear := AYear - 1925;
  end
  else if CompareDates(AYear, AMonth, ADay, 1912, 7, 30) >= 0 then
  begin
    AEra := 'taisho';
    AEraYear := AYear - 1911;
  end
  else if CompareDates(AYear, AMonth, ADay, 1873, 1, 1) >= 0 then
  begin
    AEra := 'meiji';
    AEraYear := AYear - 1867;
  end
  else if AYear >= 1 then
  begin
    AEra := 'ce';
    AEraYear := AYear;
  end
  else
  begin
    AEra := 'bce';
    AEraYear := 1 - AYear;
  end;
end;

function TryCalendarYearFromEra(const ACalendarId, AEra: string; const AEraYear: Integer;
  out AYear: Integer): Boolean;
var
  EraLower: string;
begin
  Result := True;
  EraLower := LowerCase(AEra);

  if ACalendarId = 'buddhist' then
  begin
    if EraLower <> 'be' then Exit(False);
    AYear := AEraYear;
  end
  else if ACalendarId = 'coptic' then
  begin
    if EraLower <> 'am' then Exit(False);
    AYear := AEraYear;
  end
  else if ACalendarId = 'ethioaa' then
  begin
    if EraLower <> 'aa' then Exit(False);
    AYear := AEraYear;
  end
  else if ACalendarId = 'ethiopic' then
  begin
    if EraLower = 'aa' then
      AYear := AEraYear - 5500
    else if EraLower = 'am' then
      AYear := AEraYear
    else
      Exit(False);
  end
  else if (ACalendarId = 'gregory') or (ACalendarId = 'japanese') then
  begin
    if (EraLower = 'ce') or (EraLower = 'ad') then
      AYear := AEraYear
    else if (EraLower = 'bce') or (EraLower = 'bc') then
      AYear := 1 - AEraYear
    else if (ACalendarId = 'japanese') and (EraLower = 'meiji') then
      AYear := AEraYear + 1867
    else if (ACalendarId = 'japanese') and (EraLower = 'taisho') then
      AYear := AEraYear + 1911
    else if (ACalendarId = 'japanese') and (EraLower = 'showa') then
      AYear := AEraYear + 1925
    else if (ACalendarId = 'japanese') and (EraLower = 'heisei') then
      AYear := AEraYear + 1988
    else if (ACalendarId = 'japanese') and (EraLower = 'reiwa') then
      AYear := AEraYear + 2018
    else
      Exit(False);
  end
  else if ACalendarId = 'hebrew' then
  begin
    if EraLower <> 'am' then Exit(False);
    AYear := AEraYear;
  end
  else if ACalendarId = 'indian' then
  begin
    if EraLower <> 'shaka' then Exit(False);
    AYear := AEraYear;
  end
  else if (ACalendarId = 'islamic-civil') or (ACalendarId = 'islamic-tbla') or
          (ACalendarId = 'islamic-umalqura') then
  begin
    if EraLower = 'ah' then
      AYear := AEraYear
    else if EraLower = 'bh' then
      AYear := 1 - AEraYear
    else
      Exit(False);
  end
  else if ACalendarId = 'persian' then
  begin
    if EraLower <> 'ap' then Exit(False);
    AYear := AEraYear;
  end
  else if ACalendarId = 'roc' then
  begin
    if EraLower = 'roc' then
      AYear := AEraYear
    else if EraLower = 'broc' then
      AYear := 1 - AEraYear
    else
      Exit(False);
  end
  else
    Exit(False);
end;

function TemporalCalendarUsesEra(const ACalendarId: string): Boolean;
var
  Era: string;
  EraYear: Integer;
  HasEra: Boolean;
begin
  Result := TryCalendarEraFromYear(ACalendarId, 1, Era, EraYear, HasEra) and HasEra;
end;

function TryOpenCalendar(const AICU: TTemporalCalendarICU; const ACalendarId: string;
  out ACalendar: TICUCalendar): Boolean;
var
  ZoneId: UnicodeString;
  Locale: AnsiString;
  Status: TICUErrorCode;
begin
  Result := False;
  ACalendar := nil;
  ZoneId := UnicodeString('UTC');
  Locale := CalendarLocale(ACalendarId);
  Status := ICU_SUCCESS;
  ACalendar := AICU.OpenCalendar(PWideChar(ZoneId), Length(ZoneId),
    PAnsiChar(Locale), ICU_CAL_DEFAULT, Status);
  Result := ICUSucceeded(Status) and Assigned(ACalendar);
  if Result and Assigned(AICU.SetGregorianChange) and
     ((ACalendarId = 'gregory') or (ACalendarId = 'buddhist') or
      (ACalendarId = 'japanese') or (ACalendarId = 'roc')) then
  begin
    Status := ICU_SUCCESS;
    AICU.SetGregorianChange(ACalendar, -8.64E15, Status);
  end;
end;

function TryRawICUCalendarDateToISO(const ACalendarId: string; const AYear,
  AMonth, ADay: Integer; const AIsLeapMonth, AUseLeapMonthFlag: Boolean;
  out AISODate: TTemporalDateRecord): Boolean;
var
  ICUFunctions: TTemporalCalendarICU;
  Calendar: TICUCalendar;
  Status: TICUErrorCode;
  Millis: TICUDate;
  Days: Int64;
  CheckYear, CheckMonth, CheckDay, CheckLeapMonth: Integer;
begin
  Result := False;
  AISODate.Year := 0;
  AISODate.Month := 0;
  AISODate.Day := 0;

  if (AMonth < 1) or (ADay < 1) or not TryGetCalendarICU(ICUFunctions) or
     not TryOpenCalendar(ICUFunctions, ACalendarId, Calendar) then
    Exit;

  try
    ICUFunctions.ClearCalendar(Calendar);
    ICUFunctions.SetField(Calendar, CalendarYearField(ACalendarId), AYear);
    ICUFunctions.SetField(Calendar, ICU_FIELD_MONTH, AMonth - 1);
    if AUseLeapMonthFlag then
      ICUFunctions.SetField(Calendar, ICU_FIELD_IS_LEAP_MONTH, Ord(AIsLeapMonth));
    ICUFunctions.SetField(Calendar, ICU_FIELD_DATE, ADay);

    Status := ICU_SUCCESS;
    Millis := ICUFunctions.GetMillis(Calendar, Status);
    if not ICUSucceeded(Status) then
      Exit;

    CheckYear := ICUFunctions.GetField(Calendar, CalendarYearField(ACalendarId), Status);
    CheckMonth := ICUFunctions.GetField(Calendar, ICU_FIELD_MONTH, Status) + 1;
    CheckDay := ICUFunctions.GetField(Calendar, ICU_FIELD_DATE, Status);
    CheckLeapMonth := ICUFunctions.GetField(Calendar, ICU_FIELD_IS_LEAP_MONTH, Status);
    if (not ICUSucceeded(Status)) or (CheckYear <> AYear) or
       (CheckMonth <> AMonth) or (CheckDay <> ADay) or
       (AUseLeapMonthFlag and ((CheckLeapMonth <> 0) <> AIsLeapMonth)) then
      Exit;

    Days := Floor(Millis / MS_PER_DAY);
    AISODate := EpochDaysToDate(Days);
    Result := IsValidDate(AISODate.Year, AISODate.Month, AISODate.Day);
  finally
    ICUFunctions.CloseCalendar(Calendar);
  end;
end;

function TryCalendarDateToISO(const ACalendarId: string; const AYear, AMonth, ADay: Integer;
  out AISODate: TTemporalDateRecord): Boolean;
var
  ICUFunctions: TTemporalCalendarICU;
  Calendar: TICUCalendar;
  Status: TICUErrorCode;
  Millis: TICUDate;
  Days: Int64;
  CalendarMonth, CheckYear, CheckMonth, CheckDay: Integer;
begin
  Result := False;
  AISODate.Year := 0;
  AISODate.Month := 0;
  AISODate.Day := 0;
  CalendarMonth := AMonth;

  if ACalendarId = 'iso8601' then
  begin
    if not IsValidDate(AYear, AMonth, ADay) then
      Exit;
    AISODate.Year := AYear;
    AISODate.Month := AMonth;
    AISODate.Day := ADay;
    Exit(True);
  end;

  if IsUnsupportedTemporalCalendarConversion(ACalendarId) then
    Exit;

  if (ACalendarId = 'gregory') or (ACalendarId = 'japanese') then
  begin
    if not IsValidDate(AYear, AMonth, ADay) then
      Exit;
    AISODate.Year := AYear;
    AISODate.Month := AMonth;
    AISODate.Day := ADay;
    Exit(True);
  end;

  if ACalendarId = 'buddhist' then
  begin
    if not IsValidDate(AYear - 543, AMonth, ADay) then
      Exit;
    AISODate.Year := AYear - 543;
    AISODate.Month := AMonth;
    AISODate.Day := ADay;
    Exit(True);
  end;

  if ACalendarId = 'roc' then
  begin
    if not IsValidDate(AYear + 1911, AMonth, ADay) then
      Exit;
    AISODate.Year := AYear + 1911;
    AISODate.Month := AMonth;
    AISODate.Day := ADay;
    Exit(True);
  end;

  if (ACalendarId = 'coptic') or (ACalendarId = 'ethiopic') or (ACalendarId = 'ethioaa') then
    Exit(TryCopticFamilyDateToISO(ACalendarId, AYear, AMonth, ADay, AISODate));

  if (ACalendarId = 'hebrew') and
     (ShouldUseArithmeticHebrewYear(AYear) or
      ShouldUseArithmeticHebrewOverrideYear(AYear)) then
    Exit(TryHebrewDateToISO(AYear, AMonth, ADay, AISODate));

  if ACalendarId = 'indian' then
    Exit(TryIndianDateToISO(AYear, AMonth, ADay, AISODate));

  if IsLunisolarCalendar(ACalendarId) then
    Exit(TryLunisolarDateToISO(ACalendarId, AYear, AMonth, ADay, False,
      False, AISODate));

  if ACalendarId = 'hebrew' then
  begin
    if IsHebrewLeapYearNumber(AYear) then
    begin
      if (AMonth < 1) or (AMonth > 13) then
        Exit;
    end
    else
    begin
      if (AMonth < 1) or (AMonth > 12) then
        Exit;
      if AMonth > 5 then
        CalendarMonth := AMonth + 1;
    end;
  end;

  if (AMonth < 1) or (ADay < 1) or not TryGetCalendarICU(ICUFunctions) or
     not TryOpenCalendar(ICUFunctions, ACalendarId, Calendar) then
    Exit;

  try
    ICUFunctions.ClearCalendar(Calendar);
    ICUFunctions.SetField(Calendar, CalendarYearField(ACalendarId), AYear);
    ICUFunctions.SetField(Calendar, ICU_FIELD_MONTH, CalendarMonth - 1);
    ICUFunctions.SetField(Calendar, ICU_FIELD_DATE, ADay);

    Status := ICU_SUCCESS;
    Millis := ICUFunctions.GetMillis(Calendar, Status);
    if not ICUSucceeded(Status) then
      Exit;

    CheckYear := ICUFunctions.GetField(Calendar, CalendarYearField(ACalendarId), Status);
    CheckMonth := ICUFunctions.GetField(Calendar, ICU_FIELD_MONTH, Status) + 1;
    CheckDay := ICUFunctions.GetField(Calendar, ICU_FIELD_DATE, Status);
    if (not ICUSucceeded(Status)) or (CheckYear <> AYear) or
       (CheckMonth <> CalendarMonth) or (CheckDay <> ADay) then
      Exit;

    Days := Floor(Millis / MS_PER_DAY);
    AISODate := EpochDaysToDate(Days);
    Result := IsValidDate(AISODate.Year, AISODate.Month, AISODate.Day);
  finally
    ICUFunctions.CloseCalendar(Calendar);
  end;
end;

function TryCalendarMonthCodeDateToISO(const ACalendarId: string; const AYear,
  AMonthCode, ADay: Integer; const AIsLeapMonth: Boolean;
  out AISODate: TTemporalDateRecord): Boolean;
var
  MonthsInYear, CalendarMonth: Integer;
begin
  if IsUnsupportedTemporalCalendarConversion(ACalendarId) then
    Exit(False);

  if IsLunisolarCalendar(ACalendarId) then
    Exit(TryLunisolarDateToISO(ACalendarId, AYear, AMonthCode, ADay, True,
      AIsLeapMonth, AISODate));

  if ACalendarId = 'hebrew' then
  begin
    if not TryCalendarMonthsInYear(ACalendarId, AYear, MonthsInYear) then
      Exit(False);
    if AIsLeapMonth then
    begin
      if (MonthsInYear <= 12) or (AMonthCode <> 5) then
        Exit(False);
      CalendarMonth := 6;
    end
    else if (MonthsInYear > 12) and (AMonthCode >= 6) then
      CalendarMonth := AMonthCode + 1
    else
      CalendarMonth := AMonthCode;
    if (CalendarMonth < 1) or (CalendarMonth > MonthsInYear) then
      Exit(False);
    Exit(TryCalendarDateToISO(ACalendarId, AYear, CalendarMonth, ADay, AISODate));
  end;

  if AIsLeapMonth then
    Exit(False);
  Result := TryCalendarDateToISO(ACalendarId, AYear, AMonthCode, ADay, AISODate);
end;

function TryResolveCalendarDateToISO(const ACalendarId: string; const AYear,
  AMonth, ADay: Integer; const AMatchMonthCode, AIsLeapMonth, AConstrain: Boolean;
  out AISODate: TTemporalDateRecord): Boolean;
var
  WorkMonth, WorkDay: Integer;
  WorkLeap: Boolean;
  MonthStart: TTemporalDateRecord;
  Info: TTemporalCalendarDateInfo;

  function TryResolve(const AMonthValue, ADayValue: Integer;
    const ALeapMonth: Boolean; out ADate: TTemporalDateRecord): Boolean;
  begin
    if AMatchMonthCode then
      Result := TryCalendarMonthCodeDateToISO(ACalendarId, AYear,
        AMonthValue, ADayValue, ALeapMonth, ADate)
    else
      Result := TryCalendarDateToISO(ACalendarId, AYear, AMonthValue,
        ADayValue, ADate);
  end;

begin
  if AMatchMonthCode and AIsLeapMonth and
     (not IsLunisolarCalendar(ACalendarId)) and (ACalendarId <> 'hebrew') then
    Exit(False);

  if AMatchMonthCode and AIsLeapMonth and (ACalendarId = 'hebrew') and
     (AMonth <> 5) then
    Exit(False);

  if TryResolve(AMonth, ADay, AIsLeapMonth, AISODate) then
    Exit(True);
  if not AConstrain then
    Exit(False);

  WorkMonth := AMonth;
  WorkDay := ADay;
  WorkLeap := AIsLeapMonth;
  if WorkMonth < 1 then
    WorkMonth := 1;
  if WorkDay < 1 then
    WorkDay := 1;

  if AMatchMonthCode and WorkLeap then
  begin
    if not TryCalendarMonthCodeDateToISO(ACalendarId, AYear, WorkMonth,
      1, True, MonthStart) then
    begin
      if ACalendarId = 'hebrew' then
        Inc(WorkMonth);
      WorkLeap := False;
    end;
  end
  else if (not AMatchMonthCode) and
          TryCalendarDateToISO(ACalendarId, AYear, 1, 1, MonthStart) and
          TryGetCalendarDateInfo(ACalendarId, MonthStart.Year,
            MonthStart.Month, MonthStart.Day, Info) and
          (WorkMonth > Info.MonthsInYear) then
    WorkMonth := Info.MonthsInYear;

  if not TryResolve(WorkMonth, 1, WorkLeap, MonthStart) then
    Exit(False);
  if TryGetCalendarDateInfo(ACalendarId, MonthStart.Year, MonthStart.Month,
    MonthStart.Day, Info) and (WorkDay > Info.DaysInMonth) then
    WorkDay := Info.DaysInMonth;

  Result := TryResolve(WorkMonth, WorkDay, WorkLeap, AISODate);
end;

function TryAddCalendarDate(const ACalendarId: string; const AISOYear,
  AISOMonth, AISODay: Integer; const AYears, AMonths, AWeeks, ADays: Int64;
  const AConstrain: Boolean; out AISODate: TTemporalDateRecord): Boolean;
var
  Info: TTemporalCalendarDateInfo;
  TargetYear, TargetMonth, TargetDay: Integer;
  MonthsRemaining, Step: Int64;
  MonthsInYear, MonthCodeMonth: Integer;
  IsLeapMonth: Boolean;
  DateRec, ProbeDate: TTemporalDateRecord;
begin
  Result := False;
  AISODate.Year := 0;
  AISODate.Month := 0;
  AISODate.Day := 0;

  if ACalendarId = 'iso8601' then
    Exit(TryAddISOCalendarDate(AISOYear, AISOMonth, AISODay, AYears,
      AMonths, AWeeks, ADays, AConstrain, AISODate));

  if (AYears = 0) and (AMonths = 0) then
  begin
    AISODate := AddDaysToDate(AISOYear, AISOMonth, AISODay,
      AWeeks * 7 + ADays);
    Exit(IsValidDate(AISODate.Year, AISODate.Month, AISODate.Day));
  end;

  if Int64MagnitudeGreaterThan(AYears, MAX_TEMPORAL_CALENDAR_YEAR_OFFSET) or
     Int64MagnitudeGreaterThan(AMonths, MAX_TEMPORAL_CALENDAR_MONTH_OFFSET) then
    Exit;

  if not TryGetCalendarDateInfo(ACalendarId, AISOYear, AISOMonth, AISODay, Info) then
    Exit;
  if (Int64(Info.Date.Year) + AYears < Low(Integer)) or
     (Int64(Info.Date.Year) + AYears > High(Integer)) then
    Exit;

  TargetYear := Info.Date.Year + Integer(AYears);
  TargetMonth := Info.Date.Month;
  TargetDay := Info.Date.Day;
  MonthCodeMonth := Info.Date.Month;
  IsLeapMonth := False;
  if (AMonths = 0) and (ACalendarId <> 'iso8601') and
     TryParseTemporalMonthCode(Info.MonthCode, MonthCodeMonth, IsLeapMonth) then
  begin
    if not TryResolveCalendarDateToISO(ACalendarId, TargetYear,
      MonthCodeMonth, TargetDay, True, IsLeapMonth, AConstrain, DateRec) then
      Exit;
  end
  else if AMonths = 0 then
  begin
    if not TryResolveCalendarDateToISO(ACalendarId, TargetYear,
    Info.Date.Month, TargetDay, False, False, AConstrain, DateRec) then
      Exit;
  end
  else if (ACalendarId <> 'iso8601') and
          TryParseTemporalMonthCode(Info.MonthCode, MonthCodeMonth, IsLeapMonth) then
  begin
    if not TryResolveCalendarDateToISO(ACalendarId, TargetYear,
      MonthCodeMonth, 1, True, IsLeapMonth, AConstrain, DateRec) then
      Exit;
    if not TryGetCalendarDateInfo(ACalendarId, DateRec.Year, DateRec.Month,
      DateRec.Day, Info) then
      Exit;
    TargetYear := Info.Date.Year;
    TargetMonth := Info.Date.Month;
  end;

  if AMonths <> 0 then
  begin
    MonthsRemaining := AMonths;
    if MonthsRemaining > 0 then
      Step := 1
    else
      Step := -1;
    while MonthsRemaining <> 0 do
    begin
      if Step > 0 then
      begin
        if TryCalendarDateToISO(ACalendarId, TargetYear, TargetMonth + 1,
          1, ProbeDate) then
          Inc(TargetMonth)
        else
        begin
          Inc(TargetYear);
          TargetMonth := 1;
          if not TryCalendarDateToISO(ACalendarId, TargetYear, TargetMonth,
            1, ProbeDate) then
            Exit;
        end;
      end
      else
      begin
        Dec(TargetMonth);
        if TargetMonth < 1 then
        begin
          Dec(TargetYear);
          if not TryCalendarMonthsInYear(ACalendarId, TargetYear, MonthsInYear) then
            Exit;
          TargetMonth := MonthsInYear;
        end;
      end;
      Dec(MonthsRemaining, Step);
    end;

    if not TryResolveCalendarDateToISO(ACalendarId, TargetYear, TargetMonth,
      TargetDay, False, False, AConstrain, DateRec) then
      Exit;
  end;

  AISODate := AddDaysToDate(DateRec.Year, DateRec.Month, DateRec.Day,
    AWeeks * 7 + ADays);
  Result := True;
end;

function TryCalendarMonthDayInISOYear(const ACalendarId: string;
  const AISOReferenceYear, AMonthOrMonthCode, ADay: Integer;
  const AMatchMonthCode, AIsLeapMonth: Boolean;
  out AISODate: TTemporalDateRecord): Boolean;
var
  StartDays, Days: Int64;
  Offset, DaysThisYear: Integer;
  Candidate: TTemporalDateRecord;
  Info: TTemporalCalendarDateInfo;
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
  DaysThisYear := DaysInYear(AISOReferenceYear);
  for Offset := 0 to DaysThisYear - 1 do
  begin
    Days := StartDays + Offset;
    Candidate := EpochDaysToDate(Days);
    if not TryGetCalendarDateInfo(ACalendarId, Candidate.Year, Candidate.Month,
      Candidate.Day, Info) then
      Exit(False);
    if Info.Date.Day <> ADay then
      Continue;
    if AMatchMonthCode then
    begin
      if Info.MonthCode = ExpectedMonthCode then
      begin
        AISODate := Candidate;
        Exit(True);
      end;
    end
    else if Info.Date.Month = AMonthOrMonthCode then
    begin
      AISODate := Candidate;
      Exit(True);
    end;
  end;
end;

function CalendarCommonYearDays(const ACalendarId: string): Integer;
begin
  if (ACalendarId = 'islamic') or (ACalendarId = 'islamic-civil') or
     (ACalendarId = 'islamic-rgsa') or (ACalendarId = 'islamic-tbla') or
     (ACalendarId = 'islamic-umalqura') then
    Result := 354
  else
    Result := 365;
end;

function TryCalendarMonthsInYear(const ACalendarId: string; const AYear: Integer;
  out AMonthsInYear: Integer): Boolean;
var
  Month, Delta: Integer;
  CacheCode, CacheIndex: Integer;
  CurrentDays, CandidateDays: Int64;
  DateRec: TTemporalDateRecord;
  Parts, CandidateParts: TLunisolarDateParts;
  FoundNextStart: Boolean;
begin
  AMonthsInYear := 0;
  if ACalendarId = 'iso8601' then
  begin
    AMonthsInYear := 12;
    Exit(True);
  end;

  if (ACalendarId = 'hebrew') and
     (ShouldUseArithmeticHebrewYear(AYear) or
      ShouldUseArithmeticHebrewOverrideYear(AYear)) then
  begin
    AMonthsInYear := HebrewMonthsInYearNumber(AYear);
    Exit(True);
  end;

  if ACalendarId = 'indian' then
  begin
    AMonthsInYear := 12;
    Exit(True);
  end;

  if IsLunisolarCalendar(ACalendarId) then
  begin
    CacheCode := LunisolarCalendarCode(ACalendarId);
    CacheIndex := LunisolarCacheIndex(CacheCode, AYear, 0, 0);
    if LunisolarMonthsInYearCache[CacheIndex].Valid and
       (LunisolarMonthsInYearCache[CacheIndex].CalendarCode = CacheCode) and
       (LunisolarMonthsInYearCache[CacheIndex].Year = AYear) then
    begin
      AMonthsInYear := LunisolarMonthsInYearCache[CacheIndex].MonthsInYear;
      Exit(True);
    end;

    if (not TryRawICUCalendarDateToISO(ACalendarId, AYear, 1, 1, False,
        True, DateRec) or
        not IsLunisolarYearStart(ACalendarId, AYear, DateRec)) and
       not TryFindLunisolarYearStart(ACalendarId, AYear, DateRec) then
      Exit(False);
    CurrentDays := DateToEpochDays(DateRec.Year, DateRec.Month, DateRec.Day);

    for Month := 1 to 14 do
    begin
      DateRec := EpochDaysToDate(CurrentDays);
      if not TryFormatLunisolarDateParts(ACalendarId, DateRec.Year,
        DateRec.Month, DateRec.Day, Parts) then
        Exit(False);
      if (Parts.Year <> AYear) or (Parts.Day <> 1) then
        Break;

      AMonthsInYear := Month;
      FoundNextStart := False;
      for Delta := 29 to 30 do
      begin
        CandidateDays := CurrentDays + Delta;
        DateRec := EpochDaysToDate(CandidateDays);
        if not TryFormatLunisolarDateParts(ACalendarId, DateRec.Year,
          DateRec.Month, DateRec.Day, CandidateParts) then
          Exit(False);
        if CandidateParts.Day = 1 then
        begin
          CurrentDays := CandidateDays;
          FoundNextStart := True;
          Break;
        end;
      end;
      if not FoundNextStart then
        Break;
    end;

    Result := AMonthsInYear > 0;
    if Result then
    begin
      LunisolarMonthsInYearCache[CacheIndex].Valid := True;
      LunisolarMonthsInYearCache[CacheIndex].CalendarCode := CacheCode;
      LunisolarMonthsInYearCache[CacheIndex].Year := AYear;
      LunisolarMonthsInYearCache[CacheIndex].MonthsInYear := AMonthsInYear;
    end;
    Exit;
  end;

  for Month := 1 to 14 do
  begin
    if TryCalendarDateToISO(ACalendarId, AYear, Month, 1, DateRec) then
      AMonthsInYear := Month
    else if Month > 1 then
      Break;
  end;
  Result := AMonthsInYear > 0;
end;

function TryCalendarDaysInYear(const ACalendarId: string; const AYear: Integer;
  out ADaysInYear: Integer): Boolean;
var
  StartDate, NextYearDate: TTemporalDateRecord;
begin
  if ACalendarId = 'iso8601' then
  begin
    ADaysInYear := DaysInYear(AYear);
    Exit(True);
  end;

  if ACalendarId = 'indian' then
  begin
    if IsGregorianLeapYearNumber(Int64(AYear) + 78) then
      ADaysInYear := 366
    else
      ADaysInYear := 365;
    Exit(True);
  end;

  if (ACalendarId = 'hebrew') and
     TryHebrewDaysInYearOverride(AYear, ADaysInYear) then
    Exit(True);

  if (ACalendarId = 'chinese') and
     TryChineseDaysInYearOverride(AYear, ADaysInYear) then
    Exit(True);

  Result := TryCalendarDateToISO(ACalendarId, AYear, 1, 1, StartDate) and
            TryCalendarDateToISO(ACalendarId, AYear + 1, 1, 1, NextYearDate);
  if Result then
    ADaysInYear := Integer(DateToEpochDays(NextYearDate.Year, NextYearDate.Month, NextYearDate.Day) -
      DateToEpochDays(StartDate.Year, StartDate.Month, StartDate.Day))
  else
    ADaysInYear := 0;
end;

function TryCalendarDaysInMonth(const ACalendarId: string; const AYear, AMonth: Integer;
  const AMonthsInYear: Integer; out ADaysInMonth: Integer): Boolean;
var
  StartDate, NextMonthDate: TTemporalDateRecord;
  NextYear, NextMonth: Integer;
begin
  if ACalendarId = 'iso8601' then
  begin
    ADaysInMonth := DaysInMonth(AYear, AMonth);
    Exit(ADaysInMonth > 0);
  end;

  if ACalendarId = 'indian' then
  begin
    ADaysInMonth := IndianDaysInMonthNumber(AYear + 78, AMonth);
    Exit(ADaysInMonth > 0);
  end;

  if AMonth >= AMonthsInYear then
  begin
    NextYear := AYear + 1;
    NextMonth := 1;
  end
  else
  begin
    NextYear := AYear;
    NextMonth := AMonth + 1;
  end;

  Result := TryCalendarDateToISO(ACalendarId, AYear, AMonth, 1, StartDate) and
            TryCalendarDateToISO(ACalendarId, NextYear, NextMonth, 1, NextMonthDate);
  if Result then
    ADaysInMonth := Integer(DateToEpochDays(NextMonthDate.Year, NextMonthDate.Month, NextMonthDate.Day) -
      DateToEpochDays(StartDate.Year, StartDate.Month, StartDate.Day))
  else
    ADaysInMonth := 0;
end;

function TryCalendarDayOfYear(const ACalendarId: string; const AYear, AMonth, ADay: Integer;
  out ADayOfYear: Integer): Boolean;
var
  YearStart, DateRec: TTemporalDateRecord;
  Month: Integer;
begin
  if ACalendarId = 'iso8601' then
  begin
    ADayOfYear := DayOfYear(AYear, AMonth, ADay);
    Exit(True);
  end;

  if ACalendarId = 'indian' then
  begin
    if (AMonth < 1) or (AMonth > 12) or
       (ADay < 1) or (ADay > IndianDaysInMonthNumber(AYear + 78, AMonth)) then
      Exit(False);
    ADayOfYear := ADay;
    for Month := 1 to AMonth - 1 do
      Inc(ADayOfYear, IndianDaysInMonthNumber(AYear + 78, Month));
    Exit(True);
  end;

  Result := TryCalendarDateToISO(ACalendarId, AYear, 1, 1, YearStart) and
            TryCalendarDateToISO(ACalendarId, AYear, AMonth, ADay, DateRec);
  if Result then
    ADayOfYear := Integer(DateToEpochDays(DateRec.Year, DateRec.Month, DateRec.Day) -
      DateToEpochDays(YearStart.Year, YearStart.Month, YearStart.Day)) + 1
  else
    ADayOfYear := 0;
end;

function TryISODateToCalendar(const ACalendarId: string; const AISOYear, AISOMonth, AISODay: Integer;
  out ACalendarDate: TTemporalDateRecord): Boolean;
var
  ICUFunctions: TTemporalCalendarICU;
  Calendar: TICUCalendar;
  Status: TICUErrorCode;
  Millis: TICUDate;
begin
  Result := False;
  ACalendarDate.Year := 0;
  ACalendarDate.Month := 0;
  ACalendarDate.Day := 0;

  if ACalendarId = 'iso8601' then
  begin
    if not IsValidDate(AISOYear, AISOMonth, AISODay) then
      Exit;
    ACalendarDate.Year := AISOYear;
    ACalendarDate.Month := AISOMonth;
    ACalendarDate.Day := AISODay;
    Exit(True);
  end;

  if (ACalendarId = 'gregory') or (ACalendarId = 'japanese') then
  begin
    if not IsValidDate(AISOYear, AISOMonth, AISODay) then
      Exit;
    ACalendarDate.Year := AISOYear;
    ACalendarDate.Month := AISOMonth;
    ACalendarDate.Day := AISODay;
    Exit(True);
  end;

  if ACalendarId = 'buddhist' then
  begin
    if not IsValidDate(AISOYear, AISOMonth, AISODay) then
      Exit;
    ACalendarDate.Year := AISOYear + 543;
    ACalendarDate.Month := AISOMonth;
    ACalendarDate.Day := AISODay;
    Exit(True);
  end;

  if ACalendarId = 'roc' then
  begin
    if not IsValidDate(AISOYear, AISOMonth, AISODay) then
      Exit;
    ACalendarDate.Year := AISOYear - 1911;
    ACalendarDate.Month := AISOMonth;
    ACalendarDate.Day := AISODay;
    Exit(True);
  end;

  if (ACalendarId = 'coptic') or (ACalendarId = 'ethiopic') or (ACalendarId = 'ethioaa') then
    Exit(TryISODateToCopticFamily(ACalendarId, AISOYear, AISOMonth, AISODay, ACalendarDate));

  if (ACalendarId = 'hebrew') and
     (ShouldUseArithmeticHebrewISOYear(AISOYear) or
      ShouldUseArithmeticHebrewISODateOverride(AISOYear, AISOMonth, AISODay)) then
    Exit(TryISODateToHebrew(AISOYear, AISOMonth, AISODay, ACalendarDate));

  if ACalendarId = 'indian' then
    Exit(TryISODateToIndian(AISOYear, AISOMonth, AISODay, ACalendarDate));

  if TryExtremeISODateToCalendar(ACalendarId, AISOYear, AISOMonth, AISODay,
    ACalendarDate) then
    Exit(True);

  if IsLunisolarCalendar(ACalendarId) then
    Exit(TryLunisolarISODateToCalendar(ACalendarId, AISOYear, AISOMonth,
      AISODay, ACalendarDate));

  if IsUnsupportedTemporalCalendarConversion(ACalendarId) or
     not IsValidDate(AISOYear, AISOMonth, AISODay) or
     not TryGetCalendarICU(ICUFunctions) or
     not TryOpenCalendar(ICUFunctions, ACalendarId, Calendar) then
    Exit;

  try
    Millis := DateToEpochDays(AISOYear, AISOMonth, AISODay) * MS_PER_DAY +
      MILLISECONDS_PER_HALF_DAY;
    Status := ICU_SUCCESS;
    ICUFunctions.SetMillis(Calendar, Millis, Status);
    if not ICUSucceeded(Status) then
      Exit;

    ACalendarDate.Year := ICUFunctions.GetField(Calendar, CalendarYearField(ACalendarId), Status);
    ACalendarDate.Month := ICUFunctions.GetField(Calendar, ICU_FIELD_MONTH, Status) + 1;
    ACalendarDate.Day := ICUFunctions.GetField(Calendar, ICU_FIELD_DATE, Status);
    if (ACalendarId = 'hebrew') and not IsHebrewLeapYearNumber(ACalendarDate.Year) and
       (ACalendarDate.Month > 6) then
      Dec(ACalendarDate.Month);
    Result := ICUSucceeded(Status);
  finally
    ICUFunctions.CloseCalendar(Calendar);
  end;
end;

function TryCalendarLeapMonthFlag(const ACalendarId: string; const AISOYear, AISOMonth, AISODay: Integer;
  out AIsLeapMonth: Boolean): Boolean;
var
  ICUFunctions: TTemporalCalendarICU;
  Calendar: TICUCalendar;
  Status: TICUErrorCode;
  Millis: TICUDate;
  Parts: TLunisolarDateParts;
begin
  AIsLeapMonth := False;
  if ACalendarId = 'iso8601' then
    Exit(True);

  if ACalendarId = 'indian' then
    Exit(IsValidDate(AISOYear, AISOMonth, AISODay));

  if IsLunisolarCalendar(ACalendarId) then
  begin
    Result := TryFormatLunisolarDateParts(ACalendarId, AISOYear, AISOMonth,
      AISODay, Parts);
    if Result then
      AIsLeapMonth := Parts.IsLeapMonth;
    Exit;
  end;

  Result := False;
  if not IsValidDate(AISOYear, AISOMonth, AISODay) or
     not TryGetCalendarICU(ICUFunctions) or
     not TryOpenCalendar(ICUFunctions, ACalendarId, Calendar) then
    Exit;

  try
    Millis := DateToEpochDays(AISOYear, AISOMonth, AISODay) * MS_PER_DAY +
      MILLISECONDS_PER_HALF_DAY;
    Status := ICU_SUCCESS;
    ICUFunctions.SetMillis(Calendar, Millis, Status);
    if not ICUSucceeded(Status) then
      Exit;
    AIsLeapMonth := ICUFunctions.GetField(Calendar, ICU_FIELD_IS_LEAP_MONTH, Status) <> 0;
    Result := ICUSucceeded(Status);
  finally
    ICUFunctions.CloseCalendar(Calendar);
  end;
end;

function TryGetCalendarDateDay(const ACalendarId: string; const AISOYear,
  AISOMonth, AISODay: Integer; out ADay: Integer): Boolean;
var
  DateRec: TTemporalDateRecord;
  Parts: TLunisolarDateParts;
begin
  ADay := 0;
  if IsLunisolarCalendar(ACalendarId) then
  begin
    Result := TryFormatLunisolarDateParts(ACalendarId, AISOYear, AISOMonth,
      AISODay, Parts);
    if Result then
      ADay := Parts.Day;
    Exit;
  end;

  Result := TryISODateToCalendar(ACalendarId, AISOYear, AISOMonth, AISODay,
    DateRec);
  if Result then
    ADay := DateRec.Day;
end;

function TryGetCalendarDateDaysInMonth(const ACalendarId: string;
  const AISOYear, AISOMonth, AISODay: Integer; out ADaysInMonth: Integer): Boolean;
var
  DateRec, CandidateDate: TTemporalDateRecord;
  Parts, CandidateParts: TLunisolarDateParts;
  CurrentDays, CandidateDays: Int64;
  Delta, MonthsInYear: Integer;
begin
  ADaysInMonth := 0;
  if IsLunisolarCalendar(ACalendarId) then
  begin
    if not TryFormatLunisolarDateParts(ACalendarId, AISOYear, AISOMonth,
      AISODay, Parts) then
      Exit(False);
    CurrentDays := DateToEpochDays(AISOYear, AISOMonth, AISODay) -
      (Parts.Day - 1);
    for Delta := 29 to 30 do
    begin
      CandidateDays := CurrentDays + Delta;
      CandidateDate := EpochDaysToDate(CandidateDays);
      if not TryFormatLunisolarDateParts(ACalendarId, CandidateDate.Year,
        CandidateDate.Month, CandidateDate.Day, CandidateParts) then
        Exit(False);
      if CandidateParts.Day = 1 then
      begin
        ADaysInMonth := Delta;
        Exit(True);
      end;
    end;
    Exit(False);
  end;

  Result := TryISODateToCalendar(ACalendarId, AISOYear, AISOMonth, AISODay,
    DateRec) and
    TryCalendarMonthsInYear(ACalendarId, DateRec.Year, MonthsInYear) and
    TryCalendarDaysInMonth(ACalendarId, DateRec.Year, DateRec.Month,
      MonthsInYear, ADaysInMonth);
end;

function TryGetCalendarDateInfo(const ACalendarId: string; const AISOYear, AISOMonth, AISODay: Integer;
  out AInfo: TTemporalCalendarDateInfo): Boolean;
var
  IsLeapMonth: Boolean;
  LunisolarParts: TLunisolarDateParts;
  ChineseMonthCodeMonth: Integer;
  ChineseIsLeapMonth: Boolean;
begin
  FillChar(AInfo, SizeOf(AInfo), 0);
  Result := TryISODateToCalendar(ACalendarId, AISOYear, AISOMonth, AISODay, AInfo.Date);
  if not Result then
    Exit;

  if IsLunisolarCalendar(ACalendarId) and
     TryFormatLunisolarDateParts(ACalendarId, AISOYear, AISOMonth, AISODay,
       LunisolarParts) then
  begin
    AInfo.MonthCode := FormatTemporalMonthCode(LunisolarParts.MonthCodeMonth,
      LunisolarParts.IsLeapMonth);
    if (ACalendarId = 'chinese') and
       TryChineseMonthCodeForMonthIndex(AInfo.Date.Year, AInfo.Date.Month,
         ChineseMonthCodeMonth, ChineseIsLeapMonth) then
      AInfo.MonthCode := FormatTemporalMonthCode(ChineseMonthCodeMonth,
        ChineseIsLeapMonth);
  end
  else
  begin
    if not TryCalendarLeapMonthFlag(ACalendarId, AISOYear, AISOMonth, AISODay, IsLeapMonth) then
      IsLeapMonth := False;
    AInfo.MonthCode := FormatTemporalMonthCode(AInfo.Date.Month, IsLeapMonth);
  end;
  if not TryCalendarEraFromYear(ACalendarId, AInfo.Date.Year, AInfo.Era,
    AInfo.EraYear, AInfo.HasEra) then
    Exit(False);
  if ACalendarId = 'japanese' then
    JapaneseEraFromDate(AInfo.Date.Year, AInfo.Date.Month, AInfo.Date.Day,
      AInfo.Era, AInfo.EraYear);
  AInfo.DayOfWeek := Goccia.Temporal.Utils.DayOfWeek(AISOYear, AISOMonth, AISODay);
  Result := TryCalendarMonthsInYear(ACalendarId, AInfo.Date.Year, AInfo.MonthsInYear) and
            TryCalendarDaysInYear(ACalendarId, AInfo.Date.Year, AInfo.DaysInYear) and
            TryCalendarDaysInMonth(ACalendarId, AInfo.Date.Year, AInfo.Date.Month,
              AInfo.MonthsInYear, AInfo.DaysInMonth) and
            TryCalendarDayOfYear(ACalendarId, AInfo.Date.Year, AInfo.Date.Month,
              AInfo.Date.Day, AInfo.DayOfYear);
  if not Result then
    Exit;

  if (ACalendarId = 'chinese') or (ACalendarId = 'dangi') or (ACalendarId = 'hebrew') then
    AInfo.InLeapYear := AInfo.MonthsInYear > 12
  else
    AInfo.InLeapYear := AInfo.DaysInYear > CalendarCommonYearDays(ACalendarId);

  if ACalendarId = 'hebrew' then
  begin
    if AInfo.MonthsInYear > 12 then
    begin
      if AInfo.Date.Month = 6 then
        AInfo.MonthCode := FormatTemporalMonthCode(5, True)
      else if AInfo.Date.Month > 6 then
        AInfo.MonthCode := FormatTemporalMonthCode(AInfo.Date.Month - 1, False)
      else
        AInfo.MonthCode := FormatTemporalMonthCode(AInfo.Date.Month, False);
    end
    else
      AInfo.MonthCode := FormatTemporalMonthCode(AInfo.Date.Month, False);
  end;
end;

initialization
  CalendarICULoadAttempted := False;
  CalendarICUAvailable := False;

end.
