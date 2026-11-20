unit IntlTypes;

{$I Shared.inc}

interface

type
  TIntlCollatorSensitivity = (icsBase, icsAccent, icsCase, icsVariant);
  TIntlCollatorUsage = (icuSort, icuSearch);
  TIntlNumberStyle = (insDecimal, insCurrency, insPercent, insUnit);
  TIntlNumberNotation = (innStandard, innScientific, innEngineering, innCompact);
  TIntlNumberCompactDisplay = (incdShort, incdLong);
  TIntlNumberCurrencyDisplay = (incdSymbol, incdNarrowSymbol, incdCode, incdName);
  TIntlNumberCurrencySign = (incsStandard, incsAccounting);
  TIntlNumberSignDisplay = (insdAuto, insdNever, insdAlways, insdExceptZero, insdNegative);
  TIntlNumberUnitDisplay = (inudShort, inudLong, inudNarrow);
  TIntlNumberTrailingZeroDisplay = (intzAuto, intzStripIfInteger);
  TIntlNumberRoundingMode = (inrmCeil, inrmFloor, inrmExpand, inrmTrunc,
    inrmHalfCeil, inrmHalfFloor, inrmHalfExpand, inrmHalfTrunc, inrmHalfEven);
  TIntlNumberRoundingPriority = (inrpAuto, inrpMorePrecision, inrpLessPrecision);
  TIntlNumberUseGrouping = (inugAuto, inugAlways, inugMin2, inugFalse);
  TIntlDateTimeStyle = (idtsNone, idtsFull, idtsLong, idtsMedium, idtsShort);
  TIntlPluralType = (iptCardinal, iptOrdinal);
  TIntlRelativeTimeUnit = (irtuYear, irtuQuarter, irtuMonth, irtuWeek,
    irtuDay, irtuHour, irtuMinute, irtuSecond);
  TIntlRelativeTimeNumeric = (irtnAlways, irtnAuto);
  TIntlRelativeTimeStyle = (irtsLong, irtsShort, irtsNarrow);
  TIntlListFormatType = (ilftConjunction, ilftDisjunction, ilftUnit);
  TIntlListFormatStyle = (ilfsLong, ilfsShort, ilfsNarrow);
  TIntlDisplayNameType = (idntLanguage, idntRegion, idntScript,
    idntCalendar, idntCurrency, idntDateTimeField);
  TIntlDisplayNameStyle = (idnsLong, idnsShort, idnsNarrow);
  TIntlSegmenterGranularity = (isgGrapheme, isgWord, isgSentence);
  TIntlDurationStyle = (idsLong, idsShort, idsNarrow, idsDigital);
  TIntlDurationDisplay = (iddAuto, iddAlways);

  TIntlFormatPart = record
    PartType: string;
    Value: string;
    Source: string;
    UnitIdentifier: string;
  end;
  TIntlFormatPartArray = array of TIntlFormatPart;

  TIntlNumberFormatOptions = record
    Style: TIntlNumberStyle;
    Currency: string;
    CurrencyDisplay: TIntlNumberCurrencyDisplay;
    CurrencySign: TIntlNumberCurrencySign;
    UnitIdentifier: string;
    UnitDisplay: TIntlNumberUnitDisplay;
    Notation: TIntlNumberNotation;
    CompactDisplay: TIntlNumberCompactDisplay;
    SignDisplay: TIntlNumberSignDisplay;
    UseGrouping: TIntlNumberUseGrouping;
    RoundingMode: TIntlNumberRoundingMode;
    TrailingZeroDisplay: TIntlNumberTrailingZeroDisplay;
    MinimumIntegerDigits: Integer;
    MinimumFractionDigits: Integer;
    MaximumFractionDigits: Integer;
    MinimumSignificantDigits: Integer;
    MaximumSignificantDigits: Integer;
    RoundingIncrement: Integer;
    RoundingPriority: TIntlNumberRoundingPriority;
    NumberingSystem: string;
  end;

  TIntlDateTimeFormatOptions = record
    DateStyle: TIntlDateTimeStyle;
    TimeStyle: TIntlDateTimeStyle;
    Calendar: string;
    NumberingSystem: string;
    TimeZone: string;
    Hour12: Integer;
    HourCycle: string;
    Weekday: string;
    Era: string;
    Year: string;
    Month: string;
    Day: string;
    DayPeriod: string;
    Hour: string;
    Minute: string;
    Second: string;
    FractionalSecondDigits: Integer;
    TimeZoneName: string;
  end;

  TIntlListPattern = record
    Start: string;
    Middle: string;
    EndPart: string;
    Pair: string;
  end;

  TIntlRelativeTimePattern = record
    Future: string;
    Past: string;
  end;

  TIntlPluralRuleSet = record
    Zero: string;
    One: string;
    Two: string;
    Few: string;
    Many: string;
    Other: string;
  end;

  TIntlResolvedLocale = record
    Locale: string;
    Extension: string;
  end;

  TIntlBreakIterator = record
    Handle: Pointer;
    Text: string;
  end;

  TIntlSegment = record
    Segment: string;
    Index: Integer;
    IsWordLike: Boolean;
  end;

  TStringArray = array of string;

function DefaultNumberFormatOptions: TIntlNumberFormatOptions;
function DefaultDateTimeFormatOptions: TIntlDateTimeFormatOptions;
function ContainsNulCharacter(const AStr: string): Boolean;

const
  // The calendar names a date pattern field can need, one bit each: eras
  // (G), month names as formatted (M) and as standalone (L), each
  // abbreviated, wide and narrow.
  ICU_NAME_ERA_ABBREVIATED = $001;
  ICU_NAME_ERA_WIDE = $002;
  ICU_NAME_ERA_NARROW = $004;
  ICU_NAME_MONTH_ABBREVIATED = $008;
  ICU_NAME_MONTH_WIDE = $010;
  ICU_NAME_MONTH_NARROW = $020;
  ICU_NAME_STANDALONE_MONTH_ABBREVIATED = $040;
  ICU_NAME_STANDALONE_MONTH_WIDE = $080;
  ICU_NAME_STANDALONE_MONTH_NARROW = $100;

{ The ICU_NAME_* fields an ICU date pattern or skeleton uses. Numeric months
  (M, MM) need no names, and quoted literal text is skipped. }
function ICUNameFieldsInPattern(const APattern: string): Cardinal;

implementation

function ICUNameFieldsInPattern(const APattern: string): Cardinal;
var
  I, RunLength: Integer;
  Field: Char;
  InQuote: Boolean;
begin
  Result := 0;
  InQuote := False;
  I := 1;
  while I <= Length(APattern) do
  begin
    Field := APattern[I];
    if Field = '''' then
    begin
      InQuote := not InQuote;
      Inc(I);
      Continue;
    end;
    RunLength := 1;
    while (I + RunLength <= Length(APattern)) and
          (APattern[I + RunLength] = Field) do
      Inc(RunLength);
    Inc(I, RunLength);
    if InQuote then
      Continue;
    case Field of
      'G':
        if RunLength <= 3 then
          Result := Result or ICU_NAME_ERA_ABBREVIATED
        else if RunLength = 4 then
          Result := Result or ICU_NAME_ERA_WIDE
        else
          Result := Result or ICU_NAME_ERA_NARROW;
      'M':
        case RunLength of
          3: Result := Result or ICU_NAME_MONTH_ABBREVIATED;
          4: Result := Result or ICU_NAME_MONTH_WIDE;
          5: Result := Result or ICU_NAME_MONTH_NARROW;
        end;
      'L':
        case RunLength of
          3: Result := Result or ICU_NAME_STANDALONE_MONTH_ABBREVIATED;
          4: Result := Result or ICU_NAME_STANDALONE_MONTH_WIDE;
          5: Result := Result or ICU_NAME_STANDALONE_MONTH_NARROW;
        end;
    end;
  end;
end;

function ContainsNulCharacter(const AStr: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 1 to Length(AStr) do
    if AStr[I] = #0 then
    begin
      Result := True;
      Exit;
    end;
end;

function DefaultNumberFormatOptions: TIntlNumberFormatOptions;
begin
  Result.Style := insDecimal;
  Result.Currency := '';
  Result.CurrencyDisplay := incdSymbol;
  Result.CurrencySign := incsStandard;
  Result.UnitIdentifier := '';
  Result.UnitDisplay := inudShort;
  Result.Notation := innStandard;
  Result.CompactDisplay := incdShort;
  Result.SignDisplay := insdAuto;
  Result.UseGrouping := inugAuto;
  Result.RoundingMode := inrmHalfExpand;
  Result.TrailingZeroDisplay := intzAuto;
  Result.MinimumIntegerDigits := 1;
  Result.MinimumFractionDigits := -1;
  Result.MaximumFractionDigits := -1;
  Result.MinimumSignificantDigits := -1;
  Result.MaximumSignificantDigits := -1;
  Result.RoundingIncrement := 1;
  Result.RoundingPriority := inrpAuto;
  Result.NumberingSystem := '';
end;

function DefaultDateTimeFormatOptions: TIntlDateTimeFormatOptions;
begin
  Result.DateStyle := idtsNone;
  Result.TimeStyle := idtsNone;
  Result.Calendar := '';
  Result.NumberingSystem := '';
  Result.TimeZone := '';
  Result.Hour12 := -1;
  Result.HourCycle := '';
  Result.Weekday := '';
  Result.Era := '';
  Result.Year := '';
  Result.Month := '';
  Result.Day := '';
  Result.DayPeriod := '';
  Result.Hour := '';
  Result.Minute := '';
  Result.Second := '';
  Result.FractionalSecondDigits := -1;
  Result.TimeZoneName := '';
end;

end.
